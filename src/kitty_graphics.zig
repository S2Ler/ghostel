/// Kitty Graphics Protocol support via libghostty-vt.
///
/// Queries libghostty's authoritative placement and image state during
/// each redraw cycle, converts pixel data to PPM for Emacs display,
/// and calls into Elisp to apply image overlays.
const std = @import("std");
const Allocator = std.mem.Allocator;
const emacs = @import("emacs.zig");
const GhostelTerm = @import("GhostelTerm.zig");
const gt = @import("ghostty-vt");
const ppm = @import("ppm.zig");

/// Query all visible kitty graphics placements from libghostty and
/// emit them to Elisp during redraw.
pub fn emitPlacements(env: emacs.Env, term: *GhostelTerm) !void {
    const storage = &term.terminal.screens.active.kitty_images;
    var iterator = storage.placements.iterator();
    // Iterate over all placements. Per-placement errors skip that placement only.
    while (iterator.next()) |entry| {
        emitOnePlacement(
            env,
            term,
            storage,
            entry.key_ptr,
            entry.value_ptr,
        ) catch continue;
    }
}

fn emitOnePlacement(
    env: emacs.Env,
    term: *GhostelTerm,
    storage: *const gt.kitty.graphics.ImageStorage,
    key: *const gt.kitty.graphics.ImageStorage.PlacementKey,
    placement: *const gt.kitty.graphics.ImageStorage.Placement,
) !void {
    const image = storage.images.getPtr(key.image_id) orelse return error.ImageNotFound;
    switch (placement.location) {
        .virtual => {
            const data = try getImageData(term.alloc, image);
            defer term.alloc.free(data);

            // Virtual placements (yazi-style U+10EEEE unicode placeholders).
            // The API doesn't provide viewport positions — Elisp searches
            // the buffer for placeholder characters.
            const img_val = env.makeUnibyteString(data) orelse return error.MakeString;
            var args = [_]emacs.Value{img_val};
            _ = env.funcall(emacs.sym.@"ghostel--kitty-display-virtual", &args);
        },
        .pin => |pin| try emitPinned(env, term, image, placement, pin, 0, 0),
        .relative => |rel| {
            // An unresolvable chain is never drawn.
            const chain = storage.resolveChain(rel) orelse return error.NotVisible;
            switch (chain.root.location) {
                .pin => |root_pin| try emitPinned(
                    env,
                    term,
                    image,
                    placement,
                    root_pin,
                    chain.horizontal_offset,
                    chain.vertical_offset,
                ),
                // Not drawn: the Elisp virtual path applies one image to
                // every placeholder run without attributing runs to an
                // image id, so there is no anchor to offset from.
                .virtual => return error.NotVisible,
                .relative => unreachable,
            }
        },
    }
}

/// Emit a placement anchored at PIN, offset by H_OFF/V_OFF cells.
fn emitPinned(
    env: emacs.Env,
    term: *GhostelTerm,
    image: *const gt.kitty.graphics.Image,
    placement: *const gt.kitty.graphics.ImageStorage.Placement,
    pin: *const gt.PageList.Pin,
    h_off: i32,
    v_off: i32,
) !void {
    const pixel_size = placement.pixelSize(image.*, &term.terminal);
    const grid_size = placement.gridSize(image.*, &term.terminal);
    const pages = &term.terminal.screens.active.pages;
    // A pruned anchor page leaves the pin parked at (0,0) until reaped.
    if (pin.garbage) return error.NotVisible;
    const pin_screen = pages.pointFromPin(.screen, pin.*) orelse return error.NotVisible;
    const active_tl = pages.getTopLeft(.active);
    const active_screen = pages.pointFromPin(.screen, active_tl) orelse return error.NotVisible;
    // i64: protocol offsets and grid sizes are unbounded.  Elisp clips negatives.
    const screen_row: i64 = @as(i64, @intCast(pin_screen.screen.y)) + v_off;
    const active_row: i64 = screen_row - @as(i64, @intCast(active_screen.screen.y));
    const active_col: i64 = @as(i64, @intCast(pin_screen.screen.x)) + h_off;
    const visible = active_row + grid_size.rows > 0 and active_row < term.terminal.rows and
        active_col + grid_size.cols > 0 and active_col < term.terminal.cols;

    if (!visible) return error.NotVisible;

    const data = try getImageData(term.alloc, image);
    defer term.alloc.free(data);

    const img_val = env.makeUnibyteString(data) orelse return error.MakeString;
    _ = env.f("ghostel--kitty-display-image", .{
        img_val,
        screen_row,
        active_col,
        grid_size.cols,
        grid_size.rows,
        pixel_size.width,
        pixel_size.height,
        @min(placement.source_x, image.width),
        @min(placement.source_y, image.height),
        placement.source_width,
        placement.source_height,
    });
}

/// PPM bytes for Emacs, owned by `alloc`.
fn getImageData(alloc: Allocator, image: *const gt.kitty.graphics.Image) ![]const u8 {
    // Decompression happens at transmit time; anything else is a libghostty change.
    if (image.compression != .none) return error.UnsupportedCompression;
    // Chunked transmissions still in flight have no complete bytes yet.
    const data = image.renderData().bytes() orelse return error.EmptyImage;
    if (data.len == 0 or image.width == 0 or image.height == 0) return error.EmptyImage;
    // Alpha is dropped, not composited (see ppm.createPpm doc comment).
    // PNG is decoded to RGBA at transmit time by the hook in module.zig.
    const channels: u32 = switch (image.format) {
        .png => unreachable,
        .rgba => 4,
        .rgb => 3,
        .gray_alpha => 2,
        .gray => 1,
    };
    return ppm.createPpm(alloc, data, image.width, image.height, channels) orelse error.PpmConvert;
}
