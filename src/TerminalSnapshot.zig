const std = @import("std");
const gt = @import("ghostty-vt");

const Self = @This();

storage: []u8,
start: usize,
logical_lines: usize = 0,
includes_scrollback: bool = false,
truncated_before: bool = false,
first_line_partial: bool = false,

/// The caller holds the terminal lock for capture and metadata extraction.
pub fn capture(alloc: std.mem.Allocator, term: *const gt.Terminal, max_lines: usize, max_bytes: usize) !Self {
    var result: Self = .{
        .storage = try alloc.alloc(u8, max_bytes),
        .start = max_bytes,
    };
    errdefer result.deinit(alloc);

    const screen = term.screens.active;
    const active_top = screen.pages.getTopLeft(.active);
    const top = if (term.screens.active_key == .alternate)
        active_top
    else
        screen.pages.getTopLeft(.screen);
    var bottom = screen.pages.getBottomRight(.active).?;
    bottom.x = 0;
    var rows = bottom.rowIterator(.left_up, top);
    var started = false;
    var in_history = false;
    var oldest_continues = false;

    // Traverse from the newest cell.  Neither allocation nor text extraction
    // depends on the amount of retained history before the requested suffix.
    capture_rows: while (rows.next()) |pin| {
        const history = in_history;
        if (pin.eql(active_top)) in_history = true;

        const row = pin.rowAndCell().row;
        const cells = pin.cells(.all);
        var end = cells.len;
        while (end > 0) {
            const cell = &cells[end - 1];
            if (cell.wide != .spacer_head and cell.wide != .spacer_tail and
                (cell.hasGrapheme() or (cell.hasText() and cell.codepoint() != ' '))) break;
            end -= 1;
        }
        if (!started and end == 0) continue;

        if (started and !row.wrap) {
            if (result.logical_lines == max_lines or result.start == 0) {
                result.truncated_before = true;
                break;
            }
            _ = try result.prepend('\n');
            result.logical_lines += 1;
            result.includes_scrollback = result.includes_scrollback or history;
        } else if (!started) {
            result.logical_lines = 1;
            started = true;
        }

        oldest_continues = row.wrap_continuation;
        while (end > 0) {
            end -= 1;
            const cell = &cells[end];
            if (cell.wide == .spacer_head or cell.wide == .spacer_tail) continue;
            if (cell.hasGrapheme()) {
                const codepoints = pin.grapheme(cell).?;
                var index = codepoints.len;
                while (index > 0) {
                    index -= 1;
                    if (!try result.prepend(codepoints[index])) {
                        result.truncated_before = true;
                        result.first_line_partial = true;
                        break :capture_rows;
                    }
                    result.includes_scrollback = result.includes_scrollback or history;
                }
            }
            if (!try result.prepend(if (cell.hasText()) cell.codepoint() else ' ')) {
                result.truncated_before = true;
                result.first_line_partial = true;
                break :capture_rows;
            }
            result.includes_scrollback = result.includes_scrollback or history;
        }
        result.includes_scrollback = result.includes_scrollback or history;
    }

    if (!result.truncated_before and oldest_continues) {
        result.truncated_before = true;
        result.first_line_partial = true;
    }
    if (result.start == result.storage.len) {
        result.logical_lines = 0;
        result.first_line_partial = false;
    }
    return result;
}

fn prepend(self: *Self, codepoint: u21) !bool {
    var encoded: [4]u8 = undefined;
    const len = try std.unicode.utf8Encode(codepoint, &encoded);
    if (len > self.start) return false;
    self.start -= len;
    @memcpy(self.storage[self.start..][0..len], encoded[0..len]);
    return true;
}

pub fn text(self: *const Self) []const u8 {
    return self.storage[self.start..];
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    alloc.free(self.storage);
}
