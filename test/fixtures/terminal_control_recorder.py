"""Report bounded evidence of each terminal-control input transaction."""

import hashlib
import os
import sys

from pty_setup import set_raw_stdio


def write_all(data):
    while data:
        data = data[os.write(sys.stdout.fileno(), data):]


set_raw_stdio()
canonical = "--canonical-no-echo" in sys.argv[1:]
if canonical:
    import termios

    attrs = termios.tcgetattr(sys.stdin.fileno())
    attrs[3] = (attrs[3] | termios.ICANON) & ~termios.ECHO
    termios.tcsetattr(sys.stdin.fileno(), termios.TCSANOW, attrs)

delimiter = b"\n" if canonical else b"\x04"
write_all(b"GHOSTEL_CONTROL_READY\r\n")
index = 0
count = 0
digest = hashlib.sha256()
while True:
    chunk = os.read(sys.stdin.fileno(), 4096)
    if not chunk:
        break
    parts = chunk.split(delimiter)
    for part_index, part in enumerate(parts):
        count += len(part)
        digest.update(part)
        if part_index < len(parts) - 1:
            report = "\r\nGHOSTEL_CONTROL_%d:%d:%s:END\r\n" % (
                index, count, digest.hexdigest()
            )
            write_all(report.encode("ascii"))
            index += 1
            count = 0
            digest = hashlib.sha256()
