"""Exercise terminal job control and key handling: python3 tests/terminal-pty.py."""
import os
import fcntl
import struct
from pathlib import Path
import pty
import select
import signal
import termios
import time

executable = str(Path(__file__).resolve().parents[1] / 'maxwell')
pid, terminal = pty.fork()
if pid == 0:
    fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack('HHHH', 24, 80, 0, 0))
    os.environ['TERM'] = 'xterm'
    # Never touch the developer's real remembered session.
    os.environ['MAXWELL_SESSION_FILE'] = '/tmp/maxwell-pty-session.json'
    os.execv(executable, [executable])


pending = b''


def expect(marker):
    global pending
    output = pending
    pending = b''
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        if marker in output:
            pending = output.split(marker, 1)[1]
            return
        if select.select([terminal], [], [], 0.1)[0]:
            output += os.read(terminal, 65536)
    raise AssertionError(f'Timed out waiting for {marker!r}: {output!r}')


try:
    original = termios.tcgetattr(terminal)
    expect(b'maxwell> ')
    # Preserve a partially typed command through transcript browsing.
    os.write(terminal, b'/he\x1b[5~')
    expect(b'PgUp/PgDn scroll')
    os.write(terminal, b'\x1b[6~')
    expect(b'maxwell> /he')
    os.write(terminal, b'lp\r')
    expect(b'/quit           exit')
    expect(b'press Enter to steer the model')
    expect(b'maxwell> ')
    os.write(terminal, b'/quit\r')
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        finished, status = os.waitpid(pid, os.WNOHANG)
        if finished:
            assert os.waitstatus_to_exitcode(status) == 0
            assert termios.tcgetattr(terminal) == original, 'Terminal settings not restored'
            break
        time.sleep(0.05)
    else:
        raise AssertionError('Maxwell did not exit')
    print('Terminal startup, /help, PgUp/PgDn, draft preservation, and restoration passed.')
finally:
    try:
        os.kill(pid, signal.SIGKILL)
        os.waitpid(pid, 0)
    except ProcessLookupError:
        pass
    os.close(terminal)
