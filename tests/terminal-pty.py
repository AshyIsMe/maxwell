"""Exercise terminal job control, key handling, Ctrl-T, and colour selection:
python3 tests/terminal-pty.py."""
import os
import fcntl
import shutil
import re
import struct
import tempfile
from pathlib import Path
import pty
import select
import signal
import termios
import time

executable = str(Path(__file__).resolve().parents[1] / 'maxwell')
scratch = tempfile.mkdtemp(prefix='maxwell-pty-')
CTRL_T = bytes([20])
SGR = re.compile(rb'\x1b\[[0-9;]*m')


def launch(arguments=(), environment=None):
    pid, terminal = pty.fork()
    if pid == 0:
        fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack('HHHH', 24, 80, 0, 0))
        os.environ['TERM'] = 'xterm'
        os.environ.pop('NO_COLOR', None)
        # Never touch the developer's real remembered session or image.
        os.environ['MAXWELL_SESSION_FILE'] = os.path.join(scratch, 'session.json')
        os.environ['MAXWELL_IMAGE'] = os.path.join(scratch, 'maxwell.image')
        os.environ['XDG_DATA_HOME'] = os.path.join(scratch, 'data')
        os.environ.update(environment or {})
        os.execv(executable, [executable, *arguments])
    return pid, terminal


class Session:
    def __init__(self, arguments=(), environment=None):
        self.pid, self.terminal = launch(arguments, environment)
        self.pending = b''
        self.seen = b''

    def expect(self, marker):
        output = self.pending
        self.pending = b''
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline:
            if marker in output:
                before, self.pending = output.split(marker, 1)
                self.seen += before + marker
                return
            if select.select([self.terminal], [], [], 0.1)[0]:
                output += os.read(self.terminal, 65536)
        raise AssertionError(f'Timed out waiting for {marker!r}: {output!r}')

    def send(self, data):
        os.write(self.terminal, data)

    def quit(self, original=None):
        self.send(b'/quit\r')
        # Quitting saves the chat to the (scratch) global image first.
        deadline = time.monotonic() + 30
        while time.monotonic() < deadline:
            finished, status = os.waitpid(self.pid, os.WNOHANG)
            if finished:
                assert os.waitstatus_to_exitcode(status) == 0
                if original is not None:
                    assert termios.tcgetattr(self.terminal) == original, \
                        'Terminal settings not restored'
                return
            time.sleep(0.05)
        raise AssertionError('Maxwell did not exit')

    def close(self):
        try:
            os.kill(self.pid, signal.SIGKILL)
            os.waitpid(self.pid, 0)
        except (ProcessLookupError, ChildProcessError):
            pass
        os.close(self.terminal)


session = Session()
try:
    original = termios.tcgetattr(session.terminal)
    session.expect(b'maxwell> ')
    # Preserve a partially typed command through transcript browsing.
    session.send(b'/he\x1b[5~')
    session.expect(b'PgUp/PgDn scroll')
    session.send(b'\x1b[6~')
    session.expect(b'maxwell> /he')
    # Ctrl-T toggles tool display: the screen and scrollback are cleared, the
    # transcript is drawn again, and the draft is still on the prompt line.
    session.send(CTRL_T)
    session.expect(b'\x1b[H\x1b[2J\x1b[3J')
    session.expect(b'Type /help')
    session.expect(b'maxwell> /he')
    # Ctrl-T in the viewer switches the mode there, and the main screen is
    # redrawn in the new mode on the way out.
    session.send(b'\x1b[5~')
    session.expect(b'Ctrl-T collapse tools')  # full since the first Ctrl-T
    session.send(CTRL_T)
    session.expect(b'Ctrl-T expand tools')
    session.send(b'\x1b[6~')
    session.expect(b'\x1b[3J')
    session.expect(b'maxwell> /he')
    session.send(b'lp\r')
    session.expect(b'/quit          save the chat and exit')
    session.expect(b'press Enter to steer the model')
    session.expect(b'maxwell> ')
    session.quit(original)
finally:
    session.close()

# --color always styles output even under NO_COLOR; NO_COLOR alone turns
# colour off on a terminal.
for arguments, environment, coloured in (((), {}, True),
                                         (('--color', 'always'), {'NO_COLOR': '1'}, True),
                                         ((), {'NO_COLOR': '1'}, False),
                                         (('--color', 'never'), {}, False)):
    session = Session(arguments, environment)
    try:
        session.expect(b'maxwell> ')
        session.send(b'/help\r')
        session.expect(b'press Enter to steer')
        session.expect(b'maxwell> ')
        assert bool(SGR.search(session.seen)) == coloured, (arguments, environment, session.seen)
        session.quit()
    finally:
        session.close()

# Quitting saved the chat to a large image in the scratch directory.
shutil.rmtree(scratch, ignore_errors=True)
print('Terminal startup, /help, PgUp/PgDn, Ctrl-T redraws, draft preservation, '
      'colour selection, and restoration passed.')
