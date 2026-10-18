"""Exercise the global image: chat, /checkpoint, /quit, restart with the chat
restored, the single-writer lock, --ephemeral, --print, --export-chat, and
--adopt-chat: python3 tests/checkpoint-pty.py."""
import fcntl
import http.server
import json
import os
import pty
import shutil
import select
import signal
import socketserver
import stat
import struct
import subprocess
import tempfile
import termios
import threading
import time
from pathlib import Path

executable = str(Path(__file__).resolve().parents[1] / 'maxwell')
scratch = tempfile.mkdtemp(prefix='maxwell-checkpoint-')
image = os.path.join(scratch, 'data', 'maxwell', 'maxwell.image')
SECRET = 'sk-maxwell-test-secret-0451'
ENTER = b'\r'
requests = []
compaction_delay = 0


def last_user_text(request):
    users = [m for m in request.get('messages', []) if m.get('role') == 'user']
    return str(users[-1].get('content')) if users else ''


class Provider(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        length = int(self.headers.get('Content-Length', 0))
        request = json.loads(self.rfile.read(length))
        text = last_user_text(request)
        if text.startswith('Compaction:'):
            time.sleep(compaction_delay)
            content = 'summary'
        else:
            requests.append(request)
            content = 'noted-' + str(len(requests))
        body = json.dumps({'choices': [{'message': {'role': 'assistant',
                                                    'content': content}}]}).encode()
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, *arguments):
        pass


class Server(socketserver.ThreadingTCPServer):
    allow_reuse_address = True
    daemon_threads = True


server = Server(('127.0.0.1', 0), Provider)
threading.Thread(target=server.serve_forever, daemon=True).start()
base_url = 'http://127.0.0.1:' + str(server.server_address[1]) + '/v1'
provider = ['--base-url', base_url, '--model', 'pty-model', '--compact-model', 'pty-compact']

# Everything Maxwell could write lives in the scratch directory: never the
# developer's real image, session, logins, or caches.
environment = dict(os.environ)
for name in ('MAXWELL_IMAGE', 'MAXWELL_MODEL', 'MAXWELL_BASE_URL', 'MAXWELL_ENDPOINT',
             'MAXWELL_WIRE_API', 'MAXWELL_API_KEY_ENV', 'MAXWELL_COMPACT_MODEL'):
    environment.pop(name, None)
environment.update({
    'TERM': 'xterm',
    'NO_COLOR': '1',
    'HOME': os.path.join(scratch, 'home'),
    'XDG_DATA_HOME': os.path.join(scratch, 'data'),
    'XDG_CONFIG_HOME': os.path.join(scratch, 'config'),
    'XDG_CACHE_HOME': os.path.join(scratch, 'cache'),
    'MAXWELL_SESSION_FILE': os.path.join(scratch, 'session.json'),
    'MAXWELL_AUTH_FILE': os.path.join(scratch, 'openai-auth.json'),
    'MAXWELL_SAFEAI_AUTH_FILE': os.path.join(scratch, 'safeai-auth.json'),
    'OPENAI_API_KEY': SECRET,
})
os.makedirs(environment['HOME'])


class Session:
    def __init__(self, arguments=()):
        self.pid, self.terminal = pty.fork()
        if self.pid == 0:
            fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack('HHHH', 24, 80, 0, 0))
            os.execve(executable, [executable, *arguments], environment)
        self.pending = b''

    def expect(self, marker, timeout=15):
        output = self.pending
        self.pending = b''
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if marker in output:
                self.pending = output.split(marker, 1)[1]
                return
            if select.select([self.terminal], [], [], 0.1)[0]:
                try:
                    output += os.read(self.terminal, 65536)
                except OSError:
                    break
        raise AssertionError(f'Timed out waiting for {marker!r}: {output!r}')

    def send(self, data):
        os.write(self.terminal, data)

    def wait(self, timeout=30):
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            # Keep the pty drained so the child never blocks on output.
            if select.select([self.terminal], [], [], 0.05)[0]:
                try:
                    self.pending += os.read(self.terminal, 65536)
                except OSError:
                    pass
            finished, status = os.waitpid(self.pid, os.WNOHANG)
            if finished:
                self.pid = None
                return os.waitstatus_to_exitcode(status)
        raise AssertionError('Maxwell did not exit')

    def close(self):
        if self.pid:
            try:
                os.kill(self.pid, signal.SIGKILL)
                os.waitpid(self.pid, 0)
            except (ProcessLookupError, ChildProcessError):
                pass
        os.close(self.terminal)


def run(arguments, program=executable):
    return subprocess.run([program, *arguments], env=environment, stdin=subprocess.DEVNULL,
                          capture_output=True, text=True, timeout=60)


def exported_chat():
    result = run(['--export-chat'], program=image)
    assert result.returncode == 0, result
    assert result.stdout.startswith('(:MAXWELL-CHAT :VERSION 1'), result.stdout[:200]
    return result.stdout


def view_of(request):
    return [m for m in request['messages'] if m.get('role') == 'user'][0]['content']


first = Session(provider)
try:
    first.expect(b'maxwell> ')
    first.send(b'remember the word zebra' + ENTER)
    first.expect(b'noted-1')
    first.expect(b'maxwell> ')
    first.send(b'/memory' + ENTER)
    first.expect(b'2 messages')
    first.expect(b'maxwell> ')

    # /checkpoint saves without exiting; the session goes on.
    first.send(b'/checkpoint' + ENTER)
    first.expect(b'Chat saved to ' + image.encode())
    first.expect(b'maxwell> ')
    assert os.path.exists(image) and not os.path.exists(image + '.tmp')
    saved = os.stat(image)
    assert stat.S_IMODE(saved.st_mode) == 0o700, oct(saved.st_mode)

    # A second Maxwell execs the global image, which refuses: the lock is held.
    second = run([])
    assert second.returncode == 1, second
    assert 'Another Maxwell is using the global chat' in second.stderr, second.stderr

    # --ephemeral never touches the global image or its lock.
    ephemeral = run([*provider, '--ephemeral', '--print', 'scratch question'])
    assert ephemeral.returncode == 0, ephemeral
    assert 'noted-2' in ephemeral.stdout, ephemeral.stdout
    assert os.stat(image).st_mtime_ns == saved.st_mtime_ns

    first.send(b'and the colour blue' + ENTER)
    first.expect(b'noted-3')
    first.expect(b'maxwell> ')
    first.send(b'/quit' + ENTER)
    assert first.wait() == 0
finally:
    first.close()

assert os.path.exists(os.path.join(os.path.dirname(image), 'maxwell.lock'))
assert not os.path.exists(image + '.tmp')
with open(image, 'rb') as stream:
    assert SECRET.encode() not in stream.read(), 'The API key was saved in the image'

# Restart: the binary execs the global image, which shows where the chat left
# off and gives the model the restored view.
restarted = Session()
try:
    restarted.expect(b'remember the word zebra')
    restarted.expect(b'noted-3')
    restarted.expect(b'maxwell> ')
    restarted.send(b'/memory' + ENTER)
    restarted.expect(b'4 messages')
    restarted.expect(b'maxwell> ')
    # PgUp pages back through the log from before this session.
    restarted.send(b'\x1b[5~')
    restarted.expect(b'remember the word zebra')
    restarted.expect(b'Esc returns')
    restarted.send(b'\x1b')
    restarted.expect(b'maxwell> ')
    restarted.send(b'what was the word?' + ENTER)
    restarted.expect(b'noted-4')
    restarted.expect(b'maxwell> ')
    view = view_of(requests[-1])
    for line in ('|[', '] user: remember the word zebra', '] maxwell: noted-1',
                 '] user: and the colour blue'):
        assert line in view, view
    assert 'scratch question' not in view, view

    # A long message needs a model-written summary; the next turn waits for
    # it, with a spinner on the steering line.
    compaction_delay = 2
    restarted.send(b'a long message: ' + b'lorem ipsum ' * 60 + ENTER)
    restarted.expect(b'noted-5')
    restarted.expect(b'maxwell> ')
    restarted.send(b'after the long one' + ENTER)
    restarted.expect(b'Summarizing earlier messages')
    restarted.expect(b' steer> ')
    restarted.expect(b'noted-6')
    restarted.expect(b'maxwell> ')
    assert '] summary' in view_of(requests[-1]), view_of(requests[-1])

    # Ctrl-C stops waiting and sends the message with the view so far.
    compaction_delay = 60
    restarted.send(b'another long message: ' + b'dolor sit amet ' * 50 + ENTER)
    restarted.expect(b'noted-7')
    restarted.expect(b'maxwell> ')
    restarted.send(b'impatient' + ENTER)
    restarted.expect(b'Summarizing earlier messages')
    restarted.send(b'\x03')
    restarted.expect(b'Interrupted. Sending with the view up to message 10')
    restarted.expect(b'noted-8')
    restarted.expect(b'maxwell> ')
    compaction_delay = 0
    # Ctrl-D saves and exits too.
    restarted.send(b'\x04')
    assert restarted.wait() == 0
finally:
    restarted.close()

# --print continues the same chat and saves it.
once = run(['--print', 'one-shot question'])
assert once.returncode == 0, once
assert 'noted-9' in once.stdout, once.stdout
assert 'remember the word zebra' in view_of(requests[-1])
chat = exported_chat()
for text in ('zebra', 'what was the word?', 'impatient', 'one-shot question', 'noted-9'):
    assert text in chat, text

# --adopt-chat moves the chat into this build, which becomes the global image.
before = os.stat(image).st_mtime_ns
adopted = run(['--adopt-chat'])
assert adopted.returncode == 0, adopted
assert 'Adopted 16 messages' in adopted.stdout, adopted.stdout
assert os.stat(image).st_mtime_ns != before
assert exported_chat() == chat

# Exit status follows MAIN, for the binary and for an image.
assert run(['--color', 'bogus']).returncode == 1
assert run(['--color', 'bogus'], program=image).returncode == 1

print('Checkpoint, restart, lock, --ephemeral, --print, --export-chat, and --adopt-chat passed.')
server.shutdown()
# The saved images are large.
shutil.rmtree(scratch, ignore_errors=True)
