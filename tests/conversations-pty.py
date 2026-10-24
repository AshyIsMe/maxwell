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
            time.sleep(.5)
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



# Named conversations, independent background work, and verified restart.
try:
    session = Session(provider)
    session.expect(b'maxwell> ')
    session.send(b'/chat new review' + ENTER)
    session.expect(b'Conversation review')
    session.expect(b'maxwell> ')
    session.send(b'/effort low' + ENTER)
    session.expect(b'Thinking effort: low')
    session.expect(b'maxwell> ')
    session.send(b'/chat main' + ENTER)
    session.expect(b'Conversation main')
    session.expect(b'maxwell> ')
    session.send(b'parent-private' + ENTER)
    session.expect(b'steer> ')
    session.send(b'/chat review' + ENTER)
    session.expect(b'Conversation review')
    session.expect(b'maxwell> ')
    time.sleep(1)
    session.send(b'child-private' + ENTER)
    session.expect(b'noted-2')
    session.expect(b'maxwell> ')
    assert len(requests) == 2, len(requests)
    child_view = requests[1]['messages'][1]['content']
    assert 'parent-private' not in child_view, child_view
    assert requests[1].get('reasoning_effort') == 'low', requests[1]
    session.send(b'/chat note main non-waking-important' + ENTER)
    session.expect(b'maxwell> ')
    time.sleep(.3)
    assert len(requests) == 2, len(requests)
    session.send(b'/chat main' + ENTER)
    session.expect(b'Conversation main')
    session.expect(b'maxwell> ')
    session.send(b'continue-parent' + ENTER)
    session.expect(b'noted-3')
    session.expect(b'maxwell> ')
    assert 'non-waking-important' in requests[2]['messages'][1]['content']
    assert 'child-private' not in requests[2]['messages'][1]['content']
    assert not requests[2].get('reasoning_effort'), requests[2]
    session.send(b'/chat run review delegated-question' + ENTER)
    session.expect(b'maxwell> ')
    time.sleep(1)
    session.send(b'collect-result' + ENTER)
    session.expect(b'noted-5')
    session.expect(b'maxwell> ')
    assert 'Result from review' in requests[4]['messages'][1]['content'], requests[4]
    session.send(b'/chat fork alternative' + ENTER)
    session.expect(b'Conversation alternative')
    session.expect(b'maxwell> ')
    session.send(b'/checkpoint' + ENTER)
    session.expect(b'Chat saved to', timeout=40)
    session.expect(b'maxwell> ')
    session.send(b'/restart' + ENTER)
    session.expect(b'Building and verifying', timeout=40)
    session.expect(b'Verified. Restarting', timeout=120)
    session.expect(b'maxwell> ', timeout=40)
    session.send(b'/chat' + ENTER)
    session.expect(b'alternative')
    session.expect(b'maxwell> ')
    assert os.path.isfile(image + '.rollback')
    assert stat.S_IMODE(os.stat(image + '.rollback').st_mode) == 0o700
    session.send(b'/chat review' + ENTER)
    session.expect(b'Conversation review')
    session.expect(b'maxwell> ')
    session.send(b'/effort' + ENTER)
    session.expect(b'Thinking effort: low')
    session.expect(b'maxwell> ')
    session.send(b'/quit' + ENTER)
    assert session.wait(timeout=40) == 0
    session.close()
    # The global image itself keeps the registry and selected model after exec.
    session = Session()
    session.expect(b'maxwell> ')
    session.send(b'/chat' + ENTER)
    session.expect(b'alternative')
    session.expect(b'maxwell> ')
    session.send(b'/quit' + ENTER)
    assert session.wait(timeout=40) == 0
    session.close()
    invalid = os.path.join(scratch, 'bad-state.sexp')
    Path(invalid).write_text('(:maxwell-state :version 999)')
    assert run(['--verify-resume', invalid]).returncode != 0
    Path(invalid).write_text('#.(error "must not evaluate")')
    assert run(['--verify-resume', invalid]).returncode != 0
    print('Conversation switching, private context/model, notes, delegated results, checkpoint and verified restart passed.')
finally:
    if session.pid:
        session.close()
    server.shutdown()
    shutil.rmtree(scratch, ignore_errors=True)
