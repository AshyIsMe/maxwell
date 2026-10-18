"""Exercise transcript browsing and steering while the model works:
python3 tests/terminal-pty-steering.py."""
import fcntl
import http.server
import json
import os
import pty
import select
import signal
import socketserver
import struct
import termios
import threading
import time
from pathlib import Path

executable = str(Path(__file__).resolve().parents[1] / 'maxwell')
thinking_seconds = 1.5
ENTER = bytes([13])
ESCAPE = bytes([27])
requests = []


class Provider(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        length = int(self.headers.get('Content-Length', 0))
        request = json.loads(self.rfile.read(length))
        requests.append(request)
        # Stay thinking long enough for the test to press keys meanwhile.
        time.sleep(thinking_seconds)
        text = ' '.join(str(m.get('content')) for m in request.get('messages', []))
        if 'fail' in text:
            payload = json.dumps({'error': {'message': 'deliberate failure'}}).encode()
            self.send_response(500)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Content-Length', str(len(payload)))
            self.end_headers()
            self.wfile.write(payload)
            return
        payload = json.dumps({
            'choices': [{'message': {'role': 'assistant',
                                     'content': 'reply-' + str(len(requests))}}],
        }).encode()
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(payload)))
        self.end_headers()
        self.wfile.write(payload)

    def log_message(self, *arguments):
        pass


class Server(socketserver.ThreadingTCPServer):
    allow_reuse_address = True
    daemon_threads = True


server = Server(('127.0.0.1', 0), Provider)
threading.Thread(target=server.serve_forever, daemon=True).start()
base_url = 'http://127.0.0.1:' + str(server.server_address[1]) + '/v1'

pid, terminal = pty.fork()
if pid == 0:
    fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack('HHHH', 24, 80, 0, 0))
    os.environ['TERM'] = 'xterm'
    # Never touch the developer's real remembered session.
    os.environ['MAXWELL_SESSION_FILE'] = '/tmp/maxwell-pty-steering-session.json'
    os.execv(executable, [executable, '--base-url', base_url, '--model', 'pty-model'])

pending = b''


def expect(marker, timeout=10):
    global pending
    output = pending
    pending = b''
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if marker in output:
            pending = output.split(marker, 1)[1]
            return
        if select.select([terminal], [], [], 0.1)[0]:
            output += os.read(terminal, 65536)
    raise AssertionError('Timed out waiting for ' + repr(marker) + ': ' + repr(output))


def send(data):
    os.write(terminal, data)


def steering_messages(request):
    return [m.get('content') for m in request['messages'] if m.get('role') == 'user']


try:
    expect(b'maxwell> ')

    # Start a request, then use the terminal while the model is still working.
    send(b'hello' + ENTER)
    expect(b'steer> ')

    # PgUp/PgDn browse the transcript without losing the steering prompt.
    send(ESCAPE + b'[5~')
    expect(b'PgUp/PgDn scroll')
    send(ESCAPE + b'[6~')
    expect(b'steer> ')

    # A prompt typed during the request is delivered to the model.
    send(b'be brief' + ENTER)
    expect(b'steer> be brief')
    expect(b'reply-1')
    expect(b'reply-2')
    expect(b'maxwell> ')

    assert len(requests) == 2, requests
    assert steering_messages(requests[0]) == ['hello'], requests[0]
    assert steering_messages(requests[1]) == ['hello', 'be brief'], requests[1]

    # A provider failure during a request still returns a usable prompt.
    send(b'please fail' + ENTER)
    expect(b'Error: Provider request failed')
    expect(b'maxwell> ')

    send(b'/quit' + ENTER)
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        finished, status = os.waitpid(pid, os.WNOHANG)
        if finished:
            assert os.waitstatus_to_exitcode(status) == 0
            break
        time.sleep(0.05)
    else:
        raise AssertionError('Maxwell did not exit')

    print('Transcript browsing and steering during a model request passed.')
finally:
    try:
        os.kill(pid, signal.SIGKILL)
        os.waitpid(pid, 0)
    except ProcessLookupError:
        pass
    os.close(terminal)
    server.shutdown()
