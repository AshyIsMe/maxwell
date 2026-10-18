"""Exercise transcript browsing, Ctrl-T, and steering while the model works:
python3 tests/terminal-pty-steering.py."""
import fcntl
import http.server
import json
import os
import pty
import select
import shutil
import signal
import socketserver
import struct
import tempfile
import termios
import threading
import time
from pathlib import Path

executable = str(Path(__file__).resolve().parents[1] / 'maxwell')
thinking_seconds = 1.5
ENTER = bytes([13])
ESCAPE = bytes([27])
CTRL_T = bytes([20])
TOOL_CODE = '(defun twice (x)\n  (* 2 x))\n(twice 21)'
requests = []
scratch = tempfile.mkdtemp(prefix='maxwell-pty-steering-')


def last_user_text(request):
    users = [m for m in request.get('messages', []) if m.get('role') == 'user']
    return str(users[-1].get('content')) if users else ''


def reply(handler, payload, status=200):
    body = json.dumps(payload).encode()
    handler.send_response(status)
    handler.send_header('Content-Type', 'application/json')
    handler.send_header('Content-Length', str(len(body)))
    handler.end_headers()
    handler.wfile.write(body)


class Provider(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        length = int(self.headers.get('Content-Length', 0))
        request = json.loads(self.rfile.read(length))
        requests.append(request)
        # Stay thinking long enough for the test to press keys meanwhile.
        time.sleep(thinking_seconds)
        # Only the user's latest words decide: the system prompt mentions
        # failure too.
        text = last_user_text(request)
        if 'fail' in text:
            reply(self, {'error': {'message': 'deliberate failure'}}, 500)
            return
        ran_tool = any(m.get('role') == 'tool' for m in request.get('messages', []))
        if 'run tool' in text and not ran_tool:
            call = {'id': 'call-1', 'type': 'function',
                    'function': {'name': 'common_lisp',
                                 'arguments': json.dumps({'code': TOOL_CODE})}}
            message = {'role': 'assistant', 'content': None, 'tool_calls': [call]}
        elif 'run tool' in text:
            message = {'role': 'assistant', 'content': 'tool-done'}
        else:
            message = {'role': 'assistant', 'content': 'reply-' + str(len(requests))}
        reply(self, {'choices': [{'message': message}]})

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
    # Never touch the developer's real remembered session or image.
    os.environ['MAXWELL_SESSION_FILE'] = os.path.join(scratch, 'session.json')
    os.environ['MAXWELL_IMAGE'] = os.path.join(scratch, 'maxwell.image')
    os.environ['XDG_DATA_HOME'] = os.path.join(scratch, 'data')
    # Plain text keeps the markers below contiguous.
    os.environ['NO_COLOR'] = '1'
    os.execv(executable, [executable, '--base-url', base_url, '--model', 'pty-model',
                          '--compact-model', 'pty-compact'])

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
    """The user's words in REQUEST: each turn starts with the view, then the
    turn state followed by the message."""
    users = [m.get('content') for m in request['messages'] if m.get('role') == 'user']
    assert users[0].startswith('<chat>'), users
    assert users[1].startswith('Date: '), users
    return [users[1].rsplit('\n\n', 1)[-1]] + users[2:]


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

    # A tool call is drawn as a summary block.
    send(b'run tool' + ENTER)
    expect('• lisp (defun twice (x) (* 2 x)) …'.encode())
    expect('  └ 42'.encode())
    expect(b'+2 lines (ctrl-t to expand)')
    expect(b'steer> ')
    # Ctrl-T while the model still works redraws the screen with the whole
    # code, then brings the steering line back.
    send(CTRL_T)
    expect(b'\x1b[H\x1b[2J\x1b[3J')
    expect(b'    (defun twice (x)')
    expect(b'      (* 2 x))')
    expect(b'    (twice 21)')
    expect(b'steer> ')
    expect(b'tool-done')
    expect(b'maxwell> ')

    # In the viewer, Ctrl-T collapses the block again; leaving redraws the
    # main screen in the new mode.
    send(ESCAPE + b'[5~')
    expect(b'Ctrl-T collapse tools')
    send(CTRL_T)
    expect(b'+2 lines (ctrl-t to expand)')
    expect(b'Ctrl-T expand tools')
    send(ESCAPE)
    expect(b'\x1b[3J')
    expect(b'+2 lines (ctrl-t to expand)')
    expect(b'maxwell> ')

    send(b'/quit' + ENTER)
    # Quitting saves the chat to the (scratch) global image first.
    deadline = time.monotonic() + 30
    while time.monotonic() < deadline:
        finished, status = os.waitpid(pid, os.WNOHANG)
        if finished:
            assert os.waitstatus_to_exitcode(status) == 0
            break
        time.sleep(0.05)
    else:
        raise AssertionError('Maxwell did not exit')

    print('Transcript browsing, Ctrl-T, and steering during a model request passed.')
finally:
    try:
        os.kill(pid, signal.SIGKILL)
        os.waitpid(pid, 0)
    except ProcessLookupError:
        pass
    os.close(terminal)
    server.shutdown()
    # Quitting saved the chat to a large image in the scratch directory.
    shutil.rmtree(scratch, ignore_errors=True)
