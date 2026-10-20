"""Exercise the live reasoning trace: a provider that streams its thinking
shows it above the prompt while the model works, and it goes away as soon as the
answer arrives: python3 tests/thinking-pty.py."""
import os
import fcntl
import struct
import tempfile
import threading
import time
from pathlib import Path
import pty
import select
import signal
import termios
from http.server import BaseHTTPRequestHandler, HTTPServer

executable = str(Path(__file__).resolve().parents[1] / 'maxwell')
scratch = tempfile.mkdtemp(prefix='maxwell-thinking-pty-')

REASONING = 'Let me think about it carefully.'
ANSWER = 'Hello world'

# The provider streams reasoning first, slowly enough to be seen, then the
# answer, as an OpenAI-compatible Chat Completions event stream.
STREAM = [
    b'data: {"choices":[{"delta":{"reasoning_content":"Let me think "}}]}\n\n',
    b'data: {"choices":[{"delta":{"reasoning_content":"about it "}}]}\n\n',
    b'data: {"choices":[{"delta":{"reasoning_content":"carefully."}}]}\n\n',
    b'data: {"choices":[{"delta":{"content":"Hello "}}]}\n\n',
    b'data: {"choices":[{"delta":{"content":"world"},"finish_reason":"stop"}]}\n\n',
    b'data: [DONE]\n\n',
]


class Handler(BaseHTTPRequestHandler):
    def do_POST(self):
        self.rfile.read(int(self.headers.get('Content-Length', 0)))
        self.send_response(200)
        self.send_header('Content-Type', 'text/event-stream')
        self.end_headers()
        for index, chunk in enumerate(STREAM):
            if index < len(STREAM) - 1:
                # Each piece arrives while the model is still working, so the
                # trace is drawn before the answer begins.
                time.sleep(0.6)
            self.wfile.write(chunk)
            self.wfile.flush()

    def log_message(self, *arguments):
        pass


server = HTTPServer(('127.0.0.1', 0), Handler)
threading.Thread(target=server.serve_forever, daemon=True).start()
base_url = 'http://127.0.0.1:%d/v1' % server.server_address[1]


def launch():
    pid, terminal = pty.fork()
    if pid == 0:
        fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack('HHHH', 24, 80, 0, 0))
        os.environ['TERM'] = 'xterm'
        os.environ.pop('NO_COLOR', None)
        os.environ.pop('OPENAI_API_KEY', None)
        os.environ['MAXWELL_SESSION_FILE'] = os.path.join(scratch, 'session.json')
        os.environ['MAXWELL_IMAGE'] = os.path.join(scratch, 'maxwell.image')
        os.environ['XDG_DATA_HOME'] = os.path.join(scratch, 'data')
        os.environ['MAXWELL_BASE_URL'] = base_url
        os.environ['MAXWELL_MODEL'] = 'test-model'
        os.execv(executable, [executable, '--ephemeral'])
    return pid, terminal


class Session:
    def __init__(self):
        self.pid, self.terminal = launch()
        self.seen = b''

    def read(self, seconds):
        deadline = time.monotonic() + seconds
        while time.monotonic() < deadline:
            if select.select([self.terminal], [], [], 0.1)[0]:
                try:
                    data = os.read(self.terminal, 65536)
                except OSError:
                    break
                if not data:
                    break
                self.seen += data
        return self.seen

    def send(self, data):
        os.write(self.terminal, data)

    def close(self):
        try:
            os.kill(self.pid, signal.SIGKILL)
            os.waitpid(self.pid, 0)
        except (ProcessLookupError, ChildProcessError):
            pass
        os.close(self.terminal)


session = Session()
try:
    session.read(3)
    # The provider and model are on the status row above the prompt.
    assert b'127.0.0.1/test-model  idle' in session.seen, session.seen[-200:]
    marked = len(session.seen)

    session.send(b'hi\r')
    seen = session.read(8)
    stream = seen[marked:]
    reasoning = stream.find(REASONING.encode())
    answer = stream.find(ANSWER.encode())
    assert reasoning >= 0, stream[-400:]
    assert answer > reasoning, (reasoning, answer)
    # The trace is drawn dim and italic, above the status row, which says the
    # model is thinking.
    assert b'\x1b[2;3mLet me think' in stream, stream[:400]
    assert b'thinking' in stream[:reasoning + 20], stream[:reasoning + 20]
    # When the reply arrives the trace and its row go away, leaving the answer
    # in the transcript; later the status row shows the model is idle again.
    assert b'\x1b[2A\r\x1b[J' in stream[reasoning:answer], stream[reasoning:answer]
    assert b'127.0.0.1/test-model  idle' in stream[answer:], stream[answer:]
    assert b'maxwell> ' in stream[answer:], stream[answer:]
    print('Live reasoning traces, auto-hiding when the model answers, passed.')
finally:
    session.close()
