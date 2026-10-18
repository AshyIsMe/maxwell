# Maxwell

Maxwell is a deliberately small AI coding-agent harness written in Common Lisp.
Like [pi](https://pi.dev/), it keeps the important mechanism visible: a model,
a conversation, one tool, and a loop. Unlike a shell-oriented agent, Maxwell's
only model-visible tool is a live Common Lisp evaluator.

Every tool call runs in the same SBCL process and the same
`MAXWELL/WORKSPACE` package. Definitions, loaded ASDF systems, caches, and any
other Lisp state survive from one call to the next. SBCL can checkpoint the
whole harness—including the conversation and that live state—as a resumable
executable image.

## Run it

Maxwell has no Lisp library dependencies beyond ASDF/UIOP, which ships with
SBCL. HTTP requests use `curl` internally; `curl` is not exposed as an agent
tool. Native ChatGPT OAuth additionally uses `openssl` for PKCE and ID-token
signature verification.

```sh
export OPENAI_API_KEY=...
export MAXWELL_MODEL=gpt-5
sbcl --script maxwell.lisp
```

It uses the broadly supported OpenAI chat-completions wire protocol. Point it at
any compatible service with `MAXWELL_BASE_URL`; if that service reads a
differently named key, set `MAXWELL_API_KEY_ENV` to the name of its environment
variable. `MAXWELL_ENDPOINT` can override the complete endpoint URL when a
gateway does not follow the usual `/v1/chat/completions` layout.

Chat Completions is the default because it is the widest compatibility target.
For OpenAI models that require the Responses API, including Responses-only
Codex models, add `--wire-api responses` (or set `MAXWELL_WIRE_API=responses`).
The model/tool loop remains the same, and Maxwell carries response reasoning
items through subsequent tool calls.

For example, OpenRouter can route Maxwell to Claude, Codex, Gemini, or another
tool-capable model:

```sh
export OPENROUTER_API_KEY=...
sbcl --script maxwell.lisp \
  --base-url https://openrouter.ai/api/v1 \
  --api-key-env OPENROUTER_API_KEY \
  --model anthropic/claude-sonnet-4
```

Direct Claude access also exposes an OpenAI-compatible endpoint:

```sh
export ANTHROPIC_API_KEY=...
sbcl --script maxwell.lisp \
  --base-url https://api.anthropic.com/v1 \
  --api-key-env ANTHROPIC_API_KEY \
  --model claude-sonnet-4-6
```

If the local Pi configuration already contains the Safe AI Labs provider, the
included launcher resolves its endpoint, model, and credential without printing
or copying the key into the command line:

```sh
./run.sh
```

The launcher understands the same `apiKey` forms used by Pi's `models.json`:
literal values, `$NAME`/`${NAME}` environment references, and leading
`!command` credential resolvers. Additional Maxwell arguments pass through, for
example `./run.sh --print "Inspect this project"`.

Use the default `chat` wire API for this provider; the Pi and OMP configurations
both identify it as `openai-completions`. The configured model has a 1,048,576
token context window and a 65,536-token maximum output.

To use the existing ChatGPT/Codex OAuth login instead, run:

```sh
./run-codex.sh
./run-codex.sh --print "Inspect this project"
```

Sign in once with Maxwell itself, either from the shell or its interactive
prompt:

```sh
./run-codex.sh --login
# or, inside Maxwell:
/login openai
```

This is the OAuth 2.0 Authorization Code flow with PKCE and OpenID Connect. It
opens Sign in with ChatGPT, receives the redirect on a temporary loopback
listener, validates the ID token, and stores refreshable credentials at
`~/.config/maxwell/openai-auth.json` with owner-only permissions. Override that
path with `MAXWELL_AUTH_FILE`.

When Maxwell detects SSH, login automatically switches to manual callback
mode. Open the printed URL on your local computer. After approval, the browser
may fail to connect to `127.0.0.1`; copy its **entire final URL** from the
address bar and paste that URL into Maxwell. Do not paste only the `code`, since
Maxwell also validates the returned state and issued client ID. Set
`MAXWELL_OAUTH_MODE=manual` or `MAXWELL_OAUTH_MODE=loopback` to override
automatic detection.

OpenAI's stricter documented workflow for a self-hosted VM is to complete login
locally and transfer the protected credential file over SSH. The paste flow is
a convenient SSH fallback that retains PKCE and state validation.

`run-codex.sh` is now only a provider preset. Maxwell refreshes the saved token
itself and uses the streaming Responses API with `store: false`; it does not
depend on Pi. The default model is `gpt-5.6-sol`; override it with
`MAXWELL_CODEX_MODEL`. Provider-specific fields remain available, for example:

```sh
./run-codex.sh --request-json '{"reasoning":{"effort":"high"}}'
```

For security, OAuth mode always forces the public OpenAI endpoint, the
Responses wire API, `store: false`, and `stream: true`, regardless of request
overrides.

Inside Maxwell, `/model` fetches the current account/provider catalog and shows
the selected model. Use `/model MODEL` to switch, for example:

```text
/model
/model gpt-6.1-sol
```

### Remembering the last provider and model

Maxwell remembers the provider configuration and model it last used, so a bare
invocation continues with the same service and model:

```sh
./run.sh --print "Inspect this project"   # Safe AI Labs provider
maxwell                                   # the same provider and model
./run-codex.sh --login                    # ChatGPT-plan provider
maxwell                                   # ChatGPT plan, Codex model
```

The record lives at `~/.config/maxwell/session.json` (override it with
`MAXWELL_SESSION_FILE`). Maxwell writes it whenever a session starts, when
`/model MODEL` selects a model, and after `/login openai`. It holds the base
URL, endpoint, wire API, credential variable name, OAuth provider, extra
headers, and model.

Command-line options and `MAXWELL_*` environment variables always win. Naming a
provider explicitly—`--base-url`, `--endpoint`, `--wire-api`, `--api-key-env`,
`--auth`, `--header`, or the matching environment variable—starts a fresh
configuration instead of reusing the remembered one, so `--model` alone keeps
the remembered provider while `--base-url` replaces it. Credentials are never
copied into the record: they still come from the environment variable the
provider names, or from the saved OAuth login. Delete `session.json` to forget
the last session; if it cannot be written, Maxwell simply starts without a
remembered default.

Responses requests with `"stream":true` are read as server-sent events.
Maxwell waits for `response.completed`, then feeds that complete response into
the same tool loop used for non-streaming responses. Failed, incomplete, or
truncated streams are reported as provider errors.

Use repeated `--header 'Name: value'` flags for gateway-specific headers. Use
`--request-json '{"temperature":0.2}'` for provider-specific request fields;
Maxwell always supplies the model, conversation, and tools itself. A compatible
model must support function/tool calling for the coding loop to operate.

One-shot mode:

```sh
sbcl --script maxwell.lisp --print "Explain this project"
```

Build a standalone executable and run the tests:

```sh
make test
make build
./maxwell --model gpt-5
```

Terminal behaviour is covered by two pty tests that drive the built executable:
`python3 tests/terminal-pty.py` for editing, history, and settings restoration,
and `python3 tests/terminal-pty-steering.py` for browsing and steering during a
request (it serves a deliberately slow fake provider on a local port).

## Images

At the prompt, `/save maxwell.image` writes a resumable executable and exits.
Start that file later to continue with the same conversation and Lisp state:

```sh
./maxwell.image
```

The agent itself can schedule the same operation by evaluating:

```lisp
(maxwell:request-image "maxwell.image")
```

The save happens only after the model finishes its current run, avoiding a
half-recorded tool call. The configured API key is read from its environment
variable for each request and is not stored in the image. Custom header values
*are* part of client state, so do not put secrets in `--header` if you intend to
save an image.

## Deliberate limits

- One provider family: OpenAI-compatible APIs.
- Two wire shapes: compatible Chat Completions by default, Responses when
  explicitly selected.
- One model-visible tool: `common_lisp`.
- One linear, in-memory conversation. Persist it with the Lisp image.
- No sandbox. Evaluated Lisp has the permissions of the Maxwell process.
- Tool calls are sequential because they mutate one Lisp world.

These constraints are the design. The code is small enough for Maxwell to
inspect and adapt itself using its own Lisp tool.

### Transcript navigation

At the interactive prompt, press **Page Up** to open conversation history,
including tool calls and results. **Page Up / Page Down** move one page at a
time; **Escape** returns to the prompt. Page Down past the newest page also
returns. Your unfinished input is preserved. Left/Right, Home/End, Backspace,
and Delete edit the prompt; long input scrolls horizontally.

This uses an ANSI-compatible terminal and `stty` with `/dev/tty` (currently
Linux). Redirected input/output and `TERM=dumb` retain ordinary line input.
The viewer shows conversation messages, not every CLI status line, and terminal
dimensions are read when it opens.

### Steering the model while it works

While a request is in flight the prompt stays live. A `steer> ` line marks where
you can type; pressing Enter sends that text to the model before its next
request, so you can redirect work already under way:

```text
maxwell> Refactor the parser.
[common-lisp] ...
steer> actually, keep the old API
```

The model finishes the answer it was writing, then reads your prompt and
continues in the same turn. **Page Up / Page Down** open the transcript viewer
during a request too, and a half-typed steering line is carried into the next
prompt instead of being discarded.

Steering applies while Maxwell waits for the provider and between tool calls. A
single long tool call runs with the Lisp world locked, so keys pressed during it
are handled as soon as it finishes — they wait in the terminal buffer and are
not lost.

This is done by running `curl` asynchronously and polling the terminal while it
runs, rather than by evaluating tool calls on a second thread. Tool calls keep
running on the one thread that owns the agent's Lisp world.
