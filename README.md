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
signature verification. To skip API-key variables entirely, see
[Signing in](#signing-in).

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

### Signing in

Maxwell can save a login for two providers, so that no API-key variable or
launcher script is needed. Sign in once from the shell or the interactive
prompt; Maxwell verifies the credential, stores it with owner-only permissions,
and remembers the provider for the next session:

```sh
maxwell --login safeai     # Safe AI Labs
maxwell --login openai     # ChatGPT plan
# or, inside Maxwell:
/login safeai
/login openai
```

`--auth PROVIDER` (`-a`) selects a saved login explicitly, for example
`maxwell --auth openai --print "Inspect this project"`. `--logout PROVIDER` or
`/logout PROVIDER` forgets it.

| Provider | Credential file (owner-only)         | Override                   |
|----------|--------------------------------------|----------------------------|
| `safeai` | `~/.config/maxwell/safeai-auth.json` | `MAXWELL_SAFEAI_AUTH_FILE` |
| `openai` | `~/.config/maxwell/openai-auth.json` | `MAXWELL_AUTH_FILE`        |

Both live under `$XDG_CONFIG_HOME/maxwell/` when that variable is set.
Credentials are never written to `session.json` or to saved images.

#### Safe AI Labs

Safe AI Labs does not offer OAuth to third-party clients; its own web sign-in
issues keys that work only from its Sail CLI. `--login safeai` is therefore a
browser-assisted API-key login:

1. Maxwell opens <https://safeailabs.ai/keys>, where you sign in and create an
   API key (it starts with `sail_`). Over SSH, or with
   `MAXWELL_OAUTH_MODE=manual`, it prints the URL instead of opening a browser.
2. Paste the key at the prompt. Input is hidden on an interactive terminal.
3. Maxwell checks the key with `GET /v1/models`, then saves it together with the
   endpoint and model to use.

A rejected key (`invalid_api_key`) means it was mistyped or revoked. A key that
Safe AI Labs reports as `sail_key_requires_sail` belongs to the Sail CLI; create
an API key on the keys page instead.

The provider is OpenAI-compatible Chat Completions at
`https://us-region.api.safeailabs.ai/v1`. Settings:

- `SAFEAILABS_API_KEY` — when set, used instead of the saved key.
- `MAXWELL_SAFEAI_BASE_URL` — another endpoint, such as the global
  `https://gbl.api.safeailabs.ai/v1`.
- `MAXWELL_SAFEAI_MODEL` — the model; by default the first one the key's
  `/v1/models` catalog lists at login (for example
  `deepseek/deepseek-v4.1-flash`, with a 1,048,576-token context window).

#### ChatGPT plan (OpenAI OAuth)

`--login openai` is the OAuth 2.0 Authorization Code flow with PKCE and OpenID
Connect. It opens Sign in with ChatGPT, receives the redirect on a temporary
loopback listener, validates the ID token, and stores refreshable credentials.
Maxwell refreshes the saved token itself.

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

`--auth openai` alone configures the whole provider: the streaming Responses API
with `store: false`. The default model is `gpt-5.6-sol`; override it with
`--model` or `MAXWELL_CODEX_MODEL`. Provider-specific fields remain available,
for example:

```sh
maxwell --auth openai --request-json '{"reasoning":{"effort":"high"}}'
```

For security, OAuth mode always forces the public OpenAI endpoint, the
Responses wire API, `store: false`, and `stream: true`, regardless of request
overrides.

API keys and OAuth tokens reach `curl` through a short-lived owner-only header
file rather than its command line, so other local users cannot read them with
`ps`.

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
maxwell --login safeai                    # Safe AI Labs
maxwell                                   # the same provider and model
maxwell --auth openai --print "Inspect"   # ChatGPT plan, Codex model
maxwell                                   # ChatGPT plan again
```

The record lives at `~/.config/maxwell/session.json` (override it with
`MAXWELL_SESSION_FILE`). Maxwell writes it whenever a session starts, when
`/model MODEL` selects a model, and after `/login`. It holds the base URL,
endpoint, wire API, credential variable name, login provider, extra headers,
and model.

Command-line options and `MAXWELL_*` environment variables always win. Naming a
provider explicitly—`--base-url`, `--endpoint`, `--wire-api`, `--api-key-env`,
`--auth`, `--header`, or the matching environment variable—starts a fresh
configuration instead of reusing the remembered one, so `--model` alone keeps
the remembered provider while `--base-url` replaces it. A provider's own
variables (`MAXWELL_CODEX_MODEL`, `MAXWELL_SAFEAI_MODEL`,
`MAXWELL_SAFEAI_BASE_URL`) outrank the generic ones for that provider.
Credentials are never copied into the record: they still come from the
environment variable the provider names, or from the saved login. Delete
`session.json` to forget the last session; if it cannot be written, Maxwell
simply starts without a remembered default.

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
variable, or the saved login from its credential file, for each request and is
not stored in the image. Custom header values
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
