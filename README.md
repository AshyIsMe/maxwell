# Maxwell

Maxwell is a deliberately small AI coding-agent harness written in Common Lisp.
Like [pi](https://pi.dev/), it keeps the important mechanism visible: a model,
a conversation, one tool, and a loop. Unlike a shell-oriented agent, Maxwell's
only model-visible tool is a live Common Lisp evaluator.

Every tool call runs in the same SBCL process and the same
`MAXWELL/WORKSPACE` package. Definitions, loaded ASDF systems, caches, and any
other Lisp state survive from one call to the next. The conversation is one
chat that never ends, across every project, and it lives with that Lisp state
in a single executable SBCL image that Maxwell checkpoints itself into.

## Install

Maxwell builds to a single self-contained executable, so installing it is a copy:

```sh
make build      # compiles everything into ./maxwell
make install    # copies it to ~/.local/bin
```

`make install` honours `PREFIX` (default `$(HOME)/.local`), `BINDIR` (default
`$(PREFIX)/bin`), and `DESTDIR` for staged packaging:

```sh
make install PREFIX=/usr/local
```

`make uninstall` removes it. The executable embeds an SBCL core, Maxwell
itself, and its two vendored libraries, so there is nothing else to install and
no Quicklisp, Roswell, or system package is needed.

You can also run a checkout directly, without building a binary:

```sh
sbcl --script maxwell.lisp
```

## Run it

Maxwell needs only SBCL: ASDF/UIOP ship with it, and its two Lisp libraries,
[Alexandria](https://alexandria.common-lisp.dev/) and
[FSet](https://github.com/slburson/fset), are vendored in the repository (see
[Vendored libraries](#vendored-libraries)). Nothing is fetched at build or run
time, and Quicklisp is not needed. HTTP requests use `curl` internally; `curl` is not exposed as an agent
tool. Native ChatGPT OAuth additionally uses `openssl` for PKCE and ID-token
signature verification. To skip API-key variables entirely, see
[Signing in](#signing-in).

```sh
export OPENAI_API_KEY=...
export MAXWELL_MODEL=gpt-5
sbcl --script maxwell.lisp
```

Maxwell's workspace is the directory it starts in: the Lisp tool runs there,
chat lines are tagged with the project that workspace belongs to (its git root,
or the directory name), and its `AGENTS.md` is read into memory.
`-C, --workspace DIR` sets it explicitly, so a one-shot run can name the
project:

```sh
maxwell --workspace ~/src/thing --print "Explain this project"
```

`maxwell --help` lists every option and environment variable.

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
model, and thinking effort.

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

Some models take a thinking-effort level. `/effort` shows the current one;
`/effort LEVEL` sets it to `none`, `minimal`, `low`, `medium`, or `high`, and
`/effort default` clears it. It reaches the provider in that provider's own
spelling—a `reasoning` object on Responses, `reasoning_effort` on Chat
Completions—and is remembered in `session.json` with the model. Memory
summaries never ask for an effort: they are mechanical. A provider that takes
other levels can be given one with `--request-json`.

Responses requests with `"stream":true` are read as server-sent events.
Maxwell waits for `response.completed`, then feeds that complete response into
the same tool loop used for non-streaming responses. Failed, incomplete, or
truncated streams are reported as provider errors.

Memory summaries are written by the same provider. `--compact-model MODEL` (or
`MAXWELL_COMPACT_MODEL`) picks a cheaper model for them; it is remembered with
the provider. Without it Maxwell uses the main model and says once that this
may cost more.

Use repeated `--header 'Name: value'` flags for gateway-specific headers. Use
`--request-json '{"temperature":0.2}'` for provider-specific request fields;
Maxwell always supplies the model, conversation, and tools itself. A compatible
model must support function/tool calling for the coding loop to operate.

One-shot mode continues the same chat and saves it; `--ephemeral` gives a
throwaway chat instead, for scripting:

```sh
sbcl --script maxwell.lisp --print "Explain this project"
maxwell --ephemeral --print "What is 2 + 2?"
```

Build a standalone executable and run the tests:

```sh
make test
make build
./maxwell --model gpt-5
```

Terminal behaviour is covered by pty tests that drive the built executable:
`python3 tests/terminal-pty.py` for editing, history, Ctrl-T, colour, and
settings restoration; `python3 tests/terminal-pty-steering.py` for browsing and
steering during a request (it serves a deliberately slow fake provider on a
local port); and `python3 tests/checkpoint-pty.py` for saving, restarting, the
lock, and moving the chat between builds. All of them keep their images,
sessions, and logins in a temporary directory.

### Vendored libraries

`vendor/` holds pinned copies of Alexandria and FSet, with FSet's own
dependencies (misc-extensions, mt19937, named-readtables, and the
mgl-pax-bootstrap part of mgl-pax). `maxwell.asd` puts them at the front of
ASDF's central registry, so `make build`, `sbcl --script maxwell.lisp`, and
`/reload` always use these copies, whatever else is installed, and leave your
own ASDF configuration alone.

Inside Maxwell, and in the model's `common_lisp` calls, they are reached
through package-local nicknames: `a:` for Alexandria and `f:` for FSet
(`(f:with (f:empty-map) :k 1)`). FSet shadows many CL names such as `find`,
`remove`, and `union`, so neither package `:use`s it.

To update, change a pin in `vendor/fetch.sh` (each is a commit, with its tag in
a comment), run the script, and review the diff. FSet is pinned from upstream
rather than from the Quicklisp dist, which lags behind and lacks the
misc-extensions version FSet 2.5 requires.

## Memory: one endless chat

Maxwell keeps every message—yours, its replies, its tool calls and their
results—in an append-only log, following VictorTaelin's
[UniiChat/OptMem design](https://gist.github.com/VictorTaelin/91837951a5ce5b38f341ec1ba1df6449).
A background compactor folds the log into a binary tree of one-line summaries
of at most 512 bytes. Each turn shows the model a *view* of 64–128 KB: recent
messages one per line, older ones merged, every line tagged with its project
(the git root of the workspace it came from):

```text
<chat>
0+64|[maxwell] user: asked for colour ...; maxwell: added style.lisp ...
...
1280+1|[dotfiles] user: switch the prompt to fish
</chat>
```

A turn is `[system prompt][view][turn state + your message]`. The system prompt
and the old part of the view do not change from turn to turn, so providers can
cache them. No other conversation carries over: within a turn the model sees
whole tool results, but the log keeps at most 30,000 characters of each.

The model reads further through its one tool: `(maxwell:zoom id n)` opens line
`id+n` into the two lines it was made from, `(maxwell:zoom id 1)` returns
message `id` whole, and `(maxwell:date id)` gives its time. A turn waits, with
a spinner on the steering line, until everything before your message has a
summary; Ctrl-C sends it without waiting. If summaries cannot be built, for
example because the provider is failing, the turn goes ahead with the view so
far and says so.

Summary requests run as background `curl` processes, up to 8 at once, polled
between keystrokes and between tool calls. A project's `AGENTS.md` is read into
memory at the first turn in it, and again whenever it changes. `/import PATH`
does the same for any file. `/memory` shows the message count, the tree, the
view's size, and the compactor's jobs.

## The global image

The chat lives in one executable image,
`${XDG_DATA_HOME:-~/.local/share}/maxwell/maxwell.image` (override it with
`MAXWELL_IMAGE`). The built `./maxwell`, `sbcl --script maxwell.lisp`, or any
other image starts by exec'ing it with the same arguments, so every session
continues the same chat and Lisp state; until it exists, Maxwell starts empty.
`--fresh` runs the binary or image you named instead; when it saves, its chat
replaces the global one.

Maxwell saves itself on `/quit`, Ctrl-D, after `--print`, and on `/checkpoint`,
which keeps the session going: a forked child writes `maxwell.image.tmp` next
to the image and the parent `fsync`s it and renames it into place, so a crash
loses at most the current session. If `fork` fails, Maxwell saves and exits
instead, and the new image replaces the old one once it is complete. An
exclusive lock on `maxwell.lock` admits one Maxwell at a time; a second exits
with a message. `--ephemeral` never reads, locks, or writes the global image.

To run new Maxwell code with the chat:

- `/reload` loads the source tree the binary was built from into the running
  session with ASDF, then saves. This is the usual way.
- `maxwell --adopt-chat`, run with a new build, starts the global image with
  `--export-chat`, reads the chat it prints as plain s-expressions through a
  pipe (with `*read-eval*` off), and saves the new build with it as the global
  image.

`/save PATH`, or `(maxwell:request-image "path")` from the agent, still writes
a separate snapshot and exits after saving the global chat; the agent's request
waits until its turn ends. Start a snapshot with `--fresh`, or it hands over to
the global image.

API keys are read from their environment variable, and logins from their
credential file, for each request; neither is held in the image. Custom header
values *are* part of client state, so do not put secrets in `--header`: the
image is saved automatically. The image holds the whole chat and is written
owner-only.

## Deliberate limits

- One provider family: OpenAI-compatible APIs.
- Two wire shapes: compatible Chat Completions by default, Responses when
  explicitly selected.
- One model-visible tool: `common_lisp`.
- One endless chat, compressed into a view, kept in one global image.
- No sandbox. Evaluated Lisp has the permissions of the Maxwell process.
- Tool calls are sequential because they mutate one Lisp world.

These constraints are the design. The code is small enough for Maxwell to
inspect and adapt itself using its own Lisp tool.

### Transcript navigation

Tool calls are drawn as a one-line summary with the last lines of their
result; a green or red bullet shows whether the call succeeded. **Ctrl-T**
switches every tool call between that summary and the full code and result,
and redraws the screen, at the prompt, while the model works, or in the
viewer. Replies are rendered as light markdown, and code in them is
highlighted (Common Lisp, Rust, JavaScript/TypeScript, C/C++, Go, Java,
Python, shell, JSON, TOML/YAML, diff).

Colour uses the 16 standard ANSI colours, so your terminal theme decides the
shades. It is on when output is a terminal, `TERM` is not `dumb`, and
`NO_COLOR` is unset; `--color always` or `--color never` overrides that.
Nothing styled reaches the model or the log.

At the interactive prompt, press **Page Up** to open conversation history,
including tool calls and results; scrolling back continues into earlier
sessions. **Page Up / Page Down** move one page at a time; **Escape** returns
to the prompt. Page Down past the newest page also returns. Your unfinished
input is preserved. Left/Right, Home/End, Backspace, and Delete edit the
prompt; long input scrolls horizontally. Ctrl-C clears the line, or abandons
a running turn.

This uses an ANSI-compatible terminal and `stty` with `/dev/tty` (currently
Linux). Redirected input/output and `TERM=dumb` retain ordinary line input.
The viewer shows conversation messages, not every CLI status line, and terminal
dimensions are read when it opens. A resumed chat starts by showing its last
two exchanges, dimmed.

### The status bar

On an interactive terminal, a dim row sits directly above the prompt, showing
which provider and model is answering and what Maxwell is doing: idle,
thinking, responding, tooling, summarizing, or saving. Anything printed to the
transcript clears the row first and the next prompt draws it again, so it never
disturbs what is already on screen. `/statusbar` turns it off and on, and
`/statusbar on` or `/statusbar off` says which. Redirected input and output have
no status bar.

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
(Ctrl-T too) are handled as soon as it finishes — they wait in the terminal
buffer and are not lost.

This is done by running `curl` asynchronously and polling the terminal while it
runs, rather than by evaluating tool calls on a second thread. Tool calls keep
running on the one thread that owns the agent's Lisp world.
