# PLAN: Improvements round 1

Four improvements to Maxwell:

1. **Colour.** A restrained palette, in the spirit of the Codex CLI, so the
   transcript is easy to scan.
2. **Collapsible tool calls.** Each tool call shows a short summary or its
   full code and output. **Ctrl-T** switches between the two.
3. **Syntax highlighting.** Highlight Common Lisp first, then Rust,
   JavaScript, TypeScript, and other common languages. Also render light
   markdown.
4. **OptChat memory.** One chat that never ends, compressed into a binary tree
   of summaries and shown to the model as a fixed-size *view*. It follows
   [VictorTaelin's UniiChat/OptMem design](https://gist.github.com/VictorTaelin/91837951a5ce5b38f341ec1ba1df6449).
   UniiChat stores the chat in `.jsonl` files. Maxwell keeps everything as
   Lisp data in the image, so Maxwell stays one self-contained executable.

Constraints from the README still hold: no Lisp dependencies beyond
ASDF/UIOP, one model-visible tool (`common_lisp`), and one thread that owns
the Lisp world.

---

## Decisions (from the grilling)

| Question | Decision |
|---|---|
| OptChat vs. linear history | **Replace.** OptChat becomes how Maxwell works. Each user message starts a fresh call: `[tools][system][view][turn state][message]`. `agent-history` only holds the turn in progress. |
| How the model zooms | **Lisp functions** called through `common_lisp`: `(maxwell:zoom id n)` and `(maxwell:date id)`. There is still one tool. |
| Compactor | Same provider, cheap model set with `--compact-model` / `MAXWELL_COMPACT_MODEL`. Requests run as **async curl processes polled in the existing input loop** (the same way steering is serviced). Up to 8 at once. No threads. |
| Durability | **Auto-checkpoint.** On `/quit`, Ctrl-D, and `/checkpoint`, write the image to a temp file and `rename(2)` it over the live image. A crash loses only the current session. |
| Image location | **One global chat** at `~/.local/share/maxwell/maxwell.image` (respects `XDG_DATA_HOME`; can be overridden with `MAXWELL_IMAGE`). |
| Ctrl-T | **Global toggle with redraw.** Switches every tool call between summary and full, and redraws the visible transcript in place. Works at the prompt, while the model works, and in the PgUp viewer. |
| Transpose | **Dropped.** Ctrl-T is the toggle everywhere. |
| Redraw extent | Expand only the **last ~2 pages**. PgUp then expands older history **on demand** as the user scrolls back. |
| Projects | **Tag view lines** with their project: `id+n\|[proj] text`. |
| AGENTS.md | **Import as `:note`** once per (project, content hash). An edited file is imported again. |
| Prompt-cache marks | **Out of scope** for now. |
| Markdown | **Light.** Headings, bold, italic, inline code, bullets, and highlighted fenced code, with the markers hidden. Tables and links pass through mostly unchanged. |

---

## Phase 1: Styling foundation (`src/style.lisp`)

Every later feature depends on this module. It goes after `runtime` and
before `agent` in `maxwell.asd`.

- **When colour is used.** Only when stdout is an interactive terminal, `TERM`
  is not `dumb`, and `NO_COLOR` is unset. A new `--color auto|always|never`
  flag overrides this; `always` lets the pty tests check escape codes. Store
  the result in `*color*`. Like the terminal state, recompute it at startup
  and after an image resumes, never save it.
- **Palette.** Use the 16 standard ANSI colours plus bold, dim, and italic, so
  the user's own terminal theme decides the actual shades. Do not use
  truecolor. Map faces to codes in one table so the theme can be changed in
  one place:

  | Face | Use | SGR |
  |---|---|---|
  | `:user` | user prompt text, `›` gutter | bold / cyan gutter |
  | `:assistant` | assistant prose | default |
  | `:tool-header` | `• lisp` line | bold |
  | `:tool-ok` / `:tool-error` | status bullet | green / red |
  | `:dim` | summaries, hints, metadata, line counts | dim (2) |
  | `:error` | provider and CLI errors | red |
  | `:heading` | markdown headings | bold magenta |
  | `:code-inline` | `` `code` `` | cyan |
  | `:steer` | `steer>` echo | yellow |
  | syntax faces | `:kw :str :comment :num :sym :fn :type :const :punct` | see Phase 3 |

- **API.** `(styled face string)` returns the string wrapped in SGR codes, or
  the string unchanged when colour is off. `(with-face (face stream) …)` does
  the same for a stream. `visible-length` and `ansi-wrap` measure and wrap
  text while ignoring escape sequences, and close and reopen the SGR state
  at each line break.
- **Fix the viewer's wrapper.** `transcript-lines` in `src/terminal.lisp`
  currently removes control characters. Replace it with `ansi-wrap` so the
  PgUp viewer keeps colour. Styled text must never reach the model or the
  log. Styling is applied only when rendering.
- **Status lines.** The startup banner, `/help`, `/model`, and error output
  use `:dim` and `:error`. Steering echoes use `:steer`.

## Phase 2: Transcript rendering and Ctrl-T (`src/render.lisp`)

Printing is currently spread out: `execute-tool-call` prints code and result
directly, and `run` prints the assistant's reply. Move all of it into a single
renderer that draws **transcript entries**.

- **Transcript entry.** A small struct: `kind` (`:user :assistant :tool
  :echo :steer :status`), `text`, `ok-p`, `id` (the message id in the log,
  once Phase 4 exists), and `time`. The agent loop *records* entries. Only the
  renderer *prints* them. The session's entries are kept in
  `*session-entries*`, a vector that is not saved.
- **Tool call display.** A `:tool` entry and its `:echo` result are drawn as
  one block.
  - *Summary mode* (the default):
    ```
    • lisp (defun parse-header (line) …)            ← first form, one line, highlighted
      └ => PARSE-HEADER                              ← last 1–3 result lines, dim
        … +41 lines (ctrl-t to expand)
    ```
    The bullet is green on success and red when the result is an error. The
    `eval-lisp` error format `TYPE: message` already identifies errors; return
    that as a second value so the renderer does not have to parse text.
  - *Full mode*: the whole code, highlighted as Lisp, then the whole result.
    Results print the values with `*print-pretty*`, so highlight them as Lisp
    too.
- **Ctrl-T.** `*tool-display*` is either `:summary` or `:full`. Ctrl-T
  switches it and redraws:
  - *At the prompt or while the model works:* clear the screen and scrollback
    (`ESC[H ESC[2J ESC[3J`). Re-render only the newest entries, enough to
    fill about **two terminal pages**: walk back from the newest entry,
    rendering until the rows reach `2 × rows`. Print a dim
    `… earlier history: PgUp` line above them, then redraw the prompt or
    steer line. The cost of a redraw depends on the terminal's height, not on
    how long the session has been running.
  - *Scrolling back expands on demand:* PgUp opens the viewer on the same
    rendered rows. When the user scrolls within a page of the top of what
    has been rendered, the viewer renders the next older batch of entries in
    the current mode and adds it above. It reads from `*session-entries*`
    first and then from the OptChat log once Phase 4 exists, so scrolling
    can go back through the whole chat. Rendered rows are cached per entry
    for each mode and width, and the cache is cleared when the terminal is
    resized.
  - *In the PgUp viewer:* Ctrl-T clears the row cache. It re-renders the
    entries around the current position in the new mode and keeps the scroll
    position anchored to the entry at the top of the screen. Older entries
    are rendered again lazily as the user scrolls up.
  - Remove the Ctrl-T (code 20) branch from `editor-key` and remove
    `transpose-characters` and its tests. Add Ctrl-T to `terminal-key`'s
    callers as `:toggle-tools`. It returns a new `:toggle` action, which
    `read-terminal-prompt`, `service-steering-input`, and `browse-transcript`
    each handle.
  - A long-running tool call holds the Lisp world, so Ctrl-T waits in the tty
    buffer until the call finishes. This is the same behaviour as steering
    today, and the README already explains it.
- **The PgUp viewer** renders from entries instead of building text from
  `agent-history`. It uses the lazy, on-demand rendering described above
  instead of wrapping the whole transcript when it opens. Today's
  `transcript-text` and `transcript-lines` path is removed.
- **On resume,** print the last two or three exchanges from the log, dimmed,
  so the user can see where the endless chat left off.

## Phase 3: Syntax highlighting and light markdown

### `src/highlight.lisp`

Write the lexers by hand, with no regex library. `(highlight-code string
language)` returns styled text. Unknown languages come back as plain text.

- **Common Lisp** gets a dedicated lexer and the most attention:
  - `;` line comments, nested `#| … |#` block comments, `"strings"` with
    escapes, `#\char`, numbers including ratios, `#x`, and `#b`.
  - `:keywords`, `'quote`, backquote and `,`/`,@`, `#'function`, and
    `|piped symbols|`.
  - Symbol faces come from the **live image**: `find-symbol` in `CL`
    determines special operators and macros (`:kw`), fbound CL functions
    (`:fn`), and constants (`:const`). Do not keep a separate keyword list.
  - The name after `def*` forms (`defun`, `defmacro`, `defclass`, `defstruct`,
    `defparameter`, …) gets `:fn` or `:type`.
  - Bracket depth may get optional rainbow dim/normal alternation. Off by
    default.
- **C-family table lexer.** One engine driven by a per-language spec:
  keywords, types, constants, comment syntax, string delimiters (including
  template literals and raw strings), and number syntax. Specs:
  - **Rust**, including lifetimes (`'a`), macros (`name!`), attributes
    (`#[...]`), and `r#"raw"#`
  - **JavaScript / TypeScript** (`js jsx ts tsx`), including template
    literals with `${}` and TS types and keywords
  - **C / C++, Go, Java**
- **Small lexers:** Python (`#` comments, triple-quoted strings, decorators),
  shell (`sh bash zsh`), JSON, TOML/YAML (keys, strings, comments), and
  `diff` (`+`, `-`, and `@@` lines).
- **Language detection:** from the fence info string first (`lisp`,
  `common-lisp`, `cl`, `elisp`, `scheme`, `clojure`, `rs`, `ts`, …). For
  unlabelled fences, use simple heuristics: a block that starts with `(` and
  mostly has balanced parentheses is Lisp, and `fn`/`let mut` means Rust.
  Otherwise use plain text.
- Lexers must never fail on malformed input. An unterminated string or
  comment runs to the end of the block.

### `src/markdown.lisp`

Render assistant replies line by line:

- `#`…`######` headings: `:heading`, with the markers removed.
- `**bold**`, `*italic*` / `_italic_`, and `` `inline` ``: hide the markers.
  Do not treat markers inside words such as `snake_case_name` as emphasis.
- `-`, `*`, and `1.` lists: replace the bullet with a dim `•` and keep the
  indentation.
- Fenced blocks: highlight them with `highlight-code`, indent them by two
  spaces, and do not apply inline markdown inside them.
- Tables, links, and block quotes pass through unchanged, with links dimmed.
- With colour off, the output is the original text unchanged. This matters
  for `--print` piped into other tools.

## Phase 4: The OptChat store (`src/memory.lisp`)

All state is Lisp data reachable from one global, `*chat*`, so
`save-lisp-and-die` saves it with nothing else.

```lisp
(defstruct chat
  (messages (make-array 0 :adjustable t :fill-pointer t))  ; vector of MESSAGE, index = id
  (levels   (make-array 0 :adjustable t :fill-pointer t))  ; levels[l] = adjustable vector of NODE-or-NIL
  (view     (make-array 0 :adjustable t :fill-pointer t))  ; vector of (l . i), oldest first
  (compaction-view ...)                                    ; same shape, 16–32 KB sawtooth
  (ready    ...)                                           ; queue of (l . i) ready to build
  (jobs     (make-hash-table :test 'equal))                ; (l . i) -> in-flight job (not saved live; see below)
  (version 1))

(defstruct message id kind text size time workspace)       ; kind: :user :maxwell :tool :echo :work :note
(defstruct node text size projects)                        ; node(0,i) = message i ≤512 B; node(l,i) = merge
```

- **Project tags.** Each message records its project. The project is the
  name of the git root of its workspace, or the workspace directory's name
  when there is no git root. If two workspaces have the same name, add
  parent directories until the names differ. The mapping from name to
  absolute path is kept in `*chat*`. A node's `projects` is the union of its
  children's, oldest first. View lines render as
  `id+n|[maxwell] text`, or as `id+n|[maxwell,dotfiles] text` for lines that
  cover more than one project. Above 3 projects, show the first 3 and `+N`.
  The tag counts toward the view's byte size, but not toward the node's
  512-byte limit. The tag is metadata the code adds, not text the compactor
  writes. Compaction inputs show the same tags, so summaries can say what
  happened in which project.

- **Log rules.** Messages are only appended, never edited or deleted.
  Reasoning is shown but never logged. A tool result is clipped to its head
  and tail, 30,000 characters in total. Any other long text is split into
  several consecutive messages of at most 30,000 characters each. `size` is
  the UTF-8 byte count (`sb-ext:string-to-octets`). Each message records the
  workspace it came from, because the chat is global across projects.
- **Tree.** `node(l, i)` covers messages `[i·2^l, (i+1)·2^l)` and is named
  `id+n`. A source of 512 bytes or less is its own node, with no model call.
  Two short lines that fit together are joined with a newline. Each node is
  built once and never rebuilt.
- **View.** Implement the gist's rules exactly:
  - Each new message appends its line, `(0 . i)`.
  - When the view's rendered size passes **128,000 bytes**, merge the most
    *due* sibling pairs whose parent is already built until the view is at
    most **64,000 bytes**. Due is `due = (T − last) / 2^l`, where *last* is
    the pair's last message, not its first. Ties go to the oldest pair. If the
    view cannot reach 64 KB yet, merge what it can after each later message.
  - The view is **never rebuilt** from the log. It is saved in the image as
    it is.
  - Rendering: `<chat>` followed by one `id+n|text` line per node (newlines
    turned into spaces) and `</chat>`. Messages not summarized yet are not
    shown. The turn waits for them (see Phase 6).
- **Compaction view:** the chat view merged further to a 16–32 KB sawtooth,
  merged again whenever the chat view merges. Each compaction call gets this
  view cut off at its node.
- **Ready queues, never scans.** When a message is appended, enqueue its
  `(0 . i)` once fewer than 8 earlier lines are still unbuilt. When a node is
  built and its sibling is also built, enqueue the parent. Scanning the tree
  for work becomes O(N²) over a long chat.
- **Model-facing functions,** exported from `maxwell` and documented in the
  system prompt:
  - `(zoom id n)`: with `n > 1`, the two child lines as `id+n/2|…`. With
    `n = 1`, message `id` whole, paged when it is long. Signals an error when
    `n` is not a power of two or `id` is not a multiple of `n`.
  - `(date id)`: the message's timestamp as ISO-8601.
  - The model reads memory only through `zoom`. The prompt says not to search
    `*chat*` directly, as UniiChat's prompt does.
- **AGENTS.md import.** At the start of a turn, check the workspace's git
  root, or the workspace itself, for `AGENTS.md`. If its
  `(project, sha256 of contents)` pair is not in `*chat*`'s import table,
  log it as `:note` messages tagged with the project. The note starts with
  `[AGENTS.md <path>]`, and long files are split into consecutive messages.
  Then record the pair. Unchanged files are never imported again. An edited
  file is imported again as a new note, so the latest version is the newest
  thing the view mentions, which fits the prompt's rule that *the latest
  word is the truth*. The hash is computed in Lisp (`sb-md5` is bundled;
  sha256 via `openssl dgst`, which the OAuth code already requires).
  `/import PATH` imports any other file the same way.
- **Migration.** On first start, import the messages of any old-format image
  that still has a non-empty linear `agent-history` into the log as
  `:user`, `:maxwell`, `:tool`, and `:echo` messages, then clear the history.

## Phase 5: Background compactor (`src/compactor.lisp`)

- **Jobs.** A job is a running `curl` process (generalize the async-curl code
  in `src/process.lisp` and `post-provider`) plus the node it builds, the
  retry count, and the shortest line so far.
  - `compactor-tick` starts jobs from `ready` up to 8 at once and reaps
    finished ones.
  - It runs from the same polling hook that services steering (`*input-poll*`),
    from the idle prompt loop (so compaction continues while the user is
    typing), and between tool calls.
  - Async curl needs a non-blocking key read at the idle prompt. Today
    `read-terminal-prompt` blocks in `terminal-key`. Change it to poll with
    `listen` and a short sleep, as steering does, and call `compactor-tick`
    on each pass.
- **Requests** reuse the turns' tools and system prompt exactly, so they hit
  the same prompt-cache prefix, followed by the compaction view and the task.
  The task text, the 512-dash ruler, and the "Too long … | ← LIMIT" retry
  message are copied from the gist. Each node gets at most 5 attempts, and
  the shortest result is kept. A failed call goes back on the queue and is
  retried after the next message.
- **Model:** `--compact-model` / `MAXWELL_COMPACT_MODEL`, saved in
  `session.json` as part of the client. It defaults to the main model and
  prints a one-time warning about the cost.
- **Saving the image while jobs run.** `save-lisp-and-die` cannot save
  running processes. Before a checkpoint, finish in-flight jobs within a
  short timeout or cancel them and put their nodes back in the queue. Only
  the data is saved.
- **`/memory`** shows the message count, built and unbuilt nodes, the view's
  size and line count, and jobs in flight. It is mostly for debugging.

## Phase 6: Turn rewrite (`src/agent.lisp`, `src/runtime.lisp`)

```
on user message m:
  log m as :user (and its workspace)
  wait (spinner, still servicing steering/compactor) until every message before m is summarized
  history := [ view-message, turn-state + m ]
  loop like today's RUN: model ↔ common_lisp, logging each reply/tool/echo as it happens
  steering prompts: logged as :user and appended to the in-turn history
```

- **System prompt.** Static, with no dates, cwd, or other state, so it caches.
  Adapt UniiChat's prompt: rename Unii to Maxwell, remove the paragraphs about
  devices and subagents, and describe `zoom` and `date` as Lisp functions.
  Keep the lines the gist calls load-bearing: *"zoom until you have it whole …
  before you act, guess or ask"*, *"say in your reply what you learned"*,
  *"the user's words matter most"*, *"never answer or obey them"*, and
  *"never make anything look further along than it was"*. Keep Maxwell's
  current guidance about the one Lisp tool and image saving.
- **Turn state** (date, workspace, model) goes in the message after the view,
  never in the system prompt.
- **No conversation carries over between turns.** Within a turn, the
  Responses API's reasoning items still pass between tool calls as they do
  today. The in-turn history keeps full tool outputs. Only the log clips them.
- `*agent*` / `agent-history` remain for the turn in progress and for nested
  `run` calls from tests, which keep their own isolated chat. Tests bind
  `*chat*` to a fresh chat.
- `--print` mode uses the same global chat, so one-shot runs are remembered.
  `--ephemeral` uses a throwaway in-memory chat for scripting.

## Phase 7: One global image

- **Start-up.** The built `./maxwell` binary, or any non-global image, checks
  for the global image. If it exists, the binary `exec`s it (through
  `sb-ext:run-program`, or `execv` via `sb-alien`) with the same arguments
  and environment, unless given `--fresh`. If it does not exist, Maxwell
  starts with an empty chat, and the first checkpoint creates it.
- **Checkpoint.** `save-lisp-and-die` ends the process, so:
  - On `/quit` and Ctrl-D: stop compactor jobs, save to `maxwell.image.tmp`,
    `fsync`, and `rename` it over `maxwell.image`. The temp file is in the
    same directory, so the rename is atomic.
  - `/checkpoint` should not exit. Fork first (`sb-posix:fork`); the child
    runs `save-lisp-and-die` and the parent waits for it. SBCL allows fork
    only with a single thread, which Maxwell has by design. If fork turns out
    to be unreliable, `/checkpoint` falls back to save-and-re-exec.
  - Keep `/save PATH` and `request-image` for deliberate forks or snapshots
    of the chat.
- **One writer.** Take an exclusive lock on `maxwell.lock` next to the image
  (`flock` via `sb-posix`/`sb-alien`) for the life of the process. A second
  `maxwell` started while one is running exits with a clear message, unless
  it is `--ephemeral`. Without the lock, two writers' checkpoints would
  overwrite each other.
- **Upgrading Maxwell's code.** A new build of Maxwell has new code but no
  chat. Two paths:
  - *Hot reload:* `/reload` loads the source tree into the running image with
    `asdf:load-system` (the source location is saved at build time), then
    checkpoints. This is the usual case and the most Lisp-like.
  - *Transplant:* `maxwell --adopt-chat` (new binary) runs the old global
    image with `--export-chat`. The old image prints `*chat*` as readable
    s-expressions to a pipe, which the new binary reads with
    `*read-eval* nil`, then it checkpoints. Nothing stays on disk. The chat
    `version` slot allows format migrations.
- **Secrets.** Unchanged. API keys come from the environment, and OAuth
  tokens stay in their own file. The README warning about `--header` values
  also applies to the global image now, which is saved automatically.

## Phase 8: Tests and docs

- **Unit tests** (`tests/tests.lisp`):
  - `styled` and `ansi-wrap` with colour on and off, and wrapping that keeps
    the SGR state.
  - Highlighting: golden token-face sequences for each language. Malformed
    input must not signal errors.
  - Markdown: marker removal, emphasis inside words, and fences.
  - Tool block summary and full rendering, including error bullets.
  - **The view matches push:** simulate `t = 0..20000` and check that, with
    push's own list length as the budget, merging by due picks exactly the
    lines Taelin's `push` produces (the gist's own check). Also check the
    `0+4, 4+4, 8+1, 9+1` counterexample.
  - The sawtooth: the view stays between 64 KB and 128 KB, and old lines stay
    stable between batches.
  - Compactor with a fake `model-function`: the too-long retry path, keeping
    the shortest line, and the 8-job cap.
  - `zoom` and `date` argument checks and paging.
  - Turn assembly: the system prompt is byte-identical across turns, and the
    view only grows at its end between batches.
  - Migrating a legacy `agent-history`.
  - Project tags: name disambiguation, union in merged nodes, the `+N`
    cutoff, and tag bytes counted in the view's size.
  - AGENTS.md: imported once, imported again after an edit, never imported
    again unchanged.
  - Lazy viewer: the toggle renders only about 2 pages of rows, and scrolling
    up renders older batches without changing which entry is at the top of
    the screen.
- **pty tests:** Ctrl-T toggles and redraws at the prompt, during a request,
  and in the PgUp viewer. With `--color always`, escape codes are present.
  With `NO_COLOR`, there are none.
- **Checkpoint test:** start → chat → `/quit` → restart, and check that the
  log, view, and tree are restored. A second instance is refused by the lock.
- **README.** Update *Deliberate limits*: the conversation is now "one endless
  chat, compressed into a view" instead of "one linear in-memory
  conversation". Document the colours, Ctrl-T, `zoom`/`date`, the global
  image, `/checkpoint`, `/reload`, `--adopt-chat`, `--compact-model`,
  `--color`, `--fresh`, `--ephemeral`, and `/memory`.

---

## Suggested order

1 → 3 → 2 make the UI improvements, each independently shippable.
4 → 5 → 6 → 7 make OptChat; 4 and 5 can be built and tested before the turn
loop changes. The renderer in Phase 2 draws transcript entries, so it works
with both the current history and the OptChat log.

## Out of scope for this round

- **Explicit prompt-cache marks** (Anthropic `cache_control` every 4 view
  lines). Maxwell relies on automatic prefix caching where the provider has
  it. The system prompt and the append-only view are still kept stable, so
  marks can be added later without changing the design.
- **The `work` kind.** Maxwell has no subagents yet.
