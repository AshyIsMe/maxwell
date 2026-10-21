# Plan: Maxwell in Coalton

Status: adopted. The target is a complete rewrite of Maxwell in Coalton.
The earlier module-by-module draft is withdrawn.

## 1. Decision

Maxwell becomes a Coalton program. Every module is ported: provider, auth,
memory, compactor, render, markdown, highlight, style, status, terminal,
process, cli, agent, runtime, session, stats, json. The Lisp test suite is
ported too.

What stays in Common Lisp, and only this:

| file | what | target size |
| --- | --- | --- |
| `src/package.lisp` | `defpackage` forms | ~60 lines |
| `src/platform.lisp` | FFI, OS, image mechanics, the CL evaluator, and the `coalton_eval` tool loop (read through the Coalton readtable, `eval` the generated Lisp) | ~600-800 lines |
| `maxwell.asd` | system definition, `:ct-file` components | ~40 lines |

Nothing else. No module keeps a Lisp half.

## 2. Why the Lisp remainder cannot shrink further

Four things cannot be written in Coalton. All four are platform mechanics, none
is Maxwell logic.

1. FFI. Coalton has no foreign function interface. `(lisp ...)` forms are the
   only way to reach SBCL. Measured in src: 73 `sb-alien`/`sb-posix:`/`sb-sys:`/`sb-ext:`
   uses, of which image.lisp 50 (40 plus 10 `sb-ext:`), process.lisp 9 (all
   `sb-ext:`), terminal.lisp 4, agent.lisp 3, compactor.lisp 2, cli.lisp 2, and
   auth/memory/runtime 1 each.
2. Image mechanics. `save-lisp-and-die`, `fork`, `execv`, `dup2`, `flock`,
   `waitpid`, `chmod`, `rename`, fd streams. SBCL-only, Lisp-only. This is how
   Maxwell saves itself (`/checkpoint`, `/quit`) and how the exec-shared image
   works.
3. The live evaluator. The agent's only tool is an eval on source text, and that
   cannot be a Coalton expression: Coalton compiles code, it does not host one.
   `common_lisp` becomes `coalton_eval`; it binds the Coalton readtable and a
   workspace Coalton package, splits the string into forms, and `eval`s the
   generated Lisp. That is Lisp calling into compiled Coalton, and it needs the
   compiler in the image at run time, which is why the image keeps it (section
   5). `eval-lisp` for `/lisp` stays for the same reason.
4. The entry point. Coalton runs on a CL runtime; the executable's toplevel is
   a Lisp function that calls into compiled Coalton code.

So the boundary is one file with a declared interface. Rule: no `lisp` form and
no `sb-*` call appears outside `src/platform.lisp`. CL standard functions that
Coalton lacks (string and char operations, stream output) are reached through a
small Coalton wrapper package `maxwell/cl` whose bodies are `lisp` forms; that
package is the second and last seam.

## 3. Why complete, not partial

The withdrawn draft kept image, terminal, process, provider, auth, json, cli and
runtime in Lisp because they are I/O heavy. That is the wrong cut. Those modules
hold the code most worth checking: 40 `handler-case`, 43 `cond`, 14 `case`,
4 `ecase`, 220 `setf`, 72 `getf`, 59 `gethash`. The I/O itself is a thin call
into platform; the logic around it is not thin, and it is the logic that breaks
(the 413 context blowup came from an unbounded value crossing a boundary that
nothing checked).

A mixed codebase also keeps two readtables, two build stories and two debugging
stories forever. A complete port retires the Lisp versions.

## 4. Target layout

- `src/*.ct` one file per current `src/*.lisp`, same module names, same
  package names (`maxwell/provider`, `maxwell/memory`, ...).
- `src/platform.lisp` and `src/package.lisp` as in section 1.
- `maxwell.asd`: `:defsystem-depends-on ("coalton-asdf")`, `:depends-on
  ("coalton" "coalton/library")`, components `(:file "package")`,
  `(:file "platform")`, then `(:ct-file "...")` per module, `:serial t`.
- `vendor/coalton/` plus the dependencies in section 5, on
  `asdf:*central-registry*` like the other vendored libraries.
- Tests: `tests/*.lisp` become `tests/*.ct`; the four Python pty scripts stay
  unchanged (they test the executable, not the language).

Module sizes to port, measured:

| module | lines | module | lines |
| --- | --- | --- | --- |
| provider | 850 | memory | 578 |
| auth | 784 | render | 550 |
| highlight | 704 | terminal | 401 |
| cli | 617 | agent | 333 |
| image | 326 | compactor | 314 |
| status | 234 | runtime | 218 |
| json | 216 | style | 215 |
| stats | 188 | process | 180 |
| markdown | 170 | session | 91 |
| package | 46 | total | 6,996 |

## 5. Build and image

Measured on SBCL 2.2.9, this machine, Coalton cloned at 0b5c10f (VERSION
"0.0.1"):

| quantity | value |
| --- | --- |
| cold build of coalton + 6 dependencies | 41 s |
| warm load of coalton | 1.24 s |
| compiled image, bare SBCL | 38 MB |
| compiled image, SBCL + Coalton | 72 MB |
| vendored dependencies | 8.9 MB, 7 systems |
| Coalton source, `src` + `library` | 3.6 MB (2.2 MB + 744 KB) |
| compile of a 240-line `.ct` file | 0.36 s |
| compile of a 1,200-line `.ct` file | 2.03 s |

Dependencies to vendor: coalton, coalton-asdf, coalton-compiler,
coalton/library, concrete-syntax-tree, eclector, float-features,
source-error, trivial-gray-streams, named-readtables, trivial-garbage,
alexandria, mgl-pax (+ documentation-utils). Quicklisp resolved all of them;
`vendor/` already holds alexandria and named-readtables. The working copies
used for every measurement here are `/tmp/ct/coalton` (git clone, HEAD 0b5c10f,
3.6 MB) and `/tmp/ct/quicklisp` (the six dependencies Quicklisp fetched);
`/tmp/ct/demo2.image` and `/tmp/ct/live2.image` are the saved-image experiments.
Vendoring copies those sources into `vendor/`, it does not re-resolve them.

Loading Maxwell's 6,996 lines from source scales from the 2.03 s / 1,200-line
measurement to roughly 10 s clean and 7 s on reload. Compiled `.ct` files load
as fasls, so `/reload` needs a decision: reload recompiles changed modules
only, or Maxwell is rebuilt with `make build`. Either way this is slower than
today's Lisp reload and must be measured when the port is real, not assumed.

Image cost: +34 MB, taking the Maxwell image from 72 MB to about 106 MB. The
compiler is not optional, so this is the shipped size. The agent's only tool is
`coalton_eval`: it reads Coalton source, typechecks it, generates Lisp, and runs
it inside the live image. Typechecking and codegen at run time need
`coalton-compiler` and its global environment in the image, so the compiler
stays and `/reload` can recompile changed `.ct` modules. Dropping it would
leave the agent unable to define a function, which is the one thing a coding
agent must do.

Measured on this machine, evaluating through the Coalton readtable in the
image:

| step | time |
| --- | --- |
| `(coalton-toplevel (define ...))` then one `(coalton (...))` | 3.6 ms |
| one `(coalton expr)` | 2.4 ms |

Several forms per call, split first. The Coalton readtable's open-paren macro
reads the rest of the stream, so a plain `read` loop loses everything after the
first form: measured, a string holding two `coalton-toplevel` forms defines the
first and silently drops the second. `coalton_eval` therefore splits the string
on top-level parentheses itself (tracking strings, `;` comments and `#`
character literals; no regex, no reader) and evaluates each piece. Measured on
the full path: a comment holding a parenthesis, a string holding `)(`, and a
definition used by a later form in the same call all work.

What `coalton_eval` must do, found by measurement:

- Bind `*readtable*` to `(named-readtables:find-readtable 'coalton:coalton)` and
  `*package*` to a package that uses `coalton` and `coalton-prelude`. These two
  bindings are what make the source read as Coalton.
- Bind `*load-truename*` and `*compile-file-truename*` to nil. With either
  bound, the reader looks for spans in that file and signals `COALTON-BUG "Unable to recover source span for COALTON-TOPLEVEL"`.
- Read and evaluate the forms one at a time so definitions and expressions can
  be mixed in one call.
- Errors arrive as conditions catchable by `handler-case`: `PARSE-ERROR`, and
  `TC-ERROR` for a type mismatch or unknown variable. A bad form does not
  damage the environment; the next eval proceeds.
- `define-type` may be re-evaluated with more constructors and existing callers
  keep working; changing a constructor's arity does not.

## 6. Adoption mechanics

Facts established by experiment, all of which bit me at least once:

1. `(named-readtables:in-readtable coalton:coalton)` rewrites every top-level
   form, including `defpackage` and `save-lisp-and-die`. Only the forms that
   are genuinely Coalton should be read in that readtable. Convention: each
   `.ct` file starts with `(cl:in-package ...)` and
   `(named-readtables:in-readtable coalton:coalton)`, and then contains only
   Coalton forms. Plain Lisp lives in `.lisp` files.
2. `t` reads as `True` under that readtable. Any Lisp form inside it needs
   `cl:` prefixes, and `cl:if` inside `(lisp ...)` bodies.
3. A Coalton definition is not a Lisp function. `(twice 5)` from Lisp fails
   with "invalid number of arguments"; call through `(coalton (twice 5))`, or
   export a Lisp wrapper from `maxwell/platform.lisp` that does it.
4. Redefinition across arity changes breaks old callers silently until called
   (measured: redefining a struct with an extra field then calling the old
   accessor gives `invalid number of arguments: 2`). This constrains the live
   image: `/reload` after a data-shape change must restart, as it already must
   in Lisp today.
5. Redefinition of a `define-type` in a restored image works (verified in a
   saved Coalton image).
6. `:coalton-file` expects Coalton-native `.coal` syntax; `.ct` files need
   `:ct-file`. A `.ct` file loaded without its package existing dies with
   "The variable N is unbound".
7. `sbcl --core IMAGE` must come first on the command line, before other
   options, or SBCL rejects it as a runtime option in the middle.
8. Release mode (`COALTON_ENV=release`) freezes types and compiles to
   `defstruct`; development mode keeps types as classes and unflattened. A live
   image needs development mode. Performance work needs release mode. Decide
   per build; the plan assumes development mode for the interactive image.
9. Type errors stop the whole file (`TC-ERROR`), not the one form. Porting is
   therefore done file by file with a green build before moving on.
10. Names collide with CL types: `f32`, `f64` are Coalton symbols in the
    `coalton` package. Prefix domain names accordingly.
11. `read-from-string` under the Coalton readtable returns one form and swallows
    the rest of the string: outside a compile or load operation the reader
    clones the remaining input into a source string and reads to its end. A
    `read` loop therefore stops after the first form, silently. `coalton_eval`
    splits the string itself. This is also why `coalton_eval` cannot be built
    on `read`.
12. Type errors, unknown variables and unterminated forms all come back as
    ordinary conditions (`TC-ERROR`, `PARSE-ERROR`) from `eval`, so the tool
    reports them as text and the next call works: measured, a bad form leaves
    the environment usable and later definitions keep working.

## 7. Interop rules

1. One seam for Lisp: `src/platform.lisp`. No `lisp` form elsewhere.
2. One seam for CL standard functions Coalton lacks: `maxwell/cl`, a Coalton
   module whose bodies are `lisp` forms over `cl:` functions. String and char
   work (95 `string=`, 92 `char=` in src) lands here.
3. Coalton types hold data; Lisp holds nothing but fds, pids and the runtime.
4. The agent's tool is `coalton_eval` and it lives in `src/platform.lisp`: it binds
   the Coalton readtable, a workspace Coalton package, and nil load/compile
   truenames, splits the string into top-level forms, `eval`s each and collects
   its values and printed output, and catches `error` to return the condition
   text as the tool result. It cannot be a Coalton function, because evaluating
   the generated Lisp is the whole job. The tool keeps the existing shape:
   argument `code`, a string, a single object result, and the existing
   `execute-tool-call` path in `agent.lisp` is unchanged apart from the name.
   `eval-lisp` (the `/lisp` command) stays Lisp for the same reason.
   The rename reaches three places that name the tool: `*base-system-prompt*`
   in `runtime.lisp` (a string saying the agent has one COMMON_LISP tool),
   `tests/support.lisp` and `tests/ui-tests.lisp`/`tests/tests.lisp` (7
   `common_lisp` mentions), and `src/agent.lisp`'s dispatch table. All three
   change in S0, together with a new pty check that a tool call defining a
   function through `coalton_eval` works end to end.
5. Errors become Coalton `Result` or CL conditions at the seam, never both.
   Today's 40 `handler-case` become typed results where the failure is
   expected (provider HTTP, JSON, compaction) and conditions where it is a bug.
6. All mutable state (the chat, the agent, stats) is Coalton. `setf` of
   accessors disappears in favour of `cell:write!` and record updates.

## 8. Order of work

Each step ends with `make test` and the four pty scripts passing.

- S0, foundation: vendor Coalton, `:ct-file` system, `maxwell/platform.lisp`
  with the FFI/image/eval interface and the `coalton_eval` tool loop,
  `maxwell/cl`. Build an empty Coalton Maxwell that starts, prints a status bar,
  quits, and can define a function through `coalton_eval`. This is the step that
  proves the compiler in the image works as the tool, so it comes first. 4-5
  days.
- S1, numbers and text: json, stats, style, markdown, highlight. These are pure
  and give the type discipline a footing. 4-5 days.
- S2, data: memory, session. Records, tree merging, windows, compaction. The
  largest payoff; the 413 class of bug becomes visible at the seam. 7-10 days.
- S3, model: provider, auth, compactor, agent. HTTP via `platform`, SSE
  parsing typed, jobs explicit. 10-12 days.
- S4, interface: render, status, cli, runtime, process, terminal. 8-10 days.
- S5, image: rewrite `image.lisp` (326 lines, 50 `sb-*` uses) as the platform
  half plus typed checkpoint logic. 3-4 days.
- S6, tests: port 2,237 lines of tests to Coalton; keep the pty scripts. 5-7
  days.
- S7, cut over: delete the Lisp modules, one release behind the old binary,
  compare `/stats` tok/s and first-token latency against the Lisp build.

Total: 41-53 days of one developer. Whole-file porting with a green build at
each boundary is the reason this estimate is bounded; a big-bang port is not.

## 9. Costs and risks

Costs, stated plainly:

- Time: 41-53 days, against 18-27 for the partial draft.
- Image: +34 MB for the compiler, which the `coalton_eval` tool needs at run
  time, so the image is ~106 MB.
- Cold build: 41 s for Coalton, once, into `vendor/`.
- Reload: slower than Lisp unless changed files are the only ones recompiled.
- Tests: 2,237 lines rewritten before the first ported module's tests can be
  trusted.
- No third-party Coalton libraries beyond the standard library; anything
  needed is written.
- Coalton is VERSION "0.0.1" and not 1.0. Its syntax and compiler will move.
  The vendored copy pins the version; upgrading is a deliberate act.

Risks:

- The FFI seam is the single point of failure. If `maxwell/platform.lisp`
  cannot express the checkpoint flow (fork, dup2, save-lisp-and-die inside a
  child, waitpid), the port stalls at S5. Verify that first, in S0, with a
  throwaway checkpoint rather than at day 40.
- Type discipline will surface latent bugs in memory and compactor. Budget the
  fix time; they are the bugs worth having.
- Curation of the plain-Lisp seam will erode under pressure. A test can enforce
  it: scan `src/*.ct` for "sb-" and "(lisp " and fail if either appears
  outside `maxwell/cl`.
- Development mode is slower than the Lisp build in tight loops (status bar
  refresh, highlighting). Measure at S4; if it matters, compile those modules
  with release mode and their types do not change after load.

## 10. Settled, and still open

Settled by measurement or by the user:

1. `/reload` recompiles changed `.ct` modules in the image rather than
   requiring `make build`; the compiler is already there for `coalton_eval`,
   and 0.36 s / 240 lines bounds the work.
2. The image keeps the Coalton compiler. `coalton_eval` is the agent's only
   tool, so the compiler is needed at run time and the image is ~106 MB. That
   cost is accepted; it is not an open choice.
3. There is no `coalton/json` at 0b5c10f. Checked `library/` (39 files, none
   named json) and every `*.ct`/`*.lisp`/`*.asd` in the tag; the only hit is
   `mine/src/app/check-update.lisp`. `json.lisp` (216 lines) is ported as-is to
   `json.ct`.
4. The agent's only tool becomes `coalton_eval`, not `common_lisp`; the user
   asked why anything would stay in Lisp and the answer is the four items in
   section 2, not the tool.

Still open, to answer during the step named:

1. Adopt `coalton/format` (`format.ct`, 1,243 lines, statically checked control
   strings) for all 150 `format` calls, or only the user-facing ones? Answer at
   S4; recommend all of them, since the checking is the point.
2. Release mode for the shipped executable, development mode for the
   interactive image: two build products or one? Answer at S7; recommend two,
   named in the Makefile.

## 11. First commits

1. `vendor/coalton` and the dependencies in section 5; `maxwell.asd` gains
   `coalton-asdf` as `:defsystem-depends-on` and lists each module as
   `(:ct-file ...)`; every vendored system is registered with the other six
   existing ones (alexandria, fset, mgl-pax-bootstrap, misc-extensions,
   mt19937, named-readtables) by the existing `vendor/*/` loop at the top of
   `maxwell.asd`, and `vendor/fetch.sh` gains their pinned sources.
2. `src/platform.lisp`: a Lisp file with the declared interface (run-command,
   http-post, fd watch, clock, checkpoint, exec, eval-lisp) and one Coalton
   module that calls it. Green build, no behaviour change.
3. `maxwell/cl` wrapper module: string, char, stream, and format calls that
   Coalton cannot reach directly, each a `lisp` body.
4. Port `json.lisp` to `json.ct`; the Lisp version stays until the tests pass
   against both.
5. Port `stats.lisp` and `session.lisp`; delete their Lisp versions.

After commit 5 the pattern is fixed and the remaining modules repeat it.
