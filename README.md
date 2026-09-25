# borge

*a spec-driven code orchestrator*

Borge keeps specification and implementation aligned across commits — not by
orchestrating a swarm of agents, but by restricting what an agent is allowed
to do during each phase of work and by treating the spec as the source of truth.

Other tools solve agent alignment by spawning dozens of agents working in
parallel and at cross purposes. Borge aims for the same outcome with massively
fewer tokens, by gating what the agent may touch via simple, verifiable rules:

- **You write `.borg` files.** Every file is an s-expression document that is
  a git-tracked source of truth.
- **No unspec'd code.** A module gets a `.borg` section *before* it is written.
- **Phase-gated work.** During `plan` the agent edits only spec files. During
  `make` it edits only code — if the spec needs to change, it files an issue.
  During `review` it runs drift, lint, and check.
- **The gates are the spec.** Drift is detected mechanically, not by vibes.

## The workflow

1. Edit `.borg` files in your editor
2. `borge balance` — catch structural errors first
3. `borge fmt` — canonicalize indentation
4. `borge check` — validate the whole project
5. `borge report` — see what's done and what's next

Loop this. `borge execute` implements a planned section; `borge drift` and
`borge lint` confirm the result still matches the spec.

## Install

Requires OCaml ≥ 5.1 (dependencies are anything dune, cmdliner, yojson,
ppx_deriving, sedlex, menhir).

```sh
git clone https://github.com/sroerick/borge.git
cd borge
dune build
```

Optionally install the binary:

```sh
dune install
```

## Commands

Verification & health:

| Command | Purpose |
|---|---|
| `borge balance` | Check structural balance (run this FIRST after edits) |
| `borge parse` | Full syntax validation |
| `borge check` | Recursive health check (`--worktree` for CI) |
| `borge report` | Status summary dashboard |
| `borge lint` | Semantic validation (comment integrity, spec rules) |

Working with specs:

| Command | Purpose |
|---|---|
| `borge fmt` | Auto-format to canonical indentation |
| `borge normalize` | Fmt all + accept comment deletions |
| `borge inline` | Show the inline project tree |
| `borge nodes` | AST dump with positions |
| `borge stats` | Code intelligence metrics (lines, functions, exports) |

Agent loop:

| Command | Purpose |
|---|---|
| `borge plan` | Enter plan phase (spec edits only) |
| `borge make` | Enter make phase (code edits only) |
| `borge execute` | Execute (implement) a planned section |
| `borge generate` | Generate spec sections or code from specs |
| `borge review` | Advisory review (lint + drift) |
| `borge drift` | Detect spec/code/structural drift |

Git & history:

| Command | Purpose |
|---|---|
| `borge log` | Git log for `.borg` changes |
| `borge diff` | Git diff with status annotations |
| `borge commit` | Commit with phase enforcement |
| `borge merge` / `borge merge-queue` | Transactional merge |
| `borge undo` | Revert last commit |
| `borge book` | Print the codebase as a paginated PDF book |

Exit codes: `0` success · `1` issues found · `2` usage error.

## The `.borg` format

A borge project is a tree of `.borg` files. Every file must either be inlined
by a parent (`(inline "docs/agent.borg")`) or declare `(no-inline)` — files
with neither are orphans, i.e. structural drift. The inline tree is how status
counts, drift detection, and narrative rendering know what belongs to what.

Specs are s-expressions:

```lisp
(project my-app
  (status in-progress)
  (doc (|
      Free-verbatim documentation. Verbatim strings (|like this|)
      preserve newlines and need no escaping.
    |))
  (inline "docs/engine.borg")
  (module thing
    (status planned)
    (doc (|What this module does and why.|))))
```

Comments are first-class spec material. A plain comment is a machine note; an
annotated comment carries authorship and type and is checked by lint:

```lisp
(* roerick note (|a human wrote this; agents may not edit it|) *)
(* agent note (|agents sign their own work with `agent`|) *)
```

Borge understands the convention system too, via `(convention ocaml-dune)` in
the root spec — the same tools can analyze Go codebases with the
`convention-go` convention.

## The book

One concrete expression of borge's literate-programming philosophy is the
**book printer**: `borge book` renders the spec tree as a paginated PDF with
line numbers and wide margins. The table of contents is the project's inline
tree itself — the order the author wrote — with unreferenced code appended as
a lexicographic appendix. Comments should not be checkbox coverage; the whole
codebase should be readable as a book. This one is:

- [`borge-book.pdf`](borge-book.pdf) in the repo root — yes, borge has printed itself.

## Status

In progress, pre-1.0. The design documents live in `docs/*.borg` and are the
authoritative description of intended behavior; `lib.borg` is the
implementation index. If a feature isn't in a `.borg` file, it's out of scope.

## License

No license yet — all rights reserved by the author. Get in touch if you'd
like to use it.
