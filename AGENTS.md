# Borge Project — Agent Instructions

## Spec-First Discipline

This project practices what it preaches: **every module must have a `.borg` section before it is written.** See `borge.borg` section `agent-discipline` for the full rules. The short version:

1. **No unspec'd code.** If a module has no `.borg` section, don't write it. Add the section first.
2. **Read the spec before building.** Check `.borg` files for `(status planned)` with detailed docs — those are design documents. Features not in any `.borg` file are out of scope.
3. **Follow existing patterns.** If the codebase uses `Agent.run_pi_print` for LLM-mediated generation and the spec says the same, follow that pattern. Don't invent alternatives unless the spec calls for them.
4. **Update spec status after implementation.** Change `(status planned)` → `(status implemented)` in the relevant `.borg` file. Add subsections for new modules.
5. **Check against spec during loops.** In Ralph loops or multi-step tasks, periodically verify: does my task list match the `.borg` files? If a task has no spec section, stop and ask.

## Key Spec Files

- `borge.borg` — root architecture, tool pipeline, agent discipline rules
- `ui.borg` — UI DSL design (renderer-agnostic, LLM-mediated generation)
- `db.borg` — DB DSL design (Postgres-first, RLS, LLM-mediated generation)
- `lib.borg` — module-level specs for `lib/` (each subsection = one module)
- `cli.borg` — CLI command specs
- `sexp.borg` — borge language parser/lexer/AST specs

## Known Gotchas

- OCaml record field disambiguation: when multiple record types share field names (`name`, `columns`), use explicit type annotations on lambda params
- `method` is a reserved keyword in OCaml — use `idx_method` or similar
- `include_subdirs unqualified` in `lib/dune` means all modules are flat in `borge_lib` — prefix module filenames to avoid collisions (e.g. `ui_ast.ml`, `db_ast.ml`, `json_out.ml`)
