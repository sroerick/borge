
# Implement the DB DSL for Borge

## Core Principle
The db.borg spec is the source of truth. The DB DSL does NOT generate SQL or code — "the spec IS the artifact." What exists in `bin/cmd/Generate.ml` as `run_db` / `db_cmd` is code drift: it's framed as "generate code from a DB spec section" which contradicts the spec philosophy.

## Current State
- **lib/db/**: `db_ast.ml`, `db_parse.ml`, `db_validate.ml`, `db_json.ml` — all exist and work. These are CORRECT: they parse, validate, and serialize DB specs. Build passes.
- **bin/cmd/Generate.ml**: ~~Contains `run_db` and `db_cmd` that frame DB as code generation~~ → FIXED: now framed as "inspect a DB/UI spec section"
- **db.borg**: 10/15 sections now `(status implemented)`, 5 remain `(status planned)` (correctly).

## Checklist

### Phase 1: Fix drift in Generate.ml ✅
- [x] Refactor `run_db` in Generate.ml: remove "generate code" framing. It should inspect a DB spec (find it, parse it, print summary + validation issues, optional --json). This is what it already DOES, but the docstrings/command descriptions are wrong.
- [x] Update `db_cmd` info/doc to say "inspect a DB spec section" not "generate code from a DB spec section"
- [x] Update the `cmd` group's man page to match
- [x] Similarly audit `run_ui` / `ui_cmd` — same pattern, also says "generate code" but just inspects. Fix docstrings to match reality.

### Phase 2: Update db.borg statuses ✅
- [x] `(section relationship-to-borge)` → `(status implemented)` — parse, validate, json, and CLI inspect all work
- [x] `(section schema)` → `(status implemented)` — table/column/index parsing works in db_parse.ml
- [x] `(subsection column-types)` → `(status implemented)` — types parsed as strings
- [x] `(subsection indexes)` → `(status implemented)` — index_def and parse_index work
- [x] `(section operations)` → `(status implemented)` — crud/query parsing works
- [x] `(section relations)` → `(status implemented)` — relation_def with joins works
- [x] `(section ownership)` → `(status implemented)` — ownership field on table_def works
- [x] `(section groups)` → `(status implemented)` — group/capability parsing works
- [x] `(subsection group-resolution)` → `(status planned)` — stays planned (convention-specific, no implementation)
- [x] `(section data-integration)` → `(status planned)` — stays planned (ui<>db cross-ref, not implemented)
- [x] `(section full-example)` → `(status planned)` — stays planned (example, no code)
- [x] `(section convention-targets)` → `(status planned)` — stays planned (code generation targets, explicitly not implemented)
- [x] `(section agentic-editing)` → `(status implemented)` — borge make already works for DB sections
- [x] `(section future-work)` → `(status planned)` — stays planned

### Phase 3: DB-aware lint and check ✅
- [x] Add DB-specific lint checks to `lib/check/lint.ml`:
  - Warn if (ownership column) references a column not on the table
  - Warn if (operations table) references a table that doesn't exist in the same db form
  - Warn if (references table.col) table doesn't exist
  - (These already exist in db_validate.ml — now wired up via `Db_validation` lint variant + `check_db_specs`)
- [x] Add DB awareness to `bin/cmd/Nodes.ml`: confirmed existing generic sexp walker already handles DB forms correctly (shows `(db ...)`, `(table ...)`, etc. with keyword names)
- [x] Add DB section counting to `lib/check/report.ml` — confirmed it already counts (status ...) inside db sections correctly (verified: db.borg shows 10 implemented, 5 planned)

### Phase 4: Verify and build ✅
- [x] `dune build` passes
- [x] `dune runtest` passes (8 tests, all OK)
- [x] `borge check .` passes (8/8 files)
- [x] `borge lint .` passes (0 errors, 0 warnings)
- [x] `borge report .` shows correct counts (db.borg: 10 implemented, 5 planned)
- [x] `borge generate db --help` shows "inspect a DB spec section" (not "generate code")

## Files Changed
- `bin/cmd/Generate.ml` — reframed ui_cmd and db_cmd as inspect commands
- `db.borg` — 10 sections: planned → implemented
- `lib/check/lint.ml` — added Db_validation variant and check_db_specs wiring
- `bin/cmd/Lint.ml` — added Db_validation print case
- `bin/borge_lint.ml` — added Db_validation print case
- `bin/borge_review.ml` — added Db_validation print case
- `lib/output/json_out.ml` — added Db_validation JSON case

## Result
- db.borg: **10/15 implemented** (up from 0/15)
- Project overall: **73.3%** (up from 67.6%)
- All builds, tests, check, lint, balance pass
