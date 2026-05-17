# Implement Borge UI and Borge DB

## Goal
Implement the borge UI DSL and borge DB DSL as extensions to the borge language.

## repo
`/home/roerick/dev/wyo.tech/borge/` on branch `master`

## Progress

### Done ✅
- [x] Create `lib/ui/` directory with `ui_ast.ml`, `ui_parse.ml`, `ui_validate.ml`, `ui_json.ml`
- [x] Implement `lib/ui/ui_ast.ml` — type definitions for all UI nodes
- [x] Implement `lib/ui/ui_parse.ml` — sexp→UI AST extraction
- [x] Implement `lib/ui/ui_validate.ml` — semantic validation (checks palette/spacing/font-size/radius refs, variant uniqueness, component/layout name uniqueness, page layout refs)
- [x] Implement `lib/ui/ui_json.ml` — JSON serialization
- [x] Create `lib/db/` directory with `db_ast.ml`, `db_parse.ml`, `db_validate.ml`, `db_json.ml`
- [x] Implement `lib/db/db_ast.ml` — type definitions for all DB nodes
- [x] Implement `lib/db/db_parse.ml` — sexp→DB AST extraction
- [x] Implement `lib/db/db_validate.ml` — semantic validation (checks table/column/group uniqueness, foreign key refs, ownership, operations table refs, relation table refs, group capability table refs)
- [x] Implement `lib/db/db_json.ml` — JSON serialization
- [x] Update `lib/dune` to include new modules
- [x] Add UI/DB parse tests (2 tests)
- [x] Add UI/DB validation tests (2 tests)
- [x] Add `borge generate ui` subcommand
- [x] Add `borge generate db` subcommand
- [x] Update `lib.borg` spec with ui/ and db/ sections

### Remaining (lower priority)
- [ ] Wire --json output for UI/DB sections in dedicated commands
- [ ] More comprehensive tests for edge cases
- [ ] Initial LLM prompt construction for generate ui/db (currently outputs JSON of parsed spec)

## Validation
- `dune build` passes ✅
- `dune runtest` passes — 33 tests (29 original + 4 UI/DB) ✅
- `dorge exec borge -- report` works ✅
- `borge generate ui --help` and `borge generate db --help` work ✅
- `borge check .` and `borge lint .` clean ✅

## Key Commits
- `966ccd0` implement UI and DB typed ASTs, parsers, validators, JSON serializers
- `4ab3ad5` add UI/DB parse tests, fix json_out rename
- `03419d6` update lib.borg with ui/ and db/ subdirectory sections
- `2f9cb6e` add generate ui and generate db subcommands
- `e6ef4f6` implement UI and DB semantic validators with tests
