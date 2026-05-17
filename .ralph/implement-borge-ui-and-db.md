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
- [x] Add UI/DB parse tests (4 tests)
- [x] Add UI/DB validation tests (2 tests)
- [x] Add UI/DB edge case tests (2 tests: empty app, nested elements, minimal table, crud variants)
- [x] Add `borge generate ui` and `borge generate db` subcommands
- [x] Wire --json flag to generate ui/db (JSON output mode)
- [x] Human-readable summary output for generate ui/db (tables, components, layouts, pages, routes, validation issues)
- [x] Update `lib.borg` spec with ui/ and db/ subdirectory sections

### Remaining (nice-to-have, deferred)
- [ ] Initial LLM prompt construction for generate ui/db (currently outputs summary or JSON)
- [ ] SQL migration generator target for db
- [ ] HTML/CSS generator target for ui

## Validation
- `dune build` passes ✅
- `dune runtest` passes — 37 tests total ✅
- `borge generate ui --json` outputs structured JSON ✅
- `borge generate ui` outputs human-readable summary ✅
- `borge generate db --json` outputs structured JSON ✅
- `borge generate db` outputs human-readable summary ✅
- `borge report` works ✅

## Key Commits
- `966ccd0` implement UI and DB typed ASTs, parsers, validators, JSON serializers
- `4ab3ad5` add UI/DB parse tests, fix json_out rename
- `03419d6` update lib.borg with ui/ and db/ subdirectory sections
- `2f9cb6e` add generate ui and generate db subcommands
- `e6ef4f6` implement UI and DB semantic validators with tests
- `8ba97a2` add --json flag to generate ui/db, human-readable summaries, edge case tests
