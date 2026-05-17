# Implement Borge UI and Borge DB

## Goal
Implement the borge UI DSL and borge DB DSL as extensions to the borge language. Both are new sexp forms that live inside `.borg` files — the parser already handles them as generic sexp. We need to add semantic understanding: typed AST nodes, validation, JSON output, and generate subcommands.

## repo
`/home/roerick/dev/wyo.tech/borge/` on branch `master`

## Progress

### Done ✅
- [x] Create `lib/ui/` directory with `ui_ast.ml`, `ui_parse.ml`, `ui_validate.ml`, `ui_json.ml`
- [x] Implement `lib/ui/ui_ast.ml` — type definitions for all UI nodes (ui_element, component, layout_def, page, route, theme, variant, sizing, color_value, spacing_value, etc.)
- [x] Implement `lib/ui/ui_parse.ml` — sexp→UI AST extraction (parse_ui_element, parse_theme, parse_component, parse_layout_def, parse_page, parse_routes, parse_ui_app, parse_file)
- [x] Implement `lib/ui/ui_validate.ml` — semantic validation (currently stub, returns empty issues)
- [x] Implement `lib/ui/ui_json.ml` — JSON serialization for all UI AST types
- [x] Create `lib/db/` directory with `db_ast.ml`, `db_parse.ml`, `db_validate.ml`, `db_json.ml`
- [x] Implement `lib/db/db_ast.ml` — type definitions for all DB nodes (table_def, column, operations_def, relation_def, group_def, capability, crud_spec, etc.)
- [x] Implement `lib/db/db_parse.ml` — sexp→DB AST extraction (parse_table, parse_column, parse_operations, parse_relation, parse_group, parse_db_app, parse_file)
- [x] Implement `lib/db/db_validate.ml` — semantic validation (currently stub)
- [x] Implement `lib/db/db_json.ml` — JSON serialization for all DB AST types
- [x] Update `lib/dune` to include new modules (with include_subdirs unqualified)
- [x] Rename `lib/output/json.ml` → `json_out.ml` to avoid module name collision
- [x] Add UI/DB parse tests (2 tests, both passing)
- [x] Add `borge generate ui` subcommand
- [x] Add `borge generate db` subcommand
- [x] Update `lib.borg` spec with ui/ and db/ sections

### Remaining
- [ ] Implement semantic validation for UI (check use/fill references, variant uniqueness, theme var references)
- [ ] Implement semantic validation for DB (check references, ownership columns, group capabilities)
- [ ] Wire --json output for UI/DB sections in relevant commands
- [ ] More comprehensive tests for edge cases in UI/DB parsing

### Key Decisions
- Module naming: `ui_ast`, `ui_parse`, `ui_json`, `ui_validate` and `db_ast`, `db_parse`, `db_json`, `db_validate` — avoids conflicts with `json.ml` (now `json_out.ml`)
- `parse_property`, `parse_variant`, `parse_padding`, `parse_ui_element` are mutually recursive (`let rec ... and ...`)
- `parse_component`, `parse_layout_def`, `parse_page` are also `and` definitions in the recursive group
- Record field disambiguation in OCaml requires explicit type annotations when multiple record types share field names (`name`, `columns`, etc.)
- `db_ast.ml` uses `idx_method` instead of `method` (reserved keyword in OCaml objects)

## Validation
- `dune build` passes ✅
- `dune runtest` passes — 31 tests (29 original + 2 UI/DB) ✅
- `dune exec borge -- report` works ✅
- `dune exec borge -- generate ui --help` works ✅
- `dune exec borge -- generate db --help` works ✅
- `dune exec borge -- check .` passes ✅
- `dune exec borge -- lint .` clean ✅
