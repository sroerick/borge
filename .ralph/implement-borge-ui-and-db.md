# Implement Borge UI and Borge DB

## Goal
Implement the borge UI DSL and borge DB DSL as extensions to the borge language. Both are new sexp forms that live inside `.borg` files — the parser already handles them as generic sexp. We need to add semantic understanding: typed AST nodes, validation, JSON output, and generate subcommands.

## repo
`/home/roerick/dev/wyo.tech/borge/` on branch `master`

## Current state
- Parser (menhir/sedlex) produces a generic `Ast.sexp` tree: Atom, String, List
- `Spec.ml` walks the sexp tree to extract status, project_name, section counts, etc.
- All 29 tests pass, all commands work
- `ui.borg` (24 planned sections) and `db.borg` (15 planned sections) define the DSL specs

## Implementation plan — TWO TRACKS

### Track 1: Borge UI (lib/ui/)

**New modules in `lib/ui/`:**

1. **`lib/ui/ast.ml`** — Typed UI AST. Walks the generic sexp tree and extracts typed UI nodes:
   - `ui_element`: name, layout, width, height, padding, gap, bg, color, border, corner, scroll, float, interactive, children, variants
   - `component_def`: name, properties, slots, variants
   - `layout_def`: name, root element, slots
   - `page_def`: name, layout reference, slot fills
   - `theme_def`: palette entries, spacing entries, font-size entries, radius entries
   - `route_def`: path, page name

2. **`lib/ui/parse.ml`** — Sexp→UI AST extraction. Functions like `parse_ui_element : sexp -> ui_element option`, `parse_theme : sexp -> theme_def option`, etc. Walks the generic AST and produces typed UI nodes.

3. **`lib/ui/validate.ml`** — Semantic validation. Checks that `(use button)` refers to a defined component, `(fill header-content)` refers to an existing slot, variant names are unique per element, etc.

4. **`lib/ui/json.ml`** — JSON serialization of UI AST for `--json` flag

### Track 2: Borge DB (lib/db/)

**New modules in `lib/db/`:**

1. **`lib/db/ast.ml`** — Typed DB AST. Walks the generic sexp tree and extracts typed DB nodes:
   - `table_def`: name, columns list
   - `column_def`: name, type, constraints (primary_key, unique, not_null, default, references)
   - `operations_def`: table name, crud spec, queries
   - `relation_def`: name, from table, joins, where, select, queries
   - `ownership_def`: column name
   - `group_def`: name, capabilities list
   - `capability`: table, operations, where condition

2. **`lib/db/parse.ml`** — Sexp→DB AST extraction. Functions like `parse_table : sexp -> table_def option`, `parse_column : sexp -> column_def option`, etc.

3. **`lib/db/validate.ml`** — Semantic validation. Checks that `(references users.id)` refers to an existing table, `(ownership author-id)` refers to an existing column on the same table, group capability tables exist, etc.

4. **`lib/db/json.ml`** — JSON serialization of DB AST for `--json` flag

### Shared changes:
- Update `lib/dune` to include new modules from `lib/ui/` and `lib/db/`
- Add `borge generate ui` and `borge generate db` subcommands (in `bin/cmd/`)
- Update `lib.borg` spec to reflect new subdirectories

## Design principles
- UI and DB modules are **consumers** of the existing generic sexp AST — they do NOT modify the parser
- The parser stays generic. UI/DB parsing is sexp → typed extraction
- All modules use `open Borge_lang.Ast` to work with the sexp tree
- Follow existing patterns: look at how `spec.ml`, `report.ml`, `check.ml` walk the sexp tree
- `include_subdirs unqualified` means new subdirs are flat modules in `borge_lib`

## Validation
- `dune build` passes
- `dune runtest` passes (all 29 existing tests + any new tests)
- `dune exec borge -- report` works
- New `borge generate ui` and `borge generate db` commands exist (even if initially stub)
- UI and DB ASTs can parse examples from ui.borg and db.borg full-example sections

## Checklist (in order)
- [ ] Create `lib/ui/` directory with `ast.ml`, `parse.ml`, `validate.ml`, `json.ml`
- [ ] Implement `lib/ui/ast.ml` — type definitions for all UI nodes
- [ ] Implement `lib/ui/parse.ml` — sexp→UI AST extraction
- [ ] Implement `lib/ui/validate.ml` — semantic validation
- [ ] Implement `lib/ui/json.ml` — JSON output
- [ ] Create `lib/db/` directory with `ast.ml`, `parse.ml`, `validate.ml`, `json.ml`
- [ ] Implement `lib/db/ast.ml` — type definitions for all DB nodes
- [ ] Implement `lib/db/parse.ml` — sexp→DB AST extraction
- [ ] Implement `lib/db/validate.ml` — semantic validation
- [ ] Implement `lib/db/json.ml` — JSON output
- [ ] Update `lib/dune` to include new modules
- [ ] Add `borge generate ui` command
- [ ] Add `borge generate db` command
- [ ] Update `lib.borg` spec
- [ ] Verify all tests pass and commands work
