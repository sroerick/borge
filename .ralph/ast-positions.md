# AST Position Tracking + borge nodes + balance-verbose

## Goals
- Add source position tracking to every sexp AST node ✅
- Build `borge nodes` CLI to dump AST with positions ✅
- Add `--verbose` flag to `borge balance` for paren-stack error reporting ✅
- All existing tests continue to pass throughout ✅

## Checklist

### Phase 1: Position type and AST changes ✅
- [x] Add `pos` type to `lib/sexp/ast.ml` — `{ line: int; col: int; offset: int }`
- [x] Add pos to sexp variants: `Atom of pos * symbol | String of pos * string_value | List of pos * sexp list`
- [x] Update `sexp_with_comments` to carry `end_pos` field

### Phase 2: Parser refactor for positioned AST ✅
- [x] Refactor `parse_sexp` to return positioned nodes (every constructor gets a pos)
- [x] Refactor `parse_list` to track the opening paren position
- [x] Add `current_pos` helper and `end_pos_of_sexp` helper
- [x] Ensure `parse_annotated_comment` positions still work

### Phase 3: Printer adaptation ✅
- [x] Update `print_sexp` to unpack pos tuples (printer ignores pos, just formats)
- [x] Add `_` to sexp_with_comments pattern to ignore end_pos
- [x] Verify `borge fmt` round-trips cleanly on all 6 .borg files

### Phase 4: Spec extraction and downstream fixes ✅
- [x] Update `lib/borge/spec.ml` to work with positioned AST (pos in pattern matches)
- [x] No changes needed to check.ml, report.ml, fmt.ml

### Phase 5: All existing tests pass ✅
- [x] 8 parse tests pass
- [x] 7 balance tests pass
- [x] `borge fmt` round-trips cleanly on all 6 .borg files
- [x] `borge balance` passes on all 6 .borg files
- [x] `borge check` passes on all 6 .borg files
- [x] `borge fmt --check` passes on all 6 .borg files

### Phase 6: `borge nodes` CLI ✅
- [x] Add `lib/borge/nodes.ml` — walk AST, collect node info
- [x] Add `bin/borge_nodes.ml` — CLI that calls lib and prints
- [x] Update `bin/dune` with new executable
- [x] Update `lib/borge/dune` with new module
- [x] Test: `borge nodes borge.borg` shows positioned AST dump (325 nodes)

### Phase 7: `borge balance --verbose` ✅
- [x] Add `--verbose` flag to `borge balance` via cmdliner
- [x] Add `paren_frame` type with `open_pos` and `keyword` fields
- [x] Add `peek_keyword` helper to detect keyword after `(`
- [x] Track paren stack as `paren_frame list` in balance checker
- [x] `Unclosed_parens` now carries full `paren_frame list` instead of just count
- [x] Verbose mode prints paren stack: `(1:1) project` format
- [x] Test: broken file shows full paren stack with keywords

### Phase 8: Update specs ✅
- [x] Mark `positions` section in sexp.borg as implemented
- [x] Mark `nodes` section in cli.borg and borge.borg as implemented
- [x] Mark `balance-verbose` in cli.borg and borge.borg as implemented
- [x] Mark `fmt-check` in cli.borg and borge.borg as implemented
- [x] Mark `ast-position-tracking` in borge.borg as implemented
- [x] Run `borge fmt` on all updated .borg files
- [x] Final verification: build, test, balance, check, report, fmt --check, nodes, balance --verbose

## Verification
- All 15 tests pass
- `borge nodes borge.borg` → 325 nodes with positions
- `borge balance --verbose` on broken file shows paren stack with keywords
- Report: 30 implemented | 1 in-progress | 59 planned | 33.3% completion
- All 7 CLI commands work: balance, parse, check, report, fmt, nodes, balance --verbose
