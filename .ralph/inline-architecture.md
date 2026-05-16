# Inline Architecture: Parse inline directives, build project tree, fix report

## Goals
- Parse `(inline filename.borg)` directives from the AST ✅
- Build the inline tree from a root .borg file ✅
- Fix report double-counting (walk inline tree, count each section once) ✅
- Add orphan detection to `borge check` ✅
- Add `borge inline` CLI to show the project tree ✅
- All existing tests continue to pass ✅

## Checklist

### Phase 1: Extract inline targets from AST ✅
- [x] Add `inline_targets` to `lib/borge/spec.ml` — extract list of filenames from `(inline ...)` forms
- [x] Add `has_no_inline` to `lib/borge/spec.ml` — detect `(no-inline)` declaration
- [x] Add 4 tests for inline_targets and has_no_inline

### Phase 2: Build the project tree ✅
- [x] Add `lib/borge/project.ml` — types: `project_node`, `build_error`, `build_tree`
- [x] `build_tree` recursively follows inline directives, resolves relative paths
- [x] Cycle detection via `visited` parameter
- [x] `find_roots` — find root .borg files (not inlined by any other)
- [x] `find_orphans` — find files not in any tree, no `(no-inline)`, and don't inline others
- [x] `tree_paths` — collect all paths in tree (deduplicated)
- [x] `tree_status_counts` — count statuses across tree without double-counting
- [x] `format_tree` — indented text representation

### Phase 3: Fix report double-counting ✅
- [x] Refactor `lib/borge/report.ml` to build tree from roots, walk tree paths
- [x] Add `orphans` field to `Report.result`
- [x] Report now only counts files in the inline tree
- [x] Bin file displays orphan warnings

### Phase 4: Orphan detection in check ✅
- [x] Add `warning` type to `lib/borge/check.ml` with `Orphan` variant
- [x] `Check.run` calls `Project.find_orphans`
- [x] Bin file displays warnings with ⚠ prefix
- [x] Verified: orphan.borg in /tmp/orphandir triggers warning correctly

### Phase 5: `borge inline` CLI ✅
- [x] Add `bin/borge_inline.ml` — print the inline tree
- [x] Update `bin/dune` with new executable
- [x] Handles errors: file not found, parse error, cycle detected
- [x] Output: `borge.borg (borge)` with indented children

### Phase 6: Handle directory argument for tree root ✅
- [x] `Project.find_roots` finds root candidates (files not inlined by any other)
- [x] Works correctly for the real project (borge.borg is the sole root)

### Phase 7: Update specs and verify ✅
- [x] Mark `program-architecture` in borge.borg as implemented
- [x] Mark `file-utils` in lib.borg as implemented
- [x] Add `project` section to lib.borg (implemented)
- [x] Add `inline` command to cli.borg (implemented)
- [x] Run `borge fmt` on all updated .borg files
- [x] All 19 tests pass (12 parse + 7 balance)
- [x] All 8 CLI commands work

## Verification
- 34 implemented | 1 in-progress | 57 planned | 37.0% completion (up from 33.3%)
- `borge report .` counts each file once through inline tree
- `borge check .` reports 0 warnings (no orphans)
- `borge inline .` shows the project tree correctly
- Orphan detection works (tested with /tmp/orphandir)
