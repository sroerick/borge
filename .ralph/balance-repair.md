# Implement borge balance --repair and enriched JSON output

## Goal
Make `borge balance` help agents recover from paren imbalance by treating indentation as a second source of structural truth. Follow the spec-first discipline.

## Tasks

### Phase 1: Spec update ✅
- [x] Update `sexp.borg` section `balance` with new subsections:
  - `indent-repair`: `--repair` flag that infers `)` from indentation
  - `enriched-json`: structured tree + repair_actions in JSON output
- [x] Update `.pi/skills/borge-workflow/SKILL.md` to recommend `--repair`

### Phase 2: Implementation ✅
- [x] Add `repair` module to `lib/lang/` with indentation-tree algorithm
- [x] Add `--repair` flag to CLI `bin/cmd/Balance.ml`
- [x] Add `--repair-diff` flag for diff output of proposed fixes
- [x] Enrich `Json_out.balance` with `structural_tree` and `repair_actions`
- [x] Wire `--json` output through `analyze_enriched` to include enriched data
- [x] Update test suite with repair cases (5 new tests)

### Phase 3: Fmt resilience ✅
- [x] Spec integration in `cli.borg` for `fmt --repair`
- [x] Make `fmt` fall back to balance repair when parse fails
- [x] `borge fix` equivalent: `borge fmt --repair FILE` + `borge balance --repair FILE`

### Phase 4: Verify ✅
- [x] All tests pass: `dune runtest` (15/15 green)
- [x] Manual test on broken .borg files
- [x] Verify `--repair` produces balanced output
- [x] Verify `--json` includes tree + actions on imbalanced files

## Files modified

| File | Change |
|------|--------|
| `sexp.borg` | Added `indent-repair` + `enriched-json` subsections (status implemented) |
| `cli.borg` | Added `fmt-repair` subsection (status implemented) |
| `.pi/skills/borge-workflow/SKILL.md` | Added auto-repair step to workflow |
| `lib/lang/balance.ml` | Added `repair`, `scan_line_events`, `compute_repair_actions`, `apply_repair_actions`, `analyze_enriched` |
| `lib/output/json_out.ml` | Added `balance_enriched`, `structural_node`, `indent_divergence`, `repair_action` |
| `bin/cmd/Balance.ml` | Added `--repair`, `--repair-diff` flags |
| `bin/cmd/Fmt.ml` | Added `--repair` flag with fallback on parse errors |
| `test/test_balance.ml` | Added 5 tests for repair + enriched JSON |
