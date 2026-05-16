# Implement All Planned Borge Features

## Goals
- Implement every `(status planned)` section in borge.borg and meta.borg
- Each feature: write code, verify it builds, mark section `(status implemented)`
- Zero drift after all changes

## Checklist
- [x] `borge stats` — code intelligence dashboard (lines, functions, avg-length, exports, doc_coverage)
- [x] `borge generate spec` — turn todo text into .borg section via LLM
- [x] `borge generate code` — implement planned section from spec via pi
- [x] `borge execute` — alias for `borge generate code`
- [x] `borge review --agent` — per-file code quality assessment via LLM
- [x] Update borge.borg sections to `(status implemented)` as each feature lands
- [x] Update meta.borg metrics section to `(status implemented)` when stats lands
- [x] `borge drift` passes with zero findings after all changes
- [x] `borge balance` + `borge check` pass on all .borg files
- [x] cli.borg: add stats, generate-spec, generate-code, execute subsections
- [x] lib.borg: add stats, generate, review-quality sections
- [ ] `(section code-documentation)` — doc_coverage needs proper nested-comment handling
- [ ] `(section format / error-catalog / failure-modes)` — spec docs, not code features
- [ ] nvim-borge: 23 planned ask-response features (separate project)

## Verification
- `dune build` passes ✓
- `dune runtest` all 22 tests pass ✓
- `borge balance` + `borge check` all 7 .borg files pass ✓
- `borge drift` 0 findings ✓
- Completion: 72.7% (96/132)

## Notes
- All borge-subcommand features implemented: stats, generate spec/code, execute, review --agent
- Doc coverage is a stub (count_documented returns 0) — needs real lexer for nested comments
- (section format), (section error-catalog), (section failure-modes) are spec documentation,
  not implementable code features. Will remain planned until spec text is written.
- nvim-borge's 23 planned sections are for the ask-response feature (separate repo)
- (section future-work / parser-rewrite) is a future project
