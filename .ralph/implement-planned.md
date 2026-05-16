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
- [x] `borge balance` + `borge check` pass after all changes
- [ ] `borge drift` passes with zero findings after all changes
- [ ] `(section code-documentation)` — doc coverage heuristic needs proper nested-comment handling
- [ ] `(section format / error-catalog / failure-modes)` — spec docs, no code to implement

## Commands & Conventions
- Build: `dune build` — Test: `dune runtest` — Clean: `dune clean`
- Run: `dune exec borge <cmd>` or `dune exec borge -- <cmd> <args>`
- Pi invocation: `pi -p --no-session --no-tools` (see agent.ml for pattern)
- All new commands follow the bin/cmd/X.ml + bin/borge_X.ml pattern
- Library code goes in lib/borge/

## Verification
- `dune build` passes ✓
- `dune runtest` all 22 tests pass ✓
- `borge balance` + `borge check` pass on all .borg files ✓

## Notes
- Implemented: stats, generate spec/code, execute, review --agent
- Stats has doc_coverage field but count_documented is a stub (0) — nested OCaml
  comments make simple *) scanning unreliable. Need proper lexer-based approach.
- (section format), (section error-catalog), (section failure-modes) are spec
  documentation sections — no code to implement, just spec text to write.
- (section future-work / parser-rewrite) is a future project, not current work.
