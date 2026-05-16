# Implement Everything (OCaml scope)

## Goals ✅
Implement all remaining planned features achievable in OCaml. Target: 37% → 59.2% completion.

## Checklist

### 1. Small standalone CLIs ✅
- [x] `borge version` — print version string (0.1.0)
- [x] `borge print` — parse and re-print (canonical round-trip)
- [x] `borge fmt --diff` — show unified diff between current and formatted

### 2. `borge lint` — semantic validation ✅
- [x] Invalid status value detection
- [x] Duplicate project name detection
- [x] Unknown comment type detection
- [x] Missing comment value on typed comments
- [x] Comment integrity: compare against git HEAD for human comment deletions
- [x] Pending-response check: find ask comments with empty response slots
- [x] Orphaned file detection
- [x] No-inline-on-root detection

### 3. `borge drift` — three drift layers ✅
- [x] Spec drift: sections marked implemented but not in codebase
- [x] Code drift: unspecified .ml files + undocumented let bindings
- [x] Structural drift: domain logic in wrong layer (bin/ vs lib/)

### 4. `borge normalize` ✅
- [x] Fmt all .borg files via the project tree
- [x] Reports formatted vs unchanged count

### 5. `borge check --worktree` ✅
- [x] Check: git diff is clean
- [x] Check: borge balance passes on all .borg files
- [x] Check: borge check passes
- [x] Check: dune build succeeds
- [x] Check: dune runtest passes
- [x] Pass/fail exit code

### 6. `borge review` ✅
- [x] Runs lint + drift checks
- [x] Advisory output (not pass/fail)
- [x] Summary with total item count

### 7. History semantic operations ✅
- [x] `borge log` — git log filtered to .borg file changes
- [x] `borge diff` — git diff for .borg files with status annotations
- [x] `borge undo` — git revert HEAD (convenience command)

### 8. Output conventions ✅
- [x] Exit codes: 0 success, 1 issues, 2 usage error (cmdliner standard)
- [x] Human-readable terminal output on all commands
- [ ] `--json` flag (deferred — needs yojson integration per-command)
- [ ] `--quiet` flag (deferred — trivial but repetitive)

### 9. Update all specs ✅
- [x] cli.borg: marked 17 sections implemented, added history-commands section
- [x] lib.borg: marked lint and drift as implemented, added full details
- [x] history.borg: marked all 4 sections as implemented
- [x] All .borg files formatted and passing fmt --check

## Verification
- All 19 tests pass (12 parse + 7 balance)
- 15 CLI commands: balance, parse, check, report, fmt, nodes, inline, version, print, lint, drift, normalize, review, log, diff, undo
- `borge check --worktree` runs deterministic checks
- `borge review` runs advisory review
- All 6 .borg files balance, check, and fmt --check clean
- Completion: 37.0% → 59.2%

## Out of Scope (deferred)
- Neovim plugin (23 items — entire lua codebase)
- `--json` and `--quiet` flags (2 items — cross-cutting, repetitive)
- `borge expand` (1 item — ai-prompt directives, speculative)
- Parser rewrite (sedlex+menhir)
