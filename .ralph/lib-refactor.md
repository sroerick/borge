# Refactor bin/ → lib/ so lib owns domain logic

## Goal
Move all domain logic from bin/ into lib/ so that:
- lib/borge/ owns: file utils, check results, report data, fmt operations
- bin/ owns: cmdliner args → lib call → print → exit
- No duplicated utilities across bin/ files
- Add `borge fmt --check` while we're at it

## Checklist

### Phase 1: Shared utilities into lib
- [x] Add `lib/borge/file_utils.ml` — find_borg_files, read_file
- [x] Export from borge_lib, remove duplication from bin/

### Phase 2: Check refactor
- [x] Add `lib/borge/check.ml` with typed result type (Check.result)
- [x] Check.run returns typed results, bin/borge_check.ml calls + prints
- [x] Remove reimplementation of project_name in check (use Spec.project_name)
- [x] Migrate borge_check.ml to cmdliner (was hand-rolled Sys.argv parsing)

### Phase 3: Report refactor
- [x] Add `lib/borge/report.ml` with typed result type (Report.result)
- [x] Report.run returns per-file data + totals, bin/borge_report.ml formats + prints
- [x] Separate data extraction from presentation

### Phase 4: Fmt refactor
- [x] Add `lib/borge/fmt.ml` — parse + print, returns string
- [x] bin/borge_fmt.ml: args → Fmt.format → output
- [x] Add --check flag (compare to current, exit 0/1)
- [x] Proper cmdliner arg parsing

### Phase 5: Wire up and verify
- [x] Update lib/borge/dune with new modules
- [x] All 15 tests still pass
- [x] All 6 .borg files still balance/parse/check
- [x] borge fmt --check works
- [x] borge report output unchanged

## Issues Fixed This Session
- Fixed `Unbound record field "project_name"` — needed explicit type annotation `(r : Report.file_stats)` for OCaml to resolve record fields from `open Borge_lib`
- Fixed Warning 8 (partial-match) in borge_fmt.ml — both `check` and `format` branches now handle all variants of `fmt_result`
- Fixed fmt --check false positive: `Print.print_file` produces 1 fewer newline than the file has, so `check_file` with raw string comparison always returned `CheckDirty`. Fixed by stripping trailing newlines from both sides before comparing.

## Remaining / Future
- The 1-byte trailing newline difference between format_string output and file is benign: `print_file` adds 1 trailing newline per top-level form, files on disk may have 2 (standard blank line at end). `strip_trailing_newlines` in check_file is the correct approach since trailing whitespace is insignificant in sexp format.
- Could add `borge fmt --diff` (show unified diff between current and formatted)
- Could add .mli interface files for lib modules to enforce the bin/lib boundary at compile time

## All Phases Complete ✅
