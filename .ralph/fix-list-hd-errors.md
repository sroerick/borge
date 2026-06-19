# Fix List.hd/List.tl Errors

Fix all 6 mechanical code quality errors flagged by `borge lint --mechanical`.
These are real `Failure` exceptions waiting to happen.

## Goals
- Replace every `List.hd` with safe pattern matching
- Replace `List.tl` with safe pattern matching
- Verify no new errors after each fix
- Ensure build and tests pass

## Checklist
- [x] Fix `lib/lang/balance.ml:56` — `List.hd (List.rev frames)` in `string_of_error`
- [x] Fix `lib/lang/balance.ml:600` — `List.hd (List.rev frames)` in `report_file`
- [x] Fix `lib/lang/print.ml:76` — `List.tl children`
- [x] Fix `lib/review/review_agent.ml:57` — `List.hd functions` (guarded by length=1)
- [x] Fix `test/test_ui_db.ml:179` — `List.hd a.Db_ast.tables`
- [x] Fix `test/test_ui_db.ml:182` — `List.hd t.Db_ast.columns`

## Verification
- `dune build` compiles cleanly ✓
- `dune runtest` passes (all test suites) ✓
- `borge lint --mechanical .` shows 0 List.hd/List.tl errors ✓

## Notes
- `lib/lang/balance.ml` uses `List.hd (List.rev frames)` because `frames` is ordered
  outermost-first, and the error message wants the "first opened" (outermost).
  Pattern-match on `List.rev frames` directly.
- `lib/lang/print.ml:76` uses `List.tl` to drop the keyword for recursive printing.
  Pattern-match on `children` list.
