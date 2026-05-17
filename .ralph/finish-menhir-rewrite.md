# Finish the menhir/sedlex parser rewrite for borge

## Goal
The menhir/sedlex rewrite is mechanically complete but has two bugs preventing it from replacing the old hand-written parser. Fix them, make all tests pass, and verify the full borge pipeline works.

## Checklist
- [x] Fix `peek` in lexer.ml (Sedlexing.start before next) — DONE
  - Root cause: `Sedlexing.rollback` rewinds to last `start`/`mark` checkpoint, not just one char back. Without calling `Sedlexing.start` first, it rewound to position 0, causing infinite loop returning same token.
  - Fix: added `Sedlexing.start lexbuf;` before `Sedlexing.next` in `peek` function
- [x] Fix menhir grammar to handle comments inside list forms — DONE
  - Root cause: `sexp_list` rule only accepted `sexp` items, not comments. Annotated comments inside `(project ...)` lists caused parse errors.
  - Fix: added `inner_comment_list` rule that consumes/discards comments between list items
- [x] Make all parse tests pass (ask/response fails) — DONE
  - All 12 parse tests pass, including ask/response with empty verbatim `(||)` and consecutive comments
- [x] Verify `borge check` and `borge report` work on the project's own .borg files — DONE
  - check: 6 files, 6 passed, 0 failed
  - report: 98 implemented, 5 in-progress, 5 planned, 90.7% completion
- [x] Run `borge balance` on all .borg files to verify — DONE
  - All 6 .borg files balanced
- [x] Run `dune runtest` — all green — DONE
  - 29 tests across 4 suites: balance (7), meta (4), parse (12), dune+surface (6)
