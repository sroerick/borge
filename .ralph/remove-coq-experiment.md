# Remove the Coq proof experiment from borge

## Context
The Coq proof experiment answered its question ("can borge do formal verification?") with a yes, then taught us the right tool for borge's actual invariants is types, not Coq proofs over stringly-typed data. Keeping it means maintaining an emitter, runner, .borg.meta format, CLI, lint rules, lint rules' lint rules, and a witness — all to discharge one property on an example app. Removing it.

## Rules
- Keep `(ownership ...)` DB declarations — that's a DB feature, not a proof feature
- Move proof-related spec sections to `(status planned)` with an agent note: "Removed for lack of utility — see git history. The right tool for borge's invariants is types (typed operations, phantom resolved/unresolved refs), not Coq proofs over stringly-typed data."
- Delete all proof code, tests, CLI, witness, emitted artifacts, .borg.meta
- One focused removal commit at the end
- Verify: dune build, dune runtest, borge check all pass

## Steps

### Code removal (delete files + references)
- [ ] Delete `lib/proof/` (proof_emit.ml, proof_run.ml, proof_meta.ml, proof.borg)
- [ ] Delete `lib/dune` proof modules line: `proof_emit proof_run proof_meta` + comment
- [ ] Delete `bin/cmd/Proof.ml`
- [ ] Edit `bin/cmd/dune`: remove `Proof` from modules list
- [ ] Edit `bin/borge.ml`: remove `Proof.cmd;` line
- [ ] Delete `test/test_proof.ml`
- [ ] Edit `test/dune`: remove test_proof stanza
- [ ] Delete `proof/` directory (witness + emitted artifacts — build outputs)
- [ ] Delete `examples/crud-app/.borg.meta`

### Code edits (remove proof references from shared files)
- [ ] `lib/core/spec.ml`: remove `obligation_severity` type, `property_obligation` record, `extract_asserts` function, `obligations` field on `section_mapping`, the `obl` extraction in `extract_section_mappings`
- [ ] `lib/convention.ml`: remove `"coq"` from `verify_methods` Ocaml_dune list, remove `known_provers` function (or empty it), remove the doc comment block about provers
- [ ] `lib/check/lint.ml`: remove `Unknown_prover` and `Verified_without_obligation` from the issue ADT, remove `check_obligations` function, remove `obligation_issues` from the all_issues concat, remove the match cases in the error printer
- [ ] `lib/output/json_out.ml`: remove the two obligation-related match cases
- [ ] `bin/cmd/Lint.ml`: remove the two obligation-related Printf cases + the collapse match case

### Spec edits (demote to planned + note)
- [ ] `docs/cli.borg`: remove the entire `(section proof-commands)` block (it was just added, no need to keep as planned)
- [ ] `docs/engine.borg`: `obligations` subsection → `(status planned)` + agent note. Keep `db-auth` subsection (it's already proposed/standalone)
- [ ] `lib/check/check.borg`: `obligations` subsection → `(status planned)` + agent note
- [ ] `examples/crud-app/db.borg`: remove the entire `(section auth ...)` block (the `(asserts ...)` obligation). Keep all `(ownership ...)` declarations.

### Final
- [ ] `dune build` → exit 0
- [ ] `dune runtest` → exit 0
- [ ] `borge check` → all pass
- [ ] Commit with message explaining removal

## Verification command (rerunnable)
```sh
cd /home/roerick/dev/wyo.tech/borge && eval $(opam env)
dune build && dune runtest && borge check
```

## Final Verification (externally rerunnable)

Exact monitor-rerunnable command (from a fresh shell in this worktree):

    cd /home/roerick/dev/wyo.tech/borge && eval $(opam env)
    dune build                       # exit 0
    dune runtest                     # exit 0, all suites green
    borge check                      # 35/35 passed
    borge lint examples/crud-app     # 0 errors, 0 warnings
    ./_build/default/bin/borge.exe proof   # "unknown command proof" (removed)

- Working directory: /home/roerick/dev/wyo.tech/borge
- Required environment: opam switch `poohstack` activated via `eval $(opam env)`
- Required preserved artifacts: none beyond committed source (the opam switch
  with sedlex/menhir/coq remains, though coq is no longer required by borge)
- Result of final run: build 0, runtest 0, 35/35 files check, `borge proof`
  correctly returns "unknown command", all proof code/tests/specs removed,
  2 sections demoted implemented->planned with agent notes pointing to git
  history (commits b4f82f4..bd4c89c).
- Commit: 5dda139 (refactor: remove the Coq proof experiment for lack of utility)

