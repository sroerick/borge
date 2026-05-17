# Complete Borge Implementation

## Goal
Finish all remaining implementation tasks to get borge to a fully working state.

## Tasks

### 1. Fix 8 Drift Errors
The `borge drift --modules` shows 8 unspecified-module errors:
- lib/borge_cmd/Issue.ml
- lib/borge_lib/json_out.ml
- lib/borge_lib/bug_ast.ml
- lib/borge_lib/bug_parse.ml
- lib/borge_lib/bug_write.ml
- lib/borge_lib/bug_registry.ml
- lib/borge_lib/borg_comment.ml
- lib/borge_lib/module_spec.ml

**Fix:** Add these modules to `lib.borg` exports section.

### 2. Test Lock Workflow
Verify the full agent workflow:
- `borge make` creates lock and opens pi
- Agent edits files
- `borge commit` finalizes (or `borge abort` cancels)
- Integrity checks work

### 3. Activate Semantic Review
Wire up actual LLM calls in `borge review`:
- Build prompt with function signature + docstring
- Call LLM (use pi command or API)
- Parse response into structured result
- Store in `.borg.meta`

### 4. Add Borg-Comments to Source
Demonstrate the spec↔code round-trip:
- Add `(* (fn name (doc "...")) *)` comments to key functions
- Verify `borge modules --functions --path FILE` extracts them
- Show drift detection working with real data

### 5. Implement Minimal Context
Reduce agent prompt size:
- Activate `prompt_minimal.ml`
- Parse .borg files for planned/implemented sections only
- Keep prompts under ~500 tokens
- Wire into `make`, `plan`, `drift --agent`

## Completion Criteria
- [ ] `borge drift` passes with 0 errors
- [ ] Lock workflow tested end-to-end
- [ ] Semantic review calls LLM and stores results
- [ ] At least 10 functions have borg-comments
- [ ] Agent prompts are minimal/summarized

## Progress Tracking
Update this checklist as items complete.