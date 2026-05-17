# Complete Borge Implementation

Implement all scoped features to bring borge to completion.

## Phase 1: Semantic Review System (10 tasks)
- [ ] 1.1 Create review/extract_parsetree.ml - compiler-libs integration
- [ ] 1.2 Create review/extract_functions.ml - extract let bindings  
- [ ] 1.3 Create review/extract_docs.ml - extract doc comments
- [ ] 1.4 Create review/review_prompt.ml - LLM prompt builder
- [ ] 1.5 Create review/review_parse.ml - LLM response parser
- [ ] 1.6 Create review/review_agent.ml - LLM integration
- [ ] 1.7 Implement semantic_review.ml - full orchestration
- [ ] 1.8 Update cmd_review.ml - wire up borge review FILE|DIR|--all
- [ ] 1.9 Add --stale flag support for reviewing only changed files
- [ ] 1.10 Write tests for review system

## Phase 2: Documentation Coverage (7 tasks)
- [ ] 2.1 Create doc/doc_extract.ml - extract all bindings from .ml files
- [ ] 2.2 Create doc/doc_detect.ml - detect doc comments & exempt markers
- [ ] 2.3 Create doc/doc_coverage.ml - calculate coverage stats
- [ ] 2.4 Add --docs flag to cmd_drift.ml
- [ ] 2.5 Update meta.ml to write (documentation ...) blocks
- [ ] 2.6 Show coverage in cmd_stats.ml output
- [ ] 2.7 Write tests for doc coverage

## Phase 3: Merge Queue / Agentic CI/CD (13 tasks)
- [ ] 3.1 Create merge/worktree.ml - git worktree management
- [ ] 3.2 Create merge/branch.ml - agent branch creation
- [ ] 3.3 Create merge/lockfile.ml - .pi/concurrent.lock handling
- [ ] 3.4 Create merge/queue_types.ml - queue state types
- [ ] 3.5 Create merge/queue_storage.ml - persist queue state
- [ ] 3.6 Create merge/conflict_detect.ml - file overlap analysis
- [ ] 3.7 Create merge/merge_strategies.ml - ff/rebase/merge/squash
- [ ] 3.8 Create merge/rollback.ml - revert failed merges
- [ ] 3.9 Create cmd_merge_queue.ml - list|show|submit|cancel
- [ ] 3.10 Create cmd_merge.ml - --next|--all|--dry-run
- [ ] 3.11 Create merge/agent_submit.ml - auto-submit on completion
- [ ] 3.12 Update borge CLI to include merge commands
- [ ] 3.13 Write tests for merge queue

## Phase 4: Future Command (5 tasks)
- [ ] 4.1 Create future/future.ml - scan specs for planned/in-progress
- [ ] 4.2 Create future/depends.ml - parse dependency chains
- [ ] 4.3 Create future/format.ml - format roadmap output
- [ ] 4.4 Create cmd_future.ml - borge future [--section X] [--summary]
- [ ] 4.5 Write tests for future command

## Phase 5: Implementation Testing & Verification (4 tasks)
- [ ] 5.1 Build and verify all modules compile
- [ ] 5.2 Run balance/check/lint/drift on all .borg files
- [ ] 5.3 Run OCaml test suite
- [ ] 5.4 Verify all planned sections have test coverage

## Phase 6: Agent Command Restructuring (3 tasks)
- [ ] 6.1 Rename borge generate spec → borge make spec
- [ ] 6.2 Remove borge generate code
- [ ] 6.3 Rename borge generate ui/db → borge show ui/db

## Success Criteria
- All (status planned) sections become (status implemented)
- Each feature has unit tests
- borge check passes on all .borg files
- dune build && dune runtest passes
- All commands in cli.borg are functional

## Notes
- Agentic commands (make, plan) should be tested manually after implementation
- Focus on core library implementation first, CLI wiring second
- Update lib.borg exports as new modules are added
- Follow existing patterns in lib/ directory structure