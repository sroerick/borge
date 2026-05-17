# Finish Borge Spec Implementation

Implement all planned features from meta.borg, semantic-review (lib.borg), and code-documentation (borge.borg).

## Phase 1: Core Data Types & Infrastructure

### Meta.borg Implementation
- [ ] **Meta: Create semantic_finding type** - Add to lib/check/meta.ml or new lib/meta/semantic_review_types.ml: type semantic_finding with fields doc_present, doc_accuracy, signature_match, behavior_coverage, structural_issues, confidence
- [ ] **Meta: Create documentation type** - Add documentation block type for coverage stats (total_bindings, documented, exempt, undocumented, coverage_percent)
- [ ] **Meta: Extend meta writer** - Update lib/check/meta.ml to write (semantic-review ...) and (documentation ...) blocks
- [ ] **Meta: Extend meta reader** - Update meta reader to parse new block types
- [ ] **Meta: Content hash tracking** - Add file content hash field to track staleness

### Build Integration
- [ ] **Dune: Add compiler-libs dependency** - Update borge.opam and dune files for ocaml-compiler-libs
- [ ] **Lib.dune: Update modules** - Add semantic_review, review_prompt, review_parse, doc_coverage modules

## Phase 2: Semantic Review (lib/review/)

### Core Implementation
- [ ] **Create lib/review/semantic_review.ml** - Main review orchestrator with extract_functions, review_file, batch_functions
- [ ] **Create lib/review/review_prompt.ml** - Build prompts for LLM with token management
- [ ] **Create lib/review/review_parse.ml** - Parse LLM sexp responses, never raises
- [ ] **Create lib/review/dune** - Library definition for review subdir
- [ ] **Function extraction** - Use compiler-libs Parsetree to extract let bindings, docstrings, signatures
- [ ] **Batching logic** - Group files with >20 functions into chunks of 15-20, alphabetically sorted

### CLI Integration
- [ ] **CLI: Add borge review command** - Update bin/main.ml with review subcommand
- [ ] **CLI: --file flag** - Review single file
- [ ] **CLI: --dir flag** - Review all .ml files in directory
- [ ] **CLI: --all flag** - Review entire project
- [ ] **CLI: --stale flag** - Only review files with stale metadata (content hash changed)

## Phase 3: Code Documentation Coverage

### Core Implementation
- [ ] **Create lib/doc/doc_coverage.ml** - Documentation coverage analyzer
- [ ] **Create lib/doc/dune** - Library definition
- [ ] **Binding extraction** - Use compiler-libs to find toplevel Pstr_value bindings
- [ ] **Doc detection** - Recognize (** docstring *), (* author note (|...|) *), (*| ... |*), (* exempt doc *)
- [ ] **Coverage calculation** - documented / (total - exempt) * 100
- [ ] **Undocumented names list** - Track and report specific undocumented functions

### CLI Integration
- [ ] **CLI: Add --docs flag to drift** - borge drift --docs triggers documentation analysis
- [ ] **CLI: Add undocumented-module finding** - Coverage < 50% produces drift finding
- [ ] **CLI: Add --verbose flag** - Show individual undocumented functions

## Phase 4: Meta Integration & Stats

### Integration
- [ ] **Stats: Aggregate doc coverage** - borge stats shows project-wide documentation coverage
- [ ] **Drift: Staleness check** - Check if semantic reviews are stale (content hash changed)
- [ ] **Drift: Merge semantic findings** - Replace existing review blocks, preserve static findings
- [ ] **Drift: Documentation findings** - Include doc coverage in drift output

### Threshold Configuration
- [ ] **Convention config** - Support (convention ocaml-dune (doc-coverage-threshold 0.7))
- [ ] **Warning thresholds** - >=80% good, 50-80% warning, <50% error

## Phase 5: Testing & Documentation

### Testing
- [ ] **Test semantic_review.ml** - Unit tests for extraction, batching, parsing
- [ ] **Test doc_coverage.ml** - Tests for doc detection, coverage calculation
- [ ] **Test meta writer/reader** - Round-trip tests for new block types
- [ ] **Integration tests** - End-to-end review and coverage commands

### Completion Criteria
- [ ] All tasks above completed
- [ ] `borge review FILE` works and writes to .borg.meta
- [ ] `borge drift --docs` analyzes coverage
- [ ] `borge stats` shows documentation coverage
- [ ] All specs updated from (status planned) to (status implemented)
- [ ] dune build && dune runtest passes

## Constraints
- NEVER emit the completion marker (^D) until ALL tasks are done
- After each task, update this checklist in PROGRESS.md
- Run `dune build` after each module addition
- Update spec status from (status planned) to (status implemented) as we complete sections