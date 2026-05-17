# Implement borge agent execute workflow

## Goal
Implement the full agent-driven workflow: `borge make`, `borge plan`, `borge drift --agent`, `borge commit`, `borge abort`. The agent opens pi interactively with a structured prompt. The lock directory (`./borge.lock/`) provides snapshot isolation, integrity checking, and abort/restore.

## repo
`/home/roerick/dev/wyo.tech/borge/` on branch `master`

## Spec Source
`borge.borg` sections: `execute`, `lock-mechanism`, `comment-integrity`, `agent-discipline`

## Tasks

### Infrastructure
- [ ] Create `lib/lock/` directory with lock mechanism types and operations
- [ ] Implement lock creation: snapshot all `.borg` files to `.borge.lock/original/`
- [ ] Implement lock teardown: commit (apply changes + git commit) or abort (restore from original/)
- [ ] Implement integrity check: compare `original/` with working tree for deleted human comments
- [ ] Implement journal format: structured log of proposed changes

### Prompt construction
- [ ] Build prompt from spec: read all `.borg` files, assemble context
- [ ] Add role declaration to prompt (implementer / spec-writer / observer / fixer)
- [ ] Add convention rules to prompt (from project convention)
- [ ] Add module surfaces for context (existing exports)
- [ ] Write prompt to `.borge.lock/prompt.md`

### Pi integration
- [ ] Launch pi interactively: `pi @.borge.lock/prompt.md --no-session`
- [ ] Detect terminal multiplexer (tmux, ghostty) and use appropriate launch method
- [ ] Capture pi exit status (0 = clean, non-zero = failure)

### Commands
- [ ] `borge make` — role: implementer, auto-commit on clean exit
- [ ] `borge plan` — role: spec-writer, auto-commit on clean exit
- [ ] `borge drift --agent` — role: observer, read-only by default
- [ ] `borge drift --agent --fix` — role: fixer, can write code
- [ ] `borge commit` — manual commit after `--no-commit` sessions
- [ ] `borge abort` — restore `.borg` files from original/

### Post-session
- [ ] On clean exit: integrity check → fmt → balance → git commit → remove lock
- [ ] On failure: keep lock, user can `borge commit` or `borge abort`
- [ ] Commit message derived from journal

### Validation
- `dune build` passes
- `dune runtest` passes (all existing tests still pass)
- `borge make --help`, `borge plan --help`, etc. show correct usage
- Lock creation + commit + abort workflow tested end-to-end

## Constraints
- No unspec'd code — every new module gets a subsection in `lib.borg`
- No changes to existing commands unless necessary
- `.borg` files stay in repo root — agent edits them in place
- Lock directory is metadata + snapshots, not workspace
- Human comment integrity check blocks commit (can `--force`)
