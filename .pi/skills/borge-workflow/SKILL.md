---
name: borge-workflow
description: Borge design/build/review workflow enforcement within pi sessions. File gating, phase cycling, and spec-first discipline.
---

# Borge Workflow Skill

## Overview

This project uses **borge** for spec-driven development. Files are gated by workflow phase:
- **idle**: Code files locked. Only `.borg` files editable.
- **plan**: Edit `.borg` spec files. Code remains locked.
- **make**: Edit code files. `.borg` files locked (only then do spec gaps become `borge issue new` filings).
- **review**: All files read-only.

## Tools Available

| Tool | When to call | What it does |
|------|-------------|--------------|
| `borg_plan(section="...")` | Before editing spec files | Unlocks `.borg` files for a section |
| `borg_make(section="...")` | Before editing code files | Unlocks code files, locks `.borg` files |
| `borg_review()` | After code changes | Runs `borge drift`, `lint`, `check`; enters review phase |
| `borg_commit(message="...")` | When user approves | Suggests git commit, resets phase to idle |
| `borg_abort()` | When abandoning work | Resets phase to idle, releases all locks |
| `borg_status()` | Any time | Shows current phase, target section, project sections |
| `/borg` | Quick status check | Same as `borg_status()` |

## Agent Workflow

When asked to implement a feature:

1. **Start**: Phase is `idle`. Only `.borg` files editable.
2. **Plan**: Call `borg_plan(section="target-section")` → edit `.borg` specs → update status, verify stanzas → show diff to user for confirmation
3. **Build**: Call `borg_make(section="target-section")` → edit code files. If the spec genuinely must change mid-build, file one issue: `borge issue new "description"` instead of editing `.borg`
4. **Review**: Call `borg_review()` → read drift/lint output → report to user
5. **Decide**: Present results. Ask user: "Commit? Fix? Abort?"
   - User says "commit" → `borg_commit(message="...")`
   - User says "fix" → stay in current phase or re-enter make → fix → review again
   - User says "abort" → `borg_abort()`

## Phase Rules

| Phase | What you CAN edit | What is BLOCKED |
|-------|------------------|-----------------|
| idle | `.borg` files | All code files |
| plan | `.borg` files | All code files |
| make | Code files (any non-`.borg`) | `.borg` files — spec gaps become `borge issue new` filings ONLY here |
| review | Nothing | All files (read-only) |

**The `implements` stanza in `.borg` files is for drift detection only, not for gating.** During `make` phase, all code files are unlocked regardless of whether they appear in `(implements ...)`.

## Spec Adjustments During Build Phase

`borge issue new` has exactly two legitimate triggers:
1. You are in **make phase**, discover the spec must change, and `.borg` files are locked.
2. You find a genuine defect or gap that is **outside the scope of your current instructions** — real enough that someone must act, but not yours to fix now.

It is NOT for: status flips, drift observations, verification results,
staleness notes, or capturing ideas during discussion. Those go in commit
messages, reports, or the conversation itself.

If during `make` you discover the spec needs to change:
- **Do NOT** edit `.borg` files directly (they're locked for a reason)
- **Do** file ONE issue: `borge issue new "Need to adjust X spec because Y"`
- This captures the spec gap without breaking phase discipline
- The user can review the issue and decide whether to enter a new plan phase later

In **plan phase**: edit the spec directly — that is what the phase is for.
In **open discussion / no borge phase**: just discuss. No issue, no filing.

## After Every Spec Edit (`.borg` files)

1. `borge balance FILE` — after any `.borg` edit
2. `borge parse FILE` — verify syntax
3. `borge check` — cross-file validation
4. `borge drift` — spec/code alignment check

## Auto-Repair

If balance fails: `borge balance --repair FILE > FILE.tmp && mv FILE.tmp FILE`
*Never manually count parens.*

## Status Values

Valid status in `.borg` files: `planned`, `in-progress`, `partial`, `implemented`, `verified`, `drifted`, `blank`

## Verify Stanzas

Per-section acceptance criteria. Example:
```
(subsection login-page
  (status implemented)
  (verify
    (test "test/auth_login.ml")
    (build)
    (smoke "POST /api/login"))
```

Verify stanzas describe WHAT is checked, behaviorally. Do NOT record
verification RESULTS in the spec — no `(* agent note (| verified
<date> ... |) *)` blocks, no test counts, no dates, no iteration refs,
no `/tmp/*.py` script paths. Evidence of a verification run belongs in
the commit message or a `borge issue`, never in the book. The book is a
declarative document: it states what must be true, not when or by whom
it was last checked.

## Commands Summary

| Command | When |
|---------|------|
| `borge balance FILE` | After every `.borg` edit |
| `borge balance --repair FILE` | When imbalance detected |
| `borge parse FILE` | After balance passes |
| `borge check` | Before commit |
| `borge drift` | When reviewing health |
| `borge drift --agent` | Deeper semantic analysis |
| `borge fmt FILE` | Before commit |
| `borge lint` | Advisory check |
| `borge report` | Dashboard |
| `borge future` | Roadmap view |
| `borge issue new "title"` | Make phase ONLY (spec gap), or out-of-scope defect |

## Spec Hygiene (the book is declarative)

The `.borg` book states what the system IS and MUST BE. Keep out:
- dates and "as of" status reports (staleness is invisible to readers)
- iteration/session references (`it35`, "the it12 refactor")
- changelog narration of what was deleted or pivoted (one orientation
  sentence max; git history is the changelog)
- borge issue IDs, drift/lint run notes, verification op-records
- volatile counts (test tallies, LOC, table counts) that rot silently
- project-management framing (phases, make-checklists, work packages)
  — those live in `borge issue`/plan docs, not the book

Human comments (`(* <name> ...)` for non-agent authors) are sacred:
never create, modify, or delete them.
