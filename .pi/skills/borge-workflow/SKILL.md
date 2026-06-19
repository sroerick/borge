---
name: borge-workflow
description: Automatic borge spec integrity maintenance. When working in a directory containing .borg files, keep specs in sync with code by running balance, check, and drift as needed after every edit.
---

# Borge Workflow Skill

## When to use

Whenever you are in a borge project (directory with .borg files), this skill ensures specs stay accurate and drift-free automatically.

## Workflow

After every edit to any .borg file, .ml file, or dune file:

1. **Balance first** — run `borge balance FILE` on any .borg file you touched
2. **Auto-repair if needed** — if balance fails, run `borge balance --repair FILE > FILE.tmp && mv FILE.tmp FILE`
   *Never manually count parens — the repair infers them from indentation (Parinfer-style).*
3. **Parse check** — run `borge parse FILE` to validate syntax
4. **Check project** — run `borge check` to catch cross-file issues
5. **Drift check** — run `borge drift` to detect spec/code divergence
6. **Fix findings** — address any drift findings by updating specs or code

## Priority order

- If `borge balance` fails: auto-repair from indentation, then re-run balance to confirm
- If a .borg file edit won't parse: fix the file (balance → repair → parse → retry)
- If `borge check` fails: fix the issue before committing
- If `borge drift` shows findings: update the spec or the code, whichever is stale
- If `borge drift --agent` is requested: run the LLM semantic analysis

## Commands summary

| Command | When |
|---------|------|
| `borge balance FILE` | After every .borg edit |
| `borge balance --repair FILE` | When balance detects imbalance — auto-fix from indentation |
| `borge balance --repair-diff FILE` | Preview what repair would change |
| `borge parse FILE` | After balance passes |
| `borge check` | Before commit |
| `borge drift` | When reviewing project health |
| `borge drift --agent` | When deeper analysis needed |
| `borge fmt FILE` | Before commit (canonical formatting) |
| `borge review` | Advisory: lint + drift summary |

## .borg file conventions

- Root spec: `PROJECT.borg` (e.g., `borge.borg`) declares architecture
- Sub-specs: inlined via `(inline FILE.borg)` in parent
- Every .borg file must be inlined or declare `(no-inline)`
- Status values: `planned`, `in-progress`, `partial`, `implemented`, `drifted`, `blank`
- Annotated comments: `(* author type (|content|) *)`

## Agent prompt template

When asked to work on a borge project, start with:

```
Checking borge project health...
$ borge check && borge drift
```

Then proceed with the requested work, re-running check and drift after changes.
