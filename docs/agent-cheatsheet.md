# Borge Agent Cheat Sheet

## The `#1 Rule: Balance Before You Build

Whenever you edit a `.borg` file, **run `borge balance <file.borg>` first**. The full parser gives vague "Unexpected character at EOF" errors when parens are wrong. The balance checker gives you line-accurate diagnostics with excerpts.

### Quick Fixes for Common Errors

| Error | Likely Cause | Fix |
|-------|-------------|-----|
| `End of file: N unclosed (...)` | Missing closing parens | Count all `(` and `)` — they must match. Look for `(doc (|...|)` blocks that forgot the outer `))` |
| `Line N, col M: unexpected )` | Extra closing paren | Look for doubled `))` or a close without matching open |
| `End of file: unclosed (|...) string` | Verbatim string never closed | Every `(|` needs matching `|)`. Nested verbatim strings are supported |
| `End of file: unclosed (* *) comment` | Annotated comment never closed | Every `(*` needs matching `*)` |

## Why Balance Checker Is Better

The full parser (`borge parse`) produces:
```
Parse error at line 99, column 25: Unexpected character
```

The balance checker produces:
```
✗ cli.borg — 1 issue(s):
  → End of file: 2 unclosed `(` — first opened at line 24, col 5
       ...
      Suggestion: add 2 `)` after end of file (unclosed since line 24)
```

## When to Use Which Tool

| Situation | Tool | Why |
|-----------|------|-----|
| You just edited parens/nesting | `borge balance` | Fast, line-accurate, understands verbatim/comments |
| You want full AST validation | `borge parse` | Checks all syntax: valid symbols, proper string form |
| Blue-sky editing before commit | `borge check` | Validates all `.borg` files in the project |
| Before every commit | `borge check && dune runtest` | Sanity check: all specs parse + all tests pass |

## Lexical Context Priority

The balance checker knows about these contexts and ignores parens inside them:

- `; ...` — plain comment (until newline)
- `(* ... *)` — annotated comment
- `"..."` — quoted string
- `(| ... |)` — verbatim string (supports nesting! `(| outer (| inner |) |)`)
- `<< ... >>` — inline example (supports nesting too)

## Verbatim String Nesting (Critical!)

This works and is correctly balanced:
```sexp
(doc (|Example: nested syntax (|inner|) works fine|))
```

This is also balanced:
```sexp
(doc (|Multi-line
      with (|depth 1|) and
      back to depth 0|))
```

But remember: each `(|` increments depth, each `|)` decrements it. The outer string only closes when depth hits 0.

## Workflow for the Agent

1. Read the `.borg` file you need to edit
2. Make your edits
3. **`borge balance <file.borg>`** ← Don't skip this
4. If balanced, run `borge parse <file.borg>` for full validation
5. If all good, run `borge check` to verify all project files
6. Run `dune runtest` to catch regressions
7. Commit
