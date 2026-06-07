# Enhance `borge balance` for better agent UX

## Problem
When a `.borg` file has a missing `)`, `borge balance` says:
```
End of file: N unclosed `(` — first opened at line 1, col 1
```
This is useless. Agents can't tell WHICH form is missing a close.

## Goal
Make `borge balance` surface structure, indentation hints, and smart close suggestions.

## Plan
1. **Update sexp.borg** — add `balance-enhanced-output` subsection spec ✅
2. **Update lib/lang/balance.ml** — add structural tracing, skeleton rendering, indent divergence detection, smart close suggestions ✅
3. **Update bin/borge_balance.ml** — update man page text ✅
4. **Build and test** — verify with broken test files ✅

## Results

### Implementation complete

- `sexp.borg` — added `enhanced-output` subsection under `(section balance)`, status set to `implemented`
- `borge.borg` — updated `balance-verbose` docs to mention new layers
- `lib/lang/balance.ml` — major enrichment:
  - New types: `paren_pair`, `indent_divergence`, `analysis`
  - `analyze` — character scanner that records every paren pair and its close
  - `find_divergences` — detects indent/depth mismatches
  - `render_skeleton` — shows all pairs with source lines and close status
  - `render_smart_suggestion` — mini-diff showing exactly where to insert `)`
  - `report_file` — now produces structured output for imbalanced files
- `bin/borge_balance.ml` — man page updated to document new output
- `test/test_balance.ml` — added 3 tests: analyze records pairs, analyze unclosed pairs, divergence detected

### New output format (imbalanced file)

```
✗ file.borg — N issue(s):

─── Structural skeleton (paren pairs) ───
   1: (project test              [⚠ UNCLOSED] project
   2:  (section alpha            [closed:5] section
...

─── Indent divergences (hints only) ───
  Line 13, col 2: `(` at depth 3 but column is 2 (expected 3)
    Hint: this form is under-indented — a `)` may be missing before this line

─── End of file: 1 unclosed `(` — first opened at line 1, col 1 ───
  project (line 1): missing close `)`.
    Suggestion: add `)` before line 17:
       16 |  )
    +     | )
       17 | )
    (closes project opened at line 1)
```

### All tests pass
10/10 tests passing (7 original + 3 new).
