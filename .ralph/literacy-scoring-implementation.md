# Implement Literacy Scoring in borge

## Status: COMPLETE ✅

All goals achieved and committed in 7c6bae5.

### Spec Updates
- [x] borge.borg: Update lint subsection to mention `--literacy` flag
- [x] lib.borg: Update stats subsection to mention literacy metrics

### Code: stats --literacy
- [x] lib/metrics/stats.ml: Add literacy_score fields to file_metrics
- [x] lib/metrics/stats.ml: Compute literacy averages from Doc_detect
- [x] bin/cmd/Stats.ml: Add `--literacy` flag, bar charts, insights
- [x] lib/output/json_out.ml: Add literacy to JSON output

### Code: lint --literacy  
- [x] lib/check/lint.ml: Add `Missing_literacy_score` and `Implausible_literacy_score`
- [x] lib/check/lint.ml: Add `check_literacy` function with plausibility heuristics
- [x] bin/cmd/Lint.ml: Add `--literacy` flag

### Comment Updates
- [x] parse.ml [5200], balance.ml [5100]/[5200]/[5300]
- [x] spec.ml [5200], print.ml [5200], doc_detect.ml [5300]

### Verification
- [x] `dune build` passes
- [x] `dune runtest` passes
- [x] `borge balance` and `borge parse` pass on .borg files
- [x] `borge lint --literacy` detects missing scores
- [x] `borge stats --literacy` shows literacy metrics

### Usage
```bash
borge stats --literacy .     # Per-file literacy profiles + project averages
borge lint --literacy .      # Missing or implausible scores as warnings
```
