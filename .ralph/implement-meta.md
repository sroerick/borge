# Implement .borg.meta System + Drift Redesign

## Checklist
- [x] Dune file parser (lib/borge/dune_parse.ml)
- [x] Module surface extractor (lib/borge/surface.ml)
- [x] Meta types and writer (lib/borge/meta.ml)
- [x] Staleness detection via content hashing
- [x] Redesign drift.ml to produce structured findings and write .borg.meta
- [x] Update drift CLI to support --agent flag
- [x] Implement agent prompt assembly and LLM invocation (lib/borge/agent.ml)
- [x] Agent sexp output parser and merge logic
- [x] Make fmt/check/lint/balance/parse skip .borg.meta files
- [x] Make inline/report aware of .borg.meta (don't double-count)
- [x] Tests: dune parser, module surface, meta writer, staleness (29 total)
- [x] Update meta.borg section statuses as implemented
- [x] Final dune build && dune runtest pass
