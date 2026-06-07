
# Implement the UI DSL for Borge

## Core Principle
The ui.borg spec is the source of truth. The UI DSL does NOT generate code — "the spec IS the artifact." Same discipline as DB.

## Checklist

### Phase 1: Wire UI validation into lint ✅
- [x] Add `Ui_validation` lint variant to `lib/check/lint.ml`
- [x] Add `check_ui_specs` function that calls `Ui_validate.validate`
- [x] Add `Ui_validation` case to `bin/cmd/Lint.ml`, `bin/borge_lint.ml`, `bin/borge_review.ml`
- [x] Add `Ui_validation` case to `lib/output/json_out.ml`
- [x] Verified: catches undefined palette refs, duplicate variants, undefined layout refs

### Phase 2: Fix ui_json.ml TODOs ✅
- [x] `P_padding` JSON: implemented proper padding serialization (uniform + per-side)
- [x] `P_border` JSON: implemented proper border serialization (width, color, side)
- [x] `P_corner` JSON: implemented proper corner radius serialization (uniform + per-corner)
- [x] Added `radius_to_json` helper
- [x] Added `layout_to_json` (was missing from `ui_app_to_json`)
- [x] Added `fills` in `page_to_json` (was missing slot fill serialization)

### Phase 3: Update ui.borg statuses ✅
19 sections updated from `planned` → `implemented`:
- relationship-to-borge, theme, ui-form, layout-properties, sizing, spacing,
  style-properties, interactive, scroll, floating, variants, children,
  components, layouts, pages, routes, app-structure, agentic-editing,
  root project status

5 sections remain `planned` (correctly):
- `text` — minimal parsing (no bold/italic/wrap/placeholder)
- `image` — no image parsing in ui_parse.ml
- `full-example` — example, not implementation
- `convention-targets` — code generation targets, explicitly out of scope
- `future-work` — aspirational

### Phase 4: Verify and build ✅
- [x] `dune build` passes
- [x] `dune runtest` passes (8 tests)
- [x] `borge check .` passes (8/8 files)
- [x] `borge lint .` passes (0 errors, 0 warnings)
- [x] `borge report .` shows correct counts (ui.borg: 19 implemented, 5 planned)
- [x] `borge balance ui.borg` passes

## Result
- ui.borg: **19/24 implemented** (up from 1/24)
- Project overall: **82.4%** (up from 73.3%)
- All builds, tests, check, lint, balance pass
