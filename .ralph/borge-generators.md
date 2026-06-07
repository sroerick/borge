
# Borge Code Generators

Implement three code generators that walk the typed AST and emit target source code. The specs are in `ui.borg` (subsection `ocaml-dream-target`, `clay-c-target`) and `db.borg` (subsection `postgres-sql-target`).

## Current State

- `ui_ast.ml` — typed AST with all UI types (sizing, color_value, layout_direction, padding, etc.)
- `ui_parse.ml` — extracts typed UI AST from sexp
- `ui_validate.ml` — validates UI specs
- `ui_json.ml` — serializes UI AST to JSON
- `db_ast.ml` — typed AST with all DB types (table_def, column, operations_def, group_def, etc.)
- `db_parse.ml` — extracts typed DB AST from sexp
- `db_validate.ml` — validates DB specs
- `db_json.ml` — serializes DB AST to JSON

## Files to Create

1. `lib/ui/ui_dream.ml` — ocaml-dream generator (dream_html)
2. `lib/ui/ui_clay.ml` — clay-c generator (C structs + Clay API calls)
3. `lib/db/db_sql.ml` — postgres-sql generator (DDL)

Each generator exposes `val generate : t -> string` where `t` is `ui_app` or `db_app`.

## Implementation Order

### 1. db_sql.ml (simplest — start here)
- Walk `db_app` tree
- Emit CREATE TABLE IF NOT EXISTS with columns, constraints, defaults
- Foreign key topological sort (build dep graph from `references` clauses)
- CREATE INDEX IF NOT EXISTS (with method and unique support)
- ALTER TABLE ... ENABLE ROW LEVEL SECURITY
- CREATE POLICY from ownership shorthand (4 policies: SELECT/INSERT/UPDATE/DELETE)
- CREATE POLICY from group declarations
- Operations/queries/relations as SQL comments
- Schema header with generation metadata
- Idempotent (IF NOT EXISTS for tables/indexes, DROP+CREATE for policies)

### 2. ui_dream.ml (medium complexity)
- Walk `ui_app` tree
- Resolve theme variables (Palette/Spacing_var/Font_var/Radius_var → literal values)
- Emit dream_html OCaml code: `Html.div ~a:[...] [children]`
- Layout → flex CSS classes
- Sizing → Html.a_style with flex/width/height
- Colors → resolved inline (bg, color, border)
- Spacing → Html.a_style with padding/gap
- Variants → @media style blocks
- Components → `render_x` functions with slot parameters
- Layouts → `render_x_layout` functions
- Pages → compose layout + fills
- Routes → Dream.get handlers
- Style merging (multiple a_style → single semicolon-separated attribute)
- Theme as CSS custom properties (optional --css-vars)

### 3. ui_clay.ml (medium complexity, proves renderer-agnosticism)
- Walk `ui_app` tree (same walk as ui_dream, different output)
- Force-resolve ALL theme variables at generation time (C has no runtime style lookups)
- Emit .c + .h file pair with Clay API calls
- Layout → CLAY_TOP_TO_BOTTOM / CLAY_LEFT_TO_RIGHT
- Sizing → CLAY_SIZING_GROW / FIXED / PERCENT / FIT
- Colors → { R, G, B, 255 } structs (hex → RGBA)
- Text → CLAY_TEXT with Clay_TextElementConfig
- Interaction → stub handle_ hover callbacks
- Variants → if (screen_width >= 768) conditionals
- Components → Clay_Children render_X() functions
- Routes → enum page + switch
- C identifier sanitization (hyphens → underscores, reserved word prefixes)

## Pattern

Each generator follows the same pattern as ui_json/db_json — it's a consumer of the typed AST that produces a string. Read ui_json.ml and db_json.ml to understand the existing code style, then write the generators in the same pattern.

## CLI Integration (after generators work)

Wire into `borge generate --target (ocaml-dream|clay-c|postgres-sql) SPEC.borg` command. This can be a separate step after the generators themselves work and are tested.

## Event Emission

Each generator prints `(generated TYPE-APP-NAME TARGET at PATH)` to stderr on success.

## Checklist

- [x] db_sql.ml — CREATE TABLE generation
- [x] db_sql.ml — Column constraints and defaults
- [x] db_sql.ml — Topological sort for foreign keys
- [x] db_sql.ml — CREATE INDEX generation
- [x] db_sql.ml — RLS ENABLE + ownership policies
- [x] db_sql.ml — Group policies with (can ...) / (can-all)
- [x] db_sql.ml — Operations/queries/relations as comments
- [x] db_sql.ml — Schema header and idempotency
- [x] db_sql.ml — Tests
- [x] ui_dream.ml — Theme resolution
- [x] ui_dream.ml — Element → dream_html mapping
- [x] ui_dream.ml — Layout/sizing/color/spacing → CSS
- [x] ui_dream.ml — Components → render functions
- [x] ui_dream.ml — Layouts/pages/routes → Dream code
- [x] ui_dream.ml — Variant → @media blocks
- [x] ui_dream.ml — Style merging
- [x] ui_dream.ml — Tests
- [x] ui_clay.ml — Theme resolution (compile-time only)
- [x] ui_clay.ml — Element → Clay C mapping
- [x] ui_clay.ml — Layout/sizing/color → Clay structs
- [x] ui_clay.ml — Text → CLAY_TEXT
- [x] ui_clay.ml — Components → Clay_Children functions
- [x] ui_clay.ml — Layouts/pages → C functions
- [x] ui_clay.ml — Routes → enum + switch
- [x] ui_clay.ml — C identifier sanitization
- [x] ui_clay.ml — Tests
