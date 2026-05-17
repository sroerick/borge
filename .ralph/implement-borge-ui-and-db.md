# Implement Borge UI and Borge DB

## Goal
Implement the borge UI DSL and borge DB DSL as extensions to the borge language.

## repo
`/home/roerick/dev/wyo.tech/borge/` on branch `master`

## Progress

### Done ✅
- [x] UI AST, parse, validate, JSON (lib/ui/)
- [x] DB AST, parse, validate, JSON (lib/db/)
- [x] Semantic validators (theme ref checks, name uniqueness, foreign key refs, ownership, RLS group caps)
- [x] UI/DB parse+validate+edge case+generator tests (12 tests)
- [x] `borge generate ui` / `borge generate db` subcommands with --json and --target flags
- [x] SQL migration generator — CREATE TABLE, FK refs, indexes, RLS enable/policies
- [x] SQL view generation from relations (CREATE VIEW with JOINs)
- [x] CRUD function stub generation (create/read/update/delete + named queries)
- [x] HTML/CSS generator — theme vars, flex layout, element CSS
- [x] Variant-aware CSS selectors (.button.primary, .button.secondary)
- [x] Component-aware HTML rendering (element names resolve to component definitions)
- [x] Layout slots with page fills (render_layout_html)
- [x] lib.borg ui/db section statuses updated to implemented

### Remaining (further polish, diminishing returns)
- [ ] LLM prompt construction for generate ui/db
- [ ] Parse `(use component-name)` as a real element reference in the parser
- [ ] Interactive capability → data-action attributes in HTML

## Validation
- `dune build` passes ✅
- `dune runtest` passes — 41 tests total ✅
- `borge generate ui --target html` generates HTML+CSS with layout slots and component lookup ✅
- `borge generate db --target sql` generates PostgreSQL DDL + views + CRUD stubs ✅
- `borge report` shows improved status (no more in-progress for ui/db) ✅

## Key Commits
- `966ccd0` implement UI and DB typed ASTs, parsers, validators, JSON serializers
- `e6ef4f6` implement UI and DB semantic validators with tests
- `8ba97a2` add --json flag and human-readable summaries
- `44f9a4d` add SQL and HTML/CSS generators with --target flag
- `27a204f` add SQL views, CRUD stubs, improved HTML with layout slots
- `1a063ca` variant-aware CSS selectors, component-aware HTML rendering, update lib.borg statuses
