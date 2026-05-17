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
- [x] UI/DB parse+validate+edge case tests (10 tests)
- [x] `borge generate ui` / `borge generate db` subcommands
- [x] --json and --target flags wired up
- [x] Human-readable summary output
- [x] SQL migration generator (Db_sql.generate) — CREATE TABLE, FK refs, indexes, RLS enable/policies
- [x] HTML/CSS generator (Ui_html_css.generate_html / generate_css) — theme vars, flex layout, element CSS
- [x] Generator tests (2 new — SQL and CSS generation)

### Remaining (stretch goals)
- [ ] LLM prompt construction for generate ui/db
- [ ] More robust HTML generator (page content, component instantiation)
- [ ] SQL view generation from relations
- [ ] CRUD function stub generation (e.g., Dream route stubs)

## Validation
- `dune build` passes ✅
- `dune runtest` passes — 39 tests total ✅
- `borge generate ui --target html` generates HTML+CSS ✅
- `borge generate ui --target css` generates CSS ✅
- `borge generate db --target sql` generates PostgreSQL DDL ✅
- `borge generate ui/db --json` outputs structured JSON ✅
- `borge report` works ✅

## Key Commits
- `966ccd0` implement UI and DB typed ASTs, parsers, validators, JSON serializers
- `e6ef4f6` implement UI and DB semantic validators with tests
- `8ba97a2` add --json flag to generate ui/db, human-readable summaries, edge case tests
- `44f9a4d` add SQL and HTML/CSS generators with --target flag and tests
