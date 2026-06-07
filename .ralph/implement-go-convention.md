
# Implement go-standard convention support in borge

Based on the spec in `convention-go.borg`, implement Go language support for borge so that a Go project can use `(convention go-standard)` in its root `.borg` file and get the same drift, surface, doc, and comment features that OCaml projects get today.

## Key Design Principle
The borge comment grammar is IDENTICAL between OCaml and Go. Only the outer delimiters change:
- OCaml: `(* ... *)` → Go: `/* ... */`
- Same internal grammar is parsed by the same `Borge_lang.Parse`

## Implementation Checklist (in order)

### Phase 1: Convention type and resolution
- [ ] Create `lib/convention.ml` — `Convention.t` variant type (`Ocaml_dune | Go_standard`), resolve from root .borg file, fallback to Ocaml_dune
- [ ] Add `convention` to dune modules list
- [ ] Wire Convention.resolve into the CLI entry point so commands can access the convention

### Phase 2: Go_parse module
- [ ] Create `lib/go_parse.ml` — parse go.mod for module path, walk directory tree for packages/executables/tests
- [ ] Types: go_module, go_package, go_executable, go_package_file
- [ ] Functions: parse_go_mod, discover_packages, discover_executables, parse_all
- [ ] Add to dune modules list

### Phase 3: Go_surface module
- [ ] Create `lib/go_surface.ml` — extract exported symbols from .go files (uppercase = exported)
- [ ] Types: go_symbol_kind (Func/Type/Var/Const/Method), go_symbol, go_package_surface
- [ ] Functions: extract_file_surface, extract_package_surface, find_go_files
- [ ] Add to dune modules list

### Phase 4: Go_doc_extract and Go_doc_detect modules
- [ ] Create `lib/go_doc_extract.ml` — extract Go bindings (func/type/var/const), classify exported/test/internal
- [ ] Type: go_binding_info
- [ ] Create `lib/go_doc_detect.ml` — doc comment detection for Go files
- [ ] New doc_kind variant: GoDocstring (for // comments)
- [ ] Same Borg_note/Borg_short/Exempt_marker but with `/* */` delimiters
- [ ] Add both to dune modules list

### Phase 5: Go_comment module
- [ ] Create `lib/go_comment.ml` — extract borge s-expressions from Go `/* */` block comments
- [ ] Same as Borg_comment but strips `/*` and `*/` instead of `(*` and `*)`
- [ ] Add to dune modules list

### Phase 6: Convention dispatch in Drift
- [ ] Modify `lib/check/drift.ml` to dispatch based on Convention.t
- [ ] When Go_standard: use Go_parse and Go_surface instead of Dune_parse and Surface
- [ ] When Ocaml_dune: existing behavior unchanged

### Phase 7: Update convention-go.borg statuses
- [ ] After each module is implemented and tested, update its status from `planned` to `implemented`
- [ ] Run borge balance/parse/check after each .borg edit

## Verification
After each phase:
- `dune build` must pass
- `dune runtest` must pass  
- `borge balance` and `borge parse` on convention-go.borg
- `borge check` on the whole project
