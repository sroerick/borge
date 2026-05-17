# Rewrite borge sexp parser with sedlex+menhir

## Goals
- Replace hand-written parse.ml with menhir LR(1) parser
- Replace lexer.ml char helpers with sedlex Unicode-aware tokenizer
- Produce the same AST (ast.ml) — all downstream code works identically
- Fix nested comment handling (the |)-inside-pipe-string problem)
- Enable self-documenting spec content in .borg files
- Fix doc_coverage in stats.ml (currently stub returning 0)

## Checklist
- [ ] Write sedlex lexer producing existing token types (LPAREN, RPAREN, etc.)
- [ ] Write menhir parser.mly producing Ast.sexp types
- [ ] Handle nested OCaml comments in lexer (depth tracking)
- [ ] Handle verbatim string nesting in lexer (depth tracking)
- [ ] Wire new parser as replacement for parse.ml
- [ ] Verify all existing tests pass (same AST, same behavior)
- [ ] Update lib/sexp/dune for sedlib/menhir dependencies
- [ ] Remove or archive old parse.ml
- [ ] Fix count_documented in stats.ml to use new lexer
- [ ] Add self-documenting examples back to borge.borg / sexp.borg
- [ ] borge drift zero findings

## Key Constraints
- AST types in ast.ml MUST NOT CHANGE
- Token types in lexer.ml are already defined
- balance.ml stays character-level — don't touch
- print.ml stays as-is
- Both sedlex and menhir are installed in opam

## Verification
- `dune build` must pass
- `dune runtest` all 22 tests must pass
- `borge drift` zero findings
