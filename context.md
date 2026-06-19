# Code Context

## Files Retrieved
1. `lib/lang/ast.ml` (lines 1-98) - Core AST type definitions
2. `lib/lang/parse.ml` (lines 1-55) - Parser bridge and entry points
3. `lib/lang/parser.mly` (lines 1-84) - Menhir grammar for borge file parsing
4. `lib/lang/print.ml` (lines 1-125) - Pretty-printing and serialization of AST
5. `lib/format/nodes.ml` (lines 1-114) - Node enumeration for structure analysis
6. `lib/distributed/borg_comment.ml` (lines 1-62) - Comment extraction using sexp AST
7. `lib/convention.ml` (lines 1-109) - Convention dispatch using sexp pattern matching
8. `lib/lang/error.ml` (lines 1-17) - Parse error definition

## Key Code

### AST Type Definitions (lib/lang/ast.ml)

```ocaml
type pos = {
  line : int;
  col : int;
  offset : int;
}

type verbatim_string = {
  v_content : string;
  (* The raw text between (| and |), newlines preserved *)
}

type quoted_string = {
  q_content : string;
  (* The decoded text, escapes already resolved *)
}

type string_value =
  | Quoted of quoted_string
  | Verbatim of verbatim_string

type symbol = string

(* Simple sum type - NO GADTs *)
type sexp =
  | Atom of pos * symbol
  | String of pos * string_value
  | List of pos * sexp list

(* Comments are first-class but nullable within nested lists *)
type plain_comment = {
  text : string;
  line : int;
}

type authorship =
  | Single of symbol
  | Multiple of symbol list

type comment_type =
  | Untyped  (* two-position: (* author value *) *)
  | Typed of symbol  (* three-position: (* author type value *) *)

type annotated_comment = {
  authorship : authorship;
  comment_type : comment_type;
  value : string_value option;  (* None for empty (||) *)
  start_line : int;
  end_line : int;
}

type comment_attachment =
  | Plain of plain_comment
  | Annotated of annotated_comment

type sexp_with_comments = {
  comments_before : comment_attachment list;
  node : sexp;
  end_pos : pos;  (* position after the closing delimiter *)
}

type file = {
  top_level_comments : comment_attachment list;
  top_level : sexp_with_comments list;
  trailing_comments : comment_attachment list;
}
```

### Core Pattern Matching (lib/lang/print.ml - print_sexp)

```ocaml
let rec print_sexp indent buf sexp =
  match sexp with
  | Atom (_, sym) ->
      Buffer.add_string buf sym
  | String (_, sv) ->
      Buffer.add_string buf (string_of_string_value sv)
  | List (_, []) ->
      Buffer.add_string buf "()"
  | List (_, children) ->
      if children = [] then Buffer.add_string buf "()"
      else begin
        (* ... handling for pretty-printing with keywords, comments, etc ... *)
      end
```

### Value Extraction (lib/distributed/borg_comment.ml - extract_form_info)

```ocaml
let extract_form_info sexp =
  match sexp with
  | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, form_type) :: rest) ->
      let name = match rest with
        | Borge_lang.Ast.Atom (_, n) :: _ -> n
        | _ -> ""
      in
      Some (form_type, name)
  | _ -> None
```

### Convention Resolution (lib/convention.ml - resolve_from_file)

```ocaml
let rec find_convention = function
  | [] -> Ocaml_dune
  | { Borge_lang.Ast.node; _ } :: rest ->
    (match node with
     | Borge_lang.Ast.List (_, children) ->
       let rec scan = function
         | [] -> find_convention rest
         | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "convention") :: Borge_lang.Ast.Atom (_, name) :: _) :: _ ->
           (match of_string name with
            | Some t -> t
            | None -> Ocaml_dune)
         | _ :: scan_rest -> scan scan_rest
       in
       scan children
     | _ -> find_convention rest)
```

### Node Enumeration (lib/format/nodes.ml)

```ocaml
and node_info_of_sexp sexp _indent =
  match sexp with
  | Atom (p, sym) ->
      { pos = p; kind = "Atom"; keyword = None; child_count = 0; value = Some sym }
  | String (p, sv) ->
      let val_str = string_value_summary sv in
      { pos = p; kind = "String"; keyword = None; child_count = 0; value = Some val_str }
  | List (p, children) ->
      let keyword = list_keyword children in
      { pos = p; kind = "List"; keyword; child_count = List.length children; value = None }
```

### Parser Bridge (lib/lang/parse.ml)

```ocaml
let parse_file input =
  let lexbuf = make_lexbuf input in
  let token_reader (_lb : Lexing.lexbuf) =
    read_token lexbuf
  in
  let dummy_lb = Lexing.from_string "" in
  try
    Parser.file token_reader dummy_lb
  with Parser.Error ->
    let pos = Sedlexing.lexing_position_start lexbuf in
    Error.error ~line:pos.Lexing.pos_lnum ~column:(pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
      "Parse error"
```

## Architecture

### Parser Pipeline
1. **Input**: String containing .borg file contents
2. **Lexer (sedlex)**: Tokenizes input into SYMBOL, QUOTED, SEMICOLON, VERB, LPAREN, RPAREN, HASH_LPAREN, STAR_RPAREN
3. **Parser (menhir)**: Generates parse tree from tokens into `Ast.file`
4. **Error Handling**: Menhir errors are caught and re-raised as `Error.Parse_error` with line/column

### AST Semantics
- **simple variant sum type**: `sexp = Atom | String | List` - no type constraints, no GADT tags
- **pos embedded in all nodes**: Position tracking is explicit and duplicated (every node stores its `pos`)
- **comments as attachments**: Comments are attached to nodes via `sexp_with_comments` wrapper
- **plain comments discarded in lists**: Comments nested inside lists (`inner_comment_list`) are discarded during parsing
- **round-trip support**: Both `print.ml` and `parse.ml` preserve comments for round-tripping

### Usage Patterns Across Codebase
1. **AST traversal**: `print_sexp` recursively traverses with accumulator `indent` and `Buffer`
2. **structure analysis**: `nodes.ml` collects `node_info` for every node with depth tracking
3. **comment inspection**: `borg_comment.ml` extracts form info from top-level nodes only
4. **convention detection**: `convention.ml` recursively scans `top_level` list for `(convention NAME)` forms

## Findings

**AST is NOT using GADTs.** The `sexp` type is a straightforward algebraic data type with no type arguments, no `[@@gadt]` annotations, and no pattern-level type constraints.

**Key observations:**
- All pattern matches on `sexp` are simple deconstructions: `match sexp with | Atom ... | String ... | List ...`
- No type arguments appear in any variant constructor
- Position tracking is duplicated (stored in every node) instead of using GADT tags to constrain position of each variant
- All type functions would be trivial widening or box-unboxing (e.g., `string_value -> string`)

**Pattern matching locations across lib/:**
- `lib/distributed/borg_comment.ml:38` - `extract_form_info` (lines 38-48)
- `lib/distributed/module_spec.ml:46` - AST extraction
- `lib/bug/bug_parse.ml:7,12,36,40` - bug-specific parsing logic
- `lib/format/nodes.ml:25` - `collect_nodes`/`node_info_of_sexp`
- `lib/lang/print.ml:56` - `print_sexp` (main printer)
- `lib/convention.ml:82-100` - `find_convention`/`scan`

## Start Here

No handoff needed — the AST code is fully self-contained and well-documented. The `lib/lang/ast.ml` file is the primary reference for type definitions.

If implementing semantic analysis or AST transformations, start with:
1. `lib/lang/print.ml` (lines 56-98) to see existing traversal patterns
2. `lib/format/nodes.ml` (lines 25-60) for how node enumeration is structured
3. `lib/convention.ml` (lines 75-100) to see recursive AST scanning with pattern matching