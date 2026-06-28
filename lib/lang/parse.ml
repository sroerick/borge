(** Parse .borg files using the menhir-generated parser and sedlex lexer. *)

open Ast [@@warning "-33"]

(** Create a Sedlexing.lexbuf from a string and set up position tracking. *)
let make_lexbuf input =
  let lexbuf = Sedlexing.Latin1.from_string input in
  Sedlexing.set_position lexbuf
    { Lexing.pos_fname = "<input>";
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0 };
  lexbuf

(** Adapt the sedlex lexer to work with menhir's expected interface.
    Menhir expects a Lexing.lexbuf -> token function, but our lexer
    uses Sedlexing.lexbuf. We wrap it. *)
let read_token (lexbuf : Sedlexing.lexbuf) : Parser.token =
  let tok = Lexer.read lexbuf in
  (* Convert Lexer.token to Parser.token — they're the same types
     since Parser.token is generated from the %token declarations
     which match Lexer.token. But they're in different modules. *)
  match tok with
  | Lexer.LPAREN -> LPAREN
  | Lexer.RPAREN -> RPAREN
  | Lexer.HASH_LPAREN -> HASH_LPAREN
  | Lexer.STAR_RPAREN -> STAR_RPAREN
  | Lexer.VERB s -> VERB s
  | Lexer.QUOTED s -> QUOTED s
  | Lexer.SYMBOL s -> SYMBOL s
  | Lexer.SEMICOLON s -> SEMICOLON s
  | Lexer.EOF -> EOF

(** Bridge: menhir expects a Lexing.lexbuf, but our lexer uses Sedlexing.
    We adapt tokens and copy Sedlexing positions into the dummy Lexing
    lexbuf so menhir can report accurate $startpos/$endpos/$loc. *)
let parse_file input =
  let sedlex_lexbuf = make_lexbuf input in
  reset_nested_comments ();
  let token_reader (lb : Lexing.lexbuf) =
    let tok = read_token sedlex_lexbuf in
    let start_p, end_p = Sedlexing.lexing_positions sedlex_lexbuf in
    lb.Lexing.lex_start_p <- start_p;
    lb.Lexing.lex_curr_p <- end_p;
    tok
  in
  let dummy_lb = Lexing.from_string "" in
  try
    Parser.file token_reader dummy_lb
  with
  | Parser.Error ->
      let pos, _ = Sedlexing.lexing_positions sedlex_lexbuf in
      Error.error ~line:pos.Lexing.pos_lnum ~column:(pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        "Parse error"

(* agent note [5200] (|
 *   WHAT: Parse a borge file from a string input, returning the
 *   AST or raising a parse error.
 *
 *   WHY: Convenience wrapper around parse_file for when input is
 *   already in memory as a string.
 * |) *)
let parse input =
  parse_file input
