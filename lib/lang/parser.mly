%{
open Ast

(** Convert a Lexing.position to our pos type. *)
let pos_of_lexing (p : Lexing.position) : pos =
  { line = p.pos_lnum;
    col = p.pos_cnum - p.pos_bol;
    offset = p.pos_cnum }

let mk_pos (start_pos, _end_pos) =
  pos_of_lexing start_pos

let rec end_pos_of_sexp = function
  | Atom (p, _) -> p
  | String (p, _) -> p
  | List (p, []) -> p
  | List (_, children) -> end_pos_of_sexp (List.hd (List.rev children))
%}

%token <string> SYMBOL
%token <string> QUOTED
%token <string> SEMICOLON
%token <string> VERB
%token LPAREN RPAREN
%token HASH_LPAREN STAR_RPAREN
%token EOF

%start file
%type <Ast.file> file

%%

file:
  | top = comment_list sexps = sexp_with_comments_list EOF
    { let nc = List.sort (fun (p1, _) (p2, _) -> compare p1.offset p2.offset) !nested_comments_acc in
      { top_level_comments = top;
        top_level = sexps;
        trailing_comments = [];
        nested_comments = nc } }
  | EOF
    { { top_level_comments = []; top_level = []; trailing_comments = []; nested_comments = [] } }

sexp_with_comments_list:
  | { [] }
  | xs = nonempty_sexp_with_comments_list { xs }

nonempty_sexp_with_comments_list:
  | c = comment_list s = sexp
    { let ep = end_pos_of_sexp s in
      [{ comments_before = c; node = s; end_pos = ep }] }
  | c = comment_list s = sexp rest = nonempty_sexp_with_comments_list
    { let ep = end_pos_of_sexp s in
      { comments_before = c; node = s; end_pos = ep } :: rest }

comment_list:
  | { [] }
  | c = comment rest = comment_list { c :: rest }

comment:
  | t = SEMICOLON
    { let p = pos_of_lexing $startpos in
      Plain { text = t; line = p.line } }
  | HASH_LPAREN a = author_list ty = comment_type_opt v = comment_value STAR_RPAREN
    { let sp = pos_of_lexing $startpos in
      let ep = pos_of_lexing $endpos in
      Annotated { authorship = a; comment_type = ty; value = v;
                   start_line = sp.line; end_line = ep.line } }

author_list:
  | s = SYMBOL { Single s }
  | LPAREN ss = symbol_list RPAREN { Multiple ss }

symbol_list:
  | { [] }
  | s = SYMBOL rest = symbol_list { s :: rest }

comment_type_opt:
  | { Untyped }
  | s = SYMBOL { Typed s }

comment_value:
  | { None }
  | q = QUOTED { Some (Quoted { q_content = q }) }
  | v = VERB { Some (Verbatim { v_content = v }) }

sexp:
  | s = SYMBOL
    { Atom (mk_pos $loc, s) }
  | q = QUOTED
    { String (mk_pos $loc, Quoted { q_content = q }) }
  | v = VERB
    { String (mk_pos $loc, Verbatim { v_content = v }) }
  | LPAREN RPAREN
    { List (mk_pos $loc, []) }
  | LPAREN xs = sexp_list RPAREN
    { List (mk_pos $loc, xs) }

sexp_list:
  | { [] }
  | _cs = inner_comment_list x = sexp xs = sexp_list { x :: xs }

(** Comments inside lists are captured into nested_comments_acc. *)
inner_comment_list:
  | { () }
  | c = comment _rest = inner_comment_list
    { let p = pos_of_lexing $startpos in
      nested_comments_acc := (p, c) :: !nested_comments_acc }

%%
