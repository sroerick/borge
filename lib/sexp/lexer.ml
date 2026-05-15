type token =
  | LPAREN        (* ( *)
  | RPAREN        (* ) *)
  | HASH_LPAREN   (* (* — annotated comment open *)
  | STAR_RPAREN   (* *) — annotated comment close *)
  | VERB_OPEN     (* (| — verbatim string open *)
  | VERB_CLOSE    (* |) — verbatim string close *)
  | QUOTED of string  (* "..." — quoted string, escapes resolved *)
  | SYMBOL of string (* bare word *)
  | SEMICOLON of string  (* ; text — plain comment, text includes everything to EOL *)
  | EOF

type position = {
  line : int;
  column : int;
}

type located_token = {
  token : token;
  start : position;
}

let is_symbol_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' | '?' | '!' -> true
  | _ -> false

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
