(** Sedlex-based lexer for .borg sexp files.

    Uses Sedlexing for position tracking and menhir integration,
    but does character-by-character reading via Sedlexing.next
    instead of the %sedlex PPX. This gives us proper positions
    without the PPX syntax complexity.

    Produces the token types below. Handles nested verbatim strings
    via depth tracking. *)

type token =
  | LPAREN        (* open paren *)
  | RPAREN        (* close paren *)
  | HASH_LPAREN   (* annotated comment open *)
  | STAR_RPAREN   (* annotated comment close *)
  | VERB of string (* verbatim string with content, (| already consumed *)
  | QUOTED of string  (* quoted string, escapes resolved *)
  | SYMBOL of string (* bare word *)
  | SEMICOLON of string  (* plain comment to EOL *)
  | EOF

type position = {
  line : int;
  column : int;
}

type located_token = {
  token : token;
  start : position;
}

(* agent note (|
 *   WHAT: Check if a character is valid in a borge symbol (bare word).
 *   Includes alphanumerics, dash, underscore, dot, question, exclamation.
 *
 *   WHY: Symbol parsing needs to know where words end to correctly
 *   tokenize the input. This is the character class for valid symbols.
 * |) *)
let is_symbol_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' | '?' | '!' | '=' | ':' | '/' -> true
  | _ -> false

(* agent note (|
 *   WHAT: Check if a character is whitespace (space, tab, newline, return).
 *
 *   WHY: The lexer skips whitespace between tokens. This is the
 *   character class for whitespace.
 * |) *)
let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

(** Peek at the next character without consuming it.
    Sedlexing.rollback rolls back to the last start/mark checkpoint,
    so we must call start before peeking to set a checkpoint,
    then rollback to restore. *)
let peek lexbuf =
  Sedlexing.start lexbuf;
  match Sedlexing.next lexbuf with
  | None -> None
  | Some c ->
    Sedlexing.rollback lexbuf;
    Some (Uchar.to_char c)

(** Read next character, consuming it. *)
let next_char lexbuf =
  match Sedlexing.next lexbuf with
  | None -> None
  | Some c -> Some (Uchar.to_char c)

(** Read a quoted string, resolving escapes. Opening quote consumed. *)
let read_quoted_contents lexbuf =
  let buf = Buffer.create 64 in
  let rec loop () =
    match next_char lexbuf with
    | None -> failwith "Unterminated quoted string"
    | Some '"' -> Buffer.contents buf
    | Some '\\' -> begin
        match next_char lexbuf with
        | None -> failwith "Unterminated quoted string escape"
        | Some 'n' -> Buffer.add_char buf '\n'; loop ()
        | Some 't' -> Buffer.add_char buf '\t'; loop ()
        | Some '\\' -> Buffer.add_char buf '\\'; loop ()
        | Some '"' -> Buffer.add_char buf '"'; loop ()
        | Some 'r' -> Buffer.add_char buf '\r'; loop ()
        | Some c -> Buffer.add_char buf c; loop ()
      end
    | Some c -> Buffer.add_char buf c; loop ()
  in
  loop ()

(** Read a plain comment to EOL. Opening semicolon consumed. *)
let read_plain_comment lexbuf =
  let buf = Buffer.create 64 in
  let rec loop () =
    match next_char lexbuf with
    | None | Some '\n' -> Buffer.contents buf
    | Some c -> Buffer.add_char buf c; loop ()
  in
  loop ()

(** Read a symbol. *)
let read_symbol lexbuf first_char =
  let buf = Buffer.create 32 in
  Buffer.add_char buf first_char;
  let rec loop () =
    match peek lexbuf with
    | Some c when is_symbol_char c ->
        ignore (next_char lexbuf);
        Buffer.add_char buf c; loop ()
    | _ -> Buffer.contents buf
  in
  loop ()

(** Read verbatim string content. Opening (| consumed, closing |) consumed.
    Tracks nesting depth. Returns the content between delimiters. *)
let read_verbatim_contents lexbuf =
  let buf = Buffer.create 128 in
  let depth = ref 1 in
  let rec loop () =
    match next_char lexbuf with
    | None -> failwith "Unterminated verbatim string"
    | Some '(' -> begin
        match peek lexbuf with
        | Some '|' ->
            ignore (next_char lexbuf);
            depth := !depth + 1;
            Buffer.add_string buf "(|";
            loop ()
        | _ -> Buffer.add_char buf '('; loop ()
      end
    | Some '|' -> begin
        match peek lexbuf with
        | Some ')' ->
            ignore (next_char lexbuf);
            depth := !depth - 1;
            if !depth > 0 then begin
              Buffer.add_string buf "|)";
              loop ()
            end else
              Buffer.contents buf
        | _ -> Buffer.add_char buf '|'; loop ()
      end
    | Some c -> Buffer.add_char buf c; loop ()
  in
  loop ()

(** Main token reader. Skips whitespace between tokens. *)
let rec read lexbuf =
  match next_char lexbuf with
  | None -> EOF
  | Some ' ' | Some '\t' | Some '\n' | Some '\r' -> read lexbuf
  | Some '(' -> begin
      match peek lexbuf with
      | Some '*' -> ignore (next_char lexbuf); HASH_LPAREN
      | Some '|' -> ignore (next_char lexbuf); VERB (read_verbatim_contents lexbuf)
      | _ -> LPAREN
    end
  | Some ')' -> RPAREN
  | Some '*' -> begin
      match peek lexbuf with
      | Some ')' -> ignore (next_char lexbuf); STAR_RPAREN
      | _ -> read lexbuf  (* stray *, skip *)
    end
  | Some '"' -> QUOTED (read_quoted_contents lexbuf)
  | Some ';' -> SEMICOLON (read_plain_comment lexbuf)
  | Some c when is_symbol_char c -> SYMBOL (read_symbol lexbuf c)
  | Some _ -> read lexbuf  (* skip unknown chars *)
