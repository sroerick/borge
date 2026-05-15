open Ast
open Error

type state = {
  input : string;
  mutable pos : int;
  mutable line : int;
  mutable column : int;
}

let make_state input = {
  input;
  pos = 0;
  line = 1;
  column = 1;
}

let peek s =
  if s.pos >= String.length s.input then None
  else Some s.input.[s.pos]

let advance s =
  if s.pos < String.length s.input then begin
    if s.input.[s.pos] = '\n' then begin
      s.line <- s.line + 1;
      s.column <- 1;
    end else begin
      s.column <- s.column + 1;
    end;
    s.pos <- s.pos + 1
  end

let advance_n s n =
  for _ = 1 to n do advance s done

let position s : Lexer.position = { line = s.line; column = s.column }

let at_end s = s.pos >= String.length s.input

let skip_whitespace s =
  while not (at_end s) && Lexer.is_whitespace s.input.[s.pos] do
    advance s
  done

let parse_plain_comment s =
  let start_line = s.line in
  let buf = Buffer.create 64 in
  advance s;
  while not (at_end s) && s.input.[s.pos] <> '\n' do
    Buffer.add_char buf s.input.[s.pos];
    advance s
  done;
  { text = Buffer.contents buf; line = start_line }

let parse_quoted_string s =
  let start = position s in
  advance s;
  let buf = Buffer.create 64 in
  let rec loop () =
    if at_end s then
      error ~line:start.line ~column:start.column "Unterminated quoted string";
    let ch = s.input.[s.pos] in
    if ch = '"' then begin
      advance s;
      { q_content = Buffer.contents buf }
    end else if ch = '\\' then begin
      advance s;
      if at_end s then
        error ~line:s.line ~column:s.column "Escape at end of string";
      let next = s.input.[s.pos] in
      (match next with
       | 'n' -> Buffer.add_char buf '\n'
       | 't' -> Buffer.add_char buf '\t'
       | '\\' -> Buffer.add_char buf '\\'
       | '"' -> Buffer.add_char buf '"'
       | _ ->
         if next = '\n' || next = '\r' then ()
         else error ~line:s.line ~column:s.column "Unknown escape");
      advance s;
      loop ()
    end else begin
      Buffer.add_char buf ch;
      advance s;
      loop ()
    end
  in
  Quoted (loop ())

let parse_verbatim_string s =
  let start = position s in
  advance_n s 2;
  let buf = Buffer.create 128 in
  let depth = ref 1 in
  let rec loop () =
    if at_end s then
      error ~line:start.line ~column:start.column "Unterminated verbatim string";
    if s.input.[s.pos] = '(' && s.pos + 1 < String.length s.input && s.input.[s.pos + 1] = '|' then begin
      depth := !depth + 1;
      Buffer.add_char buf '(';
      Buffer.add_char buf '|';
      advance_n s 2;
      loop ()
    end
    else if s.input.[s.pos] = '|' && s.pos + 1 < String.length s.input && s.input.[s.pos + 1] = ')' then begin
      depth := !depth - 1;
      if !depth > 0 then begin
        Buffer.add_char buf '|';
        Buffer.add_char buf ')';
      end;
      advance_n s 2;
      if !depth = 0 then
        { v_content = Buffer.contents buf }
      else
        loop ()
    end else begin
      Buffer.add_char buf s.input.[s.pos];
      advance s;
      loop ()
    end
  in
  Verbatim (loop ())

let parse_symbol s =
  let buf = Buffer.create 32 in
  while not (at_end s) && Lexer.is_symbol_char s.input.[s.pos] do
    Buffer.add_char buf s.input.[s.pos];
    advance s
  done;
  Buffer.contents buf

let rec parse_sexp s =
  skip_whitespace s;
  if at_end s then
    error ~line:s.line ~column:s.column "Unexpected end of input";

  let ch = s.input.[s.pos] in
  if ch = ';' then
    error ~line:s.line ~column:s.column "Unexpected plain comment"
  else if ch = '(' then begin
    if s.pos + 1 < String.length s.input then begin
      let next = s.input.[s.pos + 1] in
      if next = '*' then
        error ~line:s.line ~column:s.column "Unexpected annotated comment"
      else if next = '|' then
        String (parse_verbatim_string s)
      else
        parse_list s
    end else
      parse_list s
  end else if ch = '"' then
    String (parse_quoted_string s)
  else if Lexer.is_symbol_char ch then
    Atom (parse_symbol s)
  else
    error ~line:s.line ~column:s.column "Unexpected character"

and parse_list s =
  advance s;
  skip_whitespace s;
  let elements = ref [] in
  while not (at_end s) && s.input.[s.pos] <> ')' do
    let elem = parse_sexp_or_comment s in
    (match elem with
     | `Comment _ -> ()
     | `Sexp sexp -> elements := sexp :: !elements);
    skip_whitespace s
  done;
  if at_end s then
    error ~line:s.line ~column:s.column "Unterminated list";
  advance s;
  List (List.rev !elements)

and parse_sexp_or_comment s =
  skip_whitespace s;
  if at_end s then
    error ~line:s.line ~column:s.column "Unexpected end of input";

  let ch = s.input.[s.pos] in
  if ch = ';' then
    `Comment (Plain (parse_plain_comment s))
  else if ch = '(' && s.pos + 1 < String.length s.input && s.input.[s.pos + 1] = '*' then
    `Comment (Annotated (parse_annotated_comment s))
  else
    `Sexp (parse_sexp s)

and parse_annotated_comment s =
  let start_line = s.line in
  advance_n s 2;
  skip_whitespace s;

  let authorship =
    if at_end s then
      error ~line:s.line ~column:s.column "Empty annotated comment"
    else if s.input.[s.pos] = '(' then begin
      advance s;
      skip_whitespace s;
      let authors = ref [] in
      while not (at_end s) && s.input.[s.pos] <> ')' do
        let sym = parse_symbol s in
        authors := sym :: !authors;
        skip_whitespace s
      done;
      if at_end s then
        error ~line:s.line ~column:s.column "Unterminated author list";
      advance s;
      skip_whitespace s;
      Multiple (List.rev !authors)
    end else begin
      let sym = parse_symbol s in
      skip_whitespace s;
      Single sym
    end
  in

  let comment_type, value =
    if at_end s then
      error ~line:s.line ~column:s.column "Annotated comment missing value"
    else if s.input.[s.pos] = '(' && s.pos + 1 < String.length s.input && s.input.[s.pos + 1] = '|' then
      let v = parse_verbatim_string s in
      (Untyped, Some v)
    else if s.input.[s.pos] = '"' then
      let v = parse_quoted_string s in
      (Untyped, Some v)
    else begin
      let sym = parse_symbol s in
      skip_whitespace s;
      if at_end s then
        error ~line:s.line ~column:s.column "Annotated comment: unexpected symbol after author"
      else if s.input.[s.pos] = '(' && s.pos + 1 < String.length s.input && s.input.[s.pos + 1] = '|' then
        let v = parse_verbatim_string s in
        (Typed sym, Some v)
      else if s.input.[s.pos] = '"' then
        let v = parse_quoted_string s in
        (Typed sym, Some v)
      else
        error ~line:s.line ~column:s.column "Annotated comment: expected string value after type"
    end
  in

  skip_whitespace s;
  let rec find_close () =
    if at_end s then
      error ~line:start_line ~column:1 "Unterminated annotated comment";
    if s.input.[s.pos] = '*' && s.pos + 1 < String.length s.input && s.input.[s.pos + 1] = ')' then
      advance_n s 2
    else begin
      advance s;
      find_close ()
    end
  in
  find_close ();

  let end_line = s.line in
  { authorship; comment_type; value; start_line; end_line }

let collect_comments s =
  let comments = ref [] in
  let continue = ref true in
  while !continue && not (at_end s) do
    skip_whitespace s;
    if at_end s then
      continue := false
    else begin
      let ch = s.input.[s.pos] in
      if ch = ';' then begin
        let c = parse_plain_comment s in
        comments := Plain c :: !comments
      end else if ch = '(' && s.pos + 1 < String.length s.input && s.input.[s.pos + 1] = '*' then begin
        let c = parse_annotated_comment s in
        comments := Annotated c :: !comments
      end else
        continue := false
    end
  done;
  List.rev !comments

let parse_file input =
  let s = make_state input in
  let top_level_comments = collect_comments s in
  let top_level = ref [] in
  while not (at_end s) do
    skip_whitespace s;
    if at_end s then ()
    else begin
      let comments = collect_comments s in
      skip_whitespace s;
      if at_end s then ()
      else begin
        let sexp = parse_sexp s in
        top_level := { comments_before = comments; node = sexp } :: !top_level
      end
    end
  done;
  { top_level_comments; top_level = List.rev !top_level; trailing_comments = [] }

let parse input =
  parse_file input
