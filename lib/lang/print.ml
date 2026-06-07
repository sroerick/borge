open Ast

(* agent note (|
 *   WHAT: Convert a quoted string AST node to its escaped
 *   representation suitable for re-parsing.
 *
 *   WHY: When pretty-printing or re-serializing AST, quoted strings
 *   need proper escape sequences for newlines, tabs, quotes, backslashes.
 * |) *)
let string_of_quoted q =
  let buf = Buffer.create (String.length q.q_content + 8) in
  Buffer.add_char buf '"';
  String.iter (fun ch ->
    match ch with
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"' -> Buffer.add_string buf "\\\""
    | c -> Buffer.add_char buf c
  ) q.q_content;
  Buffer.add_char buf '"';
  Buffer.contents buf

(* agent note (|
 *   WHAT: Convert a verbatim string AST node to its representation
 *   in the (|content|) format.
 *
 *   WHY: Verbatim strings are printed with their delimiters for
 *   round-tripping through the parser.
 * |) *)
let string_of_verbatim v =
  Printf.sprintf "(|%s|)" v.v_content

(* agent note (|
 *   WHAT: Dispatch to the appropriate string converter based on
 *   whether the value is Quoted or Verbatim.
 *
 *   WHY: String values in borge can be either quoted or verbatim.
 *   This function handles both uniformly.
 * |) *)
let string_of_string_value = function
  | Quoted q -> string_of_quoted q
  | Verbatim v -> string_of_verbatim v

(* exempt doc *)
and spaces n = String.make n ' '

(* agent note (|
 *   WHAT: Recursively pretty-print a single sexp with indentation.
 *   Handles atoms, strings, and nested lists with proper formatting.
 *
 *   WHY: Core pretty-printing for the AST. Formats lists with keywords
 *   differently from plain lists, adding newlines for readability.
 * |) *)
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
        let keyword =
          match children with
          | [Atom (_, "doc") | Atom (_, "status"); _] -> ""
          | Atom (_, k) :: _ -> k
          | _ -> ""
        in
        let has_keyword = keyword <> "" && keyword <> "doc" && keyword <> "status" in
        if has_keyword then begin
          Buffer.add_char buf '(';
          Buffer.add_string buf keyword;
          let rest = List.tl children in
          (* First child is the name — keep it inline *)
          (* Only keep Atom names inline; lists go on their own line *)
          let further = match rest with
            | [] -> []
            | [Atom (_, n)] ->
                Buffer.add_string buf " ";
                Buffer.add_string buf n; []
            | Atom (_, n) :: tl ->
                Buffer.add_string buf " ";
                Buffer.add_string buf n; tl
            | _ -> rest
          in
          further |> List.iter (fun child ->
            Buffer.add_char buf '\n';
            Buffer.add_string buf (spaces (indent + 1));
            print_sexp (indent + 1) buf child
          );
          Buffer.add_char buf '\n';
          Buffer.add_string buf (spaces indent);
          Buffer.add_char buf ')'
        end else begin
          match children with
          | [Atom (_, "doc"); String (_, v)] ->
              Buffer.add_string buf "(doc ";
              Buffer.add_string buf (string_of_string_value v);
              Buffer.add_char buf ')'
          | [Atom (_, "status"); Atom (_, v)] ->
              Buffer.add_string buf "(status ";
              Buffer.add_string buf v;
              Buffer.add_char buf ')'
          | hd :: tl ->
              Buffer.add_char buf '(';
              print_sexp (indent + 1) buf hd;
              tl |> List.iter (fun child ->
                Buffer.add_char buf '\n';
                Buffer.add_string buf (spaces (indent + 1));
                print_sexp (indent + 1) buf child
              );
              Buffer.add_char buf '\n';
              Buffer.add_string buf (spaces indent);
              Buffer.add_char buf ')'
          | _ -> Buffer.add_string buf "()"
        end
      end

(* agent note (|
 *   WHAT: Convert a plain comment to its string representation
 *   with semicolon prefix.
 *
 *   WHY: Plain comments are printed as ; comment for round-tripping.
 * |) *)
let print_plain_comment c =
  Printf.sprintf ";%s" c.text

(* agent note (|
 *   WHAT: Convert an annotated comment to its borg representation
 *   in the (* author type (|content|) *) format.
 *
 *   WHY: Annotated comments carry author and type metadata that must
 *   be preserved for lint integrity checking.
 * |) *)
let print_annotated_comment c =
  let author_str = match c.authorship with
    | Single a -> a
    | Multiple auths -> Printf.sprintf "(%s)" (String.concat " " auths)
  in
  let type_str = match c.comment_type with
    | Untyped -> ""
    | Typed t -> Printf.sprintf " %s" t
  in
  let value_str = match c.value with
    | None -> "(|)"
    | Some sv -> string_of_string_value sv
  in
  Printf.sprintf "(* %s%s %s *)" author_str type_str value_str

(* agent note (|
 *   WHAT: Dispatch to the appropriate printer based on comment type.
 *
 *   WHY: Comments can be plain or annotated, each with different
 *   serialization formats.
 * |) *)
let print_comment_attachment = function
  | Plain c -> print_plain_comment c
  | Annotated c -> print_annotated_comment c

(* agent note (|
 *   WHAT: Pretty-print an entire borge AST to a string with
 *   proper formatting and indentation.
 *
 *   WHY: The primary output function for serializing parsed borge
 *   files back to text with readable formatting.
 * |) *)
let print_file f =
  let buf = Buffer.create 4096 in

  List.iter (fun c ->
    Buffer.add_string buf (print_comment_attachment c);
    Buffer.add_char buf '\n'
  ) f.top_level_comments;

  List.iter (fun { comments_before; node; _ } ->
    List.iter (fun c ->
      Buffer.add_string buf (print_comment_attachment c);
      Buffer.add_char buf '\n'
    ) comments_before;
    print_sexp 0 buf node;
    Buffer.add_char buf '\n'
  ) f.top_level;

  List.iter (fun c ->
    Buffer.add_string buf (print_comment_attachment c);
    Buffer.add_char buf '\n'
  ) f.trailing_comments;

  Buffer.contents buf
