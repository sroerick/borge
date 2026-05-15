open Ast

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

let string_of_verbatim v =
  Printf.sprintf "(|%s|)" v.v_content

let string_of_string_value = function
  | Quoted q -> string_of_quoted q
  | Verbatim v -> string_of_verbatim v

and spaces n = String.make n ' '

let rec print_sexp indent buf sexp =
  match sexp with
  | Atom sym ->
      Buffer.add_string buf sym
  | String sv ->
      Buffer.add_string buf (string_of_string_value sv)
  | List [] ->
      Buffer.add_string buf "()"
  | List children ->
      if children = [] then Buffer.add_string buf "()"
      else begin
        let keyword =
          match children with
          | [Atom "doc" | Atom "status"; _] -> ""
          | Atom k :: _ -> k
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
            | [Atom n] ->
                Buffer.add_string buf " ";
                Buffer.add_string buf n; []
            | Atom n :: tl ->
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
          | [Atom "doc"; String v] ->
              Buffer.add_string buf "(doc ";
              Buffer.add_string buf (string_of_string_value v);
              Buffer.add_char buf ')'
          | [Atom "status"; Atom v] ->
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

let print_plain_comment c =
  Printf.sprintf ";%s" c.text

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

let print_comment_attachment = function
  | Plain c -> print_plain_comment c
  | Annotated c -> print_annotated_comment c

let print_file f =
  let buf = Buffer.create 4096 in

  List.iter (fun c ->
    Buffer.add_string buf (print_comment_attachment c);
    Buffer.add_char buf '\n'
  ) f.top_level_comments;

  List.iter (fun { comments_before; node } ->
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
