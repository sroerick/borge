(* Parse borg-comments from OCaml source files

   Borg-comments are OCaml comments containing valid borg s-expressions:
   (* (fn name (doc "...") (exports ...)) *)

   This module extracts them for drift checking against module specs. *)

type borg_comment = {
  form_type : string;  (* "fn", "val", "type", etc. *)
  name : string;
  properties : (string * string) list;
  raw : string;
  line : int;
}

let empty = {
  form_type = "";
  name = "";
  properties = [];
  raw = "";
  line = 0;
}

(* Check if a string looks like a borg form: starts with (fn, (val, etc. *)
let is_borg_form s =
  let trimmed = String.trim s in
  if String.length trimmed < 2 then false
  else if String.get trimmed 0 <> '(' then false
  else
    let rest = String.sub trimmed 1 (String.length trimmed - 1) in
    List.exists (fun prefix ->
      String.length rest >= String.length prefix &&
      String.sub rest 0 (String.length prefix) = prefix
    ) ["fn "; "val "; "type "; "module "; "doc "; "exports "; "since "]

(* Extract the form type and name from a borg s-exp *)
let extract_form_info sexp =
  match sexp with
  | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, form_type) :: rest) ->
      let name = match rest with
        | Borge_lang.Ast.Atom (_, n) :: _ -> n
        | _ -> ""
      in
      Some (form_type, name)
  | _ -> None

(* Parse a single comment content *)
let parse_comment content ~line =
  if not (is_borg_form content) then None
  else
    match Borge_lang.Parse.parse content with
    | file ->
        (match file.Borge_lang.Ast.top_level with
         | [] -> None
         | sexp_with_comments :: _ ->
             match extract_form_info sexp_with_comments.Borge_lang.Ast.node with
             | Some (form_type, name) ->
                 Some { empty with form_type; name; raw = content; line }
             | None -> None)
    | exception _ -> None

(* Extract all borg-comments from OCaml source *)
let extract_from_source content =
  let lines = String.split_on_char '\n' content in
  let comments = ref [] in
  let current_comment = ref [] in
  let in_comment = ref false in
  let start_line = ref 0 in
  let line_num = ref 0 in

  List.iter (fun line ->
    incr line_num;
    let trimmed = String.trim line in

    (* Check for comment start *)
    if not !in_comment then begin
      if String.length trimmed >= 2 && String.sub trimmed 0 2 = "(*" then begin
        in_comment := true;
        start_line := !line_num;
        current_comment := [line]
      end
    end else begin
      (* Inside comment, accumulate *)
      current_comment := line :: !current_comment;

      (* Check for comment end *)
      if String.length trimmed >= 2 &&
         (String.sub trimmed (String.length trimmed - 2) 2 = "*)" ||
          String.contains trimmed '*' && String.contains line ')') then begin
        in_comment := false;
        let comment_text = String.concat "\n" (List.rev !current_comment) in
        (match parse_comment comment_text ~line:!start_line with
         | Some bc -> comments := bc :: !comments
         | None -> ());
        current_comment := []
      end
    end
  ) lines;

  List.rev !comments

(* Find all functions defined with borg-comments in a file *)
let find_functions path =
  let content = File_utils.read_file path in
  extract_from_source content
  |> List.filter (fun bc -> bc.form_type = "fn" || bc.form_type = "val")

(* Get just the names for drift checking *)
let function_names path =
  find_functions path
  |> List.map (fun bc -> bc.name)

(* Render back to borg format for storage *)
let render_to_borg comments =
  List.map (fun bc ->
    Printf.sprintf "(fn %s%s)"
      bc.name
      (if bc.properties = [] then ""
       else " " ^ String.concat " " (List.map (fun (k,v) -> Printf.sprintf "(%s %s)" k v) bc.properties))
  ) comments
  |> String.concat "\n"
