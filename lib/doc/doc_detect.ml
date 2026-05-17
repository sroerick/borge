(* Detect documentation comments and exempt markers in OCaml code
 *
 * roerick note (|
 *   Identifies various documentation formats and exempt markers:
 *   - Standard docstrings: (** ... *)
 *   - Borg annotated: (* author note (|...|) *)
 *   - Short borg: (*| ... |*)
 *   - Exempt: (* exempt doc *)
 * |) *)

(** Types of documentation found *)
type doc_kind =
  | Docstring of string     (** (** ... *) *)
  | Borg_note of string * string  (** (* author note (|...|) *) *)
  | Borg_short of string    (** (*| ... |*) *)
  | Exempt_marker           (** (* exempt doc *) *)

(** Where the doc was found relative to binding *)
type doc_position =
  | Preceding of int        (** N lines before binding *)
  | Inline                  (** Same line as binding - rare *)
  | None_found

(** Documentation info for a binding *)
type binding_doc = {
  binding_line : int;
  doc : (doc_kind * doc_position) option;
}

(** Check if a line is a docstring start *)
let is_docstring_start line =
  String.starts_with ~prefix:"(**" (String.trim line) &&
  not (String.starts_with ~prefix:"(***)" (String.trim line))

(** Check if a line is a borg note *)
let is_borg_note line =
  let trimmed = String.trim line in
  if String.length trimmed < 4 then false
  else if String.sub trimmed 0 2 = "(*" then
    let inner = String.sub trimmed 2 (String.length trimmed - 2) in
    let inner = String.trim inner in
    (try
      let space = String.index inner ' ' in
      let word = String.sub inner 0 space in
      let rest = String.sub inner space (String.length inner - space) |> String.trim in
      String.length word > 0 &&
      word.[0] >= 'a' && word.[0] <= 'z' &&  (* lowercase name *)
      String.starts_with ~prefix:"note " rest
    with _ -> false)
  else false

(** Check if a line is a borg short comment *)
let is_borg_short line =
  let trimmed = String.trim line in
  String.starts_with ~prefix:"(*|" trimmed

(** Check if a line is an exempt marker *)
let is_exempt_marker line =
  let trimmed = String.trim line in
  if String.length trimmed >= 4 &&
     String.sub trimmed 0 2 = "(*" &&
     String.sub trimmed (String.length trimmed - 2) 2 = "*)" then
    let inner = String.sub trimmed 2 (String.length trimmed - 4) |> String.trim in
    inner = "exempt" || inner = "exempt doc"
  else false

(** Extract content from a comment *)
let extract_comment_content line =
  let trimmed = String.trim line in
  if String.length trimmed < 4 then None
  else
    let inner = String.sub trimmed 2 (String.length trimmed - 4) |> String.trim in
    Some inner

(** Extract author and content from borg note *)
let parse_borg_note line =
  match extract_comment_content line with
  | None -> None
  | Some inner ->
      try
        let space = String.index inner ' ' in
        let author = String.sub inner 0 space in
        let rest = String.sub inner (space + 1) (String.length inner - space - 1) |> String.trim in
        if String.starts_with ~prefix:"note " rest then
          let note_content = String.sub rest 5 (String.length rest - 5) |> String.trim in
          (* Remove surrounding (| ... |) if present *)
          let note_content =
            if String.starts_with ~prefix:"(|" note_content then
              let len = String.length note_content in
              if String.sub note_content (len - 2) 2 = "|)" then
                String.sub note_content 2 (len - 4) |> String.trim
              else note_content
            else note_content
          in
          Some (author, note_content)
        else None
      with _ -> None

(** Parse a line to determine doc kind *)
let parse_doc_line line =
  if is_exempt_marker line then Some Exempt_marker
  else if is_docstring_start line then
    extract_comment_content line |> Option.map (fun c -> Docstring c)
  else if is_borg_short line then
    let trimmed = String.trim line in
    let inner = String.sub trimmed 3 (String.length trimmed - 5) |> String.trim in
    Some (Borg_short inner)
  else if is_borg_note line then
    parse_borg_note line |> Option.map (fun (a, c) -> Borg_note (a, c))
  else None

(** Find doc for a binding at a specific line *)
let find_binding_doc lines binding_line =
  let rec scan prev_lines remaining prev_line_no =
    match remaining with
    | [] -> None_found
    | line :: rest ->
        let line_no = prev_line_no + 1 in
        if line_no = binding_line then
          (* This is the binding line - check for preceding doc *)
          let rec find_preceding = function
            | [] -> None_found
            | (doc_line, doc_content) :: prev ->
                match parse_doc_line doc_content with
                | Some doc_kind -> Preceding (binding_line - doc_line)
                | None -> find_preceding prev
          in
          find_preceding prev_lines
        else
          scan ((line_no, line) :: prev_lines) rest line_no
  in
  scan [] lines 0

(** Extract all binding docs from a file *)
let extract_file_docs path =
  try
    let lines =
      let channel = open_in path in
      let rec read acc =
        try read (input_line channel :: acc) with End_of_file -> List.rev acc
      in
      let result = read [] in
      close_in channel;
      result
    in
    
    (* Find all let bindings and their docs *)
    let rec scan line_no prev_docs acc = function
      | [] -> List.rev acc
      | line :: rest ->
          let line_no = line_no + 1 in
          let trimmed = String.trim line in
          if String.starts_with ~prefix:"let" trimmed then
            if String.starts_with ~prefix:"let open " trimmed ||
               String.starts_with ~prefix:"let module " trimmed then
              scan line_no ((line_no, line) :: prev_docs) acc rest
            else
              (* Found a binding - check for preceding doc *)
              let doc =
                let rec find_doc = function
                  | [] -> None
                  | (doc_line, doc_content) :: prev ->
                      match parse_doc_line doc_content with
                      | Some doc_kind -> Some (doc_kind, Preceding (line_no - doc_line))
                      | None ->
                          (* Stop if we hit a non-comment, non-empty line *)
                          let tc = String.trim doc_content in
                          if tc = "" then find_doc prev
                          else None
                in
                find_doc prev_docs
              in
              scan line_no [] ({ binding_line = line_no; doc } :: acc) rest
          else
            scan line_no ((line_no, line) :: prev_docs) acc rest
    in
    scan 0 [] [] lines
  with e ->
    Printf.eprintf "Error extracting docs from %s: %s\n" path (Printexc.to_string e);
    []

(** Check if a binding has documentation *)
let binding_has_doc (bd : binding_doc) =
  match bd.doc with
  | None -> false
  | Some (Exempt_marker, _) -> false  (* Exempt is not documentation *)
  | Some _ -> true

(** Check if a binding is exempt from documentation requirement *)
let binding_is_exempt (bd : binding_doc) =
  match bd.doc with
  | Some (Exempt_marker, _) -> true
  | _ -> false
