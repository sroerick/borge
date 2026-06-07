(* Extract documentation comments from OCaml source files
 *
 * roerick note (|
 *   Handles multiple doc comment formats:
 *   1. Standard docstring: (** ... *) attribute on let binding
 *   2. Borg-style: (* author note (|...|) *) on preceding line
 *   3. Short borg: (*| ... |*) on preceding line
 *   4. Exempt marker: (* exempt doc *) marks binding as exempt
 * |) *)

(** Types of documentation comments found *)
type doc_type =
  | Ocamldoc of string  (** Standard (** ... *) docstring *)
  | Borg_note of string * string  (** (* author note (|...|) *) *)
  | Borg_short of string  (** (*| ... |*) *)
  | Exempt  (** (* exempt doc *) *)

(** Documentation found for a binding *)
type binding_doc = {
  line : int;
  doc_type : doc_type;
  content : string;
}

(** Regex patterns for borg-style comments *)
let borg_note_pattern =
  Str.regexp "(*\\s*\\([a-z]+\\)\\s+note\\s+(|\\([^)]*\\)|)\\s*\\*)"

(** Regex pattern for short borg comments: open-pipe-content-pipe-close *)
let borg_short_pattern =
  Str.regexp "(*|\\s*\\([^|]*\\)|\\s*\\*)"

(** Regex pattern for exemption markers in comments *)
let exempt_pattern =
  Str.regexp "(*\\s*exempt\\s+doc\\s*\\*)"

(** Extract comment body from a line *)
let extract_comment_content line =
  let stripped = String.trim line in
  if String.length stripped >= 4 &&
     String.sub stripped 0 2 = "(*" &&
     String.sub stripped (String.length stripped - 2) 2 = "*)" then
    Some (String.sub stripped 2 (String.length stripped - 4) |> String.trim)
  else
    None

(** Check if a line is a standalone borg comment *)
let is_borg_comment line =
  Str.string_match borg_note_pattern line 0 ||
  Str.string_match borg_short_pattern line 0 ||
  Str.string_match exempt_pattern line 0

(** Parse a borg-style comment *)
let parse_borg_comment line =
  if Str.string_match borg_note_pattern line 0 then
    let author = Str.matched_group 1 line in
    let content = try Str.matched_group 2 line with Not_found -> "" in
    Some (Borg_note (author, content))
  else if Str.string_match borg_short_pattern line 0 then
    let content = try Str.matched_group 1 line with Not_found -> "" in
    Some (Borg_short content)
  else if Str.string_match exempt_pattern line 0 then
    Some Exempt
  else
    None

(** Read file and extract all doc comments with their line numbers *)
let extract_all_docs path =
  try
    let channel = open_in path in
    let docs = ref [] in
    let line_num = ref 0 in
    let prev_comment = ref None in
    
    (try
      while true do
        let line = input_line channel in
        incr line_num;
        let trimmed = String.trim line in
        
        (* Check for borg-style comment *)
        (match parse_borg_comment trimmed with
         | Some doc_type ->
             let content = match doc_type with
               | Borg_note (_, c) -> c
               | Borg_short c -> c
               | Exempt -> "exempt"
               | Ocamldoc _ -> ""  (* shouldn't happen *)
             in
             prev_comment := Some { line = !line_num; doc_type; content }
         | None ->
             (* Check for standard (* ... *) comment that might be borg *)
             if String.length trimmed >= 4 &&
                String.sub trimmed 0 2 = "(*" &&
                String.sub trimmed (String.length trimmed - 2) 2 = "*)" then
               let inner = String.sub trimmed 2 (String.length trimmed - 4) |> String.trim in
               if inner = "exempt doc" || inner = "exempt" then
                 prev_comment := Some { line = !line_num; doc_type = Exempt; content = "exempt" }
               else
                 prev_comment := None
             else
               prev_comment := None)
      done
    with End_of_file -> ());
    
    close_in channel;
    List.rev !docs
  with e ->
    Printf.eprintf "Error reading %s: %s\n" path (Printexc.to_string e);
    []

(** Find doc comment preceding a line *)
let find_preceding_doc docs target_line =
  (* Look for the closest doc that is on an earlier line *)
  let rec find_closest = function
    | [] -> None
    | doc :: rest when doc.line < target_line ->
        (* Check if there's another doc between this one and the target *)
        (match find_closest rest with
         | Some closer when closer.line < target_line -> Some closer
         | _ -> Some doc)
    | _ :: rest -> find_closest rest
  in
  find_closest (List.rev docs)

(** Check if a binding at a given line has documentation *)
let has_doc docs line =
  match find_preceding_doc docs line with
  | Some { doc_type = Exempt; _ } -> `Exempt
  | Some { doc_type = (Ocamldoc _ | Borg_note _ | Borg_short _); _ } -> `Documented
  | None -> `Undocumented

(** Extract all docs from a string (for testing) *)
let extract_all_docs_from_string content =
  let lines = String.split_on_char '\n' content in
  let docs = ref [] in
  let line_num = ref 0 in
  
  List.iter (fun line ->
    incr line_num;
    let trimmed = String.trim line in
    
    (* Check for borg-style comment *)
    (match parse_borg_comment trimmed with
     | Some doc_type ->
         let content = match doc_type with
           | Borg_note (_, c) -> c
           | Borg_short c -> c
           | Exempt -> "exempt"
           | Ocamldoc _ -> ""  (* shouldn't happen *)
         in
         docs := { line = !line_num; doc_type; content } :: !docs
     | None ->
         (* Check for standard (* ... *) comment that might be borg *)
         if String.length trimmed >= 4 &&
            String.sub trimmed 0 2 = "(*" &&
            String.sub trimmed (String.length trimmed - 2) 2 = "*)" then
           let inner = String.sub trimmed 2 (String.length trimmed - 4) |> String.trim in
           if inner = "exempt doc" || inner = "exempt" then
             docs := { line = !line_num; doc_type = Exempt; content = "exempt" } :: !docs)
  ) lines;
  
  List.rev !docs
