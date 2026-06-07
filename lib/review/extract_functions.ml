(* Extract function information from OCaml source files
 *
 * roerick note (|
 *   Uses regex-based extraction to find let bindings, their names,
 *   signatures, and doc comments. This is a practical first-pass
 *   implementation. Future versions may use compiler-libs for
 *   more accurate AST-based extraction.
 * |) *)

type function_info = {
  name : string;
  signature : string;
  docstring : string option;
  line : int;
  is_recursive : bool;
  is_test : bool;  (* let%test binding *)
  is_ignored : bool;  (* let _ = ... *)
}

(** Extract function name from a let binding line *)
let extract_name_from_let line =
  let trimmed = String.trim line in
  let prefix =
    if String.length trimmed > 8 && String.sub trimmed 0 8 = "let rec " then
      Some "let rec "
    else if String.length trimmed > 4 && String.sub trimmed 0 4 = "let " then
      Some "let "
    else
      None
  in
  match prefix with
  | None -> None
  | Some pfx ->
      let rest = String.sub trimmed (String.length pfx) (String.length trimmed - String.length pfx) in
      let rest = String.trim rest in
      (* Find first space or parenthesis to get function name *)
      let name_end = try
        let space_pos = String.index rest ' ' in
        let paren_pos = try String.index rest '(' with Not_found -> space_pos in
        min space_pos paren_pos
      with Not_found ->
        try String.index rest '(' with Not_found -> String.length rest
      in
      let name = (* exempt: String.sub *) String.sub rest 0 name_end in
      (* Check for parameters to build signature *)
      let params = try
        let param_start = name_end in
        let param_end = try String.index rest '=' with Not_found -> String.length rest in
        if param_start < param_end then
          (* exempt: String.sub *) String.sub rest param_start (param_end - param_start) |> String.trim
        else
          ""
      with _ -> "" in
      Some (name, params)

(** Check if a line is a test binding *)
let is_test_line line =
  let trimmed = String.trim line in
  String.length trimmed > 4 && String.sub trimmed 0 4 = "let%"

(** Check if a line is an ignored binding *)
let is_ignored_line line =
  let trimmed = String.trim line in
  String.length trimmed > 6 && String.sub trimmed 0 6 = "let _ "

(** Check if a line contains an attribute *)
let has_attribute line =
  String.contains line '[' && String.contains line ']'

(** Extract functions from a file *)
let extract_from_file path =
  try
    (* exempt: open_in input_line *)
    let channel = open_in path in
    let functions = ref [] in
    let current_doc = ref [] in
    let line_num = ref 0 in
    
    (try
      while true do
        let line = input_line channel in
        incr line_num;
        let trimmed = String.trim line in
        
        (* Check for doc comment *)
        if String.length trimmed > 2 && String.sub trimmed 0 2 = "(*" then
          current_doc := line :: !current_doc
        else if String.length trimmed > 4 && 
                (String.sub trimmed 0 4 = "let " || 
                 (String.length trimmed > 8 && String.sub trimmed 0 8 = "let rec ")) then
          (* Check for let%test or let%lwt etc. *)
          if String.length trimmed > 4 && trimmed.[3] = '%' then
            current_doc := []  (* Reset doc for test bindings *)
          else
            match extract_name_from_let line with
            | Some (name, params) ->
                let is_rec = String.starts_with ~prefix:"let rec " trimmed in
                let is_ign = name = "_" || String.starts_with ~prefix:"_" name in
                let doc_opt = if !current_doc = [] then None else Some (String.concat "\n" (List.rev !current_doc)) in
                let sig_str = if params = "" then name else Printf.sprintf "%s %s" name params in
                functions := {
                  name;
                  signature = sig_str;
                  docstring = doc_opt;
                  line = !line_num;
                  is_recursive = is_rec;
                  is_test = false;
                  is_ignored = is_ign;
                } :: !functions;
                current_doc := []
            | None -> 
                current_doc := []
      done
    with End_of_file -> ());
    
    close_in channel;
    List.rev !functions
  with e ->
    Printf.eprintf "Error reading %s: %s\n" path (Printexc.to_string e);
    []

(** Extract all functions from a structure - stub for compatibility *)
let extract_functions ?(path = "") () =
  ignore path;
  []
