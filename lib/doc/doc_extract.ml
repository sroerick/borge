(* Extract all let bindings from .ml files for documentation coverage
 *
 * roerick note (|
 *   Gathers all top-level value bindings from OCaml source files,
 *   distinguishing between public exports and internal helpers.
 *   Filters out test bindings, ignored bindings, and local opens.
 * |) *)

open Borge_lib

(** Information about a binding extracted for doc coverage *)
type binding_info = {
  name : string;
  line : int;
  is_exported : bool;  (* Would this be visible in .mli? *)
  is_test : bool;
  is_internal : bool;  (* Starts with _ or is let%test *)
}

(** Extract binding name from a let binding line *)
let extract_binding_name line =
  let trimmed = String.trim line in
  let prefix =
    if String.starts_with ~prefix:"let rec " trimmed then
      "let rec "
    else if String.starts_with ~prefix:"let " trimmed then
      "let "
    else
      ""
  in
  if prefix = "" then
    None
  else
    let rest = String.sub trimmed (String.length prefix) (String.length trimmed - String.length prefix) in
    let rest = String.trim rest in
    (* Find first space, paren, or colon *)
    let name_end = try
      let space = try String.index rest ' ' with Not_found -> max_int in
      let paren = try String.index rest '(' with Not_found -> max_int in
      let colon = try String.index rest ':' with Not_found -> max_int in
      let equals = try String.index rest '=' with Not_found -> max_int in
      min space (min paren (min colon equals))
    with _ -> String.length rest in
    let name = String.sub rest 0 name_end |> String.trim in
    if name = "" then None else Some name

(** Check if a name is internal/ignored *)
let is_internal_name name =
  name = "_" || String.starts_with ~prefix:"_" name

(** Check if line is a test binding *)
let is_test_binding line =
  let trimmed = String.trim line in
  String.starts_with ~prefix:"let%" trimmed

(** Check if line is a local open *)
let is_local_open line =
  let trimmed = String.trim line in
  String.starts_with ~prefix:"let open " trimmed ||
  String.starts_with ~prefix:"let module " trimmed

(** Determine if a binding would be exported in an .mli *)
let would_be_exported name =
  (* In OCaml, anything without a leading _ that's not internal *)
  not (is_internal_name name) && name.[0] >= 'a' && name.[0] <= 'z'

(** Extract all bindings from a file *)
let extract_bindings path =
  try
    let channel = open_in path in
    let bindings = ref [] in
    let line_num = ref 0 in
    let in_string = ref false in
    let in_comment = ref false in
    
    (try
      while true do
        let line = input_line channel in
        incr line_num;
        let trimmed = String.trim line in
        
        (* Skip if in string or comment *)
        let rec check_string_comments i =
          if i >= String.length trimmed then ()
          else match trimmed.[i] with
            | '"' -> in_string := not !in_string; check_string_comments (i+1)
            | '(' when i+1 < String.length trimmed && trimmed.[i+1] = '*' && not !in_string ->
                in_comment := true; check_string_comments (i+2)
            | '*' when i+1 < String.length trimmed && trimmed.[i+1] = ')' && !in_comment ->
                in_comment := false; check_string_comments (i+2)
            | _ -> check_string_comments (i+1)
        in
        check_string_comments 0;
        
        (* Skip if still in comment from previous line *)  
        if not !in_string && not !in_comment then
          if String.starts_with ~prefix:"let" trimmed then
            if is_local_open trimmed then
              ()  (* Skip local opens *)
            else match extract_binding_name trimmed with
              | None -> ()
              | Some name ->
                  let is_test = is_test_binding trimmed in
                  let is_internal = is_internal_name name || is_test in
                  let is_exp = not is_internal && would_be_exported name in
                  bindings := {
                    name;
                    line = !line_num;
                    is_exported = is_exp;
                    is_test;
                    is_internal;
                  } :: !bindings
      done
    with End_of_file -> ());
    
    close_in channel;
    List.rev !bindings
  with e ->
    Printf.eprintf "Error extracting from %s: %s\n" path (Printexc.to_string e);
    []

(** Find all .ml files in a directory (recursive) *)
let find_ml_files dir =
  let rec find acc path =
    if Sys.is_directory path then
      if Filename.basename path = "_build" || Filename.basename path = ".git" then
        acc
      else
        Sys.readdir path
        |> Array.to_list
        |> List.map (Filename.concat path)
        |> List.fold_left find acc
    else if Filename.check_suffix path ".ml" then
      path :: acc
    else
      acc
  in
  find [] dir

(** Extract bindings from all .ml files in a project *)
let extract_project_bindings dir =
  let ml_files = find_ml_files dir in
  List.concat_map (fun path ->
    let bindings = extract_bindings path in
    List.map (fun b -> (path, b)) bindings
  ) ml_files

(** Count statistics for a list of bindings *)
let count_stats bindings =
  let total = List.length bindings in
  let exported = List.filter (fun b -> b.is_exported) bindings |> List.length in
  let tests = List.filter (fun b -> b.is_test) bindings |> List.length in
  let internal = List.filter (fun b -> b.is_internal) bindings |> List.length in
  (total, exported, tests, internal)
