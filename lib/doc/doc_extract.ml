(* Extract all let bindings from .ml files for documentation coverage
 *
 * agent note (|
 *   WHAT: Gathers all top-level value bindings from OCaml source files,
 *   distinguishing between public exports and internal helpers. Filters
 *   out test bindings, ignored bindings, and local opens.
 *
 *   WHY: Downstream tools (doc_coverage, lint --code-doc) need to know
 *   which bindings exist and whether they're exported, so they can
 *   check documentation coverage and enforce the literate-programming rule.
 * |) *)



(** Information about a binding extracted for doc coverage *)
type binding_info = {
  name : string;
  line : int;
  is_exported : bool;  (* Would this be visible in .mli? *)
  is_test : bool;
  is_internal : bool;  (* Starts with _ or is let%test *)
}

(* agent note (|
 *   WHAT: Parse a let-binding line to extract just the binding name.
 *   Handles "let rec name", "let name", and various patterns after
 *   the name (parameters, type annotations, etc.).
 *
 *   WHY: We need the binding name for coverage reporting — the name
 *   identifies which function is missing documentation.
 * |) *)
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
    (* Find first space, paren, or colon — that's where the name ends *)
    let name_end = try
      let space = try String.index rest ' ' with Not_found -> max_int in
      let paren = try String.index rest '(' with Not_found -> max_int in
      let colon = try String.index rest ':' with Not_found -> max_int in
      let equals = try String.index rest '=' with Not_found -> max_int in
      min space (min paren (min colon equals))
    with _ -> String.length rest in
    let name = (* exempt: String.sub *) String.sub rest 0 name_end |> String.trim in
    if name = "" then None else Some name

(* agent note (|
 *   WHAT: Check if a binding name is internal (starts with underscore).
 *
 *   WHY: Internal bindings are excluded from documentation coverage
 *   enforcement — they're implementation details, not public API.
 * |) *)
let is_internal_name name =
  name = "_" || String.starts_with ~prefix:"_" name

(* agent note (|
 *   WHAT: Check if a line is a test binding (starts with let%).
 *
 *   WHY: Test bindings are excluded from documentation coverage
 *   enforcement — they have different documentation needs than
 *   production code.
 * |) *)
let is_test_binding line =
  let trimmed = String.trim line in
  String.starts_with ~prefix:"let%" trimmed

(* agent note (|
 *   WHAT: Check if a line is a local open or module binding.
 *
 *   WHY: Local opens (let open Foo in) and module bindings
 *   (let module M = ...) are not real function definitions and
 *   must be excluded from binding extraction.
 * |) *)
let is_local_open line =
  let trimmed = String.trim line in
  String.starts_with ~prefix:"let open " trimmed ||
  String.starts_with ~prefix:"let module " trimmed

(* agent note (|
 *   WHAT: Determine if a binding would be exported in an .mli file
 *   based on naming convention: lowercase start and not internal.
 *
 *   WHY: Only exported bindings need documentation per the
 *   literate-programming rule. This heuristic approximates
 *   .mli visibility without actually parsing the interface file.
 * |) *)
let would_be_exported name =
  not (is_internal_name name) && name.[0] >= 'a' && name.[0] <= 'z'

(* agent note (|
 *   WHAT: Extract all top-level let bindings from a .ml file,
 *   returning their names, line numbers, and classification
 *   (exported, test, internal). Skips lines inside strings
 *   and comments to avoid false positives.
 *
 *   WHY: This is the primary extraction function used by
 *   Doc_coverage.calculate_file_coverage to discover which
 *   bindings exist and need documentation checks.
 * |) *)
let extract_bindings path =
  try
    (* exempt: open_in input_line *)
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
        
        (* Track whether we're inside a string or comment to
           avoid extracting bindings from string literals or
           comment blocks *)
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
        
        if not !in_string && not !in_comment && String.length line > 0 && line.[0] <> ' ' && line.[0] <> '\t' then
          if String.starts_with ~prefix:"let" trimmed then
            if is_local_open trimmed then
              ()
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

(* agent note (|
 *   WHAT: Recursively find all .ml files under a directory,
 *   skipping _build/ and .git/ directories.
 *
 *   WHY: Doc_coverage needs to scan the entire project for
 *   .ml files to calculate project-wide documentation coverage.
 * |) *)
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

(* agent note (|
 *   WHAT: Extract bindings from all .ml files in a project,
 *   returning (path, binding_info) pairs for every binding.
 *
 *   WHY: Used by higher-level analysis that needs to correlate
 *   bindings with their source files for reporting.
 * |) *)
let extract_project_bindings dir =
  let ml_files = find_ml_files dir in
  List.concat_map (fun path ->
    let bindings = extract_bindings path in
    List.map (fun b -> (path, b)) bindings
  ) ml_files

(* agent note (|
 *   WHAT: Count statistics for a list of bindings: total,
 *   exported, tests, and internal counts.
 *
 *   WHY: Provides quick summary numbers for coverage reports
 *   without needing to calculate full documentation coverage.
 * |) *)
let count_stats bindings =
  let total = List.length bindings in
  let exported = List.filter (fun b -> b.is_exported) bindings |> List.length in
  let tests = List.filter (fun b -> b.is_test) bindings |> List.length in
  let internal = List.filter (fun b -> b.is_internal) bindings |> List.length in
  (total, exported, tests, internal)
