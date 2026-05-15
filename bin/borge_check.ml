open Borge_sexp

(* Recursively find .borg files from a directory *)
let rec find_borg_files dir =
  try
    let entries = Sys.readdir dir in
    Array.fold_left (fun acc name ->
      if name = "_build" || name = ".git" then acc
      else
        let path = Filename.concat dir name in
        if Sys.is_directory path then find_borg_files path @ acc
        else if Filename.check_suffix name ".borg" then path :: acc
        else acc
    ) [] entries
  with Sys_error _ -> []

(* Extract project name from parsed AST node *)
let extract_string = function
  | Ast.Atom s | Ast.String (Quoted {q_content = s}) | Ast.String (Verbatim {v_content = s}) -> Some s
  | _ -> None

let project_name (form : Ast.sexp_with_comments) =
  match form with
  | { Ast.comments_before = _; node = Ast.List (Ast.Atom "project" :: name :: _) } ->
      extract_string name
  | _ -> None

(* Summarize a single file *)
let check_file path =
  let input =
    let ic = open_in path in
    let n = in_channel_length ic in
    let buf = Bytes.create n in
    really_input ic buf 0 n;
    close_in ic;
    Bytes.to_string buf
  in
  try
    let file = Parse.parse_file input in
    let names = List.filter_map project_name file.Ast.top_level in
    match names with
    | [] ->
        Printf.printf "  ✗ %s: no project node found\n" path;
        false
    | [name] ->
        Printf.printf "  ✓ %s: project '%s' (%d forms)\n" path name (List.length file.Ast.top_level);
        true
    | names ->
        Printf.printf "  ✗ %s: multiple project nodes: %s\n" path (String.concat ", " names);
        false
  with
  | Error.Parse_error e ->
      Printf.printf "  ✗ %s: parse error at %d:%d - %s\n" path e.Error.line e.Error.column e.Error.message;
      false

type result = { passed : int; failed : int }

let run dir =
  let files = find_borg_files dir in
  let res = List.fold_left (fun ({ passed; failed } as acc) path ->
    let ok = check_file path in
    if ok then { acc with passed = passed + 1 }
    else { acc with failed = failed + 1 }
  ) { passed = 0; failed = 0 } files in
  Printf.printf "\n%d files checked. %d passed. %d failed.\n" (List.length files) res.passed res.failed;
  if res.failed > 0 then exit 1 else exit 0

let () =
  let dir = match Array.to_list Sys.argv with
    | _ :: "check" :: d :: _ -> d
    | _ :: d :: _ -> d
    | _ -> "."
  in
  if not (Sys.is_directory dir) then (
    Printf.eprintf "Error: '%s' is not a directory\n" dir;
    exit 2
  );
  Printf.printf "Checking .borg files in '%s'...\n\n" dir;
  run dir
