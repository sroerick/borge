open Borge_lang

type file_result =
  | Ok of { path : string; project_name : string; form_count : int }
  | Error of { path : string; message : string }

type warning =
  | Orphan of string  (** path to an orphaned .borg file *)

type result = {
  files : file_result list;
  warnings : warning list;
  passed : int;
  failed : int;
}

let check_file path =
  let input = File_utils.read_file path in
  try
    let file = Parse.parse_file input in
    match Spec.project_name file with
    | None ->
        Error { path; message = "no project node found" }
    | Some name ->
        Ok { path; project_name = name; form_count = List.length file.Ast.top_level }
  with Error.Parse_error e ->
    Error { path; message = Printf.sprintf "parse error at %d:%d - %s" e.Error.line e.Error.column e.Error.message }

let run dir =
  let files = File_utils.find_borg_files dir in
  let results = List.map check_file files in
  let passed = List.filter (function Ok _ -> true | Error _ -> false) results |> List.length in
  let failed = List.filter (function Ok _ -> false | Error _ -> true) results |> List.length in
  (* Orphan detection: files not in any inline tree and no no-inline declaration *)
  let orphans = Project.find_orphans dir in
  let warnings = List.map (fun path -> Orphan path) orphans in
  { files = results; warnings; passed; failed }
