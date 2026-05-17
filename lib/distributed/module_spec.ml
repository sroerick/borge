(* Module spec discovery and parsing for distributed specs

   Each module folder has its own .borg file describing exports.
   This module discovers and parses them. *)

type module_def = {
  path : string;           (* Path to the .borg file *)
  module_name : string;    (* Derived from filename or spec *)
  exports : string list;   (* Function/val names exported *)
  raw_spec : string;       (* Raw borg content *)
}

(* Find all .borg files in subdirectories (excluding root) *)
let discover_specs dir =
  let rec walk path acc =
    try
      let entries = Sys.readdir path in
      Array.fold_left (fun acc entry ->
        let full_path = Filename.concat path entry in
        if Sys.is_directory full_path then
          if entry = ".git" || entry = "_build" || entry = ".borge-bugs" then
            acc
          else
            walk full_path acc
        else if Filename.check_suffix entry ".borg" && full_path <> (Filename.concat dir "borge.borg") then
          full_path :: acc
        else
          acc
      ) acc entries
    with _ -> acc
  in
  walk dir []

(* Extract exports from a parsed borg s-exp *)
let extract_exports sexp =
  let rec find_exports = function
    | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "exports") :: rest) ->
        List.filter_map (function
          | Borge_lang.Ast.Atom (_, name) -> Some name
          | _ -> None
        ) rest
    | Borge_lang.Ast.List (_, items) ->
        List.concat_map find_exports items
    | _ -> []
  in
  match sexp with
  | Borge_lang.Ast.List (_, _) -> find_exports sexp
  | _ -> []

(* Parse a single module spec file *)
let parse_module_spec path =
  let content = File_utils.read_file path in
  let filename = Filename.basename path in
  let module_name = Filename.remove_extension filename in

  match Borge_lang.Parse.parse content with
  | file ->
      let exports = List.concat_map (fun sexp_with_comments ->
        extract_exports sexp_with_comments.Borge_lang.Ast.node
      ) file.Borge_lang.Ast.top_level in
      Some { path; module_name; exports; raw_spec = content }
  | exception _ ->
      (* Failed to parse, return empty spec *)
      Some { path; module_name; exports = []; raw_spec = content }

(* Get module specs for all discovered .borg files *)
let load_all_specs dir =
  discover_specs dir
  |> List.filter_map parse_module_spec

(* Find functions in implementation files that correspond to module spec *)
let find_implementation_files module_path =
  let dir = Filename.dirname module_path in
  try
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f ->
      Filename.check_suffix f ".ml" &&
      not (Filename.check_suffix f "_test.ml"))
    |> List.map (fun f -> Filename.concat dir f)
  with _ -> []

(* Check referential integrity: names in spec vs names in code *)
let check_integrity module_spec impl_path =
  let functions_in_code = Borg_comment.function_names impl_path in
  let exports_set = List.map String.lowercase_ascii module_spec.exports in
  let code_set = List.map String.lowercase_ascii functions_in_code in

  (* Missing: in spec but not in code *)
  let missing = List.filter (fun name ->
    not (List.mem (String.lowercase_ascii name) code_set)
  ) module_spec.exports in

  (* Undocumented: in code but not listed in spec exports *)
  let undocumented = List.filter (fun name ->
    not (List.mem (String.lowercase_ascii name) exports_set)
  ) functions_in_code in

  (missing, undocumented)

(* Format an integrity report *)
let render_report module_name (missing, undocumented) =
  let lines = ref [] in

  if missing <> [] then begin
    lines := Printf.sprintf "  Missing implementations (in spec, not in code):" :: !lines;
    List.iter (fun name ->
      lines := Printf.sprintf "    - %s" name :: !lines
    ) missing
  end;

  if undocumented <> [] then begin
    lines := Printf.sprintf "  Undocumented functions (in code, not in exports):" :: !lines;
    List.iter (fun name ->
      lines := Printf.sprintf "    - %s" name :: !lines
    ) undocumented
  end;

  if !lines = [] then
    Printf.sprintf "%s: ✓ all exports documented" module_name
  else
    Printf.sprintf "%s:\n%s" module_name (String.concat "\n" (List.rev !lines))
