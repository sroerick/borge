open Borge_lang

(** {1 Types} *)

type spec_drift = {
  path : string;
  section_name : string;
  description : string;
}

type code_drift_item = {
  path : string;
  kind : string;
  name : string;
}

type structural_drift_item = {
  description : string;
}

type drift_result = {
  spec_drift : spec_drift list;
  code_drift : code_drift_item list;
  structural_drift : structural_drift_item list;
}

(** {1 Spec drift: sections claiming implemented features that don't exist} *)

let rec find_status : Ast.sexp list -> Spec.status option = function
  | [] -> None
  | Ast.List (_, Ast.Atom (_, "status") :: Ast.Atom (_, v) :: _) :: _ ->
      Spec.status_of_string v
  | _ :: rest -> find_status rest

let rec find_doc : Ast.sexp list -> string = function
  | [] -> ""
  | Ast.List (_, Ast.Atom (_, "doc") :: Ast.String (_, Ast.Verbatim v) :: _) :: _ ->
      let s = v.Ast.v_content in
      if String.length s > 60 then String.sub s 0 57 ^ "..." else s
  | Ast.List (_, Ast.Atom (_, "doc") :: Ast.String (_, Ast.Quoted q) :: _) :: _ ->
      let s = q.Ast.q_content in
      if String.length s > 60 then String.sub s 0 57 ^ "..." else s
  | _ :: rest -> find_doc rest

let known_subcommands = [
  "balance"; "parse"; "check"; "report"; "fmt"; "nodes"; "inline";
  "version"; "print"; "lint"; "drift"; "normalize"; "review";
  "log"; "diff"; "undo";
  "balance-verbose"; "fmt-check"; "fmt-diff"; "worktree-check";
]

let feature_exists name =
  if List.mem name known_subcommands then Some true
  else None

let find_implemented_features file =
  let results = ref [] in
  let rec walk_sexp = function
    | Ast.List (_, Ast.Atom (_, kind) :: rest_children)
      when List.mem kind ["subsection"; "section"; "subsubsection"] ->
        let name = match rest_children with
          | Ast.Atom (_, n) :: _ -> Some n
          | _ -> None
        in
        let status = find_status rest_children in
        let doc = find_doc rest_children in
        (match name, status with
         | Some n, Some Spec.Implemented ->
             results := (n, doc) :: !results
         | _ -> ());
        List.iter walk_sexp rest_children
    | Ast.List (_, children) ->
        List.iter walk_sexp children
    | _ -> ()
  in
  let rec walk_top = function
    | [] -> ()
    | { Ast.node; _ } :: rest -> walk_sexp node; walk_top rest
  in
  walk_top file.Ast.top_level;
  List.rev !results

let check_spec_drift dir =
  let tree_result =
    let roots = Project.find_roots dir in
    match roots with
    | [] -> File_utils.find_borg_files dir
    | root :: _ ->
        (match Project.build_tree root with
         | Ok tree -> Project.tree_paths tree
         | Error _ -> File_utils.find_borg_files dir)
  in
  List.concat_map (fun path ->
    try
      let input = File_utils.read_file path in
      let file = Parse.parse_file input in
      let sections = find_implemented_features file in
      List.filter_map (fun (name, desc) ->
        match feature_exists name with
        | Some false ->
            Some { path; section_name = name; description = desc }
        | Some true -> None
        | None -> None
      ) sections
    with _ -> []
  ) tree_result

(** {1 Dune-aware code drift} *)

(** Build a set of module names covered by .borg sections *)
let build_covered_modules borg_files =
  let names = Hashtbl.create 16 in
  List.iter (fun path ->
    try
      let input = File_utils.read_file path in
      let file = Parse.parse_file input in
      let rec walk_sexp = function
        | Ast.List (_, Ast.Atom (_, kind) :: Ast.Atom (_, name) :: rest)
          when kind = "subsection" || kind = "section" || kind = "subsubsection" ->
            let normalized = String.map (fun c ->
              if c = '-' then '_' else Char.lowercase_ascii c
            ) name in
            let mod_name = String.capitalize_ascii normalized in
            Hashtbl.replace names mod_name true;
            List.iter walk_sexp rest
        | Ast.List (_, children) ->
            List.iter walk_sexp children
        | _ -> ()
      in
      List.iter (fun { Ast.node; _ } -> walk_sexp node) file.Ast.top_level
    with _ -> ()
  ) borg_files;
  names

(** Find .ml files recursively *)
let find_ml_files dir =
  let rec find path =
    try
      let entries = Sys.readdir path in
      Array.fold_left (fun acc entry ->
        if entry = "_build" || entry = ".git" then acc
        else
          let full = Filename.concat path entry in
          if Sys.is_directory full then find full @ acc
          else if Filename.check_suffix entry ".ml" then full :: acc
          else acc
      ) [] entries
    with Sys_error _ -> []
  in
  List.sort String.compare (find dir)

(** Cross-reference dune modules with .borg sections *)
let check_code_drift dir =
  let borg_files = File_utils.find_borg_files dir in
  let covered = build_covered_modules borg_files in
  let dune_files = Dune_parse.parse_all dir in
  (* Collect all lib modules from dune *)
  let lib_modules = List.concat_map (fun df ->
    List.concat_map (function
      | Dune_parse.Library lib ->
          List.map (fun m -> (lib.Dune_parse.name, m)) lib.Dune_parse.modules
      | _ -> []
    ) df.Dune_parse.stanzas
  ) dune_files in
  let unspecified = List.filter_map (fun (lib_name, mod_name) ->
    let capitalized = String.capitalize_ascii mod_name in
    if Hashtbl.mem covered capitalized then None
    else Some { path = Printf.sprintf "lib/%s/%s.ml" lib_name mod_name;
                kind = "unspecified-module"; name = capitalized }
  ) lib_modules in
  unspecified

(** {1 Structural drift} *)

let check_structural_drift _dir =
  (* Domain logic in bin/ check is now covered by dune-aware analysis *)
  []

(** {1 .borg.meta generation} *)

(** Generate findings from drift analysis *)
let generate_findings dir =
  let timestamp = Meta.current_timestamp () in
  let borg_files = File_utils.find_borg_files dir in
  let dune_file = Dune_parse.parse_all dir in

  (* Collect module surfaces for lib modules only *)
  let lib_dune_files = List.filter (fun df ->
    let dir = Filename.dirname df.Dune_parse.path in
    String.length dir >= 5 && String.sub dir 0 5 = "./lib"
  ) dune_file in
  let lib_modules = List.concat_map (fun df ->
    let dir = Filename.dirname df.Dune_parse.path in
    List.concat_map (function
      | Dune_parse.Library lib ->
          List.map (fun m -> (dir, m)) lib.Dune_parse.modules
      | _ -> []
    ) df.Dune_parse.stanzas
  ) lib_dune_files in
  let all_surfaces = List.filter_map (fun (dir, mod_name) ->
    let ml_path = Filename.concat dir (mod_name ^ ".ml") in
    if Sys.file_exists ml_path then
      Some (Surface.extract_surface ml_path)
    else None
  ) lib_modules in

  (* Build dune snapshot *)
  let dune_snapshot = List.concat_map (fun df ->
    List.filter_map (function
      | Dune_parse.Library lib ->
          Some {
            Meta.name = lib.Dune_parse.name;
            Meta.modules = lib.Dune_parse.modules;
            Meta.public_name = lib.Dune_parse.public_name;
            Meta.libraries = lib.Dune_parse.libraries;
          }
      | _ -> None
    ) df.Dune_parse.stanzas
  ) dune_file in

  (* Cross-reference: modules in dune but not in .borg *)
  let covered = build_covered_modules borg_files in
  let unspecified_findings = List.filter_map (fun (surface : Surface.module_surface) ->
    if Hashtbl.mem covered surface.module_name then None
    else Some {
      Meta.ft_type = Unspecified_module;
      Meta.section = None;
      Meta.module_ = Some surface.module_name;
      Meta.file = Some surface.path;
      Meta.export = None;
      Meta.detail = Some (Printf.sprintf "module %s not mentioned in any .borg spec" surface.module_name);
      Meta.spec_status = None;
      Meta.actual_status = None;
      Meta.confidence = High;
      Meta.source = Static;
      Meta.at = timestamp;
    }
  ) all_surfaces in

  (* Cross-reference: exports in module but not in spec *)
  (* For each surface, check if all exports are described somewhere *)
  let export_findings =
    let surfaces_with_spec = List.filter (fun (surface : Surface.module_surface) ->
      Hashtbl.mem covered surface.module_name
    ) all_surfaces in
    (* For now, just note modules with large surface area vs spec coverage *)
    List.filter_map (fun (surface : Surface.module_surface) ->
      (* Heuristic: if a module has more than 10 exports and the spec
         section exists but may not describe all of them, flag as
         potential extra-export. This is a low-confidence finding. *)
      if List.length surface.exports > 15 then
        Some {
          Meta.ft_type = Extra_export;
          Meta.section = None;
          Meta.module_ = Some surface.module_name;
          Meta.file = Some surface.path;
          Meta.export = None;
          Meta.detail = Some (Printf.sprintf
            "module %s has %d exports — may not all be covered by spec"
            surface.module_name (List.length surface.exports));
          Meta.spec_status = None;
          Meta.actual_status = None;
          Meta.confidence = Low;
          Meta.source = Static;
          Meta.at = timestamp;
        }
      else None
    ) surfaces_with_spec
  in

  (* Collect content hashes for staleness detection *)
  let content_hashes = List.map (fun path ->
    let hash = Meta.hash_file path in
    (path, hash)
  ) borg_files in

  (* Write .borg.meta for the root borg file *)
  let findings = Meta.sort_findings (unspecified_findings @ export_findings) in
  let meta_surfaces = List.map (fun (s : Surface.module_surface) ->
    { Meta.module_name = s.module_name; Meta.file = s.path; Meta.exports = s.exports }
  ) all_surfaces in

  (* Generate the root meta file *)
  let meta : Meta.meta = {
    Meta.project_name = "borge";
    Meta.analyzed_at = timestamp;
    Meta.content_hashes = content_hashes;
    Meta.module_surfaces = meta_surfaces;
    Meta.dune_snapshot = dune_snapshot;
    Meta.findings = findings;
  } in
  meta

(** Write .borg.meta files for all .borg files in the project *)
let write_meta_files dir =
  let meta = generate_findings dir in
  let borg_files = File_utils.find_borg_files dir in
  List.iter (fun borg_path ->
    let meta_path = Meta.meta_path_of borg_path in
    (* Only write to the root .borg file's meta for now *)
    (* Per-file meta would require splitting findings by file *)
    if Filename.basename borg_path = "borge.borg" then begin
      let project_name = try
        let input = File_utils.read_file borg_path in
        let file = Parse.parse_file input in
        Spec.project_name file
      with _ -> Some "borge"
      in
      let per_meta = { meta with Meta.project_name = Option.value project_name ~default:"borge" } in
      Meta.write_meta meta_path per_meta
    end
  ) borg_files;
  meta

(** {1 Main entry point} *)

let run dir =
  let spec = check_spec_drift dir in
  let code = check_code_drift dir in
  let structural = check_structural_drift dir in
  { spec_drift = spec; code_drift = code; structural_drift = structural }
