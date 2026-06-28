open Borge_lang

(** Extract position info from a sexp node *)
let pos_of_sexp = function
  | Ast.Atom (p, _) -> p
  | Ast.String (p, _) -> p
  | Ast.List (p, _) -> p

(** Check if string contains a substring *)
let contains_sub s substr =
  try
    let _ = Str.search_forward (Str.regexp_string substr) s 0 in
    true
  with Not_found -> false

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

(* agent note (|
 *   WHAT: Extract the doc string from a sexp list.
 *   Looks for a (doc ...) form and returns content truncated to 60 chars.
 *   WHY: Used to show section descriptions in drift reports.
 * |) *)
(* exempt: String.sub *)
let rec find_doc : Ast.sexp list -> string = function
  | [] -> ""
  | Ast.List (_, Ast.Atom (_, "doc") :: Ast.String (_, Ast.Verbatim v) :: _) :: _ ->
      let s = v.Ast.v_content in
      if String.length s > 60 then String.sub s 0 57 ^ "..." else s
  | Ast.List (_, Ast.Atom (_, "doc") :: Ast.String (_, Ast.Quoted q) :: _) :: _ ->
      let s = q.Ast.q_content in
      if String.length s > 60 then String.sub s 0 57 ^ "..." else s
  | _ :: rest -> find_doc rest

(* agent note (|
 *   WHAT: Hardcoded list of known borge subcommands.
 *   Distinguishes real commands from user-defined section names.
 *   WHY: Spec drift detection needs to know if an implemented
 *   section is a built-in command or custom feature.
 * |) *)
let known_subcommands = [
  "balance"; "parse"; "check"; "report"; "fmt"; "nodes"; "inline";
  "version"; "print"; "lint"; "drift"; "normalize"; "review";
  "log"; "diff"; "undo";
  "balance-verbose"; "fmt-check"; "fmt-diff"; "worktree-check";
]

(* agent note (|
 *   WHAT: Check if a name is a known built-in borge command.
 *   Returns Some true if known, None if not a command.
 *   WHY: Used during spec drift to distinguish real commands
 *   from user-defined sections marked implemented.
 * |) *)
let feature_exists name =
  if List.mem name known_subcommands then Some true
  else None

(* agent note (|
 *   WHAT: Extract all implemented features from a .borg file.
 *   Walks the sexp tree looking for sections/subsections with
 *   (status implemented), returns their names and descriptions.
 *   WHY: Core spec drift detection: finds what the spec claims
 *   is implemented so we can verify it actually exists.
 * |) *)
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
         | Some n, Some (Spec.Implemented | Spec.Verified) ->
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

(* agent note (|
 *   WHAT: Check if the raw text of a section (from its first line
 *   to the closing of its list form) contains an agent note mentioning
 *   verification or "verified".
 *
 *   WHY: Implemented sections with (verify ...) stanzas should have
 *   agent notes claiming verification was run. This is a coarse
 *   text check — it scans for "agent note" and "verified|verif|verify"
 *   in the same block without full AST traversal.
 * |) *)
let section_has_verified_note section_text =
  let text = String.lowercase_ascii section_text in
  let has_agent = contains_sub text "agent note" || contains_sub text "agent response" in
  let has_verify = contains_sub text "verified" || contains_sub text "verif" in
  has_agent && has_verify

(** Get raw text for each section from file content.
    Returns (name, start_line, end_line) triples.*)
let extract_section_ranges (file : Ast.file) : (string * int * int) list =
  let rec walk_sexp start_line acc = function
    | Ast.List (_, Ast.Atom (_, kind) :: Ast.Atom (_, name) :: rest)
      when kind = "subsection" || kind = "section" || kind = "subsubsection" ->
        let end_line = List.fold_left (fun max_line sexp ->
          max max_line (walk_end sexp)
        ) start_line rest in
        (name, start_line, end_line) :: List.fold_left (walk_child start_line) acc rest
    | Ast.List (_, children) ->
        List.fold_left (walk_child start_line) acc children
    | _ -> acc
  and walk_child _ acc node =
    match node with
    | Ast.List (_, Ast.Atom (_, kind) :: Ast.Atom (_, name) :: rest)
      when kind = "subsection" || kind = "section" || kind = "subsubsection" ->
        let start_line = (pos_of_sexp node).Ast.line in
        let end_line = List.fold_left (fun max_line sexp ->
          max max_line (walk_end sexp)
        ) start_line rest in
        (name, start_line, end_line) :: List.fold_left (walk_child start_line) acc rest
    | Ast.List (_, children) ->
        List.fold_left (walk_child 0) acc children
    | _ -> acc
  and walk_end = function
    | Ast.Atom (p, _) | Ast.String (p, _) -> p.Ast.line
    | Ast.List (p, []) -> p.Ast.line
    | Ast.List (_, children) ->
        List.fold_left (fun acc child -> max acc (walk_end child)) 0 children
  in
  let rec walk_top = function
    | [] -> []
    | { Ast.node; _ } :: rest ->
        let line = (pos_of_sexp node).Ast.line in
        walk_sexp line [] node @ walk_top rest
  in
  walk_top file.top_level

(** Extract raw text between start_line and end_line from file content. *)
let text_between_lines content start_line end_line =
  let lines = String.split_on_char '\n' content in
  let rec take acc curr = function
    | [] -> List.rev acc
    | line :: rest ->
        if curr >= start_line && curr <= end_line then
          take (line :: acc) (curr + 1) rest
        else if curr > end_line then
          List.rev acc
        else
          take acc (curr + 1) rest
  in
  String.concat "\n" (take [] 1 lines)

(** Find implemented sections with verify but no verified agent note. *)
let check_missing_verify_notes path file_content file =
  let mappings = Spec.extract_section_mappings file in
  let impl_with_verify = List.filter (fun (m : Spec.section_mapping) ->
    match m.status with
    | Some (Spec.Implemented | Spec.Verified) -> m.verify <> []
    | _ -> false
  ) mappings in
  if impl_with_verify = [] then []
  else begin
    let ranges = extract_section_ranges file in
    List.filter_map (fun (m : Spec.section_mapping) ->
      match List.find_opt (fun (name, _, _) -> name = m.name) ranges with
      | Some (_, start_line, end_line) ->
          let section_text = text_between_lines file_content start_line end_line in
          if section_has_verified_note section_text then None
          else Some { path; section_name = m.name;
            description = "Implemented section with verify stanza has no agent verification note" }
      | None -> None
    ) impl_with_verify
  end

(** Read file content from git HEAD *)
let read_git_head path =
  let cmd = Printf.sprintf "git show HEAD:%s 2>/dev/null" (Filename.quote path) in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 1024 in
  (try while true do Buffer.add_string buf (input_line ic ^ "\n") done with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Some (Buffer.contents buf)
  | _ -> None

(** Compare verify stanzas between current and HEAD.
    Returns spec_drift items for any existing section whose verify changed. *)
let check_verify_drift path file =
  let current =
    try Spec.extract_section_mappings file
    with _ -> []
  in
  let head = match read_git_head path with
    | Some content ->
        (try
          let head_file = Parse.parse_file content in
          Spec.extract_section_mappings head_file
        with _ -> [])
    | None -> []
  in
  let head_map = Hashtbl.create 16 in
  List.iter (fun (m : Spec.section_mapping) ->
    Hashtbl.replace head_map m.name m.verify
  ) head;
  List.filter_map (fun (m : Spec.section_mapping) ->
    let head_verify = try Hashtbl.find head_map m.name with Not_found -> [] in
    if head_verify = [] then None  (* section didn't exist at HEAD *)
    else if head_verify <> m.verify then
      Some { path; section_name = m.name;
        description = "Verify stanza was modified since HEAD" }
    else None
  ) current

(* agent note (|
 *   WHAT: Check for spec drift in a directory.
 *   Finds all .borg files, extracts implemented features,
 *   and reports any that don't correspond to real commands.
 *   Also checks: verify stanzas modified since HEAD, and
 *   implemented sections with verify missing agent notes.
 *   WHY: One of the three drift detection modes in borge.
 * |) *)
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
  let basic_drift = List.concat_map (fun path ->
    try
      let input = File_utils.read_file path in
      let file = Parse.parse_file input in
      let sections = find_implemented_features file in
      let missing_notes = check_missing_verify_notes path input file in
      let verify_changed = check_verify_drift path file in
      let feature_check = List.filter_map (fun (name, desc) ->
        match feature_exists name with
        | Some false ->
            Some { path; section_name = name; description = desc }
        | Some true | None -> None
      ) sections in
      feature_check @ missing_notes @ verify_changed
    with _ -> []
  ) tree_result in
  basic_drift

(** {1 Dune-aware code drift} *)

(** Build a set of module names covered by .borg sections.
    Uses explicit (implements ...) declarations when present,
    falls back to convention-derived name normalization. *)
let build_covered_modules borg_files =
  let names = Hashtbl.create 16 in
  List.iter (fun path ->
    try
      let input = File_utils.read_file path in
      let file = Parse.parse_file input in
      (* Use the new extract_section_mappings which handles both
         explicit (implements ...) and implicit section names *)
      let mappings = Spec.extract_section_mappings file in
      List.iter (fun (m : Spec.section_mapping) ->
        if m.implements <> [] then
          (* Explicit mapping: (implements lib/ui/ui_dream.ml) → derive module name from filename *)
          List.iter (fun impl_path ->
            try
              let base = Filename.basename impl_path in
              let mod_name = String.capitalize_ascii
                (Filename.remove_extension base) in
              Hashtbl.replace names mod_name true
            with _ -> ()
          ) m.implements
        else
          (* Convention-derived: section "ui-dream" → "Ui_dream" *)
          let normalized = Convention.normalize_section_name m.name in
          let mod_name = String.capitalize_ascii normalized in
          Hashtbl.replace names mod_name true
      ) mappings
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

(** {1 Go-aware code drift} *)

(* agent note (|
 *   WHAT: Check for code drift in a Go project by discovering
 *   packages from directory structure and cross-referencing
 *   against .borg spec sections.
 *
 *   WHY: Go projects don't have dune files — packages are
 *   discovered from the directory layout and go.mod.
 * |) *)
let check_go_code_drift dir =
  let borg_files = File_utils.find_borg_files dir in
  let covered = build_covered_modules borg_files in
  let project = Go_parse.parse_all dir in
  (* Collect all library packages (not cmd executables) *)
  let lib_packages = Go_parse.library_packages project in
  let unspecified = List.filter_map (fun (pkg : Go_parse.go_package) ->
    let capitalized = String.capitalize_ascii pkg.Go_parse.package_name in
    if Hashtbl.mem covered capitalized then None
    else Some { path = pkg.Go_parse.dir_path;
                kind = "unspecified-package";
                name = capitalized }
  ) lib_packages in
  unspecified

(* agent note (|
 *   WHAT: Generate findings from drift analysis for a Go project.
 *   Uses Go_parse for package discovery and Go_surface for
 *   exported symbol extraction.
 *
 *   WHY: The meta generation pipeline needs Go-aware equivalents
 *   of the dune-aware code in generate_findings.
 * |) *)
let generate_go_findings dir =
  let timestamp = Meta.current_timestamp () in
  let borg_files = File_utils.find_borg_files dir in
  let project = Go_parse.parse_all dir in

  (* Collect package surfaces for library packages *)
  let lib_packages = Go_parse.library_packages project in
  let all_surfaces = List.filter_map (fun (pkg : Go_parse.go_package) ->
    if Sys.is_directory pkg.Go_parse.dir_path then
      Some (Go_surface.extract_package_surface pkg.Go_parse.dir_path)
    else None
  ) lib_packages in

  (* Build package snapshot (analogous to dune_snapshot) *)
  let package_snapshot = List.filter_map (fun (pkg : Go_parse.go_package) ->
    let surface = Go_surface.extract_package_surface pkg.Go_parse.dir_path in
    let _exports = Go_surface.exported_names surface in
    Some {
      Meta.name = pkg.Go_parse.package_name;
      Meta.modules = List.map (fun f -> Filename.chop_extension f) pkg.Go_parse.go_files;
      Meta.public_name = (match pkg.Go_parse.kind with
                          | Go_parse.Pkg -> Some pkg.Go_parse.package_name
                          | _ -> None);
      Meta.libraries = [];
    }
  ) lib_packages in

  (* Cross-reference: packages in go.mod but not in .borg *)
  let covered = build_covered_modules borg_files in
  let unspecified_findings = List.filter_map (fun (surface : Go_surface.go_package_surface) ->
    if Hashtbl.mem covered (String.capitalize_ascii surface.Go_surface.package_name) then None
    else Some {
      Meta.ft_type = Unspecified_module;
      Meta.section = None;
      Meta.module_ = Some surface.Go_surface.package_name;
      Meta.file = Some surface.Go_surface.path;
      Meta.export = None;
      Meta.detail = Some (Printf.sprintf "package %s not mentioned in any .borg spec" surface.Go_surface.package_name);
      Meta.spec_status = None;
      Meta.actual_status = None;
      Meta.confidence = High;
      Meta.source = Static;
      Meta.at = timestamp;
    }
  ) all_surfaces in

  (* Cross-reference: exports in package but not in spec *)
  let export_findings =
    let surfaces_with_spec = List.filter (fun (surface : Go_surface.go_package_surface) ->
      Hashtbl.mem covered (String.capitalize_ascii surface.Go_surface.package_name)
    ) all_surfaces in
    List.filter_map (fun (surface : Go_surface.go_package_surface) ->
      let exported = Go_surface.exported_names surface in
      if List.length exported > 15 then
        Some {
          Meta.ft_type = Extra_export;
          Meta.section = None;
          Meta.module_ = Some surface.Go_surface.package_name;
          Meta.file = Some surface.Go_surface.path;
          Meta.export = None;
          Meta.detail = Some (Printf.sprintf
            "package %s has %d exports — may not all be covered by spec"
            surface.Go_surface.package_name (List.length exported));
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

  let findings = Meta.sort_findings (unspecified_findings @ export_findings) in
  let meta_surfaces = List.map (fun (s : Go_surface.go_package_surface) ->
    let exports = Go_surface.exported_names s in
    { Meta.module_name = s.Go_surface.package_name; Meta.file = s.Go_surface.path; Meta.exports = exports }
  ) all_surfaces in

  let project_name = match project.Go_parse.go_mod with
    | Some m -> m.Go_parse.module_path
    | None -> "go-project"
  in
  let meta : Meta.meta = {
    Meta.project_name = project_name;
    Meta.analyzed_at = timestamp;
    Meta.content_hashes = content_hashes;
    Meta.module_surfaces = meta_surfaces;
    Meta.dune_snapshot = package_snapshot;
    Meta.findings = findings;
  } in
  meta

(** Write .borg.meta files for Go projects *)
let write_go_meta_files dir =
  let meta = generate_go_findings dir in
  let borg_files = File_utils.find_borg_files dir in
  List.iter (fun borg_path ->
    let meta_path = Meta.meta_path_of borg_path in
    let project_name = try
      let input = File_utils.read_file borg_path in
      let file = Parse.parse_file input in
      Spec.project_name file
    with _ -> Some "go-project"
    in
    let per_meta = { meta with Meta.project_name = Option.value project_name ~default:"go-project" } in
    Meta.write_meta meta_path per_meta
  ) borg_files;
  meta

(** {1 Main entry point with convention dispatch} *)

let run dir =
  let spec = check_spec_drift dir in
  let convention = Convention.resolve dir in
  let code = match convention with
    | Convention.Go_standard -> check_go_code_drift dir
    | Convention.Ocaml_dune -> check_code_drift dir
  in
  let structural = check_structural_drift dir in
  { spec_drift = spec; code_drift = code; structural_drift = structural }
