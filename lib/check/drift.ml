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

(** {1 Spec drift: existence checks (spec declares -> code exists)} *)

(* agent note (|
 *   WHAT: Resolve a path declared in a .borg file (an (implements ...)
 *   target or a verify-stanza artifact arg). Tries: relative to the
 *   project dir (repo-root-relative), relative to the .borg file's own
 *   directory, then as-is.
 *   WHY: Pure file-existence check — the honest core of "found in code".
 *   implements paths are conventionally repo-root-relative; verify args
 *   may be either, so we try both.
 * |) *)
let resolve_exists ~dir ~borg_path path =
  let borg_dir = Filename.dirname borg_path in
  let candidates = [
    Filename.concat dir path;
    Filename.concat borg_dir path;
    path;
  ] in
  List.exists Sys.file_exists candidates

(* agent note (|
 *   WHAT: Strip the quoting wrappers borge's printer adds to string
 *   values: surrounding "..." (quoted) or (|...|) (verbatim). Atoms are
 *   returned unchanged.
 *   WHY: verify-stanza args from the AST come back printer-quoted; we
 *   need the raw path text to test file existence.
 * |) *)
(* exempt: String.sub *)
let strip_wrappers s =
  let n = String.length s in
  if n >= 2 && String.get s 0 = '"' && String.get s (n - 1) = '"' then
    String.sub s 1 (n - 2)
  else if n >= 5 && String.sub s 0 2 = "(|" && String.sub s (n - 2) 2 = "|)" then
    String.sub s 2 (n - 4)
  else s

(* agent note (|
 *   WHAT: Check that (implements ...) paths on implemented/verified
 *   sections actually exist on disk. Pure existence: the book declares
 *   a file; does the file exist?
 *   WHY: Replaces the dead feature_exists check (which compared section
 *   names to borge's own CLI verbs — nonsense for user projects).
 *   Existence correspondence, not evidence-of-work. .mli/.ml agreement
 *   is the compiler's job, not ours.
 * |) *)
let check_implements_exist dir borg_path file =
  let mappings = Spec.extract_section_mappings file in
  List.concat_map (fun (m : Spec.section_mapping) ->
    match m.status with
    | Some (Spec.Implemented | Spec.Verified) ->
        List.filter_map (fun impl_path ->
          if resolve_exists ~dir ~borg_path impl_path then None
          else Some { path = borg_path; section_name = m.name;
            description = Printf.sprintf
              "declared (implements %s) but file not found in code" impl_path }
        ) m.implements
    | _ -> []
  ) mappings

(* agent note (|
 *   WHAT: Heuristic — does a verify-stanza argument name an artifact?
 *   True only if it has no spaces AND (carries a known file extension
 *   or contains a path separator).
 *   WHY: verify args can be free-text descriptions (pricklypear's
 *   convention), flags/IDs, or genuine file paths. Real paths have no
 *   spaces; prose descriptions do. We only check existence of
 *   artifacts, never the verification RESULTS.
 * |) *)
let looks_like_path arg =
  not (String.contains arg ' ')
  && (let lc = String.lowercase_ascii arg in
      List.exists (Filename.check_suffix lc)
        [".sh"; ".ksh"; ".py"; ".mjs"; ".ml"; ".mli"; ".sql"; ".go"; ".borg"]
      || String.contains arg '/')

(* agent note (|
 *   WHAT: Check that verify stanzas on implemented/verified sections
 *   reference artifacts that exist. Existence of the verification
 *   APPARATUS, never its results.
 *   WHY: A (verify (script foo.sh)) whose script is missing on an
 *   implemented section is real drift; we never check whether the
 *   verification passed. Planned sections are excluded — their targets
 *   may not exist yet.
 * |) *)
let check_verify_apparatus dir borg_path file =
  let mappings = Spec.extract_section_mappings file in
  List.concat_map (fun (m : Spec.section_mapping) ->
    match m.status with
    | Some (Spec.Implemented | Spec.Verified) ->
        List.concat_map (fun (v : Spec.verify_item) ->
          List.filter_map (fun arg ->
            let raw = strip_wrappers arg in
            if looks_like_path raw && not (resolve_exists ~dir ~borg_path raw) then
              Some { path = borg_path; section_name = m.name;
                description = Printf.sprintf
                  "verify (%s) references missing artifact: %s" v.Spec.method_ raw }
            else None
          ) v.Spec.args
        ) m.verify
    | _ -> []
  ) mappings

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
 *   WHAT: Check for spec drift in a directory — existence correspondence
 *   only. For each .borg file: do (implements ...) paths on
 *   implemented/verified sections exist? do verify-stanza artifact refs
 *   exist? did any verify stanza change since HEAD?
 *   WHY: Drift = does what the book declares exist in code? No evidence
 *   checks (agent-note receipts belong in commits/issues, never the book),
 *   no "is this a real command" dead check.
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
  List.concat_map (fun path ->
    try
      let input = File_utils.read_file path in
      let file = Parse.parse_file input in
      let impl_missing = check_implements_exist dir path file in
      let verify_missing = check_verify_apparatus dir path file in
      let verify_changed = check_verify_drift path file in
      impl_missing @ verify_missing @ verify_changed
    with _ -> []
  ) tree_result

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

(** {1 Orphan file detection (code -> spec completeness)} *)

(* agent note (|
 *   WHAT: Files we always treat as conventional and never flag as
 *   orphans — dotfiles, well-known top-level docs/config, and borge's
 *   own book/meta artifacts.
 *   WHY: A complete-manifest model still respects repo conventions;
 *   these files don't belong in the spec.
 * |) *)
let is_conventional_file rel =
  let basename = Filename.basename rel in
  basename <> "" && basename.[0] = '.'
  || List.mem basename [
    "README"; "README.md"; "LICENSE"; "LICENSE.md"; "AGENTS.md";
    "TODO.md"; "CHANGELOG.md"; "pipe.yaml"; "dune-project"; "dune";
  ]
  || Filename.check_suffix basename ".opam"
  || Filename.check_suffix basename ".borg.meta"
  || Filename.check_suffix basename ".borg"

(* agent note (|
 *   WHAT: Infer an orphan kind from a repo-relative path's directory.
 *   WHY: Lets the drift report distinguish unspecified scripts, docs,
 *   Nopales libs, and migrations so the declaration pass can triage.
 * |) *)
let kind_of_path path =
  let has_prefix p =
    String.length path >= String.length p
    && String.sub path 0 (String.length p) = p
  in
  if has_prefix "scripts/" then "unspecified-script"
  else if has_prefix "docs/" then "unspecified-doc"
  else if has_prefix "libs/" then "unspecified-lib"
  else if has_prefix "migrations/" then "unspecified-migration"
  else "unspecified-file"

(* agent note (|
 *   WHAT: Directory prefixes whose tracked files we audit for
 *   declaration in the spec.
 *   WHY: Scope to artifact dirs the compiler can't see (scripts, docs,
 *   Nopales packages, migrations). lib/ .ml is covered by the dune-aware
 *   unspecified-module check, not here.
 * |) *)
let watched_prefixes = ["scripts/"; "docs/"; "libs/"; "migrations/"]
let is_watched path =
  List.exists (fun p ->
    String.length path >= String.length p
    && String.sub path 0 (String.length p) = p
  ) watched_prefixes

(* agent note (|
 *   WHAT: Extract the raw path string from a sexp node (Atom or
 *   Quoted/Verbatim string, unwrapped).
 *   WHY: (untracked <path> "reason") paths may be atoms or strings.
 * |) *)
let path_of_node = function
  | Ast.Atom (_, a) -> Some a
  | Ast.String (_, Ast.Quoted q) -> Some q.Ast.q_content
  | Ast.String (_, Ast.Verbatim v) -> Some v.Ast.v_content
  | _ -> None

(* agent note (|
 *   WHAT: Parse (untracked <path> "<reason>") stanzas from .borg
 *   forms, recursing through the whole tree (they may be nested inside
 *   the (project ...) wrapper or within sections). Returns the set of
 *   excused repo-relative paths.
 *   WHY: The honest escape hatch — a file may be explicitly excused
 *   WITH a reason instead of declared. Three states per file:
 *   declared (in implements), excused (via untracked), or orphan.
 * |) *)
let extract_untracked dir =
  let excused = Hashtbl.create 16 in
  let rec walk node =
    match node with
    | Ast.List (_, Ast.Atom (_, "untracked") :: path_node :: rest) ->
        (match path_of_node path_node with
         | Some pth -> Hashtbl.replace excused pth true
         | None -> ());
        List.iter walk rest
    | Ast.List (_, children) ->
        List.iter walk children
    | _ -> ()
  in
  List.iter (fun path ->
    try
      let input = File_utils.read_file path in
      let file = Parse.parse_file input in
      List.iter (fun { Ast.node; _ } -> walk node) file.Ast.top_level
    with _ -> ()
  ) (File_utils.find_borg_files dir);
  excused

(* agent note (|
 *   WHAT: Build the set of repo-relative paths declared by (implements ...)
 *   across all .borg sections (joined paths, per extract_implements).
 *   WHY: A file is "declared" if any section's implements names it.
 * |) *)
let declared_paths dir =
  let paths = Hashtbl.create 64 in
  List.iter (fun path ->
    try
      let input = File_utils.read_file path in
      let file = Parse.parse_file input in
      let mappings = Spec.extract_section_mappings file in
      List.iter (fun (m : Spec.section_mapping) ->
        List.iter (fun p -> Hashtbl.replace paths p true) m.implements
      ) mappings
    with _ -> ()
  ) (File_utils.find_borg_files dir);
  paths

(* agent note (|
 *   WHAT: List tracked files via `git ls-files` (clean — excludes
 *   gitignored artifacts like _build/, __pycache__/). Returns [] if
 *   not a git repo.
 *   WHY: Orphan detection audits real, committed files only.
 * |) *)
let tracked_files dir =
  let cmd = Printf.sprintf "git -C %s ls-files 2>/dev/null" (Filename.quote dir) in
  let ic = Unix.open_process_in cmd in
  let lines = ref [] in
  (try while true do lines := input_line ic :: !lines done with End_of_file -> ());
  let status = Unix.close_process_in ic in
  match status with
  | Unix.WEXITED 0 -> List.rev !lines
  | _ -> []

(* agent note (|
 *   WHAT: Check that files in watched dirs (scripts/, docs/, libs/,
 *   migrations/) are declared in some .borg section, explicitly excused
 *   via (untracked ...), or conventional. Remainder = orphan drift.
 *   WHY: The book is the complete manifest — code that exists but
 *   isn't in the book is drift (the code->spec direction). Existence
 *   correspondence, both ways.
 * |) *)
let check_orphan_files dir =
  let excused = extract_untracked dir in
  let declared = declared_paths dir in
  let is_ok rel =
    is_conventional_file rel
    || Hashtbl.mem excused rel
    || Hashtbl.mem declared rel
  in
  List.filter_map (fun rel ->
    if is_watched rel && not (is_ok rel) then
      Some { path = rel; kind = kind_of_path rel; name = Filename.basename rel }
    else None
  ) (tracked_files dir)

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
  unspecified @ check_orphan_files dir

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
  unspecified @ check_orphan_files dir

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
