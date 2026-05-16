type project_node = {
  path : string;
  project_name : string option;
  children : project_node list;
  is_orphan : bool;
}

type build_error =
  | File_not_found of string
  | Parse_error of string * string  (** path, message *)
  | Cycle_detected of string list   (** the cycle path *)

(** Build the inline tree starting from a root .borg file.
    Recursively follows (inline filename.borg) directives.
    Filenames are resolved relative to the directory of the parent file. *)
let rec build_tree ?(visited=[]) path =
  if List.mem path visited then
    Error (Cycle_detected (path :: visited))
  else if not (Sys.file_exists path) then
    Error (File_not_found path)
  else
    try
      let input = File_utils.read_file path in
      let file = Borge_sexp.Parse.parse_file input in
      let name = Spec.project_name file in
      let targets = Spec.inline_targets file in
      let dir = Filename.dirname path in
      let children_with_errors =
        List.map (fun filename ->
          let child_path =
            if Filename.is_relative filename then
              Filename.concat dir filename
            else
              filename
          in
          build_tree ~visited:(path :: visited) child_path
        ) targets
      in
      (* Collect errors from children *)
      let first_error = List.find_map (function Error e -> Some e | _ -> None) children_with_errors in
      match first_error with
      | Some e -> Error e
      | None ->
          let children = List.filter_map (function Ok n -> Some n | _ -> None) children_with_errors in
          Ok { path; project_name = name; children; is_orphan = false }
    with Borge_sexp.Error.Parse_error e ->
      Error (Parse_error (path, Printf.sprintf "parse error at %d:%d - %s" e.line e.column e.message))

(** Find all .borg files that are NOT inlined by any other file.
    These are the "root" candidates. *)
let find_root_candidates dir =
  let all_files = File_utils.find_borg_files dir in
  let all_targets =
    List.concat_map (fun path ->
      try
        let input = File_utils.read_file path in
        let file = Borge_sexp.Parse.parse_file input in
        let dir = Filename.dirname path in
        List.map (fun filename ->
          if Filename.is_relative filename then
            Filename.concat dir filename
          else
            filename
        ) (Spec.inline_targets file)
      with _ -> []
    ) all_files
  in
  List.filter (fun path -> not (List.mem path all_targets)) all_files

(** Find the root .borg file in a directory.
    - If there's exactly one file not inlined by any other, that's the root.
    - If there are multiple candidates, return all of them. *)
let find_roots dir =
  find_root_candidates dir

(** Find all orphaned .borg files: not inlined by any parent, no (no-inline) declaration,
    and doesn't inline any children. Files that inline others are implicitly roots. *)
let find_orphans dir =
  let roots = find_root_candidates dir in
  List.filter (fun path ->
    try
      let input = File_utils.read_file path in
      let file = Borge_sexp.Parse.parse_file input in
      let has_inlines = Spec.inline_targets file <> [] in
      let declares_no_inline = Spec.has_no_inline file in
      not has_inlines && not declares_no_inline
    with _ -> false
  ) roots

(** Collect all paths in the tree (including root) *)
let rec tree_paths node =
  node.path :: List.concat_map tree_paths node.children

(** Total status counts across the tree (no double-counting) *)
let tree_status_counts node =
  let visited = Hashtbl.create 16 in
  let rec walk n =
    if Hashtbl.mem visited n.path then ()
    else begin
      Hashtbl.add visited n.path true;
      List.iter walk n.children
    end
  in
  walk node;
  let all_files = Hashtbl.fold (fun path _ acc -> path :: acc) visited [] in
  let impl = ref 0 in
  let prog = ref 0 in
  let plan = ref 0 in
  List.iter (fun path ->
    try
      let input = File_utils.read_file path in
      let file = Borge_sexp.Parse.parse_file input in
      let counts = Spec.status_counts file in
      impl := !impl + (try List.assoc Spec.Implemented counts with Not_found -> 0);
      prog := !prog + (try List.assoc Spec.In_progress counts with Not_found -> 0);
      plan := !plan + (try List.assoc Spec.Planned counts with Not_found -> 0);
    with _ -> ()
  ) all_files;
  (!impl, !prog, !plan)

(** Format the tree as indented text *)
let rec format_tree ?(indent=0) node =
  let pad = String.make (indent * 2) ' ' in
  let name_str = match node.project_name with
    | Some n -> Printf.sprintf "%s (%s)" (Filename.basename node.path) n
    | None -> Filename.basename node.path
  in
  let orphan_str = if node.is_orphan then " [ORPHAN]" else "" in
  let line = Printf.sprintf "%s%s%s" pad name_str orphan_str in
  line :: List.concat_map (format_tree ~indent:(indent + 1)) node.children
