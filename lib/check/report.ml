open Borge_lang

type file_stats = {
  path : string;
  project_name : string option;
  implemented : int;
  in_progress : int;
  planned : int;
}

type totals = {
  implemented : int;
  in_progress : int;
  planned : int;
}

type result = {
  files : file_stats list;
  totals : totals;
  orphans : string list;  (** .borg files not in the inline tree *)
}

(* agent note (|
 *   WHAT: Parse a .borg file and extract status counts.
 *   Returns file_stats with path, project name, and counts of
 *   implemented, in_progress, and planned sections.
 *   WHY: Used by borge report to compute per-file and aggregate
 *   statistics about spec completion.
 * |) *)
let stats_of_file path =
  let input = File_utils.read_file path in
  try
    let file = Parse.parse_file input in
    let name = Spec.project_name file in
    let counts = Spec.status_counts file in
    let impl = try List.assoc Spec.Implemented counts with Not_found -> 0 in
    let plan = try List.assoc Spec.Planned counts with Not_found -> 0 in
    let prog = try List.assoc Spec.In_progress counts with Not_found -> 0 in
    Some { path; project_name = name; implemented = impl; in_progress = prog; planned = plan }
  with _ -> None

(** Run report with inline-tree awareness.
    Builds the project tree from root file(s), walks it,
    and only counts each .borg file once. Also detects orphans. *)
let run dir =
  let roots = Project.find_roots dir in
  let trees = List.filter_map (fun root ->
    match Project.build_tree root with
    | Ok tree -> Some tree
    | Error _ -> None
  ) roots in
  (* Collect all paths from all trees (deduplicated by tree walker) *)
  let tree_paths = List.concat_map Project.tree_paths trees in
  (* Find orphans: files not in any tree *)
  let all_files = File_utils.find_borg_files dir in
  let orphans = List.filter (fun path ->
    not (List.mem path tree_paths)
  ) all_files in
  (* Build per-file stats only for tree paths *)
  let rows = List.filter_map stats_of_file tree_paths in
  let tot_impl : int = List.fold_left (fun acc (r : file_stats) -> acc + r.implemented) 0 rows in
  let tot_plan : int = List.fold_left (fun acc (r : file_stats) -> acc + r.planned) 0 rows in
  let tot_prog : int = List.fold_left (fun acc (r : file_stats) -> acc + r.in_progress) 0 rows in
  { files = rows; totals = { implemented = tot_impl; in_progress = tot_prog; planned = tot_plan }; orphans }
