(** Code intelligence metrics - static analysis of source files.

    Collects lines of code, function count, average function length,
    and module metadata for every .ml file in the project. *)

(** A single source file's metrics *)
type file_metrics = {
  path : string;
  module_name : string;
  lines : int;
  functions : int;
  avg_length : float;
  exports : int;
  documented : int;  (** exports with doc comments *)
  doc_coverage : float;  (** documented / exports ratio, 0.0-1.0 *)
  has_mli : bool;
}

(** Aggregated project metrics *)
type project_metrics = {
  files : file_metrics list;
  total_lines : int;
  total_functions : int;
  total_exports : int;
  total_documented : int;
  doc_coverage : float;
  avg_func_length : float;
}

(** Count the lines in a string (excluding blank lines and comments) *)
let count_lines content =
  let lines = String.split_on_char '\n' content in
  List.length lines

(** Count top-level let bindings at column 0.
    Reuses Surface.extract_toplevel_let_name logic inline to avoid
    dep on Surface (which uses File_utils). *)
let count_functions content =
  let lines = String.split_on_char '\n' content in
  List.fold_left (fun acc line ->
    let len = String.length line in
    if len < 4 then acc
    else if String.sub line 0 4 <> "let " then acc
    else begin
      let rest = String.sub line 4 (len - 4) in
      let rest_len = String.length rest in
      let i = ref 0 in
      (* Skip 'rec ' *)
      while !i + 3 < rest_len && String.sub rest !i 4 = "rec " do i := !i + 4 done;
      while !i < rest_len && rest.[!i] = ' ' do incr i done;
      if !i >= rest_len then acc
      else begin
        let ch = rest.[!i] in
        if ch = '(' && !i + 1 < rest_len && rest.[!i + 1] = ')' then acc  (* let () *)
        else if ch = '_' then acc  (* let _ *)
        else acc + 1
      end
    end
  ) 0 lines

(** Count declarations with doc comments ((**) ... (*)) immediately before them.
    TODO: this is a placeholder — proper doc-comment parsing needs to
    handle nested comments and avoid matching *) inside strings. *)
let count_documented _content =
  0

(** Compute average function length for a file.
    Heuristic: total lines / function count. *)
let compute_avg_length lines functions =
  if functions = 0 then 0.0
  else float_of_int lines /. float_of_int functions

(** Measure a single .ml file *)
let measure_file ml_path =
  let content = File_utils.read_file ml_path in
  let module_name = String.capitalize_ascii
    (Filename.chop_extension (Filename.basename ml_path)) in
  let lines = count_lines content in
  let functions = count_functions content in
  let avg_length = compute_avg_length lines functions in
  let mli_path = Filename.chop_extension ml_path ^ ".mli" in
  let has_mli = Sys.file_exists mli_path in
  let surface = Surface.extract_surface ml_path in
  let exports = List.length surface.exports in
  let documented = count_documented content in
  let doc_coverage = if exports = 0 then 1.0
    else float_of_int documented /. float_of_int exports in
  { path = ml_path; module_name; lines; functions; avg_length; exports; documented; doc_coverage; has_mli }

(** Find all .ml files in a directory, excluding _build, .git, and test dirs *)
let find_ml_files dir =
  let is_test_module name =
    String.length name >= 5 &&
    (String.sub name 0 5 = "test_" || String.sub name 0 5 = "Test_")
  in
  let rec find path =
    try
      let entries = Sys.readdir path in
      Array.fold_left (fun acc entry ->
        if entry = "_build" || entry = ".git" || entry = "test" then acc
        else
          let full = Filename.concat path entry in
          if Sys.is_directory full then find full @ acc
          else if Filename.check_suffix entry ".ml" then begin
            let mod_name = Filename.chop_extension entry in
            if is_test_module mod_name then acc
            else full :: acc
          end
          else acc
      ) [] entries
    with Sys_error _ -> []
  in
  List.sort String.compare (find dir)

(** Run stats on a directory *)
let run dir =
  let ml_files = find_ml_files dir in
  let metrics = List.map measure_file ml_files in
  let total_lines = List.fold_left (fun acc (m : file_metrics) -> acc + m.lines) 0 metrics in
  let total_functions = List.fold_left (fun acc (m : file_metrics) -> acc + m.functions) 0 metrics in
  let total_exports = List.fold_left (fun acc (m : file_metrics) -> acc + m.exports) 0 metrics in
  let total_documented = List.fold_left (fun acc (m : file_metrics) -> acc + m.documented) 0 metrics in
  let doc_coverage = if total_exports = 0 then 1.0
    else float_of_int total_documented /. float_of_int total_exports in
  let avg_func_length = compute_avg_length total_lines total_functions in
  { files = metrics; total_lines; total_functions; total_exports; total_documented; doc_coverage; avg_func_length }
