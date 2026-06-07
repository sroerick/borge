(* Calculate documentation coverage statistics
 *
 * agent note (|
 *   WHAT: Computes per-file and project-wide documentation coverage
 *   statistics against OCaml let bindings. Generates (documentation ...)
 *   sexp blocks for .borg.meta files.
 *
 *   WHY: borge drift --docs needs machine-readable coverage data to
 *   enforce documentation thresholds. The coverage calculation must
 *   distinguish documented, exempt, drifted, and undocumented bindings.
 * |) *)



(** Coverage statistics for a file *)
type file_coverage = {
  path : string;
  total_bindings : int;
  documented : int;
  exempt : int;
  drifted : int;  (** Documented but marked (status drifted) *)
  undocumented : int;
  coverage_percent : float;
  undocumented_names : string list;
  drifted_names : string list;
}

(** Project-wide coverage summary *)
type project_coverage = {
  files : file_coverage list;
  total_files : int;
  total_bindings : int;
  total_documented : int;
  total_exempt : int;
  total_drifted : int;
  total_undocumented : int;
  overall_percent : float;
}

(* agent note (|
 *   WHAT: Calculate documentation coverage for a single .ml file.
 *   Extracts bindings and docs, matches them, and computes coverage
 *   percentages. Skips test and internal bindings.
 *
 *   WHY: Per-file coverage is the unit of reporting for drift --docs.
 *   Each file gets its own coverage stats and appears in the summary.
 * |) *)
let calculate_file_coverage path =
  let bindings = Doc_extract.extract_bindings path in
  let binding_docs = Doc_detect.extract_file_docs path in
  
  (* Match bindings with their docs *)
  let matched = List.filter_map (fun (b : Doc_extract.binding_info) ->
    if b.is_test || b.is_internal then
      None  (* Skip tests and internal *)
    else
      let doc = List.find_opt (fun (bd : Doc_detect.binding_doc) ->
        bd.binding_line = b.line
      ) binding_docs in
      Some (b, doc)
  ) bindings in
  
  let total = List.length matched in
  let documented = List.filter (fun (_, doc_opt) ->
    match doc_opt with
    | Some bd -> Doc_detect.binding_has_doc bd
    | None -> false
  ) matched |> List.length
  in
  let exempt = List.filter (fun (_, doc_opt) ->
    match doc_opt with
    | Some bd -> Doc_detect.binding_is_exempt bd
    | None -> false
  ) matched |> List.length
  in
  let drifted = List.filter (fun (_, doc_opt) ->
    match doc_opt with
    | Some bd -> Doc_detect.binding_is_drifted bd && Doc_detect.binding_has_doc bd
    | None -> false
  ) matched |> List.length
  in
  let drifted_names = List.filter_map (fun ((b : Doc_extract.binding_info), doc_opt) ->
    match doc_opt with
    | Some bd when Doc_detect.binding_is_drifted bd -> Some b.name
    | _ -> None
  ) matched in
  let undocumented = total - documented - exempt in
  let undoc_names = List.filter_map (fun ((b : Doc_extract.binding_info), doc_opt) ->
    match doc_opt with
    | Some bd when not (Doc_detect.binding_has_doc bd || Doc_detect.binding_is_exempt bd) ->
        Some b.name
    | None -> Some b.name
    | _ -> None
  ) matched in
  
  let percent =
    if total - exempt <= 0 then 100.0
    else float_of_int documented /. float_of_int (total - exempt) *. 100.0
  in
  
  {
    path;
    total_bindings = total;
    documented;
    exempt;
    drifted;
    undocumented;
    coverage_percent = percent;
    undocumented_names = undoc_names;
    drifted_names;
  }

(* agent note (|
 *   WHAT: Calculate documentation coverage for all .ml files
 *   under a directory, aggregating per-file stats into a project summary.
 *
 *   WHY: borge drift --docs needs project-wide coverage to enforce
 *   thresholds at the repository level.
 * |) *)
let calculate_dir_coverage dir =
  let ml_files = Doc_extract.find_ml_files dir in
  let file_coverages = List.map calculate_file_coverage ml_files in
  
  let total_bindings = List.fold_left (fun acc (fc : file_coverage) ->
    acc + fc.total_bindings
  ) 0 file_coverages in
  let total_documented = List.fold_left (fun acc fc ->
    acc + fc.documented
  ) 0 file_coverages in
  let total_exempt = List.fold_left (fun acc fc ->
    acc + fc.exempt
  ) 0 file_coverages in
  let total_drifted = List.fold_left (fun acc fc ->
    acc + fc.drifted
  ) 0 file_coverages in
  let total_undocumented = List.fold_left (fun acc fc ->
    acc + fc.undocumented
  ) 0 file_coverages in
  
  let overall =
    if total_bindings - total_exempt <= 0 then 100.0
    else float_of_int total_documented /. float_of_int (total_bindings - total_exempt) *. 100.0
  in
  
  {
    files = file_coverages;
    total_files = List.length file_coverages;
    total_bindings;
    total_documented;
    total_exempt;
    total_drifted;
    total_undocumented;
    overall_percent = overall;
  }

(* agent note (|
 *   WHAT: Format per-file coverage as a human-readable string:
 *   "path: N/M documented (X.X%), E exempt, U undocumented, D drifted"
 *
 *   WHY: Used by drift --docs to display per-file results.
 *   Drifted count is only shown when > 0 to avoid noise.
 * |) *)
let format_coverage (fc : file_coverage) =
  let base = Printf.sprintf "%s: %d/%d documented (%.1f%%), %d exempt, %d undocumented"
    fc.path fc.documented fc.total_bindings fc.coverage_percent
    fc.exempt fc.undocumented in
  if fc.drifted > 0 then
    base ^ Printf.sprintf ", %d drifted" fc.drifted
  else
    base

(* agent note (|
 *   WHAT: Format project-wide coverage as a multi-line summary
 *   showing total, documented, exempt, drifted, undocumented
 *   counts, plus a list of files with undocumented bindings.
 *
 *   WHY: The primary output format for borge drift --docs.
 *   Gives humans an at-a-glance view of project documentation health.
 * |) *)
let format_project_summary (pc : project_coverage) =
  let drifted_line =
    if pc.total_drifted > 0 then [Printf.sprintf "  Drifted: %d" pc.total_drifted] else []
  in
  let undoc_files =
    if pc.total_undocumented > 0 then
      "" :: "Files with undocumented bindings:" ::
      List.filter_map (fun (fc : file_coverage) ->
        if fc.undocumented > 0 then
          Some (Printf.sprintf "  %s (%d undocumented)" fc.path fc.undocumented)
        else None
      ) pc.files
    else []
  in
  String.concat "\n" (
    Printf.sprintf "Documentation Coverage Summary (%d files):" pc.total_files ::
    Printf.sprintf "  Total bindings: %d" pc.total_bindings ::
    Printf.sprintf "  Documented: %d (%.1f%%)" pc.total_documented pc.overall_percent ::
    Printf.sprintf "  Exempt: %d" pc.total_exempt ::
    drifted_line @
    Printf.sprintf "  Undocumented: %d" pc.total_undocumented ::
    undoc_files
  )

(* agent note (|
 *   WHAT: Generate a (documentation ...) sexp block for a .borg.meta
 *   file, containing timestamp, file path, all coverage metrics,
 *   and lists of drifted and undocumented binding names.
 *
 *   WHY: .borg.meta files are the machine-readable companion to .borg
 *   specs. Borge stats and other tools read these blocks to produce
 *   project-wide reports without re-scanning source files.
 * |) *)
let generate_meta_block (fc : file_coverage) =
  let timestamp =
    let now = Unix.gmtime (Unix.time ()) in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
      (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday
      now.tm_hour now.tm_min now.tm_sec
  in
  let buf = Buffer.create 512 in
  Buffer.add_string buf "(documentation\n";
  Buffer.add_string buf (Printf.sprintf "  (analyzed-at \"%s\")\n" timestamp);
  Buffer.add_string buf (Printf.sprintf "  (file \"%s\")\n" fc.path);
  Buffer.add_string buf (Printf.sprintf "  (total-bindings %d)\n" fc.total_bindings);
  Buffer.add_string buf (Printf.sprintf "  (documented %d)\n" fc.documented);
  Buffer.add_string buf (Printf.sprintf "  (exempt %d)\n" fc.exempt);
  Buffer.add_string buf (Printf.sprintf "  (drifted %d)\n" fc.drifted);
  Buffer.add_string buf (Printf.sprintf "  (undocumented %d)\n" fc.undocumented);
  Buffer.add_string buf (Printf.sprintf "  (coverage-percent %.2f)\n" fc.coverage_percent);
  if fc.drifted_names <> [] then begin
    Buffer.add_string buf "  (drifted-names\n";
    List.iter (fun name ->
      Buffer.add_string buf (Printf.sprintf "    (name \"%s\")\n" name)
    ) fc.drifted_names;
    Buffer.add_string buf "  )\n"
  end;
  if fc.undocumented_names <> [] then begin
    Buffer.add_string buf "  (undocumented-names\n";
    List.iter (fun name ->
      Buffer.add_string buf (Printf.sprintf "    (name \"%s\")\n" name)
    ) fc.undocumented_names;
    Buffer.add_string buf "  ))\n"
  end;
  Buffer.add_string buf ")\n";
  Buffer.contents buf

(* agent note (|
 *   WHAT: Write a coverage metadata block to a .borg.meta file path.
 *
 *   WHY: Persists coverage data between runs so that drift --stale
 *   can skip files that haven't changed.
 * |) *)
let write_coverage_meta fc meta_path =
  let content = generate_meta_block fc in
  let oc = open_out meta_path in
  output_string oc content;
  close_out oc

(* agent note (|
 *   WHAT: Check if a file's coverage meets the given threshold percentage.
 *
 *   WHY: Used by drift --docs to determine exit codes: files below
 *   threshold produce warnings or errors depending on --strict.
 * |) *)
let meets_threshold (fc : file_coverage) threshold =
  fc.coverage_percent >= threshold
