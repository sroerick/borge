(* Calculate documentation coverage statistics
 *
 * roerick note (|
 *   Computes coverage percentages and generates reports.
 *   Excludes tests, internal helpers, and exempted bindings.
 *   Writes (documentation ...) blocks to .borg.meta files.
 * |) *)



(** Coverage statistics for a file *)
type file_coverage = {
  path : string;
  total_bindings : int;
  documented : int;
  exempt : int;
  undocumented : int;
  coverage_percent : float;
  undocumented_names : string list;
}

(** Project-wide coverage summary *)
type project_coverage = {
  files : file_coverage list;
  total_files : int;
  total_bindings : int;
  total_documented : int;
  total_exempt : int;
  total_undocumented : int;
  overall_percent : float;
}

(** Calculate coverage for a single file *)
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
  let undocumented = total - documented - exempt in
  let undoc_names = List.filter_map (fun ((b : Doc_extract.binding_info), doc_opt) ->
    match doc_opt with
    | Some bd when not (Doc_detect.binding_has_doc bd || Doc_detect.binding_is_exempt bd) ->
        Some b.name
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
    undocumented;
    coverage_percent = percent;
    undocumented_names = undoc_names;
  }

(** Calculate coverage for a directory *)
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
    total_undocumented;
    overall_percent = overall;
  }

(** Format coverage as human-readable string *)
let format_coverage (fc : file_coverage) =
  Printf.sprintf "%s: %d/%d documented (%.1f%%), %d exempt, %d undocumented"
    fc.path fc.documented fc.total_bindings fc.coverage_percent
    fc.exempt fc.undocumented

(** Format project summary *)
let format_project_summary (pc : project_coverage) =
  String.concat "\n" ([
    Printf.sprintf "Documentation Coverage Summary (%d files):" pc.total_files;
    Printf.sprintf "  Total bindings: %d" pc.total_bindings;
    Printf.sprintf "  Documented: %d (%.1f%%)" pc.total_documented pc.overall_percent;
    Printf.sprintf "  Exempt: %d" pc.total_exempt;
    Printf.sprintf "  Undocumented: %d" pc.total_undocumented;
  ] @ if pc.total_undocumented > 0 then [
    "";
    "Files with undocumented bindings:"
  ] @ List.filter_map (fun (fc : file_coverage) ->
    if fc.undocumented > 0 then
      Some (Printf.sprintf "  %s (%d undocumented)" fc.path fc.undocumented)
    else None
  ) pc.files
  else [])

(** Generate (documentation ...) block for .borg.meta file *)
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
  Buffer.add_string buf (Printf.sprintf "  (undocumented %d)\n" fc.undocumented);
  Buffer.add_string buf (Printf.sprintf "  (coverage-percent %.2f)\n" fc.coverage_percent);
  if fc.undocumented_names <> [] then begin
    Buffer.add_string buf "  (undocumented-names\n";
    List.iter (fun name ->
      Buffer.add_string buf (Printf.sprintf "    (name \"%s\")\n" name)
    ) fc.undocumented_names;
    Buffer.add_string buf "  ))\n"
  end;
  Buffer.add_string buf ")\n";
  Buffer.contents buf

(** Write coverage to .borg.meta file *)
let write_coverage_meta fc meta_path =
  let content = generate_meta_block fc in
  let oc = open_out meta_path in
  output_string oc content;
  close_out oc

(** Check if coverage meets threshold *)
let meets_threshold (fc : file_coverage) threshold =
  fc.coverage_percent >= threshold
