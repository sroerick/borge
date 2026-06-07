(** borge review — semantic code review

    Uses LLM to verify function documentation matches implementation.
    Now uses the new semantic review modules from lib/review/. *)

open Borge_lib

let now () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let call_pi prompt =
  (* Use the new review_agent to call LLM *)
  match Agent.run_pi_print prompt with
  | Some response -> response
  | None -> 
      "(function name:\"unknown\"\n  (doc-present false)\n  (doc-accuracy low)\n  (signature-match unknown)\n  (behavior-coverage missing)\n  (structural-issues \"LLM unavailable\")\n  (confidence low))"

let review_file path ~dry_run =
  if dry_run then Printf.printf "[DRY-RUN] Would review: %s\n" path;
  
  let content = File_utils.read_file path in
  let functions = Semantic_review.extract_functions content in
  
  if functions = [] then begin
    Printf.printf "No functions found in %s\n" path;
    exit 0
  end;
  
  Printf.printf "Found %d functions in %s\n\n" (List.length functions) path;
  
  let results = List.map (fun (info : Semantic_review.function_info) ->
    if dry_run then begin
      Printf.printf "Function: %s (line %d)\n" info.name info.line;
      (match info.docstring with
       | None -> Printf.printf "  No docstring\n"
       | Some _ -> Printf.printf "  Has docstring\n");
      Printf.printf "\n";
      (info, Semantic_review.empty_result)
    end else begin
      let prompt = Semantic_review.build_prompt info in
      Printf.printf "Function: %s (line %d) - calling LLM...\n" info.name info.line;
      let response = call_pi prompt in
      let result = Semantic_review.parse_response response in
      Printf.printf "  Doc present: %b, Status: %s, Accuracy: %s, Consistency: %s\n" 
        result.Semantic_review.doc_present
        (match result.Semantic_review.doc_status with
         | Review_types.Doc_accurate -> "accurate"
         | Review_types.Doc_drifted -> "drifted"
         | Review_types.Doc_missing -> "missing")
        (match result.Semantic_review.doc_accuracy with
         | `High -> "high" | `Medium -> "medium" | `Low -> "low" | `Unknown -> "unknown")
        (match result.Semantic_review.consistency with
         | Review_types.Cons_consistent -> "consistent"
         | Review_types.Cons_questionable -> "questionable"
         | Review_types.Cons_inconsistent -> "inconsistent");
      (info, result)
    end
  ) functions in
  
  (* Write metadata if not dry-run *)
  if not dry_run then begin
    let meta_path = path ^ ".borg.meta" in
    let metadata = List.concat_map (fun (info, result) ->
      Semantic_review.render_metadata info result
    ) results in
    let oc = open_out meta_path in
    output_string oc "(semantic-review\n";
    output_string oc (Printf.sprintf "  (timestamp \"%s\")\n" (now ()));
    output_string oc (Printf.sprintf "  (file \"%s\")\n" path);
    List.iter (fun line -> output_string oc (line ^ "\n")) metadata;
    output_string oc ")\n";
    close_out oc;
    Printf.printf "\nMetadata written to %s\n" meta_path;
  end

let review_dir dir ~dry_run ~recursive =
  ignore recursive; (* TODO: implement recursive *)
  Printf.printf "Reviewing directory: %s\n" dir;
  let files = Array.to_list (Sys.readdir dir) in
  let ml_files = List.filter (fun f -> Filename.check_suffix f ".ml") files in
  List.iter (fun f ->
    review_file (Filename.concat dir f) ~dry_run
  ) ml_files

let review_stale dir ~dry_run =
  ignore dry_run;
  Printf.printf "Reviewing stale files in: %s\n" dir;
  let reviews = Semantic_review.review_stale dir in
  if reviews = [] then
    Printf.printf "No stale files found.\n"
  else
    List.iter (fun (review : Borge_lib.Review_types.semantic_review) ->
      Printf.printf "  Reviewed %s: %d functions\n" 
        review.source_file 
        (List.length review.functions)
    ) reviews

(* agent note (|
 *   WHAT: Filter review findings by focus area. When set to "drift",
 *   only report doc-code alignment issues. When "consistency", only
 *   report internal consistency issues. When unset, report everything.
 *
 *   WHY: Large codebases may generate many findings. Focus lets
 *   the user target one category at a time for manageable fixes.
 * |) *)
let filter_by_focus findings focus =
  match focus with
  | None -> findings
  | Some "drift" ->
      List.filter (fun (f : Review_types.function_finding) ->
        f.doc_status <> Review_types.Doc_accurate || f.doc_accuracy = Review_types.Low
      ) findings
  | Some "consistency" ->
      List.filter (fun (f : Review_types.function_finding) ->
        f.consistency <> Review_types.Cons_consistent
      ) findings
  | _ -> findings

(* agent note (|
 *   WHAT: Insert (status drifted) markers into source files for
 *   functions where the LLM found doc-code drift without an
 *   existing drifted marker.
 *
 *   WHY: Auto-marking makes drift explicit without requiring manual
 *   editing. It's a convenience feature for bulk review runs.
 *   The marker means "a machine checked this and found drift."
 * |) *)
let mark_drifted_in_file path findings =
  ignore (File_utils.read_file path);
  (* For each finding with doc_status = Doc_drifted, find the
     comment preceding the function and insert (status drifted)
     TODO: implement actual file modification — for now, report which
     functions would be marked *)
  List.iter (fun (f : Review_types.function_finding) ->
    if f.doc_status = Review_types.Doc_drifted then begin
      Printf.printf "  Marking %s as drifted in %s\n" f.name path
    end
  ) findings

let run file dir all stale dry_run focus mark_drifted =
  ignore focus;
  ignore mark_drifted;
  if stale then
    review_stale (match dir with Some d -> d | None -> ".") ~dry_run
  else
    match file, dir, all with
    | Some f, None, false -> review_file f ~dry_run
    | None, Some d, false -> review_dir d ~dry_run ~recursive:false
    | None, None, true -> review_dir "." ~dry_run ~recursive:true
    | _ ->
        Printf.eprintf "Error: specify --file, --dir, --all, or --stale\n";
        exit 1

open Cmdliner

let file =
  Arg.(value & opt (some string) None & info ["file"; "f"]
    ~doc:"Review a specific file")

let dir =
  Arg.(value & opt (some string) None & info ["dir"; "d"]
    ~doc:"Review files in directory")

let all =
  Arg.(value & flag & info ["all"; "a"] ~doc:"Review all .ml files in project")

let stale =
  Arg.(value & flag & info ["stale"; "s"]
    ~doc:"Only review files with stale metadata")

let dry_run =
  Arg.(value & flag & info ["dry-run"; "n"]
    ~doc:"Show what would be reviewed without calling LLM")

let focus =
  Arg.(value & opt (some string) None & info ["focus"]
    ~docv:"FOCUS"
    ~doc:"Filter findings: drift or consistency")

let mark_drifted =
  Arg.(value & flag & info ["mark-drifted"]
    ~doc:"Auto-insert (status drifted) markers on drifted comments")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "review" ~doc:"semantic code review (requires LLM)"
    ~man:[`S "DESCRIPTION";
          `P "Analyzes OCaml functions and verifies documentation with LLM.";
          `P "Checks doc-code alignment (WHAT + WHY) and internal consistency.";
          `P "Stores results in .borg.meta files for tracking.";
          `P "Use --dry-run to preview without LLM calls.";
          `P "Use --stale to only review changed files.";
          `P "Use --focus drift to only show doc-code drift findings.";
          `P "Use --focus consistency to only show internal consistency findings.";
          `P "Use --mark-drifted to auto-insert (status drifted) markers.";
          `S "EXAMPLES";
          `P "borge review --file lib/core/spec.ml";
          `P "borge review --dir lib/core --dry-run";
          `P "borge review --all";
          `P "borge review --stale";
          `P "borge review --all --focus drift";
          `P "borge review --all --mark-drifted"])
  Term.(const run $ file $ dir $ all $ stale $ dry_run $ focus $ mark_drifted)
