(** borge review — semantic code review

    Uses LLM to verify function documentation matches implementation *)

open Borge_lib

let now () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

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
      let _prompt = Semantic_review.build_prompt info in
      (* TODO: Call LLM with prompt *)
      Printf.printf "Function: %s (line %d) - LLM call pending\n" info.name info.line;
      (info, Semantic_review.empty_result)
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

let run file dir all dry_run =
  match file, dir, all with
  | Some f, None, false -> review_file f ~dry_run
  | None, Some d, false -> review_dir d ~dry_run ~recursive:false
  | None, None, true -> review_dir "." ~dry_run ~recursive:true
  | _ ->
      Printf.eprintf "Error: specify --file, --dir, or --all\n";
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

let dry_run =
  Arg.(value & flag & info ["dry-run"; "n"]
    ~doc:"Show what would be reviewed without calling LLM")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "review" ~doc:"semantic code review (requires LLM)"
    ~man:[`S "DESCRIPTION";
          `P "Analyzes OCaml functions and verifies documentation with LLM.";
          `P "Stores results in .borg.meta files for tracking.";
          `P "Use --dry-run to preview without LLM calls.";
          `S "EXAMPLES";
          `P "borge review --file lib/core/spec.ml";
          `P "borge review --dir lib/core --dry-run";
          `P "borge review --all"])
  Term.(const run $ file $ dir $ all $ dry_run)
