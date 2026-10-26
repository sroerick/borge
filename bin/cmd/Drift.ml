(** borge drift — detect and optionally fix drift

    Static drift analysis. The --agent mode (pi observer/fixer roles,
    .borge.lock snapshot, auto-commit) was removed in Phase D of
    docs/orchestration-deprecation.borg. *)

open Borge_lib

(* agent note (|
 *   WHAT: Run static (non-agentic) drift analysis on a directory.
 *   Checks spec drift, code drift, and optionally module integrity.
 *   Prints results and returns exit code based on findings.
 *
 *   WHY: The fast, non-LLM path for drift detection. Used for
 *   CI/CD and quick checks without agent overhead.
 * |) *)
let run_static dir json quiet modules =
  (* Run classic drift analysis *)
  let _meta = Drift.write_meta_files dir in
  let result = Drift.run dir in

  (* The --modules flag used to run Module_spec integrity checks. That
     command and its module were deleted in Phase C of the orchestration
     deprecation: the check resolved a .borg file's SIBLING .ml files, so
     any spec without adjacent code (including lib.borg, its own headline
     case) folded over an empty list and reported "all exports
     documented". It was a gate that could not fail.

     The flag is kept as a no-op with a warning rather than removed
     outright so an old CI invocation still builds and says why instead
     of dying on an unknown option. Delete the flag itself in Phase D. *)
  let module_issues = [] in
  if modules then
    Printf.eprintf
      "  warning: --modules is a no-op; module integrity checking was removed\n\
      \  (it could not fail — see docs/orchestration-deprecation.borg).\n\
      \  Use borge drift and borge lint --code-doc instead.\n";

  if json then begin
    (* TODO: include module_issues in JSON *)
    if not quiet then Printf.printf "%s\n" (Yojson.Basic.to_string (Json_out.drift result));
    let total = List.length result.spec_drift + List.length result.code_drift +
                List.length result.structural_drift + List.length module_issues in
    exit (if total > 0 then 1 else 0)
  end;

  if quiet then exit 0;

  Printf.printf "Drift report for '%s'\n\n" dir;
  if result.spec_drift = [] then
    Printf.printf "Spec drift: none\n"
  else begin
    Printf.printf "Spec drift (%d sections):\n" (List.length result.spec_drift);
    List.iter (fun (d : Drift.spec_drift) ->
      Printf.printf "  ▷ %s: %s — %s
" d.path d.section_name d.description
    ) result.spec_drift
  end;
  if result.code_drift = [] then
    Printf.printf "Code drift: none\n"
  else begin
    Printf.printf "Code drift (%d items):\n" (List.length result.code_drift);
    List.iter (fun (d : Drift.code_drift_item) ->
      Printf.printf "  ▷ %s: %s (%s)\n" d.path d.name d.kind
    ) result.code_drift
  end;
  if result.structural_drift = [] then
    Printf.printf "Structural drift: none\n"
  else begin
    Printf.printf "Structural drift (%d items):\n" (List.length result.structural_drift);
    List.iter (fun (d : Drift.structural_drift_item) ->
      Printf.printf "  ▷ %s\n" d.description
    ) result.structural_drift
  end;
  let total = List.length result.spec_drift + List.length result.code_drift +
              List.length result.structural_drift + List.length module_issues in
  if module_issues <> [] then begin
    Printf.printf "Module integrity issues (%d):\n" (List.length module_issues);
    List.iter (Printf.printf "  ▷ %s\n") module_issues
  end;
  Printf.printf "\nTotal drift: %d item(s)\n" total;
  if total > 0 then exit 1 else exit 0

(* agent note (|
 *   WHAT: Run documentation coverage analysis using Doc_coverage.
 *   Reports documented/undocumented/exempt counts per file with
 *   optional verbose output showing specific binding names.
 *
 *   WHY: Implements borge drift --docs for literate programming
 *   enforcement. Exit codes indicate coverage thresholds.
 * |) *)
(* agent note (|
 *   WHAT: Read the doc-coverage-threshold from the borge.borg
 *   convention stanza. Returns the default 0.8 if not found.
 *
 *   WHY: Coverage thresholds are configurable per project, so teams
 *   can set their own standards. Reading from the spec file keeps
 *   the threshold consistent across all borge commands.
 * |) *)
let read_coverage_threshold dir =
  let borg_files = File_utils.find_borg_files dir in
  let rec try_files = function
    | [] -> 0.8  (* default *)
    | path :: rest ->
        try
          let input = File_utils.read_file path in
          (* Look for (convention ocaml-dune (doc-coverage-threshold N)) *)
          let re = Str.regexp "doc-coverage-threshold[ \t]+\\([0-9.]+\\)" in
          if Str.string_match re input 0 then
            let v = Str.matched_group 1 input in
            (match float_of_string_opt v with Some f -> f | None -> try_files rest)
          else
            try_files rest
        with _ -> try_files rest
  in
  try_files borg_files

let run_docs dir ~strict ~verbose =
  Printf.printf "Documentation coverage analysis for '%s'\n\n" dir;
  let coverage = Doc_coverage.calculate_dir_coverage dir in
  Printf.printf "%s\n" (Doc_coverage.format_project_summary coverage);
  if verbose then begin
    List.iter (fun (fc : Doc_coverage.file_coverage) ->
      if fc.undocumented_names <> [] then begin
        Printf.printf "\nUndocumented in %s:\n" fc.path;
        List.iter (fun name ->
          Printf.printf "  - %s\n" name
        ) fc.undocumented_names
      end;
      if fc.drifted_names <> [] then begin
        Printf.printf "\nDrifted in %s:\n" fc.path;
        List.iter (fun name ->
          Printf.printf "  ~ %s (status drifted)\n" name
        ) fc.drifted_names
      end
    ) coverage.files
  end;
  let threshold = read_coverage_threshold dir in
  (* Exit code logic per drift-doc-integration spec *)
  let has_error = List.exists (fun (fc : Doc_coverage.file_coverage) ->
    fc.coverage_percent < 50.0
  ) coverage.files in
  let has_warning = List.exists (fun (fc : Doc_coverage.file_coverage) ->
    fc.coverage_percent >= 50.0 && fc.coverage_percent < (threshold *. 100.0)
  ) coverage.files in
  if has_error then exit 1
  else if strict && has_warning then exit 2
  else exit 0

let run dir json quiet modules docs strict verbose =
  if docs then run_docs dir ~strict ~verbose
  else run_static dir json quiet modules

open Cmdliner

(* exempt doc: cmdliner argument definitions - purpose is self-evident *)
let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory to check for drift")

(* exempt doc: cmdliner argument definition - purpose is self-evident *)
let agent =
  Arg.(value & flag & info ["agent"]
    ~doc:"Open pi interatively for agent-powered drift analysis")

(* exempt doc: cmdliner argument definition - purpose is self-evident *)
let fix =
  Arg.(value & flag & info ["fix"]
    ~doc:"Allow agent to write code repairs (requires --agent)")

(* exempt doc: cmdliner argument definition - purpose is self-evident *)
let no_commit =
  Arg.(value & flag & info ["no-commit"]
    ~doc:"Leave changes in working tree; do not auto-commit")

(* exempt doc: cmdliner argument definition - purpose is self-evident *)
let json =
  Arg.(value & flag & info ["json"] ~doc:"Output as JSON")

(* exempt doc: cmdliner argument definition - purpose is self-evident *)
let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

(* exempt doc: cmdliner argument definition - purpose is self-evident *)
let modules =
  Arg.(value & flag & info ["modules"; "m"] ~doc:"Include module integrity checks")

(* exempt doc: cmdliner argument definition - purpose is self-evident *)
let docs =
  Arg.(value & flag & info ["docs"; "d"] ~doc:"Analyze documentation coverage")

(* exempt doc: cmdliner argument definition - purpose is self-evident *)
let strict =
  Arg.(value & flag & info ["strict"] ~doc:"Treat warnings as errors in docs mode")

(* exempt doc: cmdliner argument definition - purpose is self-evident *)
let verbose =
  Arg.(value & flag & info ["verbose"; "v"] ~doc:"Show per-binding undocumented/drifted names in docs mode")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "drift" ~doc:"detect spec, code, and structural drift"
    ~man:[`S "DESCRIPTION";
          `P "Checks for three layers of drift:";
          `P "1. Spec drift: .borg describes something not in the code";
          `P "2. Code drift: code has something not in any .borg spec";
          `P "3. Structural drift: code organization violates the convention";
          `S "DOCUMENTATION MODE";
          `P "With --docs, analyzes documentation coverage of .ml files.";
          `P "Reports which bindings have/don't have documentation.";
          `P "With --docs --strict, warnings also cause non-zero exit.";
          `P "With --docs --verbose, shows per-binding undocumented/drifted names.";
          `P "Auto-commits on clean exit (use --no-commit to review first)."])
  Term.(const run $ dir $ json $ quiet $ modules $ docs $ strict $ verbose)
