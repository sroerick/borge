(** borge drift — detect and optionally fix drift

    Without flags: static drift analysis
    With --agent: opens pi with observer role (read-only checks)
    With --agent --fix: opens pi with fixer role (can write repairs) *)

open Borge_lib

(* agent note (|
 *   WHAT: Gather spec files and module surface data needed for
 *   agent-driven drift analysis prompt. Finds all .borg files and
 *   extracts module exports from dune files.
 *
 *   WHY: The agent needs context about what specs exist and what
 *   modules are available to check for drift.
 * |) *)
let gather_prompt_data () =
  let spec_files =
    File_utils.find_borg_files "."
    |> List.map (fun path -> (path, File_utils.read_file path))
  in
  let convention = Convention.resolve "." in
  let module_surfaces =
    match convention with
    | Convention.Go_standard ->
      (try
        let project = Go_parse.parse_all "." in
        let lib_packages = Go_parse.library_packages project in
        List.filter_map (fun (pkg : Go_parse.go_package) ->
          let surface = Go_surface.extract_package_surface pkg.Go_parse.dir_path in
          Some (pkg.Go_parse.package_name, Go_surface.exported_names surface)
        ) lib_packages
      with _ -> [])
    | Convention.Ocaml_dune ->
      (try
        let dune_files = Dune_parse.parse_all "." in
        let lib_modules = List.concat_map (fun df ->
          let dir = Filename.dirname df.Dune_parse.path in
          List.concat_map (function
            | Dune_parse.Library lib ->
                List.map (fun m -> (dir, m)) lib.Dune_parse.modules
            | _ -> []
          ) df.Dune_parse.stanzas
        ) dune_files in
        List.filter_map (fun (dir, mod_name) ->
          let ml_path = Filename.concat dir (mod_name ^ ".ml") in
          if Sys.file_exists ml_path then
            try
              let surface = Surface.extract_surface ml_path in
              Some (mod_name, surface.Surface.exports)
            with _ -> Some (mod_name, [])
          else None
        ) lib_modules
      with _ -> [])
  in
  (spec_files, module_surfaces)

(* agent note (|
 *   WHAT: Write the agent prompt to a temporary file in .borge.lock/.
 *   Combines spec files and module surfaces into a prompt for the
 *   observer or fixer agent role.
 *
 *   WHY: The agent needs its instructions written to disk before
 *   pi can execute them.
 * |) *)
let write_prompt role =
  let spec_files, module_surfaces = gather_prompt_data () in
  let convention_name = Convention.name (Convention.resolve ".") in
  let prompt = Lock_prompt.make_prompt role ~convention:convention_name ~spec_files ~module_surfaces in
  let path = Lock.prompt_file () in
  let oc = open_out path in
  output_string oc prompt;
  close_out oc;
  path

(* agent note (|
 *   WHAT: Run the interactive agent mode for drift analysis.
 *   Creates a lock, writes prompt, launches pi session, and handles
 *   teardown (commit or abort) based on success/failure.
 *
 *   WHY: This is the agent-driven alternative to static drift checking.
 *   Allows the LLM to understand spec intent and find semantic drift.
 * |) *)
let run_agent ~fix ~no_commit ~dir ~quiet =
  (* Agent mode is interactive; quiet flag is only relevant for final output *)
  ignore quiet;
  ignore dir;
  let role = if fix then Lock_prompt.Fixer else Lock_prompt.Observer in
  let role_name = if fix then "fixer" else "observer" in
  
  (match Lock.create () with
   | Error msg ->
     Printf.eprintf "Error: %s\n" msg;
     exit 1
   | Ok () -> ());

  let prompt_path = write_prompt role in
  Printf.printf "Lock created at .borge.lock/\n";
  Printf.printf "Running drift analysis with role: %s\n" role_name;
  Printf.printf "Prompt written to %s\n" prompt_path;

  let exit_status = Lock_pi_launch.launch prompt_path in

  if exit_status = 0 then begin
    Printf.printf "\nAgent session completed (exit 0).\n";
    if no_commit then begin
      Printf.printf "--no-commit: changes left in exploring.\n";
      Printf.printf "Run 'borge commit' to finalize or 'borge abort' to discard.\n";
      Lock.set_state "completed";
      exit 0
    end else begin
      match Lock.teardown ~commit:true with
      | Ok msg ->
        Printf.printf "%s\n" msg;
        Lock.remove ();
        Printf.printf "Committed and lock removed.\n";
        exit 0
      | Error msg ->
        Printf.eprintf "Commit failed: %s\n" msg;
        Printf.eprintf "Run 'borge commit --force' to override or 'borge abort' to discard.\n";
        Lock.set_state "failed";
        exit 1
    end
  end else begin
    Printf.eprintf "\nAgent session failed (exit %d).\n" exit_status;
    Printf.eprintf "Lock kept at .borge.lock/ for review.\n";
    Lock.set_state "failed";
    exit exit_status
  end

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

  (* Optionally run module integrity check *)
  let module_issues = if modules then begin
    let specs = Module_spec.load_all_specs dir in
    List.concat_map (fun (spec : Module_spec.module_def) ->
      let impl_files = Module_spec.find_implementation_files spec.path in
      List.concat_map (fun impl_path ->
        let (missing, undocumented) = Module_spec.check_integrity spec impl_path in
        List.map (fun name ->
          Printf.sprintf "module %s: missing implementation of %s" spec.module_name name
        ) missing @
        List.map (fun name ->
          Printf.sprintf "module %s: undocumented function %s" spec.module_name name
        ) undocumented
      ) impl_files
    ) specs
  end else [] in

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

let run dir agent fix no_commit json quiet modules docs strict verbose =
  if docs then run_docs dir ~strict ~verbose
  else if agent then run_agent ~fix ~no_commit ~dir ~quiet
  else if fix then begin
    Printf.eprintf "Error: --fix requires --agent\n";
    exit 1
  end
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
          `S "AGENT MODE";
          `P "With --agent, opens pi interactively with observer role.";
          `P "The agent checks whether code matches spec and reports findings.";
          `P "With --agent --fix, the agent can write code repairs.";
          `P "Auto-commits on clean exit (use --no-commit to review first)."])
  Term.(const run $ dir $ agent $ fix $ no_commit $ json $ quiet $ modules $ docs $ strict $ verbose)
