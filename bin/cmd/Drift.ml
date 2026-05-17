(** borge drift — detect and optionally fix drift

    Without flags: static drift analysis
    With --agent: opens pi with observer role (read-only checks)
    With --agent --fix: opens pi with fixer role (can write repairs) *)

open Borge_lib

let gather_prompt_data () =
  let spec_files =
    File_utils.find_borg_files "."
    |> List.map (fun path -> (path, File_utils.read_file path))
  in
  let module_surfaces =
    try
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
    with _ -> []
  in
  (spec_files, module_surfaces)

let write_prompt role =
  let spec_files, module_surfaces = gather_prompt_data () in
  let prompt = Lock_prompt.make_prompt role ~convention:"ocaml-dune" ~spec_files ~module_surfaces in
  let path = Lock.prompt_file () in
  let oc = open_out path in
  output_string oc prompt;
  close_out oc;
  path

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
      Printf.printf "  ▷ %s: '%s' marked implemented but not found in code\n"
        d.path d.section_name
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

let run dir agent fix no_commit json quiet modules =
  if agent then run_agent ~fix ~no_commit ~dir ~quiet
  else if fix then begin
    Printf.eprintf "Error: --fix requires --agent\n";
    exit 1
  end
  else run_static dir json quiet modules

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory to check for drift")

let agent =
  Arg.(value & flag & info ["agent"]
    ~doc:"Open pi interatively for agent-powered drift analysis")

let fix =
  Arg.(value & flag & info ["fix"]
    ~doc:"Allow agent to write code repairs (requires --agent)")

let no_commit =
  Arg.(value & flag & info ["no-commit"]
    ~doc:"Leave changes in working tree; do not auto-commit")

let json =
  Arg.(value & flag & info ["json"] ~doc:"Output as JSON")

let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

let modules =
  Arg.(value & flag & info ["modules"; "m"] ~doc:"Include module integrity checks")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "drift" ~doc:"detect spec, code, and structural drift"
    ~man:[`S "DESCRIPTION";
          `P "Checks for three layers of drift:";
          `P "1. Spec drift: .borg describes something not in the code";
          `P "2. Code drift: code has something not in any .borg spec";
          `P "3. Structural drift: code organization violates the convention";
          `S "AGENT MODE";
          `P "With --agent, opens pi interactively with observer role.";
          `P "The agent checks whether code matches spec and reports findings.";
          `P "With --agent --fix, the agent can write code repairs.";
          `P "Auto-commits on clean exit (use --no-commit to review first)."])
  Term.(const run $ dir $ agent $ fix $ no_commit $ json $ quiet $ modules)
