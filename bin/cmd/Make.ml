(** borge make — implement code from spec

    Opens pi interactively with the implementer role.
    Creates lock, builds prompt, launches pi.
    Auto-commits on clean exit unless --no-commit. *)

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

let run no_commit dir quiet =
  ignore dir;  (* TODO: support non-root dirs *)
  (match Lock.create () with
   | Error msg ->
     Printf.eprintf "Error: %s\n" msg;
     exit 1
   | Ok () -> ());

  let prompt_path = write_prompt Lock_prompt.Implementer in
  if not quiet then begin
    Printf.printf "Lock created at .borge.lock/\n";
    Printf.printf "Prompt written to %s\n" prompt_path;
  end;

  let exit_status = Lock_pi_launch.launch prompt_path in

  if exit_status = 0 then begin
    if not quiet then Printf.printf "\nAgent session completed (exit 0).\n";
    if no_commit then begin
      if not quiet then begin
        Printf.printf "--no-commit: changes left in working tree.\n";
        Printf.printf "Run 'borge commit' to finalize or 'borge abort' to discard.\n";
      end;
      Lock.set_state "completed";
      exit 0
    end else begin
      match Lock.teardown ~commit:true with
      | Ok msg ->
        if not quiet then Printf.printf "%s\n" msg;
        Lock.remove ();
        if not quiet then Printf.printf "Committed and lock removed.\n";
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

open Cmdliner

let no_commit =
  Arg.(value & flag & info ["no-commit"]
    ~doc:"Leave changes in working tree; do not auto-commit")

let dir =
  Arg.(value & opt dir "." & info ["dir"; "d"] ~docv:"DIR"
    ~doc:"Project directory")

let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "make" ~doc:"make code match spec [launches pi]"
    ~man:[`S "DESCRIPTION";
          `P "Opens an interactive pi session with role: implementer.";
          `P "The agent reads the full spec and writes code + tests.";
          `P "Auto-commits on clean exit (use --no-commit to keep lock).";
          `P "See also: borge plan (edit spec), borge drift (check alignment)."])
  Term.(const run $ no_commit $ dir $ quiet)
