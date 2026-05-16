open Borge_lib

let run_spec todo dir =
  match Generate.generate_spec todo dir with
  | Some sexp ->
    Printf.printf "%s\n" sexp;
    exit 0
  | None ->
    Printf.eprintf "Failed to generate spec.\n";
    exit 1

let run_code section dir =
  match Generate.generate_code section dir with
  | Some _ ->
    exit 0
  | None ->
    Printf.eprintf "Failed to generate code for section '%s'.\n" section;
    exit 1

open Cmdliner

let spec_cmd : unit Cmd.t =
  let todo =
    Arg.(value & pos 0 string "" & info [] ~docv:"TODO"
      ~doc:"Todo description to turn into a .borg section")
  in
  let dir =
    Arg.(value & opt dir "." & info ["dir"; "d"] ~docv:"DIR"
      ~doc:"Project directory")
  in
  Cmd.v (Cmd.info "spec" ~doc:"generate a .borg section from a todo description"
    ~man:[`S "DESCRIPTION";
          `P "Takes a free-form todo description and uses an LLM to produce \
              a structured .borg section with (status planned)."])
  Term.(const run_spec $ todo $ dir)

let code_cmd : unit Cmd.t =
  let section =
    Arg.(value & pos 0 string "" & info [] ~docv:"SECTION"
      ~doc:"Section name to implement")
  in
  let dir =
    Arg.(value & opt dir "." & info ["dir"; "d"] ~docv:"DIR"
      ~doc:"Project directory")
  in
  Cmd.v (Cmd.info "code" ~doc:"generate implementation code from a planned section"
    ~man:[`S "DESCRIPTION";
          `P "Reads the spec for a planned section and uses an LLM to \
              generate the implementation code. The section should be \
              marked (status planned) in a .borg file."])
  Term.(const run_code $ section $ dir)

let cmd : unit Cmd.t =
  let info = Cmd.info "generate" ~doc:"generate spec sections or code from specs"
    ~man:[`S "DESCRIPTION";
          `P "Two-step pipeline: generate spec (todo -> .borg section), \
              then generate code (spec -> implementation).";
          `S "SUBCOMMANDS";
          `I ("spec", "Turn a todo into a structured .borg section");
          `I ("code", "Implement a planned section from its spec")]
  in
  Cmd.group info [spec_cmd; code_cmd]
