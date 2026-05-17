open Borge_lib

let run section dir quiet =
  ignore quiet;
  if section = "" then begin
    Printf.eprintf "Error: SECTION argument is required.\n";
    exit 1
  end;
  match Generate.generate_code section dir with
  | Some _ ->
    exit 0
  | None ->
    Printf.eprintf "Failed to execute section '%s'.\n" section;
    exit 1

open Cmdliner

let section =
  Arg.(value & pos 0 string "" & info [] ~docv:"SECTION"
    ~doc:"Section name to execute (implement)")

let dir =
  Arg.(value & opt dir "." & info ["dir"; "d"] ~docv:"DIR"
    ~doc:"Project directory")

let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "execute" ~doc:"execute (implement) a planned section"
    ~man:[`S "DESCRIPTION";
          `P "Alias for 'borge generate code'. Reads the spec for a planned \
              section and invokes an LLM to produce the implementation. \
              Planned sections are unimplemented tasks — 'execute' means \
              'implement this task'."])
  Term.(const run $ section $ dir $ quiet)
