open Borge_lib

let run path quiet =
  let result = Nodes.run path in
  if not quiet then begin
    let output = Nodes.format_result result in
    print_string output;
  end;
  exit 0

open Cmdliner

let path =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE"
    ~doc:"The .borg file to analyze")

let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "nodes" ~doc:"dump the parsed AST with types and positions"
    ~man:[`S "DESCRIPTION";
          `P "Parses the .borg file and prints each AST node with its type, \
              keyword (if list), source position, and child count."])
  Term.(const run $ path $ quiet)

