open Borge_lib

let run path quiet =
  let input = File_utils.read_file path in
  let file = Borge_lang.Parse.parse_file input in
  let output = Borge_lang.Print.print_file file in
  if not quiet then print_string output;
  exit 0

open Cmdliner

let path =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE"
    ~doc:"The .borg file to print")

let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "print" ~doc:"parse and re-print a .borg file"
    ~man:[`S "DESCRIPTION";
          `P "Parses the .borg file and re-prints it with canonical formatting. \
              Useful for verifying parse round-trips."])
  Term.(const run $ path $ quiet)

