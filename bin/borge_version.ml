let version = "0.1.0"

let run () =
  Printf.printf "borge %s\n" version;
  exit 0

open Cmdliner

let cmd =
  Cmd.v (Cmd.info "version" ~doc:"print the current version"
    ~man:[`S "DESCRIPTION"; `P "Prints the borge version number."])
  Term.(const run $ const ())

let () = ignore (Cmd.eval cmd : int)
