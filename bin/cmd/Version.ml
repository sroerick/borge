(* exempt doc *)
let version = "0.1.0"

let run quiet =
  if not quiet then Printf.printf "borge %s\n" version;
  exit 0

open Cmdliner

(* exempt doc *)
let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

(* exempt doc *)
let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "version" ~doc:"print the current version"
    ~man:[`S "DESCRIPTION"; `P "Prints the borge version number."])
  Term.(const run $ quiet)