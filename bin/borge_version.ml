open Cmdliner
let () = Borge_cmd.Version.cmd |> Cmd.eval |> exit
