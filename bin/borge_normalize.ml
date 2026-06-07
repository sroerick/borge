open Cmdliner
let () = Borge_cmd.Normalize.cmd |> Cmd.eval |> exit
