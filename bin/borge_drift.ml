open Cmdliner
let () = Borge_cmd.Drift.cmd |> Cmd.eval |> exit
