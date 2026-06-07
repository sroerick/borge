open Cmdliner
let () = Borge_cmd.Nodes.cmd |> Cmd.eval |> exit
