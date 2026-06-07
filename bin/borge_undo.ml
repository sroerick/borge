open Cmdliner
let () = Borge_cmd.Undo.cmd |> Cmd.eval |> exit
