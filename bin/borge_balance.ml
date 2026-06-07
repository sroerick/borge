open Cmdliner
let () = Borge_cmd.Balance.cmd |> Cmd.eval |> exit
