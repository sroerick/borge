open Cmdliner
let () = Borge_cmd.Lint.cmd |> Cmd.eval |> exit
