open Cmdliner
let () = Borge_cmd.Check.cmd |> Cmd.eval |> exit
