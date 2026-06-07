open Cmdliner
let () = Borge_cmd.Parse.cmd |> Cmd.eval |> exit
