open Borge_sexp.Balance
open Cmdliner

let run path =
  let ok = report_file path in
  if ok then exit 0 else exit 1

let path =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE"
    ~doc:"The .borg file to check for balance")

let cmd =
  Cmd.v (Cmd.info "balance" ~doc:"check structural balance of a .borg file"
    ~man:[`S "DESCRIPTION";
          `P "Fast balance check for .borg files. Scans characters without \
              fully parsing. Understands comments, strings, and verbatim blocks. \
              Provides line-accurate errors with excerpts and suggestions."])
  Term.(const run $ path)

let () = ignore (Cmd.eval cmd : int)
