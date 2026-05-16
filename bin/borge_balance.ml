open Borge_sexp.Balance
open Cmdliner

let run path verbose =
  let ok = report_file ~verbose path in
  if ok then exit 0 else exit 1

let path =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE"
    ~doc:"The .borg file to check for balance")

let verbose =
  Arg.(value & flag & info ["v"; "verbose"] ~doc:"Show paren stack on error")

let cmd =
  Cmd.v (Cmd.info "balance" ~doc:"check structural balance of a .borg file"
    ~man:[`S "DESCRIPTION";
          `P "Fast balance check for .borg files. Scans characters without \
              fully parsing. Understands comments, strings, and verbatim blocks. \
              Provides line-accurate errors with excerpts and suggestions.";
          `P "With --verbose, shows the full paren stack on unclosed-paren \
              errors, listing each open paren with its position and keyword."])
  Term.(const run $ path $ verbose)

let () = ignore (Cmd.eval cmd : int)
