open Borge_lang.Balance
open Borge_lib
open Cmdliner

let run path verbose json quiet =
  let text = File_utils.read_file path in
  let result = check text in
  if json then begin
    Printf.printf "%s\n" (Yojson.Basic.to_string (Json_out.balance path result));
    exit (match result with Balanced _ -> 0 | Imbalanced _ -> 1)
  end;
  if quiet then exit (match result with Balanced _ -> 0 | Imbalanced _ -> 1);
  let ok = Borge_lang.Balance.report_file ~verbose path in
  if ok then exit 0 else exit 1

let path =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE"
    ~doc:"The .borg file to check for balance")

let verbose =
  Arg.(value & flag & info ["v"; "verbose"] ~doc:"Show paren stack on error")

let json =
  Arg.(value & flag & info ["json"] ~doc:"Output as JSON")

let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "balance" ~doc:"check structural balance of a .borg file"
    ~man:[`S "DESCRIPTION";
          `P "Fast balance check for .borg files. Scans characters without \
              fully parsing. Understands comments, strings, and verbatim blocks. \
              Provides line-accurate errors with excerpts and suggestions.";
          `P "With --verbose, shows the full paren stack on unclosed-paren \
              errors, listing each open paren with its position and keyword.";
          `P "With --json, outputs structured JSON instead of formatted text."])
  Term.(const run $ path $ verbose $ json $ quiet)

