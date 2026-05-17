open Borge_lang.Balance
open Borge_lib
open Cmdliner

let run path verbose json =
  let ic = open_in path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  let text = Bytes.to_string buf in
  let result = check text in
  if json then begin
    Printf.printf "%s\n" (Yojson.Basic.to_string (Json_out.balance path result));
    exit (match result with Balanced _ -> 0 | Imbalanced _ -> 1)
  end;
  let ok = report_file ~verbose path in
  if ok then exit 0 else exit 1

let path =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE"
    ~doc:"The .borg file to check for balance")

let verbose =
  Arg.(value & flag & info ["v"; "verbose"] ~doc:"Show paren stack on error")

let json =
  Arg.(value & flag & info ["json"] ~doc:"Output as JSON")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "balance" ~doc:"check structural balance of a .borg file"
    ~man:[`S "DESCRIPTION";
          `P "Fast balance check for .borg files. Scans characters without \
              fully parsing. Understands comments, strings, and verbatim blocks. \
              Provides line-accurate errors with excerpts and suggestions.";
          `P "With --verbose, shows the full paren stack on unclosed-paren \
              errors, listing each open paren with its position and keyword.";
          `P "With --json, outputs structured JSON instead of formatted text."])
  Term.(const run $ path $ verbose $ json)

