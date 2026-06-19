open Borge_lang.Balance
open Borge_lib
open Cmdliner

let run path verbose json quiet do_repair do_repair_diff =
  let text = File_utils.read_file path in
  let result = check text in
  let imbalanced = match result with Balanced _ -> false | Imbalanced _ -> true in

  if do_repair || do_repair_diff then begin
    let repaired = Borge_lang.Balance.repair text in
    if do_repair_diff then begin
      let original_lines = String.split_on_char '\n' text in
      let repaired_lines = String.split_on_char '\n' repaired in
      Printf.printf "--- %s\n+++ %s (repaired)\n" path path;
      let rec show line_num = function
        | [], [] -> ()
        | o :: os, [] ->
            Printf.printf "-%3d | %s\n" line_num o;
            show (line_num + 1) (os, [])
        | [], f :: fs ->
            Printf.printf "+%3d | %s\n" line_num f;
            show (line_num + 1) ([], fs)
        | o :: os, f :: fs ->
            if o = f then
              Printf.printf " %3d | %s\n" line_num o
            else begin
              Printf.printf "-%3d | %s\n" line_num o;
              Printf.printf "+%3d | %s\n" line_num f
            end;
            show (line_num + 1) (os, fs)
      in
      show 1 (original_lines, repaired_lines);
      exit 0
    end else begin
      print_string repaired;
      exit 0
    end
  end;

  if json then begin
    let ea = analyze_enriched text in
    Printf.printf "%s\n" (Yojson.Basic.to_string (Json_out.balance_enriched path ea));
    exit (if imbalanced then 1 else 0)
  end;
  if quiet then exit (if imbalanced then 1 else 0);
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

let repair_flag =
  Arg.(value & flag & info ["repair"] ~doc:"Repair imbalanced parens using indentation inference")

let repair_diff_flag =
  Arg.(value & flag & info ["repair-diff"] ~doc:"Show diff of proposed repairs instead of applying")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "balance" ~doc:"check structural balance of a .borg file"
    ~man:[`S "DESCRIPTION";
          `P "Fast balance check for .borg files. Scans characters without \
              fully parsing. Understands comments, strings, and verbatim blocks. \
              Provides line-accurate errors with excerpts and suggestions.";
          `P "With --verbose, shows the full paren stack on unclosed-paren \
              errors, listing each open paren with its position and keyword.";
          `P "With --json, outputs structured JSON with structural_tree, \
              indent_divergences, and repair_actions.";
          `P "With --repair, outputs a corrected version of the file by \
              inferring parens from indentation (Parinfer-style).";
          `P "With --repair-diff, shows what --repair would change."])
  Term.(const run $ path $ verbose $ json $ quiet $ repair_flag $ repair_diff_flag)

