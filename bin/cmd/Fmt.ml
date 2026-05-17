open Borge_lib

let show_diff original formatted =
  let orig_lines = String.split_on_char '\n' original in
  let fmt_lines = String.split_on_char '\n' formatted in
  Printf.printf "--- original\n+++ formatted\n";
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
  show 1 (orig_lines, fmt_lines)

let run path check diff output quiet =
  if check then
    match Fmt.check_file path with
    | Fmt.CheckClean ->
        if not quiet then Printf.printf "%s: already formatted\n" (Filename.basename path);
        exit 0
    | Fmt.CheckDirty formatted ->
        if not quiet then Printf.printf "%s: needs formatting\n" (Filename.basename path);
        (match output with
         | Some dst ->
             let oc = open_out dst in
             output_string oc formatted;
             close_out oc
         | None -> print_endline formatted);
        exit 1
    | Fmt.Formatted _ -> (* shouldn't happen in check mode *) exit 1
  else if diff then begin
    let original = File_utils.read_file path in
    match Fmt.format_file path with
    | Fmt.Formatted formatted ->
        if not quiet then show_diff original formatted;
        if Fmt.strip_trailing_newlines original = Fmt.strip_trailing_newlines formatted
        then exit 0 else exit 1
    | _ -> exit 1
  end else
    match Fmt.format_file path with
    | Fmt.Formatted txt ->
        (match output with
         | Some dst ->
             let oc = open_out dst in
             output_string oc txt;
             close_out oc;
             if not quiet then Printf.printf "%s\n" (Filename.basename dst)
         | None -> if not quiet then print_endline txt else ());
        exit 0
    | _ -> (* check results in format mode shouldn't happen *) exit 1

open Cmdliner

let path =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE"
    ~doc:"The .borg file to format")

let output =
  Arg.(value & opt (some file) None & info ["o"] ~docv:"FILE"
    ~doc:"Write output to FILE (default: stdout)")

let check =
  Arg.(value & flag & info ["check"] ~doc:"Exit 1 if formatting would change the file")

let diff =
  Arg.(value & flag & info ["diff"] ~doc:"Show diff between current and formatted output")

let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "fmt" ~doc:"auto-format a .borg file to canonical indentation"
    ~man:[`S "DESCRIPTION";
          `P "Parses the .borg file and re-prints it with canonical indentation.";
          `P "Use --check for CI (exit 1 if formatting would change). \
              Use --diff to see what would change."])
  Term.(const run $ path $ check $ diff $ output $ quiet)

