
(** borge diff: git diff for .borg files only *)

let run dir quiet =
  let cmd = Printf.sprintf
    "git -C %s diff -- *.borg 2>/dev/null"
    (Filename.quote dir) in
  let ic = Unix.open_process_in cmd in
  let lines = ref [] in
  (try while true do lines := input_line ic :: !lines done with End_of_file -> ());
  let _status = Unix.close_process_in ic in
  match List.rev !lines with
  | [] ->
      if not quiet then Printf.printf "No uncommitted changes to .borg files.\n";
      exit 0
  | diff_lines ->
      if quiet then exit 1;
      Printf.printf "Uncommitted changes to .borg files:\n\n";
      List.iter (fun line ->
        if String.length line > 0 then begin
          match line.[0] with
          | '+' when String.length line > 1 && line.[1] <> '+' ->
              if String.length line > 10 && String.sub line 1 10 = "(status " then
                Printf.printf "  ▷ %s  [status change]\n" line
              else
                Printf.printf "  %s\n" line
          | '-' when String.length line > 1 && line.[1] <> '-' ->
              if String.length line > 10 && String.sub line 1 10 = "(status " then
                Printf.printf "  ▷ %s  [status change]\n" line
              else
                Printf.printf "  %s\n" line
          | _ ->
              Printf.printf "  %s\n" line
        end
      ) diff_lines;
      exit 1

open Cmdliner

(* exempt doc *)
let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Path to git repository (default: current directory)")

(* exempt doc *)
let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

(* exempt doc *)
let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "diff" ~doc:"git diff for .borg files with status annotations"
    ~man:[`S "DESCRIPTION";
          `P "Shows git diff filtered to .borg files. Highlights status \
              transitions and section changes for easy review."])
  Term.(const run $ dir $ quiet)

