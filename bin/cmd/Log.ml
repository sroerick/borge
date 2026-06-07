
(** borge log: git log with borge-aware summaries *)

let run dir quiet =
  let cmd = Printf.sprintf
    "git -C %s log --oneline --no-decorate -20 -- *.borg 2>/dev/null"
    (Filename.quote dir) in
  let ic = Unix.open_process_in cmd in
  let lines = ref [] in
  (* exempt: input_line *) (try while true do lines := input_line ic :: !lines done with End_of_file -> ());
  let _status = Unix.close_process_in ic in
  match List.rev !lines with
  | [] ->
      if not quiet then Printf.printf "No borge commit history found.\n";
      exit 0
  | commits ->
      if quiet then exit 0;
      Printf.printf "Borge commit history (last %d):\n\n" (List.length commits);
      List.iter (fun line ->
        Printf.printf "  %s\n" line
      ) commits;
      exit 0

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Path to git repository (default: current directory)")

let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "log" ~doc:"git log showing only borge-related commits"
    ~man:[`S "DESCRIPTION";
          `P "Shows git log filtered to commits that modified .borg files. \
              This gives a borge-aware view of project history."])
  Term.(const run $ dir $ quiet)

