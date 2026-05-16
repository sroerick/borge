
(** borge undo: git revert HEAD — convenience command *)

let run dir =
  Printf.printf "This will revert the last commit (git revert HEAD).\n";
  Printf.printf "The current state is preserved because revert creates a new commit.\n\n";
  (* Show what we're reverting *)
  let cmd = Printf.sprintf "git -C %s log -1 --oneline" (Filename.quote dir) in
  let ic = Unix.open_process_in cmd in
  (try
    let line = input_line ic in
    Printf.printf "Last commit: %s\n\n" line
  with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  let cmd = Printf.sprintf "git -C %s revert --no-edit HEAD 2>&1" (Filename.quote dir) in
  let ret = Sys.command cmd in
  if ret = 0 then begin
    Printf.printf "Reverted successfully.\n";
    exit 0
  end else begin
    Printf.eprintf "Revert failed. You may need to resolve conflicts manually.\n";
    exit 1
  end

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Path to git repository (default: current directory)")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "undo" ~doc:"revert the last commit (git revert HEAD)"
    ~man:[`S "DESCRIPTION";
          `P "Convenience command that runs git revert HEAD. This creates \
              a new commit that undoes the last one — the original commit \
              is preserved in history, so nothing is lost."])
  Term.(const run $ dir)

