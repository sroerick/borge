(** borge abort — discard current lock and restore .borg files

    Restores .borg files from .borge.lock/original/.
    Removes the lock directory.
    Code file changes are NOT reverted — use git to undo those. *)

let run quiet =
  if not (Borge_lib.Lock.is_locked ()) then begin
    Printf.eprintf "No active lock. Nothing to abort.\n";
    exit 1
  end;

  let state = Borge_lib.Lock.get_state () in
  if not quiet then begin
    Printf.printf "Lock status: %s\n" state;
    Printf.printf "Restoring .borg files from .borge.lock/original/...\n";
  end;

  Borge_lib.Lock.restore ();

  if not quiet then Printf.printf "Removing lock...\n";
  Borge_lib.Lock.remove ();

  if not quiet then begin
    Printf.printf "Done. .borg files restored.\n";
    Printf.printf "Code file changes remain — use git to undo if needed.\n";
  end;
  exit 0

open Cmdliner

(* exempt doc *)
let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

(* exempt doc *)
let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "abort" ~doc:"abort the current agent session and restore .borg files"
    ~man:[`S "DESCRIPTION";
          `P "Restores .borg files from .borge.lock/original/ and removes the lock.";
          `P "Code file changes are NOT reverted — use git checkout or git reset.";
          `P "See also: borge commit (apply changes), borge make (start implementing)."])
  Term.(const run $ quiet)
