(** borge commit — finalize the current agent session

    Applies changes from the agent session:
    1. Runs integrity check (unless --force)
    2. Formats all .borg files
    3. Runs balance check
    4. Git commits the changes
    5. Removes the lock directory

    Called automatically after make/plan/drift --agent unless --no-commit.
    Can be called manually after --no-commit sessions. *)

open Borge_lib

let run force dir =
  ignore dir;
  if not (Lock.is_locked ()) then begin
    Printf.eprintf "Error: No active lock. Nothing to commit.\n";
    Printf.eprintf "Did you mean to run 'borge make' or 'borge plan' first?\n";
    exit 1
  end;

  let state = Lock.get_state () in
  if state = "committed" then begin
    Printf.eprintf "Error: Lock already committed.\n";
    exit 1
  end;

  if state = "no-lock" then begin
    Printf.eprintf "Error: Lock state is invalid.\n";
    exit 1
  end;

  (* Integrity check unless --force *)
  if not force then begin
    match Lock.check_integrity () with
    | Error msg ->
      Printf.eprintf "Integrity check failed:\n%s\n" msg;
      Printf.eprintf "Run 'borge commit --force' to override or 'borge abort' to discard.\n";
      exit 1
    | Ok _ -> ()
  end else begin
    Printf.printf "--force: skipping integrity check.\n"
  end;

  (* Run teardown with commit *)
  match Lock.teardown ~commit:true with
  | Ok msg ->
    Printf.printf "%s\n" msg;
    Lock.remove ();
    Printf.printf "Committed and lock removed.\n";
    exit 0
  | Error msg ->
    Printf.eprintf "Commit failed: %s\n" msg;
    Printf.eprintf "Run 'borge commit --force' to override or 'borge abort' to discard.\n";
    exit 1

open Cmdliner

let force =
  Arg.(value & flag & info ["force"; "f"]
    ~doc:"Force commit even if integrity check fails")

let dir =
  Arg.(value & opt dir "." & info ["dir"; "d"] ~docv:"DIR"
    ~doc:"Project directory")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "commit" ~doc:"commit the current agent session"
    ~man:[`S "DESCRIPTION";
          `P "Applies changes from an agent session and commits them.";
          `P "Runs integrity check, formats .borg files, and creates a git commit.";
          `P "Called automatically after make/plan unless --no-commit.";
          `P "Use --force to override integrity check failures.";
          `S "INTEGRITY CHECK";
          `P "The integrity check verifies that human-authored comments were not";
          `P "deleted during the agent session. If deletions are detected,";
          `P "the commit is blocked unless --force is used."])
  Term.(const run $ force $ dir)
