(* Git merge strategies for agent queue processing
 *
 * roerick note (|
 *   Implements merge strategies:
 *   - FAST-FORWARD: Only if main hasn't moved
 *   - REBASE-THEN-MERGE: Rebase onto main, then fast-forward
 *   - MERGE-COMMIT: Always create merge commit
 *   - SQUASH: Squash all commits to single commit
 *
 *   Each strategy reports success or detailed error for rollback.
 * |) *)

type strategy =
  | FastForward
  | RebaseThenMerge
  | MergeCommit
  | Squash

type merge_result =
  | Success of string  (* Commit hash *)
  | Error of string    (* Error message *)
  | Conflicts of string list  (* List of conflicting files *)

(** Run git command and capture output *)
let git_cmd cmd =
  let full_cmd = Printf.sprintf "git %s 2>&1" cmd in
  let ic = Unix.open_process_in full_cmd in
  let output = ref "" in
  (try
    while true do
      output := !output ^ input_line ic ^ "\n"
    done
  with End_of_file -> ());
  let status = Unix.close_process_in ic in
  (status, !output)

(** Check if fast-forward is possible *)
let can_fast_forward branch =
  match git_cmd (Printf.sprintf "merge-base --is-ancestor HEAD %s" branch) with
  | Unix.WEXITED 0, _ -> true
  | _ -> false

(** Check for merge conflicts before attempting *)
let check_conflicts branch =
  (* Use merge-tree to check for conflicts without actually merging *)
  match git_cmd (Printf.sprintf "merge-tree $(git merge-base HEAD %s) HEAD %s | grep -c '<<<<<<<'" branch branch) with
  | Unix.WEXITED 0, "0\n" -> []  (* No conflicts *)
  | Unix.WEXITED 0, _ -> 
      (* Has conflicts, get list of files *)
      (match git_cmd (Printf.sprintf "merge-tree $(git merge-base HEAD %s) HEAD %s | grep '<<<<<<<'" branch branch) with
       | Unix.WEXITED 0, output ->
           output |> String.split_on_char '\n' |> List.filter (fun s -> s <> "")
       | _ -> ["unknown"])
  | _ -> []

(** Fast-forward merge *)
let fast_forward_merge branch =
  if not (can_fast_forward branch) then
    Error (Printf.sprintf "Cannot fast-forward %s: main has new commits" branch)
  else
    match git_cmd (Printf.sprintf "merge --ff-only %s" branch) with
    | Unix.WEXITED 0, _ ->
        (match git_cmd "rev-parse HEAD" with
         | Unix.WEXITED 0, hash -> Success (String.trim hash)
         | _ -> Error "Fast-forward succeeded but couldn't get commit hash")
    | _, err -> Error (Printf.sprintf "Fast-forward failed: %s" err)

(** Rebase branch onto main, then fast-forward *)
let rebase_then_merge branch =
  (* First check for conflicts *)
  let conflicts = check_conflicts branch in
  if conflicts <> [] then
    Conflicts conflicts
  else
    (* Switch to branch and rebase *)
    match git_cmd (Printf.sprintf "checkout %s" branch) with
    | Unix.WEXITED 0, _ ->
        (match git_cmd "rebase main" with
         | Unix.WEXITED 0, _ ->
             (* Rebase succeeded, switch back to main and fast-forward *)
             (match git_cmd "checkout main" with
              | Unix.WEXITED 0, _ ->
                  fast_forward_merge branch
              | _, err ->
                  Error (Printf.sprintf "Failed to checkout main after rebase: %s" err))
         | _, err ->
             (* Rebase failed, abort and report *)
             ignore (git_cmd "rebase --abort");
             ignore (git_cmd "checkout main");
             if String.contains err 'c' then
               Conflicts ["rebase conflict"]
             else
               Error (Printf.sprintf "Rebase failed: %s" err))
    | _, err -> Error (Printf.sprintf "Failed to checkout branch: %s" err)

(** Create merge commit *)
let merge_commit ~message branch =
  (* Check for conflicts first *)
  let conflicts = check_conflicts branch in
  if conflicts <> [] then
    Conflicts conflicts
  else
    let merge_msg = Printf.sprintf "Merge branch '%s': %s" branch message in
    match git_cmd (Printf.sprintf "merge -m '%s' %s" merge_msg branch) with
    | Unix.WEXITED 0, _ ->
        (match git_cmd "rev-parse HEAD" with
         | Unix.WEXITED 0, hash -> Success (String.trim hash)
         | _ -> Error "Merge succeeded but couldn't get commit hash")
    | Unix.WEXITED 1, _ ->
        (* May have conflicts, check for conflicted files *)
        (match git_cmd "diff --name-only --diff-filter=U" with
         | Unix.WEXITED 0, "" -> Error "Merge failed (no conflicts detected)"
         | Unix.WEXITED 0, files ->
             Conflicts (files |> String.split_on_char '\n' |> List.filter (fun s -> s <> ""))
         | _ -> Conflicts ["unknown"])
    | _, err -> Error (Printf.sprintf "Merge failed: %s" err)

(** Squash merge *)
let squash_merge ~message branch =
  (* Check for conflicts first *)
  let conflicts = check_conflicts branch in
  if conflicts <> [] then
    Conflicts conflicts
  else
    match git_cmd (Printf.sprintf "merge --squash --no-commit %s" branch) with
    | Unix.WEXITED 0, _ ->
        (* Check if there are changes to commit *)
        (match git_cmd "diff --cached --quiet" with
         | Unix.WEXITED 0, _ ->
             (* No changes - already up to date *)
             ignore (git_cmd "merge --abort");
             (match git_cmd "rev-parse HEAD" with
              | Unix.WEXITED 0, hash -> Success (String.trim hash)
              | _ -> Error "No changes but couldn't get commit hash")
         | _ ->
             (* Has changes, commit them *)
             let squash_msg = Printf.sprintf "Squash merge %s: %s" branch message in
             (match git_cmd (Printf.sprintf "commit -m '%s'" squash_msg) with
              | Unix.WEXITED 0, _ ->
                  (match git_cmd "rev-parse HEAD" with
                   | Unix.WEXITED 0, hash -> Success (String.trim hash)
                   | _ -> Error "Squash succeeded but couldn't get commit hash")
              | _, err -> Error (Printf.sprintf "Squash commit failed: %s" err)))
    | Unix.WEXITED 1, _ ->
        Conflicts ["conflict"]
    | _, err -> Error (Printf.sprintf "Squash merge failed: %s" err)

(** Execute merge with specified strategy *)
let execute ~strategy ~message branch =
  (* Save current state for potential rollback *)
  let before_merge =
    match git_cmd "rev-parse HEAD" with
    | Unix.WEXITED 0, hash -> Some (String.trim hash)
    | _ -> None
  in
  
  let result = match strategy with
    | FastForward -> fast_forward_merge branch
    | RebaseThenMerge -> rebase_then_merge branch
    | MergeCommit -> merge_commit ~message branch
    | Squash -> squash_merge ~message branch
  in
  
  (before_merge, result)

(** String representation of strategy *)
let string_of_strategy = function
  | FastForward -> "fast-forward"
  | RebaseThenMerge -> "rebase-then-merge"
  | MergeCommit -> "merge-commit"
  | Squash -> "squash"

(** Parse strategy from string *)
let strategy_of_string = function
  | "fast-forward" | "ff" -> Some FastForward
  | "rebase-then-merge" | "rebase" -> Some RebaseThenMerge
  | "merge-commit" | "merge" -> Some MergeCommit
  | "squash" -> Some Squash
  | _ -> None

(** Get default strategy for project *)
let default_strategy () =
  (* Could read from config file *)
  FastForward
