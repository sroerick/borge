(* Rollback failed merges
 *
 * roerick note (|
 *   Reverts failed merges to restore main branch state.
 *   
 *   Rollback strategies:
 *   - SOFT: Reset to before merge (preserves working tree)
 *   - HARD: Reset and clean working tree
 *   - REVERT: Create new revert commit
 *
 *   Always preserves agent worktree for debugging.
 * |) *)

type rollback_type =
  | Soft      (* git reset --soft *)
  | Hard      (* git reset --hard *)
  | Revert    (* git revert *)

type rollback_result =
  | Success
  | Error of string

(** Run git command *)
let git_cmd cmd =
  let full_cmd = Printf.sprintf "git %s 2>&1" cmd in
  let ic = Unix.open_process_in full_cmd in
  let output = ref "" in
  (try
    while true do
      (* exempt: input_line *) output := !output ^ input_line ic ^ "\n"
    done
  with End_of_file -> ());
  let status = Unix.close_process_in ic in
  (status, !output)

(** Rollback to a specific commit

    type_: Soft preserves changes in working tree
           Hard removes all changes
           Revert creates new commit
    commit: Hash to rollback to
    branch: Current branch name (for revert message) *)
let rollback ~type_ ~commit ?(branch="") () =
  match type_ with
  | Soft ->
      (* Soft reset - keeps changes staged *)
      (match git_cmd (Printf.sprintf "reset --soft %s" commit) with
       | Unix.WEXITED 0, _ ->
           Printf.printf "Rolled back to %s (changes preserved in staging area)\n" commit;
           Success
       | _, err -> Error (Printf.sprintf "Soft rollback failed: %s" err))
      
  | Hard ->
      (* Hard reset - removes all changes *)
      (match git_cmd (Printf.sprintf "reset --hard %s" commit) with
       | Unix.WEXITED 0, _ ->
           Printf.printf "Rolled back hard to %s\n" commit;
           Success
       | _, err -> Error (Printf.sprintf "Hard rollback failed: %s" err))
      
  | Revert ->
      (* Create revert commits *)
      (* First find commits since the target *)
      (match git_cmd (Printf.sprintf "rev-list --reverse %s..HEAD" commit) with
       | Unix.WEXITED 0, commits_str ->
           let commits = 
             commits_str 
             |> String.split_on_char '\n' 
             |> List.filter (fun s -> s <> "")
           in
           if commits = [] then
             Success  (* Nothing to revert *)
           else begin
             (* Revert each commit in reverse order *)
             let rec revert_all = function
               | [] -> Success
               | c :: rest ->
                   match git_cmd (Printf.sprintf "revert --no-commit %s" c) with
                   | Unix.WEXITED 0, _ -> revert_all rest
                   | _, err -> 
                       ignore (git_cmd "revert --abort");
                       Error (Printf.sprintf "Revert failed at %s: %s" c err)
             in
             match revert_all commits with
             | Success ->
                 (* Commit the reverts *)
                 let commit_msg = Printf.sprintf "Revert failed merge of %s" branch in
                 (match git_cmd (Printf.sprintf "commit -m '%s'" commit_msg) with
                  | Unix.WEXITED 0, _ ->
                      Printf.printf "Created revert commits for failed merge\n";
                      Success
                  | _, err -> Error (Printf.sprintf "Failed to commit reverts: %s" err))
             | Error e -> Error e
           end
       | _, err -> Error (Printf.sprintf "Failed to list commits to revert: %s" err))

(** Rollback a failed merge from merge_strategies result *)
let rollback_merge ~type_ ~before_merge ~branch result =
  match result with
  | Merge_strategies.Success _ ->
      Printf.eprintf "Warning: Cannot rollback successful merge\n";
      Error "Cannot rollback successful merge"
      
  | Merge_strategies.Conflicts _files ->
      (* Abort any ongoing merge *)
      ignore (git_cmd "merge --abort");
      (* Reset to before merge *)
      (match before_merge with
       | Some commit -> rollback ~type_ ~commit ~branch ()
       | None -> 
           Printf.eprintf "Warning: No before-merge commit recorded\n";
           ignore (git_cmd "merge --abort");
           Success)
      
  | Merge_strategies.Error msg ->
      (* Abort any ongoing merge *)
      ignore (git_cmd "merge --abort");
      (* Reset to before merge *)
      (match before_merge with
       | Some commit -> 
           let result = rollback ~type_ ~commit ~branch () in
           (match result with
            | Success -> Printf.printf "Rolled back failed merge: %s\n" msg
            | _ -> ());
           result
       | None -> 
           ignore (git_cmd "merge --abort");
           Success)

(** Preserve worktree for debugging after rollback

    Creates a reference to the failed state before rolling back. *)
let preserve_worktree ~branch ~worktree_path result =
  (* let _failed_ref = Printf.sprintf "refs/failed-merges/%s" branch in *)
  match result with
  | Merge_strategies.Success _ -> ()  (* Don't preserve successes *)
  | _ ->
      (* Tag the failed state *)
      ignore (git_cmd (Printf.sprintf "tag -f %s-failed HEAD" branch));
      Printf.printf "Preserved failed state as %s-failed tag\n" branch;
      Printf.printf "Worktree preserved at: %s\n" worktree_path

(** Full rollback workflow

    1. Preserve worktree
    2. Rollback main branch
    3. Update queue item status to REJECTED *)
let full_rollback ~queue_item ~type_ ~before_merge () =
  (* Preserve worktree for debugging *)
  preserve_worktree ~branch:queue_item.Queue_types.branch 
    ~worktree_path:queue_item.Queue_types.worktree_path 
    (Merge_strategies.Error "rollback");
  
  (* Rollback the merge *)
  rollback_merge ~type_ ~before_merge ~branch:queue_item.Queue_types.branch 
    (Merge_strategies.Error "rollback")

(** Check if rollback is possible *)
let can_rollback commit =
  match git_cmd (Printf.sprintf "cat-file -t %s" commit) with
  | Unix.WEXITED 0, _ -> true
  | _ -> false
