(* Git branch management for agent runs
 *
 * roerick note (|
 *   Manages git branches for agent runs with naming convention:
 *   agent/run-{timestamp}-{uuid}
 *   
 *   Provides creation, deletion, checking out, and conflict detection.
 *   Tracks branch state for merge queue processing.
 * |) *)

type branch_state =
  | Active      (* Branch exists and has commits *)
  | Merged      (* Branch has been merged to main *)
  | Orphaned    (* Branch exists but no PR/worktree *)
  | Conflicting (* Has merge conflicts with main *)

type branch_info = {
  name : string;
  state : branch_state;
  base_commit : string;     (* Commit where branch diverged from main *)
  head_commit : string;     (* Latest commit on branch *)
  created_at : string;      (* ISO 8601 timestamp from name *)
  author : string;          (* Last commit author *)
}

(** Generate branch name with timestamp and uuid *)
let generate_name () =
  let timestamp = 
    let now = Unix.gmtime (Unix.time ()) in
    Printf.sprintf "%04d%02d%02d-%02d%02d%02d"
      (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday
      now.tm_hour now.tm_min now.tm_sec
  in
  let uuid =
    Random.self_init ();
    Printf.sprintf "%04x%04x" (Random.int 0x10000) (Random.int 0x10000)
  in
  Printf.sprintf "agent/run-%s-%s" timestamp uuid

(** Check if a branch name follows agent naming convention *)
let is_agent_branch name =
  String.starts_with ~prefix:"agent/run-" name

(** Parse timestamp from agent branch name *)
let parse_timestamp name =
  if is_agent_branch name then
    try
      (* agent/run-YYYYMMDD-HHMMSS-uuid *)
      let prefix = "agent/run-" in
      let rest = String.sub name (String.length prefix) (String.length name - String.length prefix) in
      let dash = String.index rest '-' in
      let date = String.sub rest 0 dash in
      let time_rest = String.sub rest (dash + 1) (String.length rest - dash - 1) in
      let dash2 = String.index time_rest '-' in
      let time = String.sub time_rest 0 dash2 in
      Some (Printf.sprintf "%s-%s" date time)
    with _ -> None
  else None

(** Run git command and return output *)
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

(** Create new agent branch from current HEAD *)
let create ?(from="HEAD") () =
  let name = generate_name () in
  match git_cmd (Printf.sprintf "checkout -b %s %s" name from) with
  | Unix.WEXITED 0, _ -> Ok name
  | _, err -> Error (Printf.sprintf "Failed to create branch: %s" err)

(** Create new agent branch in bare repo (for worktrees) *)
let create_bare ?(from="main") () =
  let name = generate_name () in
  (* First create branch in main repo *)
  match git_cmd (Printf.sprintf "branch %s %s" name from) with
  | Unix.WEXITED 0, _ -> Ok name
  | _, err -> Error (Printf.sprintf "Failed to create branch: %s" err)

(** Delete a branch *)
let delete ?(force=false) name =
  let flag = if force then "-D" else "-d" in
  match git_cmd (Printf.sprintf "branch %s %s" flag name) with
  | Unix.WEXITED 0, _ -> Ok ()
  | _, err -> Error (Printf.sprintf "Failed to delete branch: %s" err)

(** List all agent branches *)
let list_agent_branches () =
  match git_cmd "branch -a --format='%(refname:short)'" with
  | Unix.WEXITED 0, output ->
      output
      |> String.split_on_char '\n'
      |> List.filter is_agent_branch
      |> List.filter (fun s -> s <> "")
  | _ -> []

(** Get branch info *)
let get_info name =
  (* Get merge base with main *)
  let base_commit = 
    match git_cmd (Printf.sprintf "merge-base %s main" name) with
    | Unix.WEXITED 0, output -> String.trim output
    | _ -> ""
  in
  (* Get HEAD commit *)
  let head_commit =
    match git_cmd (Printf.sprintf "rev-parse %s" name) with
    | Unix.WEXITED 0, output -> String.trim output
    | _ -> ""
  in
  (* Check if merged *)
  let is_merged =
    match git_cmd (Printf.sprintf "branch --merged main --list %s" name) with
    | Unix.WEXITED 0, output -> String.trim output <> ""
    | _ -> false
  in
  (* Check for conflicts *)
  let has_conflicts =
    match git_cmd (Printf.sprintf "merge-tree $(git merge-base %s main) main %s | grep -c '<<<<<<<'" name name) with
    | Unix.WEXITED 0, "0" -> false
    | _ -> true
  in
  let state = 
    if is_merged then Merged
    else if has_conflicts then Conflicting
    else Active
  in
  {
    name;
    state;
    base_commit;
    head_commit;
    created_at = parse_timestamp name |> Option.value ~default:"unknown";
    author = "";  (* Could get from git log *)
  }

(** Check if branch has conflicts with main *)
let has_conflicts name =
  (get_info name).state = Conflicting

(** Get list of files changed in branch compared to main *)
let changed_files name =
  match git_cmd (Printf.sprintf "diff --name-only main...%s" name) with
  | Unix.WEXITED 0, output ->
      output
      |> String.split_on_char '\n'
      |> List.filter (fun s -> s <> "")
  | _ -> []

(** Rebase branch onto main *)
let rebase_onto_main name =
  match git_cmd (Printf.sprintf "rebase main %s" name) with
  | Unix.WEXITED 0, _ -> Ok ()
  | _, err -> 
      (* Abort failed rebase *)
      ignore (git_cmd "rebase --abort");
      Error (Printf.sprintf "Rebase failed: %s" err)

(** Merge branch into main (fast-forward only) *)
let merge_ff name =
  match git_cmd (Printf.sprintf "merge --ff-only %s" name) with
  | Unix.WEXITED 0, _ -> Ok ()
  | _, err -> Error (Printf.sprintf "Fast-forward merge failed: %s" err)

(** Merge branch into main (create merge commit) *)
let merge_commit ~message name =
  match git_cmd (Printf.sprintf "merge -m '%s' %s" message name) with
  | Unix.WEXITED 0, _ -> Ok ()
  | _, err -> Error (Printf.sprintf "Merge failed: %s" err)

(** Squash merge branch into main *)
let merge_squash ~message name =
  match git_cmd (Printf.sprintf "merge --squash --no-commit %s" name) with
  | Unix.WEXITED 0, _ ->
      (* Commit with message *)
      (match git_cmd (Printf.sprintf "commit -m '%s'" message) with
       | Unix.WEXITED 0, _ -> Ok ()
       | _, err -> Error (Printf.sprintf "Squash commit failed: %s" err))
  | _, err -> Error (Printf.sprintf "Squash merge failed: %s" err)
