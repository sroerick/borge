(* Git worktree management for agent isolation
 *
 * roerick note (|
 *   Manages git worktrees for agent runs. Each agent gets an isolated
 *   worktree at .pi/worktrees/agent-{id}/ to prevent file conflicts.
 *   Provides create, remove, list, and path resolution functions.
 * |) *)

(** Worktree information *)
type worktree = {
  id : string;
  path : string;
  branch : string;
  created_at : string;  (* ISO 8601 *)
}

(** Default worktree base directory *)
let default_base_dir = ".pi/worktrees"

(** Ensure base directory exists *)
let ensure_base_dir () =
  let dir = default_base_dir in
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755

(** Generate worktree ID from timestamp and uuid *)
let generate_id () =
  let timestamp = 
    let now = Unix.gmtime (Unix.time ()) in
    Printf.sprintf "%04d%02d%02d-%02d%02d%02d"
      (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday
      now.tm_hour now.tm_min now.tm_sec
  in
  let uuid =
    Random.self_init ();
    Printf.sprintf "%04x" (Random.int 0x10000)
  in
  Printf.sprintf "agent-%s-%s" timestamp uuid

(** Create a new worktree
    
    Returns Error if:
    - Git is not initialized
    - Branch already exists
    - Worktree path already exists *)
let create ~branch ?(base_dir=default_base_dir) () =
  ensure_base_dir ();
  let id = generate_id () in
  let path = Filename.concat base_dir id in
  
  (* Check if git is initialized *)
  if not (Sys.file_exists ".git") then
    Error "Not a git repository"
  else if Sys.file_exists path then
    Error (Printf.sprintf "Worktree path already exists: %s" path)
  else
    (* Create worktree using git worktree add *)
    let cmd = Printf.sprintf "git worktree add -b %s %s 2>&1" branch path in
    let ic = Unix.open_process_in cmd in
    let output = ref "" in
    (try
      while true do
        (* exempt: input_line *) output := !output ^ input_line ic ^ "\n"
      done
    with End_of_file -> ());
    let status = Unix.close_process_in ic in
    
    match status with
    | Unix.WEXITED 0 ->
        let timestamp =
          let now = Unix.gmtime (Unix.time ()) in
          Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
            (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday
            now.tm_hour now.tm_min now.tm_sec
        in
        Ok { id; path; branch; created_at = timestamp }
    | _ ->
        Error (Printf.sprintf "Failed to create worktree: %s" !output)

(** Remove a worktree

    Cleans up both the worktree directory and the git worktree record.
    Preserves the branch unless delete_branch is true. *)
let remove ?(delete_branch=false) worktree =
  if not (Sys.file_exists worktree.path) then
    Error (Printf.sprintf "Worktree does not exist: %s" worktree.path)
  else
    (* Remove worktree using git worktree remove *)
    let cmd = Printf.sprintf "git worktree remove %s 2>&1" worktree.path in
    let ic = Unix.open_process_in cmd in
    let output = ref "" in
    (try
      while true do
        output := !output ^ input_line ic ^ "\n"
      done
    with End_of_file -> ());
    let status = Unix.close_process_in ic in
    
    match status with
    | Unix.WEXITED 0 ->
        (if delete_branch then
          (* Also delete the branch *)
          let cmd = Printf.sprintf "git branch -D %s 2>&1" worktree.branch in
          ignore (Sys.command cmd));
        Ok ()
    | _ ->
        Error (Printf.sprintf "Failed to remove worktree: %s" !output)

(** List all worktrees

    Returns list of borge-managed worktrees (those in .pi/worktrees/) *)
let list ?(base_dir=default_base_dir) () =
  if not (Sys.file_exists base_dir) then []
  else
    Sys.readdir base_dir
    |> Array.to_list
    |> List.filter (fun name -> String.starts_with ~prefix:"agent-" name)
    |> List.filter_map (fun id ->
        let path = Filename.concat base_dir id in
        if Sys.is_directory path then
          (* Try to determine branch from git *)
          let cmd = Printf.sprintf "cd %s && git branch --show-current 2>/dev/null" path in
          let ic = Unix.open_process_in cmd in
          (try
            let branch = input_line ic |> String.trim in
            ignore (Unix.close_process_in ic);
            Some { id; path; branch; created_at = "unknown" }
          with End_of_file | Sys_error _ ->
            ignore (Unix.close_process_in ic);
            None)
        else None
      )

(** Prune stale worktrees

    Removes worktrees that no longer have valid git refs.
    Useful for cleanup after interrupted agent runs. *)
let prune () =
  let cmd = "git worktree prune 2>&1" in
  let status = Sys.command cmd in
  if status = 0 then Ok () else Error "Failed to prune worktrees"

(** Get worktree path by ID *)
let get_path id =
  Filename.concat default_base_dir id

(** Check if a worktree exists *)
let exists id =
  Sys.file_exists (get_path id)

(** Validate worktree is clean

    Returns true if the worktree has no uncommitted changes *)
let is_clean worktree =
  let cmd = Printf.sprintf "cd %s && git status --porcelain 2>/dev/null | wc -l" worktree.path in
  let ic = Unix.open_process_in cmd in
  try
    let line = input_line ic |> String.trim in
    ignore (Unix.close_process_in ic);
    line = "0"
  with End_of_file | Sys_error _ ->
    ignore (Unix.close_process_in ic);
    false
