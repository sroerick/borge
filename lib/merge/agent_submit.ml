(* Auto-submit agent runs to merge queue on completion
 *
 * roerick note (|
 *   Hooks into agent completion to automatically submit worktrees
 *   to the merge queue. Called by borge make/plan on clean exit.
 *   
 *   Submission requires:
 *   - Worktree is in clean state (no uncommitted changes)
 *   - borge balance/check pass
 *   - Confidence score from agent self-assessment
 * |) *)

open Queue_types

(** Submission config *)
type submit_config = {
  auto_submit : bool;         (* Whether to auto-submit on completion *)
  min_confidence : float;     (* Minimum confidence to auto-submit *)
  require_clean : bool;       (* Require clean worktree *)
  require_checks : bool;      (* Require balance/check pass *)
}

(** Default config *)
let default_config = {
  auto_submit = true;
  min_confidence = 0.7;
  require_clean = true;
  require_checks = true;
}

(** Result of submission attempt *)
type submit_result =
  | Submitted of item_id      (* Successfully submitted with ID *)
  | Skipped of string         (* Skipped with reason *)
  | Error of string           (* Error occurred *)

(** Run borge check on worktree *)
let run_checks worktree_path =
  let cmd = Printf.sprintf "cd %s && borge balance >/dev/null 2>&1 && borge check >/dev/null 2>&1" worktree_path in
  Sys.command cmd = 0

(** Check if worktree is clean *)
let is_worktree_clean worktree_path =
  let cmd = Printf.sprintf "cd %s && git status --porcelain | wc -l" worktree_path in
  let ic = Unix.open_process_in cmd in
  try
    let line = input_line ic |> String.trim in
    ignore (Unix.close_process_in ic);
    line = "0"
  with _ ->
    ignore (Unix.close_process_in ic);
    false

(** Get commit message for summary *)
let get_commit_summary worktree_path =
  let cmd = Printf.sprintf "cd %s && git log -1 --pretty=format:'%%s'" worktree_path in
  let ic = Unix.open_process_in cmd in
  try
    let msg = input_line ic |> String.trim in
    ignore (Unix.close_process_in ic);
    if String.length msg > 80 then
      String.sub msg 0 77 ^ "..."
    else
      msg
  with _ ->
    ignore (Unix.close_process_in ic);
    "Agent run"

(** Estimate confidence based on agent self-assessment

    Reads optional .borge/CONFIDENCE file that agent may have written *)
let read_confidence worktree_path =
  let confidence_file = Filename.concat worktree_path ".borge/CONFIDENCE" in
  if Sys.file_exists confidence_file then
    try
      let content = File_utils.read_file confidence_file |> String.trim in
      float_of_string content
    with _ -> 0.5
  else
    0.5  (* Default if agent didn't write confidence *)

(** Get affected files from git diff *)
let get_affected_files worktree_path =
  let cmd = Printf.sprintf "cd %s && git diff --name-only HEAD~1 HEAD 2>/dev/null" worktree_path in
  let ic = Unix.open_process_in cmd in
  let files = ref [] in
  (try
    while true do
      let line = input_line ic in
      if line <> "" then files := line :: !files
    done
  with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  List.rev !files

(** Submit worktree to merge queue

    Returns the queue item ID on success *)
let submit_worktree ~worktree_path ~summary ~confidence () =
  (* Get branch from worktree *)
  let branch =
    let cmd = Printf.sprintf "cd %s && git branch --show-current" worktree_path in
    let ic = Unix.open_process_in cmd in
    try
      let b = input_line ic |> String.trim in
      ignore (Unix.close_process_in ic);
      b
    with _ ->
      ignore (Unix.close_process_in ic);
      "unknown"
  in
  
  let affected_files = get_affected_files worktree_path in
  
  (* Check lockfile conflicts *)
  let lock_conflicts = Lockfile.check_conflicts affected_files in
  if lock_conflicts <> [] then
    Error "Conflicts with active runs in lockfile"
  else
    (* Create queue item *)
    let id =
      let now = Unix.gmtime (Unix.time ()) in
      Printf.sprintf "auto-%04d%02d%02d-%02d%02d%02d-%04x"
        (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday
        now.tm_hour now.tm_min now.tm_sec
        (Random.int 0x10000)
    in
    let submitted_at =
      let now = Unix.gmtime (Unix.time ()) in
      Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
        (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday
        now.tm_hour now.tm_min now.tm_sec
    in
    let item = {
      id;
      state = Pending;
      branch;
      worktree_path;
      summary;
      affected_files;
      confidence;
      submitted_at;
      merged_at = None;
      rejected_reason = None;
    } in
    
    (* Load queue, add item, recalculate, save *)
    match Queue_storage.load () with
    | Error e -> Error (Printf.sprintf "Failed to load queue: %s" e)
    | Ok queue ->
        let queue' = add_item queue item in
        let queue'' = Conflict_detect.recalculate_states queue' in
        match Queue_storage.save queue'' with
        | Error e -> Error (Printf.sprintf "Failed to save queue: %s" e)
        | Ok () -> Submitted id

(** Auto-submit on agent completion

    Called by borge make/plan when agent exits cleanly.
    Respects config settings for clean checks and confidence threshold. *)
let auto_submit ~config ~worktree_path () =
  if not config.auto_submit then
    Skipped "auto_submit disabled in config"
  else
    (* Check worktree is clean if required *)
    if config.require_clean && not (is_worktree_clean worktree_path) then
      Skipped "worktree has uncommitted changes"
    else
      (* Run checks if required *)
      if config.require_checks && not (run_checks worktree_path) then
        Skipped "borg balance/check failed"
      else
        (* Get summary and confidence *)
        let summary = get_commit_summary worktree_path in
        let confidence = read_confidence worktree_path in
        
        (* Check confidence threshold *)
        if confidence < config.min_confidence then
          Skipped (Printf.sprintf "confidence %.2f below threshold %.2f" 
                     confidence config.min_confidence)
        else
          submit_worktree ~worktree_path ~summary ~confidence ()

(** Manual submit with explicit parameters *)
let manual_submit ~worktree_path ~summary ~confidence () =
  submit_worktree ~worktree_path ~summary ~confidence ()

(** Read config from .borge/config file

    Simple key=value format:
    auto_submit=true
    min_confidence=0.7 *)
let read_config () =
  let config_file = ".borge/config" in
  if not (Sys.file_exists config_file) then
    default_config
  else
    try
      let lines = File_utils.read_file config_file |> String.split_on_char '\n' in
      List.fold_left (fun cfg line ->
        let line = String.trim line in
        if line = "" || line.[0] = '#' then
          cfg
        else
          try
            let eq = String.index line '=' in
            let key = String.sub line 0 eq |> String.trim in
            let value = String.sub line (eq + 1) (String.length line - eq - 1) |> String.trim in
            match key with
            | "auto_submit" -> { cfg with auto_submit = bool_of_string value }
            | "min_confidence" -> { cfg with min_confidence = float_of_string value }
            | "require_clean" -> { cfg with require_clean = bool_of_string value }
            | "require_checks" -> { cfg with require_checks = bool_of_string value }
            | _ -> cfg
          with _ -> cfg
      ) default_config lines
    with _ -> default_config
