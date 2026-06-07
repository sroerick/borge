(* borge merge-queue — manage agent merge queue

    Commands:
    - list: Show pending merge queue items
    - show ID: Show details of specific item
    - submit --worktree PATH: Submit worktree to queue
    - cancel ID: Cancel a queued item *)

open Borge_lib
open Queue_types

(* agent note (|
 *   WHAT: List all pending items in the merge queue, displaying
 *   their ID, state, branch, confidence, and summary.
 *
 *   WHY: The primary command to see what's in the queue waiting
 *   to be merged.
 * |) *)
let list_items () =
  match Queue_storage.load () with
  | Error e ->
      Printf.eprintf "Error loading queue: %s\n" e;
      exit 1
  | Ok queue ->
      if queue.items = [] then
        Printf.printf "Merge queue is empty.\n"
      else begin
        Printf.printf "Merge Queue (%d items):\n\n" (List.length queue.items);
        Printf.printf "%-10s %-12s %-20s %-8s %s\n" "ID" "STATE" "BRANCH" "CONF" "SUMMARY";
        Printf.printf "%s\n" (String.make 80 '-');
        List.iter (fun (item : queue_item) ->
          let id_short = 
            if String.length item.id > 8 then String.sub item.id 0 8 else item.id
          in
          Printf.printf "%-10s %-12s %-20s %6.0f%%  %.40s\n"
            id_short
            (string_of_state item.state)
            (Filename.basename item.branch)
            (item.confidence *. 100.0)
            item.summary
        ) queue.items;
        let counts = count_by_state queue in
        Printf.printf "\nSummary: %d pending, %d ready, %d blocked, %d merging, %d merged, %d rejected\n"
          (List.assoc "pending" counts)
          (List.assoc "ready" counts)
          (List.assoc "blocked" counts)
          (List.assoc "merging" counts)
          (List.assoc "merged" counts)
          (List.assoc "rejected" counts);
      end

(* exempt doc: CLI command handler - purpose is clear from context *)
let show_item id =
  match Queue_storage.load () with
  | Error e ->
      Printf.eprintf "Error loading queue: %s\n" e;
      exit 1
  | Ok queue ->
      match find_item queue id with
      | None ->
          (* Try to find by prefix *)
          let matches = List.filter (fun (i : queue_item) ->
            String.starts_with ~prefix:id i.id
          ) queue.items in
          (match matches with
           | [item] -> show_item_detail item
           | [] ->
               Printf.eprintf "Item not found: %s\n" id;
               exit 1
           | _ ->
               Printf.eprintf "Multiple items match prefix '%s', please use full ID\n" id;
               exit 1)
      | Some item -> show_item_detail item

(* exempt doc: CLI detail printer - purpose is clear from context *)
let show_item_detail (item : queue_item) =
  Printf.printf "Merge Queue Item: %s\n" item.id;
  Printf.printf "  State: %s\n" (string_of_state item.state);
  Printf.printf "  Branch: %s\n" item.branch;
  Printf.printf "  Worktree: %s\n" item.worktree_path;
  Printf.printf "  Summary: %s\n" item.summary;
  Printf.printf "  Confidence: %.2f\n" item.confidence;
  Printf.printf "  Submitted: %s\n" item.submitted_at;
  Printf.printf "  Affected files: %d\n" (List.length item.affected_files);
  List.iter (Printf.printf "    - %s\n") (List.take 10 item.affected_files);
  if List.length item.affected_files > 10 then
    Printf.printf "    ... and %d more\n" (List.length item.affected_files - 10);
  (match item.merged_at with
   | Some t -> Printf.printf "  Merged: %s\n" t
   | None -> ());
  (match item.rejected_reason with
   | Some r -> Printf.printf "  Rejected: %s\n" r
   | None -> ());
  
  (* Show conflicts if blocked *)
  if item.state = Blocked then
    let queue = match Queue_storage.load () with Ok q -> q | Error _ -> empty_queue in
    let conflicts = Conflict_detect.is_blocked_by queue item.id in
    if conflicts <> [] then begin
      Printf.printf "\n  Blocked by conflicts with:\n";
      List.iter (fun (c : Conflict_detect.conflict) ->
        Printf.printf "    - %s: %s\n" c.item1_id 
          (String.concat ", " c.conflicting_files)
      ) conflicts
    end

(* agent note (|
 *   WHAT: Submit a completed worktree to the merge queue.
 *   Validates the worktree exists, extracts branch name, detects
 *   affected files, checks for lockfile conflicts, and creates
 *   a queue item for the worktree.
 *
 *   WHY: This is how agent worktrees get merged into main after
 *   successful completion.
 * |) *)
let submit_worktree ~worktree_path ~summary ~confidence =
  (* Validate worktree exists *)
  if not (Sys.file_exists worktree_path) then begin
    Printf.eprintf "Error: Worktree not found: %s\n" worktree_path;
    exit 1
  end;
  
  (* Get branch name from worktree *)
  let branch =
    let cmd = Printf.sprintf "cd %s && git branch --show-current 2>/dev/null" worktree_path in
    let ic = Unix.open_process_in cmd in
    try
      let b = input_line ic |> String.trim in
      ignore (Unix.close_process_in ic);
      b
    with End_of_file ->
      ignore (Unix.close_process_in ic);
      Printf.eprintf "Error: Could not determine branch for worktree\n";
      exit 1
  in
  
  (* Get affected files *)
  let affected_files = Branch.changed_files branch in
  
  (* Check for lockfile conflicts *)
  let conflicts = Lockfile.check_conflicts affected_files in
  if conflicts <> [] then begin
    Printf.eprintf "Error: Files conflict with active runs:\n";
    List.iter (fun (run : Lockfile.run_lock) ->
      Printf.eprintf "  - %s: %s\n" run.id (String.concat ", " run.files)
    ) conflicts;
    exit 1
  end;
  
  (* Create queue item *)
  let id =
    let timestamp = Unix.gmtime (Unix.time ()) in
    Printf.sprintf "mq-%04d%02d%02d-%02d%02d%02d-%04x"
      (timestamp.tm_year + 1900) (timestamp.tm_mon + 1) timestamp.tm_mday
      timestamp.tm_hour timestamp.tm_min timestamp.tm_sec
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
  
  (* Load queue, add item, recalculate states, save *)
  (match Queue_storage.load () with
   | Error e ->
       Printf.eprintf "Error loading queue: %s\n" e;
       exit 1
   | Ok queue ->
       let queue' = add_item queue item in
       let queue'' = Conflict_detect.recalculate_states queue' in
       (match Queue_storage.save queue'' with
        | Error e ->
            Printf.eprintf "Error saving queue: %s\n" e;
            exit 1
        | Ok () ->
            Printf.printf "Submitted item %s to merge queue\n" id;
            (match item.state with
             | Ready -> Printf.printf "Item is READY and can be merged\n"
             | Blocked -> Printf.printf "Item is BLOCKED - conflicts with earlier items\n"
             | _ -> ())))

let cancel_item id =
  (match Queue_storage.load () with
   | Error e ->
       Printf.eprintf "Error loading queue: %s\n" e;
       exit 1
   | Ok queue ->
       match find_item queue id with
       | None ->
           Printf.eprintf "Item not found: %s\n" id;
           exit 1
       | Some item ->
           if item.state = Merging then begin
             Printf.eprintf "Cannot cancel item currently being merged\n";
             exit 1
           end;
           let queue' = remove_item queue id in
           (match Queue_storage.save queue' with
            | Error e ->
                Printf.eprintf "Error saving queue: %s\n" e;
                exit 1
            | Ok () ->
                Printf.printf "Cancelled item %s\n" id))

open Cmdliner

(* exempt doc: cmdliner command definition - purpose is clear *)
let list_cmd : unit Cmd.t =
  Cmd.v (Cmd.info "list" ~doc:"List all queue items")
    Term.(const list_items $ const ())

(* exempt doc: cmdliner command definition - purpose is clear *)
let show_cmd : unit Cmd.t =
  let id =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"ID"
      ~doc:"Item ID (or prefix)")
  in
  Cmd.v (Cmd.info "show" ~doc:"Show item details")
    Term.(const show_item $ id)

(* exempt doc: cmdliner command definition - purpose is clear *)
let submit_cmd : unit Cmd.t =
  let worktree =
    Arg.(required & opt (some string) None & info ["worktree"; "w"]
      ~docv:"PATH" ~doc:"Path to agent worktree")
  in
  let summary =
    Arg.(value & opt string "" & info ["summary"; "s"]
      ~docv:"TEXT" ~doc:"Summary of changes")
  in
  let confidence =
    Arg.(value & opt float 0.5 & info ["confidence"; "c"]
      ~docv:"0.0-1.0" ~doc:"Confidence score (0.0-1.0)")
  in
  Cmd.v (Cmd.info "submit" ~doc:"Submit worktree to merge queue")
    Term.(const submit_worktree $ worktree $ summary $ confidence)

let cancel_cmd : unit Cmd.t =
  let id =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"ID"
      ~doc:"Item ID to cancel")
  in
  Cmd.v (Cmd.info "cancel" ~doc:"Cancel a queued item")
    Term.(const cancel_item $ id)

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "merge-queue" ~doc:"manage agent merge queue"
    ~man:[`S "DESCRIPTION";
          `P "Manages the queue of agent changes waiting to be merged.";
          `P "Items are submitted after agent runs complete.";
          `P "The queue processes items in order, checking for conflicts.";
          `S "COMMANDS";
          `I ("list", "Show all queue items with their states");
          `I ("show ID", "Show detailed info for an item");
          `I ("submit --worktree PATH", "Submit a worktree to the queue");
          `I ("cancel ID", "Remove an item from the queue")])
  (Cmd.group (Cmd.info "merge-queue") [list_cmd; show_cmd; submit_cmd; cancel_cmd])
