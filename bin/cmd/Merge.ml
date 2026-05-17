(* borge merge — process merge queue

    Commands:
    --next: Merge the first ready item
    --all:  Merge all ready items in order
    --dry-run: Show what would be merged without executing *)

open Borge_lib
open Queue_types

let run_merge ~item ~strategy ~dry_run () =
  let strategy = Merge_strategies.strategy_of_string strategy |> Option.value ~default:Merge_strategies.FastForward in
  
  Printf.printf "Merging %s (%s)\n" item.id item.summary;
  Printf.printf "  Branch: %s\n" item.branch;
  Printf.printf "  Strategy: %s\n" (Merge_strategies.string_of_strategy strategy);
  Printf.printf "  Files: %d\n" (List.length item.affected_files);
  
  if dry_run then begin
    Printf.printf "  [DRY-RUN] Would merge with %s\n" (Merge_strategies.string_of_strategy strategy);
    Ok ()
  end else begin
    (* Update state to Merging *)
    (match Queue_storage.load () with
     | Error e -> Error (Printf.sprintf "Failed to load queue: %s" e)
     | Ok queue ->
         let queue' = update_state queue item.id Merging in
         ignore (Queue_storage.save queue');
         
         (* Execute merge *)
         let before_merge = 
           match Merge_strategies.git_cmd "rev-parse HEAD" with
           | Unix.WEXITED 0, hash -> Some (String.trim hash)
           | _ -> None
         in
         
         let message = Printf.sprintf "Merge %s: %s" item.id item.summary in
         let _, result = Merge_strategies.execute ~strategy ~message item.branch in
         
         (* Update state based on result *)
         (match Queue_storage.load () with
          | Ok queue ->
              let queue'' = match result with
                | Success _ ->
                    Printf.printf "  ✓ Merged successfully\n";
                    update_state queue item.id Merged
                | Conflicts files ->
                    Printf.printf "  ✗ Conflicts in: %s\n" (String.concat ", " files);
                    let item' = { item with 
                      state = Rejected;
                      rejected_reason = Some ("Conflicts: " ^ String.concat ", " files)
                    } in
                    { queue with items = List.map (fun i -> 
                      if i.id = item.id then item' else i) queue.items }
                | Error msg ->
                    Printf.printf "  ✗ Merge failed: %s\n" msg;
                    let item' = { item with 
                      state = Rejected;
                      rejected_reason = Some msg
                    } in
                    { queue with items = List.map (fun i -> 
                      if i.id = item.id then item' else i) queue.items }
              in
              ignore (Queue_storage.save queue'');
              
              (* Rollback if failed *)
              (match result with
               | Success _ -> ()
               | _ ->
                   (match Merge_strategies.git_cmd "merge --abort" with
                    | _ -> ());
                   ignore (Rollback.rollback_merge ~type_:Soft ~before_merge ~branch:item.branch result));
              
              Ok ()
          | Error _ -> Ok ()))
  end

let merge_next ~strategy ~dry_run () =
  match Queue_storage.load () with
  | Error e ->
      Printf.eprintf "Error loading queue: %s\n" e;
      exit 1
  | Ok queue ->
      match next_ready queue with
      | None ->
          Printf.printf "No ready items in queue\n";
          exit 0
      | Some item ->
          (match run_merge ~item ~strategy ~dry_run () with
           | Ok () -> exit 0
           | Error e ->
               Printf.eprintf "Merge failed: %s\n" e;
               exit 1)

let merge_all ~strategy ~dry_run () =
  let rec merge_loop count =
    match Queue_storage.load () with
    | Error e ->
        Printf.eprintf "Error loading queue: %s\n" e;
        exit 1
    | Ok queue ->
        match next_ready queue with
        | None -> 
            Printf.printf "Merged %d item(s)\n" count;
            exit 0
        | Some item ->
            (match run_merge ~item ~strategy ~dry_run () with
             | Ok () -> merge_loop (count + 1)
             | Error e ->
                 Printf.eprintf "Merge failed: %s\n" e;
                 exit 1)
  in
  merge_loop 0

open Cmdliner

let strategy =
  Arg.(value & opt string "fast-forward"
    & info ["strategy"; "s"]
    ~docv:"STRATEGY"
    ~doc:"Merge strategy: fast-forward, rebase, merge, or squash")

let dry_run =
  Arg.(value & flag & info ["dry-run"; "n"]
    ~doc:"Show what would be merged without executing")

let next =
  Arg.(value & flag & info ["next"]
    ~doc:"Merge the first ready item")

let all =
  Arg.(value & flag & info ["all"; "a"]
    ~doc:"Merge all ready items")

let run strategy dry_run next all =
  if not next && not all then begin
    Printf.eprintf "Error: specify --next or --all\n";
    exit 1
  end;
  if next && all then begin
    Printf.eprintf "Error: cannot specify both --next and --all\n";
    exit 1
  end;
  if next then
    merge_next ~strategy ~dry_run ()
  else
    merge_all ~strategy ~dry_run ()

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "merge" ~doc:"process merge queue"
    ~man:[`S "DESCRIPTION";
          `P "Merges ready items from the agent merge queue.";
          `P "Use --next to merge one item, --all to merge all ready items.";
          `P "Supports multiple merge strategies via --strategy.";
          `S "EXAMPLES";
          `P "borge merge --next                    # Merge first ready item";
          `P "borge merge --all --strategy squash   # Squash merge all ready";
          `P "borge merge --next --dry-run          # Preview without merging";])
  Term.(const run $ strategy $ dry_run $ next $ all)
