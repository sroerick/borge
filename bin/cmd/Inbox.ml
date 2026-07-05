(** borge inbox — unified queue commands

    Implements docs/agent.borg inbox-commands. Subsumes the old bug
    commands (borge bug new → borge inbox file --source bug-report).
    One queue, one format (.borge-inbox files), many sources. *)

open Borge_lib
open Cmdliner
open Printf

let now () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

let get_user () =
  try Sys.getenv "USER" with Not_found ->
    try Sys.getenv "USER" with _ -> "unknown"

(* file: create a new inbox item *)
let cmd_file title source affects_opt doc_opt =
  let src = match Inbox.source_of_string source with
    | Some s -> s
    | None ->
      Printf.eprintf "Unknown source: %s\nValid: scope-concern, bug-report, drift-finding, review-finding\n" source;
      exit 2
  in
  let filed_by = sprintf "agent:%s" (get_user ()) in
  let (item, path) = Inbox.file_item
    ~title ~source:src ~filed_by ?affects:affects_opt ?doc:doc_opt () in
  Printf.printf "Filed inbox item %s\n" item.id;
  Printf.printf "  Status: %s\n" (Inbox.string_of_status item.status);
  Printf.printf "  Source: %s\n" (Inbox.string_of_source item.source);
  Printf.printf "  File: %s\n" path

(* list: show inbox items, optionally filtered *)
let cmd_list status_opt source_opt =
  let status = match status_opt with
    | Some s -> (match Inbox.status_of_string s with Some st -> Some st | None -> None)
    | None -> None
  in
  let source = match source_opt with
    | Some s -> (match Inbox.source_of_string s with Some src -> Some src | None -> None)
    | None -> None
  in
  let items = Inbox.list_items ~status ~source () in
  if items = [] then
    Printf.printf "Inbox is empty.\n"
  else begin
    Printf.printf "Inbox (%d items):\n" (List.length items);
    List.iter (fun (i : Inbox.inbox_item) ->
      Printf.printf "  %-12s [%-14s] [%-14s] %s\n"
        i.id (Inbox.string_of_status i.status) (Inbox.string_of_source i.source) i.title
    ) items
  end

(* show: full details of one item *)
let cmd_show id =
  match Inbox.load_item id with
  | None ->
    Printf.eprintf "Inbox item %s not found.\n" id;
    exit 1
  | Some item ->
    Printf.printf "Inbox %s\n" item.id;
    Printf.printf "  Title:   %s\n" item.title;
    Printf.printf "  Status:  %s\n" (Inbox.string_of_status item.status);
    Printf.printf "  Source:  %s\n" (Inbox.string_of_source item.source);
    Printf.printf "  Filed by: %s\n" item.filed_by;
    Printf.printf "  Created: %s\n" item.created;
    (match item.affects with
     | Some a -> Printf.printf "  Affects: %s\n" a
     | None -> ());
    (match item.resolution with
     | Some r -> Printf.printf "  Resolution: %s\n" (Inbox.string_of_resolution r)
     | None -> ());
    Printf.printf "  Doc:\n%s\n" item.doc

let cmd_acknowledge id =
  match Inbox.acknowledge id with
  | Error e -> Printf.eprintf "%s\n" e; exit 1
  | Ok (item, path) ->
    Printf.printf "Acknowledged %s\n" item.id;
    Printf.printf "  Status: %s\n" (Inbox.string_of_status item.status);
    Printf.printf "  File: %s\n" path

let cmd_approve id =
  match Inbox.approve id with
  | Error e -> Printf.eprintf "%s\n" e; exit 1
  | Ok (item, path) ->
    Printf.printf "Approved %s for spec update in Plan mode\n" item.id;
    Printf.printf "  Status: %s\n" (Inbox.string_of_status item.status);
    Printf.printf "  File: %s\n" path

let cmd_close id resolution_str =
  match Inbox.resolution_of_string resolution_str with
  | Some r ->
    (match Inbox.close id ~resolution:r with
     | Error e -> Printf.eprintf "%s\n" e; exit 1
     | Ok (item, path) ->
       Printf.printf "Closed %s\n" item.id;
       Printf.printf "  Resolution: %s\n" (Inbox.string_of_resolution r);
       Printf.printf "  File: %s\n" path)
  | None ->
    Printf.eprintf "Unknown resolution: %s\nValid: spec-updated, wontfix, duplicate, fixed-in-code\n" resolution_str;
    exit 2

let cmd_consolidate () =
  let (filed, skipped) = Inbox.consolidate () in
  Printf.printf "Consolidated: %d items filed, %d duplicates skipped\n" filed skipped

(* term definitions *)
let title =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"TITLE"
    ~doc:"Title/summary of the inbox item")

let source =
  Arg.(value & opt string "scope-concern" & info ["source"] ~docv:"SOURCE"
    ~doc:"Source: scope-concern (default), bug-report, drift-finding, review-finding")

let affects =
  Arg.(value & opt (some string) None & info ["affects"] ~docv:"SECTION"
    ~doc:"Affected spec section (e.g. lib.borg:db-sql)")

let doc_opt =
  Arg.(value & opt (some string) None & info ["doc"] ~docv:"TEXT"
    ~doc:"Description of the concern")

let id =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ID"
    ~doc:"Inbox item ID (e.g. INBOX-7)")

let status_opt =
  Arg.(value & opt (some string) None & info ["status"] ~docv:"STATUS"
    ~doc:"Filter by status (triage, acknowledged, plan-approved, closed)")

let source_opt =
  Arg.(value & opt (some string) None & info ["source"] ~docv:"SOURCE"
    ~doc:"Filter by source (scope-concern, bug-report, drift-finding, review-finding)")

let resolution =
  Arg.(required & pos 1 (some string) None & info [] ~docv:"RESOLUTION"
    ~doc:"Resolution: spec-updated, wontfix, duplicate, fixed-in-code")

let file_cmd =
  Cmd.v (Cmd.info "file" ~doc:"file a new inbox item")
    Term.(const cmd_file $ title $ source $ affects $ doc_opt)

let list_cmd =
  Cmd.v (Cmd.info "list" ~doc:"list inbox items")
    Term.(const cmd_list $ status_opt $ source_opt)

let show_cmd =
  Cmd.v (Cmd.info "show" ~doc:"show full details of an inbox item")
    Term.(const cmd_show $ id)

let acknowledge_cmd =
  Cmd.v (Cmd.info "acknowledge" ~doc:"acknowledge an item (human review)")
    Term.(const cmd_acknowledge $ id)

let approve_cmd =
  Cmd.v (Cmd.info "approve" ~doc:"approve an item for spec update in Plan mode")
    Term.(const cmd_approve $ id)

let close_cmd =
  Cmd.v (Cmd.info "close" ~doc:"close an inbox item with resolution")
    Term.(const cmd_close $ id $ resolution)

let consolidate_cmd =
  Cmd.v (Cmd.info "consolidate" ~doc:"aggregate TODOs + incomplete specs into inbox items")
    Term.(const (fun () -> cmd_consolidate ()) $ const ())

let cmd : unit Cmd.t =
  Cmd.group (Cmd.info "inbox" ~doc:"unified queue: scope concerns, bugs, drift, review")
    [file_cmd; list_cmd; show_cmd; acknowledge_cmd; approve_cmd; close_cmd; consolidate_cmd]
