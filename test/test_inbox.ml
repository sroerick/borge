(* Test inbox: unified queue for scope concerns, bugs, drift findings. *)
open Printf
open Borge_lib

let test_round_trip () =
  (* Write an inbox item to a temp file, read it back, assert equality. *)
  let item = {
    Inbox.id = "INBOX-42";
    title = "Need retry logic";
    status = Inbox.Triage;
    filed_by = "agent:implementer";
    source = Inbox.Scope_concern;
    affects = Some "lib.borg:db-sql";
    created = "2026-07-05";
    doc = "Spec doesn't cover retry.";
    resolution = None;
  } in
  let tmp = "/tmp/borge_inbox_roundtrip.borge-inbox" in
  Inbox.write item tmp;
  (match Inbox.parse_file tmp with
   | None -> Alcotest.fail "parse returned None"
   | Some read_back ->
       Alcotest.(check string) "id" item.id read_back.id;
       Alcotest.(check string) "title" item.title read_back.title;
       Alcotest.(check string) "filed-by" item.filed_by read_back.filed_by;
       Alcotest.(check string) "created" item.created read_back.created;
       Alcotest.(check string) "doc" item.doc read_back.doc;
       Alcotest.(check string) "affects" "lib.borg:db-sql" (Option.value read_back.affects ~default:""));
  Sys.command (sprintf "rm -f %s" tmp) |> ignore

let test_status_workflow () =
  (* file → acknowledge → approve → close transitions. *)
  let tmp = "/tmp/borge_inbox_workflow" in
  Sys.command (sprintf "rm -rf %s && mkdir -p %s" tmp tmp) |> ignore;
  (* Save an item, update its status through the workflow, verify each step. *)
  let item = {
    Inbox.id = "INBOX-1";
    title = "Workflow test";
    status = Inbox.Triage;
    filed_by = "agent:test";
    source = Inbox.Bug_report;
    affects = None;
    created = "2026-07-05";
    doc = "";
    resolution = None;
  } in
  let path = Filename.concat tmp "INBOX-1.borge-inbox" in
  Inbox.write item path;
  (* Simulate acknowledge: set status Acknowledged, save, reload. *)
  let acked = { item with Inbox.status = Inbox.Acknowledged } in
  Inbox.write acked path;
  (match Inbox.parse_file path with
   | Some r -> Alcotest.(check bool) "acknowledged status" true (r.status = Inbox.Acknowledged)
   | None -> Alcotest.fail "reload after acknowledge failed");
  (* Simulate close with resolution *)
  let closed = { acked with Inbox.status = Inbox.Closed; Inbox.resolution = Some Inbox.Spec_updated } in
  Inbox.write closed path;
  (match Inbox.parse_file path with
   | Some r ->
       Alcotest.(check bool) "closed status" true (r.status = Inbox.Closed);
       (match r.resolution with
        | Some Inbox.Spec_updated -> ()
        | _ -> Alcotest.fail "resolution not Spec_updated")
   | None -> Alcotest.fail "reload after close failed");
  Sys.command (sprintf "rm -rf %s" tmp) |> ignore

let test_source_variants () =
  (* All four source variants round-trip. *)
  let tmp = "/tmp/borge_inbox_sources.borge-inbox" in
  let sources = [
    Inbox.Scope_concern, "scope-concern";
    Inbox.Bug_report, "bug-report";
    Inbox.Drift_finding, "drift-finding";
    Inbox.Review_finding, "review-finding";
  ] in
  List.iter (fun (src, label) ->
    let item = {
      Inbox.id = "INBOX-X"; title = "t"; status = Inbox.Triage;
      filed_by = "a"; source = src; affects = None;
      created = "2026-07-05"; doc = ""; resolution = None;
    } in
    Inbox.write item tmp;
    (match Inbox.parse_file tmp with
     | Some r -> Alcotest.(check bool) (sprintf "source %s round-trips" label) true (r.source = src)
     | None -> Alcotest.fail (sprintf "parse failed for source %s" label))
  ) sources;
  Sys.command (sprintf "rm -f %s" tmp) |> ignore

let test_num_of_id () =
  Alcotest.(check int) "INBOX-7 -> 7" 7 (Inbox.num_of_id "INBOX-7");
  Alcotest.(check int) "INBOX-42 -> 42" 42 (Inbox.num_of_id "INBOX-42");
  Alcotest.(check int) "INBOX-1 -> 1" 1 (Inbox.num_of_id "INBOX-1");
  Alcotest.(check int) "garbage -> 0" 0 (Inbox.num_of_id "garbage")

let test_next_id_empty () =
  (* With no existing items, next_id is INBOX-1. *)
  let tmp = "/tmp/borge_inbox_nextid_empty" in
  Sys.command (sprintf "rm -rf %s && mkdir -p %s" tmp tmp) |> ignore;
  (* Temporarily redirect the default_dir by chdir-ing to tmp. *)
  let cwd = Sys.getcwd () in
  Sys.chdir tmp;
  let id = Inbox.next_id () in
  Sys.chdir cwd;
  Alcotest.(check string) "first id is INBOX-1" "INBOX-1" id;
  Sys.command (sprintf "rm -rf %s" tmp) |> ignore

let test_scan_todos () =
  (* scan_todos finds TODO/FIXME/XXX markers. *)
  let content = "line 1\nlet f = () in\n  (* TODO: fix this *)\n  (* FIXME: broken *)\n  (* XXX: hack *)\nnormal line\n" in
  let todos = Inbox.scan_todos content in
  Alcotest.(check int) "3 todos found" 3 (List.length todos);
  let lines = List.map fst todos in
  Alcotest.(check bool) "todo on line 3" true (List.mem 3 lines)

let () =
  Alcotest.run "inbox" [
    "parse", [
      "round trip", `Quick, test_round_trip;
      "source variants", `Quick, test_source_variants;
      "num_of_id", `Quick, test_num_of_id;
    ];
    "workflow", [
      "status workflow", `Quick, test_status_workflow;
    ];
    "consolidate", [
      "next id empty", `Quick, test_next_id_empty;
      "scan todos", `Quick, test_scan_todos;
    ];
  ]
