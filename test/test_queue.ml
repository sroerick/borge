(* Test merge queue: queue item lifecycle, conflict detection, state transitions.
   The library (lib/merge/) was implemented but had no dedicated test — this
   locks the core operations. *)
open Borge_lib
open Queue_types

let make_item ~id ~state ~branch ~files =
  {
    id; state; branch; worktree_path = ("/tmp/" ^ id);
    summary = "test item"; affected_files = files; confidence = 0.5;
    submitted_at = "2026-07-05T00:00:00Z"; merged_at = None; rejected_reason = None;
  }

let test_empty_queue () =
  let q = empty_queue in
  Alcotest.(check int) "empty has 0 items" 0 (List.length q.items);
  Alcotest.(check bool) "find on empty None" true (find_item q "nope" = None)

let test_add_and_find () =
  let q = empty_queue in
  let item = make_item ~id:"mq-1" ~state:Pending ~branch:"agent/run-1" ~files:["lib/foo.ml"] in
  let q' = add_item q item in
  Alcotest.(check int) "1 item after add" 1 (List.length q'.items);
  Alcotest.(check bool) "find returns the item" true
    (match find_item q' "mq-1" with Some i -> i.id = "mq-1" | None -> false)

let test_remove_item () =
  let q = empty_queue in
  let i1 = make_item ~id:"mq-1" ~branch:"b1" ~state:Pending ~files:["a.ml"] in
  let i2 = make_item ~id:"mq-2" ~branch:"b2" ~state:Pending ~files:["b.ml"] in
  let q = add_item (add_item q i1) i2 in
  let q' = remove_item q "mq-1" in
  Alcotest.(check int) "1 item after remove" 1 (List.length q'.items);
  Alcotest.(check bool) "removed item gone" true (find_item q' "mq-1" = None)

let test_update_state () =
  let q = empty_queue in
  let item = make_item ~id:"mq-1" ~branch:"b1" ~state:Pending ~files:["a.ml"] in
  let q = add_item q item in
  let q' = update_state q "mq-1" Merging in
  (match find_item q' "mq-1" with
   | Some i -> Alcotest.(check bool) "state updated to Merging" true (i.state = Merging)
   | None -> Alcotest.fail "item not found after update")

let test_count_by_state () =
  let q = empty_queue in
  let i1 = make_item ~id:"mq-1" ~branch:"b1" ~state:Pending ~files:["a.ml"] in
  let i2 = make_item ~id:"mq-2" ~branch:"b2" ~state:Ready ~files:["b.ml"] in
  let i3 = make_item ~id:"mq-3" ~branch:"b3" ~state:Ready ~files:["c.ml"] in
  let i4 = make_item ~id:"mq-4" ~branch:"b4" ~state:Merged ~files:["d.ml"] in
  let q = List.fold_left add_item q [i1; i2; i3; i4] in
  let counts = count_by_state q in
  Alcotest.(check int) "1 pending" 1 (List.assoc_opt "pending" counts |> Option.value ~default:0);
  Alcotest.(check int) "2 ready" 2 (List.assoc_opt "ready" counts |> Option.value ~default:0);
  Alcotest.(check int) "1 merged" 1 (List.assoc_opt "merged" counts |> Option.value ~default:0)

let test_conflict_detection () =
  (* Two items touching the same file: second should be blocked. *)
  let q = empty_queue in
  let i1 = make_item ~id:"mq-1" ~branch:"b1" ~state:Pending ~files:["lib/parser.ml"] in
  let i2 = make_item ~id:"mq-2" ~branch:"b2" ~state:Pending ~files:["lib/parser.ml"] in
  let i3 = make_item ~id:"mq-3" ~branch:"b3" ~state:Pending ~files:["lib/lexer.ml"] in
  let q = List.fold_left add_item q [i1; i2; i3] in
  let q' = Conflict_detect.recalculate_states q in
  (* i1 should be Ready (no predecessors), i2 Blocked (overlap with i1),
     i3 Ready (no overlap). *)
  let state_of id = match find_item q' id with Some i -> i.state | None -> failwith "missing" in
  Alcotest.(check bool) "i1 ready" true (state_of "mq-1" = Ready);
  Alcotest.(check bool) "i2 blocked (overlap)" true (state_of "mq-2" = Blocked);
  Alcotest.(check bool) "i3 ready (no overlap)" true (state_of "mq-3" = Ready)

let test_no_conflict_independent_files () =
  (* Items touching disjoint files don't block each other. *)
  let q = empty_queue in
  let i1 = make_item ~id:"mq-1" ~branch:"b1" ~state:Pending ~files:["a.ml"] in
  let i2 = make_item ~id:"mq-2" ~branch:"b2" ~state:Pending ~files:["b.ml"] in
  let q = add_item (add_item q i1) i2 in
  let q' = Conflict_detect.recalculate_states q in
  let state_of id = match find_item q' id with Some i -> i.state | None -> failwith "missing" in
  Alcotest.(check bool) "i1 ready" true (state_of "mq-1" = Ready);
  Alcotest.(check bool) "i2 ready (no overlap)" true (state_of "mq-2" = Ready)

let () =
  Alcotest.run "merge-queue" [
    "queue-types", [
      "empty queue", `Quick, test_empty_queue;
      "add and find", `Quick, test_add_and_find;
      "remove item", `Quick, test_remove_item;
      "update state", `Quick, test_update_state;
      "count by state", `Quick, test_count_by_state;
    ];
    "conflict-detect", [
      "overlap blocks", `Quick, test_conflict_detection;
      "independent files ready", `Quick, test_no_conflict_independent_files;
    ];
  ]
