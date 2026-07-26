(* Regression tests for Bug_parse / Bug_write.

   Covers BORGE-1783789294: the parser only matched Atom values and silently
   returned empty_bug for the quoted/verbatim strings Bug_write emits, so
   `borge issue list/show` returned blank id/title (default triage) and
   `borge issue close` wrote to a .borge-bug file with an empty id instead of
   the real one. These tests pin the fix: String (Quoted/Verbatim) values must
   be accepted alongside Atom values. *)

open Borge_lib

let parse_string input =
  match Borge_lang.Parse.parse input with
  | file ->
      (match file.Borge_lang.Ast.top_level with
       | [] -> Bug_ast.empty_bug
       | first :: _ -> Bug_parse.parse_bug first.node)
  | exception _ -> Bug_ast.empty_bug

(* The core regression: a bug rendered exactly as Bug_write writes it —
   quoted id and title, bare-symbol status/created/filed-by — must parse back
   with id, title, and status intact, NOT as empty_bug. *)
let test_quoted_id_title () =
  let input =
    "(borge-bug (id \"BORGE-1783789294\")\n\
     \  (title \"borge issue close strips id\")\n\
     \  (status triage)\n\
     \  (created 2026-07-11)\n\
     \  (filed-by roerick)\n\
     \  (doc \"\"))" in
  let bug = parse_string input in
  Alcotest.(check string) __LOC__ "BORGE-1783789294" bug.id;
  Alcotest.(check string) __LOC__ "borge issue close strips id" bug.title;
  Alcotest.(check string) __LOC__ "triage" (Bug_ast.string_of_status bug.status);
  Alcotest.(check string) __LOC__ "2026-07-11" bug.created;
  Alcotest.(check string) __LOC__ "roerick" bug.filed_by

(* A verbatim (|...|) doc, as used by real .borge-bug files, must parse. *)
let test_verbatim_doc () =
  let input =
    "(borge-bug (id \"BORGE-1\")\n\
     \  (title \"t\")\n  (status open)\n\
     \  (created 2026-01-01)\n  (filed-by a)\n\
     \  (doc (|multi\nline\ndoc|)))" in
  let bug = parse_string input in
  Alcotest.(check string) __LOC__ "BORGE-1" bug.id;
  Alcotest.(check string) __LOC__ "multi\nline\ndoc" bug.doc

(* Round-trip: a bug rendered by Bug_write must reparse to the same record,
   including optional fields and nested (parens) inside a quoted doc. *)
let test_round_trip () =
  let bug = {
    Bug_ast.empty_bug with
    id = "BORGE-42";
    title = "Round trip";
    status = Bug_ast.Open;
    created = "2026-02-02";
    filed_by = "tester";
    doc = "some doc with 'quotes' and (parens)";
    affects_section = Some "lib/bug";
    relates_to = ["BORGE-1"];
    drift_report_id = Some "DRIFT-9";
  } in
  let bug2 = parse_string (Bug_write.render bug) in
  Alcotest.(check string) __LOC__ bug.id bug2.id;
  Alcotest.(check string) __LOC__ bug.title bug2.title;
  Alcotest.(check string) __LOC__ "open" (Bug_ast.string_of_status bug2.status);
  Alcotest.(check string) __LOC__ bug.created bug2.created;
  Alcotest.(check string) __LOC__ bug.filed_by bug2.filed_by;
  Alcotest.(check string) __LOC__ bug.doc bug2.doc;
  Alcotest.(check (option string)) __LOC__ (Some "lib/bug") bug2.affects_section;
  Alcotest.(check (list string)) __LOC__ ["BORGE-1"] bug2.relates_to;
  Alcotest.(check (option string)) __LOC__ (Some "DRIFT-9") bug2.drift_report_id

(* Resolution round-trips (when/by are bare symbols, how is bare in v1). *)
let test_resolution_round_trip () =
  let bug = {
    Bug_ast.empty_bug with
    id = "BORGE-7";
    title = "closed bug";
    status = Bug_ast.Closed;
    resolution = Some { Bug_ast.when_ = "1700000000"; how = "closed"; by = "roerick" };
  } in
  let bug2 = parse_string (Bug_write.render bug) in
  Alcotest.(check string) __LOC__ "closed" (Bug_ast.string_of_status bug2.status);
  (match bug2.resolution with
   | Some r ->
       let r : Bug_ast.resolution = r in
       Alcotest.(check string) __LOC__ "1700000000" r.when_;
       Alcotest.(check string) __LOC__ "closed" r.how;
       Alcotest.(check string) __LOC__ "roerick" r.by
   | None -> Alcotest.fail "resolution missing after round-trip")

(* Non-borge-bug input still falls back to empty_bug (unchanged behavior). *)
let test_empty_on_garbage () =
  let bug = parse_string "(project foo (doc \"x\"))" in
  Alcotest.(check string) __LOC__ "" bug.id;
  Alcotest.(check string) __LOC__ "triage" (Bug_ast.string_of_status bug.status)

let () =
  Alcotest.run "bug parse tests" [
    "basic", [
      Alcotest.test_case "quoted id/title" `Quick test_quoted_id_title;
      Alcotest.test_case "verbatim doc" `Quick test_verbatim_doc;
      Alcotest.test_case "round trip" `Quick test_round_trip;
      Alcotest.test_case "resolution round trip" `Quick test_resolution_round_trip;
      Alcotest.test_case "empty on garbage" `Quick test_empty_on_garbage;
    ];
  ]
