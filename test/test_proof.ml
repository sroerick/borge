(* Test proof_emit: Coq representation emission from db_app. *)
open Printf
open Borge_lib
(* read_whole_ic: read all bytes from an input_channel (input_all is
   not available in this OCaml stdlib version). *)
let read_whole_ic ic =
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  Bytes.to_string buf

(* find_sub: substring search returning byte offset or None. *)
let find_sub s sub =
  let plen = String.length sub in
  let rec loop i =
    if i + plen > String.length s then None
    else if String.sub s i plen = sub then Some i
    else loop (i+1)
  in loop 0


(* Helper: check if string contains substring *)
let contains haystack needle =
  (* exempt: Str.search_forward *)
  try let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in true
  with Not_found -> false

(* A minimal app mirroring examples/crud-app/db.borg's auth-relevant subset. *)
let crud_app =
  let open Db_ast in
  {
    tables = [
      { name = "projects";
        columns = [{ name = "owner_id"; typ = "uuid"; constraints = [] }];
        indexes = [];
        ownership = Some "owner-id" };
      { name = "tasks";
        columns = [{ name = "assignee_id"; typ = "uuid"; constraints = [] }];
        indexes = [];
        ownership = Some "assignee-id" };
    ];
    operations = [];
    relations = [];
    groups = [
      { name = "admin"; can_all = true; capabilities = [] };
      { name = "member";
        can_all = false;
        capabilities = [
          { table = "tasks"; operations = ["create"; "read"; "update"];
            where_clause = Some "assignee-id = current-user" };
          { table = "projects"; operations = ["read"];
            where_clause = None };
        ] };
    ];
  }

let test_emits_table_inductive () =
  let emitted = Proof_emit.emit_representation crud_app in
  Alcotest.(check bool) "contains table inductive" true
    (contains emitted "Inductive table : Type :=");
  Alcotest.(check bool) "has Projects constructor" true
    (contains emitted "| Projects");
  Alcotest.(check bool) "has Tasks constructor" true
    (contains emitted "| Tasks")

let test_emits_owner_of () =
  let emitted = Proof_emit.emit_representation crud_app in
  Alcotest.(check bool) "contains owner_of definition" true
    (contains emitted "Definition owner_of");
  (* owner_of Projects => Some "owner-id" *)
  Alcotest.(check bool) "projects ownership recorded" true
    (contains emitted "Some \"owner-id\"");
  Alcotest.(check bool) "tasks ownership recorded" true
    (contains emitted "Some \"assignee-id\"")

let test_emits_permitted () =
  let emitted = Proof_emit.emit_representation crud_app in
  Alcotest.(check bool) "contains permitted inductive" true
    (contains emitted "Inductive permitted");
  (* member × create × tasks should produce a constructor *)
  Alcotest.(check bool) "member create tasks permitted" true
    (contains emitted "permitted_Member_Create_Tasks");
  Alcotest.(check bool) "member read projects permitted" true
    (contains emitted "permitted_Member_Read_Projects");
  (* admin is can-all — should NOT get permitted constructors (uses is_admin) *)
  Alcotest.(check bool) "no admin permitted ctor" false
    (contains emitted "permitted_Admin_")

let test_emits_is_admin () =
  let emitted = Proof_emit.emit_representation crud_app in
  Alcotest.(check bool) "is_admin inductive present" true
    (contains emitted "Inductive is_admin");
  Alcotest.(check bool) "admin is_admin fact" true
    (contains emitted "is_admin_Admin")

let test_emits_predicate_column () =
  let emitted = Proof_emit.emit_representation crud_app in
  (* predicate_column_of for member/create/tasks => Some "assignee-id" *)
  Alcotest.(check bool) "predicate column extracted from where-clause" true
    (contains emitted "Some \"assignee-id\"");
  (* predicate_column_of for member/read/projects => None (no where-clause) *)
  Alcotest.(check bool) "predicate_column_of definition present" true
    (contains emitted "Definition predicate_column_of")

let test_run_prover_discharges_witness () =
  (* End-to-end: emit the representation from a hand-built db_app mirroring
     the crud-app auth-relevant subset, then run coqc on a witness that
     imports it. coqc IS installed on this machine (installed as part of
     the obligations-impl loop), so this exercises the Pass path. *)
  let open Proof_run in
  let open Db_ast in
  let app = {
    tables = [
      { name = "projects";
        columns = [{ name = "owner_id"; typ = "uuid"; constraints = [] }];
        indexes = []; ownership = Some "owner-id" };
      { name = "tasks";
        columns = [{ name = "assignee_id"; typ = "uuid"; constraints = [] }];
        indexes = []; ownership = Some "assignee-id" };
    ];
    operations = []; relations = [];
    groups = [
      { name = "admin"; can_all = true; capabilities = [] };
      { name = "member"; can_all = false;
        capabilities = [
          { table = "tasks"; operations = ["create"; "read"; "update"];
            where_clause = Some "assignee-id = current-user" };
          { table = "projects"; operations = ["read"]; where_clause = None };
        ] };
    ];
  } in
  (* Emit to temp files under /tmp. Use a fixed dir name (cleaned first)
     since Sys.command doesn't expand $$ the way a shell would. *)
  let tmp_dir = "/tmp/borge_proof_test" in
  Sys.command (sprintf "rm -rf %s; mkdir -p %s" tmp_dir tmp_dir) |> ignore;
  let rep_path = Filename.concat tmp_dir "BorgeSchema.v" in
  Proof_emit.emit_to_file app ~path:rep_path;
  (* Write a tiny witness that imports the representation and proves a
     trivial theorem — enough to exercise the coqc-pass path. *)
  let wit_path = Filename.concat tmp_dir "test_witness.v" in
  let oc = open_out wit_path in
  output_string oc "Require Import BorgeSchema.\n";
  output_string oc "Theorem trivial : forall (g : group), g = g.\n";
  output_string oc "Proof. intros. reflexivity. Qed.\n";
  close_out oc;
  let verdict = run_prover ~witness:wit_path ~representation:rep_path ~prover:"coq" in
  (match verdict with
   | Pass { admitted } ->
       Alcotest.(check int) "0 admits on trivial proof" 0 admitted
   | other ->
       Alcotest.fail ("expected Pass, got: " ^
         (match other with
          | Fail { message } -> "Fail(" ^ message ^ ")"
          | Prover_not_installed _ -> "Prover_not_installed"
          | Representation_stale -> "Representation_stale"
          | Pass _ -> "impossible")));
  (* cleanup *)
  Sys.command (sprintf "rm -rf %s" tmp_dir) |> ignore

let test_run_prover_absent () =
  (* coqc IS installed on this machine now, so to test the absent path
     we ask for a prover that doesn't exist. *)
  let open Proof_run in
  let verdict = run_prover
    ~witness:"proof/db_auth.v"
    ~representation:"proof/BorgeSchema.v"
    ~prover:"nonexistent-prover"
  in
  (match verdict with
   | Prover_not_installed _ -> ()  (* expected *)
   | _ -> Alcotest.fail "expected Prover_not_installed for nonexistent prover");
  Alcotest.(check bool) "prover absent handled gracefully" true true

let test_meta_round_trip () =
  (* Construct a proof_block, write it, read it back, assert equality. *)
  let open Proof_meta in
  let block = {
    commit = "abc1234";
    discharged_at = "2026-07-05T01:20:00Z";
    obligations = [
      { name = "ownership-consistency";
        witness = "proof/db_auth.v";
        prover = "coq";
        verdict = Pass { admitted = 0 };
        representation = "proof/BorgeSchema.v" };
    ];
  } in
  let tmp = "/tmp/borge_proof_meta_test.meta" in
  Sys.command (sprintf "rm -f %s" tmp) |> ignore;
  write_block ~path:tmp block;
  let read_back = read_block ~path:tmp in
  (match read_back with
   | None -> Alcotest.fail "read_block returned None after write"
   | Some rb ->
       Alcotest.(check string) "commit round-trips" block.commit rb.commit;
       Alcotest.(check string) "discharged-at round-trips" block.discharged_at rb.discharged_at;
       Alcotest.(check int) "obligation count" 1 (List.length rb.obligations);
       (match rb.obligations with
        | [o] ->
            Alcotest.(check string) "obligation name" "ownership-consistency" o.name;
            Alcotest.(check string) "witness path" "proof/db_auth.v" o.witness;
            Alcotest.(check string) "representation path" "proof/BorgeSchema.v" o.representation;
            (match o.verdict with
             | Pass { admitted } -> Alcotest.(check int) "0 admits" 0 admitted
             | _ -> Alcotest.fail "verdict not Pass after round-trip")
        | _ -> Alcotest.fail "wrong obligation shape"));
  Sys.command (sprintf "rm -f %s" tmp) |> ignore

let test_meta_determinism () =
  (* Writing the same block twice produces identical output. *)
  let open Proof_meta in
  let block = {
    commit = "abc1234";
    discharged_at = "2026-07-05T01:20:00Z";
    obligations = [
      { name = "z-last"; witness = "w1.v"; prover = "coq";
        verdict = Pass { admitted = 0 }; representation = "r1.v" };
      { name = "a-first"; witness = "w2.v"; prover = "coq";
        verdict = Pass { admitted = 2 }; representation = "r2.v" };
    ];
  } in
  let tmp1 = "/tmp/borge_proof_meta_det1.meta" in
  let tmp2 = "/tmp/borge_proof_meta_det2.meta" in
  write_block ~path:tmp1 block;
  write_block ~path:tmp2 block;
  let ic1 = open_in tmp1 in
  let s1 = read_whole_ic ic1 in
  close_in ic1;
  let ic2 = open_in tmp2 in
  let s2 = read_whole_ic ic2 in
  close_in ic2;
  Alcotest.(check string) "deterministic output" s1 s2;
  (* Also verify sorting: a-first should appear before z-last. *)
  (* Verify sorting: a-first should appear before z-last. *)
  let a_pos = find_sub s1 "a-first" in
  let z_pos = find_sub s1 "z-last" in
  (match a_pos, z_pos with
   | Some a, Some z -> Alcotest.(check bool) "sorted by obligation name" true (a < z)
   | _ -> Alcotest.fail "could not find obligation names in output");
  Sys.command (sprintf "rm -f %s %s" tmp1 tmp2) |> ignore

let test_meta_merge_replaces () =
  (* merge_into_meta replaces prior (proof ...) block, keeps other content. *)
  let open Proof_meta in
  let tmp = "/tmp/borge_proof_meta_merge.meta" in
  let oc = open_out tmp in
  output_string oc {|(meta "foo"
  (analyzed-at "2026-07-05T00:00:00Z"))

(proof
  (commit "old")
  (discharged-at "2026-01-01T00:00:00Z")

  (obligation old-prop
    (witness "old.v")
    (prover coq)
    (verdict fail)
    (admitted 0)
    (representation "old_rep.v")))
|};
  close_out oc;
  let new_block = {
    commit = "new5678";
    discharged_at = "2026-07-05T02:00:00Z";
    obligations = [
      { name = "ownership-consistency"; witness = "proof/db_auth.v";
        prover = "coq"; verdict = Pass { admitted = 0 };
        representation = "proof/BorgeSchema.v" };
    ];
  } in
  merge_into_meta ~path:tmp new_block;
  let ic = open_in tmp in
  let content = read_whole_ic ic in
  close_in ic;
  (* The old (proof ... old) block should be gone; new one present. *)
  (* The old (proof ...) block with commit "old" should be gone;
     the new block with commit "new5678" should be present.
     The (meta "foo") block is unrelated and should be preserved. *)
  let has_old_commit = (find_sub content "commit \"old\"" <> None) in
  let has_new_commit = (find_sub content "new5678" <> None) in
  Alcotest.(check bool) "old proof block gone" false has_old_commit;
  Alcotest.(check bool) "new proof block present" true has_new_commit;
  let read_back = read_block ~path:tmp in
  (match read_back with
   | Some rb ->
       Alcotest.(check string) "merged commit is new" "new5678" rb.commit;
       (match rb.obligations with
        | [o] -> Alcotest.(check string) "merged obligation name" "ownership-consistency" o.name
        | _ -> Alcotest.fail "expected 1 obligation after merge")
   | None -> Alcotest.fail "read_block returned None after merge");
  Sys.command (sprintf "rm -f %s" tmp) |> ignore

let () =
  Alcotest.run "proof_emit" [
    "emit", [
      "table inductive", `Quick, test_emits_table_inductive;
      "owner_of", `Quick, test_emits_owner_of;
      "permitted", `Quick, test_emits_permitted;
      "is_admin", `Quick, test_emits_is_admin;
      "predicate column", `Quick, test_emits_predicate_column;
    ];
    "run", [
      "prover absent handled", `Quick, test_run_prover_absent;
      "prover discharges witness", `Quick, test_run_prover_discharges_witness;
    ];
    "meta", [
      "round trip", `Quick, test_meta_round_trip;
      "determinism + sort", `Quick, test_meta_determinism;
      "merge replaces prior", `Quick, test_meta_merge_replaces;
    ];
  ]
