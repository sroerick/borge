(* Test proof_emit: Coq representation emission from db_app. *)
open Printf
open Borge_lib

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
  ]
