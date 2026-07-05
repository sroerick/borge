(** Proof representation emitter.

    Takes a [Db_ast.db_app] (the typed DB spec AST) and emits a Coq
    module string representing the schema and permission model. This
    emitted file is the "representation" that a human-authored witness
    (.v file) is checked against — the witness imports this module and
    proves theorems about the [permitted] predicate and [owner_of]
    declarations.

    The emitter produces ONLY the representation, never the theorem or
    proof. Per docs/engine.borg subsection `obligations`: borge does not
    author the witness; writing the proof is authorial work. The
    emitter gives the proof author the inductives and facts to work
    against.

    See lib/proof/proof.borg subsection `proof-emit` for the spec. *)

open Printf
open Db_ast

(* agent note (|
 *   WHAT: Sanitize a borge identifier (table name, column name, group
 *   name) into a Coq-compatible identifier. Hyphens become underscores;
 *   a leading digit (rare but legal in borge groups) is prefixed with
 *   an underscore.
 *   WHY: Coq identifiers can't contain hyphens or start with digits.
 *   The .borg format allows kebab-case names that Coq would reject.
 * |) *)
let coq_ident name =
  let s = String.map (fun c -> if c = '-' then '_' else c) name in
  if String.length s > 0 then
    match s.[0] with
    | '0' .. '9' -> "_" ^ s
    | _ -> s
  else s

(* agent note (|
 *   WHAT: Capitalize the first character of a string (Coq constructor
 *   naming convention: Table_users, Group_member, etc.).
 * |) *)
let cap s =
  if String.length s = 0 then s
  else String.capitalize_ascii s

(* agent note (|
 *   WHAT: Emit the Inductive table : Type := ... declaration, one
 *   constructor per table in db_app.tables. Returns the constructor
 *   list (sanitized names) for use by downstream emitters that need to
 *   reference tables (column_of, owner_of, permitted).
 *   WHY: The witness case-analyzes over tables; this enumeration is
 *   the closed set the ownership-consistency theorem quantifies over.
 * |) *)
let emit_tables app =
  let ctors = List.map (fun (t : table_def) ->
    "  | " ^ cap (coq_ident t.name)
  ) app.tables in
  let body = String.concat "\n" ctors in
  let decl = sprintf "Inductive table : Type :=\n%s.\n" body in
  let ctor_names = List.map (fun (t : table_def) ->
    cap (coq_ident t.name)
  ) app.tables in
  decl, ctor_names

(* agent note (|
 *   WHAT: Emit the column_of inductive — one constructor per
 *   (table, column) pair. Constructor name: Col_<table>_<column>.
 *   WHY: Lets the witness prove "no group's where-predicate references
 *   a column the table does not declare" by case-analysis on column_of.
 * |) *)
let emit_columns app =
  let pairs = List.concat_map (fun (t : table_def) ->
    List.map (fun (c : column) ->
      (coq_ident t.name, coq_ident c.name)
    ) t.columns
  ) app.tables in
  let ctors = List.map (fun (t, c) ->
    sprintf "  | Col_%s_%s : column_of (%s)" t c (cap t)
  ) pairs in
  match ctors with
  | [] -> "(* no columns declared *)\n", []
  | _ ->
    let body = String.concat "\n" ctors in
    sprintf "Inductive column_of : table -> Type :=\n%s.\n" body,
    List.map (fun (t, c) -> sprintf "Col_%s_%s" t c) pairs

(* agent note (|
 *   WHAT: Emit owner_of as a Coq Definition matching the (ownership ...)
 *   declarations. Returns Some "column_name" for tables that declare
 *   ownership, None otherwise.
 *   WHY: This is the ownership fact the ownership-consistency theorem
 *   checks against — every permitted non-admin operation must have
 *   its where-predicate column match owner_of for that table.
 * |) *)
let emit_owner_of app =
  let cases = List.map (fun (t : table_def) ->
    let ctor = cap (coq_ident t.name) in
    match t.ownership with
    | Some col -> sprintf "  | %s => Some \"%s\"" ctor col
    | None -> sprintf "  | %s => None" ctor
  ) app.tables in
  match cases with
  | [] -> "Definition owner_of (t : table) : option string := None.\n"
  | _ ->
    let body = String.concat "\n" cases in
    sprintf "Definition owner_of (t : table) : option string :=\n  match t with\n%s\n  end.\n" body

(* agent note (|
 *   WHAT: Emit the group inductive, one constructor per group.
 *   Records which groups are admin (can-all) as a separate
 *   Inductive is_admin : group -> Prop with one constructor per
 *   can-all group.
 *   WHY: The ownership-consistency theorem's escape hatch is "or G is
 *   admin/can-all." is_admin encodes that fact.
 * |) *)
let emit_groups app =
  let ctors = List.map (fun (g : group_def) ->
    "  | " ^ cap (coq_ident g.name)
  ) app.groups in
  let decl = match ctors with
    | [] -> "Inductive group : Type :=\n  | Group_empty.\n"
    | _ -> sprintf "Inductive group : Type :=\n%s.\n" (String.concat "\n" ctors)
  in
  (* is_admin: one fact per can-all group. Empty if no can-all groups. *)
  let admin_ctors = List.filter_map (fun (g : group_def) ->
    if g.can_all then
      Some (sprintf "  | is_admin_%s : is_admin %s"
              (cap (coq_ident g.name)) (cap (coq_ident g.name)))
    else None
  ) app.groups in
  let admin_decl = match admin_ctors with
    | [] -> "Inductive is_admin : group -> Prop := .\n"
    | _ -> sprintf "Inductive is_admin : group -> Prop :=\n%s.\n" (String.concat "\n" admin_ctors)
  in
  decl ^ "\n" ^ admin_decl

(* agent note (|
 *   WHAT: Emit the permitted inductive — one constructor per
 *   (group, capability) pair. Each (can TBL (ops...) (where CLAUSE))
 *   in the spec becomes one permitted constructor per operation in
 *   ops. can-all groups already have is_admin and don't need entries
 *   here; the witness uses is_admin to discharge their cases.
 *   The where-predicate column (if present) is recorded as a string
 *   field on the constructor: Permitted_<group>_<op>_<table> :
 *   permitted <Group> <Op> <Table> where the predicate column is
 *   referenced in the constructor's notional signature as a string.
 *
 *   ACTUALLY: for the ownership-consistency property, the witness
 *   needs to recover which column the where-predicate references.
 *   We emit it as a Definition predicate_column_of : group ->
 *   operation -> table -> option string, parallel to owner_of,
 *   rather than embedding it in the permitted constructors. This
 *   keeps permitted a pure Prop and the predicate-column recovery a
 *   separate computation the witness can case-analyze.
 * |) *)
let emit_permitted app =
  (* permitted: one constructor per (group, capability, op) triple,
     skipping can-all groups (they go through is_admin). *)
  let ctors = List.concat_map (fun (g : group_def) ->
    if g.can_all then []
    else
      List.concat_map (fun (capb : capability) ->
        List.map (fun op ->
          let gname = cap (coq_ident g.name) in
          let tname = cap (coq_ident capb.table) in
          let op_ctor = cap op in
          sprintf "  | permitted_%s_%s_%s : permitted %s %s %s"
            (cap (coq_ident g.name)) (cap op) (cap (coq_ident capb.table))
            gname op_ctor tname
        ) capb.operations
      ) g.capabilities
  ) app.groups in
  let permitted_decl = match ctors with
    | [] ->
      (* If no permitted constructors, declare the type with an empty
         body so the witness can still reference it. *)
      "Inductive permitted : group -> operation -> table -> Prop := .\n"
    | _ ->
      sprintf "Inductive permitted : group -> operation -> table -> Prop :=\n%s.\n"
        (String.concat "\n" ctors)
  in
  (* predicate_column_of: parallel Definition recovering the where-clause
     column for a given (group, op, table). Returns None for capabilities
     without a where-clause. *)
  let pred_cases = List.concat_map (fun (g : group_def) ->
    if g.can_all then []
    else
      List.concat_map (fun (capb : capability) ->
        List.map (fun op ->
          let gname = cap (coq_ident g.name) in
          let tname = cap (coq_ident capb.table) in
          let op_ctor = cap op in
          match capb.where_clause with
          | Some clause ->
              (* Heuristic: extract the column name from "<col> = current-user".
                 Borge DB where-clauses for ownership are conventionally
                 "<column> = current-user". We extract the column name
                 verbatim; if the clause doesn't match that shape, we record
                 the whole clause as the "column" (the witness will then
                 need to handle it explicitly, which surfaces the mismatch). *)
              let col =
                (* The where_clause is a space-joined string of the atoms
                   borge's lexer kept. (The lexer currently drops `=`
                   from `col = current-user`, so the string may look
                   like "assignee-id current-user".) The column is the
                   first token in either case; extract it. *)
                match String.index_opt clause '=' with
                | Some i ->
                    let prefix = String.sub clause 0 i in
                    let trimmed = String.trim prefix in
                    if trimmed = "" then clause else trimmed
                | None ->
                    (* No `=` in the string (lexer-dropped or genuinely
                       absent). Take the first whitespace-separated token
                       as the column name. *)
                    (match String.index_opt clause ' ' with
                     | Some i -> String.sub clause 0 i
                     | None -> clause)
              in
              Some (sprintf "  | %s, %s, %s => Some \"%s\"" gname op_ctor tname col)
          | None ->
              Some (sprintf "  | %s, %s, %s => None" gname op_ctor tname)
        ) capb.operations
      ) g.capabilities
  ) app.groups in
  let pred_decl = match pred_cases with
    | [] -> "Definition predicate_column_of (g : group) (op : operation) (t : table) : option string := None.\n"
    | _ ->
      sprintf
        "Definition predicate_column_of (g : group) (op : operation) (t : table) : option string :=\n  match g, op, t with\n%s\n  | _, _, _ => None\n  end.\n"
        (String.concat "\n" (List.filter_map (fun x -> x) pred_cases))
  in
  permitted_decl ^ "\n" ^ pred_decl

(* agent note (|
 *   WHAT: Top-level emitter. Composes all the above into a single Coq
 *   module string. The module name is fixed (BorgeSchema) so witnesses
 *   can import it deterministically regardless of which .borg file was
 *   emitted from.
 *   WHY: A fixed module name means a witness file's `Require Import
 *   BorgeSchema.` line is stable; changing which spec is being proved
 *   doesn't require editing the witness's import.
 * |) *)
let emit_representation (app : db_app) : string =
  let _tables_decl, _ctors = emit_tables app in
  let columns_decl, _ = emit_columns app in
  let owner_decl = emit_owner_of app in
  let groups_decl = emit_groups app in
  let permitted_decl = emit_permitted app in
  (* operation inductive is fixed — borge DB operations are create/read/update/delete. *)
  let operation_decl =
    "Inductive operation : Type :=\n  | Create | Read | Update | Delete.\n"
  in
  sprintf
    {|
(* Auto-generated by borge proof_emit. Do not edit by hand.
   This is the representation file a witness (.v) is checked against.
   Regenerate with: borge proof emit --spec DB_SPEC.borg

   Per docs/engine.borg subsection `obligations`: borge records what
   was claimed and binds it to a commit. This file is the machine-
   readable form of the spec a proof was discharged against. *)
From Stdlib Require Import String.
Open Scope string_scope.

%s
%s
%s
%s
%s
%s
|}
    operation_decl
    _tables_decl
    columns_decl
    owner_decl
    groups_decl
    permitted_decl

(* agent note (|
 *   WHAT: Write the emitted representation to a file. Creates parent
 *   directories if missing (so `proof/db_auth_schema.v` works without
 *   the caller pre-creating proof/).
 *   WHY: The path is recorded in .borg.meta as the `representation`
 *   field; a reviewer reading the proof block can find the exact
 *   emitted file a verdict was checked against.
 * |) *)
let emit_to_file (app : db_app) ~path =
  let content = emit_representation app in
  let dir = Filename.dirname path in
  (if dir <> "" && dir <> "." && not (Sys.file_exists dir) then
     Sys.command (sprintf "mkdir -p %s" dir) |> ignore);
  let oc = open_out path in
  output_string oc content;
  close_out oc
