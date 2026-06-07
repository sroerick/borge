(** JSON serialization for borge DB specs.

    Produces Yojson-compatible JSON from the typed DB AST.
    Used by the --json flag on DB-related commands. *)

open Db_ast

(* exempt doc: simple JSON converter - name is self-documenting *)
let column_constraint_to_json = function
  | Primary_key -> `String "primary-key"
  | Unique -> `String "unique"
  | Not_null -> `String "not-null"
  | Default expr -> `Assoc [("default", `String expr)]
  | References ref -> `Assoc [("references", `String ref)]

(* exempt doc: JSON converter for column - name is self-documenting *)
let column_to_json (c : Db_ast.column) =
  `Assoc [
    ("name", `String c.name);
    ("type", `String c.typ);
    ("constraints", `List (List.map column_constraint_to_json c.constraints));
  ]

(* exempt doc: JSON converter for index - name is self-documenting *)
let index_to_json (i : Db_ast.index_def) =
  `Assoc [
    ("name", `String i.name);
    ("columns", `List (List.map (fun s -> `String s) i.columns));
    ("unique", `Bool i.unique);
  ]

(* exempt doc: JSON converter for table - name is self-documenting *)
let table_to_json (t : Db_ast.table_def) =
  `Assoc [
    ("name", `String t.name);
    ("columns", `List (List.map column_to_json t.columns));
    ("indexes", `List (List.map index_to_json t.indexes));
    ("ownership", match t.ownership with Some o -> `String o | None -> `Null);
  ]

(* exempt doc: JSON converter for CRUD spec - name is self-documenting *)
let crud_spec_to_json (spec : Db_ast.crud_spec) =
  match spec with
  | Crud_all -> `String "all"
  | Crud_except ops -> `Assoc [("except", `List (List.map (fun s -> `String s) ops))]
  | Crud_only ops -> `Assoc [("only", `List (List.map (fun s -> `String s) ops))]

(* exempt doc: JSON converter for query - name is self-documenting *)
let query_to_json (q : Db_ast.query_def) =
  `Assoc [
    ("name", `String q.name);
    ("limit", match q.limit with Some n -> `Int n | None -> `Null);
  ]

(* exempt doc: JSON converter for operations - name is self-documenting *)
let operations_to_json (o : Db_ast.operations_def) =
  `Assoc [
    ("table", `String o.table_name);
    ("crud", match o.crud with Some c -> crud_spec_to_json c | None -> `Null);
    ("queries", `List (List.map query_to_json o.queries));
  ]

(* exempt doc: JSON converter for relation - name is self-documenting *)
let relation_to_json (r : Db_ast.relation_def) =
  `Assoc [
    ("name", `String r.name);
    ("from", `String r.from_table);
    ("joins", `List (List.map (fun j ->
      `Assoc [
        ("type", `String (match j.join_type with Inner_join -> "inner" | Left_join -> "left"));
        ("table", `String j.table);
        ("on", `String j.on_condition);
      ]) r.joins));
    ("where", match r.where_clause with Some w -> `String w | None -> `Null);
    ("select", `List (List.map (fun s -> `String s) r.select_columns));
    ("queries", `List (List.map query_to_json r.queries));
  ]

(* exempt doc: JSON converter for capability - name is self-documenting *)
let capability_to_json (c : Db_ast.capability) =
  `Assoc [
    ("table", `String c.table);
    ("operations", `List (List.map (fun s -> `String s) c.operations));
    ("where", match c.where_clause with Some w -> `String w | None -> `Null);
  ]

(* exempt doc: JSON converter for group - name is self-documenting *)
let group_to_json (g : Db_ast.group_def) =
  `Assoc [
    ("name", `String g.name);
    ("can_all", `Bool g.can_all);
    ("capabilities", `List (List.map capability_to_json g.capabilities));
  ]

let db_app_to_json (app : Db_ast.db_app) =
  `Assoc [
    ("tables", `List (List.map table_to_json app.tables));
    ("operations", `List (List.map operations_to_json app.operations));
    ("relations", `List (List.map relation_to_json app.relations));
    ("groups", `List (List.map group_to_json app.groups));
  ]
