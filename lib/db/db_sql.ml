(** SQL migration generator for borge DB specs.

    Generates PostgreSQL DDL from a db_app:
    - CREATE TABLE with columns, constraints, defaults
    - Foreign key constraints
    - Indexes
    - RLS policies from ownership and groups
    - Role-based GRANT statements *)

open Db_ast

(** Escape an identifier (simple quoting) *)
let ident (s : string) : string =
  if String.contains s '-' then
    "\"" ^ String.map (fun c -> if c = '-' then '_' else c) s ^ "\""
  else if s = "" then "\"_\""
  else s

(** Convert sexp-style name to snake_case SQL identifier *)
let sql_name (s : string) : string =
  ident (String.map (fun c -> if c = '-' then '_' else c) s)

(** Map borge type names to PostgreSQL types *)
let pg_type (t : string) : string =
  match String.lowercase_ascii t with
  | "uuid" -> "uuid"
  | "text" -> "text"
  | "integer" | "int" -> "integer"
  | "bigint" -> "bigint"
  | "boolean" | "bool" -> "boolean"
  | "timestamptz" -> "timestamptz"
  | "timestamp" -> "timestamp"
  | "date" -> "date"
  | "jsonb" -> "jsonb"
  | "json" -> "json"
  | "serial" -> "serial"
  | "bigserial" -> "bigserial"
  | "numeric" | "decimal" -> "numeric"
  | "real" | "float4" -> "real"
  | "double precision" | "float8" -> "double precision"
  | "bytea" -> "bytea"
  | "varchar" -> "varchar"
  | _ -> t  (* pass through — user may specify custom types *)

(** Generate column constraints as SQL fragments *)
let column_constraints (col : column) : string list =
  List.filter_map (function
    | Primary_key -> Some "PRIMARY KEY"
    | Unique -> Some "UNIQUE"
    | Not_null -> Some "NOT NULL"
    | Default expr -> Some (Printf.sprintf "DEFAULT %s" expr)
    | References ref ->
      (* Convert "users.id" to REFERENCES "users"("id") *)
      (match String.split_on_char '.' ref with
       | [tbl; col'] ->
         Some (Printf.sprintf "REFERENCES %s(%s)" (sql_name tbl) (sql_name col'))
       | _ ->
         Some (Printf.sprintf "REFERENCES %s" ref))
  ) col.constraints

(** Generate a single column DDL line *)
let column_ddl (col : column) : string =
  let typ = pg_type col.typ in
  let constraints = String.concat " " (column_constraints col) in
  if constraints = "" then
    Printf.sprintf "  %s %s" (sql_name col.name) typ
  else
    Printf.sprintf "  %s %s %s" (sql_name col.name) typ constraints

(** Generate CREATE TABLE statement *)
let create_table (t : table_def) : string =
  let columns = List.map column_ddl t.columns in
  let body = String.concat ",\n" columns in
  Printf.sprintf "CREATE TABLE %s (\n%s\n);" (sql_name t.name) body

(** Generate CREATE INDEX statements *)
let index_method_sql = function
  | Some Btree -> "btree"
  | Some Gin -> "gin"
  | Some Gist -> "gist"
  | Some Hash -> "hash"
  | Some Spgist -> "spgist"
  | Some Brin -> "brin"
  | None -> "btree"

let create_indexes (t : table_def) : string list =
  List.map (fun (idx : index_def) ->
    let unique = if idx.unique then "UNIQUE " else "" in
    let idx_method = index_method_sql idx.idx_method in
    let cols = String.concat ", " (List.map sql_name idx.columns) in
    Printf.sprintf "CREATE %sINDEX %s ON %s USING %s (%s);"
      unique (sql_name idx.name) (sql_name t.name) idx_method cols
  ) t.indexes

(** Generate RLS: enable RLS on a table with ownership *)
let rls_enable (t : table_def) : string option =
  match t.ownership with
  | None -> None
  | Some _ ->
    Some (Printf.sprintf "ALTER TABLE %s ENABLE ROW LEVEL SECURITY;" (sql_name t.name))

(** Generate RLS policy for ownership column.
    Creates a policy that restricts rows to those where the ownership column
    matches the current user. *)
let rls_ownership_policy (t : table_def) : string option =
  match t.ownership with
  | None -> None
  | Some col_name ->
    let policy_name = Printf.sprintf "%s_owner_policy" (String.map (fun c -> if c = '-' then '_' else c) t.name) in
    Some (Printf.sprintf
      "CREATE POLICY %s ON %s\n  USING (%s = current_user);"
      (sql_name policy_name) (sql_name t.name) (sql_name col_name))

(** Generate RLS policies from group capabilities *)
let rls_group_policies (t : table_def) (groups : group_def list) : string list =
  let table_name = t.name in
  let safe_name = String.map (fun c -> if c = '-' then '_' else c) table_name in
  List.concat_map (fun (g : group_def) ->
    if g.can_all then
      (* can-all group gets a bypass policy *)
      [Printf.sprintf
        "CREATE POLICY %s_%s_all ON %s\n  USING (true) WITH CHECK (true);"
        (sql_name safe_name) (sql_name g.name) (sql_name table_name)]
    else
      List.filter_map (fun (cap : capability) ->
        if cap.table <> table_name then None
        else begin
          let ops = String.concat ", " cap.operations in
          match cap.where_clause with
          | Some cond ->
            Some (Printf.sprintf
              "CREATE POLICY %s_%s_%s ON %s\n  FOR %s\n  USING (%s);"
              (sql_name safe_name) (sql_name g.name)
              (sql_name (String.concat "_" cap.operations))
              (sql_name table_name) ops cond)
          | None ->
            Some (Printf.sprintf
              "CREATE POLICY %s_%s_%s ON %s\n  FOR %s\n  USING (true);"
              (sql_name safe_name) (sql_name g.name)
              (sql_name (String.concat "_" cap.operations))
              (sql_name table_name) ops)
        end
      ) g.capabilities
  ) groups

(** Generate CREATE VIEW from a relation *)
let create_view (rel : relation_def) : string =
  let select_cols = String.concat ", " (List.map sql_name rel.select_columns) in
  let from = Printf.sprintf "FROM %s" (sql_name rel.from_table) in
  let joins = String.concat "\n" (List.map (fun (j : join_clause) ->
    let jt = match j.join_type with
      | Inner_join -> "JOIN"
      | Left_join -> "LEFT JOIN"
    in
    Printf.sprintf "%s %s ON %s" jt (sql_name j.table) j.on_condition
  ) rel.joins) in
  let where = match rel.where_clause with
    | Some w -> Printf.sprintf "\nWHERE %s" w
    | None -> ""
  in
  Printf.sprintf "CREATE VIEW %s AS\nSELECT %s\n%s\n%s%s;"
    (sql_name rel.name) select_cols from joins where

(** Generate CRUD function stubs for a table.
    These are PostgreSQL function stubs that can be filled in by an LLM. *)
let crud_functions (ops : operations_def) : string list =
  let t = sql_name ops.table_name in
  let base = String.map (fun c -> if c = '-' then '_' else c) ops.table_name in
  let fns = ref [] in
  (match ops.crud with
  | Some crud ->
    let all_ops = match crud with
      | Crud_all -> ["create"; "read"; "update"; "delete"]
      | Crud_except excluded ->
        List.filter (fun op -> not (List.mem op excluded))
          ["create"; "read"; "update"; "delete"]
      | Crud_only included -> included
    in
    List.iter (fun op ->
      let fn_name = Printf.sprintf "%s_%s" base op in
      let stub = match op with
        | "create" ->
          Printf.sprintf
            "CREATE OR REPLACE FUNCTION %s(\n  -- TODO: add parameters from %s columns\n)\nRETURNS UUID\nLANGUAGE sql\nAS $$\n  INSERT INTO %s DEFAULT VALUES RETURNING id;\n$$;"
            fn_name t t
        | "read" ->
          Printf.sprintf
            "CREATE OR REPLACE FUNCTION %s_by_id(\n  p_id UUID\n)\nRETURNS SETOF %s\nLANGUAGE sql\nAS $$\n  SELECT * FROM %s WHERE id = p_id;\n$$;"
            fn_name t t
        | "update" ->
          Printf.sprintf
            "CREATE OR REPLACE FUNCTION %s(\n  p_id UUID\n  -- TODO: add updatable columns from %s\n)\nRETURNS void\nLANGUAGE sql\nAS $$\n  -- TODO: UPDATE %s SET ... WHERE id = p_id;\n$$;"
            fn_name t t
        | "delete" ->
          Printf.sprintf
            "CREATE OR REPLACE FUNCTION %s(\n  p_id UUID\n)\nRETURNS void\nLANGUAGE sql\nAS $$\n  DELETE FROM %s WHERE id = p_id;\n$$;"
            fn_name t
        | _ ->
          Printf.sprintf "-- TODO: %s function for %s" op t
      in
      fns := stub :: !fns
    ) all_ops
  | None -> ());
  let query_fns = List.map (fun (q : query_def) ->
    let fn_name = Printf.sprintf "%s_%s" base q.name in
    let limit = match q.limit with
      | Some n -> Printf.sprintf "\n  LIMIT %d" n
      | None -> ""
    in
    Printf.sprintf
      "CREATE OR REPLACE FUNCTION %s(\n  -- TODO: add filter parameters\n)\nRETURNS SETOF %s\nLANGUAGE sql\nAS $$\n  SELECT * FROM %s%s;\n$$;"
      fn_name t t limit
  ) ops.queries in
  List.rev !fns @ query_fns

(** Generate the full SQL migration for a db_app *)
let generate (app : db_app) : string =
  let parts = ref [] in

  (* Create tables *)
  List.iter (fun (t : table_def) ->
    parts := create_table t :: !parts;
    (* Indexes *)
    List.iter (fun idx ->
      parts := idx :: !parts
    ) (create_indexes t);
    (* RLS enable *)
    (match rls_enable t with Some s -> parts := s :: !parts | None -> ());
    (* RLS ownership policy *)
    (match rls_ownership_policy t with Some s -> parts := s :: !parts | None -> ());
    (* RLS group policies *)
    List.iter (fun s -> parts := s :: !parts) (rls_group_policies t app.groups)
  ) app.tables;

  (* Views from relations *)
  List.iter (fun (rel : relation_def) ->
    parts := create_view rel :: !parts
  ) app.relations;

  (* CRUD function stubs *)
  List.iter (fun (ops : operations_def) ->
    List.iter (fun fn -> parts := fn :: !parts) (crud_functions ops)
  ) app.operations;

  String.concat "\n\n" (List.rev !parts) ^ "\n"
