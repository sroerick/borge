(** Semantic validation for borge DB specs.

    Checks that references are valid:
    - (references table.column) refers to an existing table
    - (ownership column) refers to an existing column on the same table
    - Group capability tables exist in the schema
    - Operations reference existing tables
    - Table, group, and column names are unique *)

open Db_ast

type issue = {
  message : string;
  severity : [ `Error | `Warning ];
}

let err msg = { message = msg; severity = `Error }
let warn msg = { message = msg; severity = `Warning }

(** Build a lookup: table name -> column name list *)
let table_columns (tables : table_def list) : (string, string list) Hashtbl.t =
  let tbl = Hashtbl.create 16 in
  List.iter (fun (t : table_def) ->
    let cols = List.map (fun (c : column) -> c.name) t.columns in
    Hashtbl.add tbl t.name cols
  ) tables;
  tbl

(** Get all table names *)
let table_names (tables : table_def list) : string list =
  List.map (fun (t : table_def) -> t.name) tables

(** Check table name uniqueness *)
let check_table_uniqueness (tables : table_def list) : issue list =
  let seen = Hashtbl.create 16 in
  List.filter_map (fun (t : table_def) ->
    if Hashtbl.mem seen t.name then
      Some (err (Printf.sprintf "duplicate table name: %s" t.name))
    else begin
      Hashtbl.add seen t.name true;
      None
    end
  ) tables

(** Check a column's references *)
let check_column (table_name : string) (col : column)
    (tbl : (string, string list) Hashtbl.t) : issue list =
  List.filter_map (function
    | References ref_spec ->
      (match String.split_on_char '.' ref_spec with
       | [ref_table; _ref_col] ->
         if not (Hashtbl.mem tbl ref_table) then
           Some (err (Printf.sprintf
             "table '%s': column '%s' references undefined table '%s'"
             table_name col.name ref_table))
         else None
       | _ ->
         Some (warn (Printf.sprintf
           "table '%s': column '%s' has unusual references format: %s"
           table_name col.name ref_spec)))
    | _ -> None
  ) col.constraints

(** Check ownership: the column must exist on the table *)
let check_ownership (table : table_def) : issue list =
  match table.ownership with
  | None -> []
  | Some col_name ->
    let col_names = List.map (fun (c : column) -> c.name) table.columns in
    if not (List.mem col_name col_names) then
      [err (Printf.sprintf "table '%s': ownership references undefined column '%s'"
        table.name col_name)]
    else []

(** Check a table definition *)
let check_table (tbl : (string, string list) Hashtbl.t) (table : table_def) : issue list =
  let col_names = List.map (fun (c : column) -> c.name) table.columns in
  let col_dup_issues =
    let seen = Hashtbl.create 8 in
    List.filter_map (fun (name : string) ->
      if Hashtbl.mem seen name then
        Some (err (Printf.sprintf "table '%s': duplicate column name '%s'" table.name name))
      else begin
        Hashtbl.add seen name true;
        None
      end
    ) col_names
  in
  let col_issues = List.concat_map
    (fun (c : column) -> check_column table.name c tbl) table.columns in
  let ownership_issues = check_ownership table in
  col_dup_issues @ col_issues @ ownership_issues

(** Check operations reference existing tables *)
let check_operations (t_names : string list) (ops : operations_def) : issue list =
  if not (List.mem ops.table_name t_names) then
    [err (Printf.sprintf "operations references undefined table: %s" ops.table_name)]
  else []

(** Check a relation's from-table and join tables *)
let check_relation (t_names : string list) (rel : relation_def) : issue list =
  let from_issues =
    if rel.from_table <> "" && not (List.mem rel.from_table t_names) then
      [err (Printf.sprintf "relation '%s': from-table '%s' is undefined"
        rel.name rel.from_table)]
    else []
  in
  let join_issues = List.filter_map (fun (j : join_clause) ->
    if not (List.mem j.table t_names) then
      Some (err (Printf.sprintf "relation '%s': join table '%s' is undefined"
        rel.name j.table))
    else None
  ) rel.joins in
  from_issues @ join_issues

(** Check group capability table references *)
let check_capability (t_names : string list) (cap : capability) : issue list =
  if not (List.mem cap.table t_names) then
    [err (Printf.sprintf "group capability references undefined table: %s" cap.table)]
  else []

(** Check group name uniqueness and capability references *)
let check_groups (t_names : string list) (groups : group_def list) : issue list =
  let name_issues =
    let seen = Hashtbl.create 8 in
    List.filter_map (fun (g : group_def) ->
      if Hashtbl.mem seen g.name then
        Some (err (Printf.sprintf "duplicate group name: %s" g.name))
      else begin
        Hashtbl.add seen g.name true;
        None
      end
    ) groups
  in
  let cap_issues = List.concat_map (fun (g : group_def) ->
    List.concat_map (check_capability t_names) g.capabilities
  ) groups in
  name_issues @ cap_issues

(** Validate the complete DB app spec *)
let validate (app : db_app) : issue list =
  let tbl = table_columns app.tables in
  let t_names = table_names app.tables in
  let table_issues = check_table_uniqueness app.tables
    @ List.concat_map (check_table tbl) app.tables in
  let ops_issues = List.concat_map (check_operations t_names) app.operations in
  let rel_issues = List.concat_map (check_relation t_names) app.relations in
  let group_issues = check_groups t_names app.groups in
  table_issues @ ops_issues @ rel_issues @ group_issues
