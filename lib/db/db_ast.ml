(** Typed AST for borge DB specs.

    Defines typed representations of database-specific forms:
    tables, columns, operations, relations, ownership, and groups.
    Extraction from sexp is in Parse.ml. This module is just types. *)

(** Column constraints *)
type column_constraint =
  | Primary_key
  | Unique
  | Not_null
  | Default of string          (** default expression as string *)
  | References of string       (** table.column for foreign key *)

(** A column definition *)
type column = {
  name : string;
  typ : string;                (** PostgreSQL type name *)
  constraints : column_constraint list;
}

(** Index method *)
type index_method =
  | Btree
  | Gin
  | Gist
  | Hash
  | Spgist
  | Brin

(** An index definition *)
type index_def = {
  name : string;
  columns : string list;
  idx_method : index_method option;  (** None = default btree *)
  unique : bool;
}

(** A table definition *)
type table_def = {
  name : string;
  columns : column list;
  indexes : index_def list;
  ownership : string option;     (** column name for row-level ownership *)
}

(** CRUD specification *)
type crud_spec =
  | Crud_all                           (** full CRUD *)
  | Crud_except of string list         (** all except named operations *)
  | Crud_only of string list           (** only named operations *)

(** A named query *)
type query_def = {
  name : string;
  limit : int option;
}

(** Operations on a table *)
type operations_def = {
  table_name : string;
  crud : crud_spec option;       (** None = no CRUD *)
  queries : query_def list;
}

(** Join type *)
type join_type =
  | Inner_join
  | Left_join

(** A join clause *)
type join_clause = {
  join_type : join_type;
  table : string;
  on_condition : string;          (** raw condition string *)
}

(** A relation (cross-table query) *)
type relation_def = {
  name : string;
  from_table : string;
  joins : join_clause list;
  where_clause : string option;    (** raw where condition *)
  select_columns : string list;    (** table.column references *)
  queries : query_def list;
}

(** A capability within a group *)
type capability = {
  table : string;
  operations : string list;        (** create, read, update, delete *)
  where_clause : string option;    (** row-level filter *)
}

(** A group definition *)
type group_def = {
  name : string;
  can_all : bool;                   (** true = bypass RLS *)
  capabilities : capability list;
}

(** A complete DB application spec *)
type db_app = {
  tables : table_def list;
  operations : operations_def list;
  relations : relation_def list;
  groups : group_def list;
}
