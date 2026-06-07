open Db_ast
(** Extract typed DB AST from generic sexp trees.

    Walks the Ast.sexp tree and extracts database-specific typed
    nodes: tables, columns, operations, relations, ownership, groups. *)

open Borge_lang.Ast

let atom_name = function
  | Atom (_, name) -> Some name
  | _ -> None

let string_value = function
  | String (_, Quoted { q_content }) -> Some q_content
  | _ -> None

(** Parse column constraints from a column body *)
let parse_constraint = function
  | Atom (_, "primary-key") -> Some Primary_key
  | Atom (_, "unique") -> Some Unique
  | Atom (_, "not-null") -> Some Not_null
  | List (_, [Atom (_, "default"); Atom (_, expr)]) ->
    Some (Default expr)
  | List (_, [Atom (_, "default"); String (_, Quoted { q_content })]) ->
    Some (Default q_content)
  | List (_, [Atom (_, "references"); Atom (_, ref_spec)]) ->
    Some (References ref_spec)
  | _ -> None

(** Parse a (column name (type T) ...) form *)
let parse_column = function
  | List (_, Atom (_, "column") :: Atom (_, name) :: rest) ->
    let typ = List.find_map (function
      | List (_, [Atom (_, "type"); Atom (_, t)]) -> Some t
      | _ -> None
    ) rest in
    let constraints = List.filter_map parse_constraint rest in
    Some { name; typ = Option.value typ ~default:"text"; constraints }
  | _ -> None

(** Parse index method *)
let parse_index_method = function
  | Atom (_, "btree") -> Some Btree
  | Atom (_, "gin") -> Some Gin
  | Atom (_, "gist") -> Some Gist
  | Atom (_, "hash") -> Some Hash
  | Atom (_, "spgist") -> Some Spgist
  | Atom (_, "brin") -> Some Brin
  | _ -> None

(** Parse an (index name ...) form *)
let parse_index = function
  | List (_, Atom (_, "index") :: Atom (_, name) :: rest) ->
    let columns = List.find_map (function
      | List (_, Atom (_, "columns") :: cols) ->
        Some (List.filter_map atom_name cols)
      | Atom (_, col) -> Some [col] (* single column shorthand *)
      | _ -> None
    ) rest in
    let idx_method = List.find_map (function
      | List (_, [Atom (_, "method"); m]) -> parse_index_method m
      | _ -> None
    ) rest in
    let unique = List.exists (function
      | Atom (_, "unique") -> true | _ -> false
    ) rest in
    Some { name; columns = Option.value columns ~default:[]; idx_method; unique }
  | _ -> None

(** Parse a (table name ...) form *)
let parse_table = function
  | List (_, Atom (_, "table") :: Atom (_, name) :: rest) ->
    let columns = List.filter_map parse_column rest in
    let indexes = List.filter_map parse_index rest in
    let ownership = List.find_map (function
      | List (_, [Atom (_, "ownership"); Atom (_, col)]) -> Some col
      | _ -> None
    ) rest in
    Some { name; columns; indexes; ownership }
  | _ -> None

(** Parse CRUD specification *)
let parse_crud = function
  | List (_, [Atom (_, "crud")]) ->
    Some Crud_all
  | List (_, [Atom (_, "crud"); List (_, Atom (_, "except") :: ops)]) ->
    let names = List.filter_map atom_name ops in
    Some (Crud_except names)
  | List (_, [Atom (_, "crud"); List (_, Atom (_, "only") :: ops)]) ->
    let names = List.filter_map atom_name ops in
    Some (Crud_only names)
  | _ -> None

(** Parse a (query name ...) form *)
let parse_query = function
  | List (_, Atom (_, "query") :: Atom (_, name) :: rest) ->
    let limit = List.find_map (function
      | List (_, [Atom (_, "limit"); Atom (_, n)]) ->
        int_of_string_opt n
      | _ -> None
    ) rest in
    Some { name; limit }
  | _ -> None

(** Parse an (operations table-name ...) form *)
let parse_operations = function
  | List (_, Atom (_, "operations") :: Atom (_, table_name) :: rest) ->
    let crud = List.find_map (fun s ->
      Option.map (fun c -> c) (parse_crud s)
    ) rest in
    let queries = List.filter_map parse_query rest in
    Some { table_name; crud; queries }
  | _ -> None

(** Parse a join clause *)
let parse_join ~expected_keyword = function
  | List (_, [Atom (_, kw); Atom (_, table); List (_, [Atom (_, "on"); Atom (_, cond)])])
    when kw = expected_keyword ->
    let join_type = match kw with
      | "join" -> Inner_join
      | "left-join" -> Left_join
      | _ -> Inner_join
    in
    Some { join_type; table; on_condition = cond }
  | _ -> None

(** Parse a (relation name ...) form *)
let parse_relation = function
  | List (_, Atom (_, "relation") :: Atom (_, name) :: rest) ->
    let from_table = List.find_map (function
      | List (_, [Atom (_, "from"); Atom (_, t)]) -> Some t
      | _ -> None
    ) rest in
    let joins = List.filter_map (fun s ->
      match parse_join ~expected_keyword:"join" s with
      | Some j -> Some j
      | None -> parse_join ~expected_keyword:"left-join" s
    ) rest in
    let where_clause = List.find_map (function
      | List (_, Atom (_, "where") :: [Atom (_, cond)]) -> Some cond
      | _ -> None
    ) rest in
    let select_columns = List.find_map (function
      | List (_, Atom (_, "select") :: cols) ->
        Some (List.filter_map atom_name cols)
      | _ -> None
    ) rest in
    let queries = List.filter_map parse_query rest in
    Some { name; from_table = Option.value from_table ~default:"";
           joins; where_clause; select_columns = Option.value select_columns ~default:[];
           queries }
  | _ -> None

(** Parse a (can TABLE OPS (where COND)) capability *)
let parse_capability = function
  | List (_, Atom (_, "can") :: Atom (_, table) :: rest) ->
    let ops = List.filter_map (function
      | Atom (_, op) when List.mem op ["create"; "read"; "update"; "delete"] ->
        Some op
      | _ -> None
    ) rest in
    let where_clause = List.find_map (function
      | List (_, [Atom (_, "where"); Atom (_, cond)]) -> Some cond
      | _ -> None
    ) rest in
    Some { table; operations = ops; where_clause }
  | Atom (_, "can-all") ->
    (* can-all is handled at the group level *)
    None
  | _ -> None

(** Parse a (group name ...) form *)
let parse_group = function
  | List (_, Atom (_, "group") :: Atom (_, name) :: rest) ->
    let can_all = List.exists (function
      | List (_, [Atom (_, "can-all")]) -> true
      | _ -> false
    ) rest in
    let capabilities = List.filter_map parse_capability rest in
    Some { name; can_all; capabilities }
  | _ -> None

(** Parse a complete DB application from a (db app-name ...) form *)
let parse_db_app = function
  | List (_, Atom (_, "db") :: _ :: body) ->
    let tables = List.filter_map parse_table body in
    let operations = List.filter_map parse_operations body in
    let relations = List.filter_map parse_relation body in
    let groups =
      List.concat_map (function
        | List (_, Atom (_, "groups") :: entries) ->
          List.filter_map parse_group entries
        | _ -> []
      ) body
    in
    Some { tables; operations; relations; groups }
  | _ -> None

(** Parse all DB forms from a file's top-level sexps *)
let parse_file (file : Borge_lang.Ast.file) : db_app option =
  let rec walk = function
    | [] -> None
    | { node; _ } :: rest ->
      (match parse_db_app node with
       | Some app -> Some app
       | None -> walk rest)
  in
  match walk file.top_level with
  | Some app -> Some app
  | None ->
    let sexps = List.map (fun swc -> swc.node) file.top_level in
    let tables = List.filter_map parse_table sexps in
    let operations = List.filter_map parse_operations sexps in
    let relations = List.filter_map parse_relation sexps in
    let groups = List.concat_map (function
      | List (_, Atom (_, "groups") :: entries) ->
        List.filter_map parse_group entries
      | _ -> []
    ) sexps in
    Some { tables; operations; relations; groups }
