open Borge_lang.Ast

type node_info = {
  pos : pos;
  kind : string;           (** "Atom", "String", "List" *)
  keyword : string option;  (** For List: first Atom's name, if any *)
  child_count : int;        (** For List: number of children *)
  value : string option;    (** For Atom: the symbol; for String: truncated content *)
}

let rec collect_nodes indent sexp =
  let info = node_info_of_sexp sexp indent in
  let children = child_nodes sexp in
  info :: List.concat_map (collect_nodes (indent + 1)) children

and node_info_of_sexp sexp _indent =
  match sexp with
  | Atom (p, sym) ->
      { pos = p; kind = "Atom"; keyword = None; child_count = 0; value = Some sym }
  | String (p, sv) ->
      let val_str = string_value_summary sv in
      { pos = p; kind = "String"; keyword = None; child_count = 0; value = Some val_str }
  | List (p, children) ->
      let keyword = list_keyword children in
      { pos = p; kind = "List"; keyword; child_count = List.length children; value = None }

and child_nodes = function
  | Atom _ | String _ -> []
  | List (_, children) -> children

and list_keyword = function
  | Atom (_, k) :: _ -> Some k
  | _ -> None

and string_value_summary = function
  | Quoted q ->
      let s = q.q_content in
      if String.length s > 30 then String.sub s 0 27 ^ "..." else s
  | Verbatim v ->
      let s = v.v_content in
      if String.length s > 30 then String.sub s 0 27 ^ "...|" else s ^ "|"

type result = {
  file_path : string;
  nodes : node_info list;
}

let run path =
  let input = File_utils.read_file path in
  let file = Borge_lang.Parse.parse_file input in
  let nodes = List.concat_map (fun { node; _ } ->
    collect_nodes 0 node
  ) file.top_level in
  { file_path = path; nodes }

let format_node indent info =
  let pos_str = Printf.sprintf "%d:%d" info.pos.line info.pos.col in
  let indent_str = String.make (indent * 2) ' ' in
  match info.kind, info.keyword, info.value with
  | "Atom", _, Some sym ->
      Printf.sprintf "%s%-5s  Atom %s" indent_str pos_str sym
  | "Atom", _, None ->
      Printf.sprintf "%s%-5s  Atom" indent_str pos_str
  | "String", _, Some v ->
      Printf.sprintf "%s%-5s  String %S" indent_str pos_str v
  | "String", _, None ->
      Printf.sprintf "%s%-5s  String" indent_str pos_str
  | "List", Some kw, _ ->
      Printf.sprintf "%s%-5s  List \"%s\" [%d children]" indent_str pos_str kw info.child_count
  | "List", None, _ ->
      Printf.sprintf "%s%-5s  List [%d children]" indent_str pos_str info.child_count
  | _ ->
      Printf.sprintf "%s%-5s  %s" indent_str pos_str info.kind

let format_result result =
  let buf = Buffer.create 4096 in
  List.iter (fun info ->
    Buffer.add_string buf (format_node 0 info);
    Buffer.add_char buf '\n'
  ) result.nodes;
  Buffer.contents buf
