(* Parser for .borge-bug files *)

open Bug_ast
open Borge_lang.Ast

(* agent note (|
 *   WHAT: Extract the text content of a value sexp, accepting a bare
 *   symbol (Atom), a quoted string (String/Quoted), or a verbatim
 *   string (String/Verbatim). Returns None for lists or other shapes.
 *
 *   WHY: .borge-bug files store some values as bare symbols (status,
 *   created, filed-by) and others as quoted strings (id, title) or
 *   verbatim strings (doc). The parser must accept all three or it
 *   silently drops the value and falls back to empty_bug. That fallback
 *   is the root cause of `borge issue list/show` returning blank id/title
 *   and `borge issue close` writing to a .borge-bug file with an empty id
 *   (BORGE-1783789294). |) *)
let string_of sexp =
  match sexp with
  | Atom (_, s)
  | String (_, Quoted { q_content = s })
  | String (_, Verbatim { v_content = s }) -> Some s
  | _ -> None

let parse_status s =
  match status_of_string s with
  | Some st -> st
  | None -> Triage

let parse_resolution = function
  | List (_, sexps) ->
      let get_field name =
        List.find_map (function
          | List (_, [Atom (_, n); v]) when n = name -> string_of v
          | _ -> None
        ) sexps
      in
      (match get_field "when", get_field "how", get_field "by" with
       | Some when_, Some how, Some by -> Some { when_; how; by }
       | _ -> None)
  | _ -> None

let parse_bug sexp =
  match sexp with
  | List (_, Atom (_, "borge-bug") :: List (_, [Atom (_, "id"); id_node]) :: rest) ->
      (match string_of id_node with
       | None -> empty_bug
       | Some id ->
           let bug = { empty_bug with id } in
           List.fold_left (fun bug sexp ->
             match sexp with
             | List (_, [Atom (_, "title"); v]) ->
                 (match string_of v with Some s -> { bug with title = s } | None -> bug)
             | List (_, [Atom (_, "status"); v]) ->
                 (match string_of v with Some s -> { bug with status = parse_status s } | None -> bug)
             | List (_, [Atom (_, "created"); v]) ->
                 (match string_of v with Some s -> { bug with created = s } | None -> bug)
             | List (_, [Atom (_, "filed-by"); v]) ->
                 (match string_of v with Some s -> { bug with filed_by = s } | None -> bug)
             | List (_, [Atom (_, "doc"); v]) ->
                 (match string_of v with Some s -> { bug with doc = s } | None -> bug)
             | List (_, [Atom (_, "affects"); List (_, [Atom (_, "section"); v])]) ->
                 (match string_of v with Some s -> { bug with affects_section = Some s } | None -> bug)
             | List (_, [Atom (_, "relates-to"); List (_, [Atom (_, "bug"); v])]) ->
                 (match string_of v with Some s -> { bug with relates_to = s :: bug.relates_to } | None -> bug)
             | List (_, [Atom (_, "drift-report-id"); v]) ->
                 (match string_of v with Some s -> { bug with drift_report_id = Some s } | None -> bug)
             | List (_, Atom (_, "resolution") :: res_fields) ->
                 { bug with resolution =
                   parse_resolution (List ({ line = 0; col = 0; offset = 0 }, res_fields)) }
             | _ -> bug
           ) bug rest)
  | _ -> empty_bug

let parse_file path =
  let content = File_utils.read_file path in
  match Borge_lang.Parse.parse content with
  | file -> 
      (match file.Borge_lang.Ast.top_level with
       | [] -> empty_bug
       | first :: _ -> parse_bug first.node)
  | exception _ -> empty_bug
