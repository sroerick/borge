(* Parser for .borge-bug files *)

open Bug_ast
open Borge_lang.Ast

let extract_string sexp =
  match sexp with
  | Atom (_, s) -> s
  | _ -> ""

let extract_sexp_list sexp =
  match sexp with
  | List (_, l) -> l
  | _ -> []

let parse_status s =
  match status_of_string s with
  | Some st -> st
  | None -> Triage

let parse_resolution = function
  | List (_, sexps) ->
      let fields = List.filter_map (function
        | List (_, [Atom (_, "when"); Atom (_, v)]) -> Some ("when", v)
        | List (_, [Atom (_, "how"); Atom (_, v)]) -> Some ("how", v)
        | List (_, [Atom (_, "by"); Atom (_, v)]) -> Some ("by", v)
        | _ -> None
      ) sexps in
      let find f = try Some (List.assoc f fields) with Not_found -> None in
      (match find "when", find "how", find "by" with
       | Some when_, Some how, Some by -> Some { when_; how; by }
       | _ -> None)
  | _ -> None

let parse_bug sexp =
  match sexp with
  | List (_, Atom (_, "borge-bug") :: List (_, [Atom (_, "id"); Atom (_, id)]) :: rest) ->
      let bug = { empty_bug with id } in
      List.fold_left (fun bug sexp ->
        match sexp with
        | List (_, [Atom (_, "title"); Atom (_, title)]) ->
            { bug with title }
        | List (_, [Atom (_, "status"); Atom (_, status)]) ->
            { bug with status = parse_status status }
        | List (_, [Atom (_, "created"); Atom (_, created)]) ->
            { bug with created }
        | List (_, [Atom (_, "filed-by"); Atom (_, filed_by)]) ->
            { bug with filed_by }
        | List (_, [Atom (_, "doc"); Atom (_, doc)]) ->
            { bug with doc }
        | List (_, [Atom (_, "affects"); List (_, [Atom (_, "section"); Atom (_, section)])]) ->
            { bug with affects_section = Some section }
        | List (_, [Atom (_, "relates-to"); List (_, [Atom (_, "bug"); Atom (_, bug_id)])]) ->
            { bug with relates_to = bug_id :: bug.relates_to }
        | List (_, [Atom (_, "drift-report-id"); Atom (_, drift_id)]) ->
            { bug with drift_report_id = Some drift_id }
        | List (_, Atom (_, "resolution") :: res_fields) ->
            { bug with resolution = parse_resolution (List ({ line = 0; col = 0; offset = 0 }, res_fields)) }
        | _ -> bug
      ) bug rest
  | _ -> empty_bug

let parse_file path =
  let content = File_utils.read_file path in
  match Borge_lang.Parse.parse content with
  | file -> 
      (match file.Borge_lang.Ast.top_level with
       | [] -> empty_bug
       | first :: _ -> parse_bug first.node)
  | exception _ -> empty_bug
