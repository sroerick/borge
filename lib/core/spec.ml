open Borge_lang.Ast

type status =
  | Planned
  | In_progress
  | Partial
  | Implemented
  | Drifted
  | Blank

(* (fn status_of_string
      (doc "Convert string to status type, or None if invalid")
      (since "v1.0")) *)
let status_of_string = function
  | "planned" -> Some Planned
  | "in-progress" -> Some In_progress
  | "partial" -> Some Partial
  | "implemented" -> Some Implemented
  | "drifted" -> Some Drifted
  | "blank" -> Some Blank
  | _ -> None

(* (fn string_of_status
      (doc "Convert status type back to string representation")
      (since "v1.0")) *)
let string_of_status = function
  | Planned -> "planned"
  | In_progress -> "in-progress"
  | Partial -> "partial"
  | Implemented -> "implemented"
  | Drifted -> "drifted"
  | Blank -> "blank"

(* (fn project_name
      (doc "Extract project name from a borg file AST")
      (since "v1.0")) *)
let project_name (file : file) =
  match file.top_level with
  | [] -> None
  | sexp_with_comments :: _ ->
      match sexp_with_comments.node with
      | List (_, Atom (_, "project") :: Atom (_, name) :: _) -> Some name
      | _ -> None

(* (fn count_sections
      (doc "Count all sections in a borg file, recursively")
      (since "v1.0")) *)
let rec count_sections sexp =
  match sexp with
  | List (_, Atom (_, "section") :: _) -> 1
  | List (_, items) -> List.fold_left (fun acc item -> acc + count_sections item) 0 items
  | _ -> 0

(* (fn statuses
      (doc "Extract all status values from a borg file")
      (since "v1.0")) *)
let statuses (file : file) =
  let rec collect sexp acc =
    match sexp with
    | List (_, Atom (_, "status") :: Atom (_, status) :: _) -> status :: acc
    | List (_, items) -> List.fold_left (fun a i -> collect i.node a) acc items
    | _ -> acc
  in
  List.fold_left (fun acc (sexp, _) -> collect sexp.node acc) [] file.top_level

(* (fn status_counts
      (doc "Count occurrences of each status value")
      (since "v1.0")) *)
let status_counts (file : file) =
  let stats = statuses file in
  let counts = ["planned", 0; "in-progress", 0; "partial", 0; "implemented", 0; "drifted", 0; "blank", 0] in
  List.map (fun (status, _) ->
    (status, List.length (List.filter (fun s -> s = status) stats))
  ) counts

(* (fn has_no_inline
      (doc "Check if file declares (no-inline)")
      (since "v1.0")) *)
let has_no_inline (file : file) =
  let rec check sexp =
    match sexp with
    | List (_, Atom (_, "no-inline") :: _) -> true
    | List (_, items) -> List.exists (fun item -> check item.node) items
    | _ -> false
  in
  List.exists (fun (sexp, _) -> check sexp.node) file.top_level

(* (fn inline_targets
      (doc "Extract all (inline ...) targets from a borg file")
      (since "v1.0")) *)
let inline_targets (file : file) =
  let rec find sexp acc =
    match sexp with
    | List (_, Atom (_, "inline") :: items) ->
        let names = List.filter_map (function
          | Atom (_, name) -> Some name
          | _ -> None
        ) items in
        names @ acc
    | List (_, items) -> List.fold_left (fun a i -> find i.node a) acc items
    | _ -> acc
  in
  List.fold_left (fun acc (sexp, _) -> find sexp.node acc) [] file.top_level
