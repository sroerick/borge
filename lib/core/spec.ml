open Borge_lang.Ast

type status =
  | Planned
  | In_progress
  | Partial
  | Implemented
  | Drifted
  | Blank

(* (fn status_of_string
      (doc "Convert string to status type")
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
      (doc "Convert status type back to string")
      (since "v1.0")) *)

let string_of_status = function
  | Planned -> "planned"
  | In_progress -> "in-progress"
  | Partial -> "partial"
  | Implemented -> "implemented"
  | Drifted -> "drifted"
  | Blank -> "blank"

(** Extract the project name from a top-level (project name ...) form *)
let project_name (file : Borge_lang.Ast.file) : string option =
  let rec find = function
    | [] -> None
    | { node = List (_, Atom (_, "project") :: Atom (_, name) :: _); _ } :: _ -> Some name
    | _ :: rest -> find rest
  in
  find file.top_level

(** Count sections in the file *)
let count_sections (file : Borge_lang.Ast.file) : int =
  let rec count_in_sexp = function
    | List (_, Atom (_, kind) :: _)
      when List.mem kind ["section"; "subsection"; "subsubsection"] -> 1
    | List (_, sexps) -> List.fold_left (fun acc s -> acc + count_in_sexp s) 0 sexps
    | _ -> 0
  in
  let rec count_in_node = function
    | [] -> 0
    | { node; _ } :: rest -> count_in_sexp node + count_in_node rest
  in
  count_in_node file.top_level

(** Extract all status values from the file *)
let statuses (file : Borge_lang.Ast.file) : status list =
  let rec extract = function
    | List (_, Atom (_, "status") :: Atom (_, s) :: _) ->
      (match status_of_string s with
       | Some st -> [st]
       | None -> [])
    | List (_, sexps) -> List.concat_map extract sexps
    | _ -> []
  in
  let rec walk = function
    | [] -> []
    | { node; _ } :: rest -> extract node @ walk rest
  in
  walk file.top_level

let status_order = [Planned; In_progress; Partial; Implemented; Drifted; Blank]

(* agent note [5200] (|
 *   WHAT: Ordered list of status values for display and comparison.
 *   Used as the canonical ordering when computing status counts.
 *   WHY: Provides a consistent sort order for reports and ensures
 *   all status types appear in output even if not present in a file.
 * |) *)

(** Extract inline target filenames from (inline filename.borg) forms *)
let inline_targets (file : Borge_lang.Ast.file) : string list =
  let rec extract = function
    | List (_, Atom (_, "inline") :: Atom (_, filename) :: _) -> [filename]
    | List (_, sexps) -> List.concat_map extract sexps
    | _ -> []
  in
  let rec walk = function
    | [] -> []
    | { node; _ } :: rest -> extract node @ walk rest
  in
  walk file.top_level

(** Check if the file declares (no-inline) *)
let has_no_inline (file : Borge_lang.Ast.file) : bool =
  let rec extract = function
    | List (_, [Atom (_, "no-inline")]) -> true
    | List (_, sexps) -> List.exists extract sexps
    | _ -> false
  in
  let rec walk = function
    | [] -> false
    | { node; _ } :: rest -> extract node || walk rest
  in
  walk file.top_level

(* agent note (|
 *   WHAT: Count occurrences of each status value in a .borg file.
 *   Returns an ordered list of status-count pairs using status_order.
 *   WHY: Used by borge report to show implementation progress and
 *   by borge drift to detect sections marked implemented that aren't.
 * |) *)
let status_counts (file : Borge_lang.Ast.file) : (status * int) list =
  let st_list = statuses file in
  let init = List.map (fun s -> (s, 0)) status_order in
  List.fold_left (fun acc st ->
    let count =
      match List.assoc_opt st acc with Some v -> v + 1 | None -> 1
    in
    (st, count) :: List.remove_assoc st acc
  ) init st_list
  |> List.sort (fun (a, _) (b, _) ->
    let idx_a = List.find_index (fun s -> s = a) status_order |> Option.get in
    let idx_b = List.find_index (fun s -> s = b) status_order |> Option.get in
    compare idx_a idx_b
  )
