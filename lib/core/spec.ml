open Borge_lang.Ast

type status =
  | Planned
  | In_progress
  | Partial
  | Implemented
  | Verified
  | Drifted
  | Deprecated
  | Blank

(* (fn status_of_string
      (doc "Convert string to status type")
      (since "v1.0")) *)
let status_of_string = function
  | "planned" -> Some Planned
  | "in-progress" -> Some In_progress
  | "partial" -> Some Partial
  | "implemented" -> Some Implemented
  | "verified" -> Some Verified
  | "drifted" -> Some Drifted
  | "deprecated" -> Some Deprecated
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
  | Verified -> "verified"
  | Drifted -> "drifted"
  | Deprecated -> "deprecated"
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

let status_order = [Planned; In_progress; Partial; Implemented; Verified; Drifted; Deprecated; Blank]

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
    | List (_, Atom (_, "inline") :: String (_, sv) :: _) ->
        let s = match sv with
          | Quoted q -> q.q_content
          | Verbatim v -> v.v_content
        in
        [s]
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
(** Find status value in a list of sexp children *)
let rec find_status : Borge_lang.Ast.sexp list -> status option = function
  | [] -> None
  | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "status") :: Borge_lang.Ast.Atom (_, v) :: _) :: _ ->
      status_of_string v
  | _ :: rest -> find_status rest

(** Extract (implements ...) file paths from a list of sexp children.
    Returns paths as strings. Multiple implements forms accumulate. *)
let extract_implements children =
  let rec find_impls = function
    | [] -> []
    | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "implements") :: paths) :: rest ->
        let files = List.filter_map (function
          | Borge_lang.Ast.Atom (_, p) -> Some p
          | _ -> None
        ) paths in
        files @ find_impls rest
    | _ :: rest -> find_impls rest
  in
  find_impls children

(** A single verify criterion: method name and arguments *)
type verify_item = {
  method_ : string;
  args : string list;
}

(** Extract (verify ...) items from section children.
    Each child of verify is a method call: (method_name arg1 arg2 ...).
    Returns list of verify_item records. *)
let extract_verify (children : Borge_lang.Ast.sexp list) : verify_item list =
  let rec find_verify = function
    | [] -> []
    | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "verify") :: items) :: rest ->
        let vs = List.filter_map (function
          | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, m) :: args) ->
              let arg_strs = List.filter_map (function
                | Borge_lang.Ast.Atom (_, a) -> Some a
                | Borge_lang.Ast.String (_, v) ->
                    Some (Borge_lang.Print.string_of_string_value v)
                | _ -> None
              ) args in
              Some { method_ = m; args = arg_strs }
          | _ -> None
        ) items in
        vs @ find_verify rest
    | _ :: rest -> find_verify rest
  in
  find_verify children

(** Section mapping: name, status, explicitly declared files, verify items. *)
type section_mapping = {
  name : string;
  status : status option;
  implements : string list;
  verify : verify_item list;
}

(** Extract all section mappings from a file *)
let extract_section_mappings (file : Borge_lang.Ast.file) : section_mapping list =
  let rec walk_sexp acc = function
    | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, kind) :: rest_children)
      when List.mem kind ["subsection"; "section"; "subsubsection"] ->
        let name = match rest_children with
          | Borge_lang.Ast.Atom (_, n) :: _ -> Some n
          | _ -> None
        in
        let status = find_status rest_children in
        let impls = extract_implements rest_children in
        let verif = extract_verify rest_children in
        let acc' = match name with
          | Some n -> { name = n; status; implements = impls; verify = verif } :: acc
          | None -> acc
        in
        List.fold_left walk_sexp acc' rest_children
    | Borge_lang.Ast.List (_, children) ->
        List.fold_left walk_sexp acc children
    | _ -> acc
  in
  List.fold_left (fun acc { node; _ } -> walk_sexp acc node) [] file.top_level
  |> List.rev

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
