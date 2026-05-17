open Borge_lang

type lint_issue =
  | Invalid_status of { path : string; value : string; line : int; col : int }
  | Duplicate_project_name of { name : string; paths : string list }
  | No_inline_on_root of { path : string }
  | Unknown_comment_type of { path : string; type_name : string; line : int }
  | Missing_comment_value of { path : string; author : string; type_name : string option; line : int }
  | Deleted_human_comment of { path : string; author : string; line : int }
  | Pending_response of { path : string; author : string; line : int; question : string }
  | Orphaned_file of { path : string }

type lint_result = {
  issues : lint_issue list;
  error_count : int;
  warning_count : int;
}

let pos_of_sexp = function
  | Ast.Atom (p, _) -> p
  | Ast.String (p, _) -> p
  | Ast.List (p, _) -> p

(** Check for invalid status values *)
let check_invalid_statuses path file =
  let rec walk_sexp = function
    | Ast.List (_, Ast.Atom (_, "status") :: Ast.Atom (p, v) :: _) ->
        (match Spec.status_of_string v with
         | Some _ -> []
         | None -> [Invalid_status { path; value = v; line = p.Ast.line; col = p.Ast.col }])
    | Ast.List (_, children) -> List.concat_map walk_sexp children
    | _ -> []
  in
  let rec walk_file : Ast.sexp_with_comments list -> lint_issue list = function
    | [] -> []
    | { Ast.node; _ } :: rest -> walk_sexp node @ walk_file rest
  in
  walk_file file.Ast.top_level

(** Check for duplicate project names across files *)
let check_duplicate_names file_stats =
  let names = List.filter_map (fun (path, name) ->
    match name with Some n -> Some (n, path) | None -> None
  ) file_stats in
  let grouped = List.sort (fun (a, _) (b, _) -> String.compare a b) names in
  let rec find_dups = function
    | [] | [_] -> []
    | (n1, p1) :: (n2, p2) :: rest when n1 = n2 ->
        let paths = p1 :: p2 :: List.filter_map (fun (n, p) -> if n = n1 then Some p else None) rest in
        Duplicate_project_name { name = n1; paths } :: find_dups (List.filter (fun (n, _) -> n <> n1) rest)
    | _ :: rest -> find_dups rest
  in
  find_dups grouped

let authorship_string = function
  | Ast.Single a -> a
  | Ast.Multiple auths -> String.concat " " auths

(** Check annotated comments for unknown types and missing values *)
let check_annotated_comments path file =
  let issues = ref [] in
  let visit_comment = function
    | Ast.Annotated ac ->
        let valid_types = ["note"; "design"; "ask"; "response"; "todo"] in
        (match ac.Ast.comment_type with
         | Ast.Typed t when not (List.mem t valid_types) ->
             issues := Unknown_comment_type { path; type_name = t; line = ac.Ast.start_line } :: !issues
         | _ -> ());
        (match ac.Ast.comment_type, ac.Ast.value with
         | Ast.Typed _, None ->
             issues := Missing_comment_value {
               path; author = authorship_string ac.Ast.authorship;
               type_name = (match ac.Ast.comment_type with Ast.Typed t -> Some t | _ -> None);
               line = ac.Ast.start_line
             } :: !issues
         | _ -> ())
    | _ -> ()
  in
  let rec visit_comments = function
    | [] -> ()
    | c :: cs -> visit_comment c; visit_comments cs
  in
  let rec visit_from_sexp = function
    | Ast.List (_, children) -> List.iter visit_from_sexp children
    | _ -> ()
  in
  visit_comments file.Ast.top_level_comments;
  List.iter (fun (swc : Ast.sexp_with_comments) ->
    visit_comments swc.Ast.comments_before;
    visit_from_sexp swc.Ast.node
  ) file.Ast.top_level;
  visit_comments file.Ast.trailing_comments;
  List.rev !issues

(** Check for pending ask comments with empty response slots *)
let check_pending_responses path file =
  let asks = ref [] in
  let responses = ref [] in
  let visit_comment = function
    | Ast.Annotated ac ->
        (match ac.Ast.comment_type with
         | Ast.Typed "ask" -> asks := ac :: !asks
         | Ast.Typed "response" -> responses := ac :: !responses
         | _ -> ())
    | _ -> ()
  in
  let rec visit_comments = function
    | [] -> ()
    | c :: cs -> visit_comment c; visit_comments cs
  in
  visit_comments file.Ast.top_level_comments;
  List.iter (fun (swc : Ast.sexp_with_comments) ->
    visit_comments swc.Ast.comments_before
  ) file.Ast.top_level;
  (* An ask is "pending" if it has no response after it *)
  List.filter_map (fun ask ->
    let has_response = List.exists (fun resp ->
      resp.Ast.start_line > ask.Ast.start_line && resp.Ast.start_line <= ask.Ast.end_line + 5
    ) !responses in
    if has_response then None
    else
      let question = match ask.Ast.value with
        | Some (Ast.Verbatim v) ->
            let s = v.Ast.v_content in
            if String.length s > 40 then String.sub s 0 37 ^ "..." else s
        | Some (Ast.Quoted q) ->
            let s = q.Ast.q_content in
            if String.length s > 40 then String.sub s 0 37 ^ "..." else s
        | None -> "?"
      in
      Some (Pending_response {
        path;
        author = authorship_string ask.Ast.authorship;
        line = ask.Ast.start_line;
        question
      })
  ) !asks

(** Check for human-authored comment deletions against git HEAD *)
let check_comment_integrity path =
  let cmd = Printf.sprintf "git diff HEAD -- %s 2>/dev/null" (Filename.quote path) in
  let ic = Unix.open_process_in cmd in
  let lines = ref [] in
  (try while true do lines := input_line ic :: !lines done with End_of_file -> ());
  let _status = Unix.close_process_in ic in
  let issues = ref [] in
  List.iter (fun line ->
    if String.length line > 0 && line.[0] = '-' then begin
      let content = String.sub line 1 (String.length line - 1) in
      if String.length content > 3 && String.sub content 0 3 = "(* " then begin
        let rest = String.sub content 3 (String.length content - 3) in
        let space_idx = try String.index rest ' ' with Not_found -> -1 in
        if space_idx > 0 then begin
          let author = String.sub rest 0 space_idx in
          if author <> "agent" && author <> "bot" then
            issues := Deleted_human_comment { path; author; line = 0 } :: !issues
        end
      end
    end
  ) (List.rev !lines);
  !issues

let run dir =
  let all_files = File_utils.find_borg_files dir in
  let file_stats = List.filter_map (fun path ->
    try
      let input = File_utils.read_file path in
      let file = Parse.parse_file input in
      Some (path, Spec.project_name file, file)
    with _ -> None
  ) all_files in
  (* Collect issues per category *)
  let status_issues = List.concat_map (fun (path, _, file) ->
    check_invalid_statuses path file
  ) file_stats in
  let name_issues = check_duplicate_names (List.map (fun (p, n, _) -> (p, n)) file_stats) in
  let comment_issues = List.concat_map (fun (path, _, file) ->
    check_annotated_comments path file @
    check_comment_integrity path
  ) file_stats in
  let pending_issues = List.concat_map (fun (path, _, file) ->
    check_pending_responses path file
  ) file_stats in
  let orphan_issues = List.map (fun path ->
    Orphaned_file { path }
  ) (Project.find_orphans dir) in
  let all_issues = status_issues @ name_issues @ comment_issues @ pending_issues @ orphan_issues in
  let errors = List.filter (function
    | Invalid_status _ | Duplicate_project_name _ | Orphaned_file _ -> true
    | _ -> false
  ) all_issues in
  let warnings = List.filter (function
    | Deleted_human_comment _ | Pending_response _ | Unknown_comment_type _
    | Missing_comment_value _ -> true
    | _ -> false
  ) all_issues in
  { issues = all_issues; error_count = List.length errors; warning_count = List.length warnings }
