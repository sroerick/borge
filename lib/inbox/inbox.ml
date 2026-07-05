(** Unified queue: .borge-inbox files for scope concerns, bugs, drift
    findings, review findings.

    Implements docs/agent.borg inbox-file-format, inbox-commands,
    todo-aggregation. Replaces the narrower bug subsystem (lib/bug/) —
    the old `borge bug` commands become deprecated aliases.

    One queue, one format, tracked in git. The inbox is the outlet the
    borge-workflow gate creates: agents in Execute mode CAN'T edit .borg
    files, so they file inbox items for spec concerns. Humans triage in
    Plan mode. What this module does NOT do: enforce gating (the phase
    engine does that) or auto-file from drift/review (that wiring lives
    in the drift/review commands). *)

open Printf

(** {1 Types} *)

type status =
  | Triage
  | Acknowledged
  | Plan_approved
  | Closed

type source =
  | Scope_concern
  | Bug_report
  | Drift_finding
  | Review_finding

type resolution =
  | Spec_updated
  | Wontfix
  | Duplicate
  | Fixed_in_code

type inbox_item = {
  id : string;                    (* "INBOX-N" *)
  title : string;
  status : status;
  filed_by : string;              (* "agent" | "roerick" | "agent:implementer" *)
  source : source;
  affects : string option;        (* "lib.borg:db-sql" form, optional *)
  created : string;              (* ISO date "2026-05-31" *)
  doc : string;                   (* free-text description *)
  resolution : resolution option; (* set on close *)
}

let empty_item = {
  id = "INBOX-0"; title = ""; status = Triage; filed_by = "agent";
  source = Scope_concern; affects = None; created = ""; doc = "";
  resolution = None;
}

(** {1 String conversions} *)

let string_of_status = function
  | Triage -> "triage"
  | Acknowledged -> "acknowledged"
  | Plan_approved -> "plan-approved"
  | Closed -> "closed"

let status_of_string = function
  | "triage" -> Some Triage
  | "acknowledged" -> Some Acknowledged
  | "plan-approved" | "plan_approved" -> Some Plan_approved
  | "closed" -> Some Closed
  | _ -> None

let string_of_source = function
  | Scope_concern -> "scope-concern"
  | Bug_report -> "bug-report"
  | Drift_finding -> "drift-finding"
  | Review_finding -> "review-finding"

let source_of_string = function
  | "scope-concern" | "scope_concern" -> Some Scope_concern
  | "bug-report" | "bug_report" -> Some Bug_report
  | "drift-finding" | "drift_finding" -> Some Drift_finding
  | "review-finding" | "review_finding" -> Some Review_finding
  | _ -> None

let string_of_resolution = function
  | Spec_updated -> "spec-updated"
  | Wontfix -> "wontfix"
  | Duplicate -> "duplicate"
  | Fixed_in_code -> "fixed-in-code"

let resolution_of_string = function
  | "spec-updated" | "spec_updated" -> Some Spec_updated
  | "wontfix" -> Some Wontfix
  | "duplicate" -> Some Duplicate
  | "fixed-in-code" | "fixed_in_code" -> Some Fixed_in_code
  | _ -> None

(** {1 String escaping (for title/doc with special chars)} *)

(* Quote a string for sexp emission. Mirrors Meta.sexp_of_string: quote
   if the string contains space, paren, quote, newline, tab, or is empty. *)
let sexp_of_string s =
  if String.length s = 0 then "\"\""
  else begin
    let needs_quote = ref false in
    String.iter (fun c ->
      if c = ' ' || c = '(' || c = ')' || c = '"' || c = '\n' || c = '\t' then
        needs_quote := true
    ) s;
    if !needs_quote then begin
      let buf = Buffer.create (String.length s + 4) in
      Buffer.add_char buf '"';
      String.iter (fun c ->
        if c = '"' then Buffer.add_string buf "\\\""
        else if c = '\\' then Buffer.add_string buf "\\\\"
        else Buffer.add_char buf c
      ) s;
      Buffer.add_char buf '"';
      Buffer.contents buf
    end else s
  end

let string_content = function
  | Borge_lang.Ast.String (_, Borge_lang.Ast.Quoted q) -> Some q.q_content
  | Borge_lang.Ast.String (_, Borge_lang.Ast.Verbatim v) -> Some v.v_content
  | _ -> None

let value_of_sexp (s : Borge_lang.Ast.sexp) : string option =
  match s with
  | Borge_lang.Ast.Atom (_, a) -> Some a
  | Borge_lang.Ast.String _ -> string_content s
  | _ -> None

(** {1 Parse} *)

let parse_item (file : Borge_lang.Ast.file) : inbox_item option =
  match file.Borge_lang.Ast.top_level with
  | [] -> None
  | first :: _ ->
      (* The header form is (borge-inbox id:"INBOX-7" ...). The id:"INBOX-7"
         is lexed as: Atom "id:", then String "INBOX-7" (or Atom INBOX-7).
         Actually the colon-quoted form id:"X" — let's handle both:
         (borge-inbox (id "INBOX-7") ...) canonical, and id:"INBOX-7" tolerant. *)
      let walk_top (node : Borge_lang.Ast.sexp) =
        match node with
        | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "borge-inbox") :: header :: rest) ->
            let item = ref empty_item in
            (* Parse header: either (id "INBOX-7") or id:"INBOX-7" *)
            (match header with
             | Borge_lang.Ast.List (_, [Borge_lang.Ast.Atom (_, "id"); v]) ->
                 item := { !item with id = Option.value (value_of_sexp v) ~default:!item.id }
             | Borge_lang.Ast.Atom (_, a) when String.length a >= 4 && String.sub a 0 4 = "id:" ->
                 (* id:"INBOX-7" — the id: prefix glued to the atom; the value
                    may be a following String. Extract the part after "id:" if
                    present, else look at next sexp. *)
                 let rest_of_atom = String.sub a 4 (String.length a - 4) in
                 if rest_of_atom <> "" then
                   item := { !item with id = rest_of_atom }
             | _ -> ());
            (* Parse remaining fields *)
            List.iter (fun f ->
              match f with
              | Borge_lang.Ast.List (_, [Borge_lang.Ast.Atom (_, "title"); v]) ->
                  item := { !item with title = Option.value (value_of_sexp v) ~default:!item.title }
              | Borge_lang.Ast.List (_, [Borge_lang.Ast.Atom (_, "status"); v]) ->
                  (match value_of_sexp v with
                   | Some s ->
                       (match status_of_string s with
                        | Some st -> item := { !item with status = st }
                        | None -> ())
                   | None -> ())
              | Borge_lang.Ast.List (_, [Borge_lang.Ast.Atom (_, "filed-by"); v]) ->
                  item := { !item with filed_by = Option.value (value_of_sexp v) ~default:!item.filed_by }
              | Borge_lang.Ast.List (_, [Borge_lang.Ast.Atom (_, "source"); v]) ->
                  (match value_of_sexp v with
                   | Some s ->
                       (match source_of_string s with
                        | Some src -> item := { !item with source = src }
                        | None -> ())
                   | None -> ())
              | Borge_lang.Ast.List (_, [Borge_lang.Ast.Atom (_, "affects"); v]) ->
                  (* (affects section:"lib.borg:db-sql") — value may be a String *)
                  item := { !item with affects = value_of_sexp v }
              | Borge_lang.Ast.List (_, [Borge_lang.Ast.Atom (_, "created"); v]) ->
                  item := { !item with created = Option.value (value_of_sexp v) ~default:!item.created }
              | Borge_lang.Ast.List (_, [Borge_lang.Ast.Atom (_, "doc"); v]) ->
                  item := { !item with doc = Option.value (value_of_sexp v) ~default:!item.doc }
              | Borge_lang.Ast.List (_, [Borge_lang.Ast.Atom (_, "resolution"); v]) ->
                  (match value_of_sexp v with
                   | Some s ->
                       (match resolution_of_string s with
                        | Some r -> item := { !item with resolution = Some r }
                        | None -> ())
                   | None -> ())
              | _ -> ()
            ) rest;
            Some !item
        | _ -> None
      in
      walk_top first.Borge_lang.Ast.node

let parse_file (path : string) : inbox_item option =
  let content = File_utils.read_file path in
  let file = Borge_lang.Parse.parse_file content in
  parse_item file

(** {1 Write} *)

let render (item : inbox_item) : string =
  let buf = Buffer.create 256 in
  bprintf buf "(borge-inbox (id %s)\n" (sexp_of_string item.id);
  bprintf buf "  (title %s)\n" (sexp_of_string item.title);
  bprintf buf "  (status %s)\n" (string_of_status item.status);
  bprintf buf "  (filed-by %s)\n" (sexp_of_string item.filed_by);
  bprintf buf "  (source %s)\n" (string_of_source item.source);
  (match item.affects with
   | Some a -> bprintf buf "  (affects %s)\n" (sexp_of_string a)
   | None -> ());
  bprintf buf "  (created %s)\n" (sexp_of_string item.created);
  bprintf buf "  (doc %s)\n" (sexp_of_string item.doc);
  (match item.resolution with
   | Some r -> bprintf buf "  (resolution %s)\n" (string_of_resolution r)
   | None -> ());
  Buffer.add_string buf ")\n";
  Buffer.contents buf

let write (item : inbox_item) (path : string) =
  let oc = open_out path in
  output_string oc (render item);
  close_out oc

(** {1 Registry: .borge-inbox/ directory } *)

let default_dir = ".borge-inbox"

let ensure_dir () =
  if not (Sys.file_exists default_dir) then
    Unix.mkdir default_dir 0o755

let item_path id =
  Filename.concat default_dir (id ^ ".borge-inbox")

let list_items ?(status=None) ?(source=None) () =
  ensure_dir ();
  if not (Sys.file_exists default_dir) then []
  else begin
    let files = Array.to_list (Sys.readdir default_dir) in
    let inbox_files = List.filter (fun f -> Filename.check_suffix f ".borge-inbox") files in
    let items = List.filter_map (fun f ->
      let path = Filename.concat default_dir f in
      parse_file path
    ) (List.sort String.compare inbox_files) in
    let items = match status with
      | Some s -> List.filter (fun (i : inbox_item) -> i.status = s) items
      | None -> items
    in
    match source with
    | Some s -> List.filter (fun (i : inbox_item) -> i.source = s) items
    | None -> items
  end

let load_item id =
  let path = item_path id in
  if Sys.file_exists path then parse_file path
  else None

let save_item (item : inbox_item) =
  ensure_dir ();
  let path = item_path item.id in
  write item path;
  path

(** {1 ID generation} *)

(* Extract the integer N from an "INBOX-N" id. Returns 0 if unparseable. *)
let num_of_id id =
  match String.index_opt id '-' with
  | Some i when i + 1 < String.length id ->
      (try int_of_string (String.sub id (i+1) (String.length id - i - 1))
       with _ -> 0)
  | _ -> 0

let next_id () =
  let items = list_items () in
  let max_n = List.fold_left (fun acc (i : inbox_item) ->
    max acc (num_of_id i.id)
  ) 0 items in
  sprintf "INBOX-%d" (max_n + 1)

(** {1 Timestamp} *)

let now_date () =
  let tm = Unix.localtime (Unix.time ()) in
  sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

(** {1 Operations (used by CLI) } *)

let file_item ~title ~source ~filed_by ?affects ?(doc="") () =
  let id = next_id () in
  let item = {
    id; title; status = Triage; filed_by; source;
    affects; created = now_date (); doc; resolution = None;
  } in
  let path = save_item item in
  (item, path)

let acknowledge id =
  match load_item id with
  | None -> Error "Inbox item not found"
  | Some item ->
      let updated = { item with status = Acknowledged } in
      let path = save_item updated in
      Ok (updated, path)

let approve id =
  match load_item id with
  | None -> Error "Inbox item not found"
  | Some item ->
      let updated = { item with status = Plan_approved } in
      let path = save_item updated in
      Ok (updated, path)

let close id ~resolution =
  match load_item id with
  | None -> Error "Inbox item not found"
  | Some item ->
      let updated = { item with status = Closed; resolution = Some resolution } in
      let path = save_item updated in
      Ok (updated, path)

(** {1 Consolidate: scan for TODOs and incomplete specs} *)

(* Scan a file's text for TODO/FIXME/XXX markers. Returns (line, text) pairs. *)
let scan_todos (content : string) : (int * string) list =
  let lines = String.split_on_char '\n' content in
  List.mapi (fun i line ->
    let lowered = String.lowercase_ascii line in
    if String.length lowered >= 4 then begin
      let has_todo = (String.sub lowered 0 4 = "todo") || (try ignore (Str.search_forward (Str.regexp "todo") lowered 0); true with Not_found -> false) in
      let has_fixme = (try ignore (Str.search_forward (Str.regexp "fixme") lowered 0); true with Not_found -> false) in
      let has_xxx = (try ignore (Str.search_forward (Str.regexp "\\bxxx\\b") lowered 0); true with Not_found -> false) in
      if has_todo || has_fixme || has_xxx then Some (i + 1, String.trim line) else None
    end else None
  ) lines
  |> List.filter_map (fun x -> x)

(* Walk .borg and .ml files under the repo, return TODOs found. *)
let consolidate_todos () =
  let todo_files = ref [] in
  let rec walk dir =
    let entries = try Array.to_list (Sys.readdir dir) with _ -> [] in
    List.iter (fun entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path then begin
        (* skip _build, .git, .borge-* dirs *)
        if entry <> "_build" && entry <> ".git" && entry <> ".borge-inbox"
           && entry <> ".borge-bugs" && entry <> ".pi" && entry <> ".ralph"
           && entry <> "_opam" && entry <> "node_modules" then
          walk path
      end else if Filename.check_suffix path ".ml" || Filename.check_suffix path ".borg" then begin
        let content = try File_utils.read_file path with _ -> "" in
        let todos = scan_todos content in
        if todos <> [] then
          todo_files := (path, todos) :: !todo_files
      end
    ) entries
  in
  walk ".";
  List.rev !todo_files

(* File a scope-concern inbox item for each TODO found, deduplicating by
   title against existing items. Returns (filed, skipped) counts. *)
let consolidate () =
  let todos = consolidate_todos () in
  let existing = list_items () in
  let existing_titles = List.map (fun (i : inbox_item) -> i.title) existing in
  let filed = ref 0 in
  let skipped = ref 0 in
  List.iter (fun (path, markers) ->
    List.iter (fun (line, text) ->
      let title = sprintf "%s:%d %s" (Filename.basename path) line
        (String.sub text 0 (min 60 (String.length text))) in
      if List.mem title existing_titles then
        incr skipped
      else begin
        let doc = sprintf "Found in %s at line %d:\n  %s" path line text in
        let _ = file_item ~title ~source:Scope_concern ~filed_by:"agent:consolidate"
          ~doc () in
        incr filed
      end
    ) markers
  ) todos;
  (!filed, !skipped)
