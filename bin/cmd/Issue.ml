(** borge issue — issue tracker commands *)

open Borge_lib

(* exempt doc *)
let now () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday

(* exempt doc *)
let get_user () =
  try Sys.getenv "USER" with Not_found -> "unknown"

(** Create a new bug with a title and optional open status *)
let cmd_new title open_status with_commit =
  ignore with_commit;
  let id = Bug_ast.make_id () in
  let status = if open_status then Bug_ast.Open else Bug_ast.Triage in
  let bug = {
    Bug_ast.id;
    title;
    status;
    created = now ();
    filed_by = get_user ();
    doc = "";
    affects_section = None;
    relates_to = [];
    drift_report_id = None;
    resolution = None;
  } in
  let path = Bug_registry.save_bug bug in
  Printf.printf "Created bug %s\n" id;
  Printf.printf "  Status: %s\n" (Bug_ast.string_of_status status);
  Printf.printf "  File: %s\n" path;
  if open_status then
    Printf.printf "Run 'borge issue doc %s' to add description.\n" id

(** List all bugs, optionally filtered by status *)
let cmd_list status_opt =
  let filter_status = match status_opt with
    | Some s -> Bug_ast.status_of_string s
    | _ -> None
  in
  let bugs = Bug_registry.list_bugs ~status:filter_status () in
  if bugs = [] then
    Printf.printf "No bugs found.\n"
  else begin
    Printf.printf "Bugs (%d total):\n" (List.length bugs);
    List.iter (fun (b : Bug_ast.bug) ->
      Printf.printf "  %-20s [%8s] %s\n"
        b.id (Bug_ast.string_of_status b.status) b.title
    ) bugs
  end

(** Show detailed info for a specific bug by ID *)
let cmd_show id =
  match Bug_registry.load_bug id with
  | None ->
      Printf.eprintf "Bug '%s' not found.\n" id;
      exit 1
  | Some bug ->
      Printf.printf "%s: %s\n" bug.id bug.title;
      Printf.printf "Status: %s\n" (Bug_ast.string_of_status bug.status);
      Printf.printf "Created: %s by %s\n" bug.created bug.filed_by;
      if bug.doc <> "" then
        Printf.printf "\n%s\n" bug.doc;
      (match bug.affects_section with
       | Some s -> Printf.printf "\nAffects: %s\n" s
       | None -> ());
      (match bug.drift_report_id with
       | Some d -> Printf.printf "Drift report: %s\n" d
       | None -> ())

(** Close a bug by ID, marking it as resolved *)
let cmd_close id with_commit =
  ignore with_commit;
  match Bug_registry.close_bug id ~resolution:"closed" ~by:(get_user ()) with
  | Error msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Ok path ->
      Printf.printf "Closed bug %s\n" id;
      Printf.printf "  File: %s\n" path

open Cmdliner

(* exempt doc *)
let title =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"TITLE"
    ~doc:"Bug title")

(* exempt doc *)
let id =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ID"
    ~doc:"Bug ID (e.g., BORGE-1234567890)")

(* exempt doc *)
let status_opt =
  Arg.(value & opt (some string) None & info ["status"] ~docv:"STATUS"
    ~doc:"Filter by status (triage, open, in-progress, resolved, closed)")

(* exempt doc *)
let open_flag =
  Arg.(value & flag & info ["open"] ~doc:"Create as open (not triage)")

(* exempt doc *)
let with_commit_flag =
  Arg.(value & flag & info ["commit"] ~doc:"Commit changes to git")

let new_cmd =
  Cmd.v (Cmd.info "new" ~doc:"create a new bug")
    Term.(const cmd_new $ title $ open_flag $ with_commit_flag)

let list_cmd =
  Cmd.v (Cmd.info "list" ~doc:"list bugs")
    Term.(const cmd_list $ status_opt)

let show_cmd =
  Cmd.v (Cmd.info "show" ~doc:"show bug details")
    Term.(const cmd_show $ id)

let close_cmd =
  Cmd.v (Cmd.info "close" ~doc:"close a bug")
    Term.(const cmd_close $ id $ with_commit_flag)

let cmd : unit Cmd.t =
  Cmd.group (Cmd.info "issue" ~doc:"issue tracker")
    [new_cmd; list_cmd; show_cmd; close_cmd]
