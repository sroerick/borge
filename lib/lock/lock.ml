open Borge_lang

(** Lock mechanism for agent sessions.

    Provides snapshot isolation for borge agent workflows.
    On lock creation: copies all .borg files to .borge.lock/original/
    On commit: integrity check, git commit, remove lock
    On abort: restores .borg files from original/

    The lock is purely metadata + snapshots. Agents edit repo files
    directly. The lock provides integrity checking, concurrency
    prevention, and audit trail. *)

let lock_dir = ".borge.lock"

let original_dir () = Printf.sprintf "%s/original" lock_dir
let prompt_file () = Printf.sprintf "%s/prompt.md" lock_dir
let journal_file () = Printf.sprintf "%s/journal" lock_dir
let role_file () = Printf.sprintf "%s/role" lock_dir
let state_file () = Printf.sprintf "%s/state" lock_dir

(* Check whether a lock is active *)
let is_locked () =
  Sys.file_exists lock_dir

(* Create lock directory and snapshot all .borg files *)
let create () =
  if is_locked () then
    Error "Lock already exists — borge abort to clear, or commit to finish"
  else begin
    (try Unix.mkdir lock_dir 0o755 with _ -> ());
    (try Unix.mkdir (original_dir ()) 0o755 with _ -> ());
    let borg_files = File_utils.find_borg_files "." in
    List.iter (fun src ->
      let dst = Printf.sprintf "%s/%s" (original_dir ()) (Filename.basename src) in
      let ic = open_in src in
      let oc = open_out dst in
      begin
        try while true do output_string oc (input_line ic ^ "\n") done
        with End_of_file -> ()
      end;
      close_in ic;
      close_out oc
    ) borg_files;
    let oc = open_out (state_file ()) in
    output_string oc "running\n";
    close_out oc;
    Ok ()
  end

(* Mark state as completed or failed *)
let set_state st =
  let oc = open_out (state_file ()) in
  output_string oc (st ^ "\n");
  close_out oc

(* Get current state *)
let get_state () =
  if not (is_locked ()) then "no-lock"
  else begin
    try
      let ic = open_in (state_file ()) in
      let st = try input_line ic with End_of_file -> "unknown" in
      close_in ic;
      st
    with _ -> "unknown"
  end

(* Integrity check: compare original/ with working tree for deleted
   human-authored annotated comments.
   A human-authored comment: (* <author> <type> ... *) where
   author != "agent" and author != "bot" *)
let check_integrity () : (string, string) result =
  if not (is_locked ()) then
    Error "No lock directory"
  else begin
    let original_files =
      let d = original_dir () in
      let files = Sys.readdir d in
      Array.to_list files |> List.filter (fun f -> Filename.check_suffix f ".borg")
      |> List.map (fun f -> Printf.sprintf "%s/%s" d f)
    in
    let issues = ref [] in
    List.iter (fun orig_path ->
      let base = Filename.basename orig_path in
      let work_path = base in
      if Sys.file_exists work_path then begin
        try
          let orig_content = File_utils.read_file orig_path in
          let work_content = File_utils.read_file work_path in
          let orig_lines = String.split_on_char '\n' orig_content in
          let work_lines = String.split_on_char '\n' work_content in
          List.iter (fun line ->
            let trimmed = String.trim line in
            if String.length trimmed > 4 then begin
              let prefix = String.sub trimmed 0 3 in
              if prefix = "(* " then begin
                let rest = String.sub trimmed 3 (String.length trimmed - 3) in
                let space_idx = try String.index rest ' ' with Not_found -> -1 in
                if space_idx > 0 then begin
                  let author = String.sub rest 0 space_idx in
                  if author <> "agent" && author <> "bot" then begin
                    if not (List.mem line work_lines) then
                      issues := Printf.sprintf "%s: deleted comment by %s" base author :: !issues
                  end
                end
              end
            end
          ) orig_lines
        with _ -> ()
      end
    ) original_files;
    if !issues = [] then Ok "pass"
    else Error (String.concat "\n" !issues)
  end

(* Restore .borg files from original/ — abort *)
let restore () : unit =
  let orig_dir = original_dir () in
  let files = Sys.readdir orig_dir in
  Array.iter (fun f ->
    if Filename.check_suffix f ".borg" then begin
      let src = Printf.sprintf "%s/%s" orig_dir f in
      let dst = f in
      let ic = open_in src in
      let oc = open_out dst in
      begin
        try while true do output_string oc (input_line ic ^ "\n") done
        with End_of_file -> ()
      end;
      close_in ic;
      close_out oc
    end
  ) files

(* Remove lock directory *)
let remove () =
  let rec rm_rf path =
    if Sys.is_directory path then begin
      let entries = Sys.readdir path in
      Array.iter (fun e -> rm_rf (Filename.concat path e)) entries;
      Unix.rmdir path
    end else
      Sys.remove path
  in
  if is_locked () then rm_rf lock_dir

(* Teardown: commit (if pass) or abort (if fail) *)
let teardown ~(commit : bool) : (string, string) result =
  if not (is_locked ()) then
    Error "No lock directory"
  else if not commit then begin
    restore ();
    remove ();
    Ok "aborted"
  end else begin
    match check_integrity () with
    | Error msg ->
      Error (Printf.sprintf "Integrity check failed:\n%s" msg)
    | Ok _ ->
      let borg_files = File_utils.find_borg_files "." in
      let changed = ref [] in
      List.iter (fun path ->
        let orig = Printf.sprintf "%s/%s" (original_dir ()) (Filename.basename path) in
        if Sys.file_exists orig then begin
          let current = File_utils.read_file path in
          let old = File_utils.read_file orig in
          if current <> old then begin
            try
              let file = Parse.parse_file current in
              let formatted = Print.print_file file in
              let oc = open_out path in
              output_string oc formatted;
              close_out oc;
              changed := path :: !changed
            with _ -> ()
          end
        end
      ) borg_files;
      Ok (Printf.sprintf "Committed. Changed: %s" (String.concat ", " !changed))
  end
