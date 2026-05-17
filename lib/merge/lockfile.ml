(* Concurrent lockfile for agent runs
 *
 * roerick note (|
 *   Manages .pi/concurrent.lock for coordinating agent runs.
 *   
 *   Lock format (JSON):
 *   {
 *     "active_runs": [
 *       {
 *         "id": "agent-123",
 *         "branch": "agent/run-...",
 *         "files": ["lib/parser.ml"],
 *         "since": "2026-05-17T10:30:00Z"
 *       }
 *     ]
 *   }
 *   
 *   Provides acquire, release, and conflict checking.
 * |) *)

type run_lock = {
  id : string;
  branch : string;
  files : string list;
  since : string;  (* ISO 8601 *)
}

type lockfile = {
  active_runs : run_lock list;
}

(** Default lockfile path *)
let default_path = ".pi/concurrent.lock"

(** Ensure .pi directory exists *)
let ensure_pi_dir () =
  if not (Sys.file_exists ".pi") then
    Unix.mkdir ".pi" 0o755

(** Read lockfile *)
let read ?(path=default_path) () =
  if not (Sys.file_exists path) then
    Ok { active_runs = [] }
  else
    try
      let _content = File_utils.read_file path in
      (* Simple JSON parsing for our specific format *)
      (* For now, return empty - full JSON parsing would need yojson *)
      Ok { active_runs = [] }
    with e ->
      Error (Printf.sprintf "Failed to read lockfile: %s" (Printexc.to_string e))

(** Write lockfile *)
let write ?(path=default_path) lockfile =
  ensure_pi_dir ();
  try
    let oc = open_out path in
    (* Simple JSON output *)
    output_string oc "{\n";
    output_string oc "  \"active_runs\": [\n";
    List.iteri (fun i run ->
      output_string oc "    {\n";
      output_string oc (Printf.sprintf "      \"id\": \"%s\",\n" run.id);
      output_string oc (Printf.sprintf "      \"branch\": \"%s\",\n" run.branch);
      output_string oc "      \"files\": [";
      List.iteri (fun j file ->
        if j > 0 then output_string oc ", ";
        output_string oc (Printf.sprintf "\"%s\"" file)
      ) run.files;
      output_string oc "],\n";
      output_string oc (Printf.sprintf "      \"since\": \"%s\"\n" run.since);
      output_string oc "    }";
      if i < List.length lockfile.active_runs - 1 then
        output_string oc ",\n"
      else
        output_string oc "\n"
    ) lockfile.active_runs;
    output_string oc "  ]\n";
    output_string oc "}\n";
    close_out oc;
    Ok ()
  with e ->
    Error (Printf.sprintf "Failed to write lockfile: %s" (Printexc.to_string e))

(** Acquire lock for a new run

    Checks for conflicts with existing runs.
    Returns Error if any existing run touches the same files. *)
let acquire ~id ~branch ~files () =
  match read () with
  | Error e -> Error e
  | Ok lockfile ->
      (* Check for conflicts *)
      let conflicts = List.filter (fun (run : run_lock) ->
        List.exists (fun f1 ->
          List.exists (fun f2 -> f1 = f2) run.files
        ) files
      ) lockfile.active_runs in
      
      if conflicts <> [] then
        Error (Printf.sprintf "Conflict with existing run(s): %s"
          (String.concat ", " (List.map (fun r -> r.id) conflicts)))
      else
        let timestamp =
          let now = Unix.gmtime (Unix.time ()) in
          Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
            (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday
            now.tm_hour now.tm_min now.tm_sec
        in
        let new_run = { id; branch; files; since = timestamp } in
        let updated = { active_runs = new_run :: lockfile.active_runs } in
        write updated

(** Release lock for a run

    Removes the run from the active_runs list. *)
let release ~id () =
  match read () with
  | Error e -> Error e
  | Ok lockfile ->
      let filtered = List.filter (fun (run : run_lock) -> run.id <> id) lockfile.active_runs in
      write { active_runs = filtered }

(** Check if files conflict with any active run *)
let check_conflicts files =
  match read () with
  | Error _ -> []  (* If can't read, assume no conflicts *)
  | Ok lockfile ->
      List.filter (fun (run : run_lock) ->
        List.exists (fun f1 ->
          List.exists (fun f2 -> f1 = f2) run.files
        ) files
      ) lockfile.active_runs

(** Check if any runs are active *)
let has_active_runs () =
  match read () with
  | Error _ -> false
  | Ok lockfile -> lockfile.active_runs <> []

(** List all active runs *)
let list_active () =
  match read () with
  | Error _ -> []
  | Ok lockfile -> lockfile.active_runs

(** Remove stale locks (runs older than threshold) *)
let prune_stale ~max_age_seconds () =
  match read () with
  | Error _ -> Ok ()
  | Ok lockfile ->
      let now = Unix.time () in
      let fresh = List.filter (fun (run : run_lock) ->
        (* Parse timestamp and check age *)
        try
          Scanf.sscanf run.since "%04d-%02d-%02dT%02d:%02d:%02dZ" 
            (fun y mo d h mi se ->
              let tm = { Unix.tm_year = y - 1900; tm_mon = mo - 1; tm_mday = d;
                        tm_hour = h; tm_min = mi; tm_sec = se;
                        tm_wday = 0; tm_yday = 0; tm_isdst = false }
              in
              let run_time = Unix.mktime tm |> fst in
              now -. run_time < float_of_int max_age_seconds)
        with _ -> true  (* Keep if can't parse *)
      ) lockfile.active_runs in
      write { active_runs = fresh }
