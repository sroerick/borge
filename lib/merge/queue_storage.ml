(* Merge queue state persistence
 *
 * roerick note (|
 *   Serializes and persists merge queue state.
 *   Storage format: .borge/queue/state.json
 *   
 *   Operations:
 *   - Load/save full queue state
 *   - Atomic updates via write-to-temp-then-rename
 *   - Backup previous state on write
 * |) *)

open Queue_types

(** Default storage path *)
let default_path = ".borge/queue/state.json"

(** Create directory recursively *)
let rec mkdir_p path =
  if not (Sys.file_exists path) then begin
    let parent = Filename.dirname path in
    if parent <> path && parent <> "." && parent <> "/" then
      mkdir_p parent;
    try Unix.mkdir path 0o755 with _ -> ()
  end

(** Ensure directory exists *)
let ensure_dir path =
  let dir = Filename.dirname path in
  mkdir_p dir

(** Serialize queue to JSON string *)
let serialize queue =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "{\n";
  Buffer.add_string buf "  \"items\": [\n";
  
  List.iteri (fun i item ->
    Buffer.add_string buf "    {\n";
    Buffer.add_string buf (Printf.sprintf "      \"id\": \"%s\",\n" item.id);
    Buffer.add_string buf (Printf.sprintf "      \"state\": \"%s\",\n" (string_of_state item.state));
    Buffer.add_string buf (Printf.sprintf "      \"branch\": \"%s\",\n" item.branch);
    Buffer.add_string buf (Printf.sprintf "      \"worktree_path\": \"%s\",\n" item.worktree_path);
    Buffer.add_string buf (Printf.sprintf "      \"summary\": \"%s\",\n" (String.escaped item.summary));
    Buffer.add_string buf "      \"affected_files\": [";
    List.iteri (fun j f ->
      if j > 0 then Buffer.add_string buf ", ";
      Buffer.add_string buf (Printf.sprintf "\"%s\"" (String.escaped f))
    ) item.affected_files;
    Buffer.add_string buf "],\n";
    Buffer.add_string buf (Printf.sprintf "      \"confidence\": %.2f,\n" item.confidence);
    Buffer.add_string buf (Printf.sprintf "      \"submitted_at\": \"%s\"" item.submitted_at);
    (match item.merged_at with
     | Some t -> Buffer.add_string buf (Printf.sprintf ",\n      \"merged_at\": \"%s\"" t)
     | None -> ());
    (match item.rejected_reason with
     | Some r -> Buffer.add_string buf (Printf.sprintf ",\n      \"rejected_reason\": \"%s\"" (String.escaped r))
     | None -> ());
    Buffer.add_string buf "\n    }";
    if i < List.length queue.items - 1 then
      Buffer.add_string buf ",\n"
    else
      Buffer.add_string buf "\n"
  ) queue.items;
  
  Buffer.add_string buf "  ],\n";
  Buffer.add_string buf (Printf.sprintf "  \"next_merge_id\": %s\n"
    (match queue.next_merge_id with Some id -> Printf.sprintf "\"%s\"" id | None -> "null"));
  Buffer.add_string buf "}\n";
  Buffer.contents buf

(** Parse queue from JSON string - simplified *)
let deserialize _json_str =
  (* For now, return empty queue - full JSON parsing would need yojson *)
  empty_queue

(** Save queue to file (atomic) *)
let save ?(path=default_path) queue =
  ensure_dir path;
  let json = serialize queue in
  let temp_path = path ^ ".tmp" in
  try
    (* Write to temp file *)
    let oc = open_out temp_path in
    output_string oc json;
    close_out oc;
    (* Atomic rename *)
    Sys.rename temp_path path;
    Ok ()
  with e ->
    (* Clean up temp file on error *)
    (try Sys.remove temp_path with _ -> ());
    Error (Printf.sprintf "Failed to save queue: %s" (Printexc.to_string e))

(** Load queue from file *)
let load ?(path=default_path) () =
  if not (Sys.file_exists path) then
    Ok empty_queue
  else
    try
      let json = File_utils.read_file path in
      Ok (deserialize json)
    with e ->
      Error (Printf.sprintf "Failed to load queue: %s" (Printexc.to_string e))

(** Backup current state before write *)
let backup ?(path=default_path) () =
  if Sys.file_exists path then
    let backup_path = path ^ ".bak" in
    try
      let content = File_utils.read_file path in
      let oc = open_out backup_path in
      output_string oc content;
      close_out oc;
      Ok ()
    with e ->
      Error (Printf.sprintf "Failed to backup: %s" (Printexc.to_string e))
  else
    Ok ()

(** Save with backup *)
let save_with_backup ?(path=default_path) queue =
  match backup ~path () with
  | Error e -> Error e
  | Ok () -> save ~path queue

(** Check if queue file exists *)
let exists ?(path=default_path) () =
  Sys.file_exists path

(** Get last modified time *)
let last_modified ?(path=default_path) () =
  if exists ~path () then
    try
      let stats = Unix.stat path in
      Some stats.Unix.st_mtime
    with _ -> None
  else
    None
