(* Bug registry - manage .borge-bug files *)

open Bug_ast

let default_dir = ".borge-bugs"

let ensure_dir () =
  if not (Sys.file_exists default_dir) then
    Unix.mkdir default_dir 0o755

let bug_path id =
  Filename.concat default_dir (id ^ ".borge-bug")

let list_bugs ?(status=None) () =
  ensure_dir ();
  let files = Array.to_list (Sys.readdir default_dir) in
  let bug_files = List.filter (fun f -> Filename.check_suffix f ".borge-bug") files in
  let bugs = List.map (fun f ->
    let path = Filename.concat default_dir f in
    Bug_parse.parse_file path
  ) bug_files in
  match status with
  | None -> bugs
  | Some st -> List.filter (fun b -> b.status = st) bugs

let load_bug id =
  let path = bug_path id in
  if Sys.file_exists path then
    Some (Bug_parse.parse_file path)
  else None

let save_bug bug =
  ensure_dir ();
  let path = bug_path bug.id in
  Bug_write.write bug path;
  path

let close_bug id ~resolution ~by =
  match load_bug id with
  | None -> Error "Bug not found"
  | Some bug ->
      let res = {
        when_ = string_of_int (int_of_float (Unix.time ()));
        how = resolution;
        by;
      } in
      let updated = { bug with status = Closed; resolution = Some res } in
      let path = save_bug updated in
      Ok path
