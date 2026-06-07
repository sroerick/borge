(** Shared file utilities used by all borge commands *)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.to_string buf

(* agent note (|
 *   WHAT: Recursively find all .borg files under a directory,
 *   skipping _build and .git directories, excluding .borg.meta.
 *   WHY: Used by lint, check, and drift to find all spec files.
 * |) *)
let rec find_borg_files dir =
  try
    let entries = Sys.readdir dir in
    Array.fold_left (fun acc name ->
      if name = "_build" || name = ".git" then acc
      else
        let path = Filename.concat dir name in
        if Sys.is_directory path then find_borg_files path @ acc
        else if Filename.check_suffix name ".borg" then begin
          (* Skip .borg.meta files — they are machine-generated observations, not specs *)
          let meta_suffix = ".borg.meta" in
          let meta_len = String.length meta_suffix in
          if String.length name >= meta_len &&
             String.sub name (String.length name - meta_len) meta_len = meta_suffix
          then acc
          else path :: acc
        end
        else acc
    ) [] entries
  with Sys_error _ -> []
