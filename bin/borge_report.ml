open Borge_sexp
open Borge_lib

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.to_string buf

let report_file path =
  let input = read_file path in
  try
    let file = Parse.parse_file input in
    let name = Spec.project_name file in
    let counts = Spec.status_counts file in
    let total = List.fold_left (fun acc (_, c) -> acc + c) 0 counts in
    if total > 0 then Some (name, counts) else None
  with
  | _ -> None

let rec find_borg_files dir =
  try
    let entries = Sys.readdir dir in
    Array.fold_left (fun acc name ->
      if name = "_build" || name = ".git" then acc
      else
        let path = Filename.concat dir name in
        if Sys.is_directory path then find_borg_files path @ acc
        else if Filename.check_suffix name ".borg" then path :: acc
        else acc
    ) [] entries
  with Sys_error _ -> []

let pad_right n s =
  let len = String.length s in
  if len >= n then s else s ^ String.make (n - len) ' '

let run dir =
  let files = find_borg_files dir in
  Printf.printf "Borge Report — %d .borg file(s) in '%s'\n\n" (List.length files) dir;
  
  let total_impl = ref 0 in
  let total_plan = ref 0 in
  let total_prog = ref 0 in

  let rows = List.filter_map (fun path ->
    match report_file path with
    | None -> None
    | Some (name, counts) ->
        let short = Filename.basename path in
        let impl = try List.assoc Spec.Implemented counts with Not_found -> 0 in
        let plan = try List.assoc Spec.Planned counts with Not_found -> 0 in
        let prog = try List.assoc Spec.In_progress counts with Not_found -> 0 in
        total_impl := !total_impl + impl;
        total_plan := !total_plan + plan;
        total_prog := !total_prog + prog;
        let display = match name with Some n -> Printf.sprintf "%s (%s)" short n | None -> short in
        Some (display, impl, prog, plan)
  ) files in

  let max_name = List.fold_left (fun acc (n, _, _, _) -> max acc (String.length n)) 10 rows in
  Printf.printf "%s  implemented  in-progress  planned\n" (pad_right max_name "file");
  Printf.printf "%s\n" (String.make (max_name + 30) '-');
  List.iter (fun (name, impl, prog, plan) ->
    Printf.printf "%s      %3d          %3d       %3d\n"
      (pad_right max_name name) impl prog plan
  ) rows;

  if rows <> [] then (
    Printf.printf "%s\n" (String.make (max_name + 30) '-');
    Printf.printf "%s      %3d          %3d       %3d\n"
      (pad_right max_name "total") !total_impl !total_prog !total_plan;
  );

  Printf.printf "\n%d implemented | %d in-progress | %d planned | %d total tracked\n"
    !total_impl !total_prog !total_plan
    (!total_impl + !total_prog + !total_plan);
  
  if !total_plan > 0 then (
    let pct = float_of_int !total_impl *. 100.0 /. float_of_int (!total_impl + !total_prog + !total_plan) in
    Printf.printf "Completion: %.1f%%\n" pct
  );
  exit 0

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory to scan for .borg files")

let cmd =
  Cmd.v (Cmd.info "report" ~doc:"show status summary across all .borg files"
    ~man:[`S "DESCRIPTION";
          `P "Finds all .borg files recursively and reports how many nodes \
              are planned, in-progress, and implemented. This is the primary \
              tool for understanding project state and prioritizing work."])
  Term.(const run $ dir)

let () = ignore (Cmd.eval cmd : int)
