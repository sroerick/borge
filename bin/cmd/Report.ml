open Borge_lib

let pad_right n s =
  let len = String.length s in
  if len >= n then s else s ^ String.make (n - len) ' '

let run dir json =
  let result = Report.run dir in
  if json then begin
    Printf.printf "%s\n" (Yojson.Basic.to_string (Json.report result));
    exit 0
  end;
  Printf.printf "Borge Report — %d .borg file(s) in '%s'\n\n" (List.length result.files) dir;
  let max_name = List.fold_left (fun acc (r : Report.file_stats) ->
    let display = match r.project_name with
      | Some n -> Printf.sprintf "%s (%s)" (Filename.basename r.path) n
      | None -> Filename.basename r.path
    in
    max acc (String.length display)
  ) 10 result.files in
  Printf.printf "%s  implemented  in-progress  planned\n" (pad_right max_name "file");
  Printf.printf "%s\n" (String.make (max_name + 30) '-');
  List.iter (fun (r : Report.file_stats) ->
    let display = match r.project_name with
      | Some n -> Printf.sprintf "%s (%s)" (Filename.basename r.path) n
      | None -> Filename.basename r.path
    in
    Printf.printf "%s      %3d          %3d       %3d\n"
      (pad_right max_name display) r.implemented r.in_progress r.planned
  ) result.files;
  if result.files <> [] then (
    Printf.printf "%s\n" (String.make (max_name + 30) '-');
    Printf.printf "%s      %3d          %3d       %3d\n"
      (pad_right max_name "total") result.totals.implemented result.totals.in_progress result.totals.planned
  );
  let total = result.totals.implemented + result.totals.in_progress + result.totals.planned in
  Printf.printf "\n%d implemented | %d in-progress | %d planned | %d total tracked\n"
    result.totals.implemented result.totals.in_progress result.totals.planned total;
  if result.totals.planned > 0 then (
    let pct = float_of_int result.totals.implemented *. 100.0 /. float_of_int total in
    Printf.printf "Completion: %.1f%%\n" pct
  );
  if result.orphans <> [] then begin
    Printf.printf "\nOrphaned .borg files (no parent, no no-inline):\n";
    List.iter (fun path ->
      Printf.printf "  ⚠ %s\n" path
    ) result.orphans
  end;
  exit 0

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory to scan for .borg files")

let json =
  Arg.(value & flag & info ["json"] ~doc:"Output as JSON")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "report" ~doc:"show status summary across all .borg files"
    ~man:[`S "DESCRIPTION";
          `P "Finds all .borg files recursively and reports how many nodes \
              are planned, in-progress, and implemented.";
          `P "With --json, outputs structured JSON instead of formatted text."])
  Term.(const run $ dir $ json)

