open Borge_lib

let pad_right n s =
  let len = String.length s in
  if len >= n then s else s ^ String.make (n - len) ' '

let run dir json quiet =
  let result = Stats.run dir in
  if json then begin
    if not quiet then Printf.printf "%s\n" (Yojson.Basic.to_string (Json_out.stats result));
    exit 0
  end;
  if quiet then exit 0;
  Printf.printf "Borge Stats — %d source file(s) in '%s'\n\n" (List.length result.files) dir;
  if result.files = [] then begin
    Printf.printf "No .ml files found.\n";
    exit 0
  end;
  let max_name = List.fold_left (fun acc (m : Stats.file_metrics) ->
    max acc (String.length m.module_name)
  ) 4 result.files in
  Printf.printf "%s  lines  funcs  avg  exports  docs  mli\n" (pad_right max_name "module");
  Printf.printf "%s\n" (String.make (max_name + 50) '-');
  List.iter (fun (m : Stats.file_metrics) ->
    Printf.printf "%s  %5d  %6d  %3.0f  %7d  %3.0f%%  %s\n"
      (pad_right max_name m.module_name)
      m.lines m.functions m.avg_length m.exports
      (m.doc_coverage *. 100.0)
      (if m.has_mli then "yes" else "no")
  ) result.files;
  Printf.printf "%s\n" (String.make (max_name + 50) '-');
  Printf.printf "%s  %5d  %6d  %3.0f  %7d  %3.0f%%\n"
    (pad_right max_name "total")
    result.total_lines result.total_functions result.avg_func_length result.total_exports
    (result.doc_coverage *. 100.0);
  Printf.printf "\n%d files | %d lines | %d functions | %d exports | doc %.0f%% | avg %.1f lines/function\n"
    (List.length result.files) result.total_lines result.total_functions
    result.total_exports (result.doc_coverage *. 100.0) result.avg_func_length;
  exit 0

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory to scan for .ml files")

let json =
  Arg.(value & flag & info ["json"] ~doc:"Output as JSON")

let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "stats" ~doc:"show code intelligence metrics"
    ~man:[`S "DESCRIPTION";
          `P "Scans all .ml files and reports lines of code, function count, \
              average function length, export count, and .mli coverage.";
          `P "With --json, outputs structured JSON instead of formatted text."])
  Term.(const run $ dir $ json $ quiet)
