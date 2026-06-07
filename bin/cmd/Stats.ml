open Borge_lib

(** Pad a string to the right with spaces to reach length n *)
let pad_right n s =
  let len = String.length s in
  if len >= n then s else s ^ String.make (n - len) ' '

(** Run documentation coverage analysis and print results *)
let run_coverage dir =
  Printf.printf "Documentation Coverage — %s\n\n" dir;
  let coverage = Doc_coverage.calculate_dir_coverage dir in
  if coverage.files = [] then begin
    Printf.printf "No .ml files found.\n";
    exit 0
  end;
  Printf.printf "Overall: %.1f%% (%d/%d documented, %d exempt, %d undocumented)\n\n"
    coverage.overall_percent
    coverage.total_documented
    (coverage.total_bindings - coverage.total_exempt)
    coverage.total_exempt
    coverage.total_undocumented;
  Printf.printf "Per-file coverage:\n";
  List.iter (fun (fc : Doc_coverage.file_coverage) ->
    Printf.printf "  %-40s %6.1f%% (%d/%d doc, %d undoc)%s\n"
      (Filename.basename fc.path)
      fc.coverage_percent
      fc.documented
      (fc.total_bindings - fc.exempt)
      fc.undocumented
      (if fc.undocumented_names <> [] then
        let rec take n = function
          | [] -> []
          | _ when n <= 0 -> []
          | x :: xs -> x :: take (n - 1) xs
        in
        ": " ^ String.concat ", " (take 3 fc.undocumented_names)
      else "")
  ) coverage.files;
  if coverage.total_undocumented > 0 then exit 1 else exit 0

let run dir json quiet coverage =
  if coverage then run_coverage dir
  else
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

(* exempt doc *)
let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory to scan for .ml files")

(* exempt doc *)
let json =
  Arg.(value & flag & info ["json"] ~doc:"Output as JSON")

(* exempt doc *)
let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

(* exempt doc *)
let coverage =
  Arg.(value & flag & info ["coverage"; "c"] ~doc:"Show documentation coverage analysis")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "stats" ~doc:"show code intelligence metrics"
    ~man:[`S "DESCRIPTION";
          `P "Scans all .ml files and reports lines of code, function count, \
              average function length, export count, and .mli coverage.";
          `P "With --json, outputs structured JSON instead of formatted text.";
          `P "With --coverage, shows documentation coverage analysis."])
  Term.(const run $ dir $ json $ quiet $ coverage)
