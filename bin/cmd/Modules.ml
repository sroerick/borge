(** borge modules — check distributed module specs

    Discovers module-level .borg files and checks referential
    integrity between spec exports and code implementations. *)

open Borge_lib

(** Check all module specs for referential integrity *)
let run_check ~dir ~json =
  let specs = Module_spec.load_all_specs dir in

  if specs = [] then begin
    Printf.printf "No module specs found in %s\n" dir;
    exit 0
  end;

  let reports = List.map (fun (spec : Module_spec.module_def) ->
    let impl_files = Module_spec.find_implementation_files spec.path in
    let merged_issues = List.fold_left (fun acc impl_path ->
      let issues = Module_spec.check_integrity spec impl_path in
      let (missing1, undoc1) = acc in
      let (missing2, undoc2) = issues in
      (missing1 @ missing2, undoc1 @ undoc2)
    ) ([], []) impl_files in
    (spec, merged_issues)
  ) specs in

  if json then begin
    (* TODO: JSON output *)
    Printf.printf "{\"modules\": []}\n"
  end else begin
    Printf.printf "Module spec integrity check for '%s'\n\n" dir;

    let total_issues = ref 0 in
    List.iter (fun ((spec : Module_spec.module_def), issues) ->
      let report = Module_spec.render_report spec.module_name issues in
      Printf.printf "%s\n\n" report;
      let (missing, undoc) = issues in
      total_issues := !total_issues + List.length missing + List.length undoc
    ) reports;

    Printf.printf "Total issues: %d\n" !total_issues;
    if !total_issues > 0 then exit 1 else exit 0
  end

(** List all discovered module specs without checking *)
let run_list ~dir =
  let specs = Module_spec.load_all_specs dir in
  if specs = [] then
    Printf.printf "No module specs found.\n"
  else begin
    Printf.printf "Module specs found:\n";
    List.iter (fun (spec : Module_spec.module_def) ->
      Printf.printf "  %s (%d exports)\n" spec.module_name (List.length spec.exports)
    ) specs
  end

(** Main entry point: route to check/list/function-names *)
let run function_names path dir list json =
  if list then run_list ~dir
  else if function_names then begin
    match path with
    | None ->
        Printf.eprintf "Error: --functions requires --path FILE\n";
        exit 1
    | Some p ->
        let names = Borg_comment.function_names p in
        Printf.printf "Functions in %s:\n" p;
        List.iter (Printf.printf "  - %s\n") names
  end
  else run_check ~dir ~json

open Cmdliner

(* exempt doc *)
let function_names_flag =
  Arg.(value & flag & info ["functions"] ~doc:"List functions in a file")

(* exempt doc *)
let path =
  Arg.(value & opt (some string) None & info ["path"] ~docv:"FILE"
    ~doc:"Path to OCaml file")

(* exempt doc *)
let dir =
  Arg.(value & opt string "." & info ["dir"; "d"] ~docv:"DIR"
    ~doc:"Project directory")

(* exempt doc *)
let list_flag =
  Arg.(value & flag & info ["list"; "l"] ~doc:"List discovered module specs")

(* exempt doc *)
let json =
  Arg.(value & flag & info ["json"] ~doc:"Output as JSON")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "modules" ~doc:"check distributed module specs"
    ~man:[`S "DESCRIPTION";
          `P "Discovers module-level .borg files and checks integrity.";
          `P "Verifies that exported functions exist in code (and vice versa).";
          `P "Non-semantic check: only name referential integrity.";
          `S "EXAMPLES";
          `P "borge modules                    # check all modules";
          `P "borge modules --list             # list discovered specs";
          `P "borge modules --functions --path lib/core/spec.ml"])
  Term.(const run $ function_names_flag $ path $ dir $ list_flag $ json)
