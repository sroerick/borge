open Borge_lib

let run dir =
  Printf.printf "Borge Review — advisory report for '%s'\n\n" dir;
  (* Run lint *)
  let lint_result = Lint.run dir in
  Printf.printf "=== Lint ===\n";
  if lint_result.issues = [] then
    Printf.printf "  Clean — no issues found.\n"
  else begin
    Printf.printf "  %d issues (%d errors, %d warnings):\n"
      (List.length lint_result.issues) lint_result.error_count lint_result.warning_count;
    List.iter (function
      | Lint.Invalid_status { path; value; line; col } ->
          Printf.printf "  ✗ %s:%d:%d: invalid status '%s'\n" path line col value
      | Lint.Duplicate_project_name { name; paths } ->
          Printf.printf "  ✗ duplicate project name '%s'\n" name;
          Printf.printf "    in: %s\n" (String.concat ", " paths)
      | Lint.Orphaned_file { path } ->
          Printf.printf "  ✗ %s: orphaned .borg file\n" path
      | Lint.Unknown_comment_type { path; type_name; line } ->
          Printf.printf "  ⚠ %s:%d: unknown comment type '%s'\n" path line type_name
      | Lint.Missing_comment_value { path; author; type_name; line } ->
          let type_str = match type_name with Some t -> Printf.sprintf " %s" t | None -> "" in
          Printf.printf "  ⚠ %s:%d: comment by '%s%s' missing value\n" path line author type_str
      | Lint.Deleted_human_comment { path; author; _ } ->
          Printf.printf "  ⚠ %s: comment by '%s' was deleted\n" path author
      | Lint.Pending_response { path; author; line; question } ->
          Printf.printf "  ⚠ %s:%d: unanswered ask by '%s': %s\n" path line author question
      | Lint.No_inline_on_root { path } ->
          Printf.printf "  ⚠ %s: (no-inline) on root file\n" path
    ) lint_result.issues
  end;
  (* Run drift *)
  let drift_result = Drift.run dir in
  Printf.printf "\n=== Drift ===\n";
  Printf.printf "  Spec drift: %d sections\n" (List.length drift_result.spec_drift);
  Printf.printf "  Code drift: %d items\n" (List.length drift_result.code_drift);
  Printf.printf "  Structural drift: %d items\n" (List.length drift_result.structural_drift);
  List.iter (fun d ->
    Printf.printf "    ▷ %s\n" d.Drift.description
  ) drift_result.structural_drift;
  (* Summary *)
  let total = lint_result.error_count + lint_result.warning_count +
              List.length drift_result.spec_drift + List.length drift_result.code_drift +
              List.length drift_result.structural_drift in
  Printf.printf "\n=== Summary ===\n";
  Printf.printf "  Total items: %d\n" total;
  Printf.printf "  This is advisory — review and act on items as needed.\n";
  exit 0

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory to review")

let cmd =
  Cmd.v (Cmd.info "review" ~doc:"advisory review of .borg project health"
    ~man:[`S "DESCRIPTION";
          `P "Runs lint and drift checks, producing an advisory report. \
              Unlike borge check (pass/fail), this is for human review — \
              it surfaces information, not verdicts."])
  Term.(const run $ dir)

let () = ignore (Cmd.eval cmd : int)
