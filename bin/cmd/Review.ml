open Borge_lib

let run_agent_review dir quiet =
  if quiet then (
    (* Even in quiet mode, agent review produces output as that's its main purpose *)
    ()
  );
  Printf.printf "Borge Review — LLM code quality review for '%s'\n\n" dir;
  let results = Review_quality.run dir in
  if results = [] then begin
    Printf.printf "No implemented sections found to review.\n";
    exit 0
  end;
  List.iter (fun (section, ml_path, sexp) ->
    Printf.printf "=== %s (%s) ===\n" section ml_path;
    Printf.printf "%s\n\n" sexp
  ) results;
  exit 0

let run dir agent quiet =
  if agent then run_agent_review dir quiet
  else if quiet then exit 0
  else begin
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
  end

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory to review")

let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

let agent =
  Arg.(value & flag & info ["agent"] ~doc:
    "Run LLM-powered code quality review on implemented sections")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "review" ~doc:"advisory review of .borg project health"
    ~man:[`S "DESCRIPTION";
          `P "Runs lint and drift checks, producing an advisory report. \
              Unlike borge check (pass/fail), this is for human review — \
              it surfaces information, not verdicts.";
          `P "With --agent, runs LLM semantic review on each implemented \
              section, comparing the spec against the source code."])
  Term.(const run $ dir $ agent $ quiet)
