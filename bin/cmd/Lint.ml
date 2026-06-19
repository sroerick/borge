open Borge_lib

let run dir json quiet code_doc mechanical literacy =
  let result = Lint.run ~code_doc ~mechanical ~literacy dir in
  if json then begin
    Printf.printf "%s\n" (Yojson.Basic.to_string (Json_out.lint result));
    exit (if result.error_count > 0 then 1 else 0)
  end;
  if not quiet then Printf.printf "Linting .borg files in '%s'...\n\n" dir;
  let print_issue = function
    | Lint.Invalid_status { path; value; line; col } ->
        Printf.printf "  ✗ %s:%d:%d: invalid status value '%s'\n" path line col value
    | Lint.Duplicate_project_name { name; paths } ->
        Printf.printf "  ✗ duplicate project name '%s' in: %s\n" name (String.concat ", " paths)
    | Lint.Orphaned_file { path } ->
        Printf.printf "  ✗ %s: orphaned .borg file\n" path
    | Lint.Unknown_comment_type { path; type_name; line } ->
        Printf.printf "  ⚠ %s:%d: unknown comment type '%s'\n" path line type_name
    | Lint.Missing_comment_value { path; author; type_name; line } ->
        let type_str = match type_name with Some t -> Printf.sprintf " %s" t | None -> "" in
        Printf.printf "  ⚠ %s:%d: comment by '%s%s' missing value\n" path line author type_str
    | Lint.No_inline_on_root { path } ->
        Printf.printf "  ⚠ %s: (no-inline) on root file\n" path
    | Lint.Deleted_human_comment { path; author; _ } ->
        Printf.printf "  ⚠ %s: human-authored comment by '%s' was deleted\n" path author
    | Lint.Inserted_human_comment { path; author; _ } ->
        Printf.printf "  ⚠ %s: human-authored comment by '%s' was inserted\n" path author
    | Lint.Pending_response { path; author; line; question } ->
        Printf.printf "  ⚠ %s:%d: unanswered ask by '%s': %s\n" path line author question
    | Lint.Db_validation { path; severity; message } ->
        let mark = match severity with `Error -> "✗" | `Warning -> "⚠" in
        Printf.printf "  %s %s: DB: %s\n" mark path message
    | Lint.Ui_validation { path; severity; message } ->
        let mark = match severity with `Error -> "✗" | `Warning -> "⚠" in
        Printf.printf "  %s %s: UI: %s\n" mark path message
    | Lint.Undocumented_binding { path; name; line } ->
        Printf.printf "  ⚠ %s:%d: undocumented binding '%s'\n" path line name
    | Lint.Stale_doc_comment { path; name; line } ->
        Printf.printf "  ⚠ %s:%d: drifted doc comment on '%s'\n" path line name
    | Lint.Missing_literacy_score { path; name; line } ->
        Printf.printf "  ⚠ %s:%d: missing literacy score on '%s'\n" path line name
    | Lint.Implausible_literacy_score { path; name; line; dimension; claimed; reason } ->
        Printf.printf "  ⚠ %s:%d: %s=%d on '%s' is implausible — %s\n"
          path line dimension claimed name reason
    | Lint.Unsafe_call { path; line; call; severity; suggestion } ->
        let mark = match severity with `Error -> "✗" | `Warning -> "⚠" in
        Printf.printf "  %s %s:%d: unsafe call '%s' — %s\n" mark path line call suggestion
  in
  if quiet then begin
    (* In quiet mode, only print errors to stderr, suppress warnings *)
    List.iter (fun issue ->
      match issue with
      | Lint.Invalid_status _ | Lint.Duplicate_project_name _ | Lint.Orphaned_file _
      | Lint.Unsafe_call { severity = `Error; _ } ->
          print_issue issue
      | _ -> ()
    ) result.issues
  end else begin
    List.iter print_issue result.issues;
    Printf.printf "\n%d errors, %d warnings\n" result.error_count result.warning_count;
  end;
  if result.error_count > 0 then exit 1 else exit 0

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory to lint")

let json =
  Arg.(value & flag & info ["json"] ~doc:"Output as JSON")

let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

let code_doc =
  Arg.(value & flag & info ["code-doc"] ~doc:"Also check .ml files for undocumented bindings")

let mechanical =
  Arg.(value & flag & info ["mechanical"] ~doc:"Check .ml/.mli files for unsafe stdlib calls")

let literacy =
  Arg.(value & flag & info ["literacy"; "l"] ~doc:"Check .ml files for literacy scores")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "lint" ~doc:"semantic validation of .borg files"
    ~man:[`S "DESCRIPTION";
          `P "Checks for: invalid status values, duplicate project names, \
              unknown comment types, missing comment values, deleted human \
              comments, pending ask/response slots, and orphaned files.";
          `P "With --code-doc, also checks .ml files for undocumented \
              exported bindings and drifted doc comments.";
          `P "With --mechanical, also checks .ml/.mli files for unsafe \
              standard library calls (List.hd, List.assoc, Hashtbl.find, etc.).";
          `P "With --literacy, also checks .ml files for missing or implausible \
              literacy scores on doc comments.";
          `P "With --json, outputs structured JSON instead of formatted text."])
  Term.(const run $ dir $ json $ quiet $ code_doc $ mechanical $ literacy)
