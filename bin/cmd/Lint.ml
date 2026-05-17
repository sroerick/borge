open Borge_lib

let run dir json =
  let result = Lint.run dir in
  if json then begin
    Printf.printf "%s\n" (Yojson.Basic.to_string (Json_out.lint result));
    exit (if result.error_count > 0 then 1 else 0)
  end;
  Printf.printf "Linting .borg files in '%s'...\n\n" dir;
  List.iter (function
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
    | Lint.Pending_response { path; author; line; question } ->
        Printf.printf "  ⚠ %s:%d: unanswered ask by '%s': %s\n" path line author question
  ) result.issues;
  Printf.printf "\n%d errors, %d warnings\n" result.error_count result.warning_count;
  if result.error_count > 0 then exit 1 else exit 0

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory to lint")

let json =
  Arg.(value & flag & info ["json"] ~doc:"Output as JSON")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "lint" ~doc:"semantic validation of .borg files"
    ~man:[`S "DESCRIPTION";
          `P "Checks for: invalid status values, duplicate project names, \
              unknown comment types, missing comment values, deleted human \
              comments, pending ask/response slots, and orphaned files.";
          `P "With --json, outputs structured JSON instead of formatted text."])
  Term.(const run $ dir $ json)

