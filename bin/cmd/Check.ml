open Borge_lib

(** Print a human-readable summary of check results *)
let print_result (result : Check.result) =
  List.iter (function
    | Check.Ok { path; project_name; form_count } ->
        Printf.printf "  ✓ %s: project '%s' (%d forms)\n" path project_name form_count
    | Check.Error { path; message } ->
        Printf.printf "  ✗ %s: %s\n" path message
  ) result.files;
  List.iter (function
    | Check.Orphan path ->
        Printf.printf "  ⚠ %s: orphaned .borg file (no parent, no no-inline)\n" path
  ) result.warnings;
  Printf.printf "\n%d files checked. %d passed. %d failed. %d warnings.\n"
    (List.length result.files) result.passed result.failed (List.length result.warnings)

(** Run all checks on a worktree: diff clean, balance, check, build, test *)
let run_worktree dir quiet =
  let failures = ref [] in
  (* 1. git diff is clean *)
  let cmd = Printf.sprintf "git -C %s diff --quiet 2>/dev/null" (Filename.quote dir) in
  if Sys.command cmd <> 0 then
    failures := "git diff is not clean (uncommitted changes)" :: !failures;
  (* 2. borge balance passes on all .borg files *)
  let borg_files = File_utils.find_borg_files dir in
  List.iter (fun path ->
    let input = File_utils.read_file path in
    match Borge_lang.Balance.check input with
    | Borge_lang.Balance.Balanced _ -> ()
    | Borge_lang.Balance.Imbalanced _ ->
        failures := Printf.sprintf "borge balance: %s is imbalanced" path :: !failures
  ) borg_files;
  (* 3. borge check passes *)
  let check_result = Check.run dir in
  if check_result.failed > 0 then
    failures := "borge check: some files failed" :: !failures;
  (* 4. dune build succeeds *)
  let cmd = Printf.sprintf "cd %s && dune build 2>/dev/null" (Filename.quote dir) in
  if Sys.command cmd <> 0 then
    failures := "dune build failed" :: !failures;
  (* 5. dune runtest passes *)
  let cmd = Printf.sprintf "cd %s && dune runtest 2>/dev/null" (Filename.quote dir) in
  if Sys.command cmd <> 0 then
    failures := "dune runtest failed" :: !failures;
  (* Report *)
  if !failures = [] then begin
    if not quiet then Printf.printf "✓ Worktree check passed — clean and ready.\n";
    exit 0
  end else begin
    Printf.eprintf "✗ Worktree check failed:\n";
    List.iter (fun f -> Printf.eprintf "  - %s\n" f) (List.rev !failures);
    exit 1
  end

let run dir worktree json quiet =
  if not (Sys.is_directory dir) then (
    Printf.eprintf "Error: '%s' is not a directory\n" dir;
    exit 2
  );
  if worktree then run_worktree dir quiet
  else begin
    let result = Check.run dir in
    if json then begin
      Printf.printf "%s\n" (Yojson.Basic.to_string (Json_out.check result));
      exit 0
    end;
    if not quiet then begin
      Printf.printf "Checking .borg files in '%s'...\n\n" dir;
      print_result result;
    end;
    if result.failed > 0 then exit 1 else exit 0
  end

open Cmdliner

(* exempt doc *)
let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory to scan for .borg files")

(* exempt doc *)
let worktree =
  Arg.(value & flag & info ["worktree"] ~doc:"Deterministic check: clean diff, balance, check, build, test")

(* exempt doc *)
let json =
  Arg.(value & flag & info ["json"] ~doc:"Output as JSON")

(* exempt doc *)
let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "check" ~doc:"recursive health check across all .borg files"
    ~man:[`S "DESCRIPTION";
          `P "Finds all .borg files recursively and checks that each one \
              parses correctly and has a project node.";
          `P "With --worktree, runs a deterministic pass/fail check: \
              git diff clean, balance passes, check passes, dune build, dune test.";
          `P "With --json, outputs structured JSON instead of formatted text."])
  Term.(const run $ dir $ worktree $ json $ quiet)

