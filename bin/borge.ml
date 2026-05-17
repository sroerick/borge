(** borge — a spec-driven code orchestrator *)

open Borge_cmd
open Cmdliner

let cmds = [
  Abort.cmd;
  Balance.cmd;
  Issue.cmd;
  Check.cmd;
  Commit.cmd;
  Diff.cmd;
  Drift.cmd;
  Execute.cmd;
  Fmt.cmd;
  Generate.cmd;
  Inline.cmd;
  Lint.cmd;
  Log.cmd;
  Make.cmd;
  Modules.cmd;
  Nodes.cmd;
  Normalize.cmd;
  Parse.cmd;
  Plan.cmd;
  Print.cmd;
  Report.cmd;
  Review.cmd;
  Stats.cmd;
  Undo.cmd;
  Version.cmd;
]

let () =
  let doc = "a spec-driven code orchestrator" in
  let man = [
    `S "DESCRIPTION";
    `P "Borge reads .borg files (sexp-based specs) and provides tools to \
        validate, format, check, and orchestrate your project's specification.";
    `P "Every .borg file is a git-tracked source of truth. Borge understands \
        the inline tree, comment integrity, and drift between spec and code.";
    `S "COMMANDS";
    `P "Run borge <command> --help for details on any command.";
    `I ("balance", "Check structural balance (run this FIRST after edits)");
    `I ("parse", "Full syntax validation");
    `I ("check", "Recursive health check (add --worktree for CI)");
    `I ("report", "Status summary dashboard");
    `I ("fmt", "Auto-format to canonical indentation");
    `I ("nodes", "AST dump with positions");
    `I ("inline", "Show the inline project tree");
    `I ("lint", "Semantic validation");
    `I ("drift", "Detect spec/code/structural drift");
    `I ("normalize", "Fmt all + accept comment deletions");
    `I ("execute", "Execute (implement) a planned section");
    `I ("generate", "Generate spec sections or code from specs");
    `I ("stats", "Code intelligence metrics (lines, functions, exports)");
    `I ("review", "Advisory review (lint + drift)");
    `I ("log", "Git log for .borg changes");
    `I ("diff", "Git diff with status annotations");
    `I ("undo", "Revert last commit");
    `S "WORKFLOW";
    `P "1. Edit .borg files in your editor";
    `P "2. Run 'borge balance' to catch structural errors";
    `P "3. Run 'borge fmt' to canonicalize indentation";
    `P "4. Run 'borge check' to validate the whole project";
    `P "5. Run 'borge report' to see what's done and what's next";
    `S "EXIT CODES";
    `P "0  success (no issues found)";
    `P "1  issues found (check failed, lint errors, etc.)";
    `P "2  usage error (wrong arguments)";
  ] in
  let info = Cmd.info "borge" ~version:"0.1.0" ~doc ~man in
  let cmd = Cmd.group info cmds in
  exit (Cmd.eval cmd)
