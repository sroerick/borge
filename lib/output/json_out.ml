(** JSON serialization for borge result types using yojson.

    Each command produces typed result data. This module converts
    those types to yojson for the --json flag. *)


(* ── Report ────────────────────────────────────────────────────── *)

(* exempt doc: simple JSON converter - name is self-documenting *)
let file_stats (r : Report.file_stats) =
  `Assoc [
    "path", `String r.path;
    "project_name", (match r.project_name with Some n -> `String n | None -> `Null);
    "implemented", `Int r.implemented;
    "in_progress", `Int r.in_progress;
    "planned", `Int r.planned;
  ]

(* exempt doc: simple JSON converter - name is self-documenting *)
let totals (t : Report.totals) =
  `Assoc [
    "implemented", `Int t.implemented;
    "in_progress", `Int t.in_progress;
    "planned", `Int t.planned;
  ]

(* exempt doc: simple JSON converter - name is self-documenting *)
let report (r : Report.result) =
  `Assoc [
    "files", `List (List.map file_stats r.files);
    "totals", totals r.totals;
    "orphans", `List (List.map (fun s -> `String s) r.orphans);
  ]

(* ── Check ────────────────────────────────────────────────────── *)

(* exempt doc: simple pattern match to JSON - name is self-documenting *)
let file_result = function
  | Check.Ok { path; project_name; form_count } ->
    `Assoc [
      "status", `String "ok";
      "path", `String path;
      "project_name", `String project_name;
      "form_count", `Int form_count;
    ]
  | Check.Error { path; message } ->
    `Assoc [
      "status", `String "error";
      "path", `String path;
      "message", `String message;
    ]

(* exempt doc: simple JSON converter - name is self-documenting *)
let check (r : Check.result) =
  `Assoc [
    "files", `List (List.map file_result r.files);
    "warnings", `List (List.map (function
      | Check.Orphan path -> `Assoc [ "type", `String "orphan"; "path", `String path ]
    ) r.warnings);
    "passed", `Int r.passed;
    "failed", `Int r.failed;
  ]

(* ── Lint ──────────────────────────────────────────────────────── *)

(* exempt doc: large pattern match to JSON - purpose clear from context *)
let lint_issue = function
  | Lint.Invalid_status { path; value; line; col } ->
    `Assoc [
      "type", `String "invalid_status";
      "severity", `String "error";
      "path", `String path;
      "value", `String value;
      "line", `Int line;
      "col", `Int col;
    ]
  | Lint.Duplicate_project_name { name; paths } ->
    `Assoc [
      "type", `String "duplicate_project_name";
      "severity", `String "error";
      "name", `String name;
      "paths", `List (List.map (fun s -> `String s) paths);
    ]
  | Lint.Orphaned_file { path } ->
    `Assoc [
      "type", `String "orphaned_file";
      "severity", `String "error";
      "path", `String path;
    ]
  | Lint.No_inline_on_root { path } ->
    `Assoc [
      "type", `String "no_inline_on_root";
      "severity", `String "warning";
      "path", `String path;
    ]
  | Lint.Unknown_comment_type { path; type_name; line } ->
    `Assoc [
      "type", `String "unknown_comment_type";
      "severity", `String "warning";
      "path", `String path;
      "comment_type", `String type_name;
      "line", `Int line;
    ]
  | Lint.Missing_comment_value { path; author; type_name; line } ->
    `Assoc [
      "type", `String "missing_comment_value";
      "severity", `String "warning";
      "path", `String path;
      "author", `String author;
      "comment_type", (match type_name with Some t -> `String t | None -> `Null);
      "line", `Int line;
    ]
  | Lint.Deleted_human_comment { path; author; line } ->
    `Assoc [
      "type", `String "deleted_human_comment";
      "severity", `String "warning";
      "path", `String path;
      "author", `String author;
      "line", `Int line;
    ]
  | Lint.Pending_response { path; author; line; question } ->
    `Assoc [
      "type", `String "pending_response";
      "severity", `String "warning";
      "path", `String path;
      "author", `String author;
      "line", `Int line;
      "question", `String question;
    ]
  | Lint.Db_validation { path; severity; message } ->
    `Assoc [
      "type", `String "db_validation";
      "severity", `String (match severity with `Error -> "error" | `Warning -> "warning");
      "path", `String path;
      "message", `String message;
    ]
  | Lint.Ui_validation { path; severity; message } ->
    `Assoc [
      "type", `String "ui_validation";
      "severity", `String (match severity with `Error -> "error" | `Warning -> "warning");
      "path", `String path;
      "message", `String message;
    ]
  | Lint.Undocumented_binding { path; name; line } ->
    `Assoc [
      "type", `String "undocumented_binding";
      "path", `String path;
      "name", `String name;
      "line", `Int line;
    ]
  | Lint.Stale_doc_comment { path; name; line } ->
    `Assoc [
      "type", `String "stale_doc_comment";
      "path", `String path;
      "name", `String name;
      "line", `Int line;
    ]
  | Lint.Inserted_human_comment { path; author; line } ->
    `Assoc [
      "type", `String "inserted_human_comment";
      "severity", `String "warning";
      "path", `String path;
      "author", `String author;
      "line", `Int line;
    ]
  | Lint.Unsafe_call { path; line; call; severity; suggestion } ->
    `Assoc [
      "type", `String "unsafe_call";
      "severity", `String (match severity with `Error -> "error" | `Warning -> "warning");
      "path", `String path;
      "line", `Int line;
      "call", `String call;
      "suggestion", `String suggestion;
    ]

(* exempt doc: simple JSON wrapper - name is self-documenting *)
let lint (r : Lint.lint_result) =
  `Assoc [
    "issues", `List (List.map lint_issue r.issues);
    "error_count", `Int r.error_count;
    "warning_count", `Int r.warning_count;
  ]

(* ── Drift ─────────────────────────────────────────────────────── *)

(* exempt doc: simple JSON converter - name is self-documenting *)
let spec_drift (d : Drift.spec_drift) =
  `Assoc [
    "path", `String d.path;
    "section_name", `String d.section_name;
    "description", `String d.description;
  ]

(* exempt doc: simple JSON converter - name is self-documenting *)
let code_drift_item (d : Drift.code_drift_item) =
  `Assoc [
    "path", `String d.path;
    "kind", `String d.kind;
    "name", `String d.name;
  ]

(* exempt doc: simple JSON converter - name is self-documenting *)
let structural_drift_item (d : Drift.structural_drift_item) =
  `Assoc [
    "description", `String d.description;
  ]

(* exempt doc: simple JSON wrapper - name is self-documenting *)
let drift (r : Drift.drift_result) =
  `Assoc [
    "spec_drift", `List (List.map spec_drift r.spec_drift);
    "code_drift", `List (List.map code_drift_item r.code_drift);
    "structural_drift", `List (List.map structural_drift_item r.structural_drift);
  ]

(* ── Stats ─────────────────────────────────────────────────────── *)

(* exempt doc: simple JSON converter - name is self-documenting *)
let file_metrics (m : Stats.file_metrics) =
  `Assoc [
    "path", `String m.path;
    "module_name", `String m.module_name;
    "lines", `Int m.lines;
    "functions", `Int m.functions;
    "avg_length", `Float m.avg_length;
    "exports", `Int m.exports;
    "documented", `Int m.documented;
    "doc_coverage", `Float m.doc_coverage;
    "has_mli", `Bool m.has_mli;
  ]

(* exempt doc: simple JSON wrapper - name is self-documenting *)
let stats (r : Stats.project_metrics) =
  `Assoc [
    "files", `List (List.map file_metrics r.files);
    "total_lines", `Int r.total_lines;
    "total_functions", `Int r.total_functions;
    "total_exports", `Int r.total_exports;
    "total_documented", `Int r.total_documented;
    "doc_coverage", `Float r.doc_coverage;
    "avg_func_length", `Float r.avg_func_length;
  ]

(* ── Balance ───────────────────────────────────────────────────── *)

(* exempt doc: simple JSON converter - name is self-documenting *)
let balance_pos (p : Borge_lang.Balance.pos) =
  `Assoc [ "line", `Int p.line; "col", `Int p.col ]

(* exempt doc: simple JSON converter - name is self-documenting *)
let balance_paren_frame (f : Borge_lang.Balance.paren_frame) =
  `Assoc [
    "open_pos", balance_pos f.open_pos;
    "keyword", (match f.keyword with Some k -> `String k | None -> `Null);
  ]

(* exempt doc: pattern match to JSON - purpose clear from context *)
let balance_error_detail = function
  | Borge_lang.Balance.Unexpected_close (p, None) ->
    `Assoc [
      "type", `String "unexpected_close";
      "pos", balance_pos p;
    ]
  | Borge_lang.Balance.Unexpected_close (p, Some open_p) ->
    `Assoc [
      "type", `String "unexpected_close";
      "pos", balance_pos p;
      "matching_open", balance_pos open_p;
    ]
  | Borge_lang.Balance.Unclosed_parens frames ->
    `Assoc [
      "type", `String "unclosed_parens";
      "frames", `List (List.map balance_paren_frame (List.rev frames));
    ]
  | Borge_lang.Balance.Unclosed_context (p, ctx) ->
    `Assoc [
      "type", `String "unclosed_context";
      "pos", balance_pos p;
      "context", `String ctx;
    ]

(* exempt doc: simple JSON converter - name is self-documenting *)
let balance path = function
  | Borge_lang.Balance.Balanced { max_depth } ->
    `Assoc [
      "path", `String path;
      "status", `String "balanced";
      "max_depth", `Int max_depth;
    ]
  | Borge_lang.Balance.Imbalanced details ->
    `Assoc [
      "path", `String path;
      "status", `String "imbalanced";
      "errors", `List (List.map balance_error_detail details);
    ]
