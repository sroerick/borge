(* Semantic review types for borge review command
 *
 * roerick note (|
 *   Defines data types for semantic review findings and documentation coverage.
 *   These types map directly to the (semantic-review ...) and (documentation ...)
 *   blocks in .borg.meta files per meta.borg spec.
 * |) *)

(** Confidence level for a finding *)
type confidence = High | Medium | Low

(** Documentation accuracy assessment *)
type accuracy = High | Medium | Low

(** Documentation status — whether the doc matches the implementation *)
type doc_status = Doc_accurate | Doc_drifted | Doc_missing

(** Internal consistency assessment *)
type consistency = Cons_consistent | Cons_questionable | Cons_inconsistent

(** Signature match assessment *)
type signature_match = Accurate | Mismatch | Unknown

(** Behavior coverage assessment *)
type behavior_coverage = Complete | Partial | Missing

(** Per-function semantic finding *)
type function_finding = {
  name : string;
  doc_present : bool;
  doc_status : doc_status;
  doc_accuracy : accuracy;
  consistency : consistency;
  internal_issues : string option;  (* None or "none" means no issues *)
  signature_match : signature_match;
  behavior_coverage : behavior_coverage;
  structural_issues : string option;  (* None or "none" means no issues *)
  confidence : confidence;
  checked_at : string;  (* ISO 8601 timestamp *)
}

(** Semantic review block for a file *)
type semantic_review = {
  source_file : string;
  reviewed_at : string;  (* ISO 8601 timestamp *)
  reviewer : string;     (* e.g., "agent:semantic-reviewer" *)
  functions : function_finding list;
}

(** Documentation coverage for a file *)
type documentation = {
  analyzed_at : string;  (* ISO 8601 timestamp *)
  file : string;
  total_bindings : int;
  documented : int;
  exempt : int;
  undocumented : int;
  coverage_percent : float;
  undocumented_names : string list;
}

(** Content hash for staleness detection *)
type content_hash = {
  file_path : string;
  hash : string;  (* e.g., SHA256 or MD5 *)
}

(** Convert doc_status to string *)
let string_of_doc_status : doc_status -> string = function
  | Doc_accurate -> "accurate"
  | Doc_drifted -> "drifted"
  | Doc_missing -> "missing"

(** Convert consistency to string *)
let string_of_consistency : consistency -> string = function
  | Cons_consistent -> "consistent"
  | Cons_questionable -> "questionable"
  | Cons_inconsistent -> "inconsistent"

(** Convert confidence to string *)
let string_of_confidence : confidence -> string = function
  | High -> "high"
  | Medium -> "medium"
  | Low -> "low"

(** Convert accuracy to string *)
let string_of_accuracy : accuracy -> string = function
  | High -> "high"
  | Medium -> "medium"
  | Low -> "low"

(** Convert signature_match to string *)
let string_of_signature_match = function
  | Accurate -> "accurate"
  | Mismatch -> "mismatch"
  | Unknown -> "unknown"

(** Convert behavior_coverage to string *)
let string_of_behavior_coverage = function
  | Complete -> "complete"
  | Partial -> "partial"
  | Missing -> "missing"

(** Parse doc_status from string *)
let doc_status_of_string = function
  | "accurate" -> Some Doc_accurate
  | "drifted" -> Some Doc_drifted
  | "missing" -> Some Doc_missing
  | _ -> None

(** Parse consistency from string *)
let consistency_of_string = function
  | "consistent" -> Some Cons_consistent
  | "questionable" -> Some Cons_questionable
  | "inconsistent" -> Some Cons_inconsistent
  | _ -> None

(** Parse confidence from string *)
let confidence_of_string = function
  | "high" -> Some High
  | "medium" -> Some Medium
  | "low" -> Some Low
  | _ -> None

(** Parse accuracy from string *)
let accuracy_of_string = function
  | "high" -> Some High
  | "medium" -> Some Medium
  | "low" -> Some Low
  | _ -> None

(** Parse signature_match from string *)
let signature_match_of_string = function
  | "accurate" -> Some Accurate
  | "mismatch" -> Some Mismatch
  | "unknown" -> Some Unknown
  | _ -> None

(** Parse behavior_coverage from string *)
let behavior_coverage_of_string = function
  | "complete" -> Some Complete
  | "partial" -> Some Partial
  | "missing" -> Some Missing
  | _ -> None
