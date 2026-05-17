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

(** Signature match assessment *)
type signature_match = Accurate | Mismatch | Unknown

(** Behavior coverage assessment *)
type behavior_coverage = Complete | Partial | Missing

(** Per-function semantic finding *)
type function_finding = {
  name : string;
  doc_present : bool;
  doc_accuracy : accuracy;
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
