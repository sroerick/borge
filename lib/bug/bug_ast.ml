(* Bug AST - structured representation of .borge-bug files *)

type status =
  | Triage
  | Open
  | In_progress
  | Resolved
  | Closed

(* agent note (|
 *   WHAT: Convert a bug status to its string representation for
 *   serialization in .borge-bug files.
 *
 *   WHY: Bug files need to persist status as text.
 * |) *)
let string_of_status = function
  | Triage -> "triage"
  | Open -> "open"
  | In_progress -> "in-progress"
  | Resolved -> "resolved"
  | Closed -> "closed"

(* agent note (|
 *   WHAT: Parse a string into a bug status, handling both
 *   hyphenated and underscore variants.
 *
 *   WHY: Reading bug files requires converting text back to status type.
 * |) *)
let status_of_string = function
  | "triage" -> Some Triage
  | "open" -> Some Open
  | "in-progress" | "in_progress" -> Some In_progress
  | "resolved" -> Some Resolved
  | "closed" -> Some Closed
  | _ -> None

type resolution = {
  when_ : string;
  how : string;
  by : string;
}

type bug = {
  id : string;
  title : string;
  status : status;
  created : string;
  filed_by : string;
  doc : string;
  affects_section : string option;
  relates_to : string list;
  drift_report_id : string option;
  resolution : resolution option;
}

(* agent note (|
 *   WHAT: Generate a unique bug ID using the current timestamp.
 *   Format: BORGE-{unix_timestamp}
 *
 *   WHY: New bugs need IDs for tracking and file naming.
 * |) *)
let make_id () =
  let ts = string_of_int (int_of_float (Unix.time ())) in
  "BORGE-" ^ ts

(* agent note (|
 *   WHAT: Create an empty bug record with default/placeholder values.
 *
 *   WHY: Starting point for creating a new bug.
 * |) *)
let empty_bug = {
  id = "";
  title = "";
  status = Triage;
  created = "";
  filed_by = "";
  doc = "";
  affects_section = None;
  relates_to = [];
  drift_report_id = None;
  resolution = None;
}
