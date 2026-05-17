(* Bug AST - structured representation of .borge-bug files *)

type status =
  | Triage
  | Open
  | In_progress
  | Resolved
  | Closed

let string_of_status = function
  | Triage -> "triage"
  | Open -> "open"
  | In_progress -> "in-progress"
  | Resolved -> "resolved"
  | Closed -> "closed"

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

let make_id () =
  let ts = string_of_int (int_of_float (Unix.time ())) in
  "BORGE-" ^ ts

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
