(* Merge queue state types and transitions
 *
 * roerick note (|
 *   Defines the merge queue state machine and data structures.
 *   
 *   Queue item states:
 *   - PENDING: Submitted, waiting for merge
 *   - READY: No conflicts with queued items before it
 *   - BLOCKED: Conflicts with earlier queue item
 *   - MERGING: Currently being processed
 *   - MERGED: Successfully merged to main
 *   - REJECTED: Merge failed, worktree preserved
 * |) *)

(** Unique queue item ID *)
type item_id = string

(** State of a queue item *)
type queue_state =
  | Pending     (** Submitted, waiting for merge *)
  | Ready       (** No conflicts with earlier items *)
  | Blocked     (** Conflicts with earlier queue item *)
  | Merging     (** Currently being processed *)
  | Merged      (** Successfully merged to main *)
  | Rejected    (** Merge failed, worktree preserved *)

(** A single item in the merge queue *)
type queue_item = {
  id : item_id;
  state : queue_state;
  branch : string;              (** Git branch name *)
  worktree_path : string;       (** Path to worktree *)
  summary : string;             (** Description of changes *)
  affected_files : string list; (** Files this item touches *)
  confidence : float;           (** 0.0-1.0 confidence score *)
  submitted_at : string;        (** ISO 8601 timestamp *)
  merged_at : string option;    (** When merged, if merged *)
  rejected_reason : string option; (** Why rejected, if rejected *)
}

(** The full merge queue *)
type merge_queue = {
  items : queue_item list;      (** All queue items, in order *)
  next_merge_id : item_id option;  (** Which item is being processed *)
}

(** Event for queue transitions *)
type queue_event =
  | Submit of queue_item       (** New item submitted *)
  | MarkReady of item_id       (** Item marked ready (conflicts resolved) *)
  | MarkBlocked of item_id     (** Item blocked by earlier item *)
  | StartMerge of item_id      (** Begin processing item *)
  | CompleteMerge of item_id   (** Item successfully merged *)
  | Reject of item_id * string (** Item rejected with reason *)
  | Cancel of item_id          (** Item manually cancelled *)
  | Refresh                    (** Recalculate all states *)

(** String representation of state *)
let string_of_state = function
  | Pending -> "PENDING"
  | Ready -> "READY"
  | Blocked -> "BLOCKED"
  | Merging -> "MERGING"
  | Merged -> "MERGED"
  | Rejected -> "REJECTED"

(** Parse state from string *)
let state_of_string = function
  | "PENDING" -> Some Pending
  | "READY" -> Some Ready
  | "BLOCKED" -> Some Blocked
  | "MERGING" -> Some Merging
  | "MERGED" -> Some Merged
  | "REJECTED" -> Some Rejected
  | _ -> None

(** Create a new empty queue *)
let empty_queue = { items = []; next_merge_id = None }

(** Find item by ID *)
let find_item queue id =
  List.find_opt (fun item -> item.id = id) queue.items

(** Find index of item in queue *)
let item_index queue id =
  let rec find i = function
    | [] -> None
    | item :: rest -> if item.id = id then Some i else find (i + 1) rest
  in
  find 0 queue.items

(** Get items that come before a given item *)
let predecessors queue id =
  match item_index queue id with
  | None -> []
  | Some idx ->
      queue.items
      |> List.mapi (fun i item -> (i, item))
      |> List.filter (fun (i, _) -> i < idx)
      |> List.map snd

(** Get all items in a specific state *)
let items_in_state queue state =
  List.filter (fun item -> item.state = state) queue.items

(** Count items by state *)
let count_by_state queue =
  let counts = [
    ("pending", List.length (items_in_state queue Pending));
    ("ready", List.length (items_in_state queue Ready));
    ("blocked", List.length (items_in_state queue Blocked));
    ("merging", List.length (items_in_state queue Merging));
    ("merged", List.length (items_in_state queue Merged));
    ("rejected", List.length (items_in_state queue Rejected));
  ] in
  counts

(** Check if an item can transition to a new state *)
let can_transition item new_state =
  match item.state, new_state with
  | Pending, Ready -> true
  | Pending, Blocked -> true
  | Ready, Merging -> true
  | Ready, Blocked -> true  (* Can become blocked if new conflicts *)
  | Blocked, Ready -> true
  | Merging, Merged -> true
  | Merging, Rejected -> true
  | _, Rejected -> true     (* Can reject from any state (cancellation) *)
  | _ -> false

(** Update item state (returns new queue) *)
let update_state queue id new_state =
  { queue with
    items = List.map (fun item ->
      if item.id = id then
        let merged_at = 
          if new_state = Merged && item.state <> Merged then
            let now = Unix.gmtime (Unix.time ()) in
            Some (Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
              (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday
              now.tm_hour now.tm_min now.tm_sec)
          else item.merged_at
        in
        { item with state = new_state; merged_at }
      else item
    ) queue.items
  }

(** Remove item from queue *)
let remove_item queue id =
  { queue with items = List.filter (fun item -> item.id <> id) queue.items }

(** Add new item to end of queue *)
let add_item queue item =
  { queue with items = queue.items @ [item] }

(** Get next ready item (oldest ready item) *)
let next_ready queue =
  queue.items
  |> List.filter (fun item -> item.state = Ready)
  |> List.sort (fun a b -> String.compare a.submitted_at b.submitted_at)
  |> (function [] -> None | x :: _ -> Some x)
