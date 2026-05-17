(* File overlap analysis for merge queue conflict detection
 *
 * roerick note (|
 *   Analyzes file overlap between queue items to detect conflicts.
 *   Items that touch the same files cannot be merged in parallel.
 *   
 *   Two items conflict if they share any file in their affected_files lists.
 * |) *)

open Queue_types

(** Conflict between two queue items *)
type conflict = {
  item1_id : item_id;
  item2_id : item_id;
  conflicting_files : string list;  (** Files that both items touch *)
}

(** Check if two items have file overlap *)
let has_overlap item1 item2 =
  List.exists (fun f1 ->
    List.exists (fun f2 -> f1 = f2) item2.affected_files
  ) item1.affected_files

(** Find conflicting files between two items *)
let find_conflicting_files item1 item2 =
  List.filter (fun f1 ->
    List.exists (fun f2 -> f1 = f2) item2.affected_files
  ) item1.affected_files

(** Find all conflicts for a given item against other items *)
let find_conflicts_for item other_items =
  List.filter_map (fun other ->
    if has_overlap item other then
      Some {
        item1_id = item.id;
        item2_id = other.id;
        conflicting_files = find_conflicting_files item other;
      }
    else
      None
  ) other_items

(** Check if an item is blocked by any items before it in queue *)
let is_blocked_by queue item_id =
  match find_item queue item_id with
  | None -> []
  | Some item ->
      let predecessors = predecessors queue item_id in
      (* Only ACTIVE or MERGING predecessors can block *)
      let active_predecessors = List.filter (fun p ->
        p.state = Pending || p.state = Ready || p.state = Merging
      ) predecessors in
      find_conflicts_for item active_predecessors

(** Recalculate Ready/Blocked states for all items
    
    Rules:
    - First item is always Ready (unless already Merged/Rejected)
    - Item is Blocked if it conflicts with any Ready/Merging item before it
    - Item is Ready if no conflicts with Ready/Merging items before it *)
let recalculate_states queue =
  let rec process items processed =
    match items with
    | [] -> List.rev processed
    | item :: rest ->
        if item.state = Merged || item.state = Rejected then
          (* Keep terminal states *)
          process rest (item :: processed)
        else
          (* Check for conflicts with active processed items *)
          let active_processed = List.filter (fun p ->
            p.state = Ready || p.state = Merging
          ) processed in
          let conflicts = find_conflicts_for item active_processed in
          let new_state = if conflicts = [] then Ready else Blocked in
          let new_item = { item with state = new_state } in
          process rest (new_item :: processed)
  in
  { queue with items = process queue.items [] }

(** Find all pairs of conflicting items in queue *)
let find_all_conflicts queue =
  let rec find pairs = function
    | [] -> pairs
    | item :: rest ->
        let new_pairs = List.filter_map (fun other ->
          if has_overlap item other then
            Some {
              item1_id = item.id;
              item2_id = other.id;
              conflicting_files = find_conflicting_files item other;
            }
          else None
        ) rest in
        find (new_pairs @ pairs) rest
  in
  find [] queue.items

(** Group items into batches that can be merged in parallel

    Items in same batch must not conflict with each other.
    Items in later batches must wait for all earlier batches. *)
let group_into_batches queue =
  let items = List.filter (fun i -> 
    i.state = Ready || i.state = Pending || i.state = Blocked
  ) queue.items in
  
  let rec group batches remaining =
    if remaining = [] then List.rev batches
    else
      (* Find largest set of non-conflicting items *)
      let rec find_batch acc candidates =
        match candidates with
        | [] -> acc
        | item :: rest ->
            (* Check if item conflicts with any in acc *)
            let conflicts_with_acc = List.exists (fun a -> has_overlap item a) acc in
            if conflicts_with_acc then
              find_batch acc rest
            else
              find_batch (item :: acc) rest
      in
      let batch = find_batch [] remaining in
      let remaining' = List.filter (fun r -> 
        not (List.exists (fun b -> b.id = r.id) batch)
      ) remaining in
      group (batch :: batches) remaining'
  in
  group [] items

(** Estimate merge order based on dependencies *)
let estimate_merge_order queue =
  let batches = group_into_batches queue in
  List.mapi (fun i batch ->
    List.map (fun item -> (item.id, i + 1)) batch
  ) batches
  |> List.concat

(** Check if adding a new item would block existing items *)
let would_cause_new_blocks queue new_item =
  let queue' = add_item queue new_item in
  let conflicts = find_all_conflicts queue' in
  List.filter (fun c -> c.item2_id = new_item.id) conflicts

(** Format conflict for display *)
let format_conflict (c : conflict) =
  Printf.sprintf "%s <-> %s on %s"
    c.item1_id c.item2_id
    (String.concat ", " c.conflicting_files)
