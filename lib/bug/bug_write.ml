(* Writer for .borge-bug files *)

open Bug_ast

let render_resolution (res : resolution) =
  Printf.sprintf "  (resolution (when %s) (how %s) (by %s))"
    res.when_ res.how res.by

let render bug =
  let lines = [
    Printf.sprintf "(borge-bug (id \"%s\")" bug.id;
    Printf.sprintf "  (title \"%s\")" bug.title;
    Printf.sprintf "  (status %s)" (string_of_status bug.status);
    Printf.sprintf "  (created %s)" bug.created;
    Printf.sprintf "  (filed-by %s)" bug.filed_by;
    Printf.sprintf "  (doc \"%s\")" bug.doc;
  ] in
  let lines = match bug.affects_section with
    | Some section -> lines @ [Printf.sprintf "  (affects (section \"%s\"))" section]
    | None -> lines
  in
  let lines = if bug.relates_to <> [] then
    lines @ List.map (fun id -> Printf.sprintf "  (relates-to (bug \"%s\"))" id) bug.relates_to
  else lines
  in
  let lines = match bug.drift_report_id with
    | Some id -> lines @ [Printf.sprintf "  (drift-report-id \"%s\")" id]
    | None -> lines
  in
  let lines = match bug.resolution with
    | Some res -> lines @ [render_resolution res]
    | None -> lines
  in
  lines @ [")"]
  |> String.concat "\n"

let write bug path =
  let oc = open_out path in
  output_string oc (render bug);
  output_string oc "\n";
  close_out oc
