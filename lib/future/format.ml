(* Format roadmap output - STUB *)

open Future

let format ?_json ?_summary ?_section ?_show_deps _roadmap =
  "=== PLANNED ===\n\n=== IN PROGRESS ===\n\n=== READY FOR REVIEW ===\n"

let format_summary roadmap =
  Printf.sprintf "Planned: %d | In Progress: %d | Ready: %d"
    (List.length roadmap.planned)
    (List.length roadmap.in_progress)
    (List.length roadmap.ready_for_review)
