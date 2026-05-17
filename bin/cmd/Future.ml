(* borge future — show what's planned but not yet implemented

    Displays the roadmap of spec'd work that's planned or in-progress *)

open Borge_lib

let run dir json summary section verbose =
  (* Get roadmap from future module *)
  let roadmap = Future.get_roadmap dir in
  
  if summary then begin
    Printf.printf "%s\n" (Format.format_summary roadmap);
    exit 0
  end;
  
  if json then begin
    Printf.printf "%s\n" (Format.format ~json:true roadmap);
    exit 0
  end;
  
  (* Filter by section if specified *)
  let roadmap' = match section with
    | Some s -> Format.filter_by_section s roadmap
    | None -> roadmap
  in
  
  if verbose then begin
    (* Show dependency graph *)
    let graph = Depends.build_graph dir in
    Printf.printf "%s\n" (Format.format roadmap');
    Printf.printf "\n%s\n" (Format.format_dependency_graph graph);
    Printf.printf "\n%s\n" (Format.format_completion_order graph);
  end else
    Printf.printf "%s\n" (Format.format roadmap');
  
  exit 0

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Project directory to scan")

let json =
  Arg.(value & flag & info ["json"; "j"]
    ~doc:"Output as JSON")

let summary =
  Arg.(value & flag & info ["summary"; "s"]
    ~doc:"Show summary counts only")

let section =
  Arg.(value & opt (some string) None & info ["section"]
    ~docv:"NAME" ~doc:"Filter to specific section")

let verbose =
  Arg.(value & flag & info ["verbose"; "v"]
    ~doc:"Show dependency graph and completion order")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "future" ~doc:"show planned and in-progress work"
    ~man:[`S "DESCRIPTION";
          `P "Scans all .borg files and displays what's marked as planned";
          `P "or in-progress. This is your project's roadmap.";
          `P "";
          `P "Output is grouped by status:";
          `P "  PLANNED          - Not yet started";
          `P "  IN PROGRESS      - Currently being worked on";
          `P "  READY FOR REVIEW - Implemented but depends on planned items";
          `S "FILTERING";
          `P "Use --section to view only a specific part of the spec.";
          `P "Use --summary for a quick count overview.";
          `S "EXAMPLES";
          `P "borge future              # Show full roadmap";
          `P "borge future --summary    # Just the counts";
          `P "borge future --section db # Only database-related items";])
  Term.(const run $ dir $ json $ summary $ section $ verbose)
