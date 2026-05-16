open Borge_lib

let run dir =
  let roots = Project.find_roots dir in
  if roots = [] then begin
    Printf.printf "No root .borg files found in '%s'\n" dir;
    exit 1
  end;
  List.iter (fun root ->
    match Project.build_tree root with
    | Error (Project.File_not_found path) ->
        Printf.eprintf "Error: file not found: %s\n" path;
        exit 2
    | Error (Project.Parse_error (path, msg)) ->
        Printf.eprintf "Error: parse error in %s: %s\n" path msg;
        exit 2
    | Error (Project.Cycle_detected path) ->
        Printf.eprintf "Error: inline cycle detected: %s\n" (String.concat " -> " path);
        exit 2
    | Ok tree ->
        let lines = Project.format_tree tree in
        List.iter print_endline lines
  ) roots;
  exit 0

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory containing .borg files")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "inline" ~doc:"show the inline tree for a borge project"
    ~man:[`S "DESCRIPTION";
          `P "Finds the root .borg file and follows inline directives to \
              build and display the project tree. Also shows orphaned files \
              that are not part of any inline tree."])
  Term.(const run $ dir)

