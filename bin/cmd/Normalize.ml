open Borge_lib

(** Normalize: fmt all .borg files, accept comment deletions as intentional.
    This is the human saying "I made changes on purpose." *)

let run dir quiet =
  if not quiet then Printf.printf "Normalizing .borg files in '%s'...\n" dir;
  (* Find all .borg files via the project tree *)
  let roots = Project.find_roots dir in
  let tree_paths = match roots with
    | [] -> File_utils.find_borg_files dir
    | _ ->
        List.concat_map (fun root ->
          match Project.build_tree root with
          | Ok tree -> Project.tree_paths tree
          | Error _ -> [root]
        ) roots
  in
  (* Format each file in place *)
  let formatted = ref 0 in
  let unchanged = ref 0 in
  List.iter (fun path ->
    let input = File_utils.read_file path in
    let file = Borge_lang.Parse.parse_file input in
    let output = Borge_lang.Print.print_file file in
    if Fmt.strip_trailing_newlines input <> Fmt.strip_trailing_newlines output then begin
      let oc = open_out path in
      output_string oc output;
      close_out oc;
      if not quiet then Printf.printf "  formatted: %s\n" (Filename.basename path);
      incr formatted
    end else
      incr unchanged
  ) tree_paths;
  if not quiet then begin
    Printf.printf "\n%d files formatted, %d unchanged.\n" !formatted !unchanged;
    Printf.printf "Comment deletions are now accepted as intentional.\n";
  end;
  exit 0

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory containing .borg files")

let quiet =
  Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "normalize" ~doc:"fmt all files and accept comment deletions"
    ~man:[`S "DESCRIPTION";
          `P "Runs borge fmt on all .borg files in the project tree. \
              After normalize, any comment deletions are considered intentional — \
              borge lint will no longer warn about them."])
  Term.(const run $ dir $ quiet)
