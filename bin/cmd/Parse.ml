let parse_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let input = Bytes.create n in
  really_input ic input 0 n;
  close_in ic;
  Bytes.to_string input

let run path json diagnostics =
  let input = parse_file path in
  try
    let file = Borge_lang.Parse.parse input in
    if json then
      Printf.printf "JSON output not yet implemented\n"
    else if diagnostics then
      Printf.printf "Diagnostics output not yet implemented\n"
    else begin
      let name = Borge_lib.Spec.project_name file in
      let sections = Borge_lib.Spec.count_sections file in
      let statuses = Borge_lib.Spec.statuses file in
      Printf.printf "Valid .borg file: %s\n" path;
      (match name with
       | Some n -> Printf.printf "  Project: %s\n" n
       | None -> ());
      Printf.printf "  Sections: %d\n" sections;
      let by_status = List.fold_left (fun acc st ->
        let count = try List.assoc st acc + 1 with Not_found -> 1 in
        List.sort (fun (a, _) (b, _) -> compare a b) ((st, count) :: List.remove_assoc st acc)
      ) [] statuses in
      List.iter (fun (st, count) ->
        Printf.printf "  %s: %d\n" (Borge_lib.Spec.string_of_status st) count
      ) by_status
    end
  with
  | Borge_lang.Error.Parse_error e ->
    Printf.eprintf "Parse error at line %d, column %d: %s\n" e.line e.column e.message;
    exit 1

open Cmdliner

let path =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE"
    ~doc:"The .borg file to parse")

let json =
  Arg.(value & flag & info ["json"] ~doc:"Output AST as JSON")

let diagnostics =
  Arg.(value & flag & info ["diagnostics"] ~doc:"Output LSP-compatible diagnostics")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "parse" ~doc:"parse a .borg file and validate its structure"
    ~man:[`S "DESCRIPTION";
          `P "Parse a .borg file and validate its structure. \
              Exit 0 if the file is valid, 1 if not."])
  Term.(const run $ path $ json $ diagnostics)

