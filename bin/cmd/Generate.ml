open Borge_lib

let run_spec todo dir =
  match Generate.generate_spec todo dir with
  | Some sexp ->
    Printf.printf "%s\n" sexp;
    exit 0
  | None ->
    Printf.eprintf "Failed to generate spec.\n";
    exit 1

let run_code section dir =
  match Generate.generate_code section dir with
  | Some _ ->
    exit 0
  | None ->
    Printf.eprintf "Failed to generate code for section '%s'.\n" section;
    exit 1

let run_ui section dir =
  let borg_files = File_utils.find_borg_files dir in
  let matching_borg = List.filter (fun p ->
    try
      let input = File_utils.read_file p in
      let file = Borge_lang.Parse.parse_file input in
      let app = Ui_parse.parse_file file in
      match app with
      | Some a ->
        List.exists (fun (pg : Ui_ast.page) -> pg.Ui_ast.name = section) a.Ui_ast.pages
        || List.exists (fun (ly : Ui_ast.layout_def) -> ly.Ui_ast.name = section) a.Ui_ast.layouts
        || List.exists (fun (c : Ui_ast.component) -> c.Ui_ast.name = section) a.Ui_ast.components
      | None -> false
    with _ -> false
  ) borg_files in
  match matching_borg with
  | [] ->
    Printf.eprintf "No .borg file contains UI section '%s'\n" section;
    exit 1
  | borg_path :: _ ->
    let input = File_utils.read_file borg_path in
    let file = Borge_lang.Parse.parse_file input in
    (match Ui_parse.parse_file file with
    | Some app ->
      let json = Ui_json.ui_app_to_json app in
      Printf.printf "UI section '%s' found in %s\n" section borg_path;
      Printf.printf "%s\n" (Yojson.Basic.pretty_to_string json);
      exit 0
    | None ->
      Printf.eprintf "Failed to parse UI spec from %s\n" borg_path;
      exit 1)

let run_db section dir =
  let borg_files = File_utils.find_borg_files dir in
  let matching_borg = List.filter (fun p ->
    try
      let input = File_utils.read_file p in
      let file = Borge_lang.Parse.parse_file input in
      let app = Db_parse.parse_file file in
      match app with
      | Some a ->
        List.exists (fun (t : Db_ast.table_def) -> t.Db_ast.name = section) a.Db_ast.tables
      | None -> false
    with _ -> false
  ) borg_files in
  match matching_borg with
  | [] ->
    Printf.eprintf "No .borg file contains DB table '%s'\n" section;
    exit 1
  | borg_path :: _ ->
    let input = File_utils.read_file borg_path in
    let file = Borge_lang.Parse.parse_file input in
    (match Db_parse.parse_file file with
    | Some app ->
      let json = Db_json.db_app_to_json app in
      Printf.printf "DB table '%s' found in %s\n" section borg_path;
      Printf.printf "%s\n" (Yojson.Basic.pretty_to_string json);
      exit 0
    | None ->
      Printf.eprintf "Failed to parse DB spec from %s\n" borg_path;
      exit 1)

open Cmdliner

let spec_cmd : unit Cmd.t =
  let todo =
    Arg.(value & pos 0 string "" & info [] ~docv:"TODO"
      ~doc:"Todo description to turn into a .borg section")
  in
  let dir =
    Arg.(value & opt dir "." & info ["dir"; "d"] ~docv:"DIR"
      ~doc:"Project directory")
  in
  Cmd.v (Cmd.info "spec" ~doc:"generate a .borg section from a todo description"
    ~man:[`S "DESCRIPTION";
          `P "Takes a free-form todo description and uses an LLM to produce \
              a structured .borg section with (status planned)."])
  Term.(const run_spec $ todo $ dir)

let code_cmd : unit Cmd.t =
  let section =
    Arg.(value & pos 0 string "" & info [] ~docv:"SECTION"
      ~doc:"Section name to implement")
  in
  let dir =
    Arg.(value & opt dir "." & info ["dir"; "d"] ~docv:"DIR"
      ~doc:"Project directory")
  in
  Cmd.v (Cmd.info "code" ~doc:"generate implementation code from a planned section"
    ~man:[`S "DESCRIPTION";
          `P "Reads the spec for a planned section and uses an LLM to \
              generate the implementation code. The section should be \
              marked (status planned) in a .borg file."])
  Term.(const run_code $ section $ dir)

let ui_cmd : unit Cmd.t =
  let section =
    Arg.(value & pos 0 string "" & info [] ~docv:"SECTION"
      ~doc:"UI section name (page, layout, or component)")
  in
  let dir =
    Arg.(value & opt dir "." & info ["dir"; "d"] ~docv:"DIR"
      ~doc:"Project directory")
  in
  Cmd.v (Cmd.info "ui" ~doc:"generate code from a UI spec section"
    ~man:[`S "DESCRIPTION";
          `P "Reads a (ui ...) spec from a .borg file and generates \
              target code. The convention determines the output format \
              (HTML+CSS, React, Clay, etc.)."])
  Term.(const run_ui $ section $ dir)

let db_cmd : unit Cmd.t =
  let section =
    Arg.(value & pos 0 string "" & info [] ~docv:"TABLE"
      ~doc:"DB table name")
  in
  let dir =
    Arg.(value & opt dir "." & info ["dir"; "d"] ~docv:"DIR"
      ~doc:"Project directory")
  in
  Cmd.v (Cmd.info "db" ~doc:"generate code from a DB spec section"
    ~man:[`S "DESCRIPTION";
          `P "Reads a (db ...) spec from a .borg file and generates \
              SQL migrations and/or application code. The convention \
              determines the output format."])
  Term.(const run_db $ section $ dir)

let cmd : unit Cmd.t =
  let info = Cmd.info "generate" ~doc:"generate spec sections or code from specs"
    ~man:[`S "DESCRIPTION";
          `P "Two-step pipeline: generate spec (todo -> .borg section), \
              then generate code (spec -> implementation).";
          `S "SUBCOMMANDS";
          `I ("spec", "Turn a todo into a structured .borg section");
          `I ("code", "Implement a planned section from its spec");
          `I ("ui", "Generate code from a UI spec section");
          `I ("db", "Generate code from a DB spec section")]
  in
  Cmd.group info [spec_cmd; code_cmd; ui_cmd; db_cmd]
