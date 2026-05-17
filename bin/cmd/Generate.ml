open Borge_lib

let run_spec todo dir quiet =
  match Generate.generate_spec todo dir with
  | Some sexp ->
    if not quiet then Printf.printf "%s\n" sexp;
    exit 0
  | None ->
    Printf.eprintf "Failed to generate spec.\n";
    exit 1

let run_code section dir quiet =
  ignore quiet;
  match Generate.generate_code section dir with
  | Some _ ->
    exit 0
  | None ->
    Printf.eprintf "Failed to generate code for section '%s'.\n" section;
    exit 1

let run_ui section dir json quiet =
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
      if json then begin
        let json_val = Ui_json.ui_app_to_json app in
        Printf.printf "%s\n" (Yojson.Basic.pretty_to_string json_val)
      end else if not quiet then begin
        Printf.printf "UI spec found in %s\n" borg_path;
        (match app.Ui_ast.theme with
        | Some t ->
          let palette_count = List.filter_map (function Ui_ast.Palette_entry _ -> Some () | _ -> None) t.Ui_ast.entries in
          Printf.printf "  theme: %d palette entries, %d spacing, %d font-size, %d radius\n"
            (List.length palette_count)
            (List.filter_map (function Ui_ast.Spacing_entry _ -> Some () | _ -> None) t.Ui_ast.entries |> List.length)
            (List.filter_map (function Ui_ast.Font_size_entry _ -> Some () | _ -> None) t.Ui_ast.entries |> List.length)
            (List.filter_map (function Ui_ast.Radius_entry _ -> Some () | _ -> None) t.Ui_ast.entries |> List.length)
        | None -> ());
        Printf.printf "  components: %d\n" (List.length app.Ui_ast.components);
        List.iter (fun (c : Ui_ast.component) -> Printf.printf "    - %s\n" c.Ui_ast.name) app.Ui_ast.components;
        Printf.printf "  layouts: %d\n" (List.length app.Ui_ast.layouts);
        List.iter (fun (ly : Ui_ast.layout_def) -> Printf.printf "    - %s\n" ly.Ui_ast.name) app.Ui_ast.layouts;
        Printf.printf "  pages: %d\n" (List.length app.Ui_ast.pages);
        List.iter (fun (p : Ui_ast.page) -> Printf.printf "    - %s (layout: %s)\n" p.Ui_ast.name p.Ui_ast.layout_name) app.Ui_ast.pages;
        Printf.printf "  routes: %d\n" (List.length app.Ui_ast.routes);
        List.iter (fun (r : Ui_ast.route) -> Printf.printf "    - %s -> %s\n" r.Ui_ast.path r.Ui_ast.page_name) app.Ui_ast.routes;
        let issues = Ui_validate.validate app in
        if issues <> [] then begin
          Printf.printf "  validation issues: %d\n" (List.length issues);
          List.iter (fun (i : Ui_validate.issue) ->
            Printf.printf "    [%s] %s\n"
              (match i.severity with `Error -> "ERROR" | `Warning -> "WARN")
              i.message
          ) issues
        end
      end;
      exit 0
    | None ->
      Printf.eprintf "Failed to parse UI spec from %s\n" borg_path;
      exit 1)

let run_db section dir json quiet =
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
      if json then begin
        let json_val = Db_json.db_app_to_json app in
        Printf.printf "%s\n" (Yojson.Basic.pretty_to_string json_val)
      end else if not quiet then begin
        Printf.printf "DB spec found in %s\n" borg_path;
        Printf.printf "  tables: %d\n" (List.length app.Db_ast.tables);
        List.iter (fun (t : Borge_lib.Db_ast.table_def) ->
          Printf.printf "    - %s (%d columns" t.Db_ast.name (List.length t.Db_ast.columns);
          (match t.Db_ast.ownership with Some o -> Printf.printf ", ownership: %s" o | None -> ());
          Printf.printf ")\n"
        ) app.Db_ast.tables;
        Printf.printf "  operations: %d\n" (List.length app.Db_ast.operations);
        List.iter (fun (o : Borge_lib.Db_ast.operations_def) ->
          Printf.printf "    - %s" o.Db_ast.table_name;
          (match o.Db_ast.crud with Some _ -> Printf.printf " (CRUD)" | None -> ());
          Printf.printf "\n"
        ) app.Db_ast.operations;
        Printf.printf "  relations: %d\n" (List.length app.Db_ast.relations);
        List.iter (fun (r : Borge_lib.Db_ast.relation_def) ->
          Printf.printf "    - %s (from %s, %d joins)\n" r.Db_ast.name r.Db_ast.from_table (List.length r.Db_ast.joins)
        ) app.Db_ast.relations;
        Printf.printf "  groups: %d\n" (List.length app.Db_ast.groups);
        List.iter (fun (g : Borge_lib.Db_ast.group_def) ->
          Printf.printf "    - %s%s\n" g.Db_ast.name (if g.Db_ast.can_all then " (can-all)" else "")
        ) app.Db_ast.groups;
        let issues = Db_validate.validate app in
        if issues <> [] then begin
          Printf.printf "  validation issues: %d\n" (List.length issues);
          List.iter (fun (i : Db_validate.issue) ->
            Printf.printf "    [%s] %s\n"
              (match i.severity with `Error -> "ERROR" | `Warning -> "WARN")
              i.message
          ) issues
        end
      end;
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
  let quiet =
    Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")
  in
  Cmd.v (Cmd.info "spec" ~doc:"generate a .borg section from a todo description"
    ~man:[`S "DESCRIPTION";
          `P "Takes a free-form todo description and uses an LLM to produce \
              a structured .borg section with (status planned)."])
  Term.(const run_spec $ todo $ dir $ quiet)

let code_cmd : unit Cmd.t =
  let section =
    Arg.(value & pos 0 string "" & info [] ~docv:"SECTION"
      ~doc:"Section name to implement")
  in
  let dir =
    Arg.(value & opt dir "." & info ["dir"; "d"] ~docv:"DIR"
      ~doc:"Project directory")
  in
  let quiet =
    Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")
  in
  Cmd.v (Cmd.info "code" ~doc:"generate implementation code from a planned section"
    ~man:[`S "DESCRIPTION";
          `P "Reads the spec for a planned section and uses an LLM to \
              generate the implementation code. The section should be \
              marked (status planned) in a .borg file."])
  Term.(const run_code $ section $ dir $ quiet)

let ui_cmd : unit Cmd.t =
  let section =
    Arg.(value & pos 0 string "" & info [] ~docv:"SECTION"
      ~doc:"UI section name (page, layout, or component)")
  in
  let dir =
    Arg.(value & opt dir "." & info ["dir"; "d"] ~docv:"DIR"
      ~doc:"Project directory")
  in
  let json =
    Arg.(value & flag & info ["json"] ~doc:"Output as JSON")
  in
  let quiet =
    Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")
  in
  Cmd.v (Cmd.info "ui" ~doc:"generate code from a UI spec section"
    ~man:[`S "DESCRIPTION";
          `P "Reads a (ui ...) spec from a .borg file and generates \
              target code. The convention determines the output format \
              (HTML+CSS, React, Clay, etc.)."])
  Term.(const run_ui $ section $ dir $ json $ quiet)

let db_cmd : unit Cmd.t =
  let section =
    Arg.(value & pos 0 string "" & info [] ~docv:"TABLE"
      ~doc:"DB table name")
  in
  let dir =
    Arg.(value & opt dir "." & info ["dir"; "d"] ~docv:"DIR"
      ~doc:"Project directory")
  in
  let json =
    Arg.(value & flag & info ["json"] ~doc:"Output as JSON")
  in
  let quiet =
    Arg.(value & flag & info ["quiet"; "q"] ~doc:"Suppress all output except errors")
  in
  Cmd.v (Cmd.info "db" ~doc:"generate code from a DB spec section"
    ~man:[`S "DESCRIPTION";
          `P "Reads a (db ...) spec from a .borg file and generates \
              SQL migrations and/or application code. The convention \
              determines the output format."])
  Term.(const run_db $ section $ dir $ json $ quiet)

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
