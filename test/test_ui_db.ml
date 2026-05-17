open Borge_lib

let test_ui_parse () =
  let input = {|(ui my-app
    (theme
      (palette bg-primary "#e0d7d2")
      (palette accent "#a8421c")
      (spacing sm 8) (spacing md 16)
      (font-size base 14) (font-size lg 18)
      (radius sm 4) (radius md 8))
    (component button
      (interactive click)
      (padding (spacing md))
      (corner (radius sm))
      (variant primary (bg (palette accent)) (color "#ffffff"))
      (variant secondary (bg (palette bg-primary))))
    (layout app-shell
      (ui root
        (layout vertical)
        (width full)
        (height full)
        (children ..content)))
    (page home
      (use-layout app-shell)
      (fill content
        (text "Hello" (size (font-size lg)))))
    (routes
      (route "/" home)))|} in
  let file = Borge_lang.Parse.parse_file input in
  let app = Ui_parse.parse_file file in
  match app with
  | None -> Alcotest.fail "parse returned None"
  | Some a ->
    Alcotest.(check int) "theme present" 1 (match a.Ui_ast.theme with Some _ -> 1 | None -> 0);
    Alcotest.(check int) "components" 1 (List.length a.Ui_ast.components);
    Alcotest.(check int) "layouts" 1 (List.length a.Ui_ast.layouts);
    Alcotest.(check int) "pages" 1 (List.length a.Ui_ast.pages);
    Alcotest.(check int) "routes" 1 (List.length a.Ui_ast.routes)

let test_db_parse () =
  let input = {|(db blog
    (table users
      (column id (type uuid) (primary-key) (default gen_random_uuid))
      (column email (type text) (unique) (not-null))
      (column name (type text) (not-null))
      (column role (type text) (not-null) (default "reader"))
      (column created-at (type timestamptz) (default now)))
    (table posts
      (column id (type uuid) (primary-key) (default gen_random_uuid))
      (column author-id (type uuid) (references users.id) (not-null))
      (column title (type text) (not-null))
      (column published (type boolean) (default false))
      (ownership author-id))
    (operations posts
      (crud (except delete))
      (query by-author)
      (query published-recent (limit 20)))
    (relation post-with-author
      (from posts)
      (join users (on users.id = posts.author-id))
      (select users.name posts.title posts.published)
      (query detail))
    (groups
      (group admin (can-all))
      (group author
        (can posts (create read update) (where author-id = current-user)))
      (group reader
        (can posts (read (where published = true))))))|} in
  let file = Borge_lang.Parse.parse_file input in
  let app = Db_parse.parse_file file in
  match app with
  | None -> Alcotest.fail "parse returned None"
  | Some a ->
    Alcotest.(check int) "tables" 2 (List.length a.Db_ast.tables);
    Alcotest.(check int) "operations" 1 (List.length a.Db_ast.operations);
    Alcotest.(check int) "relations" 1 (List.length a.Db_ast.relations);
    Alcotest.(check int) "groups" 3 (List.length a.Db_ast.groups)

let test_ui_validate () =
  let input = {|(ui my-app
    (theme (palette accent "#a8421c") (spacing md 16))
    (component button
      (bg (palette nonexistent))
      (padding (spacing nonexistent))
      (variant primary (bg (palette accent)))
      (variant primary (bg (palette accent))))
    (layout app-shell
      (ui root (children ..content)))
    (page home
      (use-layout nonexistent-layout)))|} in
  let file = Borge_lang.Parse.parse_file input in
  let app = Ui_parse.parse_file file in
  match app with
  | None -> Alcotest.fail "parse returned None"
  | Some a ->
    let issues = Ui_validate.validate a in
    let errors = List.filter (fun (i : Ui_validate.issue) -> i.severity = `Error) issues in
    (* Should catch: undefined palette ref, undefined spacing ref,
       duplicate variant name, undefined layout ref *)
    Alcotest.(check int) "validation errors" 4 (List.length errors)

let test_db_validate () =
  let input = {|(db blog
    (table users
      (column id (type uuid) (primary-key))
      (column id (type text)))
    (table posts
      (column id (type uuid) (primary-key))
      (column author-id (type uuid) (references nonexistent.id))
      (ownership nonexistent-col))
    (table users
      (column id (type uuid) (primary-key)))
    (operations nonexistent-posts (crud))
    (relation bad-rel (from nonexistent))
    (groups
      (group admin (can-all))
      (group admin (can-all))
      (group reader (can nonexistent-table (read)))))|} in
  let file = Borge_lang.Parse.parse_file input in
  let app = Db_parse.parse_file file in
  match app with
  | None -> Alcotest.fail "parse returned None"
  | Some a ->
    let issues = Db_validate.validate a in
    let errors = List.filter (fun (i : Db_validate.issue) -> i.severity = `Error) issues in
    (* Should catch: duplicate column, undefined ref table, undefined ownership col,
       duplicate table, undefined ops table, undefined from-table in relation,
       duplicate group, undefined cap table *)
    Alcotest.(check int) "validation errors" 8 (List.length errors)

let () =
  Alcotest.run "UI/DB tests" [
    "ui", [Alcotest.test_case "parse" `Quick test_ui_parse;
           Alcotest.test_case "validate" `Quick test_ui_validate];
    "db", [Alcotest.test_case "parse" `Quick test_db_parse;
           Alcotest.test_case "validate" `Quick test_db_validate];
  ]
