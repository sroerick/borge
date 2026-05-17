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

let () =
  Alcotest.run "UI/DB tests" [
    "ui", [Alcotest.test_case "parse" `Quick test_ui_parse];
    "db", [Alcotest.test_case "parse" `Quick test_db_parse];
  ]
