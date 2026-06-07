open Borge_lib

let input = {|(ui test
  (layout app-shell
    (ui root
      (layout horizontal)
      (children
        (ui sidebar (children ..nav))
        (ui main (children ..content)))))|} in
let file = Borge_lang.Parse.parse_file input in
let app = Ui_parse.parse_file file in
match app with
| None -> print_endline "PARSE FAILED"
| Some a ->
  Printf.printf "layouts: %d\n" (List.length a.Ui_ast.layouts);
  List.iter (fun l ->
    Printf.printf "  layout %s, slots: %d\n" l.Ui_ast.name (List.length l.Ui_ast.slots);
    let rec print_el indent el =
      Printf.printf "%s  el %s, children: %d\n" (String.make indent ' ')
        (match el.Ui_ast.name with Some n -> n | None -> "?")
        (List.length el.Ui_ast.children);
      List.iter (print_el (indent + 2)) el.Ui_ast.children
    in
    print_el 0 l.Ui_ast.root
  ) a.Ui_ast.layouts
;;

let input = {|(ui test2
  (layout app-shell
    (ui root
      (layout horizontal)
      (children
        (ui sidebar (children ..nav))
        (ui main (children ..content)))))
  (page home
    (use-layout app-shell)
    (fill nav (text "HomeNav"))
    (fill content (text "Welcome")))|} in
let file = Borge_lang.Parse.parse_file input in
let app = Ui_parse.parse_file file in
match app with
| None -> print_endline "PAGE PARSE FAILED"
| Some a ->
  Printf.printf "pages: %d\n" (List.length a.Ui_ast.pages);
  List.iter (fun p ->
    Printf.printf "  page %s (layout: %s), fills: %d\n" p.Ui_ast.name p.Ui_ast.layout_name (List.length p.Ui_ast.fills);
    List.iter (fun f ->
      Printf.printf "    fill slot=%s, content: %d elements\n" f.Ui_ast.slot_name (List.length f.Ui_ast.content)
    ) p.Ui_ast.fills
  ) a.Ui_ast.pages
