(*| Clay C generator for borge UI specs.

    Walks the typed UI AST and produces a .c + .h file pair that
    constructs Clay layout trees callable from a raylib render loop.
    Primary use: pico firmware screens, but also any C application
    using Clay. See ui.borg section convention-targets / clay-c-target
    for the full mapping specification. |*)

open Ui_ast

(* --- Theme resolution (compile-time only — C has no runtime style lookups) --- *)

type theme_map = {
  palette : (string, string) Hashtbl.t;
  spacing : (string, int) Hashtbl.t;
  font_size : (string, int) Hashtbl.t;
  radius : (string, int) Hashtbl.t;
}

let empty_theme = {
  palette = Hashtbl.create 0;
  spacing = Hashtbl.create 0;
  font_size = Hashtbl.create 0;
  radius = Hashtbl.create 0;
}

let build_theme_map (t : theme option) =
  match t with
  | None -> empty_theme
  | Some theme ->
    let tm = { palette = Hashtbl.create 16; spacing = Hashtbl.create 16;
               font_size = Hashtbl.create 16; radius = Hashtbl.create 16 } in
    List.iter (function
      | Palette_entry (name, value) -> Hashtbl.add tm.palette name value
      | Spacing_entry (name, value) -> Hashtbl.add tm.spacing name value
      | Font_size_entry (name, value) -> Hashtbl.add tm.font_size name value
      | Radius_entry (name, value) -> Hashtbl.add tm.radius name value
    ) theme.entries;
    tm

(* --- Hex color to RGBA struct --- *)

(* Convert hex "#RRGGBB" to { R, G, B, 255 } *)
let hex_to_rgba hex =
  if String.length hex >= 7 then
    let r = int_of_string ("0x" ^ String.sub hex 1 2) in
    let g = int_of_string ("0x" ^ String.sub hex 3 2) in
    let b = int_of_string ("0x" ^ String.sub hex 5 2) in
    Printf.sprintf "{ %d, %d, %d, 255 }" r g b
  else "{ 128, 128, 128, 255 }"

(* Keyword color defaults in RGBA *)
let keyword_to_rgba = function
  | "red" -> "{ 220, 60, 60, 255 }"
  | "orange" -> "{ 220, 128, 60, 255 }"
  | "yellow" -> "{ 220, 192, 60, 255 }"
  | "green" -> "{ 60, 180, 80, 255 }"
  | "blue" -> "{ 60, 120, 220, 255 }"
  | "purple" -> "{ 128, 60, 220, 255 }"
  | "pink" -> "{ 220, 60, 160, 255 }"
  | "white" -> "{ 255, 255, 255, 255 }"
  | "black" -> "{ 0, 0, 0, 255 }"
  | "gray" -> "{ 128, 128, 128, 255 }"
  | "dark" -> "{ 40, 40, 40, 255 }"
  | "light" -> "{ 230, 230, 230, 255 }"
  | "transparent" -> "{ 0, 0, 0, 0 }"
  | _ -> "{ 136, 136, 136, 255 }"

(* Resolve a color to an RGBA C struct literal *)
let rec resolve_color_rgba (tm : theme_map) = function
  | Hex h -> hex_to_rgba h
  | Keyword k -> keyword_to_rgba k
  | Palette name ->
    (try hex_to_rgba (Hashtbl.find tm.palette name)
     with Not_found -> "/* UNRESOLVED_PALETTE */ { 0, 0, 0, 255 }")
  | With_alpha (c, a) ->
    let base = resolve_color_rgba tm c in
    (* For alpha, we'd need to parse the struct — just emit the base with a comment *)
    Printf.sprintf "/* alpha %g */ %s" a base

(* --- C identifier sanitization --- *)

let sanitize_c_identifier name =
  let name = String.map (fun c -> if c = '-' then '_' else c) name in
  let name = String.map (fun c -> if c = ' ' then '_' else c) name in
  (* Prefix reserved words *)
  match name with
  | "auto" | "break" | "case" | "char" | "const" | "continue"
  | "default" | "do" | "double" | "else" | "enum" | "extern"
  | "float" | "for" | "goto" | "if" | "int" | "long"
  | "register" | "return" | "short" | "signed" | "sizeof"
  | "static" | "struct" | "switch" | "typedef" | "union"
  | "unsigned" | "void" | "volatile" | "while" ->
    "ui_" ^ name
  | _ -> name

(* --- Sizing to Clay C --- *)

let sizing_to_clay_width = function
  | Flex n -> Printf.sprintf ".sizing.width = { CLAY_SIZING_GROW(%d), CLAY_SIZING_GROW(%d) }" n n
  | Grow -> ".sizing.width = { CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0) }"
  | Full -> ".sizing.width = CLAY_SIZING_PERCENT(1.0f)"
  | Auto -> ".sizing.width = { CLAY_SIZING_FIT, CLAY_SIZING_FIT }"
  | Px n -> Printf.sprintf ".sizing.width = { CLAY_SIZING_FIXED(%d), CLAY_SIZING_FIXED(%d) }" n n
  | Rem n -> Printf.sprintf ".sizing.width = { CLAY_SIZING_FIXED(%d), CLAY_SIZING_FIXED(%d) }" (int_of_float (n *. 16.0)) (int_of_float (n *. 16.0))
  | Pct n -> Printf.sprintf ".sizing.width = CLAY_SIZING_PERCENT(%g)" (float_of_int n /. 100.0)
  | Vh n -> Printf.sprintf "/* vh not directly supported — use FIXED */ .sizing.width = { CLAY_SIZING_FIXED(%d), CLAY_SIZING_FIXED(%d) }" (n * 8) (n * 8) (* rough estimation *)

let sizing_to_clay_height = function
  | Flex n -> Printf.sprintf ".sizing.height = { CLAY_SIZING_GROW(%d), CLAY_SIZING_GROW(%d) }" n n
  | Grow -> ".sizing.height = { CLAY_SIZING_GROW(0), CLAY_SIZING_GROW(0) }"
  | Full -> ".sizing.height = CLAY_SIZING_PERCENT(1.0f)"
  | Auto -> ".sizing.height = { CLAY_SIZING_FIT, CLAY_SIZING_FIT }"
  | Px n -> Printf.sprintf ".sizing.height = { CLAY_SIZING_FIXED(%d), CLAY_SIZING_FIXED(%d) }" n n
  | Rem n -> Printf.sprintf ".sizing.height = { CLAY_SIZING_FIXED(%d), CLAY_SIZING_FIXED(%d) }" (int_of_float (n *. 16.0)) (int_of_float (n *. 16.0))
  | Pct n -> Printf.sprintf ".sizing.height = CLAY_SIZING_PERCENT(%g)" (float_of_int n /. 100.0)
  | Vh n -> Printf.sprintf "/* vh */ .sizing.height = { CLAY_SIZING_FIXED(%d), CLAY_SIZING_FIXED(%d) }" (n * 8) (n * 8)

(* --- Spacing to Clay C --- *)

let resolve_spacing_c (tm : theme_map) = function
  | Spacing_px n -> n
  | Spacing_rem n -> int_of_float (n *. 16.0)
  | Spacing_var name ->
    (try Hashtbl.find tm.spacing name
     with Not_found -> 0)

let padding_to_clay (tm : theme_map) = function
  | Padding_uniform s ->
    let v = resolve_spacing_c tm s in
    Printf.sprintf ".padding = { %d, %d, %d, %d }" v v v v
  | Padding_sides { top; right; bottom; left } ->
    Printf.sprintf ".padding = { %d, %d, %d, %d }"
      (resolve_spacing_c tm top) (resolve_spacing_c tm right)
      (resolve_spacing_c tm bottom) (resolve_spacing_c tm left)

(* --- Radius to Clay C --- *)

let resolve_radius_c (tm : theme_map) = function
  | Radius_px n -> n
  | Radius_var name ->
    (try Hashtbl.find tm.radius name
     with Not_found -> 0)

let corner_to_clay (tm : theme_map) = function
  | Corner_uniform r ->
    let v = resolve_radius_c tm r in
    Printf.sprintf ".cornerRadius = { %d, %d, %d, %d }" v v v v
  | Corner_sides { top_left; top_right; bottom_left; bottom_right } ->
    Printf.sprintf ".cornerRadius = { %d, %d, %d, %d }"
      (resolve_radius_c tm top_left) (resolve_radius_c tm top_right)
      (resolve_radius_c tm bottom_left) (resolve_radius_c tm bottom_right)

(* --- Alignment to Clay C --- *)

let align_x_to_clay = function
  | Align_start -> "CLAY_ALIGN_X_LEFT"
  | Align_center -> "CLAY_ALIGN_X_CENTER"
  | Align_end -> "CLAY_ALIGN_X_RIGHT"

let align_y_to_clay = function
  | Align_start -> "CLAY_ALIGN_Y_TOP"
  | Align_center -> "CLAY_ALIGN_Y_CENTER"
  | Align_end -> "CLAY_ALIGN_Y_BOTTOM"

(* --- Collect all Clay config fields from properties --- *)

let collect_clay_config (tm : theme_map) (props : ui_property list) =
  let fields = ref [] in
  List.iter (function
    | P_layout Horizontal -> fields := ".layoutDirection = CLAY_LEFT_TO_RIGHT" :: !fields
    | P_layout Vertical -> fields := ".layoutDirection = CLAY_TOP_TO_BOTTOM" :: !fields
    | P_width s -> fields := sizing_to_clay_width s :: !fields
    | P_height s -> fields := sizing_to_clay_height s :: !fields
    | P_padding p -> fields := padding_to_clay tm p :: !fields
    | P_gap s -> fields := Printf.sprintf ".childGap = %d" (resolve_spacing_c tm s) :: !fields
    | P_bg c -> fields := Printf.sprintf ".backgroundColor = %s" (resolve_color_rgba tm c) :: !fields
    | P_color _ -> ()  (* handled in text elements *)
    | P_border { width = Some w; color = Some c; _ } ->
      fields := Printf.sprintf ".border = { %d, %d, %d, %d }" w w w w :: !fields;
      fields := Printf.sprintf ".borderColor = %s" (resolve_color_rgba tm c) :: !fields
    | P_border _ -> ()
    | P_corner cr -> fields := corner_to_clay tm cr :: !fields
    | P_scroll Scroll_vertical -> fields := ".scroll = { .vertical = true }" :: !fields
    | P_scroll Scroll_horizontal -> fields := ".scroll = { .horizontal = true }" :: !fields
    | P_scroll Scroll_both -> fields := ".scroll = { .vertical = true, .horizontal = true }" :: !fields
    | P_float _ -> fields := ".elementIsFloating = true" :: !fields
    | P_interactive _ -> ()  (* handled via stub functions *)
    | P_align_x a -> fields := Printf.sprintf ".childAlignment.x = %s" (align_x_to_clay a) :: !fields
    | P_align_y a -> fields := Printf.sprintf ".childAlignment.y = %s" (align_y_to_clay a) :: !fields
  ) props;
  List.rev !fields

(* --- Element to Clay C --- *)

let rec element_to_clay (tm : theme_map) indent (el : ui_element) =
  let indent_str = String.make indent ' ' in
  (* Text content: emit CLAY_TEXT directly *)
  match el.text with
  | Some t ->
    Printf.sprintf "%sCLAY_TEXT(%S, {});\n" indent_str t
  | None ->
  let config_fields = collect_clay_config tm el.properties in
  let el_id = match el.name with Some n -> sanitize_c_identifier n | None -> "element" in

  let has_interactive = List.exists (function P_interactive _ -> true | _ -> false) el.properties in
  let on_hover = if has_interactive then
    Printf.sprintf "\n%s  .onHover = handle_%s_hover," indent_str el_id
  else "" in

  let config_str = String.concat (",\n" ^ indent_str ^ "  ") config_fields in

  let buf = Buffer.create 256 in

  (* CLAY_CONTAINER open *)
  Buffer.add_string buf (Printf.sprintf
    "%sCLAY_CONTAINER(CLAY_IDI(%s), CLAY_LAYOUT_CONFIG({\n%s  %s%s\n%s}), {\n"
    indent_str el_id indent_str config_str on_hover indent_str);

  (* Children *)
  List.iter (function
    | El child -> Buffer.add_string buf (element_to_clay tm (indent + 4) child)
    | Slot_ref name -> Buffer.add_string buf (Printf.sprintf "%s/* slot: %s */\n" indent_str name)
  ) el.children;

  (* CLAY_CONTAINER close *)
  Buffer.add_string buf (Printf.sprintf "%s});\n" indent_str);

  Buffer.contents buf

(* --- Component to C function --- *)

let component_to_clay (tm : theme_map) (comp : component) =
  let fn_name = "render_" ^ sanitize_c_identifier comp.name in
  let body = element_to_clay tm 4
    { name = Some comp.name; properties = comp.properties;
      children = comp.children; variants = comp.variants; text = None } in
  Printf.sprintf "Clay_Children %s(void) {\n  return (Clay_Children){\n%s  };\n}\n\n"
    fn_name body

(* --- Layout to C function --- *)

let layout_to_clay (tm : theme_map) (layout : layout_def) =
  let fn_name = "layout_" ^ sanitize_c_identifier layout.name in
  let body = element_to_clay tm 4 layout.root in
  Printf.sprintf "void %s(int screen_width, int screen_height) {\n%s}\n\n"
    fn_name body

(* --- Page to C function --- *)

let page_to_clay (_tm : theme_map) (pg : page) =
  let fn_name = "layout_page_" ^ sanitize_c_identifier pg.name in
  let layout_fn = "layout_" ^ sanitize_c_identifier pg.layout_name in
  (* For now, just call the layout function. Slot filling would need
     more sophisticated code generation — emit the layout and add
     comments where slot content goes. *)
  let fill_comments = List.map (fun (fill : slot_fill) ->
    Printf.sprintf "  /* FILL: %s */\n" fill.slot_name
  ) pg.fills in
  let fills_str = String.concat "" fill_comments in
  Printf.sprintf "void %s(int screen_width, int screen_height) {\n%s  %s(screen_width, screen_height);\n}\n\n"
    fn_name fills_str layout_fn

(* --- Stub hover handlers for interactive elements --- *)

let collect_interactive_elements (app : ui_app) =
  let result = ref [] in
  let rec check_element (el : ui_element) =
    List.iter (function
      | P_interactive Interactive_click | P_interactive Interactive_hover ->
        (match el.name with
         | Some n -> result := n :: !result
         | None -> ())
      | _ -> ()
    ) el.properties;
    List.iter (function El child -> check_element child | Slot_ref _ -> ()) el.children
  in
  List.iter check_element app.elements;
  List.iter (fun (comp : component) -> check_element
    { name = Some comp.name; properties = comp.properties;
      children = comp.children; variants = comp.variants; text = None }) app.components;
  List.iter (fun layout -> check_element layout.root) app.layouts;
  List.rev !result

(* --- Header file --- *)

let generate_header (app : ui_app) =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "/* Generated by borge generate --target clay-c */\n";
  Buffer.add_string buf "/* DO NOT EDIT: regenerate from spec */\n\n";
  Buffer.add_string buf "#ifndef UI_APP_H\n#define UI_APP_H\n\n";
  Buffer.add_string buf "#include \"clay.h\"\n\n";

  (* Page enum *)
  if app.routes <> [] then begin
    Buffer.add_string buf "enum page {\n";
    let routes = List.map (fun (r : route) ->
      "  PAGE_" ^ String.uppercase_ascii (sanitize_c_identifier r.page_name)
    ) app.routes in
    Buffer.add_string buf (String.concat ",\n" routes);
    Buffer.add_string buf "\n};\n\n";
    Buffer.add_string buf "extern enum page current_page;\n\n";
  end;

  (* Function declarations *)
  List.iter (fun (comp : component) ->
    Buffer.add_string buf (Printf.sprintf "Clay_Children render_%s(void);\n" (sanitize_c_identifier comp.name))
  ) app.components;
  List.iter (fun (layout : layout_def) ->
    Buffer.add_string buf (Printf.sprintf "void layout_%s(int screen_width, int screen_height);\n" (sanitize_c_identifier layout.name))
  ) app.layouts;
  List.iter (fun (pg : page) ->
    Buffer.add_string buf (Printf.sprintf "void layout_page_%s(int screen_width, int screen_height);\n" (sanitize_c_identifier pg.name))
  ) app.pages;
  if app.routes <> [] then
    Buffer.add_string buf "void layout_app(int screen_width, int screen_height);\n";

  Buffer.add_string buf "\n#endif /* UI_APP_H */\n";
  Buffer.contents buf

(* --- Main generation --- *)

let generate ?(source_path = "<spec>") (app : ui_app) =
  let tm = build_theme_map app.theme in
  let buf = Buffer.create 8192 in

  (* Header *)
  Buffer.add_string buf "/* Generated by borge generate --target clay-c */\n";
  Buffer.add_string buf (Printf.sprintf "/* Source: %s */\n" source_path);
  Buffer.add_string buf "/* DO NOT EDIT: regenerate from spec */\n\n";
  Buffer.add_string buf "#include \"ui_app.h\"\n\n";

  (* Page enum global *)
  if app.routes <> [] then begin
    Buffer.add_string buf "enum page current_page = PAGE_";
    (match app.routes with r :: _ ->
      Buffer.add_string buf (String.uppercase_ascii (sanitize_c_identifier r.page_name))
    | [] -> ());
    Buffer.add_string buf ";\n\n";
  end;

  (* Stub hover handlers *)
  let interactive_els = collect_interactive_elements app in
  if interactive_els <> [] then begin
    Buffer.add_string buf "/* Interactive handlers — fill in actual behavior */\n\n";
    List.iter (fun el_name ->
      let id = sanitize_c_identifier el_name in
      Buffer.add_string buf (Printf.sprintf
        "void handle_%s_hover(Clay_HoverState state) {\n  /* TODO: implement click/hover for %s */\n}\n\n"
        id el_name)
    ) interactive_els;
  end;

  (* Components *)
  if app.components <> [] then begin
    Buffer.add_string buf "/* Components */\n\n";
    List.iter (fun comp ->
      Buffer.add_string buf (component_to_clay tm comp)
    ) app.components;
  end;

  (* Layouts *)
  if app.layouts <> [] then begin
    Buffer.add_string buf "/* Layouts */\n\n";
    List.iter (fun layout ->
      Buffer.add_string buf (layout_to_clay tm layout)
    ) app.layouts;
  end;

  (* Pages *)
  if app.pages <> [] then begin
    Buffer.add_string buf "/* Pages */\n\n";
    List.iter (fun (pg : Ui_ast.page) ->
      Buffer.add_string buf (page_to_clay tm pg)
    ) app.pages;
  end;

  (* Top-level layout switch from routes *)
  if app.routes <> [] then begin
    Buffer.add_string buf "/* Route dispatcher */\n\n";
    Buffer.add_string buf "void layout_app(int screen_width, int screen_height) {\n";
    Buffer.add_string buf "  switch (current_page) {\n";
    List.iter (fun (r : route) ->
      let page_id = String.uppercase_ascii (sanitize_c_identifier r.page_name) in
      Buffer.add_string buf (Printf.sprintf "    case PAGE_%s: layout_page_%s(screen_width, screen_height); break;\n"
        page_id (sanitize_c_identifier r.page_name))
    ) app.routes;
    Buffer.add_string buf "  }\n}\n";
  end;

  (* Elements — top-level, not in a component/layout/page *)
  if app.elements <> [] then begin
    Buffer.add_string buf "/* Elements */\n\n";
    List.iter (fun (el : Ui_ast.ui_element) ->
      Buffer.add_string buf (element_to_clay tm 0 el)
    ) app.elements;
  end;

  Buffer.contents buf

let generate_header_file = generate_header
