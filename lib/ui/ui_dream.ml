(*| dream_html OCaml generator for borge UI specs.

    Walks the typed UI AST and produces an OCaml module that
    constructs the full UI as dream_html nodes. The output is
    a .ml file that can be compiled into any Dream application.
    See ui.borg section convention-targets / ocaml-dream-target for
    the full mapping specification. |*)

open Ui_ast

(* --- Theme resolution --- *)

type theme_map = {
  palette : (string, string) Hashtbl.t;   (* name -> hex value *)
  spacing : (string, int) Hashtbl.t;       (* name -> px value *)
  font_size : (string, int) Hashtbl.t;     (* name -> px value *)
  radius : (string, int) Hashtbl.t;        (* name -> px value *)
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
    let tm = {
      palette = Hashtbl.create 16;
      spacing = Hashtbl.create 16;
      font_size = Hashtbl.create 16;
      radius = Hashtbl.create 16;
    } in
    List.iter (function
      | Palette_entry (name, value) -> Hashtbl.add tm.palette name value
      | Spacing_entry (name, value) -> Hashtbl.add tm.spacing name value
      | Font_size_entry (name, value) -> Hashtbl.add tm.font_size name value
      | Radius_entry (name, value) -> Hashtbl.add tm.radius name value
    ) theme.entries;
    tm

(* Keyword color defaults *)
let keyword_to_hex = function
  | "red" -> "#e04040"
  | "orange" -> "#e08040"
  | "yellow" -> "#e0c040"
  | "green" -> "#40c060"
  | "blue" -> "#4080e0"
  | "purple" -> "#8040e0"
  | "pink" -> "#e040a0"
  | "white" -> "#ffffff"
  | "black" -> "#000000"
  | "gray" -> "#808080"
  | "dark" -> "#282828"
  | "light" -> "#e6e6e6"
  | "transparent" -> "#00000000"
  | _ -> "#888888"  (* fallback gray *)

(* Resolve a color to a hex string *)
let rec resolve_color (tm : theme_map) = function
  | Hex h -> h
  | Keyword k -> keyword_to_hex k
  | Palette name ->
    (match Hashtbl.find_opt tm.palette name with
     | Some v -> v
     | None -> Printf.sprintf "/* UNRESOLVED PALETTE: %s */" name)
  | With_alpha (c, a) ->
    let hex = resolve_color tm c in
    (* Convert hex + alpha to rgba *)
    if String.length hex >= 7 then
      match int_of_string_opt ("0x" ^ (* exempt: String.sub *) String.sub hex 1 2),
            int_of_string_opt ("0x" ^ (* exempt: String.sub *) String.sub hex 3 2),
            int_of_string_opt ("0x" ^ (* exempt: String.sub *) String.sub hex 5 2) with
      | Some r, Some g, Some b -> Printf.sprintf "rgba(%d, %d, %d, %g)" r g b a
      | _ -> hex
    else hex

(* Resolve a spacing value to a CSS string *)
let resolve_spacing (tm : theme_map) = function
  | Spacing_px n -> Printf.sprintf "%dpx" n
  | Spacing_rem n -> Printf.sprintf "%grem" n
  | Spacing_var name ->
    (match Hashtbl.find_opt tm.spacing name with
     | Some v -> Printf.sprintf "%dpx" v
     | None -> Printf.sprintf "/* UNRESOLVED SPACING: %s */" name)

(* Resolve a font size to a CSS string *)
let resolve_font_size (tm : theme_map) = function
  | Font_px n -> Printf.sprintf "%dpx" n
  | Font_rem n -> Printf.sprintf "%grem" n
  | Font_var name ->
    (match Hashtbl.find_opt tm.font_size name with
     | Some v -> Printf.sprintf "%dpx" v
     | None -> Printf.sprintf "/* UNRESOLVED FONT-SIZE: %s */" name)

(* Resolve a radius value to a CSS string *)
let resolve_radius (tm : theme_map) = function
  | Radius_px n -> Printf.sprintf "%dpx" n
  | Radius_var name ->
    (match Hashtbl.find_opt tm.radius name with
     | Some v -> Printf.sprintf "%dpx" v
     | None -> Printf.sprintf "/* UNRESOLVED RADIUS: %s */" name)

(* --- CSS property generation --- *)

let sizing_to_css = function
  | Flex n -> Printf.sprintf "flex: %d" n
  | Grow -> "flex: 1 1 0%"
  | Full -> "width: 100%"
  | Auto -> ""
  | Px n -> Printf.sprintf "width: %dpx" n
  | Rem n -> Printf.sprintf "width: %grem" n
  | Pct n -> Printf.sprintf "width: %d%%" n
  | Vh n -> Printf.sprintf "height: %dvh" n

let sizing_to_css_height = function
  | Flex n -> Printf.sprintf "flex: %d" n
  | Grow -> "flex: 1 1 0%"
  | Full -> "height: 100%"
  | Auto -> ""
  | Px n -> Printf.sprintf "height: %dpx" n
  | Rem n -> Printf.sprintf "height: %grem" n
  | Pct n -> Printf.sprintf "height: %d%%" n
  | Vh n -> Printf.sprintf "height: %dvh" n

let padding_to_css (tm : theme_map) = function
  | Padding_uniform s -> Printf.sprintf "padding: %s" (resolve_spacing tm s)
  | Padding_sides { top; right; bottom; left } ->
    Printf.sprintf "padding: %s %s %s %s"
      (resolve_spacing tm top) (resolve_spacing tm right)
      (resolve_spacing tm bottom) (resolve_spacing tm left)

let corner_to_css (tm : theme_map) = function
  | Corner_uniform r -> Printf.sprintf "border-radius: %s" (resolve_radius tm r)
  | Corner_sides { top_left; top_right; bottom_left; bottom_right } ->
    Printf.sprintf "border-radius: %s %s %s %s"
      (resolve_radius tm top_left) (resolve_radius tm top_right)
      (resolve_radius tm bottom_left) (resolve_radius tm bottom_right)

let alignment_to_css_x = function
  | Align_start -> "items-start"
  | Align_center -> "items-center"
  | Align_end -> "items-end"

let alignment_to_css_y = function
  | Align_start -> "justify-start"
  | Align_center -> "justify-center"
  | Align_end -> "justify-end"

let scroll_to_css = function
  | Scroll_vertical -> "overflow-y: auto"
  | Scroll_horizontal -> "overflow-x: auto"
  | Scroll_both -> "overflow: auto"

(* --- Collect styles from properties --- *)

let collect_styles (tm : theme_map) (props : ui_property list) =
  let styles = ref [] in
  let classes = ref [] in
  List.iter (function
    | P_layout Horizontal -> classes := "flex-row" :: !classes
    | P_layout Vertical -> classes := "flex-col" :: !classes
    | P_width s ->
      (match sizing_to_css s with "" -> () | css -> styles := css :: !styles)
    | P_height s ->
      (match sizing_to_css_height s with "" -> () | css -> styles := css :: !styles)
    | P_padding p -> styles := padding_to_css tm p :: !styles
    | P_gap s -> styles := Printf.sprintf "gap: %s" (resolve_spacing tm s) :: !styles
    | P_bg c -> styles := Printf.sprintf "background-color: %s" (resolve_color tm c) :: !styles
    | P_color c -> styles := Printf.sprintf "color: %s" (resolve_color tm c) :: !styles
    | P_border { width = Some w; color = Some c; side } ->
      (match side with
       | Some s -> styles := Printf.sprintf "border-%s: %dpx solid %s" s w (resolve_color tm c) :: !styles
       | None -> styles := Printf.sprintf "border: %dpx solid %s" w (resolve_color tm c) :: !styles)
    | P_border { width = Some w; color = None; side } ->
      (match side with
       | Some s -> styles := Printf.sprintf "border-%s: %dpx solid" s w :: !styles
       | None -> styles := Printf.sprintf "border: %dpx solid" w :: !styles)
    | P_border _ -> ()
    | P_corner cr -> styles := corner_to_css tm cr :: !styles
    | P_scroll d -> styles := scroll_to_css d :: !styles
    | P_float Float_simple -> styles := "position: absolute" :: !styles
    | P_float Float_attach_parent -> styles := "position: relative" :: !styles
    | P_float (Float_offset offsets) ->
      styles := "position: absolute" :: !styles;
      List.iter (fun (dir, n) ->
        styles := Printf.sprintf "%s: %dpx" dir n :: !styles
      ) offsets
    | P_interactive _ -> ()  (* handled separately *)
    | P_align_x a -> classes := alignment_to_css_x a :: !classes
    | P_align_y a -> classes := alignment_to_css_y a :: !classes
  ) props;
  (List.rev !classes, List.rev !styles)

(* --- CSS custom properties from theme --- *)

let theme_to_css_vars (tm : theme_map) =
  let buf = Buffer.create 256 in
  Hashtbl.iter (fun name value ->
    Buffer.add_string buf (Printf.sprintf "  --%s: %s;\n" name value)
  ) tm.palette;
  Hashtbl.iter (fun name value ->
    Buffer.add_string buf (Printf.sprintf "  --spacing-%s: %dpx;\n" name value)
  ) tm.spacing;
  Hashtbl.iter (fun name value ->
    Buffer.add_string buf (Printf.sprintf "  --font-size-%s: %dpx;\n" name value)
  ) tm.font_size;
  Hashtbl.iter (fun name value ->
    Buffer.add_string buf (Printf.sprintf "  --radius-%s: %dpx;\n" name value)
  ) tm.radius;
  if Buffer.length buf > 0 then
    Printf.sprintf "<style>\n:root {\n%s}\n</style>\n" (Buffer.contents buf)
  else ""

(* --- Style merging: deduplicate CSS properties --- *)

(*| Merge multiple CSS style strings by splitting on ';' and keeping
    the last declaration for each property name. This prevents
    invalid HTML from duplicate style attributes. |*)

let merge_css_styles (styles : string list) =
  let seen = Hashtbl.create 16 in
  (* Process in order — later values override earlier ones *)
  List.iter (fun s ->
    let parts = String.split_on_char ';' s in
    List.iter (fun part ->
      let part = String.trim part in
      if part <> "" then begin
        match String.split_on_char ':' part with
        | [key; value] ->
          Hashtbl.replace seen (String.trim key) (String.trim value)
        | _ -> ()  (* skip malformed *)
      end
    ) parts
  ) styles;
  let result = Hashtbl.fold (fun key value acc ->
    (key ^ ": " ^ value) :: acc
  ) seen [] in
  String.concat "; " (List.sort String.compare result)

(* --- Variant → @media blocks --- *)

let default_breakpoint = 768

let sanitize_name = function
  | None -> "el"
  | Some name ->
    String.map (fun c -> if c = '-' then '_' else c) name

let variant_to_media_query (tm : theme_map) (el_name : string) (v : variant) =
  let v_classes, v_styles = collect_styles tm v.properties in
  let class_str = String.concat " " v_classes in
  let style_str = merge_css_styles v_styles in
  let el_class = sanitize_name (Some el_name) in
  let width = default_breakpoint in
  (* Wide/narrow heuristic: "wide" or "expanded" = desktop, others = mobile *)
  let is_wide = List.exists (fun s ->
    List.mem s ["wide"; "expanded"; "desktop"; "lg"; "xl"]
  ) [v.name] in
  let condition =
    if is_wide then Printf.sprintf "min-width: %dpx" width
    else Printf.sprintf "max-width: %dpx" (width - 1)
  in
  let rules = ref [] in
  if class_str <> "" then
    rules := Printf.sprintf ".%s.variant-%s { %s }" el_class v.name class_str :: !rules;
  if style_str <> "" then
    rules := Printf.sprintf ".%s.variant-%s { %s }" el_class v.name style_str :: !rules;
  if !rules = [] then ""
  else
    Printf.sprintf "@media (%s) {\n  %s\n}\n" condition (String.concat "\n  " (List.rev !rules))

let collect_variant_styles (tm : theme_map) (el : ui_element) =
  List.filter_map (fun (v : variant) ->
    match el.name with
    | Some n -> Some (variant_to_media_query tm n v)
    | None -> None
  ) el.variants
  |> List.filter (fun s -> s <> "")


(* --- Element to dream_html --- *)

let has_click_interactive (props : ui_property list) =
  List.exists (function P_interactive Interactive_click -> true | _ -> false) props

let rec element_to_dream (tm : theme_map) (el : ui_element) =
  (* Text content: emit txt node directly *)
  match el.text with
  | Some t ->
    Printf.sprintf "txt %S" t
  | None ->
  let _name = sanitize_name el.name in
  let classes, styles = collect_styles tm el.properties in
  let has_click = has_click_interactive el.properties in

  (* Build attributes list *)
  let attr_parts = ref [] in
  if classes <> [] then
    attr_parts := Printf.sprintf "class_ \"%s\""
      (String.concat " " classes) :: !attr_parts;
  if styles <> [] then
    attr_parts := Printf.sprintf "style_ \"%%s\" \"%s\""
      (merge_css_styles styles) :: !attr_parts;
  if has_click then
    attr_parts := "onclick \"/* TODO: click handler */\"" :: !attr_parts;

  (* Build children — slots are node list, elements are node,
     so we build segments and splice with @ *)
  let has_slots = List.exists (function Slot_ref _ -> true | _ -> false) el.children in
  let attrs_str = String.concat "; " (List.rev !attr_parts) in

  if not has_slots then begin
    (* Simple case: no slots, all elements *)
    let children_code = List.filter_map (function
      | El el -> Some (element_to_dream tm el)
      | Slot_ref _ -> None
    ) el.children in
    let children_str = String.concat "; " children_code in
    if attrs_str = "" && children_str = "" then
      Printf.sprintf "div [] []"
    else if attrs_str = "" then
      Printf.sprintf "div [] [%s]" children_str
    else if children_str = "" then
      Printf.sprintf "div [%s] []" attrs_str
    else
      Printf.sprintf "div [%s] [%s]" attrs_str children_str
  end else begin
    (* Slots present: build spliced expression ([el1] @ nav @ [el2]) *)
    let segments = List.filter_map (function
      | El el ->
        let code = element_to_dream tm el in
        Some (Printf.sprintf "[%s]" code)
      | Slot_ref name ->
        Some (String.map (fun c -> if c = '-' then '_' else c) name)
    ) el.children in
    let children_expr = String.concat " @ " segments in
    let children_expr = if children_expr = "" then "[]" else children_expr in
    if attrs_str = "" then
      Printf.sprintf "div [] %s" children_expr
    else
      Printf.sprintf "div [%s] %s" attrs_str children_expr
  end

(* --- Component to render function --- *)

let component_to_dream (tm : theme_map) (comp : component) =
  let fn_name = "render_" ^ String.map (fun c -> if c = '-' then '_' else c) comp.name in
  let body = element_to_dream tm
    { name = Some comp.name; properties = comp.properties;
      children = comp.children; variants = comp.variants; text = None } in
  Printf.sprintf "let %s ~(_content : node list) () : node =\n  %s\n"
    fn_name body

(* --- Layout to render function --- *)

let layout_to_dream (tm : theme_map) (layout : layout_def) =
  let fn_name = "render_" ^ String.map (fun c -> if c = '-' then '_' else c) layout.name ^ "_layout" in
  let body = element_to_dream tm layout.root in
  (* Build slot parameters from the layout's slot list *)
  let slot_params = List.map (function
    | Named_slot name -> Printf.sprintf "~(%s : node list)" (String.map (fun c -> if c = '-' then '_' else c) name)
    | Default_slot -> "~(content : node list)"
  ) layout.slots
  in
  let params_str = String.concat " " slot_params in
  Printf.sprintf "let %s %s () : node =\n  %s\n"
    fn_name params_str body

(* --- Page to render function --- *)

let page_to_dream (tm : theme_map) (_app : ui_app) (pg : page) =
  let fn_name = "render_" ^ String.map (fun c -> if c = '-' then '_' else c) pg.name in
  let layout_fn = "render_" ^ String.map (fun c -> if c = '-' then '_' else c) pg.layout_name ^ "_layout" in
  (* Build named fill arguments from page fills *)
  let fill_args = List.map (fun (fill : slot_fill) ->
    let content = List.map (element_to_dream tm) fill.content in
    let content_str = String.concat "; " content in
    let slot_name = String.map (fun c -> if c = '-' then '_' else c) fill.slot_name in
    Printf.sprintf "~%s:[%s]" slot_name content_str
  ) pg.fills in
  let fill_str = String.concat " " fill_args in
  Printf.sprintf "let %s () : node =\n  %s %s ()\n"
    fn_name layout_fn fill_str

(* --- Route to Dream handler --- *)

let route_to_dream (route : route) =
  let page_fn = "render_" ^ String.map (fun c -> if c = '-' then '_' else c) route.page_name in
  Printf.sprintf "  Dream.get %S (fun _ -> Dream_html.respond (%s ()))"
    route.path page_fn

(* --- Main generation --- *)

let generate ?(source_path = "<spec>") (app : ui_app) =
  let tm = build_theme_map app.theme in
  let buf = Buffer.create 4096 in

  (* Header *)
  Buffer.add_string buf (Printf.sprintf
    "(* Generated by borge generate --target ocaml-dream *)\n\
     (* Source: %s *)\n\
     (* DO NOT EDIT: regenerate from spec *)\n\n\
     open Dream_html\nopen HTML\n\n" source_path);

  (* CSS custom properties from theme *)
  let css_vars = theme_to_css_vars tm in
  if css_vars <> "" then
    Buffer.add_string buf (Printf.sprintf "let theme_css = %S\n\n" css_vars);

  (* Variant media queries from all elements *)
  let rec collect_variants_from_element (el : ui_element) =
    collect_variant_styles tm el @ List.concat_map (function El child -> collect_variants_from_element child | Slot_ref _ -> []) el.children
  in
  let all_variants =
    List.concat_map collect_variants_from_element app.elements
    @ List.concat_map (fun (comp : component) ->
        collect_variants_from_element
          { name = Some comp.name; properties = comp.properties;
            children = comp.children; variants = comp.variants; text = None }) app.components
    @ List.concat_map (fun (layout : layout_def) ->
        collect_variants_from_element layout.root) app.layouts
  in
  if all_variants <> [] then begin
    Buffer.add_string buf "(* Variant media queries *)\n\n";
    Buffer.add_string buf (Printf.sprintf "let variant_css = %S\n\n"
      (String.concat "\n" all_variants));
  end;

  (* Components *)
  if app.components <> [] then begin
    Buffer.add_string buf "(* Components *)\n\n";
    List.iter (fun comp ->
      Buffer.add_string buf (component_to_dream tm comp);
      Buffer.add_char buf '\n';
    ) app.components;
  end;

  (* Layouts *)
  if app.layouts <> [] then begin
    Buffer.add_string buf "(* Layouts *)\n\n";
    List.iter (fun layout ->
      Buffer.add_string buf (layout_to_dream tm layout);
      Buffer.add_char buf '\n';
    ) app.layouts;
  end;

  (* Pages *)
  if app.pages <> [] then begin
    Buffer.add_string buf "(* Pages *)\n\n";
    List.iter (fun pg ->
      Buffer.add_string buf (page_to_dream tm app pg);
      Buffer.add_char buf '\n';
    ) app.pages;
  end;

  (* Routes *)
  if app.routes <> [] then begin
    Buffer.add_string buf "(* Routes *)\n\n";
    Buffer.add_string buf "let register_routes () = Dream.router [\n";
    List.iter (fun r ->
      Buffer.add_string buf (route_to_dream r);
      Buffer.add_string buf ";\n";
    ) app.routes;
    Buffer.add_string buf "]\n";
  end;

  (* Elements — top-level, not in a component/layout/page *)
  if app.elements <> [] then begin
    Buffer.add_string buf "(* Elements *)\n\n";
    List.iter (fun (el : ui_element) ->
      let el_name = match el.name with Some n -> String.map (fun c -> if c = '-' then '_' else c) n | None -> "el" in
      Buffer.add_string buf (Printf.sprintf "let _el_%s : node = %s\n" el_name (element_to_dream tm el));
    ) app.elements;
  end;

  Buffer.contents buf
