(** HTML/CSS generator for borge UI specs.

    Generates HTML structure + CSS from a ui_app.
    The output is a proof-of-concept static target — the design
    is renderer-agnostic and an LLM can use this as scaffolding. *)

open Ui_ast

(** Convert a name to a CSS class name *)
let class_name (s : string) : string =
  String.map (fun c -> if c = '-' then '_' else c) s

(** Convert a name to a CSS class selector *)
let class_selector (s : string) : string =
  "." ^ class_name s

(** CSS spacing value *)
let css_spacing = function
  | Spacing_px n -> Printf.sprintf "%dpx" n
  | Spacing_rem f -> Printf.sprintf "%grem" f
  | Spacing_var name -> Printf.sprintf "var(--spacing-%s)" (class_name name)

(** CSS font-size value *)
let css_font_size = function
  | Font_px n -> Printf.sprintf "%dpx" n
  | Font_rem f -> Printf.sprintf "%grem" f
  | Font_var name -> Printf.sprintf "var(--font-size-%s)" (class_name name)

(** CSS radius value *)
let css_radius = function
  | Radius_px n -> Printf.sprintf "%dpx" n
  | Radius_var name -> Printf.sprintf "var(--radius-%s)" (class_name name)

(** CSS sizing value *)
let css_sizing = function
  | Flex n -> Printf.sprintf "%d" n
  | Grow -> "1"
  | Full -> "100%"
  | Auto -> "auto"
  | Px n -> Printf.sprintf "%dpx" n
  | Rem f -> Printf.sprintf "%grem" f
  | Pct n -> Printf.sprintf "%d%%" n
  | Vh n -> Printf.sprintf "%dvh" n

(** CSS color value *)
let rec css_color = function
  | Hex h -> h
  | Keyword k -> k  (* CSS keyword colors *)
  | Palette name -> Printf.sprintf "var(--palette-%s)" (class_name name)
  | With_alpha (c, a) ->
    Printf.sprintf "color-mix(in srgb, %s %d%%, transparent)"
      (css_color c) (int_of_float (a *. 100.0))

(** CSS padding *)
let css_padding = function
  | Padding_uniform s -> css_spacing s
  | Padding_sides { top = t; right = r; bottom = b; left = l } ->
    Printf.sprintf "%s %s %s %s" (css_spacing t) (css_spacing r) (css_spacing b) (css_spacing l)

(** CSS layout direction *)
let css_layout = function
  | Horizontal -> "row"
  | Vertical -> "column"

(** CSS alignment *)
let css_alignment = function
  | Align_start -> "flex-start"
  | Align_center -> "center"
  | Align_end -> "flex-end"

(** Generate CSS custom properties from theme *)
let theme_css (t : theme) : string =
  let vars = List.concat_map (function
    | Palette_entry (name, value) ->
      [Printf.sprintf "  --palette-%s: %s;" (class_name name) value]
    | Spacing_entry (name, value) ->
      [Printf.sprintf "  --spacing-%s: %dpx;" (class_name name) value]
    | Font_size_entry (name, value) ->
      [Printf.sprintf "  --font-size-%s: %dpx;" (class_name name) value]
    | Radius_entry (name, value) ->
      [Printf.sprintf "  --radius-%s: %dpx;" (class_name name) value]
  ) t.entries in
  ":root {\n" ^ String.concat "\n" vars ^ "\n}\n"

(** Generate CSS for a ui_property *)
let property_css = function
  | P_layout d -> [Printf.sprintf "flex-direction: %s;" (css_layout d)]
  | P_width s -> [Printf.sprintf "width: %s;" (css_sizing s)]
  | P_height s -> [Printf.sprintf "height: %s;" (css_sizing s)]
  | P_padding p -> [Printf.sprintf "padding: %s;" (css_padding p)]
  | P_gap s -> [Printf.sprintf "gap: %s;" (css_spacing s)]
  | P_bg c -> [Printf.sprintf "background: %s;" (css_color c)]
  | P_color c -> [Printf.sprintf "color: %s;" (css_color c)]
  | P_corner (Corner_uniform r) -> [Printf.sprintf "border-radius: %s;" (css_radius r)]
  | P_corner (Corner_sides { top_left = tl; top_right = tr; bottom_left = bl; bottom_right = br }) ->
    [Printf.sprintf "border-radius: %s %s %s %s;"
      (css_radius tl) (css_radius tr) (css_radius br) (css_radius bl)]
  | P_border b ->
    let w = match b.width with Some n -> Printf.sprintf "%dpx" n | None -> "1px" in
    let c = match b.color with Some cv -> css_color cv | None -> "currentColor" in
    [Printf.sprintf "border: %s solid %s;" w c]
  | P_scroll d ->
    [Printf.sprintf "overflow: %s;"
      (match d with Scroll_vertical -> "auto" | Scroll_horizontal -> "auto" | Scroll_both -> "auto")]
  | P_align_x a -> [Printf.sprintf "align-items: %s;" (css_alignment a)]
  | P_align_y a -> [Printf.sprintf "justify-content: %s;" (css_alignment a)]
  | P_interactive _ -> []  (* no direct CSS for interactivity *)
  | P_float _ -> []  (* complex — skip for now *)

(** Recursively generate CSS for an element, including variant selectors *)
let rec element_css (indent : int) (el : ui_element) : string list =
  let prefix = String.make indent ' ' in
  let sel = match el.name with
    | Some n -> prefix ^ class_selector n
    | None -> prefix ^ "div"
  in
  let flex_base = if el.properties <> [] then ["  display: flex;"] else [] in
  let props = List.concat_map property_css el.properties in
  let own_css =
    if flex_base @ props = [] then []
    else [sel ^ " {\n  " ^ String.concat "\n  " (flex_base @ props) ^ "\n" ^ prefix ^ "}"]
  in
  (* Variant selectors: .button.primary, .button.secondary *)
  let variant_css = List.concat_map (fun (v : variant) ->
    let v_sel = match el.name with
      | Some n -> prefix ^ class_selector n ^ "." ^ class_name v.name
      | None -> prefix ^ "div." ^ class_name v.name
    in
    let v_props = List.concat_map property_css v.properties in
    if v_props = [] then []
    else [v_sel ^ " {\n  " ^ String.concat "\n  " v_props ^ "\n" ^ prefix ^ "}"]
  ) el.variants in
  let children_css = List.concat_map (element_css indent) el.children in
  own_css @ variant_css @ children_css

(** Find a layout by name *)
let find_layout (app : ui_app) (name : string) : layout_def option =
  List.find_opt (fun (ly : layout_def) -> ly.name = name) app.layouts

(** Find a component by name *)
let find_component (app : ui_app) (name : string) : component option =
  List.find_opt (fun (c : component) -> c.name = name) app.components

(** Generate HTML for an element, using component definitions when name matches *)
let rec element_html_with_components (indent : int) (comps : (string, component) Hashtbl.t) (el : ui_element) : string =
  let prefix = String.make indent ' ' in
  (* Check if this element is a component instantiation *)
  let comp_children = match el.name with
    | Some n ->
      (match Hashtbl.find_opt comps n with
      | Some comp -> Some comp.children
      | None -> None)
    | None -> None
  in
  let cls = match el.name with
    | Some n -> Printf.sprintf " class=\"%s\"" (class_name n)
    | None -> ""
  in
  let children = match comp_children with
    | Some comp_ch ->
      (* Render component's children as the element's content *)
      List.map (element_html_with_components (indent + 2) comps) comp_ch
    | None ->
      List.map (element_html_with_components (indent + 2) comps) el.children
  in
  if children = [] then
    Printf.sprintf "%s<div%s></div>" prefix cls
  else
    Printf.sprintf "%s<div%s>\n%s\n%s</div>"
      prefix cls (String.concat "\n" children) prefix

(** Generate HTML for an element (basic, no component lookup) *)
and element_html (indent : int) (el : ui_element) : string =
  let prefix = String.make indent ' ' in
  let cls = match el.name with
    | Some n -> Printf.sprintf " class=\"%s\"" (class_name n)
    | None -> ""
  in
  if el.children = [] then
    Printf.sprintf "%s<div%s></div>" prefix cls
  else
    let children = List.map (element_html (indent + 2)) el.children in
    Printf.sprintf "%s<div%s>\n%s\n%s</div>"
      prefix cls (String.concat "\n" children) prefix

(** Render a layout's root element, replacing slot markers with content *)
let rec render_layout_html (indent : int) (comps : (string, component) Hashtbl.t) (el : ui_element) (fills : slot_fill list) : string =
  let prefix = String.make indent ' ' in
  let cls = match el.name with
    | Some n -> Printf.sprintf " class=\"%s\"" (class_name n)
    | None -> ""
  in
  (* Check if this element is a slot placeholder — indicated by name matching a fill *)
  let is_slot = match el.name with
    | Some n -> List.exists (fun (f : slot_fill) -> f.slot_name = n) fills
    | None -> false
  in
  if is_slot then begin
    (* Replace with the fill content *)
    let fill = List.find (fun (f : slot_fill) -> f.slot_name = Option.get el.name) fills in
    let content = List.map (element_html_with_components (indent + 2) comps) fill.content in
    Printf.sprintf "%s<div%s>\n%s\n%s</div>"
      prefix cls (String.concat "\n" content) prefix
  end else if el.children = [] then
    Printf.sprintf "%s<div%s></div>" prefix cls
  else
    let children = List.map (fun child ->
      render_layout_html (indent + 2) comps child fills
    ) el.children in
    Printf.sprintf "%s<div%s>\n%s\n%s</div>"
      prefix cls (String.concat "\n" children) prefix

(** Generate the full HTML page for a ui_app *)
let generate_html (app : ui_app) : string =
  let comps = Hashtbl.create 8 in
  List.iter (fun (c : component) -> Hashtbl.add comps c.name c) app.components;
  match app.pages with
  | [] -> "<!-- No pages defined -->\n"
  | page :: _ ->
    let body =
      match find_layout app page.layout_name with
      | Some ly ->
        render_layout_html 4 comps ly.root page.fills
      | None ->
        "<!-- Layout not found: " ^ page.layout_name ^ " -->"
    in
    Printf.sprintf "<!DOCTYPE html>\n<html>\n<head>\n  <title>%s</title>\n  <link rel=\"stylesheet\" href=\"style.css\">\n</head>\n<body>\n%s\n</body>\n</html>"
      page.name body

(** Generate the full CSS for a ui_app *)
let generate_css (app : ui_app) : string =
  let theme_part = match app.theme with
    | Some t -> theme_css t
    | None -> ""
  in
  let component_css = List.concat_map (fun (c : component) ->
    let el : ui_element = {
      name = Some c.name;
      properties = c.properties;
      children = c.children;
      variants = c.variants;
    } in
    element_css 0 el
  ) app.components in
  let layout_css = List.concat_map (fun (ly : layout_def) ->
    element_css 0 ly.root
  ) app.layouts in
  let element_css_all = List.concat_map (element_css 0) app.elements in
  String.concat "\n\n" (List.filter (fun s -> s <> "") (
    [theme_part] @ component_css @ layout_css @ element_css_all
  )) ^ "\n"
