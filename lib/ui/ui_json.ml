(** JSON serialization for borge UI specs.

    Produces Yojson-compatible JSON from the typed UI AST.
    Used by the --json flag on UI-related commands. *)

open Ui_ast

let sizing_to_json = function
  | Flex n -> `Assoc [("flex", `Int n)]
  | Grow -> `String "grow"
  | Full -> `String "full"
  | Auto -> `String "auto"
  | Px n -> `Assoc [("px", `Int n)]
  | Rem n -> `Assoc [("rem", `Float n)]
  | Pct n -> `Assoc [("pct", `Int n)]
  | Vh n -> `Assoc [("vh", `Int n)]

let rec color_to_json = function
  | Hex s -> `Assoc [("hex", `String s)]
  | Keyword s -> `Assoc [("keyword", `String s)]
  | Palette s -> `Assoc [("palette", `String s)]
  | With_alpha (c, a) -> `Assoc [("color", color_to_json c); ("alpha", `Float a)]

let spacing_to_json = function
  | Spacing_px n -> `Int n
  | Spacing_rem n -> `Float n
  | Spacing_var s -> `Assoc [("spacing", `String s)]

let theme_entry_to_json = function
  | Palette_entry (name, value) -> `Assoc [("palette", `String name); ("value", `String value)]
  | Spacing_entry (name, value) -> `Assoc [("spacing", `String name); ("value", `Int value)]
  | Font_size_entry (name, value) -> `Assoc [("font-size", `String name); ("value", `Int value)]
  | Radius_entry (name, value) -> `Assoc [("radius", `String name); ("value", `Int value)]

let rec variant_to_json (v : Ui_ast.variant) =
  `Assoc [
    ("name", `String v.name);
    ("properties", `List (List.map property_to_json v.properties));
    ("children", match v.children with
     | Some cs -> `List (List.map ui_element_to_json cs)
     | None -> `Null);
  ]

and property_to_json = function
  | P_layout d -> `Assoc [("layout", `String (match d with Horizontal -> "horizontal" | Vertical -> "vertical"))]
  | P_width s -> `Assoc [("width", sizing_to_json s)]
  | P_height s -> `Assoc [("height", sizing_to_json s)]
  | P_padding _ -> `Assoc [("padding", `String "(* TODO *)")]
  | P_gap s -> `Assoc [("gap", spacing_to_json s)]
  | P_bg c -> `Assoc [("bg", color_to_json c)]
  | P_color c -> `Assoc [("color", color_to_json c)]
  | P_border _ -> `Assoc [("border", `String "(* TODO *)")]
  | P_corner _ -> `Assoc [("corner", `String "(* TODO *)")]
  | P_scroll d -> `Assoc [("scroll", `String (match d with Scroll_vertical -> "vertical" | Scroll_horizontal -> "horizontal" | Scroll_both -> "both"))]
  | P_float _ -> `Assoc [("float", `String "simple")]
  | P_interactive t -> `Assoc [("interactive", `String (match t with Interactive_click -> "click" | Interactive_type -> "type" | Interactive_hover -> "hover"))]
  | P_align_x a -> `Assoc [("align-x", `String (match a with Align_start -> "start" | Align_center -> "center" | Align_end -> "end"))]
  | P_align_y a -> `Assoc [("align-y", `String (match a with Align_start -> "start" | Align_center -> "center" | Align_end -> "end"))]

and ui_element_to_json el =
  let props = `List (List.map property_to_json el.properties) in
  let children = `List (List.map ui_element_to_json el.children) in
  let variants = `List (List.map variant_to_json el.variants) in
  `Assoc [
    ("name", match el.name with Some n -> `String n | None -> `Null);
    ("properties", props);
    ("children", children);
    ("variants", variants);
  ]

let component_to_json (c : Ui_ast.component) =
  `Assoc [
    ("name", `String c.name);
    ("properties", `List (List.map property_to_json c.properties));
    ("slots", `List (List.map (function Default_slot -> `String "..content" | Named_slot s -> `Assoc [("slot", `String s)]) c.slots));
    ("variants", `List (List.map variant_to_json c.variants));
  ]

let page_to_json p =
  `Assoc [
    ("name", `String p.name);
    ("layout", `String p.layout_name);
  ]

let route_to_json r =
  `Assoc [("path", `String r.path); ("page", `String r.page_name)]

let ui_app_to_json app =
  let theme = match app.theme with
    | Some t -> `Assoc [("entries", `List (List.map theme_entry_to_json t.entries))]
    | None -> `Null
  in
  `Assoc [
    ("theme", theme);
    ("components", `List (List.map component_to_json app.components));
    ("pages", `List (List.map page_to_json app.pages));
    ("routes", `List (List.map route_to_json app.routes));
    ("elements", `List (List.map ui_element_to_json app.elements));
  ]
