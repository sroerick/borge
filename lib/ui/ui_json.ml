(** JSON serialization for borge UI specs.

    Produces Yojson-compatible JSON from the typed UI AST.
    Used by the --json flag on UI-related commands. *)

open Ui_ast

(* exempt doc: simple JSON converter pattern - name is self-documenting *)
let sizing_to_json = function
  | Flex n -> `Assoc [("flex", `Int n)]
  | Grow -> `String "grow"
  | Full -> `String "full"
  | Auto -> `String "auto"
  | Px n -> `Assoc [("px", `Int n)]
  | Rem n -> `Assoc [("rem", `Float n)]
  | Pct n -> `Assoc [("pct", `Int n)]
  | Vh n -> `Assoc [("vh", `Int n)]

(* exempt doc: simple recursive JSON converter - name is self-documenting *)
let rec color_to_json = function
  | Hex s -> `Assoc [("hex", `String s)]
  | Keyword s -> `Assoc [("keyword", `String s)]
  | Palette s -> `Assoc [("palette", `String s)]
  | With_alpha (c, a) -> `Assoc [("color", color_to_json c); ("alpha", `Float a)]

(* exempt doc: simple JSON converter - name is self-documenting *)
let spacing_to_json = function
  | Spacing_px n -> `Int n
  | Spacing_rem n -> `Float n
  | Spacing_var s -> `Assoc [("spacing", `String s)]

(* exempt doc: simple JSON converter - name is self-documenting *)
let theme_entry_to_json = function
  | Palette_entry (name, value) -> `Assoc [("palette", `String name); ("value", `String value)]
  | Spacing_entry (name, value) -> `Assoc [("spacing", `String name); ("value", `Int value)]
  | Font_size_entry (name, value) -> `Assoc [("font-size", `String name); ("value", `Int value)]
  | Radius_entry (name, value) -> `Assoc [("radius", `String name); ("value", `Int value)]

(* exempt doc: simple JSON converter - name is self-documenting *)
let padding_to_json = function
  | Padding_uniform s -> `Assoc [("uniform", spacing_to_json s)]
  | Padding_sides { top; right; bottom; left } ->
    `Assoc [("top", spacing_to_json top); ("right", spacing_to_json right);
            ("bottom", spacing_to_json bottom); ("left", spacing_to_json left)]

(* exempt doc: simple JSON converter - name is self-documenting *)
let border_spec_to_json (b : Ui_ast.border_spec) =
  `Assoc ((match b.width with Some w -> [("width", `Int w)] | None -> [])
          @ (match b.color with Some c -> [("color", color_to_json c)] | None -> [])
          @ (match b.side with Some s -> [("side", `String s)] | None -> []))

(* exempt doc: simple JSON converter - name is self-documenting *)
let radius_to_json = function
  | Radius_px n -> `Int n
  | Radius_var s -> `Assoc [("radius", `String s)]

(* exempt doc: simple JSON converter - name is self-documenting *)
let corner_radius_to_json = function
  | Corner_uniform r -> `Assoc [("uniform", radius_to_json r)]
  | Corner_sides { top_left; top_right; bottom_left; bottom_right } ->
    `Assoc [("top_left", radius_to_json top_left); ("top_right", radius_to_json top_right);
            ("bottom_left", radius_to_json bottom_left); ("bottom_right", radius_to_json bottom_right)]

(* exempt doc: recursive JSON converter - name is self-documenting *)
let rec variant_to_json (v : Ui_ast.variant) =
  `Assoc [
    ("name", `String v.name);
    ("properties", `List (List.map property_to_json v.properties));
    ("children", match v.children with
     | Some cs -> `List (List.map ui_element_to_json cs)
     | None -> `Null);
  ]

(* exempt doc: pattern match to JSON - name is self-documenting *)
and property_to_json = function
  | P_layout d -> `Assoc [("layout", `String (match d with Horizontal -> "horizontal" | Vertical -> "vertical"))]
  | P_width s -> `Assoc [("width", sizing_to_json s)]
  | P_height s -> `Assoc [("height", sizing_to_json s)]
  | P_padding p -> `Assoc [("padding", padding_to_json p)]
  | P_gap s -> `Assoc [("gap", spacing_to_json s)]
  | P_bg c -> `Assoc [("bg", color_to_json c)]
  | P_color c -> `Assoc [("color", color_to_json c)]
  | P_border b -> `Assoc [("border", border_spec_to_json b)]
  | P_corner c -> `Assoc [("corner", corner_radius_to_json c)]
  | P_scroll d -> `Assoc [("scroll", `String (match d with Scroll_vertical -> "vertical" | Scroll_horizontal -> "horizontal" | Scroll_both -> "both"))]
  | P_float _ -> `Assoc [("float", `String "simple")]
  | P_interactive t -> `Assoc [("interactive", `String (match t with Interactive_click -> "click" | Interactive_type -> "type" | Interactive_hover -> "hover"))]
  | P_align_x a -> `Assoc [("align-x", `String (match a with Align_start -> "start" | Align_center -> "center" | Align_end -> "end"))]
  | P_align_y a -> `Assoc [("align-y", `String (match a with Align_start -> "start" | Align_center -> "center" | Align_end -> "end"))]

(* exempt doc: recursive JSON converter - name is self-documenting *)
and ui_element_to_json el =
  let props = `List (List.map property_to_json el.properties) in
  let children = `List (List.map (function El el -> ui_element_to_json el | Slot_ref name -> `Assoc [("slot", `String name)]) el.children) in
  let variants = `List (List.map variant_to_json el.variants) in
  `Assoc [
    ("name", match el.name with Some n -> `String n | None -> `Null);
    ("properties", props);
    ("children", children);
    ("variants", variants);
    ("text", match el.text with Some t -> `String t | None -> `Null);
  ]

(* exempt doc: JSON converter for component - name is self-documenting *)
let component_to_json (c : Ui_ast.component) =
  `Assoc [
    ("name", `String c.name);
    ("properties", `List (List.map property_to_json c.properties));
    ("slots", `List (List.map (function Default_slot -> `String "..content" | Named_slot s -> `Assoc [("slot", `String s)]) c.slots));
    ("variants", `List (List.map variant_to_json c.variants));
  ]

(* exempt doc: JSON converter for layout - name is self-documenting *)
let layout_to_json (ly : Ui_ast.layout_def) =
  `Assoc [
    ("name", `String ly.name);
    ("root", ui_element_to_json ly.root);
    ("slots", `List (List.map (function Default_slot -> `String "..content" | Named_slot s -> `Assoc [("slot", `String s)]) ly.slots));
  ]

(* exempt doc: JSON converter for page - name is self-documenting *)
let page_to_json p =
  `Assoc [
    ("name", `String p.name);
    ("layout", `String p.layout_name);
    ("fills", `List (List.map (fun (f : Ui_ast.slot_fill) ->
      `Assoc [("slot", `String f.slot_name); ("content", `List (List.map ui_element_to_json f.content))]
    ) p.fills));
  ]

(* exempt doc: JSON converter for route - name is self-documenting *)
let route_to_json r =
  `Assoc [("path", `String r.path); ("page", `String r.page_name)]

(* exempt doc: JSON converter for full app - name is self-documenting *)
let ui_app_to_json app =
  let theme = match app.theme with
    | Some t -> `Assoc [("entries", `List (List.map theme_entry_to_json t.entries))]
    | None -> `Null
  in
  `Assoc [
    ("theme", theme);
    ("components", `List (List.map component_to_json app.components));
    ("layouts", `List (List.map layout_to_json app.layouts));
    ("pages", `List (List.map page_to_json app.pages));
    ("routes", `List (List.map route_to_json app.routes));
    ("elements", `List (List.map ui_element_to_json app.elements));
  ]
