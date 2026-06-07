(** Semantic validation for borge UI specs.

    Checks that references are valid:
    - Variant names are unique per element
    - Theme variable references refer to defined entries
    - Page layout references are defined
    - Component and layout name uniqueness *)

open Ui_ast

type issue = {
  message : string;
  severity : [ `Error | `Warning ];
}

let err msg = { message = msg; severity = `Error }
let warn msg = { message = msg; severity = `Warning }

(** Collect defined palette names from the theme *)
let palette_names (theme : theme option) : string list =
  match theme with
  | None -> []
  | Some t ->
    List.filter_map (function
      | Palette_entry (name, _) -> Some name
      | _ -> None
    ) t.entries

(** Collect defined spacing names from the theme *)
let spacing_names (theme : theme option) : string list =
  match theme with
  | None -> []
  | Some t ->
    List.filter_map (function
      | Spacing_entry (name, _) -> Some name
      | _ -> None
    ) t.entries

(** Collect defined font-size names from the theme *)
let font_size_names (theme : theme option) : string list =
  match theme with
  | None -> []
  | Some t ->
    List.filter_map (function
      | Font_size_entry (name, _) -> Some name
      | _ -> None
    ) t.entries

(** Collect defined radius names from the theme *)
let radius_names (theme : theme option) : string list =
  match theme with
  | None -> []
  | Some t ->
    List.filter_map (function
      | Radius_entry (name, _) -> Some name
      | _ -> None
    ) t.entries

(** Check a color value against defined palette names *)
let rec check_color (palette : string list) = function
  | Palette name when not (List.mem name palette) ->
    [err (Printf.sprintf "undefined palette reference: %s" name)]
  | With_alpha (c, _) ->
    check_color palette c
  | _ -> []

(** Check a spacing value against defined spacing names *)
let check_spacing (spacings : string list) = function
  | Spacing_var name when not (List.mem name spacings) ->
    [err (Printf.sprintf "undefined spacing reference: %s" name)]
  | _ -> []

(** Check a font-size value against defined font-size names *)
let check_font_size (font_sizes : string list) = function
  | Font_var name when not (List.mem name font_sizes) ->
    [err (Printf.sprintf "undefined font-size reference: %s" name)]
  | _ -> []

(** Check a radius value against defined radius names *)
let check_radius (radii : string list) = function
  | Radius_var name when not (List.mem name radii) ->
    [err (Printf.sprintf "undefined radius reference: %s" name)]
  | _ -> []

(** Check variant names are unique within an element *)
let check_variant_uniqueness (variants : variant list) : issue list =
  let seen = Hashtbl.create 8 in
  List.filter_map (fun (v : variant) ->
    if Hashtbl.mem seen v.name then
      Some (err (Printf.sprintf "duplicate variant name: %s" v.name))
    else begin
      Hashtbl.add seen v.name true;
      None
    end
  ) variants

(** Validate a single UI property against theme definitions *)
let check_property (palette, spacings, _font_sizes, radii) = function
  | P_bg c -> check_color palette c
  | P_color c -> check_color palette c
  | P_gap s -> check_spacing spacings s
  | P_padding (Padding_uniform s) -> check_spacing spacings s
  | P_padding (Padding_sides { top = t; right = r; bottom = b; left = l }) ->
    check_spacing spacings t @ check_spacing spacings r
    @ check_spacing spacings b @ check_spacing spacings l
  | P_corner (Corner_uniform r) -> check_radius radii r
  | _ -> []

(** Recursively validate a UI element *)
let rec check_element (theme_ctx, component_names) (el : ui_element) : issue list =
  let props = List.concat_map
    (check_property theme_ctx) el.properties in
  let variant_issues = check_variant_uniqueness el.variants in
  let variant_props = List.concat_map (fun (v : variant) ->
    List.concat_map (check_property theme_ctx) v.properties
  ) el.variants in
  let children = List.concat_map
    (function El el -> check_element (theme_ctx, component_names) el | Slot_ref _ -> []) el.children in
  props @ variant_issues @ variant_props @ children

(** Validate a page: check layout reference *)
let check_page (layout_names : string list) (page : page) : issue list =
  if page.layout_name <> "" && not (List.mem page.layout_name layout_names) then
    [err (Printf.sprintf "page '%s' references undefined layout: %s"
      page.name page.layout_name)]
  else []

(** Validate a component *)
let check_component (theme_ctx, component_names) (comp : component) : issue list =
  let props = List.concat_map
    (check_property theme_ctx) comp.properties in
  let variant_issues = check_variant_uniqueness comp.variants in
  let children = List.concat_map
    (function El el -> check_element (theme_ctx, component_names) el | Slot_ref _ -> []) comp.children in
  props @ variant_issues @ children

(** Validate a layout: check root element tree *)
let check_layout (theme_ctx, component_names) (ly : layout_def) : issue list =
  check_element (theme_ctx, component_names) ly.root

(** Validate the complete UI app spec *)
let validate (app : ui_app) : issue list =
  let palette = palette_names app.theme in
  let spacings = spacing_names app.theme in
  let font_sizes = font_size_names app.theme in
  let radii = radius_names app.theme in
  let theme_ctx = (palette, spacings, font_sizes, radii) in
  let component_names = List.map (fun (c : component) -> c.name) app.components in
  let layout_names = List.map (fun (ly : layout_def) -> ly.name) app.layouts in

  (* Check component name uniqueness *)
  let comp_name_issues =
    let seen = Hashtbl.create 8 in
    List.filter_map (fun (c : component) ->
      if Hashtbl.mem seen c.name then
        Some (err (Printf.sprintf "duplicate component name: %s" c.name))
      else begin
        Hashtbl.add seen c.name true;
        None
      end
    ) app.components
  in

  (* Check layout name uniqueness *)
  let layout_name_issues =
    let seen = Hashtbl.create 8 in
    List.filter_map (fun (ly : layout_def) ->
      if Hashtbl.mem seen ly.name then
        Some (err (Printf.sprintf "duplicate layout name: %s" ly.name))
      else begin
        Hashtbl.add seen ly.name true;
        None
      end
    ) app.layouts
  in

  let component_issues = List.concat_map
    (check_component (theme_ctx, component_names)) app.components in
  let layout_issues = List.concat_map
    (check_layout (theme_ctx, component_names)) app.layouts in
  let page_issues = List.concat_map
    (check_page layout_names) app.pages in
  let element_issues = List.concat_map
    (check_element (theme_ctx, component_names)) app.elements in

  comp_name_issues @ layout_name_issues @ component_issues
  @ layout_issues @ page_issues @ element_issues
