(** Extract typed UI AST from generic sexp trees.

    Walks the Ast.sexp tree produced by the borge parser and
    extracts UI-specific typed nodes. The parser stays generic;
    this module interprets the semantic structure of (ui ...),
    (theme ...), (component ...), (layout ...), (page ...),
    and (routes ...) forms. *)

open Ui_ast
open Borge_lang.Ast

(** Helpers for working with the sexp tree *)

let atom_name = function
  | Atom (_, name) -> Some name
  | _ -> None

let string_value = function
  | String (_, Quoted { q_content }) -> Some q_content
  | String (_, Verbatim { v_content }) -> Some v_content
  | _ -> None

(** Find all children of a list form matching a keyword *)
let find_forms keyword sexps =
  List.filter_map (fun swc ->
    match swc.node with
    | List (_, Atom (_, k) :: rest) when k = keyword ->
      Some (k, rest, swc)
    | _ -> None
  ) sexps

(** Find first form matching a keyword *)
let find_form keyword sexps =
  match find_forms keyword sexps with
  | [x] -> Some x
  | _ -> None

(** Extract the name from (keyword name ...) *)
let form_name = function
  | Atom (_, name) :: _ -> Some name
  | _ -> None

(** Parse sizing: integer = flex parts, or explicit units *)
let parse_sizing = function
  | Atom (_, "grow") -> Some Grow
  | Atom (_, "full") -> Some Full
  | Atom (_, "auto") -> Some Auto
  | Atom (_, s) ->
    (try Some (Flex (int_of_string s))
     with Failure _ -> None)
  | List (_, [Atom (_, "px"); Atom (_, n)]) ->
    (try Some (Px (int_of_string n)) with Failure _ -> None)
  | List (_, [Atom (_, "rem"); Atom (_, n)]) ->
    (try Some (Rem (float_of_string n)) with Failure _ -> None)
  | List (_, [Atom (_, "pct"); Atom (_, n)]) ->
    (try Some (Pct (int_of_string n)) with Failure _ -> None)
  | List (_, [Atom (_, "vh"); Atom (_, n)]) ->
    (try Some (Vh (int_of_string n)) with Failure _ -> None)
  | _ -> None

(** Parse color: hex string, keyword, or palette reference *)
let parse_color = function
  | Atom (_, s) when String.length s > 0 && s.[0] = '#' ->
    Some (Hex s)
  | Atom (_, s) ->
    (* Check if it's a known keyword color *)
    let keywords = ["red"; "orange"; "yellow"; "green"; "blue";
                     "purple"; "pink"; "white"; "black"; "gray";
                     "dark"; "light"; "transparent"] in
    if List.mem s keywords then Some (Keyword s)
    else None
  | List (_, [Atom (_, "palette"); Atom (_, name)]) ->
    Some (Palette name)
  | List (_, [Atom (_, "alpha"); _]) ->
    (* (alpha 0.6) — handled by caller *)
    None
  | _ -> None

(** Parse spacing: integer px, (rem N), or (spacing name) *)
let parse_spacing = function
  | Atom (_, n) ->
    (try Some (Spacing_px (int_of_string n))
     with Failure _ -> None)
  | List (_, [Atom (_, "rem"); Atom (_, n)]) ->
    (try Some (Spacing_rem (float_of_string n)) with Failure _ -> None)
  | List (_, [Atom (_, "spacing"); Atom (_, name)]) ->
    Some (Spacing_var name)
  | _ -> None

(** Parse font-size: integer px, (rem N), or (font-size name) *)
let parse_font_size = function
  | Atom (_, n) ->
    (try Some (Font_px (int_of_string n))
     with Failure _ -> None)
  | List (_, [Atom (_, "rem"); Atom (_, n)]) ->
    (try Some (Font_rem (float_of_string n)) with Failure _ -> None)
  | List (_, [Atom (_, "font-size"); Atom (_, name)]) ->
    Some (Font_var name)
  | _ -> None

(** Parse radius: integer px or (radius name) *)
let parse_radius = function
  | Atom (_, n) ->
    (try Some (Radius_px (int_of_string n))
     with Failure _ -> None)
  | List (_, [Atom (_, "radius"); Atom (_, name)]) ->
    Some (Radius_var name)
  | _ -> None

(** Parse layout direction *)
let parse_layout = function
  | Atom (_, "horizontal") -> Some Horizontal
  | Atom (_, "vertical") -> Some Vertical
  | _ -> None

(** Parse scroll direction *)
let parse_scroll = function
  | Atom (_, "vertical") -> Some Scroll_vertical
  | Atom (_, "horizontal") -> Some Scroll_horizontal
  | Atom (_, "both") -> Some Scroll_both
  | _ -> None

(** Parse interactive type *)
let parse_interactive = function
  | Atom (_, "click") -> Some Interactive_click
  | Atom (_, "type") -> Some Interactive_type
  | Atom (_, "hover") -> Some Interactive_hover
  | _ -> None

(** Parse a single UI property from a sexp form *)
let rec parse_property = function
  | List (_, Atom (_, "layout") :: dir :: _) ->
    Option.map (fun d -> P_layout d) (parse_layout dir)
  | List (_, Atom (_, "width") :: s :: _) ->
    Option.map (fun s -> P_width s) (parse_sizing s)
  | List (_, Atom (_, "height") :: s :: _) ->
    Option.map (fun s -> P_height s) (parse_sizing s)
  | List (_, Atom (_, "padding") :: rest) ->
    (match parse_padding rest with
     | Some p -> Some (P_padding p)
     | None -> None)
  | List (_, Atom (_, "gap") :: s :: _) ->
    Option.map (fun s -> P_gap s) (parse_spacing s)
  | List (_, Atom (_, "bg") :: c :: _) ->
    Option.map (fun c -> P_bg c) (parse_color c)
  | List (_, Atom (_, "color") :: c :: _) ->
    Option.map (fun c -> P_color c) (parse_color c)
  | List (_, Atom (_, "scroll") :: d :: _) ->
    Option.map (fun d -> P_scroll d) (parse_scroll d)
  | List (_, Atom (_, "interactive") :: t :: _) ->
    Option.map (fun t -> P_interactive t) (parse_interactive t)
  | List (_, Atom (_, "align-x") :: a :: _) ->
    (match alignment_of_string (Option.value (atom_name a) ~default:"") with
     | Some a -> Some (P_align_x a)
     | None -> None)
  | List (_, Atom (_, "align-y") :: a :: _) ->
    (match alignment_of_string (Option.value (atom_name a) ~default:"") with
     | Some a -> Some (P_align_y a)
     | None -> None)
  | List (_, Atom (_, "border") :: _) ->
    Some (P_border { width = None; color = None; side = None })
  | List (_, Atom (_, "corner") :: r :: _) ->
    Option.map (fun r -> P_corner (Corner_uniform r)) (parse_radius r)
  | List (_, Atom (_, "float") :: _) ->
    Some (P_float Float_simple)
  | _ -> None

(** Skip forms that aren't properties (children, variant, etc.) *)
and is_property_form = function
  | List (_, Atom (_, k) :: _) ->
    not (List.mem k ["children"; "variant"; "slot"; "fill";
                     "use"; "use-layout"; "text"; "image";
                     "placeholder"])
  | _ -> false

and parse_padding args = match args with
  | [s] ->
    Option.map (fun s -> Padding_uniform s) (parse_spacing s)
  | _ ->
    (* Per-side padding: (top N) (right N) (bottom N) (left N) *)
    let get_side name =
      List.find_map (function
        | List (_, [Atom (_, n); v]) when n = name -> parse_spacing v
        | _ -> None
      ) args
    in
    (match get_side "top", get_side "right",
          get_side "bottom", get_side "left" with
     | Some t, Some r, Some b, Some l ->
       Some (Padding_sides { top = t; right = r; bottom = b; left = l })
     | _ -> None)

(** Parse a (variant name ...) form *)
and parse_variant = function
  | List (_, Atom (_, "variant") :: Atom (_, name) :: rest) ->
    let props, children_forms =
      List.partition (function
        | List (_, Atom (_, "children") :: _) -> false
        | _ -> true
      ) rest
    in
    let properties = List.filter_map parse_property
      (List.filter is_property_form props) in
    let children = List.filter_map parse_ui_element children_forms in
    Some { name; properties; children = Some children }
  | _ -> None

(** Parse a (ui name ...) form *)
and parse_ui_element = function
  | List (_, Atom (_, "ui") :: rest) ->
    let name, body =
      match rest with
      | Atom (_, n) :: body -> Some n, body
      | body -> None, body
    in
    let children_forms, other =
      List.partition (function
        | List (_, Atom (_, "children") :: _) -> true
        | _ -> false
      ) body
    in
    let variant_forms, props_body =
      List.partition (function
        | List (_, Atom (_, "variant") :: _) -> true
        | _ -> false
      ) other
    in
    let properties = List.filter_map parse_property
      (List.filter is_property_form props_body) in
    let children =
      List.concat_map (function
        | List (_, Atom (_, "children") :: kids) ->
          List.filter_map parse_ui_element kids
        | _ -> []
      ) children_forms
    in
    let variants = List.filter_map parse_variant variant_forms in
    Some { name; properties; children; variants }
  | _ -> None

(** Parse a (theme ...) form *)
let parse_theme = function
  | List (_, Atom (_, "theme") :: entries) ->
    let entries = List.filter_map (function
      | List (_, [Atom (_, "palette"); Atom (_, name); String (_, Quoted { q_content })]) ->
        Some (Palette_entry (name, q_content))
      | List (_, [Atom (_, "palette"); Atom (_, name); Atom (_, value)]) ->
        Some (Palette_entry (name, value))
      | List (_, [Atom (_, "spacing"); Atom (_, name); Atom (_, n)]) ->
        (try Some (Spacing_entry (name, int_of_string n))
         with Failure _ -> None)
      | List (_, [Atom (_, "font-size"); Atom (_, name); Atom (_, n)]) ->
        (try Some (Font_size_entry (name, int_of_string n))
         with Failure _ -> None)
      | List (_, [Atom (_, "radius"); Atom (_, name); Atom (_, n)]) ->
        (try Some (Radius_entry (name, int_of_string n))
         with Failure _ -> None)
      | _ -> None
    ) entries in
    Some { entries }
  | _ -> None

(** Parse a (component name ...) form *)
and parse_component = function
  | List (_, Atom (_, "component") :: Atom (_, name) :: rest) ->
    let children_forms, props_body =
      List.partition (function
        | List (_, Atom (_, "children") :: _) -> true
        | _ -> false
      ) rest
    in
    let variant_forms, props_body' =
      List.partition (function
        | List (_, Atom (_, "variant") :: _) -> true
        | _ -> false
      ) props_body
    in
    let properties = List.filter_map parse_property
      (List.filter is_property_form props_body') in
    let children =
      List.concat_map (function
        | List (_, Atom (_, "children") :: kids) ->
          List.filter_map parse_ui_element kids
        | _ -> []
      ) children_forms
    in
    let variants = List.filter_map parse_variant variant_forms in
    (* Extract slots from children *)
    let _slots = List.concat_map (function
      | List (_, Atom (_, "children") :: kids) ->
        List.filter_map (function
          | Atom (_, s) when String.length s > 2 && String.sub s 0 2 = ".." ->
            Some Default_slot
          | List (_, [Atom (_, "slot"); Atom (_, name)]) ->
            Some (Named_slot name)
          | _ -> None
        ) kids
      | _ -> []
    ) children_forms in
    Some { name; properties; slots = _slots; children; variants }
  | _ -> None

(** Parse a (layout name ...) form *)
and parse_layout_def = function
  | List (_, Atom (_, "layout") :: Atom (_, name) :: rest) ->
    let root = List.filter_map parse_ui_element rest in
    let root_el = match root with
      | [el] -> el
      | _ -> { name = Some "root"; properties = []; children = root; variants = [] }
    in
    (* TODO: collect named slots from the sexp tree *)
    let slots = [] in
    Some { name; root = root_el; slots }
  | _ -> None

(** Parse a (page name ...) form *)
and parse_page = function
  | List (_, Atom (_, "page") :: Atom (_, name) :: rest) ->
    let layout_name, rest' =
      match rest with
      | List (_, [Atom (_, "use-layout"); Atom (_, ln)]) :: more ->
        ln, more
      | _ -> "", rest
    in
    let fills = List.filter_map (function
      | List (_, Atom (_, "fill") :: Atom (_, slot_name) :: content) ->
        let content_els = List.filter_map parse_ui_element content in
        Some { slot_name; content = content_els }
      | _ -> None
    ) rest' in
    Some { name; layout_name; fills; override_element = [] }
  | _ -> None

(** Parse a (routes ...) form *)
let parse_routes = function
  | List (_, Atom (_, "routes") :: entries) ->
    List.filter_map (function
      | List (_, [Atom (_, "route"); String (_, Quoted { q_content }); Atom (_, page_name)]) ->
        Some { path = q_content; page_name }
      | _ -> None
    ) entries
  | _ -> []

(** Parse a complete UI application from a (ui app-name ...) form *)
let parse_ui_app = function
  | List (_, Atom (_, "ui") :: _ :: body) ->
    let theme = List.filter_map parse_theme body |> fun xs ->
      match xs with [t] -> Some t | _ -> None in
    let components = List.filter_map parse_component body in
    let layouts = List.filter_map parse_layout_def body in
    let pages = List.filter_map parse_page body in
    let routes = List.concat_map (fun r ->
      match parse_routes r with
      | [] -> []
      | routes -> routes
    ) body in
    let elements = List.filter_map parse_ui_element body in
    Some { theme; components; layouts; pages; routes; elements }
  | _ -> None

(** Parse all UI forms from a file's top-level sexps *)
let parse_file (file : Borge_lang.Ast.file) : ui_app option =
  let rec walk = function
    | [] -> None
    | { node; _ } :: rest ->
      (match parse_ui_app node with
       | Some app -> Some app
       | None -> walk rest)
  in
  match walk file.top_level with
  | Some app -> Some app
  | None ->
    (* No top-level (ui ...) form — collect any individual forms *)
    let sexps = List.map (fun swc -> swc.node) file.top_level in
    let theme = List.filter_map parse_theme sexps |> fun xs ->
      match xs with [t] -> Some t | _ -> None in
    let components = List.filter_map parse_component sexps in
    let layouts = List.filter_map parse_layout_def sexps in
    let pages = List.filter_map parse_page sexps in
    let routes = List.concat_map (fun s ->
      match parse_routes s with [] -> [] | r -> r
    ) sexps in
    let elements = List.filter_map parse_ui_element sexps in
    Some { theme; components; layouts; pages; routes; elements }
