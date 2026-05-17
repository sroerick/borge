(** Typed AST for borge UI specs.

    The generic sexp parser produces Ast.sexp trees. This module
    defines typed representations of UI-specific forms: elements,
    themes, components, layouts, pages, routes, and variants.

    Extraction from sexp is in Parse.ml. This module is just types. *)

(** Sizing values *)
type sizing =
  | Flex of int           (** N fractional parts (flex: N) *)
  | Grow                  (** fill remaining space *)
  | Full                  (** 100% of parent *)
  | Auto                  (** size to content *)
  | Px of int             (** pixels *)
  | Rem of float          (** rem units *)
  | Pct of int            (** percentage *)
  | Vh of int             (** viewport height percentage *)

(** Color values *)
type color_value =
  | Hex of string         (** "#a8421c" *)
  | Keyword of string     (** blue, red, dark, white, etc. *)
  | Palette of string     (** (palette name) — theme variable reference *)
  | With_alpha of color_value * float  (** color with opacity *)

(** Spacing values *)
type spacing_value =
  | Spacing_px of int
  | Spacing_rem of float
  | Spacing_var of string  (** (spacing name) — theme variable reference *)

(** Font size values *)
type font_size_value =
  | Font_px of int
  | Font_rem of float
  | Font_var of string     (** (font-size name) — theme variable reference *)

(** Corner radius values *)
type radius_value =
  | Radius_px of int
  | Radius_var of string   (** (radius name) — theme variable reference *)

(** Layout direction *)
type layout_direction =
  | Horizontal
  | Vertical

(** Alignment *)
type alignment =
  | Align_start | Align_center | Align_end
  [@@deriving eq]

let alignment_of_string = function
  | "left" | "top" -> Some Align_start
  | "center" -> Some Align_center
  | "right" | "bottom" -> Some Align_end
  | _ -> None

(** Scroll direction *)
type scroll_direction =
  | Scroll_vertical
  | Scroll_horizontal
  | Scroll_both

(** Interactive capability *)
type interactive_type =
  | Interactive_click
  | Interactive_type
  | Interactive_hover

(** Text wrapping *)
type text_wrap =
  | Wrap_words
  | Wrap_none
  | Wrap_newlines

(** Float attachment *)
type float_spec =
  | Float_simple
  | Float_attach_parent
  | Float_offset of (string * int) list  (** offset pairs like (top 8) *)

(** Per-side padding *)
type padding =
  | Padding_uniform of spacing_value
  | Padding_sides of {
      top : spacing_value;
      right : spacing_value;
      bottom : spacing_value;
      left : spacing_value;
    }

(** Border specification *)
type border_spec = {
  width : int option;
  color : color_value option;
  side : string option;  (** None = all sides, Some "bottom" = specific side *)
}

(** Corner radius *)
type corner_radius =
  | Corner_uniform of radius_value
  | Corner_sides of {
      top_left : radius_value;
      top_right : radius_value;
      bottom_left : radius_value;
      bottom_right : radius_value;
    }

(** Parsed theme variable entry *)
type theme_entry =
  | Palette_entry of string * string      (** name, hex value *)
  | Spacing_entry of string * int         (** name, px value *)
  | Font_size_entry of string * int       (** name, px value *)
  | Radius_entry of string * int         (** name, px value *)

(** A theme definition *)
type theme = {
  entries : theme_entry list;
}

(** A named variant — overrides base properties *)
type variant = {
  name : string;
  properties : ui_property list;
  children : ui_element list option;  (** None = inherit base children *)
}
and ui_property =
  | P_layout of layout_direction
  | P_width of sizing
  | P_height of sizing
  | P_padding of padding
  | P_gap of spacing_value
  | P_bg of color_value
  | P_color of color_value
  | P_border of border_spec
  | P_corner of corner_radius
  | P_scroll of scroll_direction
  | P_float of float_spec
  | P_interactive of interactive_type
  | P_align_x of alignment
  | P_align_y of alignment

(** A UI element — the core building block *)
and ui_element = {
  name : string option;        (** None for anonymous elements *)
  properties : ui_property list;
  children : ui_element list;  (** empty if no (children ...) *)
  variants : variant list;
}

(** Slot kinds inside components and layouts *)
type slot =
  | Default_slot              (** ..content *)
  | Named_slot of string      (** (slot name) *)

(** A component definition *)
type component = {
  name : string;
  properties : ui_property list;
  slots : slot list;
  children : ui_element list;
  variants : variant list;
}

(** A layout definition *)
type layout_def = {
  name : string;
  root : ui_element;
  slots : slot list;
}

(** A slot fill — page provides content for a layout slot *)
type slot_fill = {
  slot_name : string;
  content : ui_element list;
}

(** A page definition *)
type page = {
  name : string;
  layout_name : string;
  fills : slot_fill list;
  override_element : (string * ui_property list) list;  (** element name, overridden props *)
}

(** A route mapping *)
type route = {
  path : string;
  page_name : string;
}

(** A complete UI application spec *)
type ui_app = {
  theme : theme option;
  components : component list;
  layouts : layout_def list;
  pages : page list;
  routes : route list;
  elements : ui_element list;  (** top-level elements not in a component/layout/page *)
}
