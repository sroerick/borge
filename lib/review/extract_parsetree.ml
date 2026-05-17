(* Extract OCaml parsetree from .ml/.mli files
 *
 * roerick note (|
 *   Placeholder for compiler-libs integration.
 *   Currently provides stub types and functions that will be
 *   replaced with actual compiler-libs Parsetree extraction.
 *   
 *   The full implementation will use ocaml-compiler-libs to parse
 *   .ml/.mli files into the Parsetree module for accurate AST
 *   extraction.
 * |) *)

(* Stub types that mirror Parsetree concepts *)
type structure = unit  (* Placeholder *)
type signature = unit  (* Placeholder *)
type structure_item = unit  (* Placeholder *)
type location = {
  pos_fname : string;
  pos_lnum : int;
  pos_cnum : int;
  pos_bol : int;
}

(* Stub parse function *)
let parse_file path =
  (* For now, just return empty structure - extraction happens via regexes *)
  ignore path;
  Ok ()

(* Stub interface parse *)
let parse_interface path =
  ignore path;
  Ok ()

(* Get location from structure item - stub *)
let location_of_structure_item _item =
  { pos_fname = ""; pos_lnum = 0; pos_cnum = 0; pos_bol = 0 }

(* Check if item is value binding - stub *)
let is_value_binding _item =
  false

(* Extract docstring - stub, actual extraction is in extract_docs.ml *)
let extract_docstring _attrs =
  None

(* Check for exempt marker - stub *)
let has_exempt_marker _attrs =
  false

(* Get line from location *)
let line_of_loc loc =
  loc.pos_lnum

(* Get column from location *)
let col_of_loc loc =
  loc.pos_cnum - loc.pos_bol
