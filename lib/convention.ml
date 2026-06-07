(** Convention dispatch: select the right module set based on the
    project's declared convention.

    A convention declares structural rules for a project — directory
    layout, build system, file extensions. Borge uses the convention
    to dispatch to the appropriate parsing, surface extraction, and
    doc detection modules.

    Convention resolution reads the root .borg file and extracts the
    (convention NAME) form. If none is found, defaults to ocaml-dune
    for backward compatibility.

    Comment parsing is SHARED across conventions. Borge comments in Go
    source nest the standard (* *) grammar inside Go's /* */ block
    comment delimiters: /* (* author note (|...|) *) */. This means
    Doc_detect and Borg_comment work unchanged for both languages. *)

type t =
  | Ocaml_dune
  | Go_standard

(* agent note (|
 *   WHAT: Convert a convention type to its string name.
 *
 *   WHY: Used in prompts and reports to identify the active convention.
 * |) *)
let name = function
  | Ocaml_dune -> "ocaml-dune"
  | Go_standard -> "go-standard"

(* agent note (|
 *   WHAT: Source file extension for this convention.
 *
 *   WHY: File discovery needs to know which extension to scan.
 * |) *)
let source_extension = function
  | Ocaml_dune -> ".ml"
  | Go_standard -> ".go"

(* agent note (|
 *   WHAT: Interface file extension for this convention.
 *   Go has no separate interface files — exports are determined
 *   by capitalization.
 *
 *   WHY: Surface extraction checks for .mli in OCaml but not Go.
 * |) *)
let interface_extension = function
  | Ocaml_dune -> Some ".mli"
  | Go_standard -> None

(* agent note (|
 *   WHAT: Directories to exclude when scanning for source files.
 *
 *   WHY: Each convention has its own build artifact directories
 *   that should not be scanned.
 * |) *)
let excluded_dirs = function
  | Ocaml_dune -> ["_build"; ".git"]
  | Go_standard -> ["vendor"; ".git"; "node_modules"]

(* agent note (|
 *   WHAT: Resolve a convention from its string name.
 *   Returns None for unrecognized names.
 *
 *   WHY: The (convention NAME) form in .borg files stores the
 *   convention as a plain atom. This function maps it to the
 *   typed variant used for dispatch throughout borge.
 * |) *)
let of_string = function
  | "ocaml-dune" -> Some Ocaml_dune
  | "go-standard" -> Some Go_standard
  | _ -> None

(* agent note (|
 *   WHAT: Extract the convention from a root .borg file.
 *   Looks for (convention NAME) as a direct child of the
 *   (project ...) form. Returns Ocaml_dune as default if
 *   no convention is declared.
 *
 *   WHY: Every borge command needs to know the convention to
 *   dispatch correctly. This reads it from the project root spec.
 * |) *)
let resolve_from_file path =
  try
    let input = File_utils.read_file path in
    let file = Borge_lang.Parse.parse_file input in
    let rec find_convention = function
      | [] -> Ocaml_dune  (* default *)
      | { Borge_lang.Ast.node; _ } :: rest ->
        (match node with
         | Borge_lang.Ast.List (_, children) ->
           let rec scan = function
             | [] -> find_convention rest
             | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "convention") :: Borge_lang.Ast.Atom (_, name) :: _) :: _ ->
               (match of_string name with
                | Some t -> t
                | None ->
                  Printf.eprintf "Warning: unknown convention '%s', defaulting to ocaml-dune\n" name;
                  Ocaml_dune)
             | _ :: scan_rest -> scan scan_rest
           in
           scan children
         | _ -> find_convention rest)
    in
    find_convention file.Borge_lang.Ast.top_level
  with _ ->
    Ocaml_dune  (* fallback on error *)

(* agent note (|
 *   WHAT: Resolve the convention from a project directory.
 *   Finds root .borg files and reads the convention from the
 *   first one. Falls back to Ocaml_dune if no roots are found.
 *
 *   WHY: Most borge commands operate on a directory, not a specific
 *   file. This is the primary entry point for convention resolution.
 * |) *)
let resolve dir =
  let roots = Project.find_roots dir in
  match roots with
  | [] -> Ocaml_dune
  | root :: _ -> resolve_from_file root
