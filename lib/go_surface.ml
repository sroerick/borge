(** Extract the public surface of a Go package.

    Go export rule: a name starting with an uppercase letter is exported.
    This is enforced by the compiler — no .mli equivalent needed.

    Analogous to Surface.ml (OCaml). The key difference: Go has no
    separate interface file, so export is determined by capitalization
    alone, making extraction simpler. *)

type go_symbol_kind =
  | Func
  | Method
  | Type
  | Var
  | Const

type go_symbol = {
  name : string;
  kind : go_symbol_kind;
  file : string;
  line : int;
  is_exported : bool;
}

type go_package_surface = {
  path : string;
  package_name : string;
  symbols : go_symbol list;
  source : [ `Go_inferred of string ];
}

(* agent note (|
 *   WHAT: Check if a Go identifier is exported (starts with uppercase).
 *   Includes Unicode letters and underscore handling per Go spec.
 *
 *   WHY: Go's export rule is purely syntactic: uppercase first char
 *   means exported. This is simpler than OCaml's .mli system.
 * |) *)
let is_exported_name name =
  String.length name > 0 &&
  let c = name.[0] in
  c >= 'A' && c <= 'Z'

(* agent note (|
 *   WHAT: Check if a character is valid in a Go identifier.
 *   Letters, digits, underscore. No prime (') unlike OCaml.
 *
 *   WHY: Name extraction needs to know where the identifier ends.
 * |) *)
let is_ident_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
  (c >= '0' && c <= '9') || c = '_'

(* agent note (|
 *   WHAT: Extract the identifier starting at position i in a string.
 *   Returns the identifier and the position after it.
 *
 *   WHY: Shared helper for parsing func/type/var/const names
 *   from Go source lines.
 * |) *)
let extract_ident line i =
  let len = String.length line in
  let start = ref i in
  while !start < len && line.[!start] = ' ' do incr start done;
  if !start >= len then ("", !start)
  else begin
    let fin = ref !start in
    while !fin < len && is_ident_char line.[!fin] do incr fin done;
    (String.sub line !start (!fin - !start), !fin)
  end

(* agent note (|
 *   WHAT: Extract the function name from a Go func declaration.
 *   Handles:
 *     func Foo(...)      → (Foo, Func)
 *     func (t T) Foo(...) → (Foo, Method)
 *     func bar(...)      → (bar, Func)  (unexported)
 *   Returns None for func(), func init(), func main().
 *
 *   WHY: Function extraction is the primary way to discover
 *   a Go package's public surface.
 * |) *)
let extract_func_decl line =
  let trimmed = String.trim line in
  let len = String.length trimmed in
  (* Must start with "func " *)
  if len < 5 then None
  else if String.sub trimmed 0 5 <> "func " then None
  else begin
    let rest = String.sub trimmed 5 (len - 5) in
    let rest_len = String.length rest in
    (* Check for method receiver: func (r Receiver) Name *)
    let name, kind =
      if rest_len > 0 && rest.[0] = '(' then begin
        (* Method with receiver — skip past the closing ) *)
        let depth = ref 1 in
        let i = ref 1 in
        while !i < rest_len && !depth > 0 do
          match rest.[!i] with
          | '(' -> incr depth; incr i
          | ')' -> decr depth; incr i
          | _ -> incr i
        done;
        (* Now extract the method name *)
        let (n, _) = extract_ident rest !i in
        (n, Method)
      end
      else begin
        (* Regular function *)
        let (n, _) = extract_ident rest 0 in
        (n, Func)
      end
    in
    (* Skip init, main — they're not "exported" in the API sense *)
    if name = "" || name = "init" || name = "main" then None
    else if name = "TestMain" then None  (* test helper *)
    else Some (name, kind)
  end

(* agent note (|
 *   WHAT: Check if a line is a Test or Benchmark declaration.
 *   Pattern: func TestXxx(t *testing.T) or func BenchmarkXxx(b *testing.B)
 *   Also handles method form: func (s *Suite) TestXxx()
 *
 *   WHY: Test and benchmark functions are excluded from documentation
 *   coverage enforcement, just like let%test in OCaml.
 * |) *)
let is_test_or_benchmark line =
  let trimmed = String.trim line in
  String.length trimmed >= 9 &&
  String.sub trimmed 0 5 = "func " &&
  (let rest = String.sub trimmed 5 (String.length trimmed - 5) in
   String.length rest >= 4 &&
   (String.sub rest 0 4 = "Test" || String.sub rest 0 9 = "Benchmark"))

(* agent note (|
 *   WHAT: Extract a type name from a Go type declaration.
 *   Handles: type Foo struct, type Foo interface, type Foo ...
 *   Skips: type alias inside functions, type without a name.
 *
 *   WHY: Type declarations are part of a package's public surface.
 * |) *)
let extract_type_decl line =
  let trimmed = String.trim line in
  let len = String.length trimmed in
  if len < 5 then None
  else if String.sub trimmed 0 5 <> "type " then None
  else begin
    let rest = String.sub trimmed 5 (len - 5) |> String.trim in
    let (name, _after) = extract_ident rest 0 in
    if name = "" then None
    (* Skip "type ( ..." — grouped type declarations *)
    else if name.[0] = '(' then None
    else Some name
  end

(* agent note (|
 *   WHAT: Extract a name from a Go var or const declaration.
 *   Handles: var Foo = ..., var ( ... ), const Foo = ...
 *   For grouped declarations (var/const blocks), returns the
 *   first name only — the block will be parsed line by line.
 *
 *   WHY: Var and const declarations at package level are part
 *   of the public surface.
 * |) *)
let extract_var_or_const_decl ~keyword line =
  let trimmed = String.trim line in
  let len = String.length trimmed in
  let kw_len = String.length keyword + 1 in  (* +1 for the space *)
  if len <= kw_len then None
  else if String.sub trimmed 0 kw_len <> (keyword ^ " ") then None
  else begin
    let rest = String.sub trimmed kw_len (len - kw_len) |> String.trim in
    (* Skip grouped declarations: var ( ... *)
    if String.length rest > 0 && rest.[0] = '(' then None
    else begin
      let (name, _) = extract_ident rest 0 in
      if name = "" then None
      else Some name
    end
  end

(* agent note (|
 *   WHAT: Extract all exported symbols from a .go file.
 *   Scans line by line for func, type, var, const declarations.
 *   Skips test/benchmark functions, init, main.
 *
 *   WHY: The main entry point for surface extraction from a single
 *   Go source file. Used by drift detection and reports.
 * |) *)
let extract_file_surface path =
  let symbols = ref [] in
  let line_num = ref 0 in
  let in_block_comment = ref false in
  let in_group_decl = ref false in
  let group_kind = ref Var in  (* var or const block *)

  try
    let ic = open_in path in
    (try
      while true do
        let line = input_line ic in
        incr line_num;
        let trimmed = String.trim line in

        (* Track block comments — skip declarations inside /* */ *)
        if !in_block_comment then begin
          if String.length trimmed >= 2 &&
             String.sub trimmed (String.length trimmed - 2) 2 = "*/" then
            in_block_comment := false;
          ()
        end
        (* Track grouped var/const declarations *)
        else if !in_group_decl then begin
          if String.length trimmed >= 1 && trimmed.[0] = ')' then
            in_group_decl := false
          else begin
            (* Extract names inside the group *)
            let (name, _) = extract_ident trimmed 0 in
            if name <> "" && is_exported_name name then
              symbols := { name; kind = !group_kind; file = path;
                           line = !line_num; is_exported = true } :: !symbols
          end
        end
        else begin
          (* Detect block comment start *)
          if String.length trimmed >= 2 &&
             String.sub trimmed 0 2 = "/*" then begin
            (* Check if it closes on the same line *)
            let content = String.sub trimmed 2 (String.length trimmed - 2) in
            if not (String.contains content '*' && String.contains content '/') then
              in_block_comment := true
          end
          (* Try each declaration type *)
          else if String.starts_with ~prefix:"func " trimmed then begin
            if not (is_test_or_benchmark trimmed) then
              match extract_func_decl trimmed with
              | Some (name, kind) ->
                  let is_exp = is_exported_name name in
                  symbols := { name; kind; file = path;
                               line = !line_num; is_exported = is_exp } :: !symbols
              | None -> ()
          end
          else if String.starts_with ~prefix:"type " trimmed then
            match extract_type_decl trimmed with
            | Some name ->
              let is_exp = is_exported_name name in
              symbols := { name; kind = Type; file = path;
                           line = !line_num; is_exported = is_exp } :: !symbols
            | None -> ()
          else if String.starts_with ~prefix:"var " trimmed then
            (match extract_var_or_const_decl ~keyword:"var" trimmed with
             | Some name ->
               let is_exp = is_exported_name name in
               symbols := { name; kind = Var; file = path;
                            line = !line_num; is_exported = is_exp } :: !symbols
             | None ->
               (* Check for grouped var declaration: var ( *)
               let rest = String.sub trimmed 4 (String.length trimmed - 4) |> String.trim in
               if String.length rest > 0 && rest.[0] = '(' then begin
                 in_group_decl := true;
                 group_kind := Var
               end)
          else if String.starts_with ~prefix:"const " trimmed then
            (match extract_var_or_const_decl ~keyword:"const" trimmed with
             | Some name ->
               let is_exp = is_exported_name name in
               symbols := { name; kind = Const; file = path;
                            line = !line_num; is_exported = is_exp } :: !symbols
             | None ->
               let rest = String.sub trimmed 6 (String.length trimmed - 6) |> String.trim in
               if String.length rest > 0 && rest.[0] = '(' then begin
                 in_group_decl := true;
                 group_kind := Const
               end)
          else ()
        end
      done
    with End_of_file -> ());
    close_in ic;
    List.sort (fun a b ->
      let c = String.compare a.name b.name in
      if c <> 0 then c else compare a.line b.line
    ) !symbols
  with Sys_error _ -> []

(* agent note (|
 *   WHAT: Recursively find .go files in a directory.
 *   Skips vendor/, .git/, node_modules/, and testdata/.
 *
 *   WHY: Surface extraction needs to enumerate source files.
 * |) *)
let find_go_files dir =
  let rec find path =
    try
      let entries = Sys.readdir path in
      Array.fold_left (fun acc entry ->
        if entry = "vendor" || entry = ".git" ||
           entry = "node_modules" || entry = "testdata" then acc
        else if String.length entry > 0 && entry.[0] = '.' then acc
        else
          let full = Filename.concat path entry in
          if Sys.is_directory full then find full @ acc
          else if Filename.check_suffix entry ".go" &&
                  not (String.length entry >= 8 &&
                       String.sub entry (String.length entry - 8) 8 = "_test.go") then
            full :: acc
          else acc
      ) [] entries
    with Sys_error _ -> []
  in
  List.sort String.compare (find dir)

(* agent note (|
 *   WHAT: Extract the full surface of a Go package directory.
 *   Combines all exported symbols from all .go files.
 *
 *   WHY: Used by drift detection and reports to build the
 *   complete picture of a package's public API.
 * |) *)
let extract_package_surface dir_path =
  let go_files = find_go_files dir_path in
  let all_symbols = List.concat_map extract_file_surface go_files in
  let package_name = Filename.basename dir_path in
  { path = dir_path; package_name;
    symbols = all_symbols;
    source = `Go_inferred dir_path }

(* agent note (|
 *   WHAT: Get just the exported names from a package surface.
 *   Unexported symbols are included in the surface record
 *   but filtered out here for the summary view.
 *
 *   WHY: Drift detection and reports typically only care about
 *   the public API, not internal helpers.
 * |) *)
let exported_names surface =
  List.filter_map (fun s ->
    if s.is_exported then Some s.name else None
  ) surface.symbols

(* agent note (|
 *   WHAT: Get just the names (exported and unexported) from a surface.
 *
 *   WHY: Full name listing for verbose reports.
 * |) *)
let all_names surface =
  List.map (fun s -> s.name) surface.symbols
