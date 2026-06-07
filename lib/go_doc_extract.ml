(** Extract Go binding information for documentation coverage.

    Analogous to Doc_extract (OCaml). Gathers all top-level declarations
    from Go source files, classifying them as exported, test, or internal.

    Go-specific classification:
    - func TestXxx(t *testing.T) → test binding (excluded from doc enforcement)
    - func BenchmarkXxx(b *testing.B) → test binding
    - func (s *suite) TestXxx() → test method
    - func lowercaseName → unexported (internal)
    - func UppercaseName → exported (needs doc)
    - type/var/const follow the same capitalized = exported rule *)

type go_binding_info = {
  name : string;
  line : int;
  is_exported : bool;
  is_test : bool;
  is_internal : bool;
}

(* agent note (|
 *   WHAT: Extract all top-level Go declarations from a .go file.
 *   Returns binding info with name, line, and classification
 *   (exported, test, internal).
 *
 *   WHY: Documentation coverage analysis needs to discover which
 *   declarations exist and whether they're exported (need docs)
 *   or internal (excluded from enforcement).
 * |) *)
let extract_go_bindings path =
  let bindings = ref [] in
  let line_num = ref 0 in
  let in_block_comment = ref false in
  let in_group_decl = ref false in

  try
    (* exempt: open_in input_line *)
    let ic = open_in path in
    (try
      while true do
        let line = input_line ic in
        incr line_num;
        let trimmed = String.trim line in

        (* Skip block comments *)
        if !in_block_comment then begin
          if String.length trimmed >= 2 &&
             String.sub trimmed (String.length trimmed - 2) 2 = "*/" then
            in_block_comment := false;
          ()
        end
        else if !in_group_decl then begin
          if String.length trimmed >= 1 && trimmed.[0] = ')' then
            in_group_decl := false
          else begin
            (* Parse names inside var/const group *)
            let name, _ = Go_surface.extract_ident trimmed 0 in
            if name <> "" then begin
              let is_exp = Go_surface.is_exported_name name in
              bindings := { name; line = !line_num;
                            is_exported = is_exp;
                            is_test = false;
                            is_internal = not is_exp } :: !bindings
            end
          end
        end
        else begin
          (* Block comment start *)
          if String.length trimmed >= 2 &&
             String.sub trimmed 0 2 = "/*" then begin
            let content = String.sub trimmed 2 (String.length trimmed - 2) in
            if not (String.contains content '*' && String.contains content '/') then
              in_block_comment := true
          end
          (* Func declarations *)
          else if String.starts_with ~prefix:"func " trimmed then begin
            if Go_surface.is_test_or_benchmark trimmed then begin
              let name, _kind = match Go_surface.extract_func_decl trimmed with
                | Some (n, k) -> (n, k)
                | None -> ("unknown", Go_surface.Func)
              in
              bindings := { name; line = !line_num;
                            is_exported = false; is_test = true;
                            is_internal = true } :: !bindings
            end
            else
              match Go_surface.extract_func_decl trimmed with
              | Some (name, _kind) ->
                let is_exp = Go_surface.is_exported_name name in
                bindings := { name; line = !line_num;
                              is_exported = is_exp; is_test = false;
                              is_internal = not is_exp } :: !bindings
              | None -> ()
          end
          (* Type declarations *)
          else if String.starts_with ~prefix:"type " trimmed then
            match Go_surface.extract_type_decl trimmed with
            | Some name ->
              let is_exp = Go_surface.is_exported_name name in
              bindings := { name; line = !line_num;
                            is_exported = is_exp; is_test = false;
                            is_internal = not is_exp } :: !bindings
            | None -> ()
          (* Var declarations *)
          else if String.starts_with ~prefix:"var " trimmed then
            (match Go_surface.extract_var_or_const_decl ~keyword:"var" trimmed with
             | Some name ->
               let is_exp = Go_surface.is_exported_name name in
               bindings := { name; line = !line_num;
                             is_exported = is_exp; is_test = false;
                             is_internal = not is_exp } :: !bindings
             | None ->
               let rest = String.sub trimmed 4 (String.length trimmed - 4) |> String.trim in
               if String.length rest > 0 && rest.[0] = '(' then
                 in_group_decl := true)
          (* Const declarations *)
          else if String.starts_with ~prefix:"const " trimmed then
            (match Go_surface.extract_var_or_const_decl ~keyword:"const" trimmed with
             | Some name ->
               let is_exp = Go_surface.is_exported_name name in
               bindings := { name; line = !line_num;
                             is_exported = is_exp; is_test = false;
                             is_internal = not is_exp } :: !bindings
             | None ->
               let rest = String.sub trimmed 6 (String.length trimmed - 6) |> String.trim in
               if String.length rest > 0 && rest.[0] = '(' then
                 in_group_decl := true)
          else ()
        end
      done
    with End_of_file -> ());
    close_in ic;
    List.rev !bindings
  with Sys_error _ -> []

(* agent note (|
 *   WHAT: Recursively find .go files under a directory.
 *   Skips vendor/, .git/, testdata/, and _test.go files
 *   (test files are excluded from doc coverage enforcement).
 *
 *   WHY: Coverage analysis scans non-test source files.
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
 *   WHAT: Extract bindings from all .go files in a project,
 *   returning (path, go_binding_info) pairs.
 *
 *   WHY: Project-wide coverage analysis needs all bindings
 *   paired with their source files.
 * |) *)
let extract_project_go_bindings dir =
  let go_files = find_go_files dir in
  List.concat_map (fun path ->
    let bindings = extract_go_bindings path in
    List.map (fun b -> (path, b)) bindings
  ) go_files

(* agent note (|
 *   WHAT: Count statistics for a list of bindings: total,
 *   exported, tests, and internal counts.
 *
 *   WHY: Quick summary numbers for coverage reports.
 * |) *)
let count_stats bindings =
  let total = List.length bindings in
  let exported = List.filter (fun b -> b.is_exported) bindings |> List.length in
  let tests = List.filter (fun b -> b.is_test) bindings |> List.length in
  let internal = List.filter (fun b -> b.is_internal) bindings |> List.length in
  (total, exported, tests, internal)
