(** Parse Go project structure: go.mod and directory layout.

    Analogous to Dune_parse. Discovers Go project structure by parsing
    go.mod and walking the directory tree. The directory IS the package
    in Go — no separate build file needed.

    Excluded directories: vendor/, .git/, node_modules/ *)

type go_module = {
  module_path : string;  (* e.g. "github.com/foo/bar" *)
  go_version : string;   (* e.g. "1.22" *)
}

type go_package_kind =
  | Cmd           (* cmd/FOO — executable (main package) *)
  | Internal      (* internal/FOO — private package *)
  | Pkg           (* pkg/FOO — public library package *)
  | Standard      (* other dir with .go files *)

type go_package = {
  dir_path : string;
  package_name : string;
  kind : go_package_kind;
  go_files : string list;      (* .go files in this package *)
  test_files : string list;   (* *_test.go files in this package *)
}

type go_project = {
  go_mod : go_module option;
  packages : go_package list;
}

(* agent note (|
 *   WHAT: Parse go.mod to extract the module path and Go version.
 *   Handles the standard format:
 *     module github.com/foo/bar
 *     go 1.22
 *   Ignores require/replace/exclude stanzas.
 *
 *   WHY: The module path is the root import path for the project.
 *   Go surface extraction and import resolution need it.
 * |) *)
let parse_go_mod path =
  try
    let ic = open_in path in
    let module_path = ref None in
    let go_version = ref None in
    (try
      while true do
        let line = input_line ic |> String.trim in
        if String.length line >= 7 && String.sub line 0 7 = "module " then begin
          let rest = String.sub line 7 (String.length line - 7) |> String.trim in
          module_path := Some rest
        end
        else if String.length line >= 3 && String.sub line 0 3 = "go " then begin
          let rest = String.sub line 3 (String.length line - 3) |> String.trim in
          go_version := Some rest
        end
      done
    with End_of_file -> ());
    close_in ic;
    match !module_path with
    | Some mp ->
        Some { module_path = mp;
               go_version = (match !go_version with Some v -> v | None -> "") }
    | None -> None
  with Sys_error _ -> None

(* agent note (|
 *   WHAT: Find the go.mod file by walking up from a directory.
 *
 *   WHY: Go tools find go.mod by walking up the directory tree.
 *   We follow the same convention so borge works from any subdir.
 * |) *)
let find_go_mod dir =
  let rec walk d =
    let candidate = Filename.concat d "go.mod" in
    if Sys.file_exists candidate then Some candidate
    else
      let parent = Filename.dirname d in
      if parent = d then None  (* reached root *)
      else walk parent
  in
  walk dir

(* agent note (|
 *   WHAT: Check if a directory name should be skipped during
 *   Go package discovery. Skips hidden dirs, vendor, _*, etc.
 *
 *   WHY: Go package discovery must skip directories that don't
 *   represent importable packages — vendor, hidden dirs, testdata.
 * |) *)
let should_skip_dir name =
  name = "vendor" ||
  name = ".git" ||
  name = "node_modules" ||
  name = "testdata" ||
  (String.length name > 0 && name.[0] = '.')

(* agent note (|
 *   WHAT: Classify a package directory by its position in the
 *   Go project layout.
 *
 *   WHY: go-standard convention assigns meaning to directory
 *   positions: cmd/ = executables, internal/ = private, etc.
 *   Drift detection uses this classification.
 * |) *)
let classify_package dir_path =
  let base = Filename.basename dir_path in
  let parent = Filename.dirname dir_path in
  let parent_base = Filename.basename parent in
  if parent_base = "cmd" then Cmd
  else if parent_base = "internal" || String.length base >= 9 && String.sub base 0 9 = "internal" then Internal
  else if parent_base = "pkg" then Pkg
  else Standard

(* agent note (|
 *   WHAT: Recursively discover Go packages in a directory.
 *   A directory is a package if it contains .go files.
 *   Returns go_package records with file listings.
 *
 *   WHY: Go's package model is directory-based. Walking the tree
 *   is how we discover what packages exist for drift detection.
 * |) *)
let discover_packages dir =
  let packages = ref [] in
  let rec walk path =
    try
      let entries = Sys.readdir path in
      let go_files = ref [] in
      let test_files = ref [] in
      let subdirs = ref [] in
      Array.iter (fun entry ->
        if should_skip_dir entry then ()
        else
          let full = Filename.concat path entry in
          if Sys.is_directory full then
            subdirs := full :: !subdirs
          else if Filename.check_suffix entry ".go" then
            if String.length entry >= 8 &&
               String.sub entry (String.length entry - 8) 8 = "_test.go" then
              test_files := entry :: !test_files
            else
              go_files := entry :: !go_files
          else ()
      ) entries;
      (* This directory is a package if it has .go files *)
      if !go_files <> [] || !test_files <> [] then begin
        let package_name = Filename.basename path in
        let kind = classify_package path in
        packages := { dir_path = path; package_name; kind;
                      go_files = List.sort String.compare !go_files;
                      test_files = List.sort String.compare !test_files }
                   :: !packages
      end;
      List.iter walk (List.sort String.compare !subdirs)
    with Sys_error _ -> ()
  in
  walk dir;
  List.sort (fun a b -> String.compare a.dir_path b.dir_path) !packages

(* agent note (|
 *   WHAT: Extract executable names from cmd/ subdirectories.
 *   Each subdirectory of cmd/ is a binary entrypoint.
 *
 *   WHY: Analogous to dune's (executables ...) stanza discovery.
 *   Drift detection and reports need to know what binaries exist.
 * |) *)
let discover_executables dir =
  let cmd_dir = Filename.concat dir "cmd" in
  if not (Sys.is_directory cmd_dir) then []
  else
    try
      Sys.readdir cmd_dir
      |> Array.to_list
      |> List.filter (fun name ->
        not (should_skip_dir name) &&
        Sys.is_directory (Filename.concat cmd_dir name))
      |> List.sort String.compare
    with Sys_error _ -> []

(* agent note (|
 *   WHAT: Full project scan combining go.mod parsing and directory
 *   walking. Returns a go_project with all discovered structure.
 *
 *   WHY: The primary entry point for Go project discovery.
 *   Used by drift detection and reports.
 * |) *)
let parse_all dir =
  let go_mod = match find_go_mod dir with
    | Some path -> parse_go_mod path
    | None -> None
  in
  let packages = discover_packages dir in
  { go_mod; packages }

(* agent note (|
 *   WHAT: Get all package names in the project.
 *
 *   WHY: Quick summary for reports and drift detection.
 * |) *)
let all_package_names project =
  List.map (fun p -> p.package_name) project.packages

(* agent note (|
 *   WHAT: Get all packages that are libraries (not cmd executables).
 *
 *   WHY: Drift detection cross-references library packages against
 *   .borg spec sections. Executables are thin wrappers by convention.
 * |) *)
let library_packages project =
  List.filter (fun p -> p.kind <> Cmd) project.packages

(* agent note (|
 *   WHAT: Get all executable packages (cmd/ directories).
 *
 *   WHY: Analogous to Dune_parse.all_executables.
 * |) *)
let executable_packages project =
  List.filter (fun p -> p.kind = Cmd) project.packages
