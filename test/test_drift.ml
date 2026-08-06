(** Unit tests for the drift-check machinery (existence, not evidence).

    Covers: (implements ...) path grouping, the verify-apparatus
    looks_like_path heuristic, orphan kind/conventional predicates, the
    quote-stripping helper, and end-to-end runs of the three new checks
    against temp dirs (one of them a throwaway git repo). *)

open Borge_lib

let parse input =
  Borge_lang.Parse.parse_file input

(** Write a file (creating parent dirs) for e2e tests. *)
let write_file path content =
  let rec mkdir_p d =
    if not (Sys.file_exists d) then begin
      mkdir_p (Filename.dirname d);
      Unix.mkdir d 0o755
    end
  in
  let dir = Filename.dirname path in
  if dir <> "." then mkdir_p dir;
  let oc = open_out path in
  output_string oc content;
  close_out oc

let mk_temp_dir () =
  Filename.temp_dir ~temp_dir:(Filename.get_temp_dir_name ()) "drift" "test"

let rm_rf d =
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote d)))

(* --- Pure: extract_implements path grouping --- *)

let test_implements_grouping () =
  (* component-style: (implements lib lock lock.ml) -> ONE joined path *)
  let file = parse "(project t (section s (status implemented)
    (implements lib lock lock.ml)))" in
  let mappings = Spec.extract_section_mappings file in
  let m = match mappings with [m] -> m | _ -> Alcotest.fail "expected 1 section" in
  Alcotest.(check int) __LOC__ 1 (List.length m.implements);
  (match m.implements with
   | [p] -> Alcotest.(check string) __LOC__ "lib/lock/lock.ml" p
   | _ -> Alcotest.fail "expected one joined path")

let test_implements_grouping_multi () =
  (* complete-path style: two files in one stanza *)
  let file = parse "(project t (section s (status implemented)
    (implements image/lib/a.ml image/lib/b.ml)))" in
  let mappings = Spec.extract_section_mappings file in
  let m = match mappings with [m] -> m | _ -> Alcotest.fail "expected 1 section" in
  Alcotest.(check int) __LOC__ 2 (List.length m.implements);
  Alcotest.(check bool) __LOC__ true (List.mem "image/lib/a.ml" m.implements);
  Alcotest.(check bool) __LOC__ true (List.mem "image/lib/b.ml" m.implements)

let test_implements_grouping_mixed () =
  (* two forms: component-style + complete-path *)
  let file = parse "(project t (section s (status implemented)
    (implements lib lock lock.ml)
    (implements image/lib/b.ml)))" in
  let mappings = Spec.extract_section_mappings file in
  let m = match mappings with [m] -> m | _ -> Alcotest.fail "expected 1 section" in
  let paths = List.sort String.compare m.implements in
  Alcotest.(check string) __LOC__ "image/lib/b.ml" (List.hd paths);
  Alcotest.(check string) __LOC__ "lib/lock/lock.ml" (List.nth paths 1)

(* --- Pure: looks_like_path (rejects prose descriptions) --- *)

let test_looks_like_path () =
  let cases = [
    "scripts/foo.sh", true;
    "test/spec_in_code.ml", true;
    "lib/check/drift.ml", true;
    "image serves /health and a CRUD roundtrip", false;  (* spaces *)
    "dune runtest", false;                              (* spaces, no ext/; *)
    "GET /login", false;                                (* spaces *)
    "README", false;                                    (* no ext, no /; *)
    "build", false;                                      (* bare command *)
  ] in
  List.iter (fun (arg, expected) ->
    Alcotest.(check bool) __LOC__ expected (Drift.looks_like_path arg)
  ) cases

(* --- Pure: kind_of_path / is_conventional_file / strip_wrappers --- *)

let test_kind_of_path () =
  let cases = [
    "scripts/dev.sh", "unspecified-script";
    "docs/foo.md", "unspecified-doc";
    "libs/notes/lib.pp", "unspecified-lib";
    "migrations/001_x.sql", "unspecified-migration";
    "other.txt", "unspecified-file";
  ] in
  List.iter (fun (p, k) ->
    Alcotest.(check string) __LOC__ k (Drift.kind_of_path p)
  ) cases

let test_is_conventional_file () =
  let yes name = Alcotest.(check bool) __LOC__ true (Drift.is_conventional_file name) in
  let no name = Alcotest.(check bool) __LOC__ false (Drift.is_conventional_file name) in
  yes ".gitignore"; yes ".gitkeep"; yes "README.md"; yes "LICENSE";
  yes "AGENTS.md"; yes "TODO.md"; yes "dune"; yes "dune-project";
  yes "borge.opam"; yes "foo.borg.meta"; yes "bar.borg";
  no "scripts/dev.sh"; no "docs/notes.md"; no "libs/notes/lib.pp";
  no "regular.ml"; no "migrations/001.sql"

let test_strip_wrappers () =
  Alcotest.(check string) __LOC__ "path/to/x.ml"
    (Drift.strip_wrappers "\"path/to/x.ml\"");
  Alcotest.(check string) __LOC__ "path/to/y.ml"
    (Drift.strip_wrappers "(|path/to/y.ml|)");
  Alcotest.(check string) __LOC__ "bare_atom"
    (Drift.strip_wrappers "bare_atom")

(* --- e2e: check_implements_exist (filesystem) --- *)

let test_check_implements_exist () =
  let d = mk_temp_dir () in
  let borg = "(project p (section s (status implemented)
    (implements lib/exists.ml)
    (implements lib/missing.ml)))" in
  write_file (Filename.concat d "spec.borg") borg;
  write_file (Filename.concat d "lib/exists.ml") "(* exists *)";
  let file = parse borg in
  let drift = Drift.check_implements_exist d (Filename.concat d "spec.borg") file in
  rm_rf d;
  Alcotest.(check int) __LOC__ 1 (List.length drift);
  (match drift with
   | [item] ->
       Alcotest.(check bool) __LOC__ true
         (try ignore (Str.search_forward (Str.regexp_string "missing.ml") item.description 0); true
          with Not_found -> false)
   | _ -> Alcotest.fail "expected exactly 1 implements drift")

(* --- e2e: check_verify_apparatus (filesystem) --- *)

let test_check_verify_apparatus_present () =
  let d = mk_temp_dir () in
  let borg = "(project p (section s (status implemented)
    (verify (test \"scripts/present.sh\") (smoke \"scripts/missing.sh\"))))" in
  write_file (Filename.concat d "spec.borg") borg;
  write_file (Filename.concat d "scripts/present.sh") "#!/bin/sh";
  let file = parse borg in
  let drift = Drift.check_verify_apparatus d (Filename.concat d "spec.borg") file in
  rm_rf d;
  Alcotest.(check int) __LOC__ 1 (List.length drift);
  (match drift with
   | [item] ->
       Alcotest.(check bool) __LOC__ true
         (try ignore (Str.search_forward (Str.regexp_string "missing.sh") item.description 0); true
          with Not_found -> false)
   | _ -> Alcotest.fail "expected exactly 1 verify drift")

let test_check_verify_apparatus_prose_rejected () =
  (* verify args that are prose descriptions (with spaces) must NOT be
     treated as paths. *)
  let d = mk_temp_dir () in
  let borg = "(project p (section s (status implemented)
    (verify (smoke \"image serves /health and a CRUD roundtrip\")
            (test \"dune runtest\"))))" in
  write_file (Filename.concat d "spec.borg") borg;
  let file = parse borg in
  let drift = Drift.check_verify_apparatus d (Filename.concat d "spec.borg") file in
  rm_rf d;
  Alcotest.(check int) __LOC__ 0 (List.length drift)

let test_check_verify_apparatus_planned_skipped () =
  (* planned sections may reference not-yet-existing artifacts -> not drift. *)
  let d = mk_temp_dir () in
  let borg = "(project p (section s (status planned)
    (verify (test \"scripts/never.sh\"))))" in
  write_file (Filename.concat d "spec.borg") borg;
  let file = parse borg in
  let drift = Drift.check_verify_apparatus d (Filename.concat d "spec.borg") file in
  rm_rf d;
  Alcotest.(check int) __LOC__ 0 (List.length drift)

(* --- e2e: check_orphan_files (temp git repo) --- *)

let test_check_orphan_files () =
  let d = mk_temp_dir () in
  let borg = "(project p
    (section notes (status implemented) (implements libs/notes/lib.pp))
    (untracked \"scripts/dev.sh\" \"local dev orchestration\"))" in
  write_file (Filename.concat d "spec.borg") borg;
  write_file (Filename.concat d "libs/notes/lib.pp") "()";       (* declared -> not orphan *)
  write_file (Filename.concat d "scripts/dev.sh") "#!/bin/sh";  (* excused -> not orphan *)
  write_file (Filename.concat d "scripts/other.sh") "#!/bin/sh"; (* orphan (script) *)
  write_file (Filename.concat d "docs/readme.md") "# readme";    (* orphan (doc) *)
  write_file (Filename.concat d "docs/README.md") "# top";       (* conventional -> not orphan *)
  write_file (Filename.concat d "scripts/.gitkeep") "";           (* dotfile -> not orphan *)
  (* track them so git ls-files returns them *)
  ignore (Sys.command
    (Printf.sprintf "git init -q %s && git -C %s add -A" (Filename.quote d) (Filename.quote d)));
  let orphans = Drift.check_orphan_files d in
  let paths = List.map (fun (o : Drift.code_drift_item) -> o.path) orphans |> List.sort String.compare in
  rm_rf d;
  Alcotest.(check int) __LOC__ 2 (List.length orphans);
  Alcotest.(check bool) __LOC__ true (List.mem "docs/readme.md" paths);
  Alcotest.(check bool) __LOC__ true (List.mem "scripts/other.sh" paths);
  Alcotest.(check bool) __LOC__ false (List.mem "libs/notes/lib.pp" paths);
  Alcotest.(check bool) __LOC__ false (List.mem "scripts/dev.sh" paths)

let () =
  Alcotest.run "drift tests" [
    "implements-grouping", [
      Alcotest.test_case "component-style" `Quick test_implements_grouping;
      Alcotest.test_case "multi-complete-paths" `Quick test_implements_grouping_multi;
      Alcotest.test_case "mixed-forms" `Quick test_implements_grouping_mixed;
    ];
    "heuristics", [
      Alcotest.test_case "looks_like_path" `Quick test_looks_like_path;
      Alcotest.test_case "kind_of_path" `Quick test_kind_of_path;
      Alcotest.test_case "is_conventional_file" `Quick test_is_conventional_file;
      Alcotest.test_case "strip_wrappers" `Quick test_strip_wrappers;
    ];
    "e2e-checks", [
      Alcotest.test_case "implements_exist" `Quick test_check_implements_exist;
      Alcotest.test_case "verify_apparatus_present" `Quick test_check_verify_apparatus_present;
      Alcotest.test_case "verify_apparatus_prose_rejected" `Quick test_check_verify_apparatus_prose_rejected;
      Alcotest.test_case "verify_apparatus_planned_skipped" `Quick test_check_verify_apparatus_planned_skipped;
      Alcotest.test_case "orphan_files" `Quick test_check_orphan_files;
    ];
  ]
