let parse_ok input =
  try
    let _ = Borge_lang.Parse.parse input in
    true
  with Borge_lang.Error.Parse_error _ -> false

let test_simple_project () =
  let input = "(project hello\n  (doc \"A project\")\n  (status planned))" in
  Alcotest.(check bool) __LOC__ true (parse_ok input)

let test_annotated_comment () =
  let input = "(* roerick note (|Hello|) *)\n\n(project test\n  (doc \"Test\")\n  (status planned))" in
  Alcotest.(check bool) __LOC__ true (parse_ok input)

let test_verbatim_string () =
  let input = "(project test\n  (doc (|A multi-line\ndoc string|))\n  (status planned))" in
  Alcotest.(check bool) __LOC__ true (parse_ok input)

let test_ask_response () =
  let input = "(project test\n  (doc \"Test\")\n  (* roerick ask (|Hello?|) *)\n  (* agent response (||) *)\n  (status planned))" in
  Alcotest.(check bool) __LOC__ true (parse_ok input)

let test_design () =
  let input = "(* roerick design (|Build it|) *)\n\n(project test\n  (doc \"Test\")\n  (status planned))" in
  Alcotest.(check bool) __LOC__ true (parse_ok input)

let test_plain_comments () =
  let input = "; comment\n\n(project test\n  (doc \"Test\")\n  (status planned))" in
  Alcotest.(check bool) __LOC__ true (parse_ok input)

let test_bad_unclosed_paren () =
  let input = "(project test" in
  Alcotest.(check bool) __LOC__ false (parse_ok input)

let test_project_name () =
  let input = "(project my-project\n  (doc \"Test\")\n  (status planned))" in
  let file = Borge_lang.Parse.parse input in
  let name = Borge_lib.Spec.project_name file in
  Alcotest.(check (option string)) __LOC__ (Some "my-project") name

let test_inline_targets () =
  let input = "(project test\n  (section foo\n   (doc \"Foo\")\n   (status planned)\n   (inline bar.borg)\n   (inline baz.borg)))" in
  let file = Borge_lang.Parse.parse input in
  let targets = Borge_lib.Spec.inline_targets file in
  Alcotest.(check (list string)) __LOC__ ["bar.borg"; "baz.borg"] targets

let test_inline_targets_empty () =
  let input = "(project test\n  (doc \"No inlines\")\n  (status planned))" in
  let file = Borge_lang.Parse.parse input in
  let targets = Borge_lib.Spec.inline_targets file in
  Alcotest.(check (list string)) __LOC__ [] targets

let test_has_no_inline () =
  let input = "(project test\n  (doc \"Standalone\")\n  (status planned)\n  (no-inline))" in
  let file = Borge_lang.Parse.parse input in
  Alcotest.(check bool) __LOC__ true (Borge_lib.Spec.has_no_inline file)

let test_has_no_inline_false () =
  let input = "(project test\n  (doc \"Has parent\")\n  (status planned))" in
  let file = Borge_lang.Parse.parse input in
  Alcotest.(check bool) __LOC__ false (Borge_lib.Spec.has_no_inline file)

(* Helper: collect all top-level atoms from a parsed file, flattened. *)
let collect_atoms file =
  let acc = ref [] in
  let rec walk_sexp = function
    | Borge_lang.Ast.Atom (_, a) -> acc := a :: !acc
    | Borge_lang.Ast.String _ -> ()
    | Borge_lang.Ast.List (_, children) -> List.iter walk_sexp children
  in
  List.iter (fun swc -> walk_sexp swc.Borge_lang.Ast.node) file.Borge_lang.Ast.top_level;
  List.rev !acc

let test_symbol_chars_eq_colon_slash () =
  (* Regression test for the lexer bug where = : / were silently dropped
     from symbol atoms. Before the fix, (where assignee-id = current-user)
     parsed to only [where; assignee-id; current-user] — = vanished.
     ISO timestamps (with :) and file paths (with /) hit the same bug.
     This test locks the fix: all three chars now survive as part of atoms. *)
  let input = "(where assignee-id = current-user)" in
  let file = Borge_lang.Parse.parse input in
  let atoms = collect_atoms file in
  Alcotest.(check int) __LOC__ 4 (List.length atoms);
  Alcotest.(check bool) __LOC__ true (List.mem "=" atoms);
  Alcotest.(check bool) __LOC__ true (List.mem "assignee-id" atoms);
  Alcotest.(check bool) __LOC__ true (List.mem "current-user" atoms)

let test_symbol_char_colon_timestamp () =
  (* ISO 8601 timestamps contain : which must survive lexing as a single
     atom (or at least not be silently dropped). Used in .borg.meta
     (discharged-at 2026-07-05T07:38:40Z). *)
  let input = "(discharged-at 2026-07-05T07:38:40Z)" in
  let file = Borge_lang.Parse.parse input in
  let atoms = collect_atoms file in
  Alcotest.(check bool) __LOC__ true (List.mem "2026-07-05T07:38:40Z" atoms)

let test_symbol_char_slash_path () =
  (* File paths contain / which must survive lexing. Used in .borg.meta
     (witness "proof/db_auth.v") and (representation "proof/BorgeSchema.v"). *)
  let input = "(witness proof/db_auth.v)" in
  let file = Borge_lang.Parse.parse input in
  let atoms = collect_atoms file in
  Alcotest.(check bool) __LOC__ true (List.mem "proof/db_auth.v" atoms)

let () =
  Alcotest.run "borge parse tests" [
    "basic", [
      Alcotest.test_case "simple project" `Quick test_simple_project;
      Alcotest.test_case "annotated comment" `Quick test_annotated_comment;
      Alcotest.test_case "verbatim string" `Quick test_verbatim_string;
      Alcotest.test_case "ask/response" `Quick test_ask_response;
      Alcotest.test_case "design comment" `Quick test_design;
      Alcotest.test_case "plain comments" `Quick test_plain_comments;
      Alcotest.test_case "bad paren" `Quick test_bad_unclosed_paren;
      Alcotest.test_case "project name" `Quick test_project_name;
      Alcotest.test_case "inline targets" `Quick test_inline_targets;
      Alcotest.test_case "inline targets empty" `Quick test_inline_targets_empty;
      Alcotest.test_case "has no-inline" `Quick test_has_no_inline;
      Alcotest.test_case "no-inline false" `Quick test_has_no_inline_false;
      Alcotest.test_case "symbol = retained" `Quick test_symbol_chars_eq_colon_slash;
      Alcotest.test_case "symbol : in timestamp retained" `Quick test_symbol_char_colon_timestamp;
      Alcotest.test_case "symbol / in path retained" `Quick test_symbol_char_slash_path;
    ];
  ]
