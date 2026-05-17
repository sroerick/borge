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
    ];
  ]
