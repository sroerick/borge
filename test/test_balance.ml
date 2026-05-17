open Borge_lang.Balance

let assert_balanced s =
  match check s with
  | Balanced _ -> ()
  | Imbalanced errs ->
      Alcotest.fail (Printf.sprintf "Expected balanced, got errors: %s"
        (String.concat "; " (List.map string_of_error errs)))

let assert_imbalanced s =
  match check s with
  | Balanced _ -> Alcotest.fail "Expected imbalanced, got balanced"
  | Imbalanced _ -> ()

let test_simple_balanced () =
  assert_balanced "(project test)"

let test_unclosed () =
  assert_imbalanced "(project test"

let test_extra_close () =
  assert_imbalanced "project test)"

let test_nested_balanced () =
  assert_balanced "(project test (section a (subsection b)))"

let test_verbatim_ignores_parens () =
  assert_balanced "(doc (|hello (world)|))"

let test_comment_ignores_parens () =
  assert_balanced "; this has ) parens\n(project test)"

let test_annotated_comment () =
  assert_balanced "(* agent note (|ok|) *)\n(project test)"

let () =
  Alcotest.run "Balance checker" [
    "balance", [
      Alcotest.test_case "simple balanced" `Quick test_simple_balanced;
      Alcotest.test_case "unclosed" `Quick test_unclosed;
      Alcotest.test_case "extra close" `Quick test_extra_close;
      Alcotest.test_case "nested balanced" `Quick test_nested_balanced;
      Alcotest.test_case "verbatim ignores parens" `Quick test_verbatim_ignores_parens;
      Alcotest.test_case "plain comment ignores parens" `Quick test_comment_ignores_parens;
      Alcotest.test_case "annotated comment ignored" `Quick test_annotated_comment;
    ]
  ]
