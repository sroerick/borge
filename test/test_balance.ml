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

let assert_analyze_pairs_expected s expected_pairs =
  let analysis = analyze s in
  let actual =
    List.map (fun (p : Borge_lang.Balance.paren_pair) ->
      (p.open_pos.line, p.close_pos |> Option.map (fun (c : Borge_lang.Balance.pos) -> c.line), p.keyword)
    ) analysis.pairs
  in
  if actual <> expected_pairs then
    Alcotest.fail (Printf.sprintf "Expected pairs %s, got %s"
      (String.concat "; " (List.map (fun (l, c, k) ->
         Printf.sprintf "(%d,%s,%s)" l (match c with Some x -> string_of_int x | None -> "None") (match k with Some x -> x | None -> "-")
       ) expected_pairs))
      (String.concat "; " (List.map (fun (l, c, k) ->
         Printf.sprintf "(%d,%s,%s)" l (match c with Some x -> string_of_int x | None -> "None") (match k with Some x -> x | None -> "-")
       ) actual)))

let assert_divergences s expected_count =
  let analysis = analyze s in
  let divs = find_divergences analysis.pairs in
  if List.length divs <> expected_count then
    Alcotest.fail (Printf.sprintf "Expected %d divergences, got %d" expected_count (List.length divs))

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

let test_analyze_records_pairs () =
  assert_analyze_pairs_expected
    "(project test (section a))"
    [ (1, Some 1, Some "project"); (1, Some 1, Some "section") ]

let test_analyze_unclosed_pairs () =
  let analysis = analyze "(project test (section a" in
  match analysis.pairs with
  | [p1; p2] ->
      if p1.close_pos <> None then Alcotest.fail "project should be unclosed";
      if p2.close_pos <> None then Alcotest.fail "section should be unclosed";
      if p1.keyword <> Some "project" then Alcotest.fail "wrong keyword for project";
      if p2.keyword <> Some "section" then Alcotest.fail "wrong keyword for section";
  | _ -> Alcotest.fail (Printf.sprintf "Expected 2 pairs, got %d" (List.length analysis.pairs))

let test_divergence_detected () =
  (* section gamma at col 2 but depth 3 → under-indented *)
  assert_divergences
    "(project test\n (section alpha\n  (doc \"hello\")\n (section beta\n  (doc \"world\")))"
    2

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
      Alcotest.test_case "analyze records pairs" `Quick test_analyze_records_pairs;
      Alcotest.test_case "analyze unclosed pairs" `Quick test_analyze_unclosed_pairs;
      Alcotest.test_case "divergence detected" `Quick test_divergence_detected;
    ]
  ]
