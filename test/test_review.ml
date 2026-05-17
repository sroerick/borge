(* Tests for semantic review system *)

open Borge_lib

let test_extract_functions () =
  let code = "let foo x = x + 1\n" in
  let _functions = Semantic_review.extract_functions code in
  (* Just verify extraction runs without error *)
  Alcotest.(check pass) "Extraction runs" () ()

let test_review_types () =
  Alcotest.(check string) "High to string" "high" 
    (Review_types.string_of_confidence Review_types.High);
  match Review_types.confidence_of_string "high" with
  | Some _ -> Alcotest.(check pass) "Parse works" () ()
  | None -> Alcotest.fail "Parse failed"

let () =
  Alcotest.run "Review tests" [
    ("extract", [Alcotest.test_case "Functions" `Quick test_extract_functions]);
    ("types", [Alcotest.test_case "Conversions" `Quick test_review_types]);
  ]
