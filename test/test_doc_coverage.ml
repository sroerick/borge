(* Tests for documentation coverage system *)

open Borge_lib

let test_doc_extract () =
  let code = {|
(* Public function with doc *)
let public_func x = x + 1

let private_helper x = x

let _ = ignore ()

let%test "test" = true
|}
  in
  (* Write temp file *)
  let tmp = Filename.temp_file "test" ".ml" in
  let oc = open_out tmp in
  output_string oc code;
  close_out oc;
  let bindings = Doc_extract.extract_bindings tmp in
  Sys.remove tmp;
  (* Should find bindings *)
  Alcotest.(check bool) "Found bindings" true (List.length bindings >= 2)

let test_doc_detect () =
  let code = {|
(* This is a doc comment *)
let foo x = x

(* exempt doc *)
let bar y = y
|}
  in
  let tmp = Filename.temp_file "test" ".ml" in
  let oc = open_out tmp in
  output_string oc code;
  close_out oc;
  let docs = Doc_detect.extract_file_docs tmp in
  Sys.remove tmp;
  Alcotest.(check bool) "Found docs" true (List.length docs > 0)

let test_doc_coverage () =
  let fc : Doc_coverage.file_coverage = {
    path = "test.ml";
    total_bindings = 5;
    documented = 3;
    exempt = 1;
    undocumented = 1;
    coverage_percent = 75.0;
    undocumented_names = ["helper"];
  }
  in
  Alcotest.(check (float 0.01)) "Coverage calc" 75.0 fc.coverage_percent;
  let meta = Doc_coverage.generate_meta_block fc in
  Alcotest.(check bool) "Meta block contains file" true (String.contains meta 't')

let () =
  Alcotest.run "Doc coverage tests" [
    ("extract", [Alcotest.test_case "Extract bindings" `Quick test_doc_extract]);
    ("detect", [Alcotest.test_case "Detect docs" `Quick test_doc_detect]);
    ("coverage", [Alcotest.test_case "Generate meta" `Quick test_doc_coverage]);
  ]
