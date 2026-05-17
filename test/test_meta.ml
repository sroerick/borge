open Borge_lib

let test_meta_write () =
  let meta : Meta.meta = {
    Meta.project_name = "test-project";
    Meta.analyzed_at = "2026-01-01T00:00:00Z";
    Meta.content_hashes = [("./foo.borg", "abcdef12")];
    Meta.module_surfaces = [{
      Meta.module_name = "Foo";
      Meta.file = "./lib/foo.ml";
      Meta.exports = ["bar"; "baz"];
    }];
    Meta.dune_snapshot = [{
      Meta.name = "test_lib";
      Meta.modules = ["foo"];
      Meta.public_name = Some "test.lib";
      Meta.libraries = ["unix"];
    }];
    Meta.findings = [{
      Meta.ft_type = Meta.Unspecified_module;
      Meta.section = None;
      Meta.module_ = Some "Foo";
      Meta.file = Some "./lib/foo.ml";
      Meta.export = None;
      Meta.detail = Some "module Foo not mentioned in any .borg spec";
      Meta.spec_status = None;
      Meta.actual_status = None;
      Meta.confidence = Meta.High;
      Meta.source = Meta.Static;
      Meta.at = "2026-01-01T00:00:00Z";
    }];
  } in
  let content = Meta.string_of_meta meta ~docs:None () in
  Alcotest.(check bool) __LOC__ true (String.length content > 0);
  Alcotest.(check bool) __LOC__ true
    (let s = "DO NOT EDIT" in
     String.length content >= String.length s &&
     try ignore (Str.search_forward (Str.regexp_string s) content 0); true
     with Not_found -> false);
  Alcotest.(check bool) __LOC__ true (String.contains content '(')

let test_meta_merge () =
  let static_finding : Meta.finding = {
    Meta.ft_type = Meta.Unspecified_module;
    Meta.section = None;
    Meta.module_ = Some "Foo";
    Meta.file = None;
    Meta.export = None;
    Meta.detail = None;
    Meta.spec_status = None;
    Meta.actual_status = None;
    Meta.confidence = Meta.High;
    Meta.source = Meta.Static;
    Meta.at = "2026-01-01T00:00:00Z";
  } in
  let old_agent_finding = {
    static_finding with
    Meta.ft_type = Meta.Partial_implementation;
    Meta.source = Meta.Agent;
    Meta.detail = Some "old agent finding";
  } in
  let new_agent_finding = {
    static_finding with
    Meta.ft_type = Meta.Status_mismatch;
    Meta.source = Meta.Agent;
    Meta.detail = Some "new agent finding";
  } in
  let merged = Meta.merge_agent_findings [static_finding; old_agent_finding] [new_agent_finding] in
  Alcotest.(check int) __LOC__ 2 (List.length merged);
  let has_static = List.exists (fun f -> f.Meta.source = Meta.Static) merged in
  let has_old_agent = List.exists (fun f ->
    f.Meta.source = Meta.Agent && f.Meta.detail = Some "old agent finding"
  ) merged in
  let has_new_agent = List.exists (fun f ->
    f.Meta.source = Meta.Agent && f.Meta.detail = Some "new agent finding"
  ) merged in
  Alcotest.(check bool) __LOC__ true has_static;
  Alcotest.(check bool) __LOC__ false has_old_agent;
  Alcotest.(check bool) __LOC__ true has_new_agent

let test_content_hash () =
  let h1 = Meta.content_hash "hello world" in
  let h2 = Meta.content_hash "hello world" in
  let h3 = Meta.content_hash "goodbye world" in
  Alcotest.(check string) __LOC__ h1 h2;
  Alcotest.(check bool) __LOC__ true (h1 <> h3)

let test_is_meta_file () =
  Alcotest.(check bool) __LOC__ true (Meta.is_meta_file "foo.borg.meta");
  Alcotest.(check bool) __LOC__ false (Meta.is_meta_file "foo.borg");
  Alcotest.(check bool) __LOC__ false (Meta.is_meta_file "meta");
  Alcotest.(check bool) __LOC__ false (Meta.is_meta_file "")

let () =
  Alcotest.run "meta tests" [
    "meta", [
      Alcotest.test_case "write" `Quick test_meta_write;
      Alcotest.test_case "merge" `Quick test_meta_merge;
      Alcotest.test_case "hash" `Quick test_content_hash;
      Alcotest.test_case "is_meta_file" `Quick test_is_meta_file;
    ];
  ]
