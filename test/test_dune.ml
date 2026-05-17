(** Unit tests for dune parser and module surface extractor.
    These tests work on string inputs, not filesystem paths. *)

(* --- Dune parse tests --- *)

let test_dune_library () =
  let input = "(library\n  (name my_lib)\n  (public_name my.lib)\n  (modules foo bar baz)\n  (libraries unix str))" in
  let file = Borge_lang.Parse.parse_file input in
  let stanza = match file.Borge_lang.Ast.top_level with
    | { Borge_lang.Ast.node = Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "library") :: children); _ } :: _ ->
        Borge_lib.Dune_parse.parse_library children
    | _ -> Alcotest.fail "no library stanza"
  in
  Alcotest.(check string) __LOC__ "my_lib" stanza.Borge_lib.Dune_parse.name;
  Alcotest.(check (option string)) __LOC__ (Some "my.lib") stanza.Borge_lib.Dune_parse.public_name;
  Alcotest.(check int) __LOC__ 3 (List.length stanza.Borge_lib.Dune_parse.modules);
  Alcotest.(check bool) __LOC__ true (List.mem "foo" stanza.Borge_lib.Dune_parse.modules);
  Alcotest.(check bool) __LOC__ true (List.mem "unix" stanza.Borge_lib.Dune_parse.libraries)

let test_dune_executables () =
  let input = "(executables\n  (names my_app my_tool)\n  (libraries my_lib cmdliner))" in
  let file = Borge_lang.Parse.parse_file input in
  let stanza = match file.Borge_lang.Ast.top_level with
    | { Borge_lang.Ast.node = Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "executables") :: children); _ } :: _ ->
        Borge_lib.Dune_parse.parse_executables children
    | _ -> Alcotest.fail "no executables stanza"
  in
  Alcotest.(check int) __LOC__ 2 (List.length stanza.Borge_lib.Dune_parse.names);
  Alcotest.(check bool) __LOC__ true (List.mem "my_app" stanza.Borge_lib.Dune_parse.names);
  Alcotest.(check bool) __LOC__ true (List.mem "my_lib" stanza.Borge_lib.Dune_parse.libraries)

let test_dune_test_stanza () =
  let input = "(test\n  (name test_foo)\n  (libraries my_lib alcotest))" in
  let file = Borge_lang.Parse.parse_file input in
  let stanza = match file.Borge_lang.Ast.top_level with
    | { Borge_lang.Ast.node = Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "test") :: children); _ } :: _ ->
        Borge_lib.Dune_parse.parse_test children
    | _ -> Alcotest.fail "no test stanza"
  in
  Alcotest.(check string) __LOC__ "test_foo" stanza.Borge_lib.Dune_parse.name;
  Alcotest.(check bool) __LOC__ true (List.mem "my_lib" stanza.Borge_lib.Dune_parse.libraries)

(* --- Surface extraction tests --- *)

let test_surface_exports () =
  let content = "let foo x y = x + y\nlet bar = 42\nlet _ = ignore ()\nlet () = print_endline \"hello\"\ntype t = int\nexception Error of string\nmodule M = struct end\n" in
  let exports = Borge_lib.Surface.extract_exports content in
  Alcotest.(check bool) __LOC__ true (List.mem "foo" exports);
  Alcotest.(check bool) __LOC__ true (List.mem "bar" exports);
  Alcotest.(check bool) __LOC__ true (List.mem "t" exports);
  Alcotest.(check bool) __LOC__ true (List.mem "Error" exports);
  Alcotest.(check bool) __LOC__ true (List.mem "M" exports);
  Alcotest.(check bool) __LOC__ false (List.mem "_" exports)

let test_surface_rec_bindings () =
  let content = "let rec fib n = if n < 2 then n else fib (n-1) + fib (n-2)\nlet rec fact n = if n = 0 then 1 else n * fact (n-1)\n" in
  let exports = Borge_lib.Surface.extract_exports content in
  Alcotest.(check bool) __LOC__ true (List.mem "fib" exports);
  Alcotest.(check bool) __LOC__ true (List.mem "fact" exports)

let test_surface_type_declarations () =
  let content = "type result = { ok : bool; msg : string }\ntype status = Implemented | Planned | In_progress\ntype 'a tree = Leaf | Node of 'a tree * 'a * 'a tree\n" in
  let exports = Borge_lib.Surface.extract_exports content in
  Alcotest.(check bool) __LOC__ true (List.mem "result" exports);
  Alcotest.(check bool) __LOC__ true (List.mem "status" exports);
  Alcotest.(check bool) __LOC__ true (List.mem "tree" exports)

let () =
  Alcotest.run "dune & surface tests" [
    "dune", [
      Alcotest.test_case "library stanza" `Quick test_dune_library;
      Alcotest.test_case "executables stanza" `Quick test_dune_executables;
      Alcotest.test_case "test stanza" `Quick test_dune_test_stanza;
    ];
    "surface", [
      Alcotest.test_case "exports" `Quick test_surface_exports;
      Alcotest.test_case "rec bindings" `Quick test_surface_rec_bindings;
      Alcotest.test_case "type declarations" `Quick test_surface_type_declarations;
    ];
  ]
