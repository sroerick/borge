open Borge_lib

(* Fixture builder (same pattern as test_book_walk): a throwaway
   project in a fresh temp dir, handed to the test body, then
   removed. *)
let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let rec mkdir_p d =
  if Sys.file_exists d then ()
  else begin
    mkdir_p (Filename.dirname d);
    (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  end

let with_fixture name files (f : string -> unit) =
  let dir =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "borge_book_export_%s_%d" name (Unix.getpid ()))
  in
  let cleanup () =
    ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)))
  in
  cleanup ();
  List.iter
    (fun (rel, content) ->
      let p = Filename.concat dir rel in
      mkdir_p (Filename.dirname p);
      write_file p content)
    files;
  f dir;
  cleanup ()

let read_bytes path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let buf = really_input_string ic n in
  close_in ic;
  buf

let str j k = Yojson.Safe.Util.member k j |> Yojson.Safe.Util.to_string
let arr j k = Yojson.Safe.Util.member k j |> Yojson.Safe.Util.to_list
let opt_str j k =
  match Yojson.Safe.Util.member k j with
  | `Null -> None
  | `String s -> Some s
  | other -> failwith ("expected string or null for " ^ k ^ ": " ^ Yojson.Safe.to_string other)

(* The export is the habitat loader's feed (pricklypear
   borge/habitat-book.borg export-projection): chapters in inline
   order with slug/title/status/nodes/examples, code chapters
   (.pp/.sql) in the lexicographic appendix with true source bytes. *)
let test_shape_and_order () =
  with_fixture "shape"
    [
      ( "root.borg",
        "(project demo\n (doc \"demo book\")\n (inline \"a.borg\")\n (inline \"sub/b.borg\")\n)\n" );
      ( "a.borg",
        "(section a\n (status implemented)\n (doc \"chapter a\")\n\
         (subsection a1 (doc \"sub a1\"))\n)\n" );
      ("sub/b.borg", "(section b (doc \"chapter b\"))\n");
      ("lib/x.pp", "(define one 1)\n");
      ("migrations/001_init.sql", "CREATE TABLE t(id text);\n");
    ]
  (fun dir ->
    let stem = Filename.concat dir "out" in
    let nc, nk = Book_export.export ~dir ~stem ~root:None ~mode:Book_structure.Workshop in
    Alcotest.(check int) "chapter count" 3 nc;
    Alcotest.(check int) "code chapter count" 2 nk;
    let j = Yojson.Safe.from_file (stem ^ ".book.json") in
    Alcotest.(check string) "root field" "root.borg" (str j "root");
    Alcotest.(check string) "mode field" "workshop" (str j "mode");
    let slugs = List.map (fun c -> str c "slug") (arr j "chapters") in
    Alcotest.(check (list string)) "chapters in inline order"
      [ "root"; "a"; "sub/b" ] slugs;
    let code_paths = List.map (fun c -> str c "path") (arr j "code-chapters") in
    Alcotest.(check (list string)) "code chapters in appendix order"
      [ "lib/x.pp"; "migrations/001_init.sql" ] code_paths;
    let ca = List.nth (arr j "chapters") 1 in
    Alcotest.(check string) "chapter title from first section" "a" (str ca "title");
    Alcotest.(check (option string)) "chapter status from first (status X)"
      (Some "implemented") (opt_str ca "status");
    Alcotest.(check int) "examples empty until example-fences" 0
      (List.length (arr ca "examples"));
    (* nodes: section a -> [status; doc; subsection a1] *)
    let kinds ns = List.map (fun n -> str n "kind") ns in
    Alcotest.(check (list string)) "chapter a top node is the section"
      [ "section" ] (kinds (arr ca "nodes"));
    let kids = arr (List.hd (arr ca "nodes")) "children" in
    Alcotest.(check (list string)) "section children kinds"
      [ "status"; "doc"; "subsection" ] (kinds kids);
    Alcotest.(check string) "status node body" "implemented"
      (str (List.nth kids 0) "body");
    Alcotest.(check string) "doc node body" "chapter a"
      (str (List.nth kids 1) "body");
    (* code chapter fields: lang + verbatim source *)
    let cx = List.hd (arr j "code-chapters") in
    Alcotest.(check string) "code lang" "pp" (str cx "lang");
    Alcotest.(check string) "code source is the true bytes" "(define one 1)\n"
      (str cx "source"))

(* Reader vs workshop: the scaffolding node kinds (status, verify,
   agent-note, untracked) exist only in workshop; prose, headings,
   and inline navigation stay in both. Same walk, one Book_structure
   filter. *)
let test_mode_filter () =
  with_fixture "modes"
    [
      ( "root.borg",
        "(project demo\n (inline \"a.borg\")\n)\n" );
      ( "a.borg",
        "(section a\n (status draft)\n (verify (build \"dune build\") (smoke \"s\"))\n\
         (doc \"prose here\")\n (inline \"other.borg\")\n\
         (untracked stray.ml \"why\"))\n" );
      ("other.borg", "(section other (doc \"other\"))\n");
    ]
  (fun dir ->
    let export mode stem =
      ignore (Book_export.export ~dir ~stem:(Filename.concat dir stem) ~root:None ~mode)
    in
    export Book_structure.Workshop "ws";
    export Book_structure.Reader "rd";
    let ws = Yojson.Safe.from_file (Filename.concat dir "ws.book.json") in
    let rd = Yojson.Safe.from_file (Filename.concat dir "rd.book.json") in
    let kinds j =
      let ch =
        List.find (fun c -> str c "slug" = "a") (arr j "chapters")
      in
      let sec = List.hd (arr ch "nodes") in
      List.map (fun n -> str n "kind") (arr sec "children")
    in
    Alcotest.(check (list string)) "workshop keeps scaffolding kinds"
      [ "status"; "verify"; "doc"; "inline"; "untracked" ] (kinds ws);
    Alcotest.(check (list string)) "reader strips scaffolding kinds"
      [ "doc"; "inline" ] (kinds rd);
    (* drained nested agent note: workshop node, reader gone *)
    with_fixture "nested-note"
      [
        ( "root.borg", "(project demo\n (inline \"a.borg\")\n)\n" );
        ( "a.borg",
          "(section a\n (* agent note (|\n    nested note prose\n   |) *)\n\
           (doc \"body\"))\n" );
      ]
      (fun dir2 ->
        let stem = Filename.concat dir2 "out" in
        ignore
          (Book_export.export ~dir:dir2 ~stem ~root:None
             ~mode:Book_structure.Workshop);
        let j = Yojson.Safe.from_file (stem ^ ".book.json") in
        let ch = List.find (fun c -> str c "slug" = "a") (arr j "chapters") in
        let sec = List.hd (arr ch "nodes") in
        let kids = arr sec "children" in
        Alcotest.(check (list string)) "drained agent note precedes doc"
          [ "agent-note"; "doc" ] (List.map (fun n -> str n "kind") kids);
        Alcotest.(check string) "agent note title = author+type"
          "agent note" (str (List.hd kids) "title");
        Alcotest.(check string) "agent note body" "nested note prose"
          (str (List.hd kids) "body");
        let stem2 = Filename.concat dir2 "out-rd" in
        ignore
          (Book_export.export ~dir:dir2 ~stem:stem2 ~root:None
             ~mode:Book_structure.Reader);
        let j2 = Yojson.Safe.from_file (stem2 ^ ".book.json") in
        let ch2 = List.find (fun c -> str c "slug" = "a") (arr j2 "chapters") in
        let sec2 = List.hd (arr ch2 "nodes") in
        Alcotest.(check (list string)) "reader strips the drained note"
          [ "doc" ] (List.map (fun n -> str n "kind") (arr sec2 "children"))))

(* Drift hole: an unparseable .borg exports as one raw node carrying
   the true source, in both registers (print's fallback listing
   equivalent). *)
let test_raw_fallback () =
  with_fixture "raw"
    [
      ("root.borg", "(project demo\n (inline \"a.borg\")\n)\n");
      ("a.borg", "(section x (doc \"unterminated");
    ]
  (fun dir ->
    let stem = Filename.concat dir "out" in
    let nc, _ =
      Book_export.export ~dir ~stem ~root:None ~mode:Book_structure.Workshop
    in
    Alcotest.(check int) "chapters still counted" 2 nc;
    let j = Yojson.Safe.from_file (stem ^ ".book.json") in
    let bad = List.nth (arr j "chapters") 1 in
    Alcotest.(check string) "raw chapter slug" "a" (str bad "slug");
    Alcotest.(check (option string)) "raw chapter has no status" None
      (opt_str bad "status");
    let nodes = arr bad "nodes" in
    Alcotest.(check int) "one raw node" 1 (List.length nodes);
    let n = List.hd nodes in
    Alcotest.(check string) "raw kind" "raw" (str n "kind");
    Alcotest.(check string) "raw body = true source"
      "(section x (doc \"unterminated" (str n "body"))

(* No wall clock, no absolute paths: two runs of the same tree are
   byte-identical (determinism across clones — the loader checksums
   against this). *)
let test_deterministic () =
  with_fixture "determinism"
    [
      ("root.borg", "(project demo\n (inline \"a.borg\")\n)\n");
      ("a.borg", "(section a (doc \"chapter a\") (status implemented))\n");
      ("lib/x.pp", "(define one 1)\n");
    ]
  (fun dir ->
    let s1 = Filename.concat dir "run1" in
    let s2 = Filename.concat dir "run2" in
    ignore (Book_export.export ~dir ~stem:s1 ~root:None ~mode:Book_structure.Workshop);
    ignore (Book_export.export ~dir ~stem:s2 ~root:None ~mode:Book_structure.Workshop);
    Alcotest.(check string) "two runs byte-identical"
      (read_bytes (s1 ^ ".book.json")) (read_bytes (s2 ^ ".book.json"));
    Alcotest.(check bool) "no absolute path leakage"
      false
      (try ignore (Str.search_forward (Str.regexp (Str.quote dir)) (read_bytes (s1 ^ ".book.json")) 0); true
       with Not_found -> false))

(* --root exports exactly the inline subtree: no appendix, no code
   chapters the tree does not name. *)
let test_subtree_root () =
  with_fixture "subtree"
    [
      ("root.borg", "(project demo\n (inline \"a.borg\")\n (inline \"sub/b.borg\")\n)\n");
      ("a.borg", "(section a (doc \"chapter a\") (inline \"sub/b.borg\"))\n");
      ("sub/b.borg", "(section b (doc \"chapter b\"))\n");
      ("lib/x.pp", "(define one 1)\n");
    ]
  (fun dir ->
    let stem = Filename.concat dir "out" in
    let nc, nk =
      Book_export.export ~dir ~stem ~root:(Some "a.borg") ~mode:Book_structure.Workshop
    in
    Alcotest.(check int) "subtree chapters only" 2 nc;
    Alcotest.(check int) "no unnamed code chapters" 0 nk;
    let j = Yojson.Safe.from_file (stem ^ ".book.json") in
    Alcotest.(check string) "root field is the selected file" "a.borg"
      (str j "root");
    let slugs = List.map (fun c -> str c "slug") (arr j "chapters") in
    Alcotest.(check (list string)) "subtree order" [ "a"; "sub/b" ] slugs)

(* Inline targets may be bare atoms, not just quoted strings —
   the same forms Spec.inline_targets accepts (pricklypear.borg
   writes (inline borge/philosophy.borg) atom-style). Navigation
   note, kept in BOTH registers. *)
let test_inline_atom_target () =
  with_fixture "inline-atom"
    [
      ("root.borg", "(project demo\n (inline a.borg)\n)\n");
      ("a.borg", "(section a (doc \"chapter a\"))\n");
    ]
  (fun dir ->
    let export mode stem =
      ignore (Book_export.export ~dir ~stem:(Filename.concat dir stem) ~root:None ~mode)
    in
    export Book_structure.Workshop "ws";
    export Book_structure.Reader "rd";
    let kinds j =
      let ch = List.find (fun c -> str c "slug" = "root") (arr j "chapters") in
      let proj = List.hd (arr ch "nodes") in
      List.map (fun n -> (str n "kind", str n "body")) (arr proj "children")
    in
    let ws = kinds (Yojson.Safe.from_file (Filename.concat dir "ws.book.json")) in
    let rd = kinds (Yojson.Safe.from_file (Filename.concat dir "rd.book.json")) in
    Alcotest.(check (list (pair string string)))
      "workshop: atom inline target becomes a navigation node"
      [ ("inline", "a.borg") ] ws;
    Alcotest.(check (list (pair string string)))
      "reader: inline navigation kept"
      [ ("inline", "a.borg") ] rd)

let () =
  Alcotest.run "book_export"
    [
      ( "export",
        [
          Alcotest.test_case "shape + inline order + code chapters" `Quick
            test_shape_and_order;
          Alcotest.test_case "reader strips scaffolding, workshop keeps it"
            `Quick test_mode_filter;
          Alcotest.test_case "unparseable chapter exports as raw node"
            `Quick test_raw_fallback;
          Alcotest.test_case "two runs byte-identical, no abs paths" `Quick
            test_deterministic;
          Alcotest.test_case "--root exports exactly the subtree" `Quick
            test_subtree_root;
          Alcotest.test_case "atom inline target renders in both registers" `Quick
            test_inline_atom_target;
        ] );
    ]
