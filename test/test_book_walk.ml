open Borge_lib

(* Fixture builder: materialize a throwaway project in a fresh temp dir,
   hand it to the test body, then remove it. Paths are (relative-name,
   content) pairs; parent dirs are created as needed. *)
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
      (Printf.sprintf "borge_book_walk_%s_%d" name (Unix.getpid ()))
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

let tmp_subdir name =
  let d =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "borge_%s_%d_%d" name (Unix.getpid ())
         (int_of_float (Unix.time () *. 1000.0) mod 1_000_000))
  in
  mkdir_p d;
  d

let read_bytes path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let buf = really_input_string ic n in
  close_in ic;
  buf

(* The inline tree is the chapter order; everything the tree does not
   reference (roots of their own, code) follows, lexicographic inside
   the appendix. Determinism: a second call is byte-identical, and dir
   spelled with a trailing "." component walks the same. *)
let test_inline_order () =
  with_fixture "order"
    [
      ("root.borg", "(project demo\n (doc \"demo book\")\n (inline \"a.borg\")\n (inline \"sub/b.borg\")\n)\n");
      ("a.borg", "(project a\n (doc \"chapter a\")\n)\n");
      ("sub/b.borg", "(project b\n (doc \"chapter b\")\n)\n");
      ("zzz.borg", "(project zzz\n (doc \"unreferenced chapter\")\n)\n");
      ("code/util.ml", "let hello = \"hi\"\n");
    ]
    (fun dir ->
      let files = Book_print.collect_files dir in
      Alcotest.(check (list string)) "inline tree first, appendix after"
        [ "root.borg"; "a.borg"; "sub/b.borg"; "zzz.borg"; "code/util.ml" ]
        files;
      Alcotest.(check (list string)) "deterministic across calls" files
        (Book_print.collect_files dir);
      Alcotest.(check (list string)) "same order with dir spelled dir/."
        files
        (Book_print.collect_files (Filename.concat dir ".")))

(* A root whose inline tree fails to build degrades to a lone chapter
   (stderr warning); no file is lost. *)
let test_missing_target () =
  with_fixture "missing"
    [
      ("root2.borg", "(project demo2\n (doc \"broken tree\")\n (inline \"gone.borg\")\n)\n");
      ("x.borg", "(project x\n (doc \"stray chapter\")\n)\n");
    ]
    (fun dir ->
      let files = Book_print.collect_files dir in
      Alcotest.(check (list string)) "missing target degrades, nothing lost"
        [ "root2.borg"; "x.borg" ] files)

(* The same file inlined twice keeps its first position. *)
let test_duplicate_inline () =
  with_fixture "dup"
    [
      ("root3.borg", "(project demo3\n (doc \"dup inline\")\n (inline \"a.borg\")\n (inline \"a.borg\")\n)\n");
      ("a.borg", "(project a\n (doc \"chapter a\")\n)\n");
    ]
    (fun dir ->
      let files = Book_print.collect_files dir in
      Alcotest.(check (list string)) "duplicate inline keeps first position"
        [ "root3.borg"; "a.borg" ] files)

(* Type coverage (habitat-book): .pp (Nopales product layer) and .sql
   (migrations) are code chapters — unreferenced by the inline tree,
   they land in the lexicographic appendix. Gitignored vault dumps
   (backups/, the pp-sync snapshot output) are skipped. *)
let test_type_coverage () =
  with_fixture "types"
    [
      ("root.borg", "(project demo\n (doc \"demo book\")\n)\n");
      ("libs/spec/lib.pp", "(lib spec\n (doc \"spec package\")\n)\n");
      ("migrations/001_init.sql", "create table t (id int);\n");
      ("migrations/010_tree.sql", "create table tree_nodes (id int);\n");
      ("backups/old/lib.pp", "(lib stale (doc \"vault dump\"))\n");
    ]
    (fun dir ->
      let files = Book_print.collect_files dir in
      Alcotest.(check (list string))
        ".pp/.sql are chapters, backups/ dumps skipped"
        [ "root.borg"; "libs/spec/lib.pp"; "migrations/001_init.sql";
          "migrations/010_tree.sql" ]
        files;
      Alcotest.(check (list string)) "deterministic across calls" files
        (Book_print.collect_files dir))

(* Type coverage, tex level: a .pp/.sql chapter is cited as a raw
   listing under its own extension, with the content copied through
   sanitize verbatim; the .borg chapter stays prose, not a listing. *)
let test_tex_code_extensions () =
  with_fixture "texext"
    [
      ("root.borg", "(project demo\n (doc \"demo book\")\n)\n");
      ("libs/spec/lib.pp", "(lib spec (doc \"spec package\"))\n");
      ("migrations/001_init.sql", "create table t (id int);\n");
    ]
    (fun dir ->
      let files = Book_print.collect_files dir in
      let t = tmp_subdir "texext" in
      Book_print.render_to_tex ~dir ~files ~tex_path:(Filename.concat t "book.tex");
      let tex = File_utils.read_file (Filename.concat t "book.tex") in
      let has s =
        try ignore (Str.search_forward (Str.regexp (Str.quote s)) tex 0); true
        with Not_found -> false
      in
      Alcotest.(check bool) ".pp cited under its own extension"
        true (has "{src/0001.pp}");
      Alcotest.(check bool) ".sql cited under its own extension"
        true (has "{src/0002.sql}");
      Alcotest.(check string) ".pp content copied verbatim"
        "(lib spec (doc \"spec package\"))\n"
        (read_bytes (Filename.concat t "src/0001.pp"));
      Alcotest.(check bool) ".borg chapter stays prose, not a raw listing"
        false (has "src/0000.borg}");
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote t))))

(* --root FILE: exactly the inline subtree rooted at FILE — nested
   inlines included, nothing outside it (no appendix, no siblings),
   and a lone chapter prints as a book of one. *)
let test_subtree_root () =
  with_fixture "subtree"
    [
      ("root.borg", "(project demo\n (doc \"demo book\")\n (inline \"a.borg\")\n (inline \"sub/b.borg\")\n)\n");
      ("a.borg", "(project a\n (doc \"chapter a\")\n)\n");
      ("sub/b.borg", "(project b\n (doc \"chapter b\")\n (inline \"d.borg\")\n)\n");
      ("sub/d.borg", "(project d\n (doc \"chapter d\")\n)\n");
      ("other.borg", "(project other\n (doc \"standalone chapter\")\n)\n");
      ("code/util.ml", "let hello = \"hi\"\n");
    ]
    (fun dir ->
      let subtree = Book_print.collect_subtree ~dir ~root_file:"root.borg" in
      Alcotest.(check (list string)) "full subtree in inline order"
        [ "root.borg"; "a.borg"; "sub/b.borg"; "sub/d.borg" ] subtree;
      let mid = Book_print.collect_subtree ~dir ~root_file:"sub/b.borg" in
      Alcotest.(check (list string)) "mid-tree subtree roots its own book"
        [ "sub/b.borg"; "sub/d.borg" ] mid;
      let lone = Book_print.collect_subtree ~dir ~root_file:"other.borg" in
      Alcotest.(check (list string)) "single chapter prints alone"
        [ "other.borg" ] lone;
      let leaf = Book_print.collect_subtree ~dir ~root_file:"a.borg" in
      Alcotest.(check (list string)) "leaf chapter is a book of one"
        [ "a.borg" ] leaf)

(* --root is a contract, not a degrade-able default: a missing file or
   an unbuildable tree fails hard with the reason. *)
let test_subtree_errors () =
  with_fixture "subtree-err"
    [
      ("bad.borg", "(project bad\n (doc \"broken tree\")\n (inline \"gone.borg\")\n)\n");
    ]
    (fun dir ->
      Alcotest.check_raises "missing root file fails" (Failure "--root: no such file: nope.borg")
        (fun () -> ignore (Book_print.collect_subtree ~dir ~root_file:"nope.borg"));
      Alcotest.check_raises "broken inline tree fails with the target named"
        (Failure ("--root bad.borg: inline tree error (file not found: " ^ dir ^ "/gone.borg)"))
        (fun () -> ignore (Book_print.collect_subtree ~dir ~root_file:"bad.borg")))

(* Determinism, .tex level: render twice into different temp dirs; the
   document must be byte-identical (listings cited relatively, no
   temp-dir names in the output). *)
let test_tex_deterministic () =
  with_fixture "tex"
    [
      ("root.borg", "(project demo\n (doc \"demo book\")\n (inline \"a.borg\")\n)\n");
      ("a.borg", "(project a\n (doc \"chapter a\")\n)\n");
      ("code/util.ml", "let hello = \"hi\"\n");
    ]
    (fun dir ->
      let files = Book_print.collect_files dir in
      let t1 = tmp_subdir "tex1" in
      let t2 = tmp_subdir "tex2" in
      Book_print.render_to_tex ~dir ~files ~tex_path:(Filename.concat t1 "book.tex");
      Book_print.render_to_tex ~dir ~files ~tex_path:(Filename.concat t2 "book.tex");
      let tex1 = read_bytes (Filename.concat t1 "book.tex") in
      let tex2 = read_bytes (Filename.concat t2 "book.tex") in
      Alcotest.(check string) "tex bytes identical across runs" tex1 tex2;
      Alcotest.(check string) "listings copied identically"
        (read_bytes (Filename.concat t1 "src/0002.ml"))
        (read_bytes (Filename.concat t2 "src/0002.ml"));
      ignore (Sys.command (Printf.sprintf "rm -rf %s %s"
                             (Filename.quote t1) (Filename.quote t2))))

(* Determinism, PDF level (skipped when pdflatex is absent): two full
   print runs of the same tree -- in two different directories, same
   stem -- must produce byte-identical PDFs, and manifests identical
   except rendered_at (the one allowed wall-clock field). *)
let strip_rendered_at s =
  Str.global_replace (Str.regexp "\"rendered_at\": *\"[^\" ]*\"")
    "\"rendered_at\":\"\"" s

let fixture_files =
  [ ("root.borg", "(project demo\n (doc \"demo book\")\n (inline \"a.borg\")\n)\n");
    ("a.borg", "(project a\n (doc \"chapter a\")\n)\n") ]

let test_pdf_deterministic () =
  if not (Book_print.has_cmd "pdflatex") then Alcotest.skip ()
  else
    with_fixture "pdf" fixture_files (fun dir ->
      let run_in sub =
        let d = Filename.concat dir sub in
        mkdir_p d;
        List.iter
          (fun (name, content) -> write_file (Filename.concat d name) content)
          fixture_files;
        let cwd = Sys.getcwd () in
        Sys.chdir d;
        Fun.protect ~finally:(fun () -> Sys.chdir cwd) (fun () ->
          let n = Book_print.print ~dir:"." ~stem:"book" ~root:None in
          Alcotest.(check int) "file count" 2 n);
        ( read_bytes (Filename.concat d "book.pdf"),
          File_utils.read_file (Filename.concat d "book.book.manifest") )
      in
      let pdf1, man1 = run_in "run1" in
      let pdf2, man2 = run_in "run2" in
      Alcotest.(check string) "pdf bytes identical across runs" pdf1 pdf2;
      Alcotest.(check string) "manifest identical modulo rendered_at"
        (strip_rendered_at man1) (strip_rendered_at man2))

let () =
  Alcotest.run "book_walk"
    [
      ( "walk",
        [
          Alcotest.test_case "inline order then lexicographic appendix" `Quick
            test_inline_order;
          Alcotest.test_case "missing inline target degrades to lone chapter"
            `Quick test_missing_target;
          Alcotest.test_case "duplicate inline keeps first position" `Quick
            test_duplicate_inline;
          Alcotest.test_case ".pp/.sql are code chapters; backups/ dumps skipped"
            `Quick test_type_coverage;
          Alcotest.test_case "tex cites .pp/.sql listings under own extension"
            `Quick test_tex_code_extensions;
          Alcotest.test_case "--root prints exactly the inline subtree" `Quick
            test_subtree_root;
          Alcotest.test_case "--root errors hard on missing file or broken tree"
            `Quick test_subtree_errors;
          Alcotest.test_case "tex output byte-identical across temp dirs" `Quick
            test_tex_deterministic;
          Alcotest.test_case "pdf bytes identical across runs (needs pdflatex)" `Slow
            test_pdf_deterministic;
        ] );
    ]
