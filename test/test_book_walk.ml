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
        ] );
    ]
