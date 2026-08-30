(* agent note (|
 *   WHAT: The export projection (`borge book export`): the same
 *   inline-aware walk as print (Book_print.collect_files /
 *   collect_subtree), each .borg chapter rendered through the SAME
 *   Book_structure pass (register filtering applied once), emitted
 *   as one deterministic JSON artifact <stem>.book.json.
 *   WHY: habitat-book decision 3 — one parser, two projections.
 *   This artifact feeds the pricklypear habitat loader
 *   (borge/habitat-book.borg export-projection / habitat-loader);
 *   the loader is this shape's client — keep them in lockstep.
 *   SHAPE (per habitat-book.borg export-projection):
 *     { root,                    canon dir-relative walk root
 *       mode,                    "workshop" | "reader"
 *       chapters: [{             .borg chapters, walk order
 *         slug,                  path sans .borg ("borge/philosophy")
 *         title,                 first project/section name, else slug
 *         status,                first (status X) depth-first, or null
 *         nodes: [{kind, title, body, children}],
 *         examples: [] }],       fenced blocks land here (P1.6)
 *       code-chapters: [{        .ml/.mli/.pp/.sql, appendix order
 *         path, lang, source }] }
 *   - examples carry the fenced example records (P1.6): one per
 *     ```nopales/```ocaml doc fence in document order, each
 *     {lang, src, expected?} — expected is the `expect =>` trailer
 *     or null (the v1 captured-output record; no live evaluator).
 *   - code-chapters carries the spec's hyphenated spelling; sources
 *     are the TRUE bytes (print's ASCII sanitize is a pdflatex
 *     concern only).
 *   - no wall clock anywhere: two runs of the same tree are
 *     byte-identical (the manifest sidecar keeps rendered_at).
 *   - an unparseable chapter exports as one raw node carrying its
 *     true source, in both registers — the same drift hole print
 *     renders as a fallback listing.
 * |) *)

type code_chapter = {
  path : string;
  lang : string;
  source : string;
}

let lang_of path =
  match Filename.extension path with
  | "" -> ""
  | e -> String.sub e 1 (String.length e - 1)

let rec nodes_to_json (ns : Book_structure.node list) : Yojson.Safe.t =
  `List (List.map node_to_json ns)

and node_to_json (n : Book_structure.node) : Yojson.Safe.t =
  `Assoc
    [ "kind", `String n.kind;
      "title", `String n.title;
      "body", `String n.body;
      "children", nodes_to_json n.children ]

let example_to_json (e : Book_structure.example) =
  `Assoc
    [ "lang", `String e.Book_structure.lang;
      "src", `String e.Book_structure.src;
      "expected",
      (match e.Book_structure.expected with
       | Some v -> `String v
       | None -> `Null) ]

let chapter_to_json (slug, title, status, nodes, examples) =
  `Assoc
    [ "slug", `String slug;
      "title", `String title;
      "status", (match status with Some s -> `String s | None -> `Null);
      "nodes", nodes_to_json nodes;
      "examples", `List (List.map example_to_json examples) ]

let build_chapter ~mode ~dir path =
  let slug = Filename.remove_extension path in
  let raw =
    try File_utils.read_file (Filename.concat dir path) with Sys_error _ -> ""
  in
  match Book_structure.of_file ~mode ~content:raw with
  | body ->
    let title =
      match body.Book_structure.title_hint with Some t -> t | None -> slug
    in
    ( slug,
      title,
      body.Book_structure.status,
      body.Book_structure.nodes,
      body.Book_structure.examples )
  | exception Book_structure.Fallback_raw ->
    (* Drift hole: the chapter ships as raw source, in both
       registers — same rule as print's fallback listing. *)
    ( slug,
      slug,
      None,
      [ { Book_structure.kind = "raw"; title = ""; body = raw; children = [] } ],
      [] )

(* The walk root, canon dir-relative: for --root, the resolved root
   file; otherwise the first root of the default walk (lexicographic
   when several — the same order collect_files walks them). *)
let walk_root ~dir = function
  | Some f ->
    Book_print.canon_path ~dir (Book_print.resolve_root_file ~dir ~root_file:f)
  | None ->
    (match
       Project.find_roots dir
       |> List.map (Book_print.canon_path ~dir)
       |> List.sort_uniq compare
     with
     | r :: _ -> r
     | [] -> "")

let code_chapter_to_json c =
  `Assoc
    [ "path", `String c.path;
      "lang", `String c.lang;
      "source", `String c.source ]

(* Writes <stem>.book.json; returns (chapters, code chapters). *)
let export ~dir ~stem ~root ~mode =
  let files =
    match root with
    | Some f -> Book_print.collect_subtree ~dir ~root_file:f
    | None -> Book_print.collect_files dir
  in
  if files = [] then failwith "no source files found";
  let borg_files = List.filter (fun p -> Filename.check_suffix p ".borg") files in
  let code_files = List.filter (fun p -> not (Filename.check_suffix p ".borg")) files in
  let chapters = List.map (build_chapter ~mode ~dir) borg_files in
  let code_chapters =
    List.map
      (fun path ->
        let source =
          try File_utils.read_file (Filename.concat dir path) with Sys_error _ -> ""
        in
        { path; lang = lang_of path; source })
      code_files
  in
  let mode_str =
    Book_structure.(match mode with Workshop -> "workshop" | Reader -> "reader")
  in
  let json =
    `Assoc
      [ "root", `String (walk_root ~dir root);
        "mode", `String mode_str;
        "chapters", `List (List.map chapter_to_json chapters);
        "code-chapters", `List (List.map code_chapter_to_json code_chapters) ]
  in
  let oc = open_out (stem ^ ".book.json") in
  output_string oc (Yojson.Safe.to_string json);
  output_char oc '\n';
  close_out oc;
  (List.length chapters, List.length code_chapters)
