(* agent note (|
 *   WHAT: Manifest sidecar for the codebook round trip.
 *   JSON (yojson): stem, rendered_at, backend, tree_checksum,
 *   files (ordered), file_pages (file -> start_page).
 *   WHY: The round-trip anchor. Line numbers are printed on the
 *   page by listings, so the manifest never stores line ranges —
 *   it only records which file begins on which page (so a margin
 *   note in the gutter between two files is unambiguous) plus a
 *   tree checksum that lets absorb detect a stale book.
 * |) *)

type page_entry = {
  file : string;
  start_page : int;
}

type manifest = {
  stem : string;
  rendered_at : string;
  backend : string;
  tree_checksum : string;
  files : string list;
  file_pages : page_entry list;
}

let content_digest path =
  try
    let content = File_utils.read_file path in
    Digest.to_hex (Digest.string content)
  with Sys_error _ -> ""

(* Ordered digest over (path, content digest). Ordered so a file
   reorder is detectable, not just content changes. *)
let tree_checksum ~dir files =
  let buf = Buffer.create 4096 in
  List.iter (fun path ->
    Buffer.add_string buf path;
    Buffer.add_char buf '\x00';
    Buffer.add_string buf (content_digest (Filename.concat dir path));
    Buffer.add_char buf '\x01'
  ) files;
  Digest.to_hex (Digest.string (Buffer.contents buf))

let now_iso () =
  let open Unix in
  let t = time () in
  let tm = localtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let build ~stem ~backend ~tree_checksum ~files ~file_pages =
  {
    stem;
    rendered_at = now_iso ();
    backend;
    tree_checksum;
    files;
    file_pages;
  }

let page_entry_to_json e =
  `Assoc [
    ("file", `String e.file);
    ("start_page", `Int e.start_page);
  ]

let to_json m =
  let json =
    `Assoc [
      ("stem", `String m.stem);
      ("rendered_at", `String m.rendered_at);
      ("backend", `String m.backend);
      ("tree_checksum", `String m.tree_checksum);
      ("files", `List (List.map (fun f -> `String f) m.files));
      ("file_pages", `List (List.map page_entry_to_json m.file_pages));
    ]
  in
  Yojson.Safe.pretty_to_string json

let write ~path m =
  let oc = open_out path in
  output_string oc (to_json m);
  output_string oc "\n";
  close_out oc
