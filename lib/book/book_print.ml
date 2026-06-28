(* agent note (|
 *   WHAT: v1 codebook printer. Walks the source tree in
 *   deterministic filesystem-order, emits a LaTeX document with
 *   the listings package (line numbers, framed code, a wide
 *   right-hand margin for hand annotations), runs pdflatex until
 *   the .aux/.toc stabilize (a multi-page TOC grows across passes),
 *   and writes a manifest sidecar via Book_manifest.
 *   WHY: The "print" half of the book round trip (borge.borg
 *   section book). Code-only, no spec interleaving; spec-order
 *   and absorb are long-tail.
 * |) *)

let skip_dirs =
  ["_build"; ".git"; ".pi"; ".ralph"; ".borge-bugs"; ".borge.lock";
   "_build_test"; "node_modules"; "_opam"; ".opamswitch"]

let is_source name =
  Filename.check_suffix name ".ml" || Filename.check_suffix name ".mli"

(* lib/ first, then bin/, then test/, then everything else. *)
let dir_priority p =
  let len = String.length p in
  if len >= 4 && String.sub p 0 4 = "lib/" then 0
  else if len >= 4 && String.sub p 0 4 = "bin/" then 1
  else if len >= 5 && String.sub p 0 5 = "test/" then 2
  else 3

let rec collect ~root ~rel acc =
  let dir = if rel = "" then root else Filename.concat root rel in
  let entries =
    try Sys.readdir dir |> Array.to_list
    with Sys_error _ -> []
  in
  List.fold_left (fun acc name ->
    if List.mem name skip_dirs then acc
    else
      let full = Filename.concat dir name in
      let r = if rel = "" then name else Filename.concat rel name in
      if (try Sys.is_directory full with Sys_error _ -> false)
      then collect ~root ~rel:r acc
      else if is_source name then r :: acc
      else acc
  ) acc (List.sort compare entries)

let collect_files dir =
  let files = collect ~root:dir ~rel:"" [] in
  List.sort (fun a b ->
    let pa = dir_priority a and pb = dir_priority b in
    if pa <> pb then compare pa pb else compare a b
  ) files

(* Escape LaTeX special chars for use in \section{}, \markboth{}, headers.
   Paths contain _, ., /, alphanumerics — only _ needs escaping,
   but we escape the full set to be safe. *)
let escape_text s =
  let buf = Buffer.create (String.length s + 4) in
  String.iter (fun c ->
    match c with
    | '\\' -> Buffer.add_string buf "\\textbackslash{}"
    | '_' | '&' | '%' | '#' | '$' | '^' | '~' | '{' | '}' ->
        Buffer.add_char buf '\\'; Buffer.add_char buf c
    | _ -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let preamble () =
  "\\documentclass[10pt,oneside]{article}\n" ^
  "\\usepackage[T1]{fontenc}\n" ^
  "\\usepackage{lmodern}\n" ^
  "\\usepackage{listings}\n" ^
  "\\usepackage{xcolor}\n" ^
  "\\usepackage[a4paper, left=2cm, right=7cm, top=2.5cm, bottom=2.5cm, marginparwidth=5.5cm, marginparsep=8pt]{geometry}\n" ^
  "\\usepackage{fancyhdr}\n" ^
  "\\pagestyle{fancy}\n" ^
  "\\fancyhf{}\n" ^
  "\\fancyhead[L]{borge codebook}\n" ^
  (* \leftmark = the first mark set on the page. Each file starts with
     \clearpage + \markboth{file}{file}, so \leftmark is the current
     file with no lag. (\rightmark lags by one section by design.) *)
  "\\fancyhead[R]{\\leftmark}\n" ^
  "\\fancyfoot[C]{\\thepage}\n" ^
  "\\renewcommand{\\headrulewidth}{0.4pt}\n" ^
  "\\lstset{basicstyle=\\ttfamily\\footnotesize, numbers=left, numberstyle=\\tiny\\color{gray}, stepnumber=1, firstnumber=1, frame=single, rulecolor=\\color{gray!50}, breaklines=true, breakatwhitespace=true, showstringspaces=false, tabsize=2, xleftmargin=2em, numbersep=10pt, columns=fullflexible, keepspaces=true}\n"

(* Sanitize UTF-8 to ASCII for pdflatex+listings, which is byte-oriented.
   Maps common punctuation to ASCII equivalents, falls back to '?' for the
   rest. The book is for reading code; ASCII-fied punctuation in comments
   is fine, and this keeps the output deterministic. *)
let sanitize content =
  let buf = Buffer.create (String.length content) in
  let i = ref 0 in
  let n = String.length content in
  while !i < n do
    let c = Char.code content.[!i] in
    if c < 128 then begin
      Buffer.add_char buf content.[!i];
      incr i
    end else begin
      let len =
        if c land 0xE0 = 0xC0 then 2
        else if c land 0xF0 = 0xE0 then 3
        else if c land 0xF8 = 0xF0 then 4
        else 1
      in
      let seq =
        if !i + len <= n then String.sub content !i len
        else String.sub content !i (n - !i)
      in
      (match seq with
       | "\xe2\x80\x94" -> Buffer.add_string buf "--"   (* em dash   *)
       | "\xe2\x80\x93" -> Buffer.add_char buf '-'       (* en dash   *)
       | "\xe2\x80\x99" | "\xe2\x80\x98" -> Buffer.add_char buf '\''
       | "\xe2\x80\x9c" | "\xe2\x80\x9d" -> Buffer.add_char buf '"'
       | "\xc2\xa0" -> Buffer.add_char buf ' '           (* nbsp      *)
       | _ -> Buffer.add_char buf '?');
      i := !i + len
    end
  done;
  Buffer.contents buf

let render_to_tex ~dir ~files ~tex_path =
  let src_dir = Filename.concat (Filename.dirname tex_path) "src" in
  (try Unix.mkdir src_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let oc = open_out tex_path in
  output_string oc (preamble ());
  output_string oc "\\begin{document}\n";
  output_string oc "\\tableofcontents\n";
  List.iteri (fun i f ->
    let escaped = escape_text f in
    let sanitized_path = Filename.concat src_dir (Printf.sprintf "%04d.ml" i) in
    let full = Filename.concat dir f in
    let content = sanitize (try File_utils.read_file full with Sys_error _ -> "") in
    let soc = open_out sanitized_path in
    output_string soc content;
    close_out soc;
    (* \markboth sets leftmark+rightmark to the filename. Combined with
       \clearpage per file, \leftmark in the header tracks the current
       file across continuation pages with no lag. *)
    Printf.fprintf oc "\\clearpage\n\\section{%s}\n\\label{file:%s}\n\\markboth{%s}{%s}\n\\lstinputlisting[firstnumber=1]{%s}\n"
      escaped f escaped escaped sanitized_path
  ) files;
  output_string oc "\\end{document}\n";
  close_out oc

let has_cmd name =
  Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" name) = 0

let file_hash path =
  try
    let content = File_utils.read_file path in
    Digest.to_hex (Digest.string content)
  with Sys_error _ -> ""

(* Run pdflatex until the .aux and .toc hashes stop changing, max 5
   passes. A multi-page TOC grows across passes: pass 1 has no .toc
   (TOC renders 1 page, page numbers wrong), pass 2 renders the full
   TOC (pushing content down), pass 3 displays + records the corrected
   page numbers. Looping on the .aux+.toc hash catches the stabilization
   the way latexmk does. *)
let run_pdflatex ~tmpdir ~tex =
  let cmd =
    Printf.sprintf "pdflatex -interaction=nonstopmode -halt-on-error -output-directory=%s %s >/dev/null 2>&1"
      (Filename.quote tmpdir) (Filename.quote tex)
  in
  let aux = Filename.concat tmpdir "book.aux" in
  let toc = Filename.concat tmpdir "book.toc" in
  let rec loop ~prev_hash ~passes =
    let _ = Sys.command cmd in
    let h = file_hash aux ^ ":" ^ file_hash toc in
    if passes >= 5 then ()
    else if h = prev_hash then ()
    else loop ~prev_hash:h ~passes:(passes + 1)
  in
  loop ~prev_hash:"" ~passes:1

(* Parse book.aux for \newlabel{file:PATH}{{sec}{PAGE}{...}{...}{}} entries.
   Returns (path, start_page) for every labeled file. *)
let aux_parse ~aux =
  let s = (try File_utils.read_file aux with Sys_error _ -> "") in
  let re = Str.regexp "\\\\newlabel{file:\\([^}]+\\)}{{[^}]*}{\\([0-9]+\\)}" in
  let rec loop start acc =
    try
      let _ = Str.search_forward re s start in
      let path = Str.matched_group 1 s in
      let page = int_of_string (Str.matched_group 2 s) in
      loop (Str.match_end ()) ((path, page) :: acc)
    with Not_found -> List.rev acc
  in
  loop 0 []

let copy_file ~src ~dst =
  let ic = open_in src in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  let oc = open_out dst in
  output_bytes oc buf;
  close_out oc

let rm_rf dir =
  let rec walk d =
    List.iter (fun name ->
      let full = Filename.concat d name in
      if Sys.is_directory full then walk full
      else Sys.remove full
    ) (Sys.readdir d |> Array.to_list)
  in
  try walk dir; Unix.rmdir dir with _ -> ()

(* Returns the number of files printed. On failure, keeps tmpdir
   and prints the log path so the user can diagnose. *)
let print ~dir ~stem =
  if not (has_cmd "pdflatex") then begin
    if has_cmd "groff" then
      failwith "groff backend not implemented in v1 (install pdflatex/texlive)"
    else
      failwith "no PDF backend found: need pdflatex on PATH"
  end;
  let files = collect_files dir in
  if files = [] then
    failwith "no source files found";
  let tmpdir =
    let rec try_n n =
      if n > 50 then failwith "could not create temp dir";
      let name = Printf.sprintf "/tmp/borgebook%d_%d" (Unix.getpid ()) n in
      (try Unix.mkdir name 0o700; name
       with Unix.Unix_error (Unix.EEXIST, _, _) -> try_n (n + 1))
    in
    try_n 0
  in
  let tex = Filename.concat tmpdir "book.tex" in
  try
    render_to_tex ~dir ~files ~tex_path:tex;
    run_pdflatex ~tmpdir ~tex;
    let aux = Filename.concat tmpdir "book.aux" in
    let page_map = aux_parse ~aux in
    let file_pages = List.map (fun f ->
      { Book_manifest.file = f;
        start_page = (try List.assoc f page_map with Not_found -> 0) }
    ) files in
    let checksum = Book_manifest.tree_checksum ~dir files in
    let manifest = Book_manifest.build ~stem ~backend:"latex-listings"
      ~tree_checksum:checksum ~files ~file_pages in
    let pdf = Filename.concat tmpdir "book.pdf" in
    if not (Sys.file_exists pdf) then
      failwith (Printf.sprintf "pdflatex produced no pdf; see %s/book.log" tmpdir);
    copy_file ~src:pdf ~dst:(stem ^ ".pdf");
    Book_manifest.write ~path:(stem ^ ".book.manifest") manifest;
    rm_rf tmpdir;
    List.length files
  with e ->
    prerr_endline (Printf.sprintf "borge book: pdflatex failed; inspect %s/book.log" tmpdir);
    raise e
