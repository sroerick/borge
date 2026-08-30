(* agent note (|
 *   WHAT: v2 codebook printer. Chapters follow the root project's
 *   (inline ...) tree order (the spec IS the book); unreferenced
 *   files append lexicographically as the appendix. --root FILE
 *   prints exactly the inline subtree rooted at one file (course
 *   roots, single chapters). Emits a LaTeX document with the
 *   listings package (line numbers, framed code, a wide right-hand
 *   margin for hand annotations), runs pdflatex until the .aux/.toc
 *   stabilize (a multi-page TOC grows across passes), and writes a
 *   manifest sidecar via Book_manifest.
 *   WHY: The "print" half of the book round trip (borge.borg
 *   section book; pricklypear borge/habitat-book.borg
 *   export-projection). One walk shared with book export and the
 *   habitat loader; no readdir order survives.
 *   MODES: register is a render mode, not a document fork
 *   (habitat-book decision 1). Workshop (default) shows the full
 *   workflow scaffolding: status labels, verify blocks, agent
 *   notes, untracked lines. Reader strips that scaffolding for
 *   students — prose, headings, and code only. Drift holes stay
 *   visible in both: a file that fails to parse falls back to a
 *   raw listing in either register.
 *   DETERMINISM: pdflatex runs with cwd = the work dir and cites the
 *   sanitized listings by relative path (no PID-bearing tmpdir leaks
 *   into the .tex), and SOURCE_DATE_EPOCH/FORCE_SOURCE_DATE pin the
 *   PDF timestamps + /ID to a fixed epoch. Two runs of the same tree
 *   produce byte-identical PDFs; wall clock survives in exactly one
 *   artifact, the manifest's rendered_at.
 * |) *)

(* Register lives in Book_structure (applied once, shared with book
   export — habitat-book decision 3); this alias keeps the CLI and
   tests spelled Book_print.Workshop/Reader. *)
type mode = Book_structure.mode = Workshop | Reader

(* Example fences (P1.6): nopales gets a defined listings language
   (below); ocaml maps to the listings built-in as its [Objective]
   Caml dialect (plain Caml is the light dialect — wrong keywords).
   The value is brace-protected: listings' optional-argument scanner
   does not balance nested [..] on its own. *)
let example_latex_lang = function
  | "ocaml" -> "{[Objective]Caml}"
  | l -> l

let skip_dirs =
  ["_build"; ".git"; ".pi"; ".ralph"; ".borge-bugs"; ".borge.lock";
   "_build_test"; "node_modules"; "_opam"; ".opamswitch";
   (* pp-sync snapshot vaults (pricklypear backups/): gitignored
      branch dumps, not source — printing them would add hundreds of
      stale chapters to the appendix. *)
   "backups";
   (* Course composition roots (pricklypear borge/habitat-book.borg
      course-documents): course/*.borg files are chapter SELECTIONS
      consumed via --root, never chapters of the canonical book.
      Without this, a new course root would become a second walk root
      (they sort before pricklypear.borg) and silently reorganize the
      spec's inline TOC. *)
   "course"]

let is_source name =
  (* .borg yes; .borg.meta no — that's machine-generated, not the spec *)
  (Filename.check_suffix name ".borg"
   && not (Filename.check_suffix name ".borg.meta"))
  || Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mli"
  (* Type coverage (habitat-book): .pp is the Nopales product layer,
     .sql is the migrations — both print as code chapters. *)
  || Filename.check_suffix name ".pp"
  || Filename.check_suffix name ".sql"

(* Canonical dir-relative path form: absolutize against the CWD, strip
   the canonical dir prefix, drop "." components. The readdir walk and
   Project.build_tree both normalize to this form, so chapter membership
   is an exact string compare however dir was spelled (".", "./",
   relative, absolute, trailing slash or not). *)
let canon_path ~dir p =
  (* Fold "." and ".." components: course composition roots reference
     chapters as ../borge/x.borg (inline targets resolve relative to
     the parent file's dir), and the book must dedup + label them
     exactly like the root-written borge/x.borg spelling. ".." above
     the top of a relative path is kept (same as pre-fold behavior for
     such paths). *)
  let fold_dots parts =
    let rec go acc = function
      | [] -> List.rev acc
      | "." :: rest -> go acc rest
      | ".." :: rest ->
        (match acc with
         | [] | [ "" ] -> go (".." :: acc) rest
         | _ :: tl -> go tl rest)
      | c :: rest -> go (c :: acc) rest
    in
    go [] parts
  in
  let drop_dots p =
    let is_abs = String.length p > 0 && p.[0] = '/' in
    let parts =
      fold_dots (String.split_on_char '/' p)
    in
    String.concat "/" ((if is_abs then [""] else []) @ parts)
  in
  let abs p =
    if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p
  in
  let dir_abs = drop_dots (abs dir) in
  let p_abs = drop_dots (abs p) in
  let prefix = dir_abs ^ "/" in
  let n = String.length prefix in
  if String.length p_abs >= n && String.sub p_abs 0 n = prefix then
    String.sub p_abs n (String.length p_abs - n)
  else p_abs

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

let tree_error_msg = function
  | Project.File_not_found p -> "file not found: " ^ p
  | Project.Parse_error (p, m) -> Printf.sprintf "%s: %s" p m
  | Project.Cycle_detected cyc -> String.concat " -> " cyc

(* True when a canon dir-relative path lives under a course
   composition directory (see skip_dirs). *)
let is_composition_root rel =
  match String.index_opt rel '/' with
  | Some i -> String.sub rel 0 i = "course"
  | None -> false

(* The default walk's roots: candidate roots minus course composition
   roots, lexicographic. Shared with book_export's walk_root so the
   manifest's root field names the same canonical book the chapters
   came from (one walk, two projections). *)
let default_roots ~dir =
  Project.find_roots dir
  |> List.map (canon_path ~dir)
  |> List.filter (fun p -> not (is_composition_root p))
  |> List.sort_uniq compare

(* Inline-aware ordered walk (spec: pricklypear borge/habitat-book.borg
   export-projection; borge self-spec docs/book.borg book-print):

   1. The root project's inline tree defines chapter order, depth-first:
      each root .borg first, then each (inline ...) target in written
      order. Multiple roots (rare) order lexicographically; a file
      inlined twice keeps its first position.
   2. Every source file the tree does not reference — .ml/.mli code
      and stray .borg alike — sorts lexicographically AFTER the tree
      (the appendix). There is no readdir order anywhere: two fresh
      clones print the same book.

   A root whose inline tree fails to build (missing target, cycle,
   parse error) degrades to a lone chapter with a stderr warning; the
   book still renders, deterministically. *)
let collect_files dir =
  let all = collect ~root:dir ~rel:"" [] in
  let roots = default_roots ~dir in
  let chapters =
    List.concat_map (fun root_rel ->
      match Project.build_tree (Filename.concat dir root_rel) with
      | Ok tree -> List.map (canon_path ~dir) (Project.tree_paths tree)
      | Error err ->
        prerr_endline
          (Printf.sprintf
             "borge book: inline tree error at %s (%s); printing it as a lone chapter"
             root_rel (tree_error_msg err));
        [root_rel]
    ) roots
  in
  let seen = Hashtbl.create 64 in
  let chapters =
    List.filter (fun p ->
      if Hashtbl.mem seen p then false
      else (Hashtbl.add seen p (); true)) chapters
  in
  let appendix =
    List.filter (fun p -> not (Hashtbl.mem seen p)) all |> List.sort compare
  in
  chapters @ appendix

(* --root FILE resolution: relative to dir first, then to the CWD. *)
let resolve_root_file ~dir ~root_file =
  if Filename.is_relative root_file
     && Sys.file_exists (Filename.concat dir root_file)
  then Filename.concat dir root_file
  else root_file

(* Explicit-root walk (--root FILE): the book is exactly the inline
   subtree rooted at FILE — course roots, single-chapter prints.
   Nothing outside the subtree prints: no appendix, no sibling roots.
   Unlike the default walk (which degrades to a lone chapter on a
   broken tree so the whole book still renders), an explicit selection
   is a contract: a missing file or an unbuildable tree is a hard
   error. FILE resolves relative to dir first, then to the CWD. *)
let collect_subtree ~dir ~root_file =
  let root_abs = resolve_root_file ~dir ~root_file in
  if not (Sys.file_exists root_abs) then
    failwith (Printf.sprintf "--root: no such file: %s" root_file);
  match Project.build_tree root_abs with
  | Ok tree -> List.map (canon_path ~dir) (Project.tree_paths tree)
  | Error err ->
    failwith
      (Printf.sprintf "--root %s: inline tree error (%s)"
         root_file (tree_error_msg err))

(* Escape LaTeX special chars for use in \section{}, \markboth{}, headers.
   Paths contain _, ., /, alphanumerics — only _ needs escaping,
   but we escape the full set to be safe. *)
let escape_text s =
  let buf = Buffer.create (String.length s + 4) in
  String.iter (fun c ->
    match c with
    | '\\' -> Buffer.add_string buf "\\textbackslash{}"
    | '^' -> Buffer.add_string buf "\\textasciicircum{}"
    | '~' -> Buffer.add_string buf "\\textasciitilde{}"
    | '_' | '&' | '%' | '#' | '$' | '{' | '}' ->
        Buffer.add_char buf '\\'; Buffer.add_char buf c
    | _ -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let preamble ~mode =
  let head =
    match mode with
    | Workshop -> "borge codebook"
    | Reader -> "borge codebook (reader)"
  in
  "\\documentclass[10pt,oneside]{article}\n" ^
  "\\usepackage[T1]{fontenc}\n" ^
  "\\usepackage{lmodern}\n" ^
  "\\usepackage{listings}\n" ^
  "\\usepackage{xcolor}\n" ^
  "\\usepackage[a4paper, left=2cm, right=7cm, top=2.5cm, bottom=2.5cm, marginparwidth=5.5cm, marginparsep=8pt]{geometry}\n" ^
  "\\usepackage{fancyhdr}\n" ^
  "\\pagestyle{fancy}\n" ^
  "\\fancyhf{}\n" ^
  Printf.sprintf "\\fancyhead[L]{%s}\n" (escape_text head) ^
  (* \leftmark = the first mark set on the page. Each file starts with
     \clearpage + \markboth{file}{file}, so \leftmark is the current
     file with no lag. (\rightmark lags by one section by design.) *)
  "\\fancyhead[R]{\\leftmark}\n" ^
  "\\fancyfoot[C]{\\thepage}\n" ^
  "\\renewcommand{\\headrulewidth}{0.4pt}\n" ^
  "\\lstset{basicstyle=\\ttfamily\\footnotesize, numbers=left, numberstyle=\\tiny\\color{gray}, stepnumber=1, firstnumber=1, frame=single, rulecolor=\\color{gray!50}, breaklines=true, breakatwhitespace=true, showstringspaces=false, tabsize=2, xleftmargin=2em, numbersep=10pt, columns=fullflexible, keepspaces=true}\n" ^
  "\\lstdefinelanguage{nopales}{morekeywords={define,lambda,let,if,else,cond,match,when,do,module,open,deftype,route,public-route,checkout,fork,merge,with-transaction,delay,force,load-library,whoami,current-author,is-admin?,create-user,get,create,update,delete,rows,type-of,str,list,car,cdr,cons,dict-get,dict-set,render-html,example-eval,book/verify},sensitive=false,morecomment=[l]{;},morestring=[b]\"}\n"

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

(* --- .borg -> LaTeX renderer (over the shared structure) ---
   Book_structure.of_file parses the sexp and applies the register
   filter ONCE, shared with book export (habitat-book decision 3:
   one parser, two projections); this pass only emits LaTeX from the
   node tree — headings, prose paragraphs, and the workshop
   scaffolding (status markers, verify scaffold lines, agent-note
   blockquotes, untracked notes). Inline notes render in BOTH modes
   (navigation, not scaffolding). Falls back to raw listing if the
   file fails to parse — in both modes: that fallback is the
   print-side drift hole. *)

(* Emit prose: split on blank lines into paragraphs, escape, emit. *)
let render_prose buf text =
  let paras = Str.split (Str.regexp "\n[ \t]*\n") text in
  List.iter (fun p ->
    let p = String.trim p in
    if p <> "" then begin
      Buffer.add_string buf (escape_text p);
      Buffer.add_string buf "\n\n"
    end
  ) paras

(* Example fence rendering: the listing (nopales = the defined
   language, ocaml = the listings built-in; no line numbers — these
   are snippets, not file chapters) plus the captured output — the
   `expect =>` trailer — as a small italic annotation. Both
   registers: examples are content, not scaffolding. The source is
   already ASCII-sanitized (sanitize runs before the parse). *)
let render_example buf (e : Book_structure.example) =
  Buffer.add_string buf
    (Printf.sprintf "\\begin{lstlisting}[language=%s, numbers=none]\n"
       (example_latex_lang e.Book_structure.lang));
  Buffer.add_string buf e.Book_structure.src;
  if e.Book_structure.src = ""
     || e.Book_structure.src.[String.length e.Book_structure.src - 1] <> '\n'
  then Buffer.add_char buf '\n';
  Buffer.add_string buf "\\end{lstlisting}\n\n";
  match e.Book_structure.expected with
  | Some v ->
    Buffer.add_string buf "\\textit{[expect => ";
    Buffer.add_string buf (escape_text v);
    Buffer.add_string buf "]}";
    Buffer.add_string buf "\\par\n\n"
  | None -> ()

(* Doc prose, segmented by the SHARED fence parser
   (Book_structure.fence_split — one fence parser serves both
   projections, decision 3): text -> paragraphs, example fences ->
   listings. Unparsed fences ride along inside Text and render as
   plain escaped prose. *)
let render_doc buf body =
  List.iter
    (function
      | Book_structure.Text t -> render_prose buf t
      | Book_structure.Code e -> render_example buf e)
    (Book_structure.fence_split body)

let rec render_node buf n =
  match n.Book_structure.kind with
  | "project" | "section" | "subsection" ->
    let cmd =
      match n.Book_structure.kind with
      | "project" -> "section*"
      | "section" -> "subsection*"
      | _ -> "subsubsection*"
    in
    Printf.bprintf buf "\\%s{%s}\n" cmd (escape_text n.Book_structure.title);
    List.iter (render_node buf) n.Book_structure.children
  | "doc" -> render_doc buf n.Book_structure.body
  | "status" ->
    Printf.bprintf buf "\\textit{[status: %s]}\\par\n"
      (escape_text n.Book_structure.body)
  | "inline" ->
    Printf.bprintf buf "\\textit{[inlines %s]}\\par\n"
      (escape_text n.Book_structure.body)
  | "verify" ->
    if n.Book_structure.body = "" then
      Buffer.add_string buf "\\textit{[verify]}\\par\n"
    else begin
      Buffer.add_string buf "\\begin{quote}\\small\\itshape\n";
      Printf.bprintf buf "[verify] %s\n" (escape_text n.Book_structure.body);
      Buffer.add_string buf "\\end{quote}\n\n"
    end
  | "agent-note" ->
    Buffer.add_string buf "\\begin{quote}\n";
    Printf.bprintf buf "\\textit{-- %s:} " (escape_text n.Book_structure.title);
    render_prose buf n.Book_structure.body;
    Buffer.add_string buf "\\end{quote}\n\n"
  | "untracked" ->
    if n.Book_structure.body = "" then
      Printf.bprintf buf "\\textit{[untracked %s]}\\par\n"
        (escape_text n.Book_structure.title)
    else
      Printf.bprintf buf "\\textit{[untracked %s: %s]}\\par\n"
        (escape_text n.Book_structure.title) (escape_text n.Book_structure.body)
  | _ -> ()

let borg_to_latex ~mode ~content =
  let body = Book_structure.of_file ~mode ~content in
  let buf = Buffer.create 4096 in
  List.iter (render_node buf) body.Book_structure.nodes;
  Buffer.contents buf

let render_to_tex ~mode ~dir ~files ~tex_path =
  let src_dir = Filename.concat (Filename.dirname tex_path) "src" in
  (try Unix.mkdir src_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (* Listings are cited by a path relative to tex_path's directory
     (pdflatex runs with cwd there): keeps the .tex byte-identical
     across runs regardless of the temp dir's name. *)
  let rel_listing i ext =
    Filename.concat "src" (Printf.sprintf "%04d.%s" i ext)
  in
  let oc = open_out tex_path in
  output_string oc (preamble ~mode);
  output_string oc "\\begin{document}\n";
  output_string oc "\\tableofcontents\n";
  List.iteri (fun i f ->
    let escaped = escape_text f in
    let full = Filename.concat dir f in
    Printf.fprintf oc "\\clearpage\n\\section{%s}\n\\label{file:%s}\n\\markboth{%s}{%s}\n"
      escaped f escaped escaped;
    if Filename.check_suffix f ".borg" then begin
      (* Render the .borg spec as prose (parsed), not raw sexp. Falls
         back to a raw listing if the file fails to parse. *)
      let raw = try File_utils.read_file full with Sys_error _ -> "" in
      let body =
        try Some (borg_to_latex ~mode ~content:(sanitize raw))
        with Book_structure.Fallback_raw -> None
      in
      match body with
      | Some b -> output_string oc b
      | None ->
        let sanitized_path = Filename.concat src_dir (Printf.sprintf "%04d.borg" i) in
        let soc = open_out sanitized_path in
        output_string soc (sanitize raw);
        close_out soc;
        Printf.fprintf oc "\\lstinputlisting[firstnumber=1]{%s}\n"
          (rel_listing i "borg")
    end else begin
      (* Code chapter (.ml/.mli/.pp/.sql): raw listing, cited under
         its own extension so the temp file stays honest. *)
      let ext =
        match Filename.extension f with
        | "" -> "ml"
        | e -> String.sub e 1 (String.length e - 1)
      in
      let sanitized_path = Filename.concat src_dir (Printf.sprintf "%04d.%s" i ext) in
      let content = sanitize (try File_utils.read_file full with Sys_error _ -> "") in
      let soc = open_out sanitized_path in
      output_string soc content;
      close_out soc;
      Printf.fprintf oc "\\lstinputlisting[firstnumber=1]{%s}\n"
        (rel_listing i ext)
    end
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
   the way latexmk does.

   Runs with cwd = tmpdir (the tex cites its listings relatively) and
   SOURCE_DATE_EPOCH/FORCE_SOURCE_DATE pinned so the PDF's creation
   date and /ID derive from a fixed epoch, not the wall clock —
   byte-identical PDFs across runs and clones. *)
let run_pdflatex ~tmpdir ~tex =
  let cmd =
    Printf.sprintf
      "cd %s && SOURCE_DATE_EPOCH=0 FORCE_SOURCE_DATE=1 pdflatex -interaction=nonstopmode -halt-on-error -output-directory=. %s >/dev/null 2>&1"
      (Filename.quote tmpdir) (Filename.quote (Filename.basename tex))
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
   and prints the log path so the user can diagnose. root selects an
   explicit inline subtree (None = the whole book). *)
let print ~dir ~stem ~root ~mode =
  if not (has_cmd "pdflatex") then begin
    if has_cmd "groff" then
      failwith "groff backend not implemented in v1 (install pdflatex/texlive)"
    else
      failwith "no PDF backend found: need pdflatex on PATH"
  end;
  let files =
    match root with
    | Some f -> collect_subtree ~dir ~root_file:f
    | None -> collect_files dir
  in
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
    render_to_tex ~mode ~dir ~files ~tex_path:tex;
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
