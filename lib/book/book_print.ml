(* agent note (|
 *   WHAT: v2 codebook printer. Chapters follow the root project's
 *   (inline ...) tree order (the spec IS the book); unreferenced
 *   files append lexicographically as the appendix. Emits a LaTeX
 *   document with the listings package (line numbers, framed code,
 *   a wide right-hand margin for hand annotations), runs pdflatex
 *   until the .aux/.toc stabilize (a multi-page TOC grows across
 *   passes), and writes a manifest sidecar via Book_manifest.
 *   WHY: The "print" half of the book round trip (borge.borg
 *   section book; pricklypear borge/habitat-book.borg
 *   export-projection). One walk shared with book export and the
 *   habitat loader; no readdir order survives.
 * |) *)

let skip_dirs =
  ["_build"; ".git"; ".pi"; ".ralph"; ".borge-bugs"; ".borge.lock";
   "_build_test"; "node_modules"; "_opam"; ".opamswitch"]

let is_source name =
  (* .borg yes; .borg.meta no — that's machine-generated, not the spec *)
  (Filename.check_suffix name ".borg"
   && not (Filename.check_suffix name ".borg.meta"))
  || Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mli"

(* Canonical dir-relative path form: absolutize against the CWD, strip
   the canonical dir prefix, drop "." components. The readdir walk and
   Project.build_tree both normalize to this form, so chapter membership
   is an exact string compare however dir was spelled (".", "./",
   relative, absolute, trailing slash or not). *)
let canon_path ~dir p =
  let drop_dots p =
    let is_abs = String.length p > 0 && p.[0] = '/' in
    let parts =
      List.filter (fun c -> c <> "" && c <> ".") (String.split_on_char '/' p)
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
  let roots =
    Project.find_roots dir
    |> List.map (canon_path ~dir)
    |> List.sort_uniq compare
  in
  let chapters =
    List.concat_map (fun root_rel ->
      match Project.build_tree (Filename.concat dir root_rel) with
      | Ok tree -> List.map (canon_path ~dir) (Project.tree_paths tree)
      | Error err ->
        let msg =
          match err with
          | Project.File_not_found p -> "file not found: " ^ p
          | Project.Parse_error (p, m) -> Printf.sprintf "%s: %s" p m
          | Project.Cycle_detected cyc -> String.concat " -> " cyc
        in
        prerr_endline
          (Printf.sprintf
             "borge book: inline tree error at %s (%s); printing it as a lone chapter"
             root_rel msg);
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

(* --- .borg -> LaTeX renderer (parses the sexp; no raw sexp in the PDF) ---
   section/subsection -> headings, (doc ...) -> prose paragraphs,
   (* author ... (|...|) *) comments -> attributed blockquotes,
   (status X) -> an italic marker. Other forms (inline, verify,
   depends-on, ...) are structural noise and are skipped. Falls back
   to raw listing if the file fails to parse. *)
exception Fallback_raw

let string_value_text = function
  | Borge_lang.Ast.Quoted q -> q.Borge_lang.Ast.q_content
  | Borge_lang.Ast.Verbatim v -> v.Borge_lang.Ast.v_content

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

let render_borg_comment buf c =
  match c with
  | Borge_lang.Ast.Plain _ -> ()  (* skip (; machine comments *)
  | Borge_lang.Ast.Annotated ac ->
    let author =
      match ac.Borge_lang.Ast.authorship with
      | Borge_lang.Ast.Single a -> a
      | Borge_lang.Ast.Multiple xs -> String.concat "," xs
    in
    let typ = match ac.Borge_lang.Ast.comment_type with
      | Borge_lang.Ast.Untyped -> ""
      | Borge_lang.Ast.Typed t -> " " ^ t
    in
    let body =
      match ac.Borge_lang.Ast.value with
      | Some v -> String.trim (string_value_text v)
      | None -> ""
    in
    if body <> "" then begin
      Buffer.add_string buf "\\begin{quote}\n";
      Printf.bprintf buf "\\textit{-- %s%s:} " (escape_text author) (escape_text typ);
      render_prose buf body;
      Buffer.add_string buf "\\end{quote}\n\n"
    end

let rec render_borg_node buf depth pending node =
  let open Borge_lang.Ast in
  let drain limit =
    let rec loop () =
      match !pending with
      | (cpos, c) :: rest when cpos.offset < limit ->
          render_borg_comment buf c;
          pending := rest;
          loop ()
      | _ -> ()
    in
    loop ()
  in
  match node with
  | Atom _ | String _ -> ()
  | List (_pos, children) as lst ->
    let head_is s = match children with Atom (_, x) :: _ -> x = s | _ -> false in
    let rest = match children with _ :: r -> r | [] -> [] in
    if head_is "project" || head_is "section" || head_is "subsection" then begin
      let name = match rest with Atom (_, n) :: _ -> n | _ -> "" in
      let cmd =
        if head_is "project" then "section*"
        else if head_is "section" then "subsection*"
        else "subsubsection*"
      in
      (* Drain nested comments that appear after the keyword atom but
         before the name atom (or first inner child). *)
      (match rest with
       | Atom (p, _) :: _ -> drain p.offset
       | child :: _ -> drain (start_pos_of child).offset
       | [] -> drain (end_pos_of lst).offset);
      Printf.bprintf buf "\\%s{%s}\n" cmd (escape_text name);
      let inner = match rest with _ :: r -> r | _ -> [] in
      List.iter (fun child ->
        drain (start_pos_of child).offset;
        render_borg_node buf (depth + 1) pending child
      ) inner;
      drain (end_pos_of lst).offset
    end
    else if head_is "doc" then begin
      match rest with String (_, v) :: _ -> render_prose buf (string_value_text v) | _ -> ()
    end
    else if head_is "details" then begin
      List.iter (fun child ->
        drain (start_pos_of child).offset;
        render_borg_node buf depth pending child
      ) rest;
      drain (end_pos_of lst).offset
    end
    else if head_is "status" then begin
      match rest with Atom (_, s) :: _ ->
        Printf.bprintf buf "\\textit{[status: %s]}\\par\n" (escape_text s)
      | _ -> ()
    end
    else if head_is "inline" then begin
      match rest with String (_, v) :: _ ->
        Printf.bprintf buf "\\textit{[inlines %s]}\\par\n" (escape_text (string_value_text v))
      | _ -> ()
    end
    else ()   (* skip structural forms: verify, depends-on, convention, ... *)

let borg_to_latex ~content =
  let open Borge_lang.Ast in
  let file =
    try Borge_lang.Parse.parse_file content
    with _ -> raise Fallback_raw
  in
  let buf = Buffer.create 4096 in
  let sorted_nested =
    List.stable_sort (fun (p1, _) (p2, _) -> compare p1.offset p2.offset) file.nested_comments
  in
  let pending = ref sorted_nested in
  List.iter (render_borg_comment buf) file.Borge_lang.Ast.top_level_comments;
  List.iter (fun swc ->
    List.iter (render_borg_comment buf) swc.Borge_lang.Ast.comments_before;
    render_borg_node buf 0 pending swc.Borge_lang.Ast.node
  ) file.Borge_lang.Ast.top_level;
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
    let full = Filename.concat dir f in
    Printf.fprintf oc "\\clearpage\n\\section{%s}\n\\label{file:%s}\n\\markboth{%s}{%s}\n"
      escaped f escaped escaped;
    if Filename.check_suffix f ".borg" then begin
      (* Render the .borg spec as prose (parsed), not raw sexp. Falls
         back to a raw listing if the file fails to parse. *)
      let raw = try File_utils.read_file full with Sys_error _ -> "" in
      let body =
        try Some (borg_to_latex ~content:(sanitize raw))
        with Fallback_raw -> None
      in
      match body with
      | Some b -> output_string oc b
      | None ->
        let sanitized_path = Filename.concat src_dir (Printf.sprintf "%04d.borg" i) in
        let soc = open_out sanitized_path in
        output_string soc (sanitize raw);
        close_out soc;
        Printf.fprintf oc "\\lstinputlisting[firstnumber=1]{%s}\n" sanitized_path
    end else begin
      let sanitized_path = Filename.concat src_dir (Printf.sprintf "%04d.ml" i) in
      let content = sanitize (try File_utils.read_file full with Sys_error _ -> "") in
      let soc = open_out sanitized_path in
      output_string soc content;
      close_out soc;
      Printf.fprintf oc "\\lstinputlisting[firstnumber=1]{%s}\n" sanitized_path
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
