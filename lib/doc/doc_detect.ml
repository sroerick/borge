(* Detect documentation comments and exempt markers in OCaml code
 *
 * agent note (|
 *   WHAT: Identifies documentation formats, exempt markers, and drift
 *   status markers preceding let bindings in OCaml source files.
 *   Supports docstrings, borg-annotated comments, short borg comments,
 *   exempt markers, and (status drifted) detection.
 *
 *   WHY: Downstream tools (doc_coverage, lint) need to know whether
 *   a binding is documented, exempt, or has a known-stale comment.
 *   The drifted status lets us distinguish between "no comment" and
 *   "comment exists but is known to be inaccurate."
 * |) *)

(** Literacy score for a comment: Story, Explain, Teach, Edge.
    Each digit 0--7. Zeros are honest defaults. *)
type literacy_score = {
  story : int;
  explain : int;
  teach : int;
  edge : int;
}

(** Types of documentation found *)
type doc_kind =
  | Docstring of string              (** (** ... *) *)
  | Borg_note of string * string * literacy_score option  (** author, content, score *)
  | Borg_short of string             (** (*| ... |*) *)
  | Exempt_marker                    (** (* exempt doc *) *)

(** Where the doc was found relative to binding *)
type doc_position =
  | Preceding of int        (** N lines before binding *)
  | Inline                  (** Same line as binding - rare *)
  | None_found

(* agent note [5300] (|
 *   WHAT: Check whether a comment string contains a (status drifted) marker.
 *   Scans the entire text for the literal substring "(status drifted)".
 *
 *   WHY: The (status drifted) marker signals that a comment is known
 *   to be out of date with its function's implementation. This is
 *   better than silently inaccurate documentation — it makes the
 *   lie explicit and actionable.
 * |) *)
let has_drifted_marker text =
  let rec loop i =
    if i + 15 > String.length text then false
    else if (* exempt: String.sub *) String.sub text i 15 = "(status drifted)" then true
    else loop (i + 1)
  in
  loop 0

(* agent note (|
 *   WHAT: Check whether any doc_kind variant contains a drifted marker.
 *
 *   WHY: When extracting docs, we need to know if the comment is
 *   drifted so we can classify it separately in coverage reports —
 *   drifted comments count as documented but are flagged for update.
 * |) *)
let doc_kind_is_drifted = function
  | Docstring content -> has_drifted_marker content
  | Borg_note (_, content, _) -> has_drifted_marker content
  | Borg_short content -> has_drifted_marker content
  | Exempt_marker -> false

(* agent note (|
 *   WHAT: Parse a 4-digit literacy score [STEX] from a string.
 *   Each digit 0--7 representing Story, Explain, Teach, Edge.
 *   Returns None if no valid score pattern found.
 * |) *)
let parse_literacy_score s =
  try
    let open_idx = String.index s '[' in
    let close_idx = String.index_from s (open_idx + 1) ']' in
    let digits = String.sub s (open_idx + 1) (close_idx - open_idx - 1) in
    if String.length digits = 4 then
      let is_digit c = c >= '0' && c <= '9' in
      if is_digit digits.[0] && is_digit digits.[1] &&
         is_digit digits.[2] && is_digit digits.[3] then
        Some {
          story = Char.code digits.[0] - Char.code '0';
          explain = Char.code digits.[1] - Char.code '0';
          teach = Char.code digits.[2] - Char.code '0';
          edge = Char.code digits.[3] - Char.code '0';
        }
      else None
    else None
  with _ -> None

(** Documentation info for a binding *)
type binding_doc = {
  binding_line : int;
  doc : (doc_kind * doc_position) option;
  is_drifted : bool;  (** True when comment has (status drifted) marker *)
}

(* agent note (|
 *   WHAT: Check if a line starts a standard OCaml docstring: open-paren-star-star.
 *   Excludes three-star which is a documentation-comment escape.
 *
 *   WHY: Standard docstrings are the most common OCaml documentation
 *   format and must be recognized to calculate accurate coverage.
 * |) *)
let is_docstring_start line =
  String.starts_with ~prefix:"(**" (String.trim line) &&
  not (String.starts_with ~prefix:"(***)" (String.trim line))

(* agent note (|
 *   WHAT: Check if a line is a borg-annotated comment in the form
 *   open-paren-star author note verbatim-content close-paren-star.
 *   Validates the structure: open-paren-star then author name then
 *   "note" then content.
 *
 *   WHY: Borg-annotated comments are the preferred documentation format
 *   in borge projects because they carry authorship and can be integrity-checked.
 * |) *)
let is_borg_note line =
  let trimmed = String.trim line in
  if String.length trimmed < 4 then false
  else if String.sub trimmed 0 2 = "(*" then
    let inner = String.sub trimmed 2 (String.length trimmed - 2) in
    let inner = String.trim inner in
    (try
      let space = String.index inner ' ' in
      let word = (* exempt: String.sub *) String.sub inner 0 space in
      let rest = String.sub inner space (String.length inner - space) |> String.trim in
      String.length word > 0 &&
      word.[0] >= 'a' && word.[0] <= 'z' &&  (* lowercase name *)
      String.starts_with ~prefix:"note " rest
    with _ -> false)
  else false

(* agent note (|
 *   WHAT: Check if a line is a short borg comment using the
 *   open-paren-pipe / close-pipe-paren format.
 *
 *   WHY: Short borg comments provide a lighter-weight alternative
 *   to full borg-annotated comments when authorship tracking isn't needed.
 * |) *)
let is_borg_short line =
  let trimmed = String.trim line in
  String.starts_with ~prefix:"(*|" trimmed

(* agent note (|
 *   WHAT: Check if a line is an exemption marker using the
 *   exempt-doc comment convention.
 *
 *   WHY: Some bindings are internal helpers whose purpose is obvious
 *   from their call site. The exemption marker lets the author
 *   explicitly opt out of documentation enforcement.
 * |) *)
let is_exempt_marker line =
  let trimmed = String.trim line in
  if String.length trimmed >= 4 &&
     String.sub trimmed 0 2 = "(*" &&
     String.sub trimmed (String.length trimmed - 2) 2 = "*)" then
    let inner = String.sub trimmed 2 (String.length trimmed - 4) |> String.trim in
    inner = "exempt" ||
    inner = "exempt doc" ||
    String.starts_with ~prefix:"exempt doc " inner ||
    String.starts_with ~prefix:"exempt doc:" inner
  else false

(* agent note (|
 *   WHAT: Strip the (* and *) delimiters from a comment line,
 *   returning the inner content.
 *
 *   WHY: Used by parse_borg_note and parse_doc_line to extract
 *   the meaningful content from within comment delimiters.
 * |) *)
let extract_comment_content line =
  let trimmed = String.trim line in
  if String.length trimmed < 4 then None
  else
    let inner = String.sub trimmed 2 (String.length trimmed - 4) |> String.trim in
    Some inner

(* agent note (|
 *   WHAT: Parse a borg-annotated comment line to extract author and
 *   content. Handles the borg-note format with author and
 *   verbatim-wrapped content,
 *   stripping the (|...|) wrapper if present.
 *
 *   WHY: Borg-annotated comments carry structured authorship data
 *   that the linter uses for integrity checks. Parsing out the
 *   author and content separately enables both lint and review.
 * |) *)
let parse_borg_note line =
  match extract_comment_content line with
  | None -> None
  | Some inner ->
      try
        let space = String.index inner ' ' in
        let author = (* exempt: String.sub *) String.sub inner 0 space in
        let rest = String.sub inner (space + 1) (String.length inner - space - 1) |> String.trim in
        if String.starts_with ~prefix:"note " rest then
          let note_raw = String.sub rest 5 (String.length rest - 5) |> String.trim in
          let score = parse_literacy_score note_raw in
          let note_after_score =
            match score with
            | Some _ ->
                (try
                  let close_idx = String.index note_raw ']' in
                  String.sub note_raw (close_idx + 1) (String.length note_raw - close_idx - 1) |> String.trim
                with _ -> note_raw)
            | None -> note_raw
          in
          (* Remove surrounding (| ... |) if present *)
          let note_content =
            if String.starts_with ~prefix:"(|" note_after_score then
              let len = String.length note_after_score in
              if len >= 2 && String.sub note_after_score (len - 2) 2 = "|)" then
                String.sub note_after_score 2 (len - 4) |> String.trim
              else note_after_score
            else note_after_score
          in
          Some (author, note_content, score)
        else None
      with _ -> None

(* agent note (|
 *   WHAT: Classify a single line as one of the four doc kinds
 *   (Docstring, Borg_note, Borg_short, Exempt_marker) or return
 *   None if the line is not a documentation comment.
 *
 *   WHY: The doc detection pipeline needs to identify what kind
 *   of documentation precedes each binding so coverage and lint
 *   can apply the right rules.
 * |) *)
let parse_doc_line line =
  if is_exempt_marker line then Some Exempt_marker
  else if is_docstring_start line then
    extract_comment_content line |> Option.map (fun c -> Docstring c)
  else if is_borg_short line then
    let trimmed = String.trim line in
    let inner = String.sub trimmed 3 (String.length trimmed - 5) |> String.trim in
    Some (Borg_short inner)
  else if is_borg_note line then
    parse_borg_note line |> Option.map (fun (a, c, s) -> Borg_note (a, c, s))
  else None

(* agent note (|
 *   WHAT: Find documentation for a binding at a specific line number
 *   by scanning backward from the binding line through preceding lines.
 *
 *   WHY: Used for targeted lookups when we know a binding's line
 *   number and just need its associated doc comment.
 * |) *)
let find_binding_doc lines binding_line =
  let rec scan prev_lines remaining prev_line_no =
    match remaining with
    | [] -> None_found
    | line :: rest ->
        let line_no = prev_line_no + 1 in
        if line_no = binding_line then
          (* This is the binding line - check for preceding doc *)
          let rec find_preceding = function
            | [] -> None_found
            | (doc_line, doc_content) :: prev ->
                match parse_doc_line doc_content with
                | Some _doc_kind -> Preceding (binding_line - doc_line)
                | None -> find_preceding prev
          in
          find_preceding prev_lines
        else
          scan ((line_no, line) :: prev_lines) rest line_no
  in
  scan [] lines 0

(* agent note (|
 *   WHAT: Extract all binding docs from a .ml file by scanning
 *   for let bindings and their preceding doc comments. Handles
 *   both single-line and multi-line comments by tracking comment
 *   open/close state. For each binding found, records whether
 *   its comment has drifted status.
 *
 *   WHY: The primary entry point for doc detection. Returns one
 *   binding_doc per binding with the closest preceding doc comment,
 *   used by Doc_coverage for coverage calculation and by Lint for
 *   code-doc checks.
 * |) *)
let extract_file_docs path =
  try
    let lines = File_utils.read_lines path in
    
    (* Find all let bindings and their docs.
       We track multi-line comment state: when we encounter a (*
       that doesn't close on the same line, we note whether it
       started as a doc comment. When *) closes it, we treat the
       whole block as a doc line at the position of the closing paren-star. *)
    let rec scan line_no prev_docs acc in_ml_comment ml_comment_is_doc ml_comment_first_line = function
      | [] -> List.rev acc
      | line :: rest ->
          let line_no = line_no + 1 in
          let trimmed = String.trim line in
          
          if in_ml_comment then begin
            (* Inside a multi-line comment: look for closing delimiter *)
            let closes_here = String.length trimmed >= 2 &&
              String.sub trimmed (String.length trimmed - 2) 2 = "*)" in
            if closes_here then
              (* End of multi-line comment — record it as a doc line
                 if it started as a doc comment *)
              let prev_docs' =
                if ml_comment_is_doc then
                  let marker = match ml_comment_first_line with
                    | None -> "doc-comment"
                    | Some first_line ->
                        let score_opt = parse_literacy_score first_line in
                        (match score_opt with
                         | None -> "doc-comment"
                         | Some s -> Printf.sprintf "borg-scored:%d%d%d%d" s.story s.explain s.teach s.edge)
                  in
                  (line_no, marker) :: prev_docs
                else
                  (line_no, line) :: prev_docs
              in
              scan line_no prev_docs' acc false false None
                (if String.starts_with ~prefix:"let" (String.trim (String.sub trimmed 0 (String.length trimmed - 2))) then [line] else rest)
            else
              scan line_no prev_docs acc true ml_comment_is_doc ml_comment_first_line rest
          end
          
          else if String.starts_with ~prefix:"let" trimmed then
            if String.starts_with ~prefix:"let open " trimmed ||
               String.starts_with ~prefix:"let module " trimmed then
              scan line_no ((line_no, line) :: prev_docs) acc false false None rest
            else
              (* Found a binding — check for preceding doc *)
              let doc =
                let rec find_doc = function
                  | [] -> None
                  | (doc_line, doc_content) :: prev ->
                      (* "doc-comment" is our marker for a recognized
                         multi-line doc comment. "borg-scored:*" carries
                         a literacy score extracted from the first line. *)
                      if doc_content = "doc-comment" then
                        Some (Docstring "", Preceding (line_no - doc_line))
                      else if String.starts_with ~prefix:"borg-scored:" doc_content then
                        let digits = String.sub doc_content 12 4 in
                        let score = {
                          story = Char.code digits.[0] - Char.code '0';
                          explain = Char.code digits.[1] - Char.code '0';
                          teach = Char.code digits.[2] - Char.code '0';
                          edge = Char.code digits.[3] - Char.code '0';
                        } in
                        Some (Borg_note ("agent", "", Some score), Preceding (line_no - doc_line))
                      else
                        match parse_doc_line doc_content with
                        | Some doc_kind -> Some (doc_kind, Preceding (line_no - doc_line))
                        | None ->
                            let tc = String.trim doc_content in
                            if tc = "" then find_doc prev
                            else None
                in
                find_doc prev_docs
              in
              let is_drifted = match doc with
                | None -> false
                | Some (kind, _) -> doc_kind_is_drifted kind
              in
              scan line_no [] ({ binding_line = line_no; doc; is_drifted } :: acc) false false None rest
          else
            (* Check if this line starts a multi-line comment that
               doesn't close on the same line *)
            let starts_ml_doc =
              String.starts_with ~prefix:"(*" trimmed &&
              not (String.length trimmed >= 4 &&
                   String.sub trimmed (String.length trimmed - 2) 2 = "*)") &&
              (is_docstring_start trimmed || is_borg_note trimmed ||
               String.starts_with ~prefix:"(* agent note" trimmed ||
               String.starts_with ~prefix:"(* roerick note" trimmed ||
               String.starts_with ~prefix:"(* (fn" trimmed)
            in
            let starts_ml_nondoc =
              String.starts_with ~prefix:"(*" trimmed &&
              not (String.length trimmed >= 4 &&
                   String.sub trimmed (String.length trimmed - 2) 2 = "*)") &&
              not starts_ml_doc
            in
            if starts_ml_doc then
              let first_line = Some trimmed in
              scan line_no prev_docs acc true true first_line rest
            else if starts_ml_nondoc then
              scan line_no prev_docs acc true false None rest
            else
              scan line_no ((line_no, line) :: prev_docs) acc false false None rest
    in
    scan 0 [] [] false false None lines
  with e ->
    Printf.eprintf "Error extracting docs from %s: %s\n" path (Printexc.to_string e);
    []

(* agent note (|
 *   WHAT: Check if a binding has documentation (not exempt, not missing).
 *   Drifted comments still count as documented — the gap is acknowledged,
 *   which is better than having no comment at all.
 *
 *   WHY: Coverage calculation needs to distinguish documented from
 *   undocumented. Drifted comments are still "there" even if stale.
 * |) *)
let binding_has_doc (bd : binding_doc) =
  match bd.doc with
  | None -> false
  | Some (Exempt_marker, _) -> false
  | Some _ -> true

(* agent note (|
 *   WHAT: Check if a binding is exempt from documentation requirement
 *   (has the (* exempt doc *) marker).
 *
 *   WHY: Exempted bindings are excluded from coverage enforcement.
 *   They don't count against the author — they're explicitly opted out.
 * |) *)
let binding_is_exempt (bd : binding_doc) =
  match bd.doc with
  | Some (Exempt_marker, _) -> true
  | _ -> false

(* agent note (|
 *   WHAT: Check if a binding has a drifted comment — one marked
 *   with (status drifted), indicating it's known to be stale.
 *
 *   WHY: Drifted comments get special treatment in reports: they're
 *   documented (not missing) but flagged for update. This is the
 *   middle ground between accurate and absent documentation.
 * |) *)
let binding_is_drifted (bd : binding_doc) = bd.is_drifted
