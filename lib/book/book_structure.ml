(* agent note (|
 *   WHAT: The shared structural projection of one .borg chapter:
 *   parse -> a tree of typed nodes, with register (workshop/reader)
 *   filtering applied ONCE, here.
 *   WHY: habitat-book decision 3 — one parser, two projections.
 *   book print renders this structure to LaTeX; book export renders
 *   it to JSON. Forking the AST walk would let the paper and the
 *   habitat drift apart (different scaffolding rules per projection),
 *   so both consume this pass (pricklypear borge/habitat-book.borg
 *   export-projection).
 *   NODE VOCABULARY (kind / title / body / children):
 *     project | section | subsection   headings (title = name;
 *                                      children nest)
 *     doc        prose block (body = text, paragraphs blank-line
 *                separated)
 *     status     (status X)            workshop only
 *     verify     compact scaffold summary (body)   workshop only
 *     agent-note annotated author comment (title = author [+ type],
 *                body = text)          workshop only
 *     untracked  (untracked path "reason")           workshop only
 *     inline     (inline path) — navigation, kept in BOTH registers
 *     raw        export-only drift-hole node (whole-file source) for
 *                chapters that fail to parse; print renders the same
 *                failure as a fallback listing
 *   Chapter-level title/status are extracted mode-independently:
 *   title = first project/section/subsection name (document order),
 *   status = first (status X) anywhere (depth-first).
 *   The comment-drain ordering reproduces book_print v2 exactly
 *   (drain before each child, at section head, and at each list end),
 *   so a print run over this pass is byte-identical to the
 *   pre-extraction renderer — the loop-4 determinism harness doubles
 *   as this refactor's regression check.
 * |) *)

type mode = Workshop | Reader

type node = {
  kind : string;
  title : string;
  body : string;
  children : node list;
}

(* --- Example fences (habitat-book.borg export-projection, P1.6) ---
   Doc-block convention shared by print and the habitat web render:
   ```nopales / ```ocaml fenced blocks inside doc strings, with an
   optional `expect => <value>` trailer line after the closing
   fence. fence_split segments a doc body into prose text and
   example records; ONE parser serves both projections (decision 3):
   print renders the segments (text -> paragraphs, example ->
   listing + expect annotation), export emits the example records
   on the chapter. The doc node's body itself stays verbatim —
   segments are derived. Unparsed fences (other languages,
   unterminated) stay raw text everywhere: no record, prose render.
   The expect trailer is the v1 captured-output record (habitat-book
   decision 4: no live OCaml evaluator; print does not execute code). *)

type example = {
  lang : string;             (* "nopales" | "ocaml" *)
  src : string;              (* fence body, one \n per source line *)
  expected : string option;  (* `expect => v` trailer, if present *)
}

type segment = Text of string | Code of example

type chapter_body = {
  title_hint : string option;
  status : string option;
  nodes : node list;
  examples : example list;
}

let example_languages = [ "nopales"; "ocaml" ]

(* Fence lines: ```lang opens (empty lang = untyped fence), a bare
   ``` line closes. Strict end-of-line anchor: `` `lang extra`` is
   not a fence — malformed fences stay raw text. *)
let fence_open_re = Str.regexp "^[ \t]*```[ \t]*\\([A-Za-z0-9_-]*\\)[ \t]*$"
let fence_close_re = Str.regexp "^[ \t]*```[ \t]*$"
let expect_re = Str.regexp "^[ \t]*expect[ \t]*=>[ \t]*\\(.*\\)$"

let fence_split (text : string) : segment list =
  let lines = Array.of_list (String.split_on_char '\n' text) in
  let len = Array.length lines in
  let out = ref [] in
  let buf = Buffer.create 0 in
  let flush () =
    if Buffer.length buf > 0 then begin
      out := Text (Buffer.contents buf) :: !out;
      Buffer.clear buf
    end
  in
  let add_text ln =
    Buffer.add_string buf ln;
    Buffer.add_char buf '\n'
  in
  let i = ref 0 in
  while !i < len do
    let ln = lines.(!i) in
    if Str.string_match fence_open_re ln 0 then begin
      let lang = Str.matched_group 1 ln in
      if List.mem lang example_languages then begin
        (* Example fence: collect src until the close fence. An
           unterminated fence is unparsed — fall back to raw text. *)
        flush ();
        let src = Buffer.create 0 in
        let closed = ref false in
        incr i;
        while not !closed && !i < len do
          if Str.string_match fence_close_re lines.(!i) 0 then closed := true
          else begin
            Buffer.add_string src lines.(!i);
            Buffer.add_char src '\n'
          end;
          incr i
        done;
        if not !closed then begin
          add_text ln;
          Buffer.add_string buf (Buffer.contents src)
        end
        else begin
          (* Optional expect trailer: the next non-blank line. If it
             matches, it is consumed into the record (removed from
             the prose stream — print re-renders it as the
             annotation, the web render reads the record). *)
          let j = ref !i in
          while !j < len && String.trim lines.(!j) = "" do incr j done;
          let expected =
            if !j < len && Str.string_match expect_re lines.(!j) 0 then begin
              let v = String.trim (Str.matched_group 1 lines.(!j)) in
              i := !j + 1;
              Some v
            end
            else None
          in
          out := Code { lang; src = Buffer.contents src; expected } :: !out
        end
      end
      else begin
        (* Non-example fence (other lang or untyped): raw text
           through its close — a ```nopales inside must not leak. *)
        add_text ln;
        incr i;
        let closed = ref false in
        while not !closed && !i < len do
          add_text lines.(!i);
          if Str.string_match fence_close_re lines.(!i) 0 then closed := true;
          incr i
        done
      end
    end
    else begin
      add_text ln;
      incr i
    end
  done;
  flush ();
  List.rev !out

(* Chapter examples: every fenced example in doc nodes, document
   order (preorder over the node tree). Mode-independent — examples
   are content, not register scaffolding. *)
let rec examples_of_nodes acc = function
  | [] -> List.rev acc
  | n :: rest ->
    let acc =
      if n.kind = "doc" then
        List.fold_left
          (fun acc seg -> match seg with Code e -> e :: acc | Text _ -> acc)
          acc (fence_split n.body)
      else acc
    in
    examples_of_nodes acc (n.children @ rest)

let examples_of nodes = examples_of_nodes [] nodes

(* Parse failure = drift hole. Print falls back to a raw listing;
   export falls back to a raw node — in both registers. *)
exception Fallback_raw

let string_value_text = function
  | Borge_lang.Ast.Quoted q -> q.Borge_lang.Ast.q_content
  | Borge_lang.Ast.Verbatim v -> v.Borge_lang.Ast.v_content

let agent_note_node (ac : Borge_lang.Ast.annotated_comment) =
  let author =
    match ac.Borge_lang.Ast.authorship with
    | Borge_lang.Ast.Single a -> a
    | Borge_lang.Ast.Multiple xs -> String.concat "," xs
  in
  let typ =
    match ac.Borge_lang.Ast.comment_type with
    | Borge_lang.Ast.Untyped -> ""
    | Borge_lang.Ast.Typed t -> " " ^ t
  in
  let body =
    match ac.Borge_lang.Ast.value with
    | Some v -> String.trim (string_value_text v)
    | None -> ""
  in
  if body = "" then None
  else Some { kind = "agent-note"; title = author ^ typ; body; children = [] }

let comment_node ~mode = function
  | Borge_lang.Ast.Plain _ -> None  (* (; machine comments *)
  | Borge_lang.Ast.Annotated ac ->
    if mode = Workshop then agent_note_node ac else None

(* First project/section/subsection name in document order. *)
let title_hint_of file =
  let open Borge_lang.Ast in
  let acc = ref None in
  (try
     ignore
       (fold_sexps file () (fun () s ->
            match (s, !acc) with
            | List (_, Atom (_, ("project" | "section" | "subsection")) :: Atom (_, n) :: _), None ->
              acc := Some n;
              raise Exit
            | _ -> ()))
   with Exit -> ());
  !acc

(* First (status X) anywhere, depth-first (fold order = document
   order). Mode-independent: the chapter's status field is data, not
   register scaffolding. *)
let status_of file =
  let open Borge_lang.Ast in
  let acc = ref None in
  ignore
    (fold_sexps file () (fun () s ->
         match (s, !acc) with
         | List (_, Atom (_, "status") :: Atom (_, v) :: _), None -> acc := Some v
         | _ -> ()));
  !acc

let of_file ~mode ~content =
  let open Borge_lang.Ast in
  let file =
    try Borge_lang.Parse.parse_file content with _ -> raise Fallback_raw
  in
  let sorted_nested =
    List.stable_sort (fun (p1, _) (p2, _) -> compare p1.offset p2.offset)
      file.nested_comments
  in
  let pending = ref sorted_nested in
  (* Drain annotated comments whose position precedes [limit], in
     source order — the same drain points book_print v2 used. *)
  let drain limit =
    let out = ref [] in
    let rec loop () =
      match !pending with
      | (cpos, c) :: rest when cpos.offset < limit ->
        (match comment_node ~mode c with
         | Some n -> out := n :: !out
         | None -> ());
        pending := rest;
        loop ()
      | _ -> ()
    in
    loop ();
    List.rev !out
  in
  let rec build node =
    match node with
    | Atom _ | String _ -> []
    | List (_pos, children) as lst ->
      let head_is s = match children with Atom (_, x) :: _ -> x = s | _ -> false in
      let rest = match children with _ :: r -> r | [] -> [] in
      if head_is "project" || head_is "section" || head_is "subsection" then begin
        let name = match rest with Atom (_, n) :: _ -> n | _ -> "" in
        let kind =
          if head_is "project" then "project"
          else if head_is "section" then "section"
          else "subsection"
        in
        (* Comments between the keyword atom and the name atom (or
           first inner child) drain BEFORE the heading — print v2
           emitted them there, so they become siblings preceding the
           section node. *)
        let pre =
          match rest with
          | Atom (p, _) :: _ -> drain p.offset
          | child :: _ -> drain (start_pos_of child).offset
          | [] -> drain (end_pos_of lst).offset
        in
        let inner = match rest with _ :: r -> r | [] -> [] in
        let kids =
          List.concat_map
            (fun child -> drain (start_pos_of child).offset @ build child)
            inner
        in
        let post = drain (end_pos_of lst).offset in
        pre @ [ { kind; title = name; body = ""; children = kids @ post } ]
      end
      else if head_is "doc" then begin
        match rest with
        | String (_, v) :: _ ->
          [ { kind = "doc"; title = ""; body = string_value_text v; children = [] } ]
        | _ -> []
      end
      else if head_is "details" then begin
        (* details is transparent: splice its children at this level. *)
        List.concat_map
          (fun child -> drain (start_pos_of child).offset @ build child)
          rest
        @ drain (end_pos_of lst).offset
      end
      else if head_is "status" then begin
        if mode = Workshop then
          match rest with
          | Atom (_, s) :: _ -> [ { kind = "status"; title = ""; body = s; children = [] } ]
          | _ -> []
        else []
      end
      else if head_is "inline" then begin
        (* Target may be a quoted/verbatim string OR a bare atom —
           the same forms Spec.inline_targets accepts (the walk
           follows both spellings; the navigation note must too). *)
        match rest with
        | String (_, v) :: _ ->
          [ { kind = "inline"; title = ""; body = string_value_text v; children = [] } ]
        | Atom (_, p) :: _ ->
          [ { kind = "inline"; title = ""; body = p; children = [] } ]
        | _ -> []
      end
      else if head_is "verify" then begin
        let pre =
          match rest with
          | child :: _ -> drain (start_pos_of child).offset
          | [] -> []
        in
        let items =
          List.filter_map
            (fun child ->
              match child with
              | List (_, Atom (_, h) :: r) ->
                let strs =
                  List.filter_map
                    (function String (_, v) -> Some (string_value_text v) | _ -> None)
                    r
                in
                Some (if strs = [] then h else h ^ ": " ^ String.concat "; " strs)
              | _ -> None)
            rest
        in
        let node =
          if mode = Workshop then
            [ { kind = "verify"; title = ""; body = String.concat "; " items; children = [] } ]
          else []
        in
        pre @ node @ drain (end_pos_of lst).offset
      end
      else if head_is "untracked" then begin
        let pre =
          match rest with
          | child :: _ -> drain (start_pos_of child).offset
          | [] -> []
        in
        let node =
          if mode = Workshop then
            match rest with
            | path_node :: r ->
              let path_text =
                match path_node with
                | Atom (_, p) -> p
                | String (_, v) -> string_value_text v
                | _ -> ""
              in
              let reasons =
                List.filter_map
                  (function String (_, v) -> Some (string_value_text v) | _ -> None)
                  r
              in
              let reason = match reasons with x :: _ -> x | [] -> "" in
              [ { kind = "untracked"; title = path_text; body = reason; children = [] } ]
            | [] -> []
          else []
        in
        pre @ node @ drain (end_pos_of lst).offset
      end
      else [] (* depends-on, convention, ... stay structural noise *)
  in
  let nodes =
    List.filter_map (comment_node ~mode) file.top_level_comments
    @ List.concat_map
        (fun swc ->
          List.filter_map (comment_node ~mode) swc.comments_before @ build swc.node)
        file.top_level
  in
  { title_hint = title_hint_of file;
    status = status_of file;
    nodes;
    examples = examples_of nodes }
