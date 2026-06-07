(** Fast structural balance checker — scans characters without fully parsing.
    Aware of all borge lexical contexts: comments, strings, verbatim blocks. *)

type pos = { line : int; col : int }

type paren_frame = {
  open_pos : pos;
  keyword : string option;  (** e.g. "section", "doc", None for bare lists *)
}

type error_detail =
  | Unexpected_close of pos * pos option
  | Unclosed_parens of paren_frame list
  | Unclosed_context of pos * string

type check_result =
  | Balanced of { max_depth : int }
  | Imbalanced of error_detail list

(* --- Detailed analysis types --- *)

type paren_pair = {
  mutable open_pos : pos;
  mutable close_pos : pos option;
  keyword : string option;
  depth : int;  (** 1-indexed nesting level *)
}

type indent_divergence = {
  line : int;
  col : int;
  depth : int;
  expected_col : int;
}

type analysis = {
  result : check_result;
  pairs : paren_pair list;
}

(* agent note (|
 *   WHAT: Convert a balance error detail to a human-readable string
 *   describing the problem and where it occurred.
 *
 *   WHY: Error messages from the balance checker need to be presented
 *   to users in a readable form. Each error variant has specific context.
 * |) *)
let string_of_error = function
  | Unexpected_close (p, None) ->
      Printf.sprintf "Line %d, col %d: unexpected `)` — no matching `(`" p.line p.col
  | Unexpected_close (p, Some open_p) ->
      Printf.sprintf "Line %d, col %d: unexpected `)` — opened at line %d, col %d"
        p.line p.col open_p.line open_p.col
  | Unclosed_parens frames ->
      let count = List.length frames in
      begin match List.rev frames with
      | first :: _ ->
          Printf.sprintf "End of file: %d unclosed `(` — first opened at line %d, col %d"
            count first.open_pos.line first.open_pos.col
      | [] -> "End of file: unclosed `(`"
      end
  | Unclosed_context (p, ctx) ->
      Printf.sprintf "End of file: unclosed %s (opened at line %d, col %d)"
        ctx p.line p.col

(** Render the paren stack for verbose output *)
let string_of_paren_stack (frames : paren_frame list) =
  let buf = Buffer.create 128 in
  Buffer.add_string buf "Paren stack (innermost first):\n";
  List.iter (fun (frame : paren_frame) ->
    match frame.keyword with
    | Some kw ->
        Printf.bprintf buf "  (%d:%d) %s\n" frame.open_pos.line frame.open_pos.col kw
    | None ->
        Printf.bprintf buf "  (%d:%d) (\n" frame.open_pos.line frame.open_pos.col
  ) frames;
  Buffer.contents buf

module State = struct
  type t =
    | Normal
    | Plain_comment
    | Annotated_comment
    | Quoted_string
    | Verbatim of int
    | Inline_example of int

  let name = function
    | Normal -> "normal"
    | Plain_comment -> "; comment"
    | Annotated_comment -> "(* *) comment"
    | Quoted_string -> "\"...\" string"
    | Verbatim _ -> "(|...|) string"
    | Inline_example _ -> "<<...>> example"
end

type frame = { state : State.t; pos : pos }

(* agent note (|
 *   WHAT: Create a position record from a line and column number.
 *
 *   WHY: Position tracking is used throughout balance checking to
 *   report where errors occurred. This is a simple constructor.
 * |) *)
let mk_pos line col = { line; col }

(** Peek at the keyword following an open paren.
    Scans forward from `start` looking for a symbol character.
    Returns None if the next thing is a close-paren, quote, or another open-paren. *)
let peek_keyword text start len =
  let i = ref start in
  (* Skip whitespace *)
  while !i < len && (let ch = text.[!i] in ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r') do
    incr i
  done;
  if !i >= len then None
  else begin
    let ch = text.[!i] in
    if ch = ')' || ch = '(' || ch = '"' then None
    else begin
      (* Read symbol characters *)
      let buf = Buffer.create 32 in
      while !i < len && Lexer.is_symbol_char text.[!i] do
        Buffer.add_char buf text.[!i];
        incr i
      done;
      let kw = Buffer.contents buf in
      if kw = "" then None else Some kw
    end
  end

(** Main check: returns structured result without printing.
    Fast path — no pair tracking, minimal allocation. *)
let check text =
  let len = String.length text in
  let line = ref 1 in
  let col = ref 1 in
  let state_stack = ref [] in
  let paren_stack : paren_frame list ref = ref [] in
  let max_depth = ref 0 in
  let errors = ref [] in

  let push_state st =
    state_stack := { state = st; pos = mk_pos !line !col } :: !state_stack
  in
  let pop_state () =
    match !state_stack with
    | [] -> ()
    | _ :: rest -> state_stack := rest
  in
  let current_state () =
    match !state_stack with
    | [] -> State.Normal
    | frame :: _ -> frame.state
  in
  let here () = mk_pos !line !col in

  let i = ref 0 in
  while !i < len do
    let ch = text.[!i] in

    if ch = '\n' then (
      line := !line + 1;
      col := 1;
      incr i;
      if current_state () = State.Plain_comment then pop_state ();
    ) else (
      match current_state () with
      | State.Normal ->
          if ch = ';' then (
            push_state State.Plain_comment;
            incr i; incr col
          ) else if ch = '(' && !i + 1 < len && text.[!i + 1] = '*' then (
            push_state State.Annotated_comment;
            i := !i + 2; col := !col + 2
          ) else if ch = '(' && !i + 1 < len && text.[!i + 1] = '|' then (
            push_state (State.Verbatim 1);
            i := !i + 2; col := !col + 2
          ) else if ch = '"' then (
            push_state State.Quoted_string;
            incr i; incr col
          ) else if ch = '(' then (
            let keyword = peek_keyword text (!i + 1) len in
            paren_stack := { open_pos = here (); keyword } :: !paren_stack;
            max_depth := max !max_depth (List.length !paren_stack);
            incr i; incr col
          ) else if ch = ')' then (
            match !paren_stack with
            | [] ->
                errors := Unexpected_close (here (), None) :: !errors;
                incr i; incr col
            | _ :: rest ->
                paren_stack := rest;
                incr i; incr col
          ) else (
            incr i; incr col
          )

      | State.Plain_comment ->
          incr i; incr col

      | State.Annotated_comment ->
          if ch = '*' && !i + 1 < len && text.[!i + 1] = ')' then (
            pop_state ();
            i := !i + 2; col := !col + 2
          ) else (
            incr i; incr col
          )

      | State.Quoted_string ->
          if ch = '\\' && !i + 1 < len then (
            i := !i + 2; col := !col + 2
          ) else if ch = '"' then (
            pop_state ();
            incr i; incr col
          ) else (
            incr i; incr col
          )

      | State.Verbatim depth ->
          if ch = '(' && !i + 1 < len && text.[!i + 1] = '|' then (
            push_state (State.Verbatim (depth + 1));
            i := !i + 2; col := !col + 2
          ) else if ch = '|' && !i + 1 < len && text.[!i + 1] = ')' then (
            pop_state ();
            i := !i + 2; col := !col + 2
          ) else if ch = '<' && !i + 1 < len && text.[!i + 1] = '<' then (
            push_state (State.Inline_example 1);
            i := !i + 2; col := !col + 2
          ) else (
            incr i; incr col
          )

      | State.Inline_example depth ->
          if ch = '<' && !i + 1 < len && text.[!i + 1] = '<' then (
            push_state (State.Inline_example (depth + 1));
            i := !i + 2; col := !col + 2
          ) else if ch = '>' && !i + 1 < len && text.[!i + 1] = '>' then (
            pop_state ();
            i := !i + 2; col := !col + 2
          ) else (
            incr i; incr col
          )
    )
  done;

  (* Unclosed lexical contexts *)
  let context_errors =
    List.map (fun frame ->
      Unclosed_context (frame.pos, State.name frame.state)
    ) !state_stack
  in

  let paren_error =
    match !paren_stack with
    | [] -> []
    | frames ->
        [ Unclosed_parens (List.rev frames) ]
  in

  let all_errors = List.rev (!errors @ paren_error @ context_errors) in
  if all_errors = [] then
    Balanced { max_depth = !max_depth }
  else
    Imbalanced all_errors

(* agent note (|
 *   WHAT: Run a structural analysis of a .borg file, recording
 *   every paren pair (open and close), lexical errors, and
 *   indent/depth divergences.
 *
 *   WHY: The enhanced balance report needs structural data to
 *   produce the skeleton view, divergence hints, and smart
 *   close suggestions for imbalanced files.
 * |) *)
let analyze text =
  let len = String.length text in
  let line = ref 1 in
  let col = ref 1 in
  let state_stack = ref [] in
  let paren_stack : paren_frame list ref = ref [] in
  let pair_stack : paren_pair list ref = ref [] in
  let all_pairs : paren_pair list ref = ref [] in
  let max_depth = ref 0 in
  let errors = ref [] in

  let push_state st =
    state_stack := { state = st; pos = mk_pos !line !col } :: !state_stack
  in
  let pop_state () =
    match !state_stack with
    | [] -> ()
    | _ :: rest -> state_stack := rest
  in
  let current_state () =
    match !state_stack with
    | [] -> State.Normal
    | frame :: _ -> frame.state
  in
  let here () = mk_pos !line !col in

  let i = ref 0 in
  while !i < len do
    let ch = text.[!i] in

    if ch = '\n' then (
      line := !line + 1;
      col := 1;
      incr i;
      if current_state () = State.Plain_comment then pop_state ();
    ) else (
      match current_state () with
      | State.Normal ->
          if ch = ';' then (
            push_state State.Plain_comment;
            incr i; incr col
          ) else if ch = '(' && !i + 1 < len && text.[!i + 1] = '*' then (
            push_state State.Annotated_comment;
            i := !i + 2; col := !col + 2
          ) else if ch = '(' && !i + 1 < len && text.[!i + 1] = '|' then (
            push_state (State.Verbatim 1);
            i := !i + 2; col := !col + 2
          ) else if ch = '"' then (
            push_state State.Quoted_string;
            incr i; incr col
          ) else if ch = '(' then (
            let keyword = peek_keyword text (!i + 1) len in
            let open_pos = here () in
            let depth = List.length !paren_stack + 1 in
            paren_stack := { open_pos; keyword } :: !paren_stack;
            max_depth := max !max_depth depth;
            let pair = { open_pos; close_pos = None; keyword; depth } in
            pair_stack := pair :: !pair_stack;
            all_pairs := pair :: !all_pairs;
            incr i; incr col
          ) else if ch = ')' then (
            (match !pair_stack with
            | [] -> ()
            | pair :: rest ->
                pair.close_pos <- Some (here ());
                pair_stack := rest;
            );
            (match !paren_stack with
            | [] ->
                errors := Unexpected_close (here (), None) :: !errors;
                incr i; incr col
            | _ :: rest ->
                paren_stack := rest;
                incr i; incr col
            )
          ) else (
            incr i; incr col
          )

      | State.Plain_comment ->
          incr i; incr col

      | State.Annotated_comment ->
          if ch = '*' && !i + 1 < len && text.[!i + 1] = ')' then (
            pop_state ();
            i := !i + 2; col := !col + 2
          ) else (
            incr i; incr col
          )

      | State.Quoted_string ->
          if ch = '\\' && !i + 1 < len then (
            i := !i + 2; col := !col + 2
          ) else if ch = '"' then (
            pop_state ();
            incr i; incr col
          ) else (
            incr i; incr col
          )

      | State.Verbatim depth ->
          if ch = '(' && !i + 1 < len && text.[!i + 1] = '|' then (
            push_state (State.Verbatim (depth + 1));
            i := !i + 2; col := !col + 2
          ) else if ch = '|' && !i + 1 < len && text.[!i + 1] = ')' then (
            pop_state ();
            i := !i + 2; col := !col + 2
          ) else if ch = '<' && !i + 1 < len && text.[!i + 1] = '<' then (
            push_state (State.Inline_example 1);
            i := !i + 2; col := !col + 2
          ) else (
            incr i; incr col
          )

      | State.Inline_example depth ->
          if ch = '<' && !i + 1 < len && text.[!i + 1] = '<' then (
            push_state (State.Inline_example (depth + 1));
            i := !i + 2; col := !col + 2
          ) else if ch = '>' && !i + 1 < len && text.[!i + 1] = '>' then (
            pop_state ();
            i := !i + 2; col := !col + 2
          ) else (
            incr i; incr col
          )
    )
  done;

  (* Unclosed lexical contexts *)
  let context_errors =
    List.map (fun frame ->
      Unclosed_context (frame.pos, State.name frame.state)
    ) !state_stack
  in

  let paren_error =
    match !paren_stack with
    | [] -> []
    | frames ->
        [ Unclosed_parens (List.rev frames) ]
  in

  let all_errors = List.rev (!errors @ paren_error @ context_errors) in
  let result =
    if all_errors = [] then
      Balanced { max_depth = !max_depth }
    else
      Imbalanced all_errors
  in
  { result; pairs = List.rev !all_pairs }

(** Find indent divergences: open parens whose column does not
    match their canonical depth. *)
let find_divergences (pairs : paren_pair list) =
  List.filter_map (fun (pair : paren_pair) ->
    let expected = pair.depth in
    if pair.open_pos.col <> expected then
      Some ({ line = pair.open_pos.line; col = pair.open_pos.col; depth = pair.depth; expected_col = expected } : indent_divergence)
    else
      None
  ) pairs

(** Render indent divergences as human-readable hints. *)
let render_divergence d =
  Printf.printf "  Line %d, col %d: `(` at depth %d but column is %d (expected %d)\n"
    d.line d.col d.depth d.col d.expected_col;
  if d.col > d.expected_col then
    Printf.printf "    Hint: this form is over-indented — check for an extra `)` above this line\n"
  else
    Printf.printf "    Hint: this form is under-indented — a `)` may be missing before this line\n"

(** Render a structural skeleton of all paren pairs with source previews. *)
let render_skeleton text pairs =
  let lines = String.split_on_char '\n' text in
  let max_line_num =
    List.fold_left (fun acc p -> max acc p.open_pos.line) 0 pairs
  in
  let line_digits = String.length (string_of_int max_line_num) in
  let fmt_line n = Printf.sprintf "%*d" line_digits n in
  List.iter (fun p ->
    let source =
      try List.nth lines (p.open_pos.line - 1) with _ -> ""
    in
    let preview_len = 58 in
    let preview =
      let s = String.trim source in
      let len = String.length s in
      if len > preview_len then String.sub s 0 preview_len ^ "..." else s
    in
    let status = match p.close_pos with
      | Some c -> Printf.sprintf "[closed:%d]" c.line
      | None -> "[⚠ UNCLOSED]"
    in
    let kw = match p.keyword with Some k -> k | None -> "" in
    Printf.printf "  %s: %-58s %s %s\n" (fmt_line p.open_pos.line) preview status kw
  ) pairs

(** Compute the indent (leading spaces) of a source line. *)
let line_indent line_text =
  let len = String.length line_text in
  let i = ref 0 in
  while !i < len && line_text.[!i] = ' ' do incr i done;
  !i

(** Find the next line at or before a given indent, starting from a line.
    Returns the line number and text of the boundary line, or None. *)
let find_sibling_boundary text start_line target_indent =
  let lines = String.split_on_char '\n' text in
  let total = List.length lines in
  let rec scan line_num =
    if line_num > total then None
    else begin
      let line_text =
        try List.nth lines (line_num - 1) with _ -> ""
      in
      let trimmed = String.trim line_text in
      if trimmed <> "" then begin
        let ind = line_indent line_text in
        if ind <= target_indent then
          Some (line_num, line_text)
        else
          scan (line_num + 1)
      end else
        scan (line_num + 1)
    end
  in
  scan start_line

(** Render a smart close suggestion for a single unclosed paren frame. *)
let render_smart_suggestion text (pairs : paren_pair list) (frame : paren_frame) =
  (* Find the pair matching this frame *)
  let matching_pairs =
    List.filter (fun p ->
      p.close_pos = None &&
      p.open_pos.line = frame.open_pos.line &&
      p.open_pos.col = frame.open_pos.col
    ) pairs
  in
  match matching_pairs with
  | [] -> ()
  | pair :: _ ->
      let kw = match pair.keyword with Some k -> k | None -> "form" in
      Printf.printf "  %s (line %d): missing close `)`.\n" kw pair.open_pos.line;
      let target_indent = pair.open_pos.col - 1 in
      let search_start = pair.open_pos.line + 1 in
      match find_sibling_boundary text search_start target_indent with
      | None ->
          let lines = String.split_on_char '\n' text in
          let last_line = List.length lines in
          Printf.printf "    Suggestion: add `)` at the end of the file (after line %d).\n" last_line;
          Printf.printf "    Add: %s)\n" (String.make target_indent ' ')
      | Some (boundary_line, boundary_text) ->
          let indent_str = String.make target_indent ' ' in
          let prev_line_text =
            try
              let lines = String.split_on_char '\n' text in
              List.nth lines (boundary_line - 2)
            with _ -> ""
          in
          Printf.printf "    Suggestion: add `)` before line %d:\n" boundary_line;
          Printf.printf "      %3d | %s\n" (boundary_line - 1) prev_line_text;
          Printf.printf "    +     | %s)\n" indent_str;
          Printf.printf "      %3d | %s\n" boundary_line boundary_text;
          Printf.printf "    (closes %s opened at line %d)\n" kw pair.open_pos.line

(** Extract lines [start_line; end_line] inclusive from text *)
let excerpt text start_line end_line =
  let lines = String.split_on_char '\n' text in
  let total = List.length lines in
  let start_line = max 1 (min start_line total) in
  let end_line = max 1 (min end_line total) in
  let indexed = List.mapi (fun i l -> (i + 1, l)) lines in
  List.filter_map (fun (i, line) ->
    if i >= start_line && i <= end_line then
      Some (Printf.sprintf "%3d | %s" i line)
    else None
  ) indexed

(** Print detailed report for a file *)
let report_file ?(verbose=false) path =
  (* exempt: open_in *)
  let ic = open_in path in
  try
    let n = in_channel_length ic in
    let buf = Bytes.create n in
    really_input ic buf 0 n;
    close_in ic;
    let text = Bytes.to_string buf in
  let analysis = analyze text in
  match analysis.result with
  | Balanced { max_depth } ->
      Printf.printf "✓ %s — balanced (max nesting depth %d)\n" path max_depth;
      true
  | Imbalanced errs ->
      Printf.printf "✗ %s — %d issue(s):\n" path (List.length errs);

      (* Structural skeleton *)
      Printf.printf "\n─── Structural skeleton (paren pairs) ───\n";
      render_skeleton text analysis.pairs;

      (* Indent divergences *)
      let divergences = find_divergences analysis.pairs in
      (match divergences with
      | [] -> ()
      | divs ->
          Printf.printf "\n─── Indent divergences (hints only) ───\n";
          List.iter render_divergence divs;
      );

      (* Errors with smart suggestions *)
      List.iter (fun err ->
        let msg = string_of_error err in
        Printf.printf "\n─── %s ───\n" msg;
        (* Smart suggestion *)
        (match err with
        | Unclosed_parens frames ->
            List.iter (render_smart_suggestion text analysis.pairs) frames
        | Unexpected_close (p, _) ->
            Printf.printf "  This `)` has no matching `(`.\n";
            Printf.printf "  Suggestion: remove it or match it with an opening `(` before line %d.\n" p.line
        | Unclosed_context (p, ctx) ->
            Printf.printf "  Suggestion: close the %s that opened at line %d, col %d.\n"
              ctx p.line p.col
        );
        (* Show context: 2 lines before and after error *)
        let surrounding =
          match err with
          | Unexpected_close (p, _) ->
              excerpt text (p.line - 1) (p.line + 1)
          | Unclosed_parens frames ->
              (match List.rev frames with
              | first :: _ ->
                  excerpt text (first.open_pos.line - 1) (first.open_pos.line + 1)
              | [] -> [])
          | Unclosed_context (p, _) ->
              excerpt text (p.line - 1) (p.line + 1)
        in
        List.iter (fun l -> Printf.printf "      %s\n" l) surrounding;
        if verbose then begin
          match err with
          | Unclosed_parens frames ->
              print_string (string_of_paren_stack frames)
          | _ -> ()
        end;
      ) errs;
      false
  with e ->
    close_in ic;
    raise e
