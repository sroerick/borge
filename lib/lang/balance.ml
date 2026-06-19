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
  | Indent_mismatch of pos * paren_frame  (** structural `)` column does not match `(` column *)

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

(* agent note [5200] (|
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
  | Indent_mismatch (p, frame) ->
      let kw = match frame.keyword with Some k -> k | None -> "form" in
      Printf.sprintf "Line %d, col %d: structural `)` at column %d does not match `(` at column %d (%s, opened line %d)"
        p.line p.col p.col frame.open_pos.col kw frame.open_pos.line

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

(* agent note [5100] (|
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
  let seen_non_ws = ref false in

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
      seen_non_ws := false;
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
            seen_non_ws := false;
            i := !i + 2; col := !col + 2
          ) else if ch = '(' && !i + 1 < len && text.[!i + 1] = '|' then (
            push_state (State.Verbatim 1);
            seen_non_ws := false;
            i := !i + 2; col := !col + 2
          ) else if ch = '"' then (
            push_state State.Quoted_string;
            seen_non_ws := false;
            incr i; incr col
          ) else if ch = '(' then (
            let keyword = peek_keyword text (!i + 1) len in
            paren_stack := { open_pos = here (); keyword } :: !paren_stack;
            max_depth := max !max_depth (List.length !paren_stack);
            seen_non_ws := true;
            incr i; incr col
          ) else if ch = ')' then (
            let is_structural = not !seen_non_ws in
            seen_non_ws := true;
            (match !paren_stack with
            | [] ->
                errors := Unexpected_close (here (), None) :: !errors;
                incr i; incr col
            | frame :: rest ->
                if is_structural && frame.open_pos.col <> !col then (
                  errors := Indent_mismatch (here (), frame) :: !errors
                );
                paren_stack := rest;
                incr i; incr col
            )
          ) else (
            if ch <> ' ' && ch <> '\t' then seen_non_ws := true;
            incr i; incr col
          )

      | State.Plain_comment ->
          incr i; incr col

      | State.Annotated_comment ->
          if ch = '*' && !i + 1 < len && text.[!i + 1] = ')' then (
            pop_state ();
            seen_non_ws := true;
            i := !i + 2; col := !col + 2
          ) else (
            incr i; incr col
          )

      | State.Quoted_string ->
          if ch = '\\' && !i + 1 < len then (
            i := !i + 2; col := !col + 2
          ) else if ch = '"' then (
            pop_state ();
            seen_non_ws := true;
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
            seen_non_ws := true;
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
            seen_non_ws := true;
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

(* agent note [5300] (|
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
  let seen_non_ws = ref false in

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
      seen_non_ws := false;
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
            seen_non_ws := false;
            i := !i + 2; col := !col + 2
          ) else if ch = '(' && !i + 1 < len && text.[!i + 1] = '|' then (
            push_state (State.Verbatim 1);
            seen_non_ws := false;
            i := !i + 2; col := !col + 2
          ) else if ch = '"' then (
            push_state State.Quoted_string;
            seen_non_ws := false;
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
            seen_non_ws := true;
            incr i; incr col
          ) else if ch = ')' then (
            let is_structural = not !seen_non_ws in
            seen_non_ws := true;
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
            | frame :: rest ->
                if is_structural && frame.open_pos.col <> !col then (
                  errors := Indent_mismatch (here (), frame) :: !errors
                );
                paren_stack := rest;
                incr i; incr col
            )
          ) else (
            if ch <> ' ' && ch <> '\t' then seen_non_ws := true;
            incr i; incr col
          )

      | State.Plain_comment ->
          incr i; incr col

      | State.Annotated_comment ->
          if ch = '*' && !i + 1 < len && text.[!i + 1] = ')' then (
            pop_state ();
            seen_non_ws := true;
            i := !i + 2; col := !col + 2
          ) else (
            incr i; incr col
          )

      | State.Quoted_string ->
          if ch = '\\' && !i + 1 < len then (
            i := !i + 2; col := !col + 2
          ) else if ch = '"' then (
            pop_state ();
            seen_non_ws := true;
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
            seen_non_ws := true;
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
            seen_non_ws := true;
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
(* ── Indent-based repair (Parinfer-style) ─────────────────────────── *)

type line_event = {
  ev_line : int;
  ev_open_col : int option;       (** column of first structural `(` *)
  ev_keyword : string option;
  ev_structural_close_col : int option;  (** column of first structural `)` *)
  ev_inline_close_count : int;    (** number of inline `)` *)
}

type repair_action =
  | Insert_line of { line : int; indent : int; reason : string }
  | Trim_end of { line : int; count : int; reason : string }

(** Per-line scanner: for every line, record first open-paren and total
    close-parens, skipping comments/strings/verbatim. *)
let scan_line_events text =
  let len = String.length text in
  let line = ref 1 in
  let col = ref 1 in
  let state_stack = ref [] in
  let events = ref [] in
  let current_open = ref None in
  let current_keyword = ref None in
  let current_structural_close = ref None in
  let current_inline_closes = ref 0 in
  let seen_non_ws = ref false in

  let push_state st = state_stack := st :: !state_stack in
  let pop_state () = state_stack := (match !state_stack with _::t -> t | [] -> []) in
  let current_state () = match !state_stack with [] -> State.Normal | s::_ -> s in
  let flush_event () =
    events := {
      ev_line = !line;
      ev_open_col = !current_open;
      ev_keyword = !current_keyword;
      ev_structural_close_col = !current_structural_close;
      ev_inline_close_count = !current_inline_closes;
    } :: !events;
    current_open := None;
    current_keyword := None;
    current_structural_close := None;
    current_inline_closes := 0;
    seen_non_ws := false
  in

  let i = ref 0 in
  while !i < len do
    let ch = text.[!i] in

    if ch = '\n' then (
      flush_event ();
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
            seen_non_ws := false;
            i := !i + 2; col := !col + 2
          ) else if ch = '(' && !i + 1 < len && text.[!i + 1] = '|' then (
            push_state (State.Verbatim 1);
            seen_non_ws := false;
            i := !i + 2; col := !col + 2
          ) else if ch = '"' then (
            push_state State.Quoted_string;
            seen_non_ws := false;
            incr i; incr col
          ) else if ch = '(' then (
            if !current_open = None then (
              current_open := Some !col;
              current_keyword := peek_keyword text (!i + 1) len;
            );
            seen_non_ws := true;
            incr i; incr col
          ) else if ch = ')' then (
            if not !seen_non_ws then (
              if !current_structural_close = None then
                current_structural_close := Some !col
            ) else (
              incr current_inline_closes
            );
            seen_non_ws := true;
            incr i; incr col
          ) else (
            if ch <> ' ' && ch <> '\t' then seen_non_ws := true;
            incr i; incr col
          )

      | State.Plain_comment ->
          incr i; incr col

      | State.Annotated_comment ->
          if ch = '*' && !i + 1 < len && text.[!i + 1] = ')' then (
            pop_state ();
            seen_non_ws := true;
            i := !i + 2; col := !col + 2
          ) else (
            incr i; incr col
          )

      | State.Quoted_string ->
          if ch = '\\' && !i + 1 < len then (
            i := !i + 2; col := !col + 2
          ) else if ch = '"' then (
            pop_state ();
            seen_non_ws := true;
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
            seen_non_ws := true;
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
            seen_non_ws := true;
            i := !i + 2; col := !col + 2
          ) else (
            incr i; incr col
          )
    )
  done;
  (* Only flush final event if there is actual content — prevents
     a phantom empty line from stealing EOF repair actions. *)
  if !current_open <> None || !current_structural_close <> None || !current_inline_closes > 0 then
    flush_event ();
  List.rev !events

type indent_form = {
  if_line : int;
  if_col : int;
  if_keyword : string option;
}

(** Build repair actions from indentation-based tree inference.
    Algorithm:
    - Each line with an open-paren at col C starts a form.
    - Before opening a new form at col C, close all forms with col > C.
    - Structural `)` must match its opener's column; mismatches are trimmed.
    - Inline `)` pops from the stack normally.
    - At EOF, close everything remaining with properly indented new lines.
*)
let compute_repair_actions (events : line_event list) =
  let stack : indent_form list ref = ref [] in
  let actions : repair_action list ref = ref [] in

  let insert_closes before_line forms =
    (* Insert from innermost (highest indent) to outermost *)
    let sorted = List.sort (fun a b -> compare b.if_col a.if_col) forms in
    List.iter (fun form ->
      actions := Insert_line { line = before_line - 1; indent = form.if_col;
        reason = Printf.sprintf "close %s opened at line %d" (match form.if_keyword with Some k -> k | None -> "form") form.if_line }
        :: !actions
    ) sorted
  in

  let close_forms target_col new_line =
    let rec aux acc =
      match !stack with
      | [] -> acc
      | top :: rest when top.if_col >= target_col ->
          stack := rest;
          aux (top :: acc)
      | _ -> acc
    in
    let to_close = aux [] in
    if to_close <> [] then insert_closes new_line to_close
  in

  List.iter (fun ev ->
    (* Handle open first: close deeper forms, then push new form *)
    (match ev.ev_open_col with
    | Some c ->
        close_forms c ev.ev_line;
        stack := { if_line = ev.ev_line; if_col = c; if_keyword = ev.ev_keyword } :: !stack
    | None -> ());

    (* Handle inline closes: pop from stack *)
    if ev.ev_inline_close_count > 0 then (
      let rec pop count =
        if count <= 0 then ()
        else match !stack with
        | [] ->
            actions := Trim_end { line = ev.ev_line; count = count;
              reason = "unexpected `)` — no matching open-paren" } :: !actions
        | _ :: rest ->
            stack := rest;
            pop (count - 1)
      in
      pop ev.ev_inline_close_count
    );

    (* Handle structural close: must match top of stack *)
    (match ev.ev_structural_close_col with
    | Some c ->
        (match !stack with
        | top :: rest when top.if_col = c ->
            stack := rest
        | _ ->
            (* Mismatched structural close or unexpected — trim it *)
            actions := Trim_end { line = ev.ev_line; count = 1;
              reason = "structural `)` does not match open-paren indentation" } :: !actions
        )
    | None -> ()
    );
  ) events;

  (* EOF: close any remaining *)
  (match !stack with
  | [] -> ()
  | remaining ->
      let last_ev = match List.rev events with e::_ -> e.ev_line | [] -> 1 in
      insert_closes (last_ev + 1) remaining;
      stack := []
  );

  List.rev !actions

(** Apply repair actions to text, producing the corrected file. *)
let apply_repair_actions text actions =
  let raw_lines = String.split_on_char '\n' text in
  let lines, has_trailing_newline =
    match List.rev raw_lines with
    | "" :: rest -> List.rev rest, true
    | _ -> raw_lines, false
  in
  let lines = Array.of_list lines in

  (* Group actions by line *)
  let inserts = Hashtbl.create 16 in
  let trims = Hashtbl.create 16 in
  List.iter (function
    | Insert_line { line; indent; reason } ->
        let existing = try Hashtbl.find inserts line with Not_found -> [] in
        Hashtbl.replace inserts line ((indent, reason) :: existing)
    | Trim_end { line; count; reason = _ } ->
        let existing = try Hashtbl.find trims line with Not_found -> 0 in
        Hashtbl.replace trims line (existing + count)
  ) actions;

  let result = ref [] in
  for i = 0 to Array.length lines - 1 do
    let line_num = i + 1 in
    let line_text = lines.(i) in

    (* Apply trim to current line *)
    let trim_count = try Hashtbl.find trims line_num with Not_found -> 0 in
    let trimmed =
      let rec trim s n =
        if n <= 0 then s
        else
          let len = String.length s in
          if len > 0 && s.[len - 1] = ')' then trim (String.sub s 0 (len - 1)) (n - 1)
          else s
      in
      trim line_text trim_count
    in
    result := trimmed :: !result;

    (* Insert new lines after this line *)
    match try Some (Hashtbl.find inserts line_num) with Not_found -> None with
    | Some items ->
        (* Sort by indent descending so innermost closes appear first *)
        let sorted = List.sort (fun (a, _) (b, _) -> compare b a) items in
        List.iter (fun (indent, _) ->
          result := (String.make (indent - 1) ' ' ^ ")") :: !result
        ) sorted
    | None -> ()
  done;

  let text = String.concat "\n" (List.rev !result) in
  if has_trailing_newline then text ^ "\n" else text

let repair text =
  let events = scan_line_events text in
  let actions = compute_repair_actions events in
  apply_repair_actions text actions

(** Enriched analysis with structural tree and repair actions. *)
type structural_node = {
  node_line : int;
  node_depth : int;
  node_keyword : string option;
  node_open_col : int;
  node_closed : bool;
  node_close_line : int option;
}

type enriched_analysis = {
  enriched_result : check_result;
  enriched_pairs : paren_pair list;
  enriched_divergences : indent_divergence list;
  enriched_tree : structural_node list;
  enriched_repair_actions : repair_action list;
}

let analyze_enriched text =
  let analysis = analyze text in
  let tree =
    List.map (fun (p : paren_pair) ->
      { node_line = p.open_pos.line;
        node_depth = p.depth;
        node_keyword = p.keyword;
        node_open_col = p.open_pos.col;
        node_closed = p.close_pos <> None;
        node_close_line = Option.map (fun (c : pos) -> c.line) p.close_pos;
      }
    ) analysis.pairs
  in
  let divergences = find_divergences analysis.pairs in
  let actions =
    match analysis.result with
    | Balanced _ -> []
    | Imbalanced _ ->
        let events = scan_line_events text in
        compute_repair_actions events
  in
  { enriched_result = analysis.result;
    enriched_pairs = analysis.pairs;
    enriched_divergences = divergences;
    enriched_tree = tree;
    enriched_repair_actions = actions;
  }

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
      List.iter (fun err ->
        Printf.printf "  • %s\n" (string_of_error err);
        if verbose then begin
          match err with
          | Unclosed_parens frames ->
              print_string (string_of_paren_stack frames)
          | _ -> ()
        end;
      ) errs;
      Printf.printf "\n  Run `borge balance --repair %s` to auto-fix.\n" path;
      false
  with e ->
    close_in ic;
    raise e
