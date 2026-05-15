

(** Fast structural balance checker — scans characters without fully parsing.
    Aware of all borge lexical contexts: comments, strings, verbatim blocks. *)

type pos = { line : int; col : int }

type error_detail =
  | Unexpected_close of pos * pos option
  | Unclosed_parens of pos * int
  | Unclosed_context of pos * string

type check_result =
  | Balanced of { max_depth : int }
  | Imbalanced of error_detail list

let string_of_error = function
  | Unexpected_close (p, None) ->
      Printf.sprintf "Line %d, col %d: unexpected `)` — no matching `(`" p.line p.col
  | Unexpected_close (p, Some open_p) ->
      Printf.sprintf "Line %d, col %d: unexpected `)` — opened at line %d, col %d"
        p.line p.col open_p.line open_p.col
  | Unclosed_parens (p, count) ->
      Printf.sprintf "End of file: %d unclosed `(` — first opened at line %d, col %d"
        count p.line p.col
  | Unclosed_context (p, ctx) ->
      Printf.sprintf "End of file: unclosed %s (opened at line %d, col %d)"
        ctx p.line p.col

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

let mk_pos line col = { line; col }

(** Main check: returns structured result without printing *)
let check text =
  let len = String.length text in
  let line = ref 1 in
  let col = ref 1 in
  let state_stack = ref [] in
  let paren_stack = ref [] in
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
            paren_stack := here () :: !paren_stack;
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
    | open_pos :: _ ->
        [ Unclosed_parens (open_pos, List.length !paren_stack) ]
  in

  let all_errors = List.rev (!errors @ paren_error @ context_errors) in
  if all_errors = [] then
    Balanced { max_depth = !max_depth }
  else
    Imbalanced all_errors

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
let report_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  let text = Bytes.to_string buf in
  match check text with
  | Balanced { max_depth } ->
      Printf.printf "✓ %s — balanced (max nesting depth %d)\n" path max_depth;
      true
  | Imbalanced errs ->
      Printf.printf "✗ %s — %d issue(s):\n" path (List.length errs);
      List.iter (fun err ->
        let msg = string_of_error err in
        Printf.printf "  → %s\n" msg;
        (* Show context: 2 lines before and after error *)
        let surrounding =
          match err with
          | Unexpected_close (p, _) ->
              excerpt text (p.line - 1) (p.line + 1)
          | Unclosed_parens (p, _) ->
              excerpt text (p.line - 1) (p.line + 1)
          | Unclosed_context (p, _) ->
              excerpt text (p.line - 1) (p.line + 1)
        in
        List.iter (fun l -> Printf.printf "      %s\n" l) surrounding;
        (* Suggestion *)
        match err with
        | Unexpected_close (p, _) ->
            Printf.printf "      Suggestion: check for extra `)` near line %d\n" p.line
        | Unclosed_parens (p, count) ->
            Printf.printf "      Suggestion: add %d `)` after end of file (unclosed since line %d)\n"
              count p.line
        | Unclosed_context (p, ctx) ->
            Printf.printf "      Suggestion: close the %s that opened at line %d\n"
              ctx p.line
      ) errs;
      false
