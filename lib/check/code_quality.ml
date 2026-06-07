(*| Mechanical code quality checker for OCaml source files.

    Scans .ml/.mli files for mechanical anti-patterns:
    - unsafe stdlib calls (List.hd, List.tl, List.assoc, Hashtbl.find,
      String.sub, Str.search_forward, int_of_string, float_of_string,
      open_in, input_line, etc.)

    Regex-based phase: catches unambiguous calls that always warrant
    attention. Planned AST-based phase for polymorphic equality and
    deep match nesting.

    Supports (* exempt: FUNC *) comments to suppress warnings on the
    line where the call appears or the line immediately following.
  |*)

type severity = Error | Warning

type issue = {
  path : string;
  line : int;
  call : string;
  severity : severity;
  suggestion : string;
}

type rule = {
  name : string;
  pattern : Str.regexp;
  severity : severity;
  suggestion : string;
}

(* --- Rules --- *)

let rules : rule list = [
  (* Unconditionally unsafe — will crash on empty input *)
  { name = "List.hd";
    pattern = Str.regexp "List.hd[ \t)]";
    severity = Error;
    suggestion = "Pattern-match on the list instead." };
  { name = "List.tl";
    pattern = Str.regexp "List.tl[ \t)]";
    severity = Error;
    suggestion = "Pattern-match on the list instead." };

  (* Exception-based control flow — safe only when guarded. Flag all. *)
  { name = "List.assoc";
    pattern = Str.regexp "List.assoc[ \t]+[^ ;)]";
    severity = Warning;
    suggestion = "Use List.assoc_opt instead." };
  { name = "Hashtbl.find";
    pattern = Str.regexp "Hashtbl.find[ \t]+[^ ;)]";
    severity = Warning;
    suggestion = "Use Hashtbl.find_opt instead." };
  { name = "String.sub";
    pattern = Str.regexp "String.sub[ \t]+[^ ;)]";
    severity = Warning;
    suggestion = "Ensure bounds are valid, or use String.sub_opt when available." };
  { name = "Str.search_forward";
    pattern = Str.regexp "Str.search_forward[ \t]+[^ ;)]";
    severity = Warning;
    suggestion = "Wrap in try/with Not_found or use a safe alternative." };
  { name = "int_of_string";
    pattern = Str.regexp "int_of_string[ \t]+[^ ;)]";
    severity = Warning;
    suggestion = "Use int_of_string_opt instead." };
  { name = "float_of_string";
    pattern = Str.regexp "float_of_string[ \t]+[^ ;)]";
    severity = Warning;
    suggestion = "Use float_of_string_opt instead." };
  { name = "open_in";
    pattern = Str.regexp "open_in[ \t]+[^ ;)]";
    severity = Warning;
    suggestion = "Use In_channel.with_open_text for automatic close." };
  { name = "input_line";
    pattern = Str.regexp "input_line[ \t]+[^ ;)]";
    severity = Warning;
    suggestion = "Use In_channel.input_line for safe None-returning variant." };
]

(* --- Exemption support --- *)

let exempt_re = Str.regexp "\\(\\* exempt:.*\\*\\)"

let extract_exemptions line =
  try
    (* exempt: Str.search_forward *)
    ignore (Str.search_forward exempt_re line 0);
    Str.matched_string line
  with Not_found -> ""

let is_exempt_line line =
  try
    (* exempt: Str.search_forward *)
    ignore (Str.search_forward exempt_re line 0);
    true
  with Not_found -> false

let exempts_input_line line =
  let s = extract_exemptions line in
  s <> "" && (String.contains s 'i' && String.contains s 'n')  (* crude: contains "input_line" pattern *)

(* --- Scanning logic --- *)

let has_length_guard line =
  try
    let sub_pos = (* exempt: Str.search_forward *) Str.search_forward (Str.regexp "String\\.sub") line 0 in
    let before = String.sub line 0 sub_pos in
    let has_guard_re = Str.regexp "String\\.length\\|> \\|>= \\|try " in
    ignore ((* exempt: Str.search_forward *) Str.search_forward has_guard_re before 0);
    true
  with Not_found -> false

(* Internal length guard: String.sub x (String.length x - N) M — self-bounded *)
let has_internal_length_guard line =
  try
    (* exempt: Str.search_forward *)
    ignore (Str.search_forward (Str.regexp "String\\.sub.*String\\.length") line 0);
    true
  with Not_found -> false

(* Prefix comparison: String.sub x 0 N =|<> "str" — if length check was on previous line *)
let is_prefix_comparison line prev =
  try
    (* exempt: Str.search_forward *)
    ignore (Str.search_forward (Str.regexp "String\\.sub .* 0 [0-9]+.*[<>=].*\"") line 0);
    (* exempt: Str.search_forward *)
    ignore (Str.search_forward (Str.regexp "< [0-9]+\\|> [0-9]+\\|>= [0-9]+") prev 0);
    true
  with Not_found -> false

(* Loop-invariant substring: String.sub x !i (!j - !i) *)
let is_loop_invariant line =
  try
    (* exempt: Str.search_forward *)
    ignore (Str.search_forward (Str.regexp "String\\.sub.*![a-z_]+.*(!.*-.*!)") line 0);
    true
  with Not_found -> false

let check_file path lines =
  let issues = ref [] in
  let line_num = ref 1 in
  let prev_exempt = ref false in
  let block_exempts_input = ref false in
  let prev_line = ref "" in
  List.iter (fun line ->
    (* Block-level exemption: if line exempts both open_in and input_line,
       carry the exemption through the block until close_in *)
    if exempts_input_line line then block_exempts_input := true;
    if String.contains line 'c' && String.contains line 'l' &&
       String.contains line 'o' && String.contains line 's' &&
       String.contains line 'e' && String.contains line '_' &&
       String.contains line 'i' then
      block_exempts_input := false;
    let exempt = !prev_exempt || is_exempt_line line || !block_exempts_input in
    prev_exempt := is_exempt_line line;
    if not exempt then begin
      List.iter (fun (rule : rule) ->
        try
          let _ = (* exempt: Str.search_forward *) Str.search_forward rule.pattern line 0 in
          (* Skip comment lines to reduce false positives in block comments *)
          if not (String.starts_with ~prefix:"(*" (String.trim line)) then
            (* Skip String.sub calls with obvious guards on same or previous line *)
            if rule.name = "String.sub" &&
               (has_length_guard line ||
                has_length_guard !prev_line ||
                has_internal_length_guard line ||
                is_prefix_comparison line !prev_line ||
                is_loop_invariant line) then
              ()
            else
              issues := { path; line = !line_num; call = rule.name;
                          severity = rule.severity; suggestion = rule.suggestion } :: !issues
        with Not_found -> ()
      ) rules
    end;
    prev_line := line;
    incr line_num
  ) lines;
  List.rev !issues

(*| Public entry point: scan a directory of .ml/.mli files for mechanical issues. |*)
let run dir : issue list =
  let rec find_files acc path =
    if not (Sys.file_exists path) then acc
    else if Sys.is_directory path then
      if Filename.basename path = "_build" || Filename.basename path = ".git" then
        acc
      else
        Sys.readdir path
        |> Array.to_list
        |> List.map (Filename.concat path)
        |> List.fold_left find_files acc
    else if Filename.check_suffix path ".ml" || Filename.check_suffix path ".mli" then
      path :: acc
    else
      acc
  in
  let files = find_files [] dir in
  List.concat_map (fun path ->
    let lines = File_utils.read_lines path in
    check_file path lines
  ) files
