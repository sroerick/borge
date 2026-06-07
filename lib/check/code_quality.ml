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

let is_exempt_line line =
  try let _ = Str.search_forward exempt_re line 0 in true
  with Not_found -> false

(* --- Scanning logic --- *)

let check_file path lines =
  let issues = ref [] in
  let line_num = ref 1 in
  let prev_exempt = ref false in
  List.iter (fun line ->
    let exempt = !prev_exempt || is_exempt_line line in
    prev_exempt := is_exempt_line line;
    if not exempt then begin
      List.iter (fun (rule : rule) ->
        try
          let _ = Str.search_forward rule.pattern line 0 in
          (* Skip comment lines to reduce false positives in block comments *)
          if not (String.starts_with ~prefix:"(*" (String.trim line)) then
            issues := { path; line = !line_num; call = rule.name;
                        severity = rule.severity; suggestion = rule.suggestion } :: !issues
        with Not_found -> ()
      ) rules
    end;
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
    try
      let ic = open_in path in
      let lines = ref [] in
      (try while true do lines := input_line ic :: !lines done with End_of_file -> ());
      close_in ic;
      check_file path (List.rev !lines)
    with _ -> []
  ) files
