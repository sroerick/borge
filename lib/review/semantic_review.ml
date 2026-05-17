(* Semantic code review - orchestration layer
 *
 * roerick note (|
 *   Orchestrates the semantic review pipeline:
 *   1. Extract functions from source files
 *   2. Build prompts and call LLM
 *   3. Parse responses into structured findings
 *   4. Write findings to .borg.meta files
 *
 *   This module ties together extract_parsetree, extract_functions,
 *   review_prompt, review_parse, and review_agent into a cohesive
 *   workflow for borge review command.
 * |) *)

open Review_types
open Borge_lang

(** Review a single file and return findings *)
let review_file = Review_agent.review_file

(** Review all .ml files in a directory *)
let review_dir dir =
  ignore (File_utils.find_borg_files dir);
  let ml_files = 
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".ml")
    |> List.map (Filename.concat dir)
  in
  
  let findings = List.filter_map (fun path ->
    if Sys.file_exists path && not (Sys.is_directory path) then
      Review_agent.review_file path
    else
      None
  ) ml_files in
  
  findings

(** Review files that have changed since last review *)
let review_stale dir =
  (* Find all .ml files *)
  let ml_files = 
    let rec find_ml acc dir =
      if Sys.file_exists dir && Sys.is_directory dir then
        try
          Sys.readdir dir
          |> Array.to_list
          |> List.fold_left (fun acc f ->
              let path = Filename.concat dir f in
              if Sys.is_directory path then
                if f <> "_build" && f <> ".git" then
                  find_ml acc path
                else
                  acc
              else if Filename.check_suffix f ".ml" then
                path :: acc
              else
                acc
          ) acc
        with _ -> acc
      else
        acc
    in
    find_ml [] dir
  in
  
  (* Filter to files that have changed since last reviewed_at *)
  (* For now, review all files - staleness detection needs .borg.meta reader *)
  let findings = List.filter_map Review_agent.review_file ml_files in
  findings

(** Review files related to a spec section *)
let review_section ~borg_file ~section_name =
  (* Find the section in the borg file *)
  try
    let input = File_utils.read_file borg_file in
    let _file = Parse.parse_file input in
    
    (* Look for matching section in exports or surface *)
    (* This is simplified - would need proper section extraction *)
    let section_text =
      try
        let start = Str.search_forward (Str.regexp ("(section " ^ section_name)) input 0 in
        (* Find matching closing paren *)
        let depth = ref 1 in
        let i = ref (start + 9 + String.length section_name) in
        while !depth > 0 && !i < String.length input do
          match input.[!i] with
          | '(' -> incr depth; incr i
          | ')' -> decr depth; incr i
          | '"' ->
              incr i;
              while !i < String.length input && input.[!i] <> '"' do
                if input.[!i] = '\\' then incr i;
                incr i
              done;
              incr i
          | '|' when !i + 1 < String.length input && input.[!i + 1] = ')' ->
              incr i;  (* verbatim string *)
              incr i
          | _ -> incr i
        done;
        String.sub input start (!i - start)
      with Not_found -> ""
    in
    
    if section_text = "" then
      Error "Section not found"
    else
      (* Find implementation file - heuristic based on section name *)
      let impl_filename = 
        Str.global_replace (Str.regexp "_") "-" section_name ^ ".ml"
      in
      let impl_path = Filename.concat "lib" impl_filename in
      
      if Sys.file_exists impl_path then
        match Review_agent.review_file impl_path with
        | Some review -> Ok review
        | None -> Error "Review failed"
      else
        Error (Printf.sprintf "Implementation file not found: %s" impl_path)
  with e ->
    Error (Printexc.to_string e)

(** Convert semantic review to metadata string *)
let review_to_metadata (review : semantic_review) : string =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "(semantic-review\n";
  Buffer.add_string buf (Printf.sprintf "  (source-file \"%s\")\n" review.source_file);
  Buffer.add_string buf (Printf.sprintf "  (reviewed-at \"%s\")\n" review.reviewed_at);
  Buffer.add_string buf (Printf.sprintf "  (reviewer \"%s\")\n" review.reviewer);
  Buffer.add_string buf "  (functions\n";
  
  List.iter (fun (f : function_finding) ->
    Buffer.add_string buf (Printf.sprintf "    (function name:\"%s\"\n" f.name);
    Buffer.add_string buf (Printf.sprintf "      (doc-present %b)\n" f.doc_present);
    Buffer.add_string buf (Printf.sprintf "      (doc-accuracy %s)\n" (string_of_accuracy f.doc_accuracy));
    Buffer.add_string buf (Printf.sprintf "      (signature-match %s)\n" (string_of_signature_match f.signature_match));
    Buffer.add_string buf (Printf.sprintf "      (behavior-coverage %s)\n" (string_of_behavior_coverage f.behavior_coverage));
    (match f.structural_issues with
     | Some i -> Buffer.add_string buf (Printf.sprintf "      (structural-issues \"%s\")\n" i)
     | None -> ());
    Buffer.add_string buf (Printf.sprintf "      (confidence %s)\n" (string_of_confidence f.confidence));
    Buffer.add_string buf (Printf.sprintf "      (checked-at \"%s\"))\n" f.checked_at);
  ) review.functions;
  
  Buffer.add_string buf "  ))\n";
  Buffer.add_string buf ")\n";
  Buffer.contents buf

(** Legacy function extraction - kept for backward compatibility *)
let extract_functions_simple content =
  (* Simple regex-based extraction for files that don't parse with compiler-libs *)
  let lines = String.split_on_char '\n' content in
  let functions = ref [] in
  let current_doc = ref [] in
  let line_num = ref 0 in
  
  List.iter (fun line ->
    incr line_num;
    let trimmed = String.trim line in
    
    if String.length trimmed > 2 && String.sub trimmed 0 2 = "(*" then
      current_doc := line :: !current_doc
    else if String.length trimmed > 7 && String.sub trimmed 0 7 = "let rec" then
      let name = try
        let rest = String.sub trimmed 8 (String.length trimmed - 8) in
        let space_pos = try String.index rest ' ' with Not_found -> String.length rest in
        String.sub rest 0 space_pos
      with _ -> "unknown" in
      functions := {
        Extract_functions.name;
        signature = "";
        docstring = if !current_doc = [] then None else Some (String.concat "\n" (List.rev !current_doc));
        line = !line_num;
        is_recursive = true;
        is_test = false;
        is_ignored = false;
      } :: !functions;
      current_doc := []
    else if String.length trimmed > 3 && String.sub trimmed 0 3 = "let" then
      let name = try
        let rest = String.sub trimmed 4 (String.length trimmed - 4) in
        let space_pos = try String.index rest ' ' with Not_found -> String.length rest in
        String.sub rest 0 space_pos
      with _ -> "unknown" in
      functions := {
        Extract_functions.name;
        signature = "";
        docstring = if !current_doc = [] then None else Some (String.concat "\n" (List.rev !current_doc));
        line = !line_num;
        is_recursive = false;
        is_test = false;
        is_ignored = false;
      } :: !functions;
      current_doc := []
  ) lines;
  
  List.rev !functions

(** Backwards compatibility for cmd/Review.ml *)

type function_info = {
  name : string;
  signature : string;
  docstring : string option;
  body : string;
  line : int;
}

type review_result = {
  doc_present : bool;
  doc_accuracy : [ `High | `Medium | `Low | `Unknown ];
  structural_issues : string option;
  confidence : [ `High | `Medium | `Low ];
}

let empty_result = {
  doc_present = false;
  doc_accuracy = `Unknown;
  structural_issues = None;
  confidence = `Low;
}

let extract_functions content : function_info list =
  let simple_list = extract_functions_simple content in
  List.map (fun (info : Extract_functions.function_info) -> {
    name = info.name;
    signature = info.signature;
    docstring = info.docstring;
    body = "";  (* Not used by old code *)
    line = info.line;
  }) simple_list

let build_prompt (info : function_info) =
  let doc_part = match info.docstring with
    | None -> "No docstring found."
    | Some d -> Printf.sprintf "Docstring:\n%s" d
  in
  Printf.sprintf {|Review this OCaml function:

Function name: %s
Body:
%s

%s

Evaluate:
1. Is there a docstring? (yes/no)
2. If yes, does it accurately describe what the function does?
   (high/medium/low/unknown)
3. Are there structural issues (complexity, unclear flow)?
   Describe briefly or say "none"
4. Your confidence in this assessment (high/medium/low)

Respond in this format:
Doc present: yes/no
Accuracy: high/medium/low/unknown
Issues: description or "none"
Confidence: high/medium/low
|}
    info.name info.body doc_part

(** Legacy parse_response for backwards compatibility *)
let parse_response response : review_result =
  let lines = String.split_on_char '\n' response in
  let result = ref empty_result in
  
  List.iter (fun line ->
    let line = String.trim line in
    if String.length line > 12 && String.sub line 0 12 = "Doc present:" then
      result := { !result with doc_present = 
        let v = String.trim (String.sub line 12 (String.length line - 12)) in
        v = "yes" }
    else if String.length line > 10 && String.sub line 0 10 = "Accuracy:" then
      let v = String.trim (String.sub line 10 (String.length line - 10)) in
      let acc = match v with
        | "high" -> `High | "medium" -> `Medium | "low" -> `Low | _ -> `Unknown
      in
      result := { !result with doc_accuracy = acc }
    else if String.length line > 7 && String.sub line 0 7 = "Issues:" then
      let issues = String.trim (String.sub line 7 (String.length line - 7)) in
      result := { !result with structural_issues = if issues = "none" then None else Some issues }
    else if String.length line > 11 && String.sub line 0 11 = "Confidence:" then
      let v = String.trim (String.sub line 11 (String.length line - 11)) in
      let conf = match v with
        | "high" -> `High | "medium" -> `Medium | _ -> `Low
      in
      result := { !result with confidence = conf }
  ) lines;
  
  !result

(** Legacy render_metadata for backwards compatibility *)
let render_metadata (info : function_info) (result : review_result) =
  let doc_acc = match result.doc_accuracy with
    | `High -> "high" | `Medium -> "medium" | `Low -> "low" | `Unknown -> "unknown"
  in
  let conf = match result.confidence with
    | `High -> "high" | `Medium -> "medium" | `Low -> "low"
  in
  let lines = [
    Printf.sprintf "  (function name:\"%s\" line:%d" info.name info.line;
    Printf.sprintf "    (doc-present %b)" result.doc_present;
    Printf.sprintf "    (doc-accuracy %s)" doc_acc;
    Printf.sprintf "    (confidence %s)" conf;
  ] in
  let lines = match result.structural_issues with
    | Some i -> lines @ [Printf.sprintf "    (structural-issues \"%s\")" i]
    | None -> lines
  in
  lines @ ["  )"]
