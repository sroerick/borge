(* Semantic code review - verify function docs match implementation *)

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

(* Simple extraction - looks for let bindings and preceding comments *)
let extract_functions content : function_info list =
  let lines = String.split_on_char '\n' content in
  let functions = ref [] in
  let current_doc = ref [] in
  let line_num = ref 0 in
  
  List.iter (fun line ->
    incr line_num;
    let trimmed = String.trim line in
    
    (* Check for doc comment *)
    if String.length trimmed > 2 && String.sub trimmed 0 2 = "(*" then
      current_doc := line :: !current_doc
    else if String.length trimmed > 7 && String.sub trimmed 0 7 = "let rec" then
      let name = try
        let rest = String.sub trimmed 8 (String.length trimmed - 8) in
        let space_pos = try String.index rest ' ' with Not_found -> String.length rest in
        String.sub rest 0 space_pos
      with _ -> "unknown" in
      functions := {
        name;
        signature = "";
        docstring = if !current_doc = [] then None else Some (String.concat "\n" (List.rev !current_doc));
        body = line;
        line = !line_num;
      } :: !functions;
      current_doc := []
    else if String.length trimmed > 3 && String.sub trimmed 0 3 = "let" then
      let name = try
        let rest = String.sub trimmed 4 (String.length trimmed - 4) in
        let space_pos = try String.index rest ' ' with Not_found -> String.length rest in
        String.sub rest 0 space_pos
      with _ -> "unknown" in
      functions := {
        name;
        signature = "";
        docstring = if !current_doc = [] then None else Some (String.concat "\n" (List.rev !current_doc));
        body = line;
        line = !line_num;
      } :: !functions;
      current_doc := []
  ) lines;
  
  List.rev !functions

(* Build LLM prompt for doc/verification *)
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

(* Parse LLM response into structured result *)
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

(* Store result in .borg.meta format *)
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
