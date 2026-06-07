(* Build prompts for semantic review LLM queries
 *
 * roerick note (|
 *   Assembles prompts for semantic review that constrain the LLM
 *   to return structured sexp output. This is critical for parseability.
 * |) *)

(** Build a prompt for reviewing a single function.
    The prompt asks the LLM to evaluate three dimensions:
    1. Doc-code alignment (WHAT and WHY)
    2. Internal consistency (dead branches, unused params, contradictions)
    3. Overall review quality *)
let build_function_prompt (info : Extract_functions.function_info) =
  let doc_section = match info.docstring with
    | Some d -> Printf.sprintf "Documentation:\n%s\n\n" d
    | None -> "Documentation: [none]\n\n"
  in
  Printf.sprintf {|Review this OCaml function:

Function name: %s
Signature: %s
Line: %d
Recursive: %s

%s
Implementation:
%s

Evaluate the following and return ONLY a sexp response in this exact format:

(function name:"%s"
  (doc-present %s)
  (doc-status accurate|drifted|missing)
  (doc-accuracy high|medium|low)
  (consistency consistent|questionable|inconsistent)
  (internal-issues "description or none")
  (signature-match accurate|mismatch|unknown)
  (behavior-coverage complete|partial|missing)
  (structural-issues "description or none")
  (confidence high|medium|low))

Evaluation criteria:

Doc-code alignment — check both WHAT and WHY:
- doc-present: Does any documentation exist for this function?
- doc-status: Is the doc accurate (matches code), drifted (has
  (status drifted) marker or clearly stale), or missing?
- doc-accuracy: Does the documentation answer:
  1. WHAT does this function do? (inputs, outputs, side effects)
  2. WHY does this function exist? (design reason, constraint)
  A doc that only restates the signature scores medium at best.

Internal consistency — check for code problems:
- consistency: Is the function internally consistent?
  - consistent: no issues detected
  - questionable: minor issues (unused parameter, redundant check)
  - inconsistent: major issues (unreachable branch, parameter
    always passed the same value, logic contradicts the doc)
- internal-issues: Describe any problems found, or "none"

Other checks:
- signature-match: Does the implementation match the signature?
- behavior-coverage: Does the doc cover all behaviors and edge cases?
- structural-issues: Code smells, complexity, unclear flow
- confidence: Your confidence in this assessment
|}
    info.name
    info.signature
    info.line
    (if info.is_recursive then "yes" else "no")
    doc_section
    "[implementation not shown - infer from signature]"
    info.name
    (if info.docstring <> None then "true" else "false")

(** Build a batched prompt for reviewing multiple functions
    
    Token management: If prompt would exceed ~8k tokens, this function
    should be called with smaller batches. *)
let build_batch_prompt (functions : Extract_functions.function_info list) =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "Review these OCaml functions:\n\n";
  
  List.iter (fun (info : Extract_functions.function_info) ->
    Buffer.add_string buf (Printf.sprintf "--- Function: %s ---\n" info.name);
    Buffer.add_string buf (Printf.sprintf "Signature: %s\n" info.signature);
    Buffer.add_string buf (Printf.sprintf "Line: %d\n" info.line);
    (match info.docstring with
     | Some d -> Buffer.add_string buf (Printf.sprintf "Doc: %s\n" d)
     | None -> Buffer.add_string buf "Doc: [none]\n");
    Buffer.add_string buf "\n"
  ) functions;
  
  Buffer.add_string buf {|For each function, return a (function ...) sexp block:

(function name:"FUNCTION_NAME"
  (doc-present true|false)
  (doc-status accurate|drifted|missing)
  (doc-accuracy high|medium|low)
  (consistency consistent|questionable|inconsistent)
  (internal-issues "description or none")
  (signature-match accurate|mismatch|unknown)
  (behavior-coverage complete|partial|missing)
  (structural-issues "description or none")
  (confidence high|medium|low))

Return all findings wrapped in a (findings ...) block:
(findings
  (function name:"func1" ...)
  (function name:"func2" ...)
  ...)

If no findings for a function, omit it from the response.
For doc-status: accurate = doc matches code, drifted = doc is
stale or has (status drifted) marker, missing = no doc.
For consistency: consistent = no issues, questionable = minor
(unused param), inconsistent = major (dead branch, logic error).
|};
  
  Buffer.contents buf

(** Build prompt comparing spec section to implementation file *)
let build_spec_comparison_prompt ~spec_section ~module_surface ~functions =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "Compare this spec section to its implementation:\n\n";
  Buffer.add_string buf "--- SPEC SECTION ---\n";
  Buffer.add_string buf spec_section;
  Buffer.add_string buf "\n--- END SPEC ---\n\n";
  
  Buffer.add_string buf "--- MODULE EXPORTS ---\n";
  Buffer.add_string buf (Printf.sprintf "Module: %s\n" module_surface.Surface.module_name);
  Buffer.add_string buf (Printf.sprintf "Exports: %s\n" (String.concat ", " module_surface.exports));
  Buffer.add_string buf (Printf.sprintf "File: %s\n\n" module_surface.path);
  
  Buffer.add_string buf "--- FUNCTIONS TO REVIEW ---\n";
  List.iter (fun (info : Extract_functions.function_info) ->
    Buffer.add_string buf (Printf.sprintf "- %s: %s\n" info.name info.signature)
  ) functions;
  Buffer.add_string buf "\n";
  
  Buffer.add_string buf {|Check for these drift indicators:
1. Spec says function exists but it's not in the module
2. Spec behavior doesn't match implementation
3. Implementation has functions not in spec (potential scope creep)
4. Documentation in spec is stale compared to code

Return findings as sexp:
(findings
  (finding type:"phantom-spec|missing-implementation|doc-stale|scope-creep"
    (function NAME)
    (detail "description")
    (confidence high|medium|low))
  ...)

If no drift detected, return: (findings)
|};
  
  Buffer.contents buf

(** Estimate token count (rough approximation) *)
let estimate_tokens text =
  (* Rough estimate: ~4 chars per token for English text *)
  String.length text / 4

(** Split functions into batches that fit within token limit *)
let batch_functions (functions : Extract_functions.function_info list) ~max_tokens =
  let rec batch acc current_batch current_tokens = function
    | [] ->
        if current_batch = [] then List.rev acc
        else List.rev (List.rev current_batch :: acc)
    | f :: rest ->
        let prompt = build_function_prompt f in
        let tokens = estimate_tokens prompt in
        if current_tokens + tokens > max_tokens && current_batch <> [] then
          batch (List.rev current_batch :: acc) [f] tokens rest
        else
          batch acc (f :: current_batch) (current_tokens + tokens) rest
  in
  batch [] [] 0 functions
