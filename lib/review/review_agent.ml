(* LLM integration for semantic review
 *
 * roerick note (|
 *   Handles calling the LLM via BORGE_AGENT_CMD for semantic review.
 *   Uses the same Agent.run_pi_print mechanism as other borge
 *   generation commands for consistency.
 * |) *)

open Review_types

(** Run semantic review on a single function *)
let review_function (info : Extract_functions.function_info) : function_finding option =
  let prompt = Review_prompt.build_function_prompt info in
  match Agent.run_pi_print prompt with
  | None ->
      Printf.eprintf "Warning: No LLM response for function %s\n" info.name;
      None
  | Some response ->
      let timestamp =
        let now = Unix.gmtime (Unix.time ()) in
        Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
          (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday
          now.tm_hour now.tm_min now.tm_sec
      in
      match Review_parse.parse_response_with_timestamp response timestamp with
      | Ok [finding] ->
          Some finding
      | Ok (finding :: rest) ->
          (* Multiple findings, take the first or the one matching our function name *)
          if info.name = finding.name then
            Some finding
          else
            List.find_opt (fun (f : function_finding) -> f.name = info.name) (finding :: rest)
      | Ok [] ->
          (* No findings - assume no issues *)
          Some {
            name = info.name;
            doc_present = info.docstring <> None;
            doc_status = if info.docstring = None then Doc_missing else Doc_drifted;
            doc_accuracy = if info.docstring = None then Low else Medium;
            consistency = Cons_consistent;
            internal_issues = None;
            signature_match = Unknown;
            behavior_coverage = if info.docstring = None then Missing else Partial;
            structural_issues = None;
            confidence = Low;
            checked_at = timestamp;
          }
      | Error e ->
          Printf.eprintf "Warning: Failed to parse LLM response for %s: %s\n" info.name e;
          None

(** Run semantic review on a batch of functions *)
let review_batch (functions : Extract_functions.function_info list) : function_finding list =
  if functions = [] then []
  else if List.length functions = 1 then
    match review_function (List.hd functions) with
    | Some f -> [f]
    | None -> []
  else
    (* Batch review *)
    let prompt = Review_prompt.build_batch_prompt functions in
    match Agent.run_pi_print prompt with
    | None ->
        Printf.eprintf "Warning: No LLM response for batch review\n";
        []
    | Some response ->
        let timestamp =
          let now = Unix.gmtime (Unix.time ()) in
          Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
            (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday
            now.tm_hour now.tm_min now.tm_sec
        in
        match Review_parse.parse_response_with_timestamp response timestamp with
        | Ok findings -> findings
        | Error e ->
            Printf.eprintf "Warning: Failed to parse batch response: %s\n" e;
            []

(** Run semantic review on a file *)
let review_file path =
  Printf.printf "Reviewing %s...\n%!" path;
  
  (* Extract functions from file *)
  let functions = Extract_functions.extract_from_file path in
  
  if functions = [] then begin
    Printf.printf "  No functions found in %s\n" path;
    None
  end else begin
    Printf.printf "  Found %d functions\n" (List.length functions);
    
    (* Batch functions to respect token limits *)
    let batches = Review_prompt.batch_functions functions ~max_tokens:6000 in
    Printf.printf "  Split into %d batches\n" (List.length batches);
    
    (* Review each batch *)
    let all_findings = List.concat_map review_batch batches in
    
    let timestamp =
      let now = Unix.gmtime (Unix.time ()) in
      Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
        (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday
        now.tm_hour now.tm_min now.tm_sec
    in
    
    Some {
      source_file = path;
      reviewed_at = timestamp;
      reviewer = "agent:semantic-reviewer";
      functions = all_findings;
    }
  end

(** Run semantic review comparing spec to implementation *)
let review_spec_section ~spec_section ~module_surface ~impl_path =
  let functions = Extract_functions.extract_from_file impl_path in
  
  let prompt = Review_prompt.build_spec_comparison_prompt
      ~spec_section
      ~module_surface
      ~functions
  in
  
  match Agent.run_pi_print prompt with
  | None ->
      Printf.eprintf "Warning: No LLM response for spec comparison\n";
      None
  | Some response ->
      (* For spec comparison, we return the raw response for now
         - would need additional parsing for drift findings *)
      Some response
