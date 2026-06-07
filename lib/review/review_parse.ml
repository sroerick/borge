(* Parse LLM responses into typed semantic findings
 *
 * roerick note (|
 *   Parses sexp responses from semantic review LLM queries.
 *   Never raises on parse failure — always returns result type
 *   to avoid corrupting .borg.meta files.
 * |) *)

open Review_types

(** Parse result type *)
type 'a parse_result =
  | Ok of 'a
  | Error of string

(** Extract sexp from response (handles markdown fences) *)
let extract_sexp response =
  let lines = String.split_on_char '\n' response in
  let rec find_start = function
    | [] -> None
    | line :: rest ->
        let trimmed = String.trim line in
        if String.length trimmed >= 2 && String.sub trimmed 0 2 = "(" then
          Some (line :: rest)
        else if trimmed = "```" then
          find_end rest
        else
          find_start rest
  and find_end = function
    | [] -> None
    | line :: rest ->
        if String.trim line = "```" then
          find_start rest
        else
          Some (line :: "(" :: rest) (* Add opening paren if missing *)
  in
  match find_start lines with
  | Some lines ->
      let sexp_str = String.concat "\n" lines in
      Some sexp_str
  | None -> None

(** Parse a single sexp atom *)
let parse_atom s =
  let s = String.trim s in
  if String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"' then
    String.sub s 1 (String.length s - 2)
  else
    s

(** Simple sexp parsing - extracts key-value pairs from function blocks *)
let rec parse_function_fields acc = function
  | [] -> Ok (List.rev acc)
  | field :: rest ->
      let field = String.trim field in
      if String.length field = 0 then
        parse_function_fields acc rest
      else if field.[0] = '(' then
        (* Extract field name and value *)
        let close_paren = try String.index field ')' with Not_found -> String.length field - 1 in
        let inner = String.sub field 1 (close_paren - 1) in
        let parts = String.split_on_char ' ' (String.trim inner) in
        (match parts with
         | name :: values ->
             let value = String.concat " " values in
             parse_function_fields ((name, value) :: acc) rest
         | [] -> parse_function_fields acc rest)
      else
        parse_function_fields acc rest

(** Parse function block from sexp string *)
let parse_function_block sexp_str =
  (* Extract name from (function name:"..." ...) *)
  let name_pattern = Str.regexp "name:\"\\([^\"]*\\)\"" in
  if Str.string_match name_pattern sexp_str 0 then
    let name = Str.matched_group 1 sexp_str in
    
    (* Extract other fields *)
    let doc_present =
      if Str.string_match (Str.regexp "(doc-present[ 	]+true") sexp_str 0 then true
      else if Str.string_match (Str.regexp_string "(doc-present false)") sexp_str 0 then false
      else false
    in
    
    let doc_accuracy =
      if Str.string_match (Str.regexp "(doc-accuracy[ 	]+high") sexp_str 0 then High
      else if Str.string_match (Str.regexp "(doc-accuracy[ 	]+medium") sexp_str 0 then Medium
      else if Str.string_match (Str.regexp "(doc-accuracy[ 	]+low") sexp_str 0 then Low
      else Low
    in
    
    let signature_match =
      if Str.string_match (Str.regexp "(signature-match[ 	]+accurate") sexp_str 0 then Accurate
      else if Str.string_match (Str.regexp "(signature-match[ 	]+mismatch") sexp_str 0 then Mismatch
      else Unknown
    in
    
    let (behavior_coverage : behavior_coverage) =
      if Str.string_match (Str.regexp "(behavior-coverage[ 	]+complete") sexp_str 0 then Complete
      else if Str.string_match (Str.regexp "(behavior-coverage[ 	]+partial") sexp_str 0 then Partial
      else Missing
    in
    
    let structural_issues =
      if Str.string_match (Str.regexp "(structural-issues[\t ]+\"") sexp_str 0 then
        let issues_start = Str.match_end () in
        match String.index_from sexp_str issues_start '"' with
        | exception Not_found -> None
        | issues_end ->
            let issues = String.sub sexp_str issues_start (issues_end - issues_start) in
            if issues = "none" || issues = "" then None else Some issues
      else
        None
    in
    
    let (conf : confidence) =
      if Str.string_match (Str.regexp "(confidence[ 	]+high") sexp_str 0 then High
      else if Str.string_match (Str.regexp "(confidence[ 	]+medium") sexp_str 0 then Medium
      else Low
    in
    
    let doc_status =
      if Str.string_match (Str.regexp "(doc-status[ \t]+accurate") sexp_str 0 then Doc_accurate
      else if Str.string_match (Str.regexp "(doc-status[ \t]+drifted") sexp_str 0 then Doc_drifted
      else if Str.string_match (Str.regexp "(doc-status[ \t]+missing") sexp_str 0 then Doc_missing
      else if doc_present then Doc_accurate else Doc_missing
    in
    
    let (consistency : consistency) =
      if Str.string_match (Str.regexp "(consistency[ \t]+consistent") sexp_str 0 then Cons_consistent
      else if Str.string_match (Str.regexp "(consistency[ \t]+questionable") sexp_str 0 then Cons_questionable
      else if Str.string_match (Str.regexp "(consistency[ \t]+inconsistent") sexp_str 0 then Cons_inconsistent
      else Cons_consistent
    in
    
    let internal_issues =
      if Str.string_match (Str.regexp "(internal-issues[\t ]+\"") sexp_str 0 then
        let issues_start = Str.match_end () in
        match String.index_from sexp_str issues_start '"' with
        | exception Not_found -> None
        | issues_end ->
            let issues = String.sub sexp_str issues_start (issues_end - issues_start) in
            if issues = "none" || issues = "" then None else Some issues
      else
        None
    in
    
    Ok {
      name;
      doc_present;
      doc_status;
      doc_accuracy;
      consistency;
      internal_issues;
      signature_match;
      behavior_coverage;
      structural_issues;
      confidence = conf;
      checked_at = "";  (* Will be filled in later *)
    }
  else
    Error "No name found in function block"

(** Parse a (findings ...) block containing multiple (function ...) blocks *)
let parse_findings response =
  match extract_sexp response with
  | None -> Error "No sexp found in response"
  | Some sexp_str ->
      (* Find all (function ...) blocks *)
      let function_pattern = Str.regexp "(function[ 	]+" in
      let rec find_functions acc pos =
        try
          let _ = Str.search_forward function_pattern sexp_str pos in
          let start = Str.match_beginning () in
          (* Find matching closing paren by counting *)
          let depth = ref 1 in
          let i = ref (start + 9) in  (* Skip "(function" *)
          while !depth > 0 && !i < String.length sexp_str do
            match sexp_str.[!i] with
            | '(' -> incr depth; incr i
            | ')' -> decr depth; incr i
            | '"' ->  (* Skip string *)
                incr i;
                while !i < String.length sexp_str && sexp_str.[!i] <> '"' do
                  if sexp_str.[!i] = '\\' then incr i;
                  incr i
                done;
                incr i
            | _ -> incr i
          done;
          let func_str = String.sub sexp_str start (!i - start) in
          find_functions (func_str :: acc) !i
        with Not_found -> List.rev acc
      in
      
      let function_blocks = find_functions [] 0 in
      let findings = List.filter_map (fun block ->
        match parse_function_block block with
        | Ok finding -> Some finding
        | Error _ -> None
      ) function_blocks in
      
      if findings = [] && not (String.contains sexp_str '(') then
        Error "No findings could be parsed"
      else
        Ok findings

(** Parse response with timestamp *)
let parse_response_with_timestamp response timestamp =
  match parse_findings response with
  | Ok findings ->
      Ok (List.map (fun f -> { f with checked_at = timestamp }) findings)
  | Error e ->
      Error e

(** Returns true if response is valid sexp findings *)
let is_valid_response response =
  match parse_findings response with
  | Ok [] -> true  (* Empty findings is valid *)
  | Ok _ -> true
  | Error _ -> false
