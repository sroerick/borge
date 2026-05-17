(** LLM-powered code quality review.

    Compare code against its spec section and generate quality findings:
    does the implementation match, are exports documented, are there
    dead functions, is error handling present? *)

open Borge_lang

type review_finding = {
  section : string;
  file : string;
  checks : string list;  (** which checks passed *)
  issues : string list;  (** which checks failed with detail *)
  quality : [ `high | `medium | `low ];
}

let build_review_prompt section_name borg_path spec_text source_path source_content =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "You are reviewing code quality against a specification.\n";
  Buffer.add_string buf "Compare the spec section with the source code and assess:\n\n";
  Buffer.add_string buf "1. Does every spec-described feature exist in code?\n";
  Buffer.add_string buf "2. Are all exports documented? (check .mli or docstrings)\n";
  Buffer.add_string buf "3. Are there dead functions (unused exports)?\n";
  Buffer.add_string buf "4. Is error handling present where the spec says 'returns error'?\n";
  Buffer.add_string buf "5. Is the literate description accurate?\n\n";
  Buffer.add_string buf (Printf.sprintf "Section: %s\n" section_name);
  Buffer.add_string buf (Printf.sprintf "Spec file: %s\n\n" borg_path);
  Buffer.add_string buf "--- SPEC ---\n";
  Buffer.add_string buf spec_text;
  Buffer.add_string buf "\n--- END SPEC ---\n\n";
  Buffer.add_string buf (Printf.sprintf "Source file: %s\n" source_path);
  Buffer.add_string buf "--- CODE ---\n";
  Buffer.add_string buf source_content;
  Buffer.add_string buf "\n--- END CODE ---\n\n";
  Buffer.add_string buf "Return ONLY a sexp review:\n";
  Buffer.add_string buf "(review\n";
  Buffer.add_string buf "  (section SECTION_NAME)\n";
  Buffer.add_string buf "  (file FILE_PATH)\n";
  Buffer.add_string buf "  (quality high|medium|low)\n";
  Buffer.add_string buf "  (checks (pass \"description\") ...)\n";
  Buffer.add_string buf "  (issues (fail \"description\") ...))\n\n";
  Buffer.add_string buf "If no issues, return:\n";
  Buffer.add_string buf "(review (section SECTION) (file FILE) (quality high) (checks) (issues))\n";
  Buffer.contents buf

let review_section dir borg_path section_name =
  (* Find the section text in the .borg file *)
  let input = File_utils.read_file borg_path in
  let len = String.length input in
  let target = Printf.sprintf "(section %s" section_name in
  let start =
    let rec search i =
      if i + String.length target > len then 0
      else if String.sub input i (String.length target) = target then i
      else search (i + 1)
    in search 0
  in
  let spec_text =
    if start = 0 then "(section unknown)"
    else begin
      (* Find the next section start after this one *)
      let next_section =
        let rec search i =
          if i + 8 > len then len
          else if i > start + String.length target &&
                  String.sub input i 8 = "(section" then i
          else search (i + 1)
        in search (start + 1)
      in
      String.trim (String.sub input start (next_section - start))
    end
  in
  (* Find the source file for this section *)
  let ml_files = Stats.find_ml_files dir in
  let matching_ml = List.filter (fun p ->
    let base = Filename.chop_extension (Filename.basename p) in
    String.lowercase_ascii base = String.lowercase_ascii section_name ||
    String.lowercase_ascii base = String.lowercase_ascii (section_name ^ "_cmd")
  ) ml_files in
  match matching_ml with
  | ml_path :: _ ->
    let source_content = File_utils.read_file ml_path in
    let prompt = build_review_prompt section_name borg_path spec_text ml_path source_content in
    let response = Agent.run_pi_print prompt in
    (match response with
     | Some text -> Some (Agent.extract_sexp_from_response text)
     | None -> None),
    ml_path
  | [] ->
    Printf.eprintf "  No source file found for section '%s'\n" section_name;
    None, ""

(** Run quality review on all implemented sections *)
let run dir =
  let borg_files = File_utils.find_borg_files dir in
  let findings = ref [] in
  List.iter (fun borg_path ->
    try
      let input = File_utils.read_file borg_path in
      let file = Parse.parse_file input in
      List.iter (fun { Ast.node; _ } ->
        (match node with
         | Ast.List (_, Ast.Atom (_, "section") :: Ast.Atom (_, name) ::
                        Ast.List (_, Ast.Atom (_, "status") :: Ast.Atom (_, status) :: _) :: _)
           when status = "implemented" ->
             let result, ml_path = review_section dir borg_path name in
             (match result with
              | Some sexp ->
                  findings := (name, ml_path, sexp) :: !findings
              | None -> ())
         | _ -> ())
      ) file.Ast.top_level
    with _ -> ()
  ) borg_files;
  List.rev !findings
