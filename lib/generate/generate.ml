(** Generate spec sections and code from specs.

    Two-step pipeline:
    1. borge generate spec — turns a todo into a .borg section
    2. borge generate code — implements a planned section from its spec

    Both use pi -p (single-shot) via Agent.run_pi_print. *)

open Borge_lang

(** {1 Generate spec: todo → .borg section} *)

let build_spec_prompt todo_text project_name convention =
  let buf = Buffer.create 512 in
  Buffer.add_string buf "You are a specification writer.\n";
  Buffer.add_string buf "Given a task description, produce a .borg section.\n\n";
  Buffer.add_string buf "Rules:\n";
  Buffer.add_string buf "- Use (section NAME) as the top-level form\n";
  Buffer.add_string buf "- Include (doc \"...\") with a one-line description\n";
  Buffer.add_string buf "- Set (status planned)\n";
  Buffer.add_string buf "- Add (details (doc (| ... |))) with implementation notes\n";
  Buffer.add_string buf "- Add (subsection ...) for each logical unit if needed\n";
  Buffer.add_string buf "- Return ONLY the sexp, no markdown fences, no commentary\n\n";
  Buffer.add_string buf (Printf.sprintf "Project: %s\n" project_name);
  (match convention with
   | Some c -> Buffer.add_string buf (Printf.sprintf "Convention: %s\n" c)
   | None -> ());
  Buffer.add_string buf (Printf.sprintf "\nTask:\n%s\n" todo_text);
  Buffer.add_string buf "\n\nProduce a .borg section:\n";
  Buffer.contents buf

(* agent note (|
 *   WHAT: Generate a .borg spec section from a todo description
 *   using the LLM. Parses the result and returns it as a string.
 *
 *   WHY: The borge spec --spec command uses this to turn a human
 *   todo into structured borg specification.
 * |) *)
let generate_spec todo_text dir =
  let project_name =
    let roots = Project.find_roots dir in
    match roots with
    | [root] ->
      (try
        let input = File_utils.read_file root in
        let file = Parse.parse_file input in
        (match Spec.project_name file with Some n -> n | None -> "unknown")
      with _ -> "unknown")
    | _ -> "unknown"
  in
  let prompt = build_spec_prompt todo_text project_name (Some (Convention.name (Convention.resolve dir))) in
  let response = Agent.run_pi_print prompt in
  match response with
  | None ->
    Printf.eprintf "Error: no LLM response. Install pi or set BORGE_AGENT_CMD.\n";
    None
  | Some text ->
    (* Try to extract just the sexp from the response *)
    let sexp = Agent.extract_sexp_from_response text in
    (* Validate it parses *)
    try
      let _ = Parse.parse_file sexp in
      Some sexp
    with _ ->
      Printf.eprintf "Warning: LLM output is not valid sexp. Raw output:\n%s\n" text;
      Some text

(** {1 Generate code: spec → implementation} *)

(* agent note (|
 *   WHAT: Generate implementation code from a spec section using LLM.
 *   Takes the section name, path to borg file, spec text, and surfaces.
 *
 *   WHY: The borge spec --code command uses this to have an agent
 *   implement a planned section.
 * |) *)

let build_code_prompt section_name borg_path spec_text surfaces =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "You are implementing a planned section of a specification.\n";
  Buffer.add_string buf "Read the spec and write the implementation code.\n\n";
  Buffer.add_string buf (Printf.sprintf "Section: %s\n" section_name);
  Buffer.add_string buf (Printf.sprintf "Spec file: %s\n\n" borg_path);
  Buffer.add_string buf "--- SPEC SECTION ---\n";
  Buffer.add_string buf spec_text;
  Buffer.add_string buf "\n--- END SPEC ---\n\n";
  if surfaces <> [] then begin
    Buffer.add_string buf "--- EXISTING MODULE SURFACES ---\n";
    List.iter (fun (s : Surface.module_surface) ->
      Buffer.add_string buf (Printf.sprintf "Module %s (%s): %s\n"
        s.module_name s.path (String.concat ", " s.exports))
    ) surfaces;
    Buffer.add_string buf "--- END SURFACES ---\n\n"
  end;
  Buffer.add_string buf "Rules:\n";
  Buffer.add_string buf "- Write the implementation in OCaml\n";
  Buffer.add_string buf "- Follow existing project conventions\n";
  Buffer.add_string buf "- Add the module to the dune (modules ...) stanza\n";
  Buffer.add_string buf "- Make sure dune build passes\n";
  Buffer.add_string buf "- Return ONLY the code, no markdown fences\n";
  Buffer.contents buf

(** Find a section by name in a parsed .borg file *)
let find_section_text borg_path section_name =
  try
    let input = File_utils.read_file borg_path in
    let len = String.length input in
    let start =
      let target = Printf.sprintf "(section %s" section_name in
      let rec search i =
        if i + String.length target > len then raise Not_found
        else if String.sub input i (String.length target) = target then i
        else search (i + 1)
      in
      search 0
    in
    (* Find the matching closing paren by walking from start *)
    let depth = ref 0 in
    let fin = ref len in
    for i = start to len - 1 do
      if !fin = len then ()  (* already found end *)
      else begin
        if input.[i] = '(' then incr depth
        else if input.[i] = ')' then begin
          decr depth;
          if !depth = 0 then fin := i + 1
        end
      end
    done;
    String.trim ((* exempt: String.sub *) String.sub input start (!fin - start))
  with _ ->
    Printf.sprintf "(section %s (status planned))" section_name

let generate_code section_name dir =
  let borg_files = File_utils.find_borg_files dir in
  let dune_file = Dune_parse.parse_all dir in
  let lib_dune_files = List.filter (fun df ->
    let d = Filename.dirname df.Dune_parse.path in
    String.length d >= 5 && String.sub d 0 5 = "./lib"
  ) dune_file in
  let lib_modules = List.concat_map (fun df ->
    let dir = Filename.dirname df.Dune_parse.path in
    List.concat_map (function
      | Dune_parse.Library lib ->
          List.map (fun m -> (dir, m)) lib.Dune_parse.modules
      | _ -> []
    ) df.Dune_parse.stanzas
  ) lib_dune_files in
  let all_surfaces = List.filter_map (fun (dir, mod_name) ->
    let ml_path = Filename.concat dir (mod_name ^ ".ml") in
    if Sys.file_exists ml_path then
      Some (Surface.extract_surface ml_path)
    else None
  ) lib_modules in
  let matching_borg = List.filter (fun p ->
    try
      let input = File_utils.read_file p in
      let file = Parse.parse_file input in
      let found = ref false in
      List.iter (fun { Ast.node; _ } ->
        (match node with
         | Ast.List (_, Ast.Atom (_, "section") :: Ast.Atom (_, name) :: _) ->
             if name = section_name then found := true
         | _ -> ());
      ) file.Ast.top_level;
      !found
    with _ -> false
  ) borg_files in
  match matching_borg with
  | [] ->
    Printf.eprintf "Error: no .borg file contains section '%s'\n" section_name;
    None
  | borg_path :: _ ->
    let spec_text = find_section_text borg_path section_name in
    let prompt = build_code_prompt section_name borg_path spec_text all_surfaces in
    let response = Agent.run_pi_print prompt in
    match response with
    | None ->
      Printf.eprintf "Error: no LLM response. Install pi or set BORGE_AGENT_CMD.\n";
      None
    | Some text ->
      Printf.printf "%s\n" text;
      Some text
