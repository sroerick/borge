(* Lightweight prompt builder with minimal context

   Instead of loading full .borg files, we extract:
   - Planned sections (names + docs) — the backlog
   - Implemented section names — guardrails
   - Convention name only (not full rules)
   - Module names only (not exports)

   This keeps the prompt under ~2K tokens instead of 10K+ *)

type role =
  | Implementer
  | Spec_writer
  | Observer
  | Fixer

let role_name = function
  | Implementer -> "implementer"
  | Spec_writer -> "spec-writer"
  | Observer -> "observer"
  | Fixer -> "fixer"

let role_description = function
  | Implementer ->
    "You are an implementer. Make code match planned sections. " ^
    "You may write code and tests. You may NOT modify .borg files."
  | Spec_writer ->
    "You are a spec-writer. Edit .borg files directly to describe intent. " ^
    "You may NOT write code files."
  | Observer ->
    "You are an observer. Check that code matches spec. Read-only."
  | Fixer ->
    "You are a fixer. Repair drift by writing code. You may NOT modify .borg."

(* Minimal section info: name, status, and short doc *)
type section_summary = {
  name : string;
  status : string;
  doc : string;
}

(* Extract planned and implemented sections from a parsed file *)
let summarize_borg_file content : section_summary list =
  (* Simple extraction: look for (section ... (status X) ... (doc "...")) *)
  let lines = String.split_on_char '\n' content in
  let sections = ref [] in
  let current_section = ref None in
  let current_status = ref "" in
  let current_doc = ref "" in
  
  List.iter (fun line ->
    let trimmed = String.trim line in
    
    (* Match (section name *)
    if String.length trimmed > 10 && 
       String.sub trimmed 0 9 = "(section " then
      begin
        let rest = String.sub trimmed 9 (String.length trimmed - 9) in
        (* Extract section name until space or paren *)
        let name_end = try String.index rest ' ' with Not_found -> 
                       try String.index rest ')' with Not_found -> String.length rest in
        let name = String.sub rest 0 name_end in
        current_section := Some name;
        current_status := "";
        current_doc := ""
      end;
    
    (* Match (status X) *)
    if String.length trimmed > 9 && 
       String.sub trimmed 0 8 = "(status " then
      begin
        let rest = String.sub trimmed 8 (String.length trimmed - 8) in
        let status_end = try String.index rest ')' with Not_found -> String.length rest in
        let status = String.sub rest 0 status_end in
        current_status := status
      end;
    
    (* Match (doc "...") *)
    if String.length trimmed > 7 && 
       String.sub trimmed 0 6 = "(doc \"" then
      begin
        let rest = String.sub trimmed 6 (String.length trimmed - 6) in
        (* Extract until closing quote *)
        try
          let doc_end = String.index rest '"' in
          let doc = String.sub rest 0 doc_end in
          current_doc := doc
        with Not_found -> ()
      end;
    
    (* On section close, save if has name *)
    if !current_section <> None && (String.length trimmed = 0 || trimmed.[0] = ')') then
      begin
        let name = Option.get !current_section in
        if !current_status = "planned" || !current_status = "implemented" then
          sections := { name; status = !current_status; doc = !current_doc } :: !sections;
        current_section := None
      end
  ) lines;
  
  List.rev !sections

let make_prompt_minimal (role : role) ~(planned : section_summary list)
    ~(implemented : section_summary list) ~(modules : string list) : string =
  let buf = Buffer.create 2048 in

  Buffer.add_string buf (role_description role);
  Buffer.add_string buf "\n\n";

  (* Backlog: planned sections *)
  if planned <> [] then begin
    Buffer.add_string buf "## BACKLOG (planned) ##\n";
    List.iter (fun s ->
      Buffer.add_string buf (Printf.sprintf "- %s: %s\n" s.name s.doc)
    ) planned;
    Buffer.add_string buf "\n"
  end;

  (* Guardrails: implemented sections *)
  if implemented <> [] then begin
    Buffer.add_string buf "## GUARDRAILS (implemented) ##\n";
    List.iter (fun s ->
      Buffer.add_string buf (Printf.sprintf "- %s\n" s.name)
    ) implemented;
    Buffer.add_string buf "\n"
  end;

  (* Existing modules for context *)
  if modules <> [] then begin
    Buffer.add_string buf "## EXISTING MODULES ##\n";
    List.iter (fun m ->
      Buffer.add_string buf (Printf.sprintf "- %s\n" m)
    ) modules;
    Buffer.add_string buf "\n"
  end;

  Buffer.contents buf

(* DEPRECATED: full context version preserved for reference *)
let make_prompt = make_prompt_minimal
