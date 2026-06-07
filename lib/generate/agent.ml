(** Agent-driven drift analysis via pi in print mode.

    Uses pi -p (single-shot, process and exit) to send structured
    analysis prompts and parse sexp findings. No RPC protocol — a
    simple stdin/stdout pipe through temp files. No timeout — runs
    until the model completes.

    Fallback: BORGE_AGENT_CMD env var for custom LLM invocation. *)

open Borge_lang

(** {1 Prompt assembly} *)

let build_prompt borg_path surfaces findings =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "You are analyzing a codebase against its specification.\n";
  Buffer.add_string buf "For each section below, compare the spec description with\n";
  Buffer.add_string buf "the source code and return findings.\n\n";
  Buffer.add_string buf (Printf.sprintf "Spec file: %s\n" borg_path);
  (try
    let content = File_utils.read_file borg_path in
    Buffer.add_string buf "\n--- SPEC ---\n";
    Buffer.add_string buf content;
    Buffer.add_string buf "\n--- END SPEC ---\n"
  with _ -> ());
  if surfaces <> [] then begin
    Buffer.add_string buf "\n--- MODULE SURFACES ---\n";
    List.iter (fun (s : Surface.module_surface) ->
      Buffer.add_string buf (Printf.sprintf "Module %s (%s):\n  exports: %s\n"
        s.module_name s.path (String.concat ", " s.exports))
    ) surfaces;
    Buffer.add_string buf "--- END SURFACES ---\n"
  end;
  if findings <> [] then begin
    Buffer.add_string buf "\n--- EXISTING STATIC FINDINGS ---\n";
    List.iter (fun (f : Meta.finding) ->
      Buffer.add_string buf (Printf.sprintf "  [%s/%s] %s\n"
        (Meta.string_of_source f.Meta.source)
        (Meta.string_of_confidence f.Meta.confidence)
        (match f.Meta.detail with Some d -> d | None -> Meta.string_of_finding_type f.Meta.ft_type))
    ) findings;
    Buffer.add_string buf "--- END FINDINGS ---\n"
  end;
  Buffer.add_string buf "\nReturn ONLY a sexp list of findings. Each finding is:\n";
  Buffer.add_string buf "(finding TYPE\n";
  Buffer.add_string buf "  (section SECTION_NAME)\n";
  Buffer.add_string buf "  (detail EXPLANATION)\n";
  Buffer.add_string buf "  (confidence LEVEL))\n\n";
  Buffer.add_string buf "Valid types: partial-implementation, status-mismatch,\n";
  Buffer.add_string buf "             missing-export, extra-export, phantom-spec\n";
  Buffer.add_string buf "Valid confidence levels: high, medium, low\n\n";
  Buffer.add_string buf "If no findings, return: (findings)\n\n";
  Buffer.add_string buf "Example:\n";
  Buffer.add_string buf "(findings\n";
  Buffer.add_string buf "  (finding partial-implementation\n";
  Buffer.add_string buf "    (section drift)\n";
  Buffer.add_string buf "    (detail \"spec mentions undocumented-bindings check but it was removed\")\n";
  Buffer.add_string buf "    (confidence high)))\n";
  Buffer.contents buf

(** {1 Agent output parsing} *)

let parse_agent_response response timestamp =
  try
    let file = Parse.parse_file response in
    let findings = ref [] in
    let rec walk = function
      | [] -> ()
      | { Ast.node; _ } :: rest ->
        (match node with
         | Ast.List (_, Ast.Atom (_, "findings") :: items) ->
             List.iter (fun item ->
               match item with
               | Ast.List (_, Ast.Atom (_, "finding") :: Ast.Atom (_, type_str) :: fields) ->
                   let ft_type = match type_str with
                     | "partial-implementation" -> Meta.Partial_implementation
                     | "status-mismatch" -> Meta.Status_mismatch
                     | "missing-export" -> Meta.Missing_export
                     | "extra-export" -> Meta.Extra_export
                     | "phantom-spec" -> Meta.Phantom_spec
                     | _ -> Meta.Partial_implementation
                   in
                   let section = List.find_map (function
                     | Ast.List (_, Ast.Atom (_, "section") :: Ast.Atom (_, s) :: _) -> Some s
                     | _ -> None
                   ) fields in
                   let detail = List.find_map (function
                     | Ast.List (_, Ast.Atom (_, "detail") :: Ast.String (_, Ast.Quoted q) :: _) ->
                         Some q.Ast.q_content
                     | Ast.List (_, Ast.Atom (_, "detail") :: Ast.String (_, Ast.Verbatim v) :: _) ->
                         Some v.Ast.v_content
                     | _ -> None
                   ) fields in
                   let confidence =
                     let c_opt = List.find_map (function
                       | Ast.List (_, Ast.Atom (_, "confidence") :: Ast.Atom (_, c) :: _) ->
                           (match c with
                            | "high" -> Some Meta.High
                            | "medium" -> Some Meta.Medium
                            | "low" -> Some Meta.Low
                            | _ -> Some Meta.Low)
                       | _ -> None
                     ) fields in
                     match c_opt with Some c -> c | None -> Meta.Low
                   in
                   findings := {
                     Meta.ft_type;
                     Meta.section;
                     Meta.module_ = None;
                     Meta.file = None;
                     Meta.export = None;
                     Meta.detail;
                     Meta.spec_status = None;
                     Meta.actual_status = None;
                     Meta.confidence;
                     Meta.source = Meta.Agent;
                     Meta.at = timestamp;
                   } :: !findings
               | _ -> ()
             ) items
         | _ -> ()
        );
        walk rest
    in
    walk file.Ast.top_level;
    List.rev !findings
  with _ ->
    []

(** {1 Pi -p mode integration} *)

let find_pi () =
  try
    let p = Sys.getenv "BORGE_PI_PATH" in
    if Sys.file_exists p then Some p else None
  with Not_found ->
  try
    let ic = Unix.open_process_in "command -v pi 2>/dev/null" in
    let path = String.trim (input_line ic) in
    let _ = Unix.close_process_in ic in
    if String.length path > 0 && Sys.file_exists path then Some path
    else None
  with _ -> None

let extract_sexp_from_response text =
  (* Scan the response for a sexp starting with (findings or (finding.
     Returns the substring from the first (findings) we find, or
     the full text if no clear starting point. *)
  let len = String.length text in
  let rec find start =
    if start >= len then
      text  (* no sexp found, return whole thing for parse attempt *)
    else if start + 8 <= len &&
            (* exempt: String.sub *) String.sub text start 8 = "(findings" then
      (* exempt: String.sub *) String.sub text start (len - start)
    else if start + 8 <= len &&
            (* exempt: String.sub *) String.sub text start 8 = "(finding " then
      (* exempt: String.sub *) String.sub text start (len - start)
    else
      find (start + 1)
  in
  find 0

let run_pi_print prompt =
  match find_pi () with
  | None -> None
  | Some pi_path ->
    let tmp_prompt = Filename.temp_file "borge_prompt" ".txt" in
    let tmp_out = Filename.temp_file "borge_response" ".txt" in
    let oc_prompt = open_out tmp_prompt in
    output_string oc_prompt prompt;
    close_out oc_prompt;
    Printf.eprintf "  pi: running semantic analysis...";
    flush stderr;
    let cmd = Printf.sprintf "%s -p --no-session --no-tools < '%s' > '%s' 2>&1"
      pi_path tmp_prompt tmp_out in
    let start_time = Unix.gettimeofday () in
    let pid = Unix.fork () in
    if pid = 0 then begin
      let _ = Sys.command cmd in
      exit 0
    end else begin
      let running = ref true in
      while !running do
        match Unix.waitpid [Unix.WNOHANG] pid with
        | 0, _ -> Printf.eprintf "."; flush stderr; Unix.sleep 1
        | _, _ -> running := false
      done;
      let elapsed = Unix.gettimeofday () -. start_time in
      Printf.eprintf " done (%.1fs)\n" elapsed;
      let response =
        try File_utils.read_file tmp_out with _ -> "" in
      Sys.remove tmp_prompt;
      Sys.remove tmp_out;
      if String.length response > 0 then begin
        let sexp_part = extract_sexp_from_response response in
        if sexp_part != response then
          Printf.eprintf "  pi: extracted sexp from response\n";
        Some sexp_part
      end else begin
        Printf.eprintf "  pi: no response received\n";
        None
      end
    end

(** {1 Fallback: BORGE_AGENT_CMD} *)

let get_agent_cmd () =
  try Some (Sys.getenv "BORGE_AGENT_CMD")
  with Not_found -> None

let run_agent_cmd cmd prompt =
  Printf.eprintf "  Using BORGE_AGENT_CMD (%s)...\n" cmd;
  let tmp_in = Filename.temp_file "borge_agent" ".txt" in
  let tmp_out = Filename.temp_file "borge_agent" ".out" in
  let oc = open_out tmp_in in
  output_string oc prompt;
  close_out oc;
  let ret = Sys.command (Printf.sprintf "%s < %s > %s 2>&1" cmd tmp_in tmp_out) in
  let response = if ret = 0 then
    (try File_utils.read_file tmp_out with _ -> "") else "" in
  Sys.remove tmp_in;
  Sys.remove tmp_out;
  if ret = 0 then Some response else None

(** {1 Main agent entry point} *)

let run_agent dir =
  Printf.eprintf "  Running static analysis...\n";
  let timestamp = Meta.current_timestamp () in
  let meta = Drift.generate_findings dir in
  let n_static = List.length (List.filter (fun (f : Meta.finding) ->
    f.Meta.source = Meta.Static) meta.Meta.findings) in
  Printf.eprintf "  Static: %d findings. Starting agent...\n" n_static;
  let borg_files = File_utils.find_borg_files dir in
  let dune_files = Dune_parse.parse_all dir in
  let lib_dune_files = List.filter (fun df ->
    let d = Filename.dirname df.Dune_parse.path in
    String.length d >= 5 && String.sub d 0 5 = "./lib"
  ) dune_files in
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
  let agent_findings =
    let root_borg = List.filter (fun p ->
      Filename.basename p = "borge.borg") borg_files in
    List.concat_map (fun borg_path ->
      let prompt = build_prompt borg_path all_surfaces meta.Meta.findings in
      let response = match run_pi_print prompt with
        | Some r -> Some r
        | None ->
          (match get_agent_cmd () with
           | Some cmd -> run_agent_cmd cmd prompt
           | None ->
             Printf.eprintf "  No LLM available. Install pi or set BORGE_AGENT_CMD.\n";
             None)
      in
      match response with
      | Some r -> parse_agent_response r timestamp
      | None -> []
    ) root_borg
  in
  Printf.eprintf "  Agent: %d new findings.\n" (List.length agent_findings);
  let merged_findings = Meta.merge_agent_findings meta.Meta.findings agent_findings in
  let updated_meta = { meta with Meta.findings = merged_findings } in
  List.iter (fun borg_path ->
    if Filename.basename borg_path = "borge.borg" then begin
      let meta_path = Meta.meta_path_of borg_path in
      let project_name = try
        let input = File_utils.read_file borg_path in
        let file = Parse.parse_file input in
        Spec.project_name file
      with _ -> Some "borge"
      in
      let per_meta = { updated_meta with
        Meta.project_name = Option.value project_name ~default:"borge" } in
      Meta.write_meta meta_path per_meta
    end
  ) borg_files;
  (updated_meta, agent_findings)
