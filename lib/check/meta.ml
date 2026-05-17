(** .borg.meta file types and writer.

    .borg.meta files are companion files to .borg specs. They hold
    machine-generated observations about code reality: module surfaces,
    dune snapshots, and structured findings from static and agent analysis.

    Key invariants:
    - .borg.meta files are machine-written, never hand-edited
    - The writer is idempotent: same inputs → same output
    - Findings are sorted and deterministic for clean git diffs
    - Agent findings replace (not append) previous agent findings
    - Static findings are preserved across agent runs *)


(** {1 Types} *)

type confidence = High | Medium | Low

type finding_source = Static | Agent

type finding_type =
  | Unspecified_module
  | Phantom_spec
  | Partial_implementation
  | Status_mismatch
  | Missing_export
  | Extra_export
  | Structural_drift

type finding = {
  ft_type : finding_type;
  section : string option;
  module_ : string option;
  file : string option;
  export : string option;
  detail : string option;
  spec_status : string option;
  actual_status : string option;
  confidence : confidence;
  source : finding_source;
  at : string;  (** ISO 8601 timestamp *)
}

type module_surface_entry = {
  module_name : string;
  file : string;
  exports : string list;
}

type dune_library_entry = {
  name : string;
  modules : string list;
  public_name : string option;
  libraries : string list;
}

type content_hash = string

type meta = {
  project_name : string;
  analyzed_at : string;
  content_hashes : (string * content_hash) list;
  module_surfaces : module_surface_entry list;
  dune_snapshot : dune_library_entry list;
  findings : finding list;
}

(** {1 String conversions} *)

let string_of_confidence = function
  | High -> "high"
  | Medium -> "medium"
  | Low -> "low"

let string_of_source = function
  | Static -> "static"
  | Agent -> "agent"

let string_of_finding_type = function
  | Unspecified_module -> "unspecified-module"
  | Phantom_spec -> "phantom-spec"
  | Partial_implementation -> "partial-implementation"
  | Status_mismatch -> "status-mismatch"
  | Missing_export -> "missing-export"
  | Extra_export -> "extra-export"
  | Structural_drift -> "structural-drift"

(** {1 Sexp generation} *)

let sexp_of_string s =
  (* Simple: if s contains spaces or special chars, quote it *)
  if String.length s = 0 then "\"\""
  else begin
    let needs_quote = ref false in
    String.iter (fun c ->
      if c = ' ' || c = '(' || c = ')' || c = '"' || c = '\n' || c = '\t' then
        needs_quote := true
    ) s;
    if !needs_quote then
      let buf = Buffer.create (String.length s + 4) in
      Buffer.add_char buf '"';
      String.iter (fun c ->
        if c = '"' then Buffer.add_string buf "\\\""
        else if c = '\\' then Buffer.add_string buf "\\\\"
        else Buffer.add_char buf c
      ) s;
      Buffer.add_char buf '"';
      Buffer.contents buf
    else s
  end

let sexp_of_findings findings =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "(findings";
  List.iter (fun f ->
    Buffer.add_char buf '\n';
    Buffer.add_string buf "  (finding ";
    Buffer.add_string buf (string_of_finding_type f.ft_type);
    (match f.section with Some s -> Buffer.add_string buf (Printf.sprintf "\n    (section %s)" (sexp_of_string s)) | None -> ());
    (match f.module_ with Some s -> Buffer.add_string buf (Printf.sprintf "\n    (module %s)" (sexp_of_string s)) | None -> ());
    (match f.file with Some s -> Buffer.add_string buf (Printf.sprintf "\n    (file %s)" (sexp_of_string s)) | None -> ());
    (match f.export with Some s -> Buffer.add_string buf (Printf.sprintf "\n    (export %s)" (sexp_of_string s)) | None -> ());
    (match f.detail with Some s -> Buffer.add_string buf (Printf.sprintf "\n    (detail %s)" (sexp_of_string s)) | None -> ());
    (match f.spec_status with Some s -> Buffer.add_string buf (Printf.sprintf "\n    (spec-status %s)" (sexp_of_string s)) | None -> ());
    (match f.actual_status with Some s -> Buffer.add_string buf (Printf.sprintf "\n    (actual-status %s)" (sexp_of_string s)) | None -> ());
    Buffer.add_string buf (Printf.sprintf "\n    (confidence %s)" (string_of_confidence f.confidence));
    Buffer.add_string buf (Printf.sprintf "\n    (source %s)" (string_of_source f.source));
    Buffer.add_string buf (Printf.sprintf "\n    (at %s)" (sexp_of_string f.at));
    Buffer.add_string buf ")"
  ) findings;
  Buffer.add_string buf ")";
  Buffer.contents buf

let sexp_of_module_surfaces surfaces =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "(module-surface";
  List.iter (fun s ->
    Buffer.add_char buf '\n';
    Buffer.add_string buf (Printf.sprintf "  (%s\n    (file %s)" (sexp_of_string s.module_name) (sexp_of_string s.file));
    Buffer.add_string buf "\n    (exports";
    List.iter (fun e ->
      Buffer.add_char buf ' ';
      Buffer.add_string buf (sexp_of_string e)
    ) s.exports;
    Buffer.add_string buf "))";
  ) surfaces;
  Buffer.add_string buf ")";
  Buffer.contents buf

let sexp_of_dune_snapshot libs =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "(dune-snapshot";
  List.iter (fun lib ->
    Buffer.add_char buf '\n';
    Buffer.add_string buf (Printf.sprintf "  (library %s" (sexp_of_string lib.name));
    (match lib.public_name with
     | Some pn -> Buffer.add_string buf (Printf.sprintf "\n    (public_name %s)" (sexp_of_string pn))
     | None -> ());
    Buffer.add_string buf "\n    (modules";
    List.iter (fun m -> Buffer.add_char buf ' '; Buffer.add_string buf (sexp_of_string m)) lib.modules;
    Buffer.add_string buf ")";
    Buffer.add_string buf "\n    (libraries";
    List.iter (fun l -> Buffer.add_char buf ' '; Buffer.add_string buf (sexp_of_string l)) lib.libraries;
    Buffer.add_string buf "))";
  ) libs;
  Buffer.add_string buf ")";
  Buffer.contents buf

let sexp_of_content_hashes hashes =
  let buf = Buffer.create 128 in
  Buffer.add_string buf "(content-hashes";
  List.iter (fun (path, hash) ->
    Buffer.add_string buf (Printf.sprintf "\n  (%s %s)" (sexp_of_string path) (sexp_of_string hash));
  ) hashes;
  Buffer.add_string buf ")";
  Buffer.contents buf

(** {1 Writing} *)

let string_of_meta m =
  let buf = Buffer.create 512 in
  Buffer.add_string buf "(; DO NOT EDIT — generated by borge drift. Changes will be overwritten. ;)\n\n";
  Buffer.add_string buf "(meta ";
  Buffer.add_string buf (sexp_of_string m.project_name);
  Buffer.add_char buf '\n';
  Buffer.add_string buf (Printf.sprintf "  (analyzed-at %s)\n" (sexp_of_string m.analyzed_at));
  if m.content_hashes <> [] then begin
    Buffer.add_string buf "  ";
    Buffer.add_string buf (sexp_of_content_hashes m.content_hashes);
    Buffer.add_char buf '\n';
  end;
  if m.module_surfaces <> [] then begin
    Buffer.add_string buf "  ";
    Buffer.add_string buf (sexp_of_module_surfaces m.module_surfaces);
    Buffer.add_char buf '\n';
  end;
  if m.dune_snapshot <> [] then begin
    Buffer.add_string buf "  ";
    Buffer.add_string buf (sexp_of_dune_snapshot m.dune_snapshot);
    Buffer.add_char buf '\n';
  end;
  if m.findings <> [] then begin
    Buffer.add_string buf "  ";
    Buffer.add_string buf (sexp_of_findings m.findings);
    Buffer.add_char buf '\n';
  end;
  Buffer.add_string buf ")\n";
  Buffer.contents buf

(** Write a .borg.meta file *)
let write_meta path m =
  let content = string_of_meta m in
  let oc = open_out path in
  output_string oc content;
  close_out oc

(** {1 Content hashing} *)

(** Simple hash of file contents for staleness detection *)
let content_hash content =
  (* FNV-1a 32-bit hash, hex encoded *)
  let h = ref (Int32.of_int 2166136261) in
  String.iter (fun c ->
    h := Int32.logxor !h (Int32.of_int (Char.code c));
    h := Int32.mul !h (Int32.of_int 16777619)
  ) content;
  Printf.sprintf "%08lx" !h

(** Hash a file by reading it and computing content_hash *)
let hash_file path =
  try content_hash (File_utils.read_file path)
  with Sys_error _ -> ""

(** {1 Timestamps} *)

let current_timestamp () =
  let cmd = "date -u +%Y-%m-%dT%H:%M:%SZ" in
  let ic = Unix.open_process_in cmd in
  let line = input_line ic in
  let _ = Unix.close_process_in ic in
  String.trim line

(** {1 Finding comparison for deterministic ordering} *)

let compare_findings a b =
  let c = compare (string_of_finding_type a.ft_type) (string_of_finding_type b.ft_type) in
  if c <> 0 then c
  else compare a.detail b.detail

(** Sort findings for deterministic output *)
let sort_findings findings =
  List.sort compare_findings findings

(** {1 Merge logic} *)

(** Merge agent findings into existing findings.
    - Keep all static findings from existing
    - Remove all agent findings from existing
    - Add new agent findings
    - Sort for determinism *)
let merge_agent_findings existing new_agent =
  let static_only = List.filter (fun f -> f.source = Static) existing in
  sort_findings (static_only @ new_agent)

(** {1 Meta file path} *)

(** Get the .borg.meta path for a given .borg file *)
let meta_path_of borg_path =
  borg_path ^ ".meta"

(** Check if a path is a .borg.meta file *)
let is_meta_file path =
  String.length path > 10 &&
  String.sub path (String.length path - 10) 10 = ".borg.meta"

(** {1 Reading .borg.meta files} *)

(** Parse a .borg.meta file. Returns None if file doesn't exist or parse fails.
    This is intentionally simple — we don't need to parse our own output back
    into full typed structures. We just need to check if it exists and is valid. *)
let read_meta_exists path =
  Sys.file_exists path

(** Read the analyzed-at timestamp from a .borg.meta file *)
let read_analyzed_at path =
  try
    let input = File_utils.read_file path in
    (* Quick: find (analyzed-at "...") *)
    let re = Str.regexp {|\(analyzed-at "[^"]*"\)|} in
    if Str.string_match re input 0 then
      Some (Str.matched_group 1 input)
    else None
  with _ -> None
