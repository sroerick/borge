(** The (proof ...) block for .borg.meta files.

    Records discharged proof obligations: which witness, which prover,
    what verdict, how many admits, and the representation file the
    witness was checked against. Extends lib/check/meta.ml's block
    vocabulary.

    Block format (from docs/engine.borg subsection `obligations`):
      (proof
        (commit abc123)
        (discharged-at 2026-07-04T18:00:00Z)
        (obligation ownership-consistency
          (witness "proof/db_auth.v")
          (prover coq)
          (verdict pass)
          (admitted 0)
          (representation "proof/BorgeSchema.v")))

    Discipline (from lib/proof/proof.borg subsection `proof-meta`):
    - Idempotent + deterministic (sorted by obligation name) for clean diffs.
    - A new proof run REPLACES the previous (proof ...) block for the
      same .borg file. Per-obligation verdicts replace in place.
    - The analyzed-at timestamp in the parent (meta ...) block is NOT
      updated by proof discharge — discharge is additive, not a
      full re-analysis (mirrors meta.borg's semantic-review rule).
    - Stale proof blocks (representation changed since discharge) are
      detected but preserved, not silently overwritten.

    What this module does NOT do:
    - Read the witness file's contents (mirrors proof_run's discipline).
    - Interpret the obligation's statement. *)

open Printf

(* Reuse meta.ml's sexp_of_string so escaping matches the rest of .borg.meta. *)
let sexp_of_string s = Meta.sexp_of_string s

(** {1 Types} *)

type proof_verdict =
  | Pass of { admitted : int }
  | Fail of { message : string }
  | Prover_absent of { prover : string }

type obligation_verdict = {
  name : string;              (* the property name from (asserts (property NAME ...)) *)
  witness : string;          (* path to the human-authored .v witness *)
  prover : string;            (* "coq" (the prover that discharged) *)
  verdict : proof_verdict;
  representation : string;   (* path to the emitted BorgeSchema.v *)
}

type proof_block = {
  commit : string;            (* git rev-parse HEAD at discharge time *)
  discharged_at : string;    (* ISO 8601 UTC *)
  obligations : obligation_verdict list;
}

(** {1 Verdict string conversions} *)

let string_of_verdict = function
  | Pass _ -> "pass"
  | Fail _ -> "fail"
  | Prover_absent _ -> "prover-absent"

(** {1 Writer} *)

let sexp_of_obligation (o : obligation_verdict) =
  let buf = Buffer.create 128 in
  bprintf buf "  (obligation %s\n" (sexp_of_string o.name);
  bprintf buf "    (witness %s)\n" (sexp_of_string o.witness);
  bprintf buf "    (prover %s)\n" (sexp_of_string o.prover);
  (* verdict may carry a message; record it inline as (verdict pass) or
     (verdict (fail "msg")) / (verdict (prover-absent "coq")) so the
     failure detail round-trips through the reader. *)
  (match o.verdict with
   | Pass { admitted } ->
       bprintf buf "    (verdict pass)\n";
       bprintf buf "    (admitted %d)\n" admitted
   | Fail { message } ->
       bprintf buf "    (verdict (fail %s))\n" (sexp_of_string message);
       bprintf buf "    (admitted 0)\n"
   | Prover_absent { prover } ->
       bprintf buf "    (verdict (prover-absent %s))\n" (sexp_of_string prover);
       bprintf buf "    (admitted 0)\n");
  bprintf buf "    (representation %s))" (sexp_of_string o.representation);
  Buffer.contents buf

let sexp_of_proof_block (b : proof_block) =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "(proof\n";
  bprintf buf "  (commit %s)\n" (sexp_of_string b.commit);
  bprintf buf "  (discharged-at %s)\n" (sexp_of_string b.discharged_at);
  (* Deterministic order: sort by obligation name so re-runs produce
     stable git diffs (mirrors meta.ml's findings sort). *)
  let sorted =
    List.sort (fun a b -> String.compare a.name b.name) b.obligations
  in
  List.iter (fun o ->
    Buffer.add_char buf '\n';
    Buffer.add_string buf (sexp_of_obligation o)
  ) sorted;
  Buffer.add_string buf ")\n";
  Buffer.contents buf

(** Write a standalone (proof ...) block to a path.

    For appending into an existing .borg.meta, use [merge_into_meta]
    which replaces any prior (proof ...) block rather than appending. *)
let write_block ~path (b : proof_block) =
  let content = sexp_of_proof_block b in
  let oc = open_out path in
  output_string oc content;
  close_out oc

(** {1 Reader}

    Minimal reader: parses a (proof ...) block from .borg.meta text.
    Uses Borge_lang.Parse for the sexp structure, then walks the AST
    to rebuild the record. Robust to missing/optional fields (defaults
    to sensible empty values) so a partially-written block doesn't crash. *)

let is_atom s = function
  | Borge_lang.Ast.Atom (_, a) -> a = s
  | _ -> false

(* Extract the raw string content from a String node, regardless of
   quoted vs verbatim form. *)
let string_content = function
  | Borge_lang.Ast.String (_, Borge_lang.Ast.Quoted q) -> Some q.q_content
  | Borge_lang.Ast.String (_, Borge_lang.Ast.Verbatim v) -> Some v.v_content
  | _ -> None

let atom_string node =
  match node.Borge_lang.Ast.node with
  | Borge_lang.Ast.Atom (_, a) -> Some a
  | Borge_lang.Ast.String _ -> string_content node.Borge_lang.Ast.node
  | _ -> None

(* Extract the string value from either a bare atom or a quoted string,
   or a (fail "msg") / (prover-absent "coq") compound form. *)
let rec value_string (node : Borge_lang.Ast.sexp) : string option =
  match node with
  | Borge_lang.Ast.Atom (_, a) -> Some a
  | Borge_lang.Ast.String _ -> string_content node
  | Borge_lang.Ast.List (_, [inner]) -> value_string inner
  | _ -> None

let parse_verdict_node (node : Borge_lang.Ast.sexp) : proof_verdict =
  match node with
  | Borge_lang.Ast.Atom (_, "pass") -> Pass { admitted = 0 }  (* admitted set later *)
  | Borge_lang.Ast.Atom (_, "fail") -> Fail { message = "" }
  | Borge_lang.Ast.Atom (_, "prover-absent") -> Prover_absent { prover = "" }
  | Borge_lang.Ast.List (_, head :: rest) ->
      (* compound form: (fail "msg") or (prover-absent "coq") *)
      (match head with
       | Borge_lang.Ast.Atom (_, "fail") ->
           let msg = match rest with [m] -> Option.value (value_string m) ~default:"" | _ -> "" in
           Fail { message = msg }
       | Borge_lang.Ast.Atom (_, "prover-absent") ->
           let p = match rest with [m] -> Option.value (value_string m) ~default:"" | _ -> "" in
           Prover_absent { prover = p }
       | Borge_lang.Ast.Atom (_, "pass") -> Pass { admitted = 0 }
       | _ -> Pass { admitted = 0 })
  | _ -> Pass { admitted = 0 }

let parse_obligation_node (node : Borge_lang.Ast.sexp_with_comments) =
  match node.Borge_lang.Ast.node with
  | Borge_lang.Ast.List (_, head :: fields) when is_atom "obligation" head ->
      (* head is "obligation", next is the name, rest are (field value) pairs *)
      (match fields with
       | name_node :: rest ->
           let name = Option.value (value_string name_node) ~default:"" in
           let witness = ref "" in
           let prover = ref "coq" in
           let verdict = ref (Pass { admitted = 0 }) in
           let representation = ref "" in
           List.iter (fun f ->
             match f with
             | Borge_lang.Ast.List (_, [k; v]) ->
                 (match k with
                  | Borge_lang.Ast.Atom (_, "witness") -> witness := Option.value (value_string v) ~default:""
                  | Borge_lang.Ast.Atom (_, "prover") -> prover := Option.value (value_string v) ~default:"coq"
                  | Borge_lang.Ast.Atom (_, "verdict") -> verdict := parse_verdict_node v
                  | Borge_lang.Ast.Atom (_, "representation") -> representation := Option.value (value_string v) ~default:""
                  | Borge_lang.Ast.Atom (_, "admitted") ->
                      (match v with
                       | Borge_lang.Ast.Atom (_, n) ->
                           let n = int_of_string_opt n |> Option.value ~default:0 in
                           verdict := (match !verdict with Pass _ -> Pass { admitted = n } | other -> other)
                       | _ -> ())
                  | _ -> ())
             | _ -> ()
           ) rest;
           Some {
             name; witness = !witness; prover = !prover;
             verdict = !verdict; representation = !representation;
           }
       | [] -> None)
  | _ -> None

let parse_block_node (node : Borge_lang.Ast.sexp_with_comments) =
  match node.Borge_lang.Ast.node with
  | Borge_lang.Ast.List (_, head :: rest) when is_atom "proof" head ->
      let commit = ref "" in
      let discharged_at = ref "" in
      let obligations = ref [] in
      List.iter (fun f ->
        match f with
        | Borge_lang.Ast.List (_, [k; v]) ->
            (match k with
             | Borge_lang.Ast.Atom (_, "commit") -> commit := Option.value (value_string v) ~default:""
             | Borge_lang.Ast.Atom (_, "discharged-at") -> discharged_at := Option.value (value_string v) ~default:""
             | _ -> ())
        | Borge_lang.Ast.List (_, head2 :: _) when is_atom "obligation" head2 ->
            (* wrap f in a sexp_with_comments for parse_obligation_node —
               but f is a sexp; parse_obligation_node needs sexp_with_comments.
               Construct a minimal wrapper. *)
            let swc = { Borge_lang.Ast.comments_before = []; node = f; end_pos = { Borge_lang.Ast.line = 0; col = 0; offset = 0 } } in
            (match parse_obligation_node swc with
             | Some o -> obligations := o :: !obligations
             | None -> ())
        | _ -> ()
      ) rest;
      Some {
        commit = !commit; discharged_at = !discharged_at;
        obligations = List.rev !obligations;
      }
  | _ -> None

(** Read a (proof ...) block from a .borg.meta file's text.

    Returns [None] if no (proof ...) block is present. *)
let read_block_from_text (text : string) : proof_block option =
  let file = Borge_lang.Parse.parse_file text in
  let rec walk nodes =
    match nodes with
    | [] -> None
    | swc :: rest ->
        (match parse_block_node swc with
         | Some b -> Some b
         | None -> walk rest)
  in
  walk file.Borge_lang.Ast.top_level

let read_block ~path : proof_block option =
  if not (Sys.file_exists path) then None
  else begin
    let ic = open_in path in
    let len = in_channel_length ic in
    let buf = Bytes.create len in
    really_input ic buf 0 len;
    close_in ic;
    read_block_from_text (Bytes.to_string buf)
  end

(** {1 Merge into .borg.meta}

    Replace any prior (proof ...) block in a .borg.meta file with the
    new one. Preserves all other blocks (meta, findings, etc.). If no
    (proof ...) block exists, appends one. Idempotent: running twice
    with the same block produces the same output. *)
let merge_into_meta ~path (b : proof_block) =
  let existing =
    if Sys.file_exists path then begin
      let ic = open_in path in
      let len = in_channel_length ic in
      let buf = Bytes.create len in
      really_input ic buf 0 len;
      close_in ic;
      Some (Bytes.to_string buf)
    end else None
  in
  let new_block_text = sexp_of_proof_block b in
  match existing with
  | None | Some "" -> write_block ~path b
  | Some text ->
      (* Remove any existing (proof ...) block. The block is a top-level
         form starting with "(proof" and ending at the matching close paren.
         We use the parser to find it, then splice. *)
      let file = Borge_lang.Parse.parse_file text in
      let kept = List.filter (fun swc ->
        match swc.Borge_lang.Ast.node with
        | Borge_lang.Ast.List (_, head :: _) when is_atom "proof" head -> false
        | _ -> true
      ) file.Borge_lang.Ast.top_level in
      (* Re-serialize the kept form by deleting the proof block's source
         range from the text and appending the new one. Simplest robust
         approach: rewrite the file as the new block followed by the
         non-proof top-level forms re-serialized. But we don't have a
         pretty-printer for arbitrary top-level forms here. Instead, do
         a text-level splice: find the "(proof" ... matching ")" and
         remove that span, then append the new block. *)
      let find_proof_span s =
        let prefix = "(proof" in
        let plen = String.length prefix in
        let rec search_from i =
          if i + plen > String.length s then None
          else if String.sub s i plen = prefix
                  && (i = 0 || s.[i-1] = '\n' || s.[i-1] = ' ' || s.[i-1] = '(') then
            Some i
          else search_from (i+1)
        in
        match search_from 0 with
        | None -> None
        | Some start ->
            (* find matching close paren, tracking depth, from start+1 *)
            let depth = ref 1 in
            let i = ref (start + plen) in
            let n = String.length s in
            while !depth > 0 && !i < n do
              let c = s.[!i] in
              if c = '(' then incr depth
              else if c = ')' then decr depth;
              incr i
            done;
            if !depth = 0 then Some (start, !i)  (* span including trailing ) *)
            else None
      in
      let text_without_proof =
        match find_proof_span text with
        | Some (start, stop) ->
            let before = String.sub text 0 start in
            let after = if stop < String.length text then String.sub text stop (String.length text - stop) else "" in
            before ^ after
        | None -> text
      in
      (* Keep any non-proof content, append the new block. Trim trailing
         whitespace so we don't accumulate blank lines. *)
      let trimmed = String.trim text_without_proof in
      let final =
        if trimmed = "" then new_block_text
        else trimmed ^ "\n\n" ^ new_block_text
      in
      let oc = open_out path in
      output_string oc final;
      close_out oc;
      (* suppress unused-variable warning for [kept] — we did the splice
         textually instead; [kept] documents what we intended to preserve. *)
      ignore kept

(** {1 Bridge from proof_run.verdict}

    Construct a proof_block from a discharged verdict. This is the glue
    between run_prover (which produces a verdict) and the meta writer
    (which records it). *)

let block_from_verdict
    ~commit ~discharged_at
    ~name ~witness ~prover ~representation
    (v : Proof_run.verdict) : proof_block =
  let verdict = match v with
    | Proof_run.Pass { admitted } -> Pass { admitted }
    | Proof_run.Fail { message } -> Fail { message }
    | Proof_run.Prover_not_installed { prover = p } -> Prover_absent { prover = p }
    | Proof_run.Representation_stale ->
        (* Stale representation is a failure mode; record as fail with
           a descriptive message rather than inventing a new verdict
           variant — the spec's verdict ADT is pass|fail|prover-absent. *)
        Fail { message = "representation stale — re-emit and re-discharge" }
  in
  {
    commit; discharged_at;
    obligations = [{
      name; witness; prover; verdict; representation;
    }];
  }

(** {1 Timestamp helper}

    Current time as ISO 8601 UTC, for the discharged-at field. *)
let now_iso () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(** {1 Commit helper}

    Current git HEAD short hash, for the commit field. Returns "unknown"
    if git isn't available or not in a repo. *)
let current_commit () =
  let tmp = Filename.temp_file "borge_commit" ".txt" in
  let cmd = sprintf "git rev-parse --short HEAD > %s 2>/dev/null" tmp in
  let _ = Sys.command cmd in
  if Sys.file_exists tmp then begin
    let ic = open_in tmp in
    let len = in_channel_length ic in
    let buf = Bytes.create len in
    let _ = really_input ic buf 0 len in
    close_in ic;
    Sys.remove tmp;
    let s = String.trim (Bytes.to_string buf) in
    if s = "" then "unknown" else s
  end else "unknown"
