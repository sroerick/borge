(** borge proof — proof obligation commands

    Implements docs/cli.borg subsections proof-emit-command, proof-run-command,
    proof-show-command. Wires the lib/proof/ library (Proof_emit, Proof_run,
    Proof_meta) to CLI verbs so the obligations pipeline is user-facing.

    Three subcommands:
      borge proof emit --spec DB.borg [--output PATH]
        Emit the Coq representation (BorgeSchema.v) from a DB spec.
      borge proof run --spec BORG --obligation NAME [--witness PATH] [--record]
        Discharge an obligation: emit rep, run coqc, print verdict.
      borge proof show --spec BORG
        Print the recorded (proof ...) block from .borg.meta. *)

open Borge_lib
open Cmdliner
open Printf

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

(* Parse a .borg file's text into its AST. *)
let parse_borg_file path =
  let text = File_utils.read_file path in
  Borge_lang.Parse.parse_file text

(* Find a (db ...) form anywhere in a parsed file (top-level or nested
   under (project ...)). Returns the first db_app found. *)
let find_db_app file =
  Db_parse.parse_file file

(* Find an obligation by name in a .borg file. Returns the obligation
   record and the section's (implements ...) targets if any. *)
let find_obligation file obligation_name =
  let mappings = Spec.extract_section_mappings file in
  let rec search = function
    | [] -> None
    | m :: rest ->
      let m = (m : Spec.section_mapping) in
      (match List.find_opt (fun (o : Spec.property_obligation) ->
        o.name = obligation_name) m.obligations with
       | Some o -> Some (o, m.implements)
       | None -> search rest)
  in
  search mappings

(* Default representation output path: proof/BorgeSchema.v in the repo root. *)
let default_rep_path () = "proof/BorgeSchema.v"

(* Compute the .borg.meta companion path for a .borg spec.
   examples/crud-app/db.borg → examples/crud-app/.borg.meta *)
let meta_path_for spec_path =
  let dir = Filename.dirname spec_path in
  if dir = "." then ".borg.meta" else Filename.concat dir ".borg.meta"

(* ------------------------------------------------------------------ *)
(* emit                                                                *)
(* ------------------------------------------------------------------ *)

let cmd_emit spec output_opt =
  let file = parse_borg_file spec in
  (match find_db_app file with
   | None ->
     eprintf "Error: no (db ...) form found in %s\n" spec;
     exit 1
   | Some db ->
     let out_path = match output_opt with
       | Some p -> p
       | None -> default_rep_path ()
     in
     Proof_emit.emit_to_file db ~path:out_path;
     printf "Emitted Coq representation: %s\n" out_path;
     printf "  Tables: %d, Groups: %d\n"
       (List.length db.tables) (List.length db.groups);
     printf "  Discharge with: borge proof run --spec %s --obligation NAME\n" spec)

(* ------------------------------------------------------------------ *)
(* run                                                                 *)
(* ------------------------------------------------------------------ *)

let cmd_run spec obligation_name witness_override record_flag =
  let file = parse_borg_file spec in
  (* Find the obligation in the spec. *)
  (match find_obligation file obligation_name with
   | None ->
     eprintf "Error: obligation %S not found in %s\n" obligation_name spec;
     eprintf "  Use 'borge nodes %s' to see declared obligations.\n" spec;
     exit 1
   | Some (obligation, implements_targets) ->
     (* Determine the DB spec to emit the representation from.
        The obligation lives in a section that may (implements ...) a
        DB spec. If the section implements a path, use that. Otherwise
        assume the (db ...) form is in the same file as the obligation. *)
     let db_spec_path =
       match implements_targets with
       | target :: _ when Sys.file_exists target -> target
       | _ -> spec
     in
     let db_file = parse_borg_file db_spec_path in
     (match find_db_app db_file with
      | None ->
        eprintf "Error: no (db ...) form found in %s\n" db_spec_path;
        exit 1
      | Some db ->
        (* Emit representation to a temp path (the witness's directory
           so coqc's -Q resolves). The witness path from the spec is
           repo-relative; resolve it against CWD. *)
        let witness = match witness_override with
          | Some w -> w
          | None -> obligation.witness
        in
        (* Emit the representation into the same directory as the
           witness so coqc -Q <dir> "" finds it. The module name
           is BorgeSchema (fixed) so we name the file BorgeSchema.v. *)
        let wit_dir = Filename.dirname witness in
        let wit_dir = if wit_dir = "" then "." else wit_dir in
        let rep_path = Filename.concat wit_dir "BorgeSchema.v" in
        Proof_emit.emit_to_file db ~path:rep_path;
        printf "Emitted representation: %s\n" rep_path;
        printf "Witness: %s\n" witness;
        printf "Prover: %s\n" obligation.prover;
        (* Run the prover. *)
        let verdict = Proof_run.run_prover
          ~witness ~representation:rep_path ~prover:obligation.prover in
        (* Print the verdict. *)
        let summary = Proof_run.verdict_summary
          ~prover:obligation.prover
          ~obligation_name:obligation.name
          ~verdict in
        printf "%s\n" summary;
        (* Optionally record to .borg.meta. *)
        if record_flag then begin
          let commit = Proof_meta.current_commit () in
          let discharged_at = Proof_meta.now_iso () in
          let block = Proof_meta.block_from_verdict
            ~commit ~discharged_at
            ~name:obligation.name
            ~witness
            ~prover:obligation.prover
            ~representation:rep_path
            verdict in
          let meta_path = meta_path_for spec in
          Proof_meta.merge_into_meta ~path:meta_path block;
          printf "Recorded verdict to %s\n" meta_path
        end;
        (* Exit code: 0 on Pass or prover-absent (not an error per spec),
           1 on Fail. *)
        (match verdict with
         | Proof_run.Fail _ -> exit 1
         | _ -> exit 0)))

(* ------------------------------------------------------------------ *)
(* show                                                                *)
(* ------------------------------------------------------------------ *)

let cmd_show spec =
  let meta_path = meta_path_for spec in
  (match Proof_meta.read_block ~path:meta_path with
   | None ->
     printf "No proof recorded for %s\n" spec;
     printf "  (looked for: %s)\n" meta_path;
   | Some block ->
     printf "Proof block for %s\n" spec;
     printf "  Commit: %s\n" block.commit;
     printf "  Discharged at: %s\n" block.discharged_at;
     printf "  Obligations (%d):\n" (List.length block.obligations);
     List.iter (fun (o : Proof_meta.obligation_verdict) ->
       printf "    %s\n" o.name;
       printf "      witness: %s\n" o.witness;
       printf "      prover: %s\n" o.prover;
       printf "      representation: %s\n" o.representation;
       (match o.verdict with
        | Proof_meta.Pass { admitted } ->
          printf "      verdict: pass (%d admitted)\n" admitted
        | Proof_meta.Fail { message } ->
          printf "      verdict: fail — %s\n" message
        | Proof_meta.Prover_absent { prover } ->
          printf "      verdict: prover-absent (%s)\n" prover)
     ) block.obligations)

(* ------------------------------------------------------------------ *)
(* cmdliner terms                                                      *)
(* ------------------------------------------------------------------ *)

let spec_arg =
  Arg.(required & opt (some string) None & info ["spec"]
    ~docv:"PATH" ~doc:"Path to the .borg spec file")

let obligation_arg =
  Arg.(required & opt (some string) None & info ["obligation"]
    ~docv:"NAME" ~doc:"Obligation name (from (asserts (property NAME ...)))")

let output_arg =
  Arg.(value & opt (some string) None & info ["output"]
    ~docv:"PATH" ~doc:"Output path (default: proof/BorgeSchema.v)")

let witness_arg =
  Arg.(value & opt (some string) None & info ["witness"]
    ~docv:"PATH" ~doc:"Override witness path from the spec")

let record_flag =
  Arg.(value & flag & info ["record"]
    ~doc:"Write the verdict to the spec's .borg.meta as a (proof ...) block")

let emit_cmd : unit Cmd.t =
  Cmd.v (Cmd.info "emit" ~doc:"emit Coq representation from a DB spec")
    Term.(const cmd_emit $ spec_arg $ output_arg)

let run_cmd : unit Cmd.t =
  Cmd.v (Cmd.info "run" ~doc:"discharge a proof obligation")
    Term.(const cmd_run $ spec_arg $ obligation_arg $ witness_arg $ record_flag)

let show_cmd : unit Cmd.t =
  Cmd.v (Cmd.info "show" ~doc:"show recorded verdict from .borg.meta")
    Term.(const cmd_show $ spec_arg)

let cmd : unit Cmd.t =
  Cmd.group (Cmd.info "proof" ~doc:"proof obligation commands: emit, run, show"
    ~man:[`S "DESCRIPTION";
          `P "Discharges (asserts (property ...)) obligations via external provers.";
          `P "Borge declares; the prover discharges; borge records.";
          `S "COMMANDS";
          `I ("emit", "Emit a Coq representation from a DB spec");
          `I ("run", "Discharge an obligation by running the prover");
          `I ("show", "Show the recorded verdict from .borg.meta")])
    [emit_cmd; run_cmd; show_cmd]
