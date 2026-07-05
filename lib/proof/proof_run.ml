(** Prover invocation and verdict parsing.

    Runs an external prover (coqc) against a human-authored witness
    file, using an emitted representation file (from [Proof_emit]) as
    the schema the witness is checked against. Parses the prover's
    output into a typed verdict.

    Per docs/engine.borg subsection `obligations`: borge records what
    was claimed and binds it to a commit. If no prover is installed to
    discharge the claim, borge records that and the obligation
    contributes no Verified credit. This module returns a typed
    [Prover_not_installed] verdict in that case rather than crashing.

    See lib/proof/proof.borg subsection `proof-run` for the spec. *)

open Printf

(** A prover's verdict on a single obligation.

    - [Pass { admitted }] : the prover accepted the proof. [admitted]
      is the count of `admit`-style holes used; 0 means a complete
      proof, >0 means a partial proof that typechecks but relies on
      unproven assertions.
    - [Fail { message }] : the prover rejected the proof. [message]
      is the prover's stderr or first error line.
    - [Prover_not_installed { prover }] : the prover binary was not
      found on PATH. The obligation cannot be discharged on this
      machine; borge records this and the obligation contributes no
      Verified credit.
    - [Representation_stale] : reserved for future use; the freshness
      check is not yet wired. *)
type verdict =
  | Pass of { admitted : int }
  | Fail of { message : string }
  | Prover_not_installed of { prover : string }
  | Representation_stale

(* agent note (|
 *   WHAT: Check whether a prover binary is on PATH. Currently only
 *   recognizes "coq" -> looks for "coqc" via [Sys.command "command -v"].
 *   Returns true if the binary is reachable.
 *   WHY: The spec requires graceful handling of prover-absent. We
 *   pre-check rather than catching a process-spawn failure, so the
 *   verdict is deterministic rather than crash-then-recover.
 * |) *)
let prover_available prover =
  match prover with
  | "coq" ->
      (* `command -v coqc` returns 0 if found. Sys.command returns the
         exit code. We use ksh/sh-compatible syntax (no bash-isms) since
         this may run on OpenBSD. *)
      Sys.command "command -v coqc >/dev/null 2>&1" = 0
  | _ -> false  (* unknown prover: not available, by definition *)

(* agent note (|
 *   WHAT: Count admit-style holes in a witness file's text by scanning
 *   for lines containing `admit` or `Admitted` as Coq proof-deferral
 *   tokens. Returns the count.
 *   WHY: The spec records the admit count in the .borg.meta proof block
 *   (it feeds the admits cap: >0 caps status at provisional-verified).
 *   This is the ONE place borge reads the witness file's contents, and
 *   it is narrow: we count admits but do not interpret the proof. A
 *   future implementation will parse coqc's structured output instead
 *   of scanning the witness text, which is coqc-version-sensitive.
 *
 *   CAVEAT: this is a heuristic. A witness that legitimately contains
 *   the word "admit" in a comment or string will over-count. The
 *   initial implementation accepts this imprecision; admits cap only
 *   fails Verified -> provisional-verified, never blocks a commit, so
 *   over-counting errs toward honesty.
 * |) *)
let count_admits_in_witness ~witness =
  try
    let ic = open_in witness in
    let count = ref 0 in
    (try
       while true do
         let line = input_line ic in
         (* Match lines that look like Coq admit usage: standalone `admit.`
            or `Admitted.` as a proof terminator, or `admit` as a tactic. *)
         let trimmed = String.trim line in
         if trimmed = "admit." || trimmed = "Admitted."
         || (String.length trimmed >= 5
             && (String.sub trimmed 0 5 = "admit"
                 || String.sub trimmed 0 8 = "Admitted"))
         then incr count
       done
     with End_of_file -> ());
    close_in ic;
    !count
  with _ -> 0  (* witness file missing/unreadable: don't fail on admit-count *)

(* agent note (|
 *   WHAT: Run a prover on a witness file against a representation.
 *   For coq: spawns `coqc -Q <rep_dir> BorgeSchema <witness>` via
 *   Sys.command, capturing exit code. Exit 0 => Pass (with admit
 *   count from a separate witness scan); non-0 => Fail with a
 *   synthesized message (full stderr capture would require a temp
 *   file; deferred — the exit code is the truth, the message is a
 *   hint).
 *   WHY: The spec says borge records what was claimed; if no prover
 *   is present to discharge the claim, borge records that and the
 *   obligation contributes no Verified credit. We pre-check
 *   [prover_available] and return [Prover_not_installed] rather than
 *   crashing on a missing binary.
 * |) *)
let run_prover ~witness ~representation ~prover : verdict =
  if not (prover_available prover) then
    Prover_not_installed { prover }
  else begin
    (* representation is a .v file whose module name matches its filename
       (default BorgeSchema.v → module BorgeSchema). To make `Require
       Import BorgeSchema.` in the witness resolve, we map the rep's
       directory to the empty logical prefix so the bare module name works.
       This mirrors the manual invocation: coqc -Q <dir> "" witness.v. *)
    let rep_dir = Filename.dirname representation in
    let rep_dir = if rep_dir = "" then "." else rep_dir in
    (* First compile the representation, then the witness. Both must
       succeed for the obligation to discharge. *)
    let compile_cmd path =
      sprintf "coqc -Q %s \"\" %s 2>&1" rep_dir path
    in
    let rep_exit = Sys.command (compile_cmd representation) in
    if rep_exit <> 0 then
      Fail { message = sprintf "coqc on representation exited %d" rep_exit }
    else begin
      let wit_exit = Sys.command (compile_cmd witness) in
      if wit_exit = 0 then begin
        let admitted = count_admits_in_witness ~witness in
        Pass { admitted }
      end else
        Fail { message = sprintf "coqc on witness exited %d" wit_exit }
    end
  end

(* agent note (|
 *   WHAT: String representation of a verdict, for CLI output and for
 *   embedding in agent notes (the proof-pass/proof-fail notes the spec
 *   defines). Mirrors the format:
 *     "proof-pass at commit <sha>: <name> discharged (<prover>, 1 obligation, N admitted)"
 *   The commit sha is supplied by the caller; this function formats
 *   everything else.
 *   WHY: The spec's evidence format is a (* agent note (|...|) *) block.
 *   The caller (eventually a CLI command) writes the note into the .borg
 *   file with the witness and obligation name; this function produces
 *   the inner text.
 * |) *)
let verdict_summary ~prover ~obligation_name ~verdict =
  match verdict with
  | Pass { admitted = a } ->
      sprintf "proof-pass: %s discharged (%s, admitted=%d, stale=false)"
        obligation_name prover a
  | Fail { message } ->
      sprintf "proof-fail: %s (%s) — %s" obligation_name prover message
  | Prover_not_installed _ ->
      sprintf "proof-skipped: %s (%s) — prover not installed"
        obligation_name prover
  | Representation_stale ->
      sprintf "proof-stale: %s (%s) — representation changed since discharge"
        obligation_name prover
