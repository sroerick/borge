(* Launch an interactive agent with a structured prompt — REMOVED.

   This module used to run `pi @prompt_path` from the Borge_lib side, which
   was the interactive half of the orchestration experiment. Phase B of
   docs/orchestration-deprecation.borg removes it.

   It survives as a stub for one reason only: bin/cmd/Plan.ml, Make.ml and
   Drift.ml (run_agent) still call it, and those commands are not deleted
   until Phase D. A stub that refuses is strictly better than deleting the
   module now and leaving three files that do not compile — the deprecation
   phases are meant to be independently revertable, not to require a
   big-bang edit.

   When Phase D lands, this file is deleted along with its three callers. *)

(* agent note (|
 *   WHAT: Return a failure status instead of launching an agent.
 *
 *   WHY: Callers treat a non-zero status as "session failed" and already
 *   have a path for it (they keep the lock and report the exit code). So
 *   returning 127 — the shell's "command not found" — makes `borge plan`,
 *   `borge make` and `borge drift --agent` fail loudly and honestly rather
 *   than hanging on a child that will never produce anything.
 * |) *)
let launch (_prompt_path : string) : int =
  prerr_endline
    "borge: no agent launcher (orchestration removed).\n\
    \  Write the prompt yourself, or use your own agent runtime.\n\
    \  See docs/orchestration-deprecation.borg.";
  127
