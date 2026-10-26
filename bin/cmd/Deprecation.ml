(** Deprecation banners for the orchestration half.

    Borge is splitting into two halves: the GATES (balance, check, fmt,
    drift, lint, report, book, inline, nodes, stats, modules, log, diff)
    and the ORCHESTRATION (plan, make, execute, generate, review, commit,
    abort, undo, merge, merge-queue, inbox, issue).

    The orchestration half makes borge the thing driving an agent: it
    writes a prompt, launches pi, holds a .borge.lock, watches an exit
    code, and mediates conflicts between concurrent agent branches. That
    is a harness, and it is being retired. See
    docs/orchestration-deprecation.borg.

    Why a runtime banner and not just spec status: the deprecated commands
    still work, so nothing forces anyone to notice a spec change. One line
    on stderr is the cheapest thing that stops new usage from accreting
    while the phases land.

    This module is deleted in Phase D along with the commands it warns
    about. It is deliberately dependency-free so that deleting it is a
    grep-and-revert, not an untangling. *)

(* agent note (|
 *   WHAT: The single deprecation notice text, parameterised by command.
 *   WHY: Every deprecated command should say the same thing, name the
 *   replacement, and point at the chapter. One string, one source.
 * |) *)
let notice ~cmd ~replacement =
  Printf.sprintf
    "borge %s is deprecated (orchestration half) and will be removed.\n\
    \  borge stays a spec toolkit: %s.\n\
    \  See docs/orchestration-deprecation.borg.\n"
    cmd replacement

(* agent note (|
 *   WHAT: Print a deprecation notice to stderr unless BORGE_NO_DEPRECATION
 *   is set.
 *
 *   WHY: stderr so it never corrupts stdout that a script may be parsing.
 *   The env var exists so CI and scripted callers can silence it without
 *   redirecting stderr (which would also hide real errors).
 * |) *)
let warn ~cmd ~replacement =
  match Sys.getenv_opt "BORGE_NO_DEPRECATION" with
  | Some ("1" | "true" | "yes") -> ()
  | _ -> prerr_string (notice ~cmd ~replacement)
