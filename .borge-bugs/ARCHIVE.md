# Migrated borge issues

Archived from `.borge-bugs` (7 records) before `borge issue` and `lib/bug` were retired.

Source files are unchanged; this is a readable projection of them.

| status | count |
|---|---|
| triage | 2 |
| closed | 5 |

---

## BORGE-1785545289 — report does not count sections with (status partial) — pricklypear has ~45 partial sections invisible in report totals, so '54.6% complete' and the 130-tracked denominator understate the ledger. Include a partial column.

- **status:** triage
- **created:** 2026-07-31
- **filed by:** roerick

---

## BORGE-1786034799 — Drift refactor landed (existence not evidence + orphan detection): update lib/check/check.borg subsection drift to describe orphan-file detection (scripts/docs/libs/migrations) and the new implements-existence + verify-apparatus checks; also resolve docs/agent-cheatsheet.md now flagged as an orphan (declare in a spec section or excuse via (untracked)).

- **status:** triage
- **created:** 2026-08-06
- **filed by:** roerick

---

## BORGE-1783788555 — Add 'deprecated' status value for retired chapters

- **status:** closed
- **created:** 2026-07-11
- **filed by:** roerick

Borge's status enum (spec.ml) is: planned, in-progress,
    partial, implemented, verified, drifted, blank. There is no
    status for a chapter that has been RETIRED from the book but
    is kept for historical context.

    This came up in pricklypear it35: we pivoted from a
    multi-process Mirage unikernel topology (supervisor +
    per-user images + authd) to a single-image model. The old
    chapters (architecture.borg, supervisor.borg, multi-user.borg)
    are no longer authoritative for the live design, but deleting
    them loses the record of WHY the pivot happened and the
    prior intent. We want a status that means 'this chapter is
    retired; the successor is X; do not implement against it.'

    Without it, we are forced to use 'planned' or 'partial' on
    retired chapters, which is misleading (implies the work is
    still ahead) or 'blank' (implies no decision). Neither
    communicates 'intentionally abandoned in favor of Y'.

    Proposed status: 'deprecated'
      - valid in (status deprecated) and lint
      - report counts it separately (like drifted)
      - drift/check treats deprecated sections as out of scope
        (no unspecified-module noise from their modules)
      - convention: pair with an agent note pointing at the
        successor chapter

    Alternative names: 'retired', 'abandoned', 'superseded'.
    'deprecated' is the most familiar (matches language/lib
    ecosystems) and distinct from the existing set.

---

## BORGE-1783789294 — borge issue close strips id/title/context; writes blank record

- **status:** closed
- **created:** 2026-07-11
- **note:** source record is malformed (unescaped quotes in its doc body); fields recovered leniently
- **filed by:** roerick

`borge issue close ID` does not preserve the bug's identity or
    context. Observed when closing BORGE-1783788555 after fixing it:

    The original file .borge-bugs/BORGE-1783788555.borge-bug was
    LEFT UNCHANGED (status still 'triage', full doc intact), and a
    NEW file .borge-bugs/.borge-bug was created with the closed
    state — but that new record is blank:

      (borge-bug (id "")
        (title "")
        (status closed)
        (created )
        (filed-by )
        (doc "")
        (resolution (when 1783789187) (how closed) (by roerick)))

    Problems:
      1. id, title, filed-by, created are all empty strings — the
         closed record has no link back to which bug it resolves.
      2. The original .borge-bug file keeps status 'triage' — so
         `borge issue list --status triage` still shows the bug as
         open even though it was closed. The two disagree.
      3. The closed record is written to a fixed filename
         '.borge-bug' (no id in the name), so closing a second bug
         would OVERWRITE the first closed record.

    Expected behaviour: closing a bug should mark the ORIGINAL
    file's status as 'closed' (or 'resolved') and append a
    (resolution ...) stanza to it, in place — not fork a blank
    twin into a collision-prone filename. At minimum the closed
    record must carry the id and title.

    Repro: borge issue new "test" ; borge issue close <id> ;
    cat .borge-bugs/<id>.borge-bug ; cat .borge-bugs/.borge-bug

---

## BORGE-1785545278 — drift CLI: 'marked implemented but not found in code' label is hardcoded (bin/cmd/Drift.ml:177) and ignores d.description — every spec_drift item prints it. Real causes are 'missing agent verification note' / 'verify stanza modified since HEAD'. Also the feature_exists check is dead code (never returns Some false). Print d.description instead.

- **status:** closed
- **created:** 2026-07-31
- **filed by:** roerick

---

## BORGE-1785545310 — drift check_missing_verify_notes contradicts current spec-hygiene doctrine: drift demands agent verification notes on implemented sections with verify stanzas, but book doctrine forbids recording verification RESULTS in the spec. Result: 51/52 drift items in pricklypear are this false-positive class. Either drop the check or reframe it (e.g. verify stanza stale vs git). NOTE: issue IDs collide when two issues are filed in the same second — 1785545289 was overwritten once.

- **status:** closed
- **created:** 2026-07-31
- **filed by:** roerick

---

## BORGE-1788068865 — borge.borg inlines a nonexistent lib.borg — borge inline/report hard-fail on borge's own repo. Root borge.borg ends its inline list with (inline 

- **status:** closed
- **created:** 2026-08-29
- **note:** source record is malformed (unescaped quotes in its doc body); fields recovered leniently
- **filed by:** roerick

