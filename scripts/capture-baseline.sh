#!/bin/sh
# capture-baseline.sh — freeze the observable output of the gate commands.
#
# Why: every deprecation phase and the convention refactor have exactly one
# real regression test — "does drift/lint/report still say the same thing?"
# There is no unit test for that. So we capture the answers before touching
# anything, and diff against them after.
#
# Usage:
#   scripts/capture-baseline.sh OUTDIR            # capture
#   scripts/capture-baseline.sh --diff OUTDIR     # compare current to OUTDIR
#
# Exit 1 on any difference (in --diff mode).

set -u

# Repos chosen to cover the conventions that exist today:
#   borge        ocaml-dune, the self-test
#   eyegor       ocaml-dune, 13 borg chapters (largest OCaml book)
#   rein         ocaml-dune, has a stale .borge.lock
#   pricklypear  ocaml-dune, 103 .borge-bug files (Phase E migration target)
#   teachings    go-standard, 2 borg chapters (the only Go+borg repo)
REPOS="
$HOME/dev/wyo.tech/borge
$HOME/dev/wyo.tech/eyegor
$HOME/dev/wyo.tech/rein
$HOME/dev/wyo.tech/pricklypear
$HOME/dev/wyo.tech/teachings
"

MODE=capture
OUT=""
case "${1:---help}" in
  --help|-h) sed -n '2,20p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
  --diff) MODE=diff; OUT="${2:?--diff needs a baseline dir}" ;;
  *) OUT="$1" ;;
esac

eval "$(opam env --switch=poohstack --set-switch)" 2>/dev/null

slug() {
  # /home/u/dev/wyo.tech/borge -> wyo.tech_borge
  printf '%s' "$1" | sed "s|^$HOME/dev/||; s|/|_|g"
}

rc=0
[ "$MODE" = capture ] && mkdir -p "$OUT"

for repo in $REPOS; do
  [ -d "$repo" ] || { printf 'skip (missing): %s\n' "$repo"; continue; }
  name=$(slug "$repo")
  mkdir -p "$OUT/$name"
  for cmd in drift lint report; do
    # --json where supported; report has no --json so capture text with
    # absolute paths stripped so the baseline is machine-portable.
    case $cmd in
      drift|lint) raw=$(cd "$repo" && borge "$cmd" --json 2>/dev/null) ;;
      report)     raw=$(cd "$repo" && borge report 2>/dev/null | sed "s|$repo|<REPO>|g") ;;
    esac
    printf '%s\n' "$raw" | sed "s|$repo|<REPO>|g" > "$OUT/$name/$cmd.txt"
  done
  printf 'captured: %s\n' "$name"
done

[ "$MODE" = capture ] && { printf '\nbaseline written to %s\n' "$OUT"; exit 0; }

printf '\n== diff vs %s ==\n' "$OUT"
for repo in $REPOS; do
  [ -d "$repo" ] || continue
  name=$(slug "$repo")
  for cmd in drift lint report; do
    f="$OUT/$name/$cmd.txt"
    [ -f "$f" ] || { printf 'MISSING baseline: %s\n' "$f"; rc=1; continue; }
    case $cmd in
      drift|lint) raw=$(cd "$repo" && borge "$cmd" --json 2>/dev/null) ;;
      report)     raw=$(cd "$repo" && borge report 2>/dev/null | sed "s|$repo|<REPO>|g") ;;
    esac
    now=$(printf '%s\n' "$raw" | sed "s|$repo|<REPO>|g")
    if ! printf '%s' "$now" | diff -q "$f" - >/dev/null 2>&1; then
      printf 'CHANGED: %s/%s\n' "$name" "$cmd"
      printf '%s' "$now" | diff "$f" - | head -20
      rc=1
    fi
  done
done

[ $rc -eq 0 ] && printf 'all baselines identical\n'
exit $rc
