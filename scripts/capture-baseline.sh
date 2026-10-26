#!/bin/sh
# capture-baseline.sh — freeze the observable output of the gate commands.
#
# Why: every deprecation phase and the convention refactor have exactly one
# real regression test — "does drift/lint/report still say the same thing?"
# There is no unit test for that. So we capture the answers before touching
# anything, and diff against them after.
#
# WHICH BINARY: set BORGE_BIN explicitly. This script deliberately does NOT
# rely on `borge` from PATH — that resolves to the opam-installed binary,
# which lags the tree you are editing, and a baseline captured from it
# measures the OLD code no matter what you just built. That failure mode
# silently makes every diff pass.
#
# Usage:
#   BORGE_BIN=/path/to/borge.exe scripts/capture-baseline.sh OUTDIR
#   BORGE_BIN=/path/to/borge.exe scripts/capture-baseline.sh --diff OUTDIR
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
  --help|-h) sed -n '2,22p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
  --diff) MODE=diff; OUT="${2:?--diff needs a baseline dir}" ;;
  *) OUT="$1" ;;
esac

BORGE_BIN="${BORGE_BIN:-}"
if [ -z "$BORGE_BIN" ]; then
  printf 'BORGE_BIN is required (absolute path to the borge exe under test).\n' >&2
  printf 'Refusing to fall back to PATH: that measures the installed binary,\n' >&2
  printf 'not the tree being changed, and every diff would falsely pass.\n' >&2
  exit 2
fi
[ -x "$BORGE_BIN" ] || { printf 'not executable: %s\n' "$BORGE_BIN" >&2; exit 2; }
BORGE_BIN=$(cd "$(dirname "$BORGE_BIN")" && pwd)/$(basename "$BORGE_BIN")

slug() {
  # /home/u/dev/wyo.tech/borge -> wyo.tech_borge
  printf '%s' "$1" | sed "s|^$HOME/dev/||; s|/|_|g"
}

# Produce one command's output for one repo on stdout, with the repo's
# absolute path normalised so the baseline is portable across checkouts.
# The binary under test is invoked by absolute path, from the repo's cwd so
# that relative-path conventions inside borge behave normally.
emit() {
  repo=$1
  cmd=$2
  case $cmd in
    drift|lint) (cd "$repo" && "$BORGE_BIN" "$cmd" --json 2>/dev/null) ;;
    report)     (cd "$repo" && "$BORGE_BIN" report 2>/dev/null) ;;
  esac | sed "s|$repo|<REPO>|g"
}

rc=0

if [ "$MODE" = capture ]; then
  [ -n "$OUT" ] || { printf 'usage: %s OUTDIR\n' "$0" >&2; exit 2; }
  for repo in $REPOS; do
    [ -d "$repo" ] || { printf 'skip (missing): %s\n' "$repo"; continue; }
    name=$(slug "$repo")
    mkdir -p "$OUT/$name"
    for cmd in drift lint report; do
      emit "$repo" "$cmd" > "$OUT/$name/$cmd.txt"
    done
    printf 'captured: %s\n' "$name"
  done
  printf '\nbaseline written to %s (binary %s)\n' "$OUT" "$BORGE_BIN"
  exit 0
fi

# --- diff mode -------------------------------------------------------------
# NOTE: this arm must never write into $OUT. An earlier version created
# $OUT/$name and wrote the current output before comparing, which made the
# comparison current-vs-current and always passed. Keep every write out of
# the baseline directory.
[ -d "$OUT" ] || { printf 'no baseline dir: %s\n' "$OUT" >&2; exit 2; }

printf '== diff vs %s (binary %s) ==\n' "$OUT" "$BORGE_BIN"
for repo in $REPOS; do
  [ -d "$repo" ] || { printf 'skip (missing): %s\n' "$repo"; continue; }
  name=$(slug "$repo")
  for cmd in drift lint report; do
    saved="$OUT/$name/$cmd.txt"
    if [ ! -f "$saved" ]; then
      printf 'MISSING baseline: %s\n' "$saved"
      rc=1
      continue
    fi
    now=$(emit "$repo" "$cmd")
    # Command substitution strips trailing newlines from both sides, so a
    # trailing-newline difference alone cannot produce a false "changed".
    if [ "$(cat "$saved")" != "$now" ]; then
      printf 'CHANGED: %s/%s\n' "$name" "$cmd"
      printf '%s\n' "$now" | diff "$saved" - | head -20
      rc=1
    fi
  done
done

[ $rc -eq 0 ] && printf 'all baselines identical\n'
exit $rc
