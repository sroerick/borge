#!/bin/sh
# crud-app demo — generate and run a task-tracker CRUD app
#
# Prerequisites:
#   - PostgreSQL running locally (or set PGHOST/PGPORT/PGUSER)
#   - dream + dream_html opam packages (for the OCaml server)
#
# Usage:
#   ./run.sh sql     # Apply DDL to Postgres
#   ./run.sh web     # Build and run the Dream web app
#   ./run.sh show    # Just print all generated outputs

set -e

HERE="$(cd "$(dirname "$0")" && pwd)"

gen_sql() {
    echo ">> Generating PostgreSQL DDL..."
    borge generate target postgres-sql "$HERE/db.borg" > "$HERE/generated.sql"
    echo "   Wrote generated.sql ($(wc -l < "$HERE/generated.sql") lines)"
}

gen_ml() {
    echo ">> Generating OCaml dream_html..."
    borge generate target ocaml-dream "$HERE/ui.borg" > "$HERE/generated.ml"
    echo "   Wrote generated.ml ($(wc -l < "$HERE/generated.ml") lines)"
}

gen_c() {
    echo ">> Generating Clay C..."
    borge generate target clay-c "$HERE/ui.borg" > "$HERE/generated.c"
    echo "   Wrote generated.c ($(wc -l < "$HERE/generated.c") lines)"
}

cmd_sql() {
    gen_sql
    echo ""
    echo ">> Applying DDL to PostgreSQL..."
    psql -f "$HERE/generated.sql" || {
        echo "ERROR: psql failed. Is PostgreSQL running?"
        echo "       Set PGHOST/PGPORT/PGUSER if needed."
        exit 1
    }
    echo "   Done. Tables: users, projects, tasks, comments"
    echo ""
    echo "   RLS policies created for ownership columns."
    echo "   Group policies: admin (can-all), member, viewer."
}

cmd_web() {
    gen_ml
    echo ""
    echo ">> Building Dream web app..."
    # The generated.ml is a library module — it needs a main.ml
    # that calls Dream.run with the register_routes function.
    # For now, print the generated module and instructions.
    echo "   generated.ml requires a Dream main to run."
    echo "   Include it in a Dream project and call register_routes()."
    echo ""
    echo "   Quick start:"
    echo "     open Dream_html"
    echo "     let () = Dream.run ~port:8080 (Ui_generated.register_routes)"
    echo ""
    echo "   Generated routes:"
    grep 'Dream.get' "$HERE/generated.ml" || true
}

cmd_show() {
    gen_sql
    gen_ml  
    gen_c
    echo ""
    echo "=========================================="
    echo "  PostgreSQL DDL (generated.sql)"
    echo "=========================================="
    cat "$HERE/generated.sql"
    echo ""
    echo "=========================================="
    echo "  OCaml dream_html (generated.ml)"
    echo "=========================================="
    cat "$HERE/generated.ml"
    echo ""
    echo "=========================================="
    echo "  Clay C (generated.c)"
    echo "=========================================="
    cat "$HERE/generated.c"
}

case "${1:-show}" in
    sql)  cmd_sql ;;
    web)  cmd_web ;;
    show) cmd_show ;;
    *)
        echo "Usage: $0 [sql|web|show]"
        echo "  sql  - Generate SQL and apply to Postgres"
        echo "  web  - Generate OCaml and show how to run as Dream app"
        echo "  show - Generate all targets and print (default)"
        exit 1 ;;
esac
