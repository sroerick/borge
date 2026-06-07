# Code Context

## Files Retrieved

1. **bin/borge.ml** (all lines) - Main CLI entry point that chains together all subcommand modules
2. **bin/cmd/Generate.ml** (all lines) - Contains the `borge generate` command with subcommands: `spec`, `code`, `ui`, `db`
3. **lib/generate/generate.ml** (all lines) - Contains `generate_spec` and `generate_code` helper functions used by the CLI
4. **lib/db/db_sql.ml** (all lines) - PostgreSQL DDL generator for `db` specs
5. **lib/ui/ui_dream.ml** (all lines) - OCaml Dream HTML generator for `ui` specs
6. **lib/ui/ui_clay.ml** (all lines) - Clay C generator for `ui` specs
7. **lib/db/db_json.ml** (all lines) - JSON serialization for DB specs
8. **lib/dune** (lines 1-15) - Defines the `borge_lib` library with all internal modules including the three target generators

## Key Code

### CLI Entry Structure

The CLI uses **Cmdliner** for argument parsing:

```ocaml
let cmds = [
  Generate.cmd;
  (* other commands... *)
]

let () = exit (Cmd.eval (Cmd.group info cmds))
```

### Generate Subcommands (bin/cmd/Generate.ml)

Four subcommands exist in a Cmdliner group:

- **spec**: `borge generate spec <TODO> [--dir <dir>] [--quiet]`
  - Calls `Generate.generate_spec todo dir`
- **code**: `borge generate code <SECTION> [--dir <dir>] [--quiet]`
  - Calls `Generate.generate_code section dir`
- **ui**: `borge generate ui <SECTION> [--dir <dir>] [--json] [--quiet]`
  - Parses and prints UI specs, doesn't generate code
- **db**: `borge generate db <TABLE> [--dir <dir>] [--json] [--quiet]`
  - Parses and prints DB specs, doesn't generate code

**The problem**: Currently no `--target` flag exists. All three generators (postgres-sql, ocaml-dream, clay-c) are internal library functions but hidden from the CLI.

### Generate Functions (lib/generate/generate.ml)

```ocaml
let generate_spec todo_text dir = ...
let generate_code section_name dir = ...
```

These are called by the CLI but don't accept target parameters yet.

### Target Generators (Already Exposed as Library Functions)

All three target generators exist in `lib/` and are exposed via the `borge_lib` library (lib/dune):

1. **db_sql.generate** (`lib/db/db_sql.ml:230`) - Takes `db_app`, returns SQL string
   - Signature: `let generate ?(source_path = "<spec>") (app : db_app) = Buffer.contents buf`
   - Already handles: `CREATE TABLE`, `CREATE INDEX`, RLS policies, operations/relations as comments

2. **ui_dream.generate** (`lib/ui/ui_dream.ml:392`) - Takes `ui_app`, returns OCaml module string
   - Signature: `let generate ?(source_path = "<spec>") (app : ui_app) = Buffer.contents buf`
   - Handles: Component render functions, layouts, pages, routes with dream_html nodes

3. **ui_clay.generate** (`lib/ui/ui_clay.ml:378`) - Takes `ui_app`, returns C code string
   - Signature: `let generate ?(source_path = "<spec>") (app : ui_app) = Buffer.contents buf`
   - AND `let generate_header_file` for .h file generation
   - Handles: Component functions, layouts, pages, route dispatcher

### Pattern for Usage

Currently unused in CLI, but the library interface is:

```ocaml
(* pattern for db_sql *)
let sql = Db_sql.generate ~source_path:borg_path db_app
Printf.printf "%s\n" sql

(* pattern for ui_dream *)
let dream_code = Ui_dream.generate ~source_path:borg_path ui_app
(* write to file or stdout *)

(* pattern for ui_clay *)
let clay_c = Ui_clay.generate ~source_path:borg_path app
let clay_h = Ui_clay.generate_header_file app
(* write both files *)
```

## Architecture

### Command Flow

```
borge binary (bin/borge.ml)
  → Cmdliner group with Generate.cmd
  → bin/cmd/Generate.ml: generate_ui / generate_db / generate_spec / generate_code
     → lib/generate/generate.ml: generate_spec / generate_code
  → lib/db/db_parse.parse_file → db_ast.db_app
  → lib/ui/ui_parse.parse_file → ui_ast.ui_app
  → (target generators in lib/):
     → Db_sql.generate(db_app) → SQL string
     → Ui_dream.generate(ui_app) → .ml string
     → Ui_clay.generate(ui_app) → .c + .h strings
```

### Dependency Graph (Relevant Modules)

```
bin/cmd/Generate.ml
  → imports via:
      - open Borge_lib
      - Generate.* from lib/generate/generate.ml
      - Parse/format from Borge_lib (parsing .borg files)
      - File_utils (reading files)

lib/generate/generate.ml
  → Borge_lang.Parse
  → Agent.run_pi_print (LLM integration)
  → Project.find_roots
  → Convention.resolve

lib/db/db_sql.ml
  → Db_ast.db_app
  → Creates: SQL CREATE TABLE, CREATE INDEX, RLS policies

lib/ui/ui_dream.ml
  → Ui_ast.ui_app
  → Creates: OCaml .ml with dream_html nodes

lib/ui/ui_clay.ml
  → Ui_ast.ui_app
  → Creates: .c + .h with Clay layout trees
```

## Start Here

**Start at bin/cmd/Generate.ml** because:
1. It's the CLI entry point for `borge generate`
2. It already has the subcommand structure (`ui_cmd`, `db_cmd`)
3. This is where you need to add the new `target_cmd` subcommand
4. The pattern for `ui_cmd` and `db_cmd` can serve as a reference
5. After adding the subcommand, you'll call into the existing target generators in lib/

**Specific duties in Generate.ml**:
1. Add a `target_cmd` subcommand to the Cmdliner group (similar to `ui_cmd` and `db_cmd`)
2. Add `--target` argument accepting: `postgres-sql`, `ocaml-dream`, `clay-c`
3. Update `run_ui` and `run_db` handlers (or convert them) to support optional target generation
4. Wire up calls to `Db_sql.generate`, `Ui_dream.generate`, `Ui_clay.generate` based on target

**Implementation notes**:
- The three target generators already exist in lib/, no new files needed
- They all have `~source_path` optional parameter—use from current borg file path
- For clay, you'll generate both `generate` and `generate_header_file` outputs
- Follow the existing JSON/quiet flag pattern in `run_ui`/`run_db` for output control
- The output mode is code generation (not JSON inspection of specs)

### Wait, I need to reconsider `ui_cmd` and `db_cmd`

The current `ui_cmd` and `db_cmd` are **inspecting** commands (parse, validate, summarize). They have a `--json` flag but **don't generate code**. The comment says: "The UI spec IS the artifact — no code is generated. Use borge make to edit UI specs interactively."

Your goal is to add **code generation** via `--target` flag. You have two options:

**Option A (Clean break)**: Keep `ui` and `db` as inspection-only. Add a new `generate ui ... --target ...` subtype.

**Option B (Hybrid)**: Modify existing `ui` and `db` to support both inspection and generation based on flags.

Given the existing code, I'd suggest **Option A first**: keep inspection-only behavior for clarity, add a new `--target` param for code generation. If you want to simplify syntax, you could rename or consolidate later—start here since it's spec-driven and clearer.

**Updated first file**: bin/cmd/Generate.ml lines 322+ where the subcommands are defined (after `db_cmd`). Add a new top-level `target_cmd` there.

**Next steps after adding the subcommand**:
1. Create `run_postgres_sql ~dir ~quiet` that:
   - Parses DB spec
   - Calls `Db_sql.generate ~source_path:borg_path db_app`
   - Outputs SQL to stdout (or file if needed)

2. Create `run_dream ~dir ~quiet` that:
   - Parses UI spec
   - Calls `Ui_dream.generate ~source_path:borg_path ui_app`
   - Outputs .ml code to stdout (or file)

3. Create `run_clay ~dir ~quiet` that:
   - Parses UI spec
   - Calls `Ui_clay.generate ~source_path:borg_path app`
   - Calls `Ui_clay.generate_header_file app`
   - Outputs both files

4. Update the Cmdliner `target_cmd` to dispatch based on `--target` value.