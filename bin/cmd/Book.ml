open Borge_lib

let print_run dir stem root mode =
  let dir = match dir with Some d -> d | None -> "." in
  try
    let n = Book_print.print ~dir ~stem ~root ~mode in
    Printf.printf "wrote %s.pdf and %s.book.manifest (%d files)\n" stem stem n;
    exit 0
  with
  | Failure msg -> prerr_endline ("borge book: " ^ msg); exit 1
  | e -> prerr_endline ("borge book: " ^ Printexc.to_string e); exit 1

open Cmdliner

let dir_arg =
  Arg.(value & pos 0 (some string) None & info [] ~docv:"DIR"
    ~doc:"Source directory to print (defaults to the current directory)")

let stem_arg =
  Arg.(value & opt string "borge-book" & info ["o"] ~docv:"STEM"
    ~doc:"Output stem: writes STEM.pdf and STEM.book.manifest")

let root_arg =
  Arg.(value & opt (some string) None & info ["root"] ~docv:"FILE"
    ~doc:"Print exactly the inline subtree rooted at FILE (course roots, \
          single chapters). Nothing outside the subtree prints. Resolved \
          relative to DIR first, then the working directory.")

let mode_arg =
  Arg.(value & opt (enum ["workshop", Book_print.Workshop;
                           "reader", Book_print.Reader])
         Book_print.Workshop
       & info ["mode"] ~docv:"MODE"
    ~doc:"Render register. workshop (default) shows the full workflow \
          scaffolding: status labels, verify blocks, agent notes, untracked \
          lines. reader strips the scaffolding for students — prose, \
          headings, and code only. Drift holes (raw fallback listings for \
          unparseable files) render in both registers.")

let print_cmd : unit Cmd.t =
  Cmd.v (Cmd.info "print" ~doc:"print the codebase as a paginated PDF book"
    ~man:[`S "DESCRIPTION";
          `P "Chapters follow the project's (inline ...) tree order; \
              unreferenced files append lexicographically. Output is \
              deterministic: two runs of the same tree produce \
              byte-identical PDFs (only the manifest's rendered_at \
              carries wall-clock time). Emits a paginated PDF with line \
              numbers on every page and a wide right-hand margin for \
              hand annotations. Writes a <STEM>.book.manifest sidecar \
              (file identity + tree checksum) alongside the PDF.";
          `P "--root FILE renders exactly the inline subtree rooted at \
              FILE — a course root prints as that course, a single \
              chapter prints alone. A missing file or unbuildable tree \
              is a hard error.";
          `P "--mode workshop (default) shows the workflow scaffolding: \
              status labels, verify blocks, agent notes, untracked \
              lines. --mode reader strips it: prose, headings, code. \
              Drift holes render in both registers.";
          `S "EXAMPLES";
          `P "borge book print .            # writes borge-book.pdf";
          `P "borge book print lib -o lib   # writes lib.pdf";
          `P "borge book print --root course/cs.borg -o cs  # one subtree";
          `P "borge book print . --mode reader -o reader  # student edition"])
    Term.(const print_run $ dir_arg $ stem_arg $ root_arg $ mode_arg)

let cmd : unit Cmd.t =
  Cmd.group (Cmd.info "book" ~doc:"print the codebase as a book")
    [print_cmd]
