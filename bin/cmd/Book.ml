open Borge_lib

let print_run dir stem =
  let dir = match dir with Some d -> d | None -> "." in
  try
    let n = Book_print.print ~dir ~stem in
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

let print_cmd : unit Cmd.t =
  Cmd.v (Cmd.info "print" ~doc:"print the codebase as a paginated PDF book"
    ~man:[`S "DESCRIPTION";
          `P "Chapters follow the project's (inline ...) tree order; \
              unreferenced files append lexicographically. Emits a paginated \
              PDF with line numbers on every page and a wide right-hand \
              margin for hand annotations. Writes a <STEM>.book.manifest \
              sidecar (file identity + tree checksum) alongside the PDF.";
          `S "EXAMPLES";
          `P "borge book print .            # writes borge-book.pdf";
          `P "borge book print lib -o lib   # writes lib.pdf"])
    Term.(const print_run $ dir_arg $ stem_arg)

let cmd : unit Cmd.t =
  Cmd.group (Cmd.info "book" ~doc:"print the codebase as a book")
    [print_cmd]
