(lang dune 3.17)

(name borge)

(using menhir 3.0)

(generate_opam_files true)

(package
  (name borge)
  (description "A spec-driven code orchestrator")
  (depends
    (ocaml (>= 5.1))
    (dune (>= 3.17))
    (cmdliner (>= 1.3))
    (yojson (>= 2.2))
    (ppx_deriving (>= 6.0))
    sedlex
    menhir
    menhirLib))
