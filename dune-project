(lang dune 3.1)
;; I want to use menhir for some examples, but
;; it's not clear how this interacts???
(using menhir 2.1)

(name asai)

(generate_opam_files true)

(source
 (github RedPRL/kai))

(authors "Reed Mullanix <reedmullanix@gmail.com>")

(maintainers "Reed Mullanix <reedmullanix@gmail.com>")

(license Apache-2.0)

(package
 (name asai)
 (synopsis "A library for diagnostics and span-associated metadata")
 (depends
   ocaml
   dune
   ;; [TODO: Reed M, 08/06/2022] I should conditionally compile this!
   (notty (>= 0.2))
   (lsp (>= 1.12))
   (eio (>= 0.2))
   (eio_main (>= 0.2))
   ;; Pinned Deps
   algaeff
   bwd
   ;; [TODO: Reed M, 07/06/2022] I should conditionally compile this!
   (menhir (>= 20220210))
   ;; Tests
   (alcotest (and :with-test (>= 1.5)))
   ))
