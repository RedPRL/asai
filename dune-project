(lang dune 3.1)
;; I want to use menhir for some examples, but
;; it's not clear how this interacts???
(using menhir 2.1)

(name asai)

(generate_opam_files true)

(source (github RedPRL/asai))
(license Apache-2.0)
(authors "The RedPRL Development Team")
(maintainers "favonia <favonia@gmail.com>")

(package
 (name asai)
 (synopsis "A library for constructing and printing compiler diagnostics")
 (description "This package offers an implementation of compiler diagnostics and supports multiple handlers for displaying diagnostics to the end user. Currently, the package comes with built-in handlers for GitHub Actions workflow commands and UNIX terminals.")
 (depends
   (ocaml (>= 5.0))
   (algaeff (>= 1.0))
   (bwd (>= 2.2))
   (notty (>= 0.2.3))
   (alcotest (and :with-test (>= 1.5)))
   (odoc :with-doc)))
(package
 (name asai-lsp)
 (synopsis "LSP handler for the package asai")
 (description "This package offers an language server protocol (LSP) handler that can turn an application using the asai package into a LSP server. This is an experimental prototype and very likely its design will significantly change in future versions.")
 (depends
   (ocaml (>= 5.0))
   (bwd (>= 2.2))
   (eio (>= 0.12))
   (eio_main (>= 0.12))
   (lsp (>= 1.15))
   (asai (= :version))
   (odoc :with-doc)))
(package
 (name asai-examples)
 (synopsis "Examples of the package asai")
 (description "This package offers some examples of the asai package. They are organized into a separate package to reduce the dependencies of the main package.")
 (depends
   (ocaml (>= 5.0))
   (algaeff (>= 1.0))
   (bwd (>= 2.2))
   (menhir (>= 20220210))
   (asai (= :version))
   (odoc :with-doc)))
