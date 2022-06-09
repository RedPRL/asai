(lang dune 3.1)
;; I want to use menhir for some examples, but
;; it's not clear how this interacts???
(using menhir 2.1)

(name kai)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name kai)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dune
   ;; Pinned Deps
   algaeff
   bwd
   ;; [TODO: Reed M, 07/06/2022] I should conditionally compile this!
   (menhir (>= 20220210)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
