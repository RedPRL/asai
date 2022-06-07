(lang dune 3.1)

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
   (uuseg (>= 14.0))
   (containers (>= 3.7))
   ;; Pinned Deps
   bwd
   )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
