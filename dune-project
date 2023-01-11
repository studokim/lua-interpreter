(lang dune 3.6)

(name lua)

(generate_opam_files false)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lua)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune angstrom)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
