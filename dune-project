(lang dune 3.0)

(name semver)

(generate_opam_files true)

(source
 (github rgrinberg/ocaml-semver))

(authors "Tikhon Jelvis")

(maintainers "Rudi Grinberg")

(license BSD-3-Clause)

(documentation https://github.com/returntocorp/ocaml-semver)

(package
 (name semver)
 (synopsis "Semantic Versioning (semver) library")
 (description "")
 (depends
   (alcotest :with-test)
   dune
   ocaml
 )
 (tags
  (semver "semantic versioning" version)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
