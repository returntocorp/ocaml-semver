(lang dune 2.0)

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
 (description "Semver is a lightweight library for parsing, comparing, and manipulating version identifiers in 3 parts such as \"1.2.3\". \"Semver\" stands for Semantic Versioning and refers to the standard documented at https://semver.org/. This library is independent from the opam package named \"semver2\".")
 (depends
   (alcotest :with-test)
   dune
   ocaml
 )
 (tags
  (semver "semantic versioning" version)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
