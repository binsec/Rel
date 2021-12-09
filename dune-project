(lang dune 2.8)
(using menhir 2.0)
(generate_opam_files true)

(name relse)
(version "0.3.0")
(maintainers "Lesly-Ann Daniel <lesly-ann.daniel@ens-rennes.fr>")
(authors
  "Adel Djoudi"
  "Benjamin Farinier"
  "Frédéric Recoules"
  "Josselin Feist"
  "Lesly-Ann Daniel"
  "Manh-Dung Nguyen"
  "Mathilde Ollivier"
  "Matthieu Lemerre"
  "Olivier Nicole"
  "Richard Bonichon"
  "Robin David"
  "Ta Thanh Dinh"
  "Yaëlle Vinçont"
  "Guillaume Girol"
)
(license LGPL-2.1-or-later)
(homepage "https://binsec.github.io")
(source (github binsec/Rel))
(bug_reports "https://github.com/binsec/Rel/issues")

(package
 (name relse)
 (synopsis "Symbolic Binary Analyzer for Constant-Time and Secret-Erasure")
 (description "
Binsec/Rel is an extension of the binary analysis plateform Binsec that implements relational symbolic execution (RelSE) for constant-time and secret-erasure verification.")
 (depends
  (ocaml (and (>= 4.05) (< 4.14)))
  (menhir (and :build (>= 20181113)))
  (ocamlgraph (>= 1.8.5))
  (zarith (>= 1.4))
  mmap
  unisim_archisec))
