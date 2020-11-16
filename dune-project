(lang dune 2.7)
(name mlbdd)
(version 0.7)

(generate_opam_files true)

(source (github arlencox/mlbdd))
(license BSD)
(authors "Arlen Cox <arlencox@gmail.com>")
(maintainers "Arlen Cox <arlencox@gmail.com>")
(homepage "https://github.com/arlencox/mlbdd")

(package
(name mlbdd)
(synopsis "An OCaml library for Binary Decision Diagrams (BDDs)")
(description "The mlbdd library provides a simple, easy-to-use, easy-to-extend implementation of binary decision diagrams (BDDs) in OCaml. It is well tested and well documented. The library itself has no dependencies and is thus easy to include in applications that might, for example, be compiled with js_of_ocaml or other tools that rely on pure OCaml. It is also easier to integrate with existing projects due to its lack of dependencies.  Critically, this BDD implementation uses a garbage-collection-aware hashing scheme, so that unused nodes can be collected.  Additionally, this implementation uses complement edges to significantly improve performance over the simplest BDD implementations.")
)
