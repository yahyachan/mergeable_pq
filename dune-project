(lang dune 3.6)

(name mergeable_pq)

(generate_opam_files true)

(source
 (github yahyachan/mergeable_pq))

(authors "yahyachan")

(maintainers "yahyachan")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name mergeable_pq)
 (synopsis "Implementations of various mergeable heaps in OCaml.")
 (description "Implementations of various mergeable heaps in OCaml.")
 (depends (ocaml (>= "4.08.0")) dune)
 (tags
  ("data structure" "priority queue")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
