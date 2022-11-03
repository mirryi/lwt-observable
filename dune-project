(lang dune 3.4)
(generate_opam_files true)

(name lwt-observable)
(source
 (github mirryi/lwt-observable))
(authors "Eric Zhao")
(maintainers "Eric Zhao")
(license MIT)

(package
 (name lwt-observable)
 (synopsis "Asynchronous push streams for ==lwt==.")
 (depends ocaml dune lwt lwt-dllist (odoc :with-doc)))
