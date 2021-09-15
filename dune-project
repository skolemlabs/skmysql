(lang dune 2.8)
(name ezmysql)

(license Apache-2.0)
(maintainers "Hezekiah M. Carty <hezekiah.carty@elastic.co>" "Zach Baylin <zach@skolem.tech>")
(authors "Hezekiah M. Carty <hezekiah.carty@elastic.co>" "Zach Baylin <zach@skolem.tech>")

(source (github skolemlabs/ezmysql))

(generate_opam_files true)

(package
 (name ezmysql)
 (synopsis "Simple wrapper over ocaml-mysql")
 (depends
  (ocaml (>= 4.08.0))
  (astring (>= 0.8.5))
  (calendar (>= 2.04))
  (dune (>= 2.8.0))
  (fmt (>= 0.8.5))
  (mysql8 :build)
  (ocamlgraph (>= 2.0.0))
  (rresult (>= 0.6.0))
  (uri (>= 4.2.0))
  (elastic-apm :build)))
