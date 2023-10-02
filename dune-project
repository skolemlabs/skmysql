(lang dune 2.8)
(name skmysql)

(license Apache-2.0)
(maintainers "Zach Baylin <zach@skolem.tech>")
(authors "Hezekiah M. Carty <hezekiah.carty@elastic.co>" "Zach Baylin <zach@skolem.tech>")

(source (github skolemlabs/skmysql))

(generate_opam_files true)

(version 1.4.0)

(package
 (name skmysql)
 (synopsis "Simple wrapper over ocaml-mysql")
 (depends
  mysql8
  skapm
  (ocaml (>= 4.14.0))
  (astring (>= 0.8.5))
  (calendar (>= 2.04))
  (dune (>= 2.8.0))
  (fmt (>= 0.8.5))
  (ocamlgraph (>= 2.0.0))
  (rresult (>= 0.6.0))
  (uri (>= 4.1.0))
  (logs (>= 0.6.0))))

(package
 (name skmysql_ppx)
 (synopsis "PPX for making skmysql queries")
 (depends
  (ppxlib (>= 0.20.0))
  (skmysql :version)))

(package
 (name skmysql_ppx_deriving)
 (synopsis "PPX for making skmysql tables from types")
 (depends
  (ppxlib (>= 0.20.0))
  (skmysql :version)))
