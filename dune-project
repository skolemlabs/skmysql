(lang dune 2.8)
(name skmysql)

(license Apache-2.0)
(maintainers "Zach Baylin <zach@skolem.tech>")
(authors "Hezekiah M. Carty <hezekiah.carty@elastic.co>" "Zach Baylin <zach@skolem.tech>")

(source (github skolemlabs/skmysql))

(package
 (name skmysql)
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
  (uri (>= 4.1.0))
  (skapm :build)))

(package
 (name skmysql_ppx)
 (synopsis "PPX for making skmysql queries")
 (depends
  (ppxlib (>= 0.20.0))
  (skmysql :build)))

(package
 (name skmysql_ppx_deriving)
 (synopsis "PPX for making skmysql tables from types")
 (depends
  (ppxlib (>= 0.20.0))
  (skmysql :build)))
