(lang dune 3.11)

(name orderbook)

(version 0.1.0)

(generate_opam_files true)

(license MIT)

(authors "Jason Pereira")

(maintainers "Jason Pereira")

(package
 (name orderbook)
 (synopsis "Price-time-priority limit order book and matching engine")
 (description
  "A FIFO limit order book and matching engine in OCaml, with a market-event \
   simulator, property-based invariant tests, and a benchmark harness.")
 (depends
  (ocaml
   (>= 5.1))
  (dune
   (>= 3.11))
  core
  core_unix
  core_bench
  ppx_jane
  ppx_expect
  base_quickcheck))
