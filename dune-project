(lang dune 2.9)
(name tezos-contract-metadata)

(source (github oxheadalpha/tezos-contract-metadata))
(license MIT)
(authors "Tezos Devteam")
(maintainers "seb@mondet.org")

(generate_opam_files true)

(package
 (name tezos-contract-metadata)
 (synopsis "Download and parse TZIP-016 Tezos contract metadata")
 (description "A library to download and parse TZIP-016 Tezos contract metadata")
 (version 0.2)
 (depends
  (base (>= "0.14.0"))
  (base58 (>= "1.0"))
  (digestif (>= "0.9.0"))
  (dune (>= "2.9"))
  (fmt (>= "0.9.0"))
  (lwt (>= "5.4.2"))
  (ppx_inline_test (>= "0.14.0"))
  (tezos-micheline (>= "10.2"))
  (tezos-tooling :with-test)
 )
)

(package
 (name tezos-contract-metadata-test)
 (synopsis "Tests for tezos-contract-metadata")
 (description "A separate projeect because of different tests")
 (depends
  (base (>= "0.14.0"))
  (dune (>= "2.9"))
  (ppx_expect (>= "0.14.0"))
  (ppx_inline_test (>= "0.14.0"))
  (tezos-contract-metadata (= "0.2"))
 )
)

(package
 (name tezos-contract-metadata-cli)
 (synopsis "A CLI to download and parse TZIP-016 Tezos contract metadata")
 (description "A CLI to download and parse TZIP-016 Tezos contract metadata")
 (depends
  (cohttp (>= "4.0.0"))
  (cohttp-lwt-unix (>= "4.0.0"))
  (dune (>= "2.9"))
  (fmt (>= "0.9.0"))
  (lwt (>= "5.4.2"))
  (ppx_expect (>= "0.14.0"))
  (ppx_inline_test (>= "0.14.0"))
  (tezos-contract-metadata (= "0.2"))
  (tezos-micheline (>= "10.2"))
  (tezos-tooling :with-test)
  (tls(>= "0.15.0"))
 )
)

;; Vendors
(package (name tezai-base58-digest))