(lang dune 2.8)
;; (name flextesa)
(generate_opam_files true)
(license MIT)
(source (uri git+https://gitlab.com/tezos/flextesa.git))
(homepage https://gitlab.com/tezos/flextesa)
(bug_reports https://gitlab.com/tezos/flextesa)
(authors "Seb Mondet <seb@mondet.org>" "Mark Nichols")
(maintainers "seb@mondet.org")

(package
 (name tezai-base58-digest)
 (synopsis "Tezos Base58 hash-encodings based on Digestif")
 (description "Tezos Base58 hash-encodings, including common prefixes, \
               based on the Digestif library.")
 (version 0.2)
 (depends
  (digestif (>= "0.9.0"))
  (dune (>= "2.9"))
  (ppx_inline_test (>= "0.14.0"))
  (ppx_expect (>= "0.14.0"))
 )
)

(package
 (name tezai-tz1-crypto)
 (synopsis "Tezos Ed25519 Accounts Based on Mirage-Crypto")
 (description "Tezos Ed25519 Accounts Based on Mirage-Crypto.")
 (version 0.2)
 (depends
  "tezai-base58-digest"
  "mirage-crypto"
  (dune (>= "2.9"))
  (ppx_inline_test ( >= "0.14.0"))
  (ppx_expect (>= "0.14.0"))
 )
)

(package
 (name flextesa)
 (synopsis "Flexible Tezos Sandboxes: A library to define sandboxed Tezos networks")
 (description "Flexible Tezos Sandboxes: A library to define sandboxed Tezos networks.")
 (depends
  (dune (> 2.8))
  ("genspio" (= "0.0.2"))
  base
  fmt
  ppx_sexp_conv
  ezjsonm
  dum
  cmdliner
  tls
  cohttp-lwt-unix
  (pp (>= 1.1.2))
  tezai-base58-digest
  tezai-tz1-crypto
  ))

(package
 (name flextesa-cli)
 (synopsis "A CLI application using Flextesa-lib to define a few scenarios")
 (description "A CLI application using Flextesa-lib to define a few scenarios.")
 (depends
  (dune (> 2.8))
  flextesa
  )
  )
