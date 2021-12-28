# Tezos Contract Metadata

> TZIP-{016,012,021} Contract Metadata in OCaml, see
> [gitlab.com/tezos/tzip#current-tzips](https://gitlab.com/tezos/tzip#current-tzips).

## Build

Get dependencies:

```
opam switch create . 4.12.0
eval $(opam env)
opam install --deps-only tezos-contract-metadata.opam
opam install ocamlformat.0.19.0 merlin # For development.
```

Then

```
dune build @check
```


Linting:

```
dune build @fmt --auto-promote
```

## Test

```
opam install --deps-only tezos-contract-metadata-test.opam

dune test
```

Note that some tests are in src/test to allow a lwt.unix dependency.  Since
some users of ths lib might want to use js_of_ocaml.lwt, we don't want to depend
on some any lwt in src/lib.

## Vendor Libraries

Tezai-base58-digest, vendored because unreleased (name and canonical location
are temporary):

```default
 $ flextesa=/path/to/flextesa/
 # mkdir -p  vendor/tezai-base58-digest
 $ rsync -va $flextesa/src/lib_base58_digest/ vendor/tezai-base58-digest
 $ git -C $flextesa describe --always
7503cc81
```




## History

This library was initially part of the Octez merge-request
[!2447](https://gitlab.com/tezos/tezos/-/merge_requests/2447) (*Add TZIP-016
(Contract Metadata) implementation (library + tezos-client)*, `@smondet`).  And
has been continuously in use in a couple of projects, including
[tzcomet.io](https://tzcomet.io/).
