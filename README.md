# Tezos Contract Metadata

> TZIP-{016,012,021} Contract Metadata in OCaml, see
> [gitlab.com/tezos/tzip#current-tzips](https://gitlab.com/tezos/tzip#current-tzips).

## Build

Get dependencies:

```
opam switch create . 4.12.0
eval $(opam env)
opam install --deps-only src/lib/tezos-contract-metadata.opam
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
dune test
```

Note that some tests are in src/test to allow a lwt.unix dependency.  Since
some users of ths lib might want to use js_of_ocaml.lwt, we don't want to depend
on some any lwt in src/lib.


## History

This library was initially part of the Octez merge-request
[!2447](https://gitlab.com/tezos/tezos/-/merge_requests/2447) (*Add TZIP-016
(Contract Metadata) implementation (library + tezos-client)*, `@smondet`).  And
has been continuously in use in a couple of projects, including
[tzcomet.io](https://tzcomet.io/).
