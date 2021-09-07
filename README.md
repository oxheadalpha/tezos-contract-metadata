# Tezos Contract Metadata

> TZIP-{016,012,021} Contract Metadata in OCaml

## Build

Get dependencies:

```
opam switch create . 4.12.0
opam install --deps-only src/lib/tezos-contract-metadata.opam
opam install ocamlformat.0.19.0 merlin
```

Then

```
eval $(opam env)
dune build @check
```

