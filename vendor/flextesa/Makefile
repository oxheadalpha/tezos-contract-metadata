.PHONY: fmt all build vendors clean

all: build

build:
	dune build @check src/test/main.exe src/app/main.exe && \
             ln -sf _build/default/src/app/main.exe flextesa

test:
	dune runtest

clean:
	dune clean

fmt:
	dune build flextesa.opam flextesa-cli.opam \
             tezai-base58-digest.opam \
             tezai-tz1-crypto.opam \
             @fmt --auto-promote
