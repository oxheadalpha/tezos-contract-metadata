Flextesa: Flexible Tezos Sandboxes
==================================

This repository contains the Flextesa library used in
[tezos/tezos](https://gitlab.com/tezos/tezos) to build the `tezos-sandbox`
[tests](https://tezos.gitlab.io/developer/flextesa.html), as well as some extra
testing utilities, such as the `flextesa` application, which may be useful to
the greater community (e.g. to test third party tools against fully functional
Tezos sandboxes).


<!--TOC-->


## Run With Docker

The current _released_ image is `oxheadalpha/flextesa:20211221` (also available
as `oxheadalpha/flextesa:latest`):

It is built top of the `flextesa` executable and Octez suite, for 2
architectures: `linux/amd64` and `linux/arm64/v8` (tested on Apple Silicon); it
also contains the `*box` scripts to quickly start networks with predefined
parameters. For instance:

```sh
image=oxheadalpha/flextesa:20211221
script=hangzbox
docker run --rm --name my-sandbox --detach -p 20000:20000 \
       -e block_time=3 \
       "$image" "$script" start
```

All the available scripts start single-node full-sandboxes (i.e. there is a
baker advancing the blockchain):

- `hangzbox`: Hangzhou protocol.
- `ithacabox`: Ithaca protocol.
- `alphabox`: Alpha protocol, the development version
  of the `J` protocol at the time the docker-build was last updated.
    - See also `docker run "$image" tezos-node --version`.

The default `block_time` is 5 seconds.

See also the accounts available by default:

```default
$ docker exec my-sandbox $script info
Usable accounts:

- alice
  * edpkvGfYw3LyB1UcCahKQk4rF2tvbMUk8GFiTuMjL75uGXrpvKXhjn
  * tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb
  * unencrypted:edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq
- bob
  * edpkurPsQ8eUApnLUJ9ZPDvu98E8VNj4KtJa1aZr16Cr5ow5VHKnz4
  * tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6
  * unencrypted:edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt

Root path (logs, chain data, etc.): /tmp/mini-box (inside container).
```

The implementation for these scripts is `src/scripts/tutorial-box.sh`, they are
just calls to `flextesa mini-net` (see its general
[documentation](./src/doc/mini-net.md)).

The scripts run sandboxes with archive nodes for which the RPC port is `20 000`.
You can use any client, including the `tezos-client` inside the docker
container, which happens to be already configured:

```default
$ alias tcli='docker exec my-sandbox tezos-client'
$ tcli get balance for alice
2000000 ꜩ
```

You can always stop the sandbox, and clean-up your resources with:
`docker kill my-sandbox`.

The scripts inherit the [mini-net](./src/doc/mini-net.md)'s support for
user-activated-upgrades (a.k.a. “hard forks”). For instance, this command starts
a Hangzhou sandbox which switches to Ithaca at level 20:

```default
$ docker run --rm --name my-sandbox --detach -p 20000:20000 \
         -e block_time=2 \
         "$image" hangzbox start --hard-fork 20:Ithaca:
```

With `tcli` above and `jq` you can keep checking the following to observe the
protocol change:

```default
$ tcli rpc get /chains/main/blocks/head/metadata | jq .level_info,.protocol
{
  "level": 24,
  "level_position": 23,
  "cycle": 2,
  "cycle_position": 7,
  "expected_commitment": true
}
"PsiThaCaT47Zboaw71QWScM8sXeMM7bbQFncK9FLqYc6EKdpjVP"
```

Notes:

- The default cycle length in the sandboxes is 8 blocks and switching protocols
  before the end of the first cycle is not supported by Octez.
- The `hangzbox` script can also switch to `Alpha` (e.g.
  `--hard-fork 16:Alpha:`).

These scripts correspond to the tutorial at
<https://assets.tqtezos.com/docs/setup/2-sandbox/> (which is now deprecated but
still relevant).


## Build

With Opam ≥ 2.1:

```sh
opam switch create . --deps-only \
     --formula='"ocaml-base-compiler" {>= "4.13" & < "4.14"}'
eval $(opam env)
opam install --deps-only --with-test --with-doc \
     ./tezai-base58-digest.opam ./tezai-tz1-crypto.opam \
     ./flextesa.opam ./flextesa-cli.opam # Most of this should be already done.
opam install merlin ocamlformat.0.19.0    # For development.
```

Then:

    make

The above builds the `flextesa` library, the `flextesa` command line application
(see `./flextesa --help`) and the tests (in `src/test`).


## MacOSX Users

At runtime, sandboxes usually depend on a couple of linux utilities.

If you are on Mac OS X, you can do `brew install coreutils util-linux`. Then run
the tests with:

```
export PATH="/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/util-linux/bin:$PATH"
```

## Build Of The Docker Image

See `./Dockerfile`, it often requires modifications with each new version of
Octez or for new protocols, the version of the Octez static binaries (`x86_64`
and `arm64`) is set in `src/scripts/get-octez-static-binaries.sh`.

There are 2 images: `-build` (all dependencies) and `-run` (stripped down image
with only runtime requirements).

The `x86_64` images are built by the CI, see the job `docker:images:` in
`./.gitlab-ci.yml`.

To build locally:

```sh
docker build --target build_step -t flextesa-build .
docker build --target run_image -t flextesa-run .
```

Do not forget to test it:
`docker run -it "$image" hangzbox start`

To build the **released multi-architecture images**, we use
[buildx](https://docs.docker.com/buildx/working-with-buildx/).  In short, this
is the build itself:

```sh
docker buildx build --platform linux/arm64/v8,linux/amd64  . \
       --target run_image \
       --tag oxheadalpha/flextesa:test-20220320 \
       --tag oxheadalpha/flextesa:test-latest \
       --push
```

The build does not fit within the limits of Gitlab-CI.  Here are the
instructions for Ubuntu 20.04 (Using a “click next” AWS instance: start an
_“Ubuntu Server 20.04 LTS”_ host, the build can use quite a few CPUs at once and
requires a larger disk, e.g. 128 GiB).

Setting up Docker:

```sh
sudo apt update
sudo apt install docker.io
sudo adduser ubuntu docker
```

(may have to `sudo su ubuntu` to really get _into the group_)

Install the `buildx` CLI plugin:

```sh
mkdir -p ~/.docker/cli-plugins/
curl -L -o ~/.docker/cli-plugins/docker-buildx https://github.com/docker/buildx/releases/download/v0.7.1/buildx-v0.7.1.linux-amd64
chmod a+x ~/.docker/cli-plugins/docker-buildx
docker buildx --help # Test it !
```

Prepare the Qemu setup:

```sh
docker run --rm --privileged multiarch/qemu-user-static \
       --reset -p yes --credential yes
```

Prepare the buildx environment:

```sh
docker login # Interactive, asks for user/password
docker buildx create --use # Starts a container to clean-up later
```

Get the checkout of Flextesa you want to build:

```sh
git clone https://gitlab.com/smondet/flextesa -b smondet-docker-arm64
cd flextesa
```

And, finally, start the build/tag/push in one go:

```sh
docker buildx build --platform linux/arm64/v8,linux/amd64  . \
       --target run_image \
       --tag oxheadalpha/flextesa:rc-20211210 \
       --tag oxheadalpha/flextesa:rc-latest \
       --push
```



## More Documentation

The command `flextesa mini-net [...]` has a dedicated documentation
page: [The `mini-net` Command](./src/doc/mini-net.md).

The API documentation of the Flextesa OCaml library starts here:
[Flextesa: API](https://tezos.gitlab.io/flextesa/lib-index.html).

Some documentation, including many examples, is part of the `tezos/tezos`
repository:
[Flexible Network Sandboxes](https://tezos.gitlab.io/developer/flextesa.html)
(it uses the `tezos-sandbox` executable which is implemented there).

Blog posts:

- [2019-06-14](https://obsidian.systems/blog/introducing-flextesa-robust-testing-tools-for-tezos-and-its-applications)
- [2021-10-14](https://medium.com/the-aleph/new-flextesa-docker-image-and-some-development-news-f0d5360f01bd)
- [2021-11-29](https://medium.com/the-aleph/flextesa-new-image-user-activated-upgrades-tenderbake-cc7602781879)

TQ Tezos' [Digital Assets on Tezos](https://assets.tqtezos.com)
documentation shows how to quickly set up a
[docker sandbox](https://assets.tqtezos.com/setup/2-sandbox)
(uses the docker images from this repository).
