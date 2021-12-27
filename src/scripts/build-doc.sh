#! /bin/sh

set -e

usage () {
    cat >&2 <<EOF
usage: $0 <output-path>

EOF
}

shout () {
    YELLOW='\033[0;33m'
    NC='\033[0m'
    if [ "no_color" = "true" ]; then
        printf "$@"
    else
        printf "$YELLOW"; printf "$@" ; printf "$NC"
    fi
}

say () {
    shout "[Make-doc] " >&2
    printf "$@" >&2
    printf "\n" >&2
}

if ! [ -f src/scripts/build-doc.sh ] ; then
    say "This script should run from the root of the flextesa tree."
    exit 1
fi

output_path="$1"
if [ "$output_path" = "" ] ; then
    shout "Missing argument"
    usage
    exit 2
fi

mkdir -p "$output_path/api"

opam exec -- opam install --yes odoc odig omd.1.3.1

say "Build @doc"
opam exec -- dune build @doc


cp -r _build/default/_doc/_html/* "$output_path/"
chmod -R u+rw "$output_path/"

say "Getting Odig theme"

cp -r $(odig odoc-theme path odig.light)/* "$output_path/"

lib_index_fragment=$(mktemp "/tmp/lib-index-XXXX.html")
odoc html-frag src/doc/index.mld \
     -I _build/default/src/lib_base58_digest/.tezai_base58_digest.objs/byte/ \
     -I _build/default/src/lib_tz1_crypto/.tezai_tz1_crypto.objs/byte/ \
     -I _build/default/src/lib/.flextesa.objs/byte/ -o "$lib_index_fragment"
lib_index="$output_path/lib-index.html"

main_index_fragment=$(mktemp "/tmp/main-index-XXXX.html")
toc_spot=$(awk '/^<!--TOC-->/ { print NR + 1; exit 0; }' README.md)
say "README TOC is at line $toc_spot"
head -n "$toc_spot" ./README.md  | omd > "$main_index_fragment"
{ echo '# Table of Contents' ; tail +$toc_spot  ./README.md ; } \
    | omd -otoc >> "$main_index_fragment"
tail +$toc_spot  ./README.md \
    | sed 's@https://tezos.gitlab.io/flextesa/lib-index.html@./lib-index.html@' \
    | sed 's@./src/doc/mini-net.md@./mini-net.html@' \
    | omd >> "$main_index_fragment"
main_index="$output_path/index.html"


mini_net_fragment=$(mktemp "/tmp/mini-net-XXXX.html")
{
    cat ./src/doc/mini-net.md ;
    cat <<'EOF'

## Manpage Of mini-net

For convenience, here is the output of `flextesa mini-net --help`:

EOF
    echo '``````'
    ./flextesa mini-net --help=plain
    echo '``````'
} | omd >> "$mini_net_fragment"
mini_net="$output_path/mini-net.html"

make_page () {
    input="$1"
    output="$2"
    title="$3"
cat > "$output" <<EOF
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>$title</title>
    <link rel="stylesheet" href="./odoc.css"/>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
  </head>
  <body class="odoc">
    <main class="odoc-content">
EOF
cat "$input" >> "$output"
cat >> "$output" <<'EOF'
    </main>
  </body>
</html>
EOF
}

make_page "$lib_index_fragment" "$lib_index" "Flextesa: API"
make_page "$main_index_fragment" "$main_index" "Flextesa: Home"
make_page "$mini_net_fragment" "$mini_net" "Flextesa: Mini-net Command"


say "done: file://$PWD/$main_index"
say "done: file://$PWD/$mini_net"
say "done: file://$PWD/$lib_index"
say "done: file://$PWD/$output_path/flextesa/Flextesa/index.html"


