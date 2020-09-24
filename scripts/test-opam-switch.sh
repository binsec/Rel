#!/bin/sh
# test-opam-switch.sh is, as its name hints, a shell script facilitating the
# testing of new OCaml compiler versions, along with the dependencies needed to
# compile BINSEC


switch="$1"

echo "Testing OCaml version $switch ..."

opam switch create ${switch}
eval $(opam env)

# merlin is not really needed but helps if one wants to fix things in this new
# opam sandbox
packages="ocamlfind piqi piqilib zarith menhir ocamlgraph llvm zmq merlin"

echo "Installing packages $packages ... "
opam install -y ${packages}


echo "----"
echo "Now run ./configure; make clean; make to test this OCaml version" 
echo "----"
echo "You may also need to add a case for the corresponding OCaml version in
configure.ac. Run autoconf before if this is the case."

