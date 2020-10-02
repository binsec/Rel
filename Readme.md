# Binsec/Rel: Efficient Relational Symbolic Execution for Constant-Time at Binary-Level
Binsec/Rel is an extension of the binary analysis plateform
[Binsec](https://github.com/binsec/binsec) that implements relational
symbolic execution (RelSE) for constant-time (CT) verification.

If you are interested, you can read the
[paper](https://binsec.github.io/assets/publications/papers/2020-sp.pdf),
published at 2020 IEEE Symposium on Security and Privacy (SP).

Benchmarks to test Binsec/Rel: https://github.com/binsec/rel_bench

*Docker incoming*

## Installation
``` bash
# Install Ocaml and prerequisite packages for BINSEC via OPAM
sudo apt update
sudo apt install ocaml ocaml-native-compilers camlp4-extra opam protobuf-compiler libgmp-dev libzmq3-dev llvm-6.0-dev cmake pkg-config
opam init
opam switch 4.05.0
opam install menhir ocamlgraph piqi zarith zmq.5.0.0 llvm.6.0.0 oUnit

# Additional packages (optional)
# opam install merlin ocp-indent caml-mode tuareg ocamlfind

# Checkout source code
git clone https://github.com/binsec/Rel.git binsec-rel

# Compile source code
cd binsec-rel
autoconf
./configure
cd src
make depend
make binsec
```

Print the help:
``` bash
$ binsec --help
```

## Binsec/Rel Plugin
The Rel plugin is located under `src/relse/`.
