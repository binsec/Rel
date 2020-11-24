# Binsec/Rel: Efficient Relational Symbolic Execution for Constant-Time at Binary-Level
Binsec/Rel is an extension of the binary analysis plateform
[Binsec](https://github.com/binsec/binsec) that implements relational
symbolic execution (RelSE) for constant-time (CT) verification.

If you are interested, you can read the
[paper](https://binsec.github.io/assets/publications/papers/2020-sp.pdf),
published at 2020 IEEE Symposium on Security and Privacy (SP).

Benchmarks to test Binsec/Rel: https://github.com/binsec/rel_bench

## Installation
### Docker
The docker contains necessary files for running Binsec/Rel and the benchmarks to test it.

1. Download the [image](https://doi.org/10.5281/zenodo.4134497).

2. Import the image:
```
docker load < binsec-rel.tar
```

3. Run the container:
```
docker run -it binsec-rel /bin/bash
```

4. Run `./update.sh` to get the latest version of Binsec/Rel.

5. Run the tests with `cd rel_bench; make tests`

You are ready to go ! (Read https://github.com/binsec/rel_bench for examples on how to use Binsec/Rel).

### From sources
**Requirements**: boolector (recommended boolector-3.2.0), z3, yices or cvc4.

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
