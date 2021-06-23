# Binsec/Rel: Symbolic Binary Analyzer for Constant-Time and Secret-Erasure
Binsec/Rel is an extension of the binary analysis plateform
[Binsec](https://github.com/binsec/binsec) that implements relational
symbolic execution (RelSE) for constant-time and secret-erasure verification.

You can find some use cases to test Binsec/Rel at https://github.com/binsec/rel_bench

### Publications
For more details about Binsec/Rel for constant-time analysis, you can read the
[paper](https://binsec.github.io/assets/publications/papers/2020-sp.pdf),
published at 2020 IEEE Symposium on Security and Privacy (SP).

The artifact for this paper is located on this repository, under the tag
[SP20](https://github.com/binsec/Rel/tree/SP20).



## Installation
### Docker (Old version)
The docker contains necessary files for running Binsec/Rel and the use cases to test it.

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

You are ready to go ! Read https://github.com/binsec/rel_bench for examples on
how to use Binsec/Rel.

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



## Overview of Binsec and the Binsec/Rel plugin
### Source code
The source code for the Rel plugin is located under `src/relse/`, you can find a
[Readme](./src/relse/Readme.md) that details the structure of the code.

### Binsec/Rel: a Bounded-Verification and Bug-Finding Tool
Binsec is a symbolic execution tool for binary code which makes no approximation
on the semantics of the program:
- Binsec does not **over-approximate** the semantics of the programs like tools
  based on abstract interpretation. Therefore, when Binsec finds a violation of
  a property, it is a true violation.
- By default, binsec does not **under-approximate** the semantics of the program
  like standard bug-finding tools based on dynamic symbolic execution. Binsec
  explores all possible branches of a program until reaching a given depth (or
  until it times-out). Therefore, when Binsec exhaustively explores a program
  without finding a bug, then there is no bugs in the program. Note that in
  practice, some values are concretized in our experiments e.g. the initial
  value of the stack pointer `esp`, or the size of arrays (see limitations). In
  these cases, we can only claim the absence of bugs in programs initialized
  according to these concrete values.
  
Note that Binsec (like most program analyzer) is not a **verified** program
analyzer like [Verasco](http://compcert.inria.fr/verasco/), so it might contain
bugs that can weaken these guarantees. Therefore, if you use Binsec/Rel to
verify real cryptographic code, do not trust it blindly and always double check
the results.

### Memory model and simplifications
Binsec uses a **flat memory model** in which the whole memory is represented as
a symbolic array, mapping addresses (32-bit symbolic bitvectors) to values
(8-bit symbolic bitvectors).

Optimizations based on *read-over-write* are implemented to simplify load
operations on-the-fly during the symbolic execution and improve performance.

*More details on the memory model and the optimizations in Binsec/Rel will be
coming later.*

### Limitations
1. For now, Binsec only supports 32-bit programs (support for 64-bit is almost
   ready and should be released soon).
2. Binsec does not support dynamic memory allocation. Therefore, in our
   experiments, the size of the symbolic input (keys, plaintext) is fixed.
3. In our experiments, we also concertize the value of the initial stack pointer
   `esp`. Keeping the stack pointer symbolic might lead to spurious
   violations---for instance, the solver can find a violation when initializing
   `esp` to a spurious position in order to overrite values in the `.data`
   section. An alternative option would be to constrain the initial value of
   `esp` to an interval of possible values.
4. Binsec/Rel supports ARM binaries but it has only be tested on small examples,
   the performance might vary significantly from x86 because of different
   decoding choices.

### Possible extensions
Here is a list of possible extensions that, I think, would be interesting to add
to Binsec/Rel (if someday I find the time):
- Constraint initial `esp` to an interval of values [medium difficulty],
- Add constraints on initial symbolic input [medium difficulty],
- Infer relational invariants for verifying programs with loops [hard].

*If you have any ideas, or suggestions of improvement, I'd be really happy to
hear them!*



## Using Binsec/Rel

### Specifying secret and public inputs
There are three ways to specify secrets:

1. Specify secret and public input directly in the C program using dummy
   functions `high_input_N(void* ptr, int size)` and `low_input_N(void* ptr, int
   size)`, defined in the [`libsym`
   library](https://github.com/binsec/rel_bench/blob/main/src/__libsym__/sym.h).
   See an example on
   [donna](https://github.com/binsec/rel_bench/blob/main/src/ct/donna/donna_wrapper.c#L9),
   or on [secret-erasure](https://github.com/binsec/rel_bench/blob/main/src/secret-erasure/secret-erasure.c).

2. Use global variables to store secrets and use `-relse-high-sym symbol_name`
   to indicate symbols in the Elf file that contain secret input.

3. Use the option `-relse-high-var` to specify secrets as offsets from the
   initial esp. This option requires some reverse engineering and is more
   complex to use, you should probably refrain from using it except when you
   have no other choice.

### Configure the initial memory
By default, the initial memory is fully symbolic--even the `.data` section which
contains global variables. This means that the solver can choose any value
instead of the global variable that you carefully initialized in your
program---which is probably not what you want. This section details how to
configure the initial memory so that Binsec won't return spurious violations.
There are three command line arguments to configure the initial memory:

- `-sse-load-ro-sections`: load the content of all read-only sections in the
  binary (e.g. `.rodata` section). The alternative is to keep the corresponding
  locations symbolic. (You probably want to enable it.)
  
- `-sse-load-sections sections`: specify sections to load from the binary in the
  initial symbolic memory. For instance, `-sse-load-sections
  .got.plt,.data,.plt` will load the content of the sections `.got.plt,`,
  `.data,`, and `.plt` directly from the binary file. Take care of the
  initialization of the `.bss` section which contains both variables initialized
  to 0 (that we want to concretize) and uninitialized variables (that we want to
  keep symbolic). This problem can be solve by initializing specific addresses
  from file (see `-sse-memory`) or by specifying public input explicitly (see
  section [specifying secret and public
  inputs](#specifying-secret-and-public-inputs)).
  
- `-sse-memory path`: define the path of the initialization file. See for
  example the [initialization file for
  aes](https://github.com/binsec/rel_bench/blob/main/src/ct/bearssl/memory_aes_big.txt)
  or the example below.


```
# Set initial esp to 0xffff0000
esp<32>:=0xffff0000;

# Initialize from the binary 1024 bytes in initial memory at address 0x08080808
@[0x08080808,1024] from_file;
```


### Binsec/Rel options
By default, Binsec/Rel starts its analysis from the start of the main function
and ends at the end of the main function. You can change the default entrypoint
with the command line option `-entrypoint` and the addresses to stop the
analysis with `-sse-no-explore`, e.g. `-sse-no-explore 0xdeadc0de,0xdeadc0d3`.

Binsec command line arguments can be listed with `binsec --help`. Options
specific to the Rel plugin can be listed with `-relse-help`. We explain here the
most important options:

- `-relse-fp level`: sets the frequency of insecurity checks to `level` (see
  *fp* optimization in the paper). `level` can take the following values:
  + `never`: never check insecurity queries,
  + `instr`: (default) check insecurity queries at each instruction,
  + `block`: pack insecurity queries and check them at the end of a basic block
    (loosing precision in the reported counterexample),
  + `blockprecise`: same as `block` but without loosing precision.
  
- `-relse-leak-info option`: select what to do when finding a violation,
  `option` can take the following values:
  + `halt`: stops symbolic execution after finding a violation,
  + `instr`: continue the exploration but report the faulting instruction only
    once.
    
- `-relse-property prop`: select the property to check:
  + `ct`: (default) check constant-time,
  + `secret-erasure`: check secret-erasure,
  
- `-relse-stat-file path`: define the path of the .csv file to write the
  results. If not set, outputs the results to the standard output.

- `-relse-paths n`, `-sse-depth n`, `-relse-timeout n`: set respectively, the
  number of paths to explore (default 0), the maximum depth of a path (default
  1000), and the timeout (in seconds) of the exploration (default 0). Set to 0
  for infinite.
  
- `-sse-jump-enum`: set the maximum number of jump targets to explore at dynamic
  jumps.


The following arguments can also be useful to increase the verbosity of the
analysis when debugging:

- `-sse-comment`: add comments to the generated formulas to indicate, for
  instance, the program locations corresponding to a given expression.

- `-sse-smt-dir path`: set a directory to save smt formulas generated during
  symbolic execution.

- `-relse-debug-level n`: set the verbosity of the debug to `n` where `0 <= n
  <= 10`. 


Look at [Binsec/Rel benchmarks](https://github.com/binsec/rel_bench) for more
examples on how to use Binsec/Rel.


### Common pitfalls
- Usually, copying the initialization file from one program to the other does
  not work. Memory addresses that are initialized from file for one program,
  might be spurious or non0existing addresses in another program.

- If you get spurious violations, check that the solver did not initialized
  `esp` to a spurious address. If so, you can concretize `esp` using the
  initialization file (see option `-sse-memory path`).

- Another way to get spurious violations is to forget to initialize parts of the
  memory (see section ["Configure the initial memory"](#configure-the-initial-memory)).
