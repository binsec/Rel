# Information on the Rel plugin

## Global structure
The main file, [relse](relse.ml), initializes the symbolic execution and
 executes instructions.

The sate of a symbolic path is defined in the module
[Relse_path.Path_state](relse_path.mli). This module mostly defines interfaces
between other modules and symbolic state.

Symbolic states are defined in [Relse_symbolic](Relse_symbolic.mli). It is a
crucial part of RelSE that contains operations on the symbolic memory, the
symbolic store, and the path constraint.

Relational expressions are defined in the file [Rel_expr](rel_expr.mli)

The interface with the solver is defined in [Relse_smt.Solver](relse_smt.mli)
and translation of DBAs to Formulas is defined in
[Relse_smt.Translate](relse_smt.mli).

Insecurity checks are handled in the module
[Relse_insecurity](Relse_insecurity.mli).

Finally, [Relse_utils](relse_utils.mli) provides utility functions,
[Relse_options](relse_options.mli) defines the input options of the RelSE,
[Relse_stats](relse_stats.mli) handles the metrics outputted by the RelSE, and
[Relse_stubs](relse_stubs.mli) defines the function/instruction that are stubbed
during RelSE.

