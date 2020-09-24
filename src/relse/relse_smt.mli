module Path_state = Relse_path.Path_state

module Solver : sig
  
  (* val with_solver :
   *   Relse_stats.query_type ->
   *   Relse_path.Path_state.t -> (Solver.Session.t -> 'a) -> 'a option *)
      
  (* val default : 'a -> 'a option -> 'a *)

  (** [check_sat_with_asserts ps list] Check the satisfiability of the
     path [ps] with the extra assertions in [list] *)
  val check_sat_with_asserts :
    Relse_stats.query_type -> Formula.bl_term list -> Path_state.t ->
    Formula.status * Path_state.t
                             
  (** [check_sat ps] Check the satisfiability of the path [ps] *)
  val check_sat :
    Path_state.t ->
    Formula.status * Path_state.t

  (** [get_model_with_asserts pas list] Get a model assigning values
     to symbolic inputs that exercises the path [ps] with the extra
     assertions specified in [list]*)
  val get_model_with_asserts : Formula.bl_term list -> Path_state.t -> Smt_model.t
                         
  (** [get_model ps] Get a model assigning values to symbolic
      inputs that exercises the path [ps] *)
  val get_model : Path_state.t -> Smt_model.t

  (** [enumerate_values n expr State.t] Returns a maximum of [n]
     values satisfying [expr] in [path_state] *)
  val enumerate_values :
    int -> Formula.bv_term -> Path_state.t ->
    Bitvector.t list * Path_state.t
end

module Translate : sig
  
  (** [expr symbolic_state e high] Returns the two formulas
      representing the evaluation of the expressions [e] on the state
      [symbolic_state] in the original program and its renamed version.
      If [e] uses undeclared variables, those variables are declared as
      high if [high=true] or low otherwise.  *)
  val expr :
    ?high:bool -> Relse_symbolic.State.t -> Dba.Expr.t -> Rel_expr.rel_bv
                                                       
  (** [assignment lval rval path_state] If [high=true], all the
     variables created by [rval] are high. *)
  val assignment :
    ?high:bool -> Dba.LValue.t -> Dba.Expr.t -> Path_state.t ->
    Path_state.t
end
