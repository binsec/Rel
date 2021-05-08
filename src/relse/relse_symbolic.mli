(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)
type value_t = Rel_expr.rel_bv
type index_t = Rel_expr.rel_bv
type constraint_t = Rel_expr.rel_pc

module State : sig
  type t
  
  val initialisations : t -> int Bitvector.Collection.Htbl.t

  (** [init_mem_at addr size st] Initializes the memory of the symbolic state
      [st] at address [addr] with size [size] *)
  val init_mem_at : addr:Bitvector.t -> size:int -> t -> unit
  
  (** [create store] Create a new state with store [store] *)
  val create : unit -> t

  (** [add_assert st h]  Add the assertion [h] to the state [st]*)
  val add_assertion : Formula.bl_term -> t -> t

  (** [declare level name var_type t] declares a the variable [name]
     with type [var_type] and security level [level] in state [t] *)
  val declare : Relse_utils.level -> string -> Formula.sort -> t -> t
  
  (** [comment cmt st] Add the comment [cmt] to the current formula in [st] *)
  val comment : string -> t -> t

  (** [untaint r_expr st] If the untainting option is set and [r_expr
     = <r_expr_l|r_expr_r>] is relational, deduces the variables
     [<v_l|v_r>] that must be equal in both sides of the expression
     given that [r_expr_l] and [r_expr_r] are equal, and add them to
     variables to untaint (replace further [v_r] occurences with [v_l]
     occurences). *)
  (* val untaint_bl : Rel_expr.rel_pc -> t -> (Rel_expr.rel_pc * t) *)
  val untaint_bv : Rel_expr.rel_bv -> t -> t
  

  val pc_update : t -> constraint_t -> t
  val var_assign : t -> string -> Size.Bit.t -> value_t -> t
  val memory_store : t -> int -> index_t -> value_t -> t

  
  (** Return the current path constraint *)
  val path_constraint : t -> Formula.bl_term

  (** [var_load st name size] Returns the value of the variable [name]
     in the symbolic store *)
  val var_load : t -> string -> Size.Bit.t -> value_t

  (** [memory_select st size r_index] Selects [size] bits in memory at
     index [r_index] *)
  val memory_select : t -> Dba.size -> index_t-> value_t

  (** Returns the current formula of the symbolic state *)
  val formula : t -> Formula.formula
end
