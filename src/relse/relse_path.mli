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

(** The symbolic state and symbolic path of the current symbolic execution *)
module Path_state :
sig

  module Sym_state = Relse_symbolic.State
  type t

  (** Current depth of the symbolic execution *)
  val depth : t -> int

  (** [status ps] Return the status of the path [ps] (SAT, UNSAT,
     UNKNOWN) *)
  val status : t -> Formula.status

  (** Create a new path *)
  val create :
    ?depth:int -> ?path:Relse_utils.AddressList.t option -> ?block_index:int ->
    Sym_state.t -> Instruction.t -> t

  (** If no-comment option is disabled, add a comment to the symbolic
     state *)
  val maybe_add_comment : t -> string -> t
  
  (** Returns the current instuction *)
  val get_instruction : t -> Instruction.t

  (** Returns the current instuction in a DBA form *)
  val get_dba_instruction : t -> Dba.Instr.t
  (* val get_current_statement : t -> Dba_types.Sym_statement.t *)

  (** [get_pc ps] Returns the paths contraint of the path state [ps] *)
  val get_pc : t -> Formula.bl_term
  
  val symbolic_state : t -> Sym_state.t
  
  (* TODO: remove one of them *)
  (** Set the symbolic state *)
  val set_symbolic_state : Sym_state.t -> t -> t
  val on_symbolic_state : (Sym_state.t -> Sym_state.t) -> t -> t


  (** Get the virtual address of the current instruction *)
  val virtual_address : t -> Virtual_address.t

  (** Get the address of the current instruction as a Caddress *)
  val location : t -> Dba_types.Caddress.t

  (** Get the current statement (Caddr + DBA instuction) *)
  val get_current_statement : t ->  Dba_types.Statement.t
  
  (** [get_block_index ps] Returns the block index of [ps] *)
  val get_block_index : t -> int

  (* (\** {3 Modifiers} *\) *) 

  (** [set_block_index idx path_state] Set the block index of the path *)
  val set_block_index : int -> t -> t

  (** Jump to the specified virtual address *)
  val goto_vaddr : Virtual_address.t -> t -> t

  (** [assign value ps] Perform the update [value] on the symbolic store of [ps] *)
  val assign: Relse_utils.assignment_t -> t -> t
  
  (** [update_pc_cond r_expr ps] Update path state [ps] after a
     conditional with the relational condition [r_expr]. *)
  val update_pc_cond : ?checked:bool -> Formula.bl_term Rel_expr.t -> t -> t

  (** [update_pc_static r_expr ps] Update the path state [ps] with the
     relational consition [r_expr] after a dynamic jump *)
  val update_pc_dynamic : ?checked:bool -> Formula.bl_term Rel_expr.t -> t -> t

  (* [add_assertion expr ps] Add assertion expr to ps formula *)
  val add_assertion : Formula.bl_term -> t -> t

  (** Initializes the memory at address [addr] *)
  val with_init_mem_at: addr:Bitvector.t -> size:int -> t -> t

  (** [address_belongs_to_init addr p] Check if the address [addr]
      belongs to the initialized memory locations of the symbolic state *)
  val address_belongs_to_init: addr:Bitvector.t -> t -> bool

  val mark_sat : t -> t

  (** {2 Printers} *)
    
  (** [pp_path ps] Pretty print the current location of [ps] *)
  val pp_loc : Format.formatter -> t -> unit
    
  (** [pp_path ps path_number] Pretty print the path leading to [ps]*)
  val pp_path : t -> int -> unit

end
