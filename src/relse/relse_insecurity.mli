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

(** Module that handles all the insecurity checks.
    Contains all the checks related to a path.

    Depends on the following options:
    - LeakInfo: determines the information related to an insecurity
      query
    - FaultPacking: determines the frequency of insecurity checks
    - Dedup: the deduplication parameter for insecurity checks
    - PrintModel: determines whether a model is printed for satisfiable
      insecurity queries
*)
module type INSECURITY_STATE = sig
  type t

  (** Creates a new empty insecurity state *)
  val initialize : Relse_path.Path_state.t -> (Relse_path.Path_state.t * t)

  (** [declare_input ?level lval name ps] Declares a variable [name]
     with security level [level] and assigns the lvalue [lval := name]
     in [ps] *)
  val declare_input: ?level:Relse_utils.level -> Dba.LValue.t -> string -> Relse_path.Path_state.t -> t ->
    Relse_smt.Path_state.t * t
  
  (** [declare_input_addr ?level addr name ps] Declares a variable
     [name] with secuirty level [level] and stores it a address [addr]
     in ps: [@[addr] := name] *)
  val declare_input_addr: ?level:Relse_utils.level -> Dba.Expr.t -> string -> Relse_path.Path_state.t -> t ->
    Relse_smt.Path_state.t * t
  
  (** [eval instr ps t] add insecurity checks corresponding to
     instruction [instr] in constex [ps] and check insecurity queries
     if necessary. *)
  val eval : Relse_path.Path_state.t -> t -> (Relse_path.Path_state.t * t)

  (** [end_path qtype terminated ps t] Process the end of a path and
     check all remaining insecurity queries. [terminated] indicates
     that the end of the program has been reached (e.g. not just the
     an aborted path). *)
  val end_path : ?qtype:Relse_stats.query_type -> terminated:bool -> Relse_path.Path_state.t -> t ->
    (Relse_path.Path_state.t * t)
end

(** Returns the module corresponding to an insecurity state according
   to command line options *)
val init: unit -> (module INSECURITY_STATE)
