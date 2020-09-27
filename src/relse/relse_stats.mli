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
(** The exit codes *)
type status = Insecure | Max_Depth | Max_Paths | Solver_Timeout | Timeout | EnumLimit
type query_type = Exploration | Control_Insecurity | Memory_Insecurity | Insecurity | Model | Enum

type t

val empty: t

val get_exit_code : unit -> int
val set_status : status -> unit
  
val update_status : Formula.status -> Dba_types.Statement.t -> query_type -> unit
val add_query : float -> Formula.status -> query_type -> unit
val add_query_size : int -> unit

(** Get the list of insecure addresses *)
val get_insecurity_addresses: unit -> Relse_utils.AddressList.t

(** Updates the done/spared insecurity checks *)
val add_done_check : unit -> unit
val add_spared_check : unit -> unit

val add_instruction : unit -> unit
val add_dba_instruction : unit -> unit
val add_path : unit -> unit
val add_conditional : unit -> unit
val add_fork : unit -> unit
val set_start : unit -> unit

(* val get : unit -> t *)

val pp : Format.formatter -> t -> unit

(** Print the statistics at the end of the execution*)
val print_stats : unit -> unit

(** Get current execution time *)
val get_time: unit -> float
