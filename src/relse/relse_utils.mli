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
type assignment_t =
  | Var of string * Size.Bit.t * Rel_expr.rel_bv (* name, value *)
  | Mem of Dba.size * Rel_expr.rel_bv * Rel_expr.rel_bv (* size, index, value *)

val word_size_bits : unit -> Size.Bit.t      (* 32 bits *)
val word_size_bytes : unit -> Size.Byte.t      (* 32 bits *)

val is_sse : unit -> bool

val get_main_symbol: unit -> Loader.Symbol.t

val int_to_bitvector : int -> Bitvector.t

val dba_constant_from_int: int -> Dba.Expr.t

(** [is_loadable addr] Returns true if [addr] it is in a read-only
   section or a section specified in cmdline *)
val is_loadable :  Bitvector.t -> bool

(** [read_bitvector bv size] Reads [size] bytes in the image of the
   executable at address [bv] *)
val read_bitvector : Bitvector.t -> int -> Bitvector.t

(** [temp_file ()] create a new temporary file *)
val temp_file : unit -> string

(** [mk_var_name basename idx] *)
val mk_var_name : string -> int -> string

type status =
  | TRUE
  | FALSE
  | UNKNOWN

val formula_status : status -> Formula.status

val solving_attempt : Formula.bl_term -> status


type comparison_type = Equal | Distinct | NotComparable
val compare_bv : Formula.bv_term -> Formula.bv_term -> comparison_type

val normalize_simple : Formula.bv_term -> Formula.bv_term -> Formula.bv_term
val normalize_rel : Formula.bv_term Rel_expr.t-> Formula.bv_term Rel_expr.t -> Formula.bv_term Rel_expr.t


(** Keep track of a list of addresses *)
module AddressList : sig
  type t
  val create : name:string -> t
  val extend : Dba_types.Statement.t -> t -> t
  val pp_address_trace_to_file : t -> int -> unit

  (** [find addr al] Returns true if [addr] is in the address list [addr] *)
  val find: Dba_types.Statement.t -> t -> bool
end
