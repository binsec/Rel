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

(** Module for relational expressions
    [Rel e e'] is the type for relational expression
    [Simple e] is the type for non-relational expressions
*)
type 'a t = Rel of 'a * 'a | Simple of 'a
type proj_t = Left | Right | Value

type rel_memory = Formula.ax_term t
type rel_pc = Formula.bl_term t
type rel_bv = Formula.bv_term t

exception Should_not_be_relational of string

(** [is_relational expr] Returns [true] if [expr] is a relational expression and
    [false] if it is a simple expression *)
val is_relational : 'a t -> bool

val mk_rel : 'a -> 'a -> 'a t
val mk_simple : 'a -> 'a t

(** [deduplicate r_expr] turns the relational expression [Rel e e']
    into a simple expression if [e = e']*)
val deduplicate : 'a t -> 'a t
val deduplicate_eq : ('a -> 'a -> bool) -> 'a t -> 'a t

(** [left r_expr]
    Returns the left value of the relational expression [r_expr]
    or its unique value if it is [Simple] *)
val left : 'a t -> 'a

(** [right r_expr]
    Returns the right value of the relational expression [r_expr]
    or its unique value if it is [Simple] *)
val right : 'a t -> 'a

(** [proj p r_expr] Returns the value of the relational expression
   [r_expr] corresponding to the projection [p], or its unique value
   if it is [Simple] *)
val proj : proj_t -> 'a t -> 'a

(** [value r_expr]
    Returns the value of the simple expression [r_expr]
    Raises [Invalid_argument] if the expression is not [Simple] *)
val value : 'a t -> 'a

(** [apply f r_expr] Applies the function [f] to the relational expression [r_expr] *)
val apply : ('a -> 'b) -> 'a t -> 'b t

(** [apply2 f r_expr1 r_expr2] Applies the relational function [f] to
    the relational expressions [r_expr1] and [r_expr2] *)
val apply2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

(** [apply3 f r_expr1 r_expr2 r_expr3] Applies the relational function [f] to
    the relational expressions [r_expr1], [r_expr2] ans [r_expr3]*)
val apply3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

val fold0 : ('a -> 'a -> 'b) -> 'a t -> 'b
val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val fold2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c

(** [apply_r_func f r_expr] Applies the relational function [f] to the relational expression [r_expr] *)
val apply_r_func : ('a -> 'b) t -> 'a t -> 'b t

(** [to_string f r_expr] Pretty print the relation expression [r_expr] according to function [f] *)
val to_string : ('a -> string) -> 'a t -> string
val pp_rexpr: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

(** [equal f r_e r_e'] True if [r_e] and [r_e'] are equal as defined
   by the function [f] *)
val equal: ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

(** [hash f r_e] Returns the hash of [r_e] as defined by the function
   [f] *)
val hash: ('a -> int) -> 'a t -> int

module RelHashtbl(H:Hashtbl.HashedType) : Hashtbl.S with type key = H.t t
module RelHashamt(H:Hashtbl.HashedType) : Hashamt.S with type key = H.t t

module RelBvTermHashtbl : Hashtbl.S with type key = Formula.bv_term t
module RelBvTermHashamt : Hashamt.S with type key = Formula.bv_term t
