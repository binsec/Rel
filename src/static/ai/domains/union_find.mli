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

(** Union-find for equality domain *)

module Make : functor (Val: Ai_sigs.Domain) ->
sig
  type t
  type data
  type thresholds = (int array * int array * int array * int array)

  val create : unit -> t

  val bottom: t

  val make : Dba.LValue.t -> Val.t -> t

  val find : t -> Dba.LValue.t -> Dba.LValue.t option * Val.t option

  val union : t -> Dba.LValue.t -> Dba.LValue.t -> Val.t -> t

  val join : t -> t -> t

  val widen : t -> t -> thresholds -> t

  val to_string : t -> string

  val pp : Format.formatter -> t -> unit

  val print : t -> unit

  val remove : t -> Dba.LValue.t -> t

  val remove_syntax_overlaps : t -> Dba.LValue.t -> t

  val copy_equalities : t -> t

  val get_nb_names : t -> int

  val get_nb_classes : t -> int

  val refine: Dba.Expr.t -> Val.t -> t -> t

  val is_same_class: t -> Dba.LValue.t -> Dba.LValue.t -> bool

  val get_elements : t -> Dba.LValue.t list
end
