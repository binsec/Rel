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
type 'a t = Rel of 'a * 'a | Simple of 'a
type proj_t = Left | Right | Value

type rel_memory = Formula.ax_term t
type rel_pc = Formula.bl_term t
type rel_bv = Formula.bv_term t

exception Should_not_be_relational of string

let is_relational expr =
  match expr with
  | Rel _ -> true
  | _ -> false

let mk_simple expr = Simple expr
let mk_rel expr expr' = Rel (expr, expr')

let deduplicate r_expr =
  match r_expr with
  | Rel (expr, expr') ->
    if expr <> expr' then Rel (expr, expr')
    else mk_simple expr
  | r_expr -> r_expr

let deduplicate_eq equals r_expr =
  match r_expr with
  | Rel (expr, expr') ->
    if equals expr expr'
    then mk_simple expr
    else r_expr
  | _ -> r_expr

let left r_expr =
  match r_expr with
  | Rel (expr, _) -> expr
  | Simple expr -> expr

let right r_expr =
  match r_expr with
  | Rel (_, expr) -> expr
  | Simple expr -> expr

let value r_expr =
  match r_expr with
  | Rel _ ->
    raise (Should_not_be_relational "Rel_expr.value expects a simple expression.")
  | Simple expr -> expr

let proj p v =
  match p with
  | Left -> left v
  | Right -> right v
  | Value -> value v

let apply f rexpr =
  match rexpr with
  | Rel (expr,expr') ->
    let x = f expr
    and x' = f expr'
    in mk_rel x x'
  | Simple expr ->
    let x = f expr
    in mk_simple x

let apply_r_func f rexpr =
  match f with
  | Rel (f,f') ->
    let x = f (left rexpr)
    and x' = f' (right rexpr)
    in mk_rel x x'
  | Simple f -> apply f rexpr

let apply2 f rexpr1 rexpr2 =
  apply_r_func (apply f rexpr1) rexpr2

let apply3 f rexpr1 rexpr2 rexpr3 =
  apply_r_func (apply2 f rexpr1 rexpr2) rexpr3

let fold0 f rexpr =
  f (left rexpr) (right rexpr)

let fold f rexpr x =
  match rexpr with
  | Rel (expr,expr') ->
    f expr x |> f expr'
  | Simple expr ->
    f expr x

let fold2 f rexpr1 rexpr2 x =
  match rexpr1,rexpr2 with
  | (Simple expr1, Simple expr2) ->
    f expr1 expr2 x
  | _ ->
    f (left rexpr1) (left rexpr2) x
    |> f (right rexpr1) (right rexpr2)

let to_string f rexpr =
    match rexpr with
  | Rel (expr, expr') -> "<" ^ f expr ^ " | " ^ f expr' ^ ">"
  | Simple expr -> "<" ^ f expr ^ ">"

let pp_rexpr pp_expr ppf rexpr =
  match rexpr with
  | Rel (expr, expr') -> Format.fprintf ppf "<%a | %a>" pp_expr expr pp_expr expr'
  | Simple expr -> Format.fprintf ppf "<%a>" pp_expr expr

(* Modules *)

type 'a r_expr_t = 'a t

let equal f r_expr r_expr' =
  match r_expr, r_expr' with
  | Rel (e1, e2), Rel (e1', e2') when f e1 e1' && f e2 e2' -> true
  | Simple e, Simple e' when f e e' -> true 
  | _ -> false

let hash f r_expr =
  match r_expr with
  | Rel (e1, e2) -> Hashtbl.hash (f e1, f e2) 
  | Simple e -> f e

module RelHashtbl(H : Hashtbl.HashedType) = Hashtbl.Make
    (struct
      type t = H.t r_expr_t
      let equal bv1 bv2 = equal H.equal bv1 bv2
      let hash bv = hash H.hash bv
    end)

module RelHashamt(H : Hashtbl.HashedType) = Hashamt.Make
    (struct
      type t = H.t r_expr_t
      let equal bv1 bv2 = equal H.equal bv1 bv2
      let hash bv = hash H.hash bv
    end)

module RelBvTermHashtbl = RelHashtbl(struct
    open Formula
    type t = bv_term
    let equal bv1 bv2 = bv1 = bv2
    let hash bv = bv.bv_term_hash
  end)

module RelBvTermHashamt = RelHashamt(struct
    open Formula
    type t = bv_term
    let equal bv1 bv2 = bv1 = bv2
    let hash bv = bv.bv_term_hash
  end)
