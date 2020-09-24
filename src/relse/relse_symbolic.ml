(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

open Relse_options

type value_t = Rel_expr.rel_bv
type index_t = Rel_expr.rel_bv
type constraint_t = Rel_expr.rel_pc

let word_size () = Relse_utils.word_size_bits () |> Size.Bit.to_int
let byte_size = Basic_types.Constants.bytesize |> Natural.to_int

module F = struct

  (** Name of a simple variable in the (original/renamed) program *)
  let full name index = name ^ "_" ^ (string_of_int index)
  let initial name = name ^ "_i"
  let left name = name ^ "_l"
  let right name = name ^ "_r"

  let rel_name high name =
    if high
    then Rel_expr.mk_rel (left name) (right name)
    else Rel_expr.mk_simple name

  let mk_initial_name high name =
    rel_name high (initial name)
      
  (** [memory] manipulation *)
  let memory = "__memory"
  let memory_type () = Formula.ax_sort (word_size ()) byte_size
  let r_mem idx =
    let r_name = rel_name true memory in
    Rel_expr.apply (fun name -> full name idx) r_name
  let s_mem idx = Rel_expr.mk_simple (full memory idx)

  (** [pc] manipulation *)
  let pc = "__pc"
  let full_pc = full pc

  (** [formula] manipulation *)      
  let var name =
    let open Formula in
    function
    | BlSort       -> BlVar (bl_var name)
    | BvSort i     -> BvVar (bv_var name i)
    | AxSort (i,j) -> AxVar (ax_var name i j) 

  let decl =
    let open Formula in
    function
    | BlVar v -> mk_bl_decl v []
    | BvVar v -> mk_bv_decl v []
    | AxVar v -> mk_ax_decl v []

  let def value var =
    let open Formula in
    match value.term_desc, var with
    | BlTerm value, BlVar v -> mk_bl_def v [] value
    | BvTerm value, BvVar v -> mk_bv_def v [] value
    | AxTerm value, AxVar v -> mk_ax_def v [] value
    | _ -> failwith "F.def has incompatible types"

  (* let term =
   *   let open Formula in
   *   function
   *   | BlVar v -> mk_bl_term (mk_bl_var v)
   *   | BvVar v -> mk_bv_term (mk_bv_var v)
   *   | AxVar v -> mk_ax_term (mk_ax_var v) *)

  let memory_term mem_name =
    Formula.(mk_ax_var (ax_var mem_name (word_size ()) byte_size))

  let mk_bv var_type name =
    match var_type with
    | Formula.BvSort i ->
      Formula.(mk_bv_var (bv_var name i))
    | _ -> failwith "[F.mk_bv_term] only for bitvectors"

  let normalize r_name r_value var_type =
    let r_iv = Rel_expr.apply (mk_bv var_type) r_name in
    let r_result = Relse_utils.normalize_rel r_value r_iv in
    let pp_value = Rel_expr.to_string Formula_pp.print_bv_term r_value in
    let pp_iv = Rel_expr.to_string Formula_pp.print_bv_term r_iv in
    let pp_result = Rel_expr.to_string Formula_pp.print_bv_term r_result in
    Logger.debug ~level:8 "[Symbolic state][Normalize] Expr=%s -- \
                           Iv=%s --> %s\n" pp_value pp_iv pp_result;
    r_result
end

(** Manipultaion of the formula *)
let fml_add_entry fml entry =
  Formula.push_front entry fml

let fml_assign var_type name value fml =
  let open Formula in
  let var = F.var name var_type in
  let definition = F.def value var in
  fml_add_entry fml (mk_define definition)

module Memory :
sig
  type t
  val create : store_type:store_type -> t
  val init_mem_at : addr:Bitvector.t -> size:int -> t -> unit
  val initialisations : t -> int Bitvector.Collection.Htbl.t
  val store : size:int -> index_t -> value_t -> Formula.formula -> t -> (Formula.formula * t)
  val select : t -> size:int -> index_t -> value_t
  val add_declaration : t -> Formula.formula -> Formula.formula
end = struct
  open Formula
  open Formula_utils

  module BH = Bitvector.Collection.Htbl

  type select_t = Found of Formula.bv_term | NotFound | Abort of Formula.ax_term

  (* Address and intervals *)

  type address = {
    base : bv_term;
    delta : Bigint.t;
  }

  let default base = { base; delta = Bigint.zero_big_int }

  let get_address bv =
    match bv.bv_term_desc with
    | BvCst bv ->
      let base = mk_bv_zeros (Bitvector.size_of bv) in
      let delta = Bitvector.value_of bv in
      { base; delta }
    | BvBnop (b,bv1,bv2) ->
      (match b with
       | BvAdd ->
         (match is_bv_cst bv1, is_bv_cst bv2 with
          | Some bv1, Some bv2 ->
            default (mk_bv_cst (Bitvector.add bv1 bv2))
          | Some bv1, None ->
            { base = bv2; delta = Bitvector.value_of bv1 }
          | None, Some bv2 ->
            { base = bv1; delta = Bitvector.value_of bv2 }
          | None, None -> default bv)
       | BvSub ->
         (match is_bv_cst bv1, is_bv_cst bv2 with
          | Some bv1, Some bv2 ->
            default (mk_bv_cst (Bitvector.sub bv1 bv2))
          | Some bv1, None ->
            { base = mk_bv_neg bv2; delta = Bitvector.value_of bv1 }
          | None, Some bv2 ->
            { base = bv1; delta = Bigint.minus_big_int (Bitvector.value_of bv2)}
          | None, None -> default bv)
       | _ -> default bv)
    | BvFun (_,_)
    | BvLet (_,_)
    | BvUnop (_,_)
    | BvIte (_,_,_)
    | Select (_,_,_) -> default bv

  let no_result results = Array.for_all ((=) None) results
  let r_no_result results =
    match results with
    | Rel_expr.Simple r -> no_result r
    | Rel_expr.Rel (r1,r2) -> no_result r1 && no_result r2

  let get_result results =
    let bv = ref (results.(0)) in
    Array.iteri
      (fun i opt -> if i > 0 then
          match !bv with
          | None -> ()
          | Some bv1 ->
            match opt with
            | None -> bv := None
            | Some bv2 -> bv := Some (mk_bv_concat bv2 bv1))
      results; !bv

  (* Get the result from an array of relational bv [results].
     Return:
     - None if [results] contains at least one None,
     - Some Simple bv if [results] contains no shadow expression,
     - Some Rel (bv,bv') if [results] contains at least one shadow expression,
*)
  (* let r_get_result results =
   *   let print i bv =
   *     match bv with
   *     | None -> Printf.printf "results.(%d) = None\n" i
   *     | Some r_bv ->
   *       Printf.printf "results.(%d) = %s\n" i (Rel_expr.to_string Formula_pp.print_bv_term r_bv);
   *   (\* Printf.printf "%s" (Rel_expr.to_string Formula_pp.print_bv_term !bv); *\)
   *   in
   *   Printf.printf "--- GET RESULTS ---\n";
   *   let bv = ref results.(0) in
   *   print 0 !bv;
   *   Array.iteri 
   *     (fun i opt -> if i > 0 then
   *         (print i opt;
   *         match !bv with
   *         | None -> ()
   *         | Some r_bv1 ->
   *           match opt with
   *           | None -> bv := None
   *           | Some r_bv2 -> bv := Some (Rel_expr.apply2 mk_bv_concat r_bv2 r_bv1)))
   *     results;
   *     Printf.printf "--- END ---\n"; !bv *)

  let r_get_result results =
    let bv = ref results.(0) in
    Array.iteri 
      (fun i opt -> if i > 0 then
          match !bv with
          | None -> ()
          | Some r_bv1 ->
            match opt with
            | None -> bv := None
            | Some r_bv2 -> bv := Some (Rel_expr.apply2 mk_bv_concat r_bv2 r_bv1))
      results; !bv

  
  let mk_relational_result results =
    if Rel_expr.is_relational !results
    then ()
    else
      let r = Rel_expr.value !results in
      results := Rel_expr.(mk_rel r (Array.copy r))

  (* update [result] with [bv] if the location at [address] and size [m] matches
     [address'] and size [n]*)
  let rel_update_results results address m address' n r_bv =
    let size = (Rel_expr.left r_bv).bv_term_size / n in
    let open Bigint in
    if eq_big_int address.delta address'.delta && m = n && no_result (Rel_expr.left !results)
    then Some r_bv (* perfect match *)
    else begin
      let delta_m = pred_big_int (add_int_big_int m address.delta) in
      let delta_n = pred_big_int (add_int_big_int n address'.delta) in
      if lt_big_int delta_m address'.delta ||
         lt_big_int delta_n address.delta
      then None (* no interval intersection *)
      else
        begin
          if Rel_expr.is_relational r_bv then mk_relational_result results;
          let max_big = min_big_int delta_m delta_n in
          let min_big = max_big_int address.delta address'.delta in
          let base_m  = sub_big_int min_big address.delta |> int_of_big_int in
          let base_n  = sub_big_int min_big address'.delta |> int_of_big_int in
          let loop = (sub_big_int max_big min_big |> int_of_big_int) in
          let update i results bv = 
            if results.(i + base_m) = None then
              let lo = (base_n + i) * size in
              let hi = (base_n + i + 1) * size - 1 in
              let bv = mk_bv_extract Interval.{lo; hi} bv in
              results.(i + base_m) <- Some bv;
              Logger.debug ~level:9 "[Symbolic memory][update_results] \
                                     results[%d] <- %s"
                (i + base_m) (Formula_pp.print_bv_term bv)
          in
          for i = 0 to loop do
            let _ = Rel_expr.apply2 (update i) !results r_bv in ()
          done;
          Rel_expr.(match apply get_result !results with
              | Rel (Some bv, Some bv') -> Some (mk_rel bv bv')
              | Rel (None, None) -> None
              | Simple (Some bv) -> Some (mk_simple bv)
              | Simple None -> None
              | _ -> failwith "Invalid case")
        end
    end

  (* let get_interval t bv =
   *   match is_bv_cst bv with
   *   | Some bv -> Interval.BitVecFlat.equal bv
   *   | None ->
   *     try BvTermHashtbl.find t bv
   *     with Not_found -> Interval.BitVecFlat.top (bv_size bv) *)

  (* let set_interval t bv itv =
   *   get_interval t bv
   *   |> Interval.BitVecFlat.inter itv
   *   |> BvTermHashtbl.replace t bv *)


  (* Environments *)

  module type ENV =
  sig
    type t
    val create : store_type:store_type -> t
    val select : t -> int -> index_t -> select_t Rel_expr.t
    val store : int -> index_t -> value_t -> Formula.formula -> t -> Formula.formula * t
    val add_declaration : t -> init_mem:Formula.term -> Formula.formula -> Formula.formula
  end

  module StandardEnv : ENV =
  struct
    type t = {
      mem_index: int;
      duplicated: bool;
      store_type: store_type;
    }

    let create ~store_type =
      { mem_index=0; duplicated=(store_type == SelfComposed); store_type }

    let get_mem_name t = (if t.duplicated
                          then F.r_mem t.mem_index
                          else F.s_mem t.mem_index)
    let get_mem_term t = Rel_expr.apply F.memory_term (get_mem_name t)

    let select t _ _  =
      let abort name = Abort (F.memory_term name)
      in Rel_expr.apply abort (get_mem_name t)
    
    let store size r_index r_val fml t =
      let old_mem = get_mem_term t in
      let mem_index = t.mem_index + 1 in
      let duplicated = t.duplicated ||
                       Rel_expr.is_relational r_index ||
                       Rel_expr.is_relational r_val in
      if (duplicated && t.store_type == Sse) then
        failwith "Cannot duplicate memory in SSE";
      let t = { t with mem_index; duplicated; } in
      let new_mem = get_mem_name t in
      let proj_store p fml =
        let open Rel_expr in
        let old_mem = proj p old_mem in
        let new_mem = proj p new_mem in
        let index = proj p r_index in
        let value = proj p r_val in 
        let value = mk_ax_term (mk_store size old_mem index value) in
        fml_assign (F.memory_type ()) new_mem value fml
      in
      if t.duplicated
      then fml |> proj_store Rel_expr.Left |> proj_store Rel_expr.Right, t
      else proj_store Rel_expr.Value fml, t

    let add_declaration t ~init_mem fml =
      let mem_name =
        if t.store_type == SelfComposed then F.r_mem 0 else F.s_mem 0
      in
      let add_mem_def name fml =
        let mem_def = F.var name (F.memory_type ()) |> F.def init_mem in
        Formula.push_back_define mem_def fml
      in Rel_expr.fold add_mem_def mem_name fml
  end

  let initial_memory = F.(full memory 0)

  module ListEnv : ENV =
  struct
    type node =
      | Simple of (int * Formula.bv_term * Formula.bv_term)
      (* Simple index and simple value: mem[x] = <e> *)
      | RelValue of (int * Formula.bv_term * value_t)
      (* Simple index and relational value: mem[x] = <e|e'> *)
      | RelIndex of (int * index_t * value_t)
      (* Relational index and relational value: mem[x]|r = e and mem[x']|l = e'*)

    type t = {
      list: node list;
      mem_index: int;
      duplicated: bool;
      store_type: store_type;
    }

    let create ~store_type =
      { list=[]; mem_index=0; duplicated=(store_type == SelfComposed); store_type }

    let depth = ref max_int

    let get_mem_name t = (if t.duplicated
                          then F.r_mem t.mem_index
                          else F.s_mem t.mem_index)
    let get_mem_term t = Rel_expr.apply F.memory_term (get_mem_name t)
    
    let abort t = Rel_expr.apply (fun name -> Abort name) (get_mem_term t)

    let mk_node size index value =
      match index, value with
      | Rel_expr.Simple i, Rel_expr.Simple v -> Simple (size, i, v)
      | Rel_expr.Simple i, _ -> RelValue (size, i, value)
      | _, _ -> RelIndex (size, index, value)

    let simple_select t address size results fuel =
      let rec simple_select list address size results fuel =
        let get_results size' index' bv' list =
          let address' = get_address index' in
          if equal_bv_term address.base address'.base then
            match rel_update_results results address size address' size' bv' with
            | None -> simple_select list address size results (fuel-1)
            | Some r -> Rel_expr.apply (fun x -> Found x) r
          else
            begin
              Logger.debug ~level:9 "[Symbolic memory][memory_select] \
                                     Aborted: %s not comparable with \
                                     %s\n"
                (Formula_pp.print_bv_term address.base)
                (Formula_pp.print_bv_term address'.base);
              abort t
            end
        in
        match list with
        | [] when r_no_result !results -> Rel_expr.mk_simple NotFound
        | _ when fuel <= 0 -> abort t
        (* We give up before the end of the list. TODO -> keep track of
           the memory on which we give up to search from it *)
        | Simple (size', index', bv') :: xs ->
          get_results size' index' (Rel_expr.mk_simple bv') xs
        | RelValue (size', index', r_bv') :: xs ->
          get_results size' index' r_bv' xs
        | _ -> failwith "Relational indexes in simple select -> TODO!"
      in simple_select t.list address size results fuel
    (* match status1, status2 with
     * | Equal, Equal -> Rel_expr.apply (fun x -> Found x) v
     * | Distinct, Distinct -> find_index mem' i
     * | _ ->
     *   let v1 = continue status1 Rel_expr.left
     *   and v2 = continue status2 Rel_expr.right
     *   in Rel_expr.mk_rel v1 v2 *)    

    let projected_select t address size results fuel p = 
      let abort = Abort (Rel_expr.proj p (get_mem_term t)) in 
      let rec projected_select list address size results fuel =
        let get_results size' index' bv' list =
          let address' = get_address index' in
          let bv' = Rel_expr.mk_simple bv' in
          if equal_bv_term address.base address'.base then
            match rel_update_results results address size address' size' bv' with
            | None -> projected_select list address size results (fuel-1)
            | Some bv -> Found (Rel_expr.value bv)
          else
            begin
              Logger.debug ~level:9 "[Symbolic memory][memory_select] \
                                     Aborted: %s not comparable with \
                                     %s\n"
                (Formula_pp.print_bv_term address.base)
                (Formula_pp.print_bv_term address'.base);
              abort
            end
        in
        match list with
        | [] -> NotFound
        | _ when fuel <= 0 -> abort
        (* We give up before the end of the list. TODO -> keep track of
           the memory on which we give up to search from it *)
        | Simple (size', index', bv') :: xs ->
          get_results size' index' bv' xs
        | RelValue (size', index', r_bv') :: xs ->
          get_results size' index' (Rel_expr.proj p r_bv') xs
        | RelIndex (size', r_index', r_bv') :: xs ->
          get_results size' (Rel_expr.proj p r_index') (Rel_expr.proj p r_bv') xs
      in projected_select t.list address size results fuel

    let select t size index =
      match index with
      | Rel_expr.Simple index ->
        let results = ref (Rel_expr.mk_simple (Array.init size (fun  _ -> None))) in
        let address = get_address index in
        simple_select t address size results !depth
      | Rel_expr.Rel _ ->
        if t.store_type == Sse then
          failwith "Cannot select relational index in SSE";
        let result_l = ref (Rel_expr.mk_simple (Array.init size (fun  _ -> None))) in
        let result_r = ref (Rel_expr.mk_simple (Array.init size (fun  _ -> None))) in
        let r_result = Rel_expr.mk_rel result_l result_r in
        let r_address = Rel_expr.apply get_address index in
        let projected_select p =
          let address = Rel_expr.proj p r_address in
          let result = Rel_expr.proj p r_result in
          projected_select t address size result !depth p
        in
        Rel_expr.(mk_rel (projected_select Left) (projected_select Right))

    let store size r_index r_val fml t =
      (* Update the list *)
      let list = mk_node size r_index r_val :: t.list in
      (* Update the formula *)
      let old_mem = get_mem_term t in
      let mem_index = t.mem_index + 1 in
      let duplicated = t.duplicated ||
                       Rel_expr.is_relational r_index ||
                       Rel_expr.is_relational r_val in
      if (duplicated && t.store_type == Sse) then
        failwith "Cannot duplicate memory in SSE";
      let store_type = t.store_type in
      let t = { list; mem_index; duplicated; store_type; } in
      let new_mem = get_mem_name t in
      let proj_store p fml =
        let open Rel_expr in
        let old_mem = proj p old_mem in
        let new_mem = proj p new_mem in
        let index = proj p r_index in
        let value = proj p r_val in 
        let value = mk_ax_term (mk_store size old_mem index value) in
        fml_assign (F.memory_type ()) new_mem value fml
      in
      if t.duplicated
      then fml |> proj_store Rel_expr.Left |> proj_store Rel_expr.Right, t
      else proj_store Rel_expr.Value fml, t

    let add_declaration _ ~init_mem fml =
      let mem_name = initial_memory in
      let mem_def = F.var mem_name (F.memory_type ()) |> F.def init_mem
      in Formula.push_back_define mem_def fml
  end

  module MapEnv : ENV =
  struct
    
    module M = Basic_types.BigInt.Map
    
    type base_type =
      | None_b
      | First_b of Formula.bv_term Rel_expr.t
      | Some_b of Formula.bv_term Rel_expr.t

    type t = {
      map: value_t M.t;
      current_base: base_type;       (* The base of the last accessed index *)
      mem_index: int;                (* Current index of the memory in the formula *)
      duplicated: bool;              (* Is the memory duplicated in the formula ? *)
      store_type: store_type;        (* Type of the store: sse | self-composed | shadow *)
    }

    let create ~store_type =
      { map=M.empty;
        current_base=None_b;
        mem_index=0;
        duplicated=(store_type == SelfComposed);
        store_type }

    (* let depth = ref max_int *)

    let get_mem_name t = (if t.duplicated
                          then F.r_mem t.mem_index
                          else F.s_mem t.mem_index)
    let get_mem_term t = Rel_expr.apply F.memory_term (get_mem_name t)
    
    let abort t = Rel_expr.apply (fun name -> Abort name) (get_mem_term t)

    let base_equals addr base = equal_bv_term addr.base base
    let r_base r_addr = Rel_expr.apply (fun addr -> addr.base) r_addr
    let r_delta r_addr = Rel_expr.apply (fun addr -> addr.delta) r_addr
    
    let lookup t size r_addr not_found =
      let r_offset = r_delta r_addr |> Rel_expr.deduplicate in
      if Rel_expr.is_relational r_offset then
        failwith "Relational indexes not implemented in lookup of map environment";
      let offset = Rel_expr.value r_offset in
      (* Logger.debug ~level:9 "[memory_select] Aborted: %s not comparable with %s"; *)
      let results = (Array.init size (fun i -> M.find_opt (Bigint.add_int_big_int i offset) t.map)) in
      match r_get_result results with
      | Some r_bv -> Rel_expr.apply (fun x -> Found x) r_bv
      | None ->
        if no_result results then not_found else (abort t)
    
    let select t size index =
      let debug_abort base  current_base =
        Logger.debug ~level:9 "[Symbolic memory][memory_select] Aborted: %s not comparable with %s"
          (Rel_expr.to_string (fun x -> Formula_pp.print_bv_term x.base) base)
          (Rel_expr.to_string Formula_pp.print_bv_term current_base)
      in
      let address = Rel_expr.apply get_address index in
      begin match t.current_base with
        | First_b current_base ->
          if Rel_expr.equal base_equals address current_base then
            (* When the offset is not found, select from initial memory *)
            let not_found = Rel_expr.mk_simple NotFound in
            lookup t size address not_found
          else
            (debug_abort address current_base; abort t)
        | Some_b current_base ->
          if Rel_expr.equal base_equals address current_base then
            (* When the offset is not found, abort *)
            let not_found = abort t in
            lookup t size address not_found
          else
            (debug_abort address current_base; abort t)
        | None_b ->
          (* Select form the initial memory *)
          Rel_expr.mk_simple NotFound
      end

    let store_update_fml size r_index r_val fml t = (* TODO merge parts with the ListEnv *)
      (* Update the formula *)
      let old_mem = get_mem_term t in
      let mem_index = t.mem_index + 1 in
      let duplicated = t.duplicated ||
                       Rel_expr.is_relational r_index ||
                       Rel_expr.is_relational r_val in
      if (duplicated && t.store_type == Sse) then
        failwith "Cannot duplicate memory in SSE";
      let store_type = t.store_type in
      let t = { t with mem_index; duplicated; store_type; } in
      let new_mem = get_mem_name t in
      let proj_store p fml =
        let open Rel_expr in
        let old_mem = proj p old_mem in
        let new_mem = proj p new_mem in
        let index = proj p r_index in
        let value = proj p r_val in 
        let value = mk_ax_term (mk_store size old_mem index value) in
        fml_assign (F.memory_type ()) new_mem value fml
      in
      if t.duplicated
      then fml |> proj_store Rel_expr.Left |> proj_store Rel_expr.Right, t
      else proj_store Rel_expr.Value fml, t

    let rec update map r_addr n bv =
      let r_offset = r_delta r_addr |> Rel_expr.deduplicate in
      if Rel_expr.is_relational r_offset then
        failwith "Relational indexes not implemented in update of map environment";
      let offset = Rel_expr.value r_offset in
      if n = 0 then map
      else
        let lo = (n-1) * byte_size in
        let hi = n * byte_size - 1 in
        let bv' = Rel_expr.apply (mk_bv_extract Interval.{lo; hi}) bv in
        let map' = M.add (Bigint.add_int_big_int (n-1) offset) bv' map in
        update map' r_addr (n-1) bv
    
    let store size r_index r_val fml t =
      let fml, t = store_update_fml size r_index r_val fml t in          
      let r_address = Rel_expr.apply get_address r_index in
      let map, current_base =
        match t.current_base with
        | First_b base | Some_b base as current_base ->
          if Rel_expr.equal base_equals r_address base
            then
              (* Put the index in the current table *)
              let map = update t.map r_address size r_val in
              map, current_base
            else
              (* Create a new table *)
              let current_base = Some_b (r_base r_address) in
              let map = update M.empty r_address size r_val in
              map, current_base
          | None_b ->
            (* Create a new table with this base *)
            let current_base = First_b (r_base r_address) in
            let map = update M.empty r_address size r_val in
            map, current_base
        in fml, { t with current_base; map; }

    let add_declaration _ ~init_mem fml =
      let mem_name = initial_memory in
      let mem_def = F.var mem_name (F.memory_type ()) |> F.def init_mem
      in Formula.push_back_define mem_def fml
  end

  type pack =
    | Std of StandardEnv.t
    | List  of ListEnv.t
    | Map of MapEnv.t

  type t = {
    pack : pack;
    store_type : store_type;

    (* Table of of initial memory locations to read, shared between memories *)
    initialisation : int BH.t;  (* bitvector * size *)
  }

  let init_mem_at ~addr ~size t =
    match BH.find_opt t.initialisation addr with
    | Some size' when size <= size' ->
      Logger.debug ~level:3 "[Symbolic memory][init_mem_at] Already in memory %s<%d>"
        (Bitvector.to_hexstring addr) size;
    | _ ->
      Logger.debug ~level:3 "[Symbolic memory][init_mem_at] Add %s<%d>"
        (Bitvector.to_hexstring addr) size;
      BH.replace t.initialisation addr size

  let initialisations t = t.initialisation

  let create ~store_type =
    let pack =
      match MemoryType.get () with
      | MemStd -> Std (StandardEnv.create ~store_type)
      | _ when store_type = SelfComposed -> failwith "SelfComp Memory with row not implemented yet"
      | MemList -> List (ListEnv.create ~store_type)
      | MemMap -> Map (MapEnv.create ~store_type) in
    let initialisation = BH.create 100 in
    { pack; store_type; initialisation; }

  let store ~size r_index r_value fml t =
    let fml, pack = match t.pack with
      | Std env ->
        let fml, env = (StandardEnv.store size r_index r_value fml env)
        in fml, Std env
      | List env -> 
        let fml, env = (ListEnv.store size r_index r_value fml env)
        in fml, List env 
      | Map env -> 
        let fml, env = (MapEnv.store size r_index r_value fml env)
        in fml, Map env 
    in fml, { t with pack }

  let select t ~size r_index =
    let mk_select index_term mem_term =
      Formula.mk_select size mem_term index_term
    in     
    let process_value value index_term =
      match value with
      | Found value ->
        (* If found, return the value *)
        Logger.debug ~level:9 "[memory_select] Found %s\n" (Formula_pp.print_bv_term value);
        value
      | NotFound ->             (* Select from initial memory *)
        begin
          match Formula_utils.is_bv_cst index_term with
          (* If not found, and the address is concrete, select from
             initial memory *)
          | Some bv when Relse_utils.is_loadable bv ->
            let value = Relse_utils.read_bitvector bv size |>
                        Formula.mk_bv_cst in
            Logger.debug ~level:9 "[Symbolic memory][memory_select] Found in initial memory: %a\n"
              Formula_pp.pp_bv_term value;
            init_mem_at ~addr:bv ~size:size t;
            value
          | _ ->
            (* Else, make a select from initial memory *)
            Logger.debug ~level:9 "[Symbolic memory][memory_select] NotFound\n";
            mk_select index_term (F.memory_term initial_memory)
        end
      | Abort last_mem ->
        begin
          match Formula_utils.is_bv_cst index_term with
          (* There might be a big chance that the select is from the initial memory *)
          (* If the select is concrete, we can add this address to the
             initial address to load *)
          | Some bv when Relse_utils.is_loadable bv ->
            Logger.debug ~level:9 "[Symbolic memory][memory_select] \
                                   Aborted / added %a to init_mem_at\n"
              Bitvector.pp bv;
            init_mem_at ~addr:bv ~size:size t;
            mk_select index_term last_mem 
          | _ ->
            (* Else, make select from last memory *)        
            Logger.debug ~level:9 "[Symbolic memory][memory_select] Aborted\n";
            mk_select index_term last_mem
        end
    in
    let r_val =
      match t.pack with
      | Std env -> StandardEnv.select env size r_index
      | List env -> ListEnv.select env size r_index
      | Map env -> MapEnv.select env size r_index in
    Rel_expr.apply2 process_value r_val r_index |> Rel_expr.deduplicate (*TODO Remove ?*)

  let add_declaration t fml =
    (* Initialize the symbolic memory *)
    let mem_var = F.(var memory (F.memory_type ())) in
    let mem_declaration = F.decl mem_var in
    let symbolic_memory = mk_ax_var (ax_var F.memory (word_size ()) byte_size) in
    let load_at addr size mem =
      assert (word_size () = Bitvector.size_of addr);
      let bv = mk_bv_cst (Relse_utils.read_bitvector addr size) in
      mk_store size mem (mk_bv_cst addr) bv
    in
    let init_mem =
      mk_ax_term (BH.fold load_at t.initialisation symbolic_memory) in
    let fml = match t.pack with
      | Std env -> StandardEnv.add_declaration env ~init_mem fml
      | List env -> ListEnv.add_declaration env ~init_mem fml
      | Map env -> MapEnv.add_declaration env ~init_mem fml
    in Formula.push_back_declare mem_declaration fml
end

module VarStore :
sig
  type t
  val create : store_type:store_type -> t
  val declare : high:bool -> string -> Formula.sort -> Formula.formula -> t -> (Formula.formula * t)
  val load : t -> string -> Size.Bit.t -> value_t
  val assign : string -> Size.Bit.t -> value_t -> Formula.formula -> t -> (Formula.formula * t)
  val add_declarations : t -> Formula.formula -> Formula.formula
end = struct  
  module Env = struct
    
    type var_infos = {
      value: value_t;
      index: int; (* Index of intermediate variable in the symbolic formula *)
      var_type: Formula.sort;
    }

    module M = Basic_types.String.Map

    type t = {
      variables: var_infos M.t;
      
      mutable definitions : (bool * Formula.sort) M.t;
      (** Variables that are loaded before they are defined.
          (high * var_type)
          (Shared by states). *)
      
      canonical: bool;
      store_type: store_type;
      
    }

    let create ~canonical ~store_type =
      let variables = M.empty in
      let definitions = M.empty in
      { variables; definitions ; canonical; store_type; }

    let get_infos t name = M.find_opt name t.variables

    let next_index t name var_type =
      match get_infos t name with
      | Some infos when infos.var_type <> var_type ->
        failwith "Store.get_last_index with wrong type"
      | Some infos -> infos.index + 1
      | None -> 0

    let duplicate t = t.store_type == SelfComposed
    let duplicate high t = duplicate t || high

    let fml_declare store_type high var_type r_name r_value fml =
      (* Add the variable to the formula *)
      let fml_declare name fml =
        let decl = F.decl (F.var name var_type)  in
        fml_add_entry fml (Formula.mk_declare decl)
      in
      match store_type with
      | Sse when high ->
        failwith "Cannot declare high variable in Sse"
      | Sse ->
        fml_declare (Rel_expr.value r_name) fml
      | SelfComposed ->
        let fml = fml_declare (Rel_expr.left r_name) fml in
        if high then
          fml_declare (Rel_expr.right r_name) fml
        else
          let term_l = Formula.mk_bv_term (Rel_expr.left r_value) in
          fml_assign var_type (Rel_expr.right r_name) term_l fml
      | Shadow -> Rel_expr.fold fml_declare r_name fml

    let declare high name var_type fml t =
      let initial_index = -1 in
      let duplicate = duplicate high t in
      let r_initial_name = F.rel_name duplicate name in
      let r_initial_value = Rel_expr.apply (F.mk_bv var_type) r_initial_name in
      (* Add the variables to the store *)
      let store_declare value =
        let var_info = { value; index=initial_index; var_type } in
        { t with variables=(M.add name var_info t.variables) }
      in
      let t = store_declare r_initial_value in
      (* Add the variable to the formula *)
      let fml_declare name fml =
        let decl = F.decl (F.var name var_type)  in
        fml_add_entry fml (Formula.mk_declare decl)
      in
      let fml = 
        match t.store_type with
        | Sse when high ->
          failwith "Cannot declare high variable in Sse"
        | Sse ->
          fml_declare (Rel_expr.value r_initial_name) fml
        | SelfComposed ->
          let fml = fml_declare (Rel_expr.left r_initial_name) fml in
          if high then
            fml_declare (Rel_expr.right r_initial_name) fml
          else
            let term_l = Formula.mk_bv_term (Rel_expr.left r_initial_value) in
            fml_assign var_type (Rel_expr.right r_initial_name) term_l fml
        | Shadow -> Rel_expr.fold fml_declare r_initial_name fml
      in fml,t

    let declare_in_place ?(high=false) name var_type t = (* TODO merge with declare *)
      Logger.debug ~level:3 "[Symbolic state][declare_in_place] Store: adding %s" name;
      let duplicate = duplicate high t in
      (* Add the variable to definitions *)
      t.definitions <- M.add name (high, var_type) (t.definitions);
      (* Compute the value of the variable *)      
      (F.rel_name duplicate (F.initial name)) |>
      Rel_expr.apply (F.mk_bv var_type)

    let add_declarations t fml =
      (* Add the variable declarations to the formula *)
      let add_decl name (high,var_type) fml =
        let duplicate = duplicate high t in
        let r_name = F.mk_initial_name duplicate name in
        let r_value = Rel_expr.apply (F.mk_bv var_type) r_name in
        fml_declare t.store_type high var_type r_name r_value fml      
      in
      Formula.append
        fml
        (M.fold add_decl t.definitions Formula.empty)

    let get_last_value t name var_type =
      match get_infos t name with
      | Some infos when infos.var_type <> var_type ->
        failwith "Store.get_last_value with wrong type"
      | Some infos -> Some infos.value
      | None -> None

    let load t name size =
      let var_type = Formula.bv_sort (Size.Bit.to_int size) in
      match get_last_value t name var_type with
      | Some rel_val -> rel_val
      | None -> declare_in_place name var_type t


    let assign name size r_val fml t = (* Simple name, relational value *)
      let var_type = Formula.bv_sort (Size.Bit.to_int size) in
      let index = next_index t name var_type in
      let duplicate = duplicate (Rel_expr.is_relational r_val) t in
      let r_name = F.rel_name duplicate name |>
                   Rel_expr.apply (fun name -> F.full name index) in
      
      (* Add the variable to the formula *)
      let r_term = Rel_expr.apply Formula.mk_bv_term r_val in
      let fml = Rel_expr.fold2 (fml_assign var_type) r_name r_term fml in
      
      (* Add to the variable map *)
      let r_value =
        if t.canonical then
          F.normalize r_name r_val var_type
        else
          Rel_expr.apply (F.mk_bv var_type) r_name
      in
      let infos = { value=r_value; index; var_type; } in
      let variables =  M.add name infos t.variables
      in fml, { t with variables }
  end

  type t = Env.t

  let create ~store_type =
    let canonical = Canonical.get () in
    (* match store_type with
     * | SelfComposed when canonical ->
     *   failwith "SelfComposed var-store with canonical form not implemented yet"
     * | Shadow when (not canonical) ->
     *   failwith "Shadow var-store VarStore without canonical form not implemented yet"
     * | _ -> (); *)
    Env.create ~canonical ~store_type
  
  let declare ~high name var_type fml t =
    Env.declare high name var_type fml t
  
  let load t name size =
    Env.load t name size
  
  let assign name size r_val fml t =
   Env.assign name size r_val fml t
   
  let add_declarations t fml =
    Env.add_declarations t fml  
end

module PC :
sig
  type t
  val create : store_type:store_type -> t
  val get : t -> Formula.bl_term
  val update : constraint_t -> Formula.formula -> t -> (Formula.formula * t)
  val add_initial : Formula.formula -> Formula.formula       
end = struct

  let fml_assign pc_index term fml =
    fml_assign Formula.bl_sort (F.full_pc pc_index) term fml
  
  type t = {
    index : int;
    store_type : store_type;
  }

  let create ~store_type =
    let index = 0 in
    let store_type = store_type
    in { index; store_type }

  (* PC *)
  let get pc =
    Formula.(mk_bl_var (bl_var (F.full_pc pc.index)))

  let update rel_cond fml pc =
    let open Formula in
    let cond = Rel_expr.(match rel_cond with
      | Simple c -> c
      | Rel _ when pc.store_type = Sse ->
        failwith "Cannot update Sse store with relational value"
      | Rel (c,c') -> mk_bl_and c c')
    in
    let pc_term = mk_bl_and (get pc) cond in
    let next_index = pc.index + 1 in
    let pc = { pc with index = next_index } in
    fml_assign next_index (mk_bl_term pc_term) fml, pc
         
  let add_initial fml =
    fml_assign 0 Formula.(mk_bl_term mk_bl_true) fml
end

(*
  Containts the list of untainted variables.
  An untainted variable is represented as the right side of the
  relational variable. When an occurence of the right variable is
  detected, it is replaced by its mapping (the left variable).
*)
module Untainting =
struct
  open Rel_expr
  open Formula

  module M = Formula.VarMap
  
  type t = Formula.var M.t

  let empty = M.empty

  let add v v' u =
    Logger.debug ~level:1 "[Symbolic state][untaint] Adding: %s |-> %s"
      (Formula_utils.var_name v) (Formula_utils.var_name v');
    M.add v v' u

  (* WARNING: check before use *)
  (* let untaint_bl u r_bl =
   *   match deduplicate r_bl with
   *   | Rel (bl1, bl2) ->
   *     let pp_bl = Rel_expr.to_string Formula_pp.print_bl_term r_bl in
   *     Logger.debug ~level:1 "[Symbolic state][untaint] %s" pp_bl;
   *     let u = match bl1.bl_term_desc, bl2.bl_term_desc with
   * 
   *       (\* (\\* BvComp *\\)
   *        * | BvComp (BvEqual,bv1,bv2), BvComp (BvEqual,bv1',bv2')
   *        *   when equal_bv_term bv2 bv2' ->
   *        *   (match Formula_utils.is_bv_var bv1, Formula_utils.is_bv_var bv1' with
   *        *    | Some v, Some v' -> add (BvVar v') (BvVar v) u (\\* We can deduce v  = v' *\\)
   *        *    | _ -> u
   *        *   )
   *        * | BvComp (BvEqual,bv1,bv2), BvComp (BvEqual,bv1',bv2')
   *        *   when equal_bv_term bv1 bv1'->
   *        *   (match Formula_utils.is_bv_var bv2, Formula_utils.is_bv_var bv2' with
   *        *    | Some v, Some v' -> add (BvVar v') (BvVar v) u (\\* We can deduce v  = v' *\\)
   *        *    | _ -> u
   *        *   )
   *        * | BvComp _, BvComp _ -> u *\)
   *       (\* Others *\)
   *         (\* | BlFun  of bl_var * term list
   *          * | BlLet  of def list * bl_term
   *          * | BlUnop of bl_unop * bl_term
   *          * | BlBnop of bl_bnop * bl_term * bl_term
   *          * | BlComp of bl_comp * bl_term * bl_term
   *          * | BvComp of bv_comp * bv_term * bv_term
   *          * | AxComp of ax_comp * ax_term * ax_term
   *          * | BlIte  of bl_term * bl_term * bl_term         *\)
   *       | _ -> failwith "[untaint_bl] Not implemented yet"
   *     in bl1, u
   *   | Simple bl -> bl, u *)

  let rec untaint_bv u r_bv =
    match deduplicate r_bv with
    | Rel (bv, bv') ->
      let pp_bv = Rel_expr.to_string Formula_pp.print_bv_term r_bv in
      Logger.debug ~level:1 "[Symbolic state][untaint] %s" pp_bv;
      (match bv.bv_term_desc, bv'.bv_term_desc with
       (* We found a variable *)
       | BvFun (v,[]), BvFun (v',[]) ->
         add (BvVar v') (BvVar v) u (* We can deduce v  = v' *)

       (* BvUnop *)
       | BvUnop (BvNot,bv), BvUnop (BvNot,bv')
       | BvUnop (BvNeg,bv), BvUnop (BvNeg,bv') ->
         untaint_bv u (Rel_expr.mk_rel bv bv')

       (* BvBnop *)

       (* BvSub *)
       | BvBnop (BvSub,bv1,bv2), BvBnop (BvSub,bv1',bv2')
         when equal_bv_term bv2 bv2' ->
         untaint_bv u (Rel_expr.mk_rel bv1 bv1')
       | BvBnop (BvSub,bv1,bv2), BvBnop (BvSub,bv1',bv2')
         when equal_bv_term bv1 bv1' ->
         untaint_bv u (Rel_expr.mk_rel bv2 bv2')

       (* BvAdd *)
       | BvBnop (BvAdd,bv1,bv2), BvBnop (BvAdd,bv1',bv2')
         when equal_bv_term bv2 bv2' ->
         untaint_bv u (Rel_expr.mk_rel bv1 bv1')
       | BvBnop (BvAdd,bv1,bv2), BvBnop (BvAdd,bv1',bv2')
         when equal_bv_term bv1 bv1' ->
         untaint_bv u (Rel_expr.mk_rel bv2 bv2')

       (* BvConcat *)
       | BvBnop (BvConcat,bv1,bv2), BvBnop (BvConcat,bv1',bv2')
         when bv1.bv_term_size = bv1'.bv_term_size
           && bv2.bv_term_size = bv2'.bv_term_size ->
         untaint_bv (untaint_bv u (Rel_expr.mk_rel bv1 bv1'))
           (Rel_expr.mk_rel bv2 bv2')
       | BvBnop (BvAdd,bv1,bv2), BvBnop (BvAdd,bv1',bv2')
         when equal_bv_term bv1 bv1' ->
         untaint_bv u (Rel_expr.mk_rel bv2 bv2')

       (* Can't say anything more *)
       | BvBnop (BvAdd,_,_), BvBnop (BvAdd,_,_)
       | BvBnop (BvOr,_,_), BvBnop (BvOr,_,_)
       | BvBnop (BvAnd,_,_), BvBnop (BvAnd,_,_)
       | BvBnop (BvCmp,_,_), BvBnop (BvCmp,_,_)
       | BvBnop (BvMul,_,_), BvBnop (BvMul,_,_)
       (* TODO: Multiplication rule only works when there is no
           overflow -> we need to add interval informations here *)
       | BvBnop (BvShl,_,_), BvBnop (BvShl,_,_)
       | BvBnop (BvLshr,_,_), BvBnop (BvLshr,_,_)
       | BvIte (_,_,_), BvIte (_,_,_)
       | Select _, Select _ -> u (* TODO: no untainting of memory *)
       (* | BvCst  of Bitvector.t
        * | BvFun  of bv_var * term list
        * | BvLet  of def list * bv_term
        * | BvUnop of bv_unop * bv_term
        * | BvBnop of bv_bnop * bv_term * bv_term
        * | BvIte  of bl_term * bv_term * bv_term *)

       (* | _ -> failwith "[untaint_bv] Not implemented yet") *)
       | _ -> Format.printf "[untaint_bv] TODO: I a lazy tonight"; u)
    | Simple _ -> u
  
  let subst_bv u bv =
    let subst v bv =
      match M.find_opt v u with
      | Some v' ->
        Logger.debug ~level:1 "[Symbolic state][untaint] Substituting: %s |-> %s"
          (Formula_utils.var_name v) (Formula_utils.var_name v');
        let var_name = Formula_utils.var_name v in
        let r s = if s = var_name then Formula_utils.var_name v' else var_name in
        Formula_transformation.rename_bv_term r bv
      | None -> bv
    in
    let varset = Formula_utils. bv_term_variables bv in
    Formula.VarSet.fold subst varset bv

  let subst_bl u bl =
    let subst v bl =
      match M.find_opt v u with
      | Some v' ->
        Logger.debug ~level:1 "[Symbolic state][untaint] Substituting: %s |-> %s"
          (Formula_utils.var_name v) (Formula_utils.var_name v');
        let var_name = Formula_utils.var_name v in
        let r s = if s = var_name then Formula_utils.var_name v' else var_name in
        Formula_transformation.rename_bl_term r bl
      | None -> bl
    in
    let varset = Formula_utils.bl_term_variables bl in
    Formula.VarSet.fold subst varset bl

  (* let subst_ax u ax =
   *   let subst v ax =
   *     match M.find_opt v u with
   *     | Some v' ->
   *       Logger.debug ~level:1 "[Symbolic state][untaint] Substituting: %s |-> %s"
   *         (Formula_utils.var_name v) (Formula_utils.var_name v');
   *       let var_name = Formula_utils.var_name v in
   *       let r s = if s = var_name then Formula_utils.var_name v' else var_name in
   *       Formula_transformation.rename_ax_term r ax
   *     | None -> ax
   *   in
   *   let varset = Formula_utils.ax_term_variables ax in
   *   Formula.VarSet.fold subst varset ax *)

  
  let subst_rbv u r_bv =
    match r_bv with
      (* Substitute in the right side of the relational expression *)
    | Rel (bv_l,bv_r) -> Rel (bv_l,subst_bv u bv_r) |> deduplicate
    | _ -> r_bv

  (* let subst_rax u r_ax =
   *   match r_ax with
   *   (\* Substitute in the right side of the relational expression *\)
   *   | Rel (ax_l,ax_r) -> Rel (ax_l,subst_ax u ax_r) |> deduplicate
   *   | _ -> r_ax *)

end

module State =
struct

  type t = {
    (* The current formula *)
    formula : Formula.formula;

    (* Symbolic state *)
    var_store : VarStore.t;
    memory: Memory.t;
    pc: PC.t;

    (* Hypothesis to add to the formula *)
    hypothesis : Formula.bl_term list;

    (* List of variables to untaint *)
    untainter : Untainting.t;
  }

  let initialisations t = Memory.initialisations t.memory

  let init_mem_at ~addr ~size st =
    Memory.init_mem_at ~addr ~size st.memory

  let create () =
    let option_string =
      begin match SymbolicStore.get () with
        | Sse -> "SSE"
        | SelfComposed -> "SelfComposed"
        | Shadow -> "Shadow"
      end
      ^ " - " ^
      begin if Canonical.get ()
        then "Canonical Form" else "Intermediate Variables"
      end
      ^ " - " ^
      begin match MemoryType.get () with
        | MemStd -> "Standard Memory"
        | MemList-> "ROW with List"
        | MemMap -> "ROW with Map" end
    in
    Logger.debug ~level:1 "[State.create] %s " option_string;
    let store_type = SymbolicStore.get () in
    let formula = Formula.empty |> PC.add_initial in
    let var_store = VarStore.create ~store_type in
    let memory = Memory.create ~store_type in
    let pc = PC.create ~store_type in
    let hypothesis = [] in
    let untainter = Untainting.empty in
    { formula; var_store; memory; pc; hypothesis; untainter; }

  
  (* { Formula } *)
  
  let comment cmt t =
    let comment = Formula.mk_comment cmt in
    { t with formula = Formula.push_front comment t.formula }

  let formula t =
    let add_hypothesis formula term =
      Formula.push_front_assert term formula in
    let fml = t.formula
              |> VarStore.add_declarations t.var_store
              |> Memory.add_declaration t.memory
              |> Formula.push_front_comment "Making Hypothesis" in
    List.fold_left add_hypothesis fml t.hypothesis
  
  let add_assertion hypothesis t =
    Logger.debug ~level:1 "[Symbolic state][Assert] Adding hypothesis \
                           %a" Formula_pp.pp_bl_term hypothesis;
    let formula = fml_add_entry t.formula (Formula.mk_assert hypothesis)
    in { t with formula }

  
  (* { Variables } *)
       
  let declare high name var_type t =
    let formula, var_store =
      VarStore.declare ~high name var_type t.formula t.var_store
    in { t with formula; var_store }
    
  let declare_high name sort t =
    Logger.debug ~level:1 "[Symbolic state][declare] %s as High" name;
    declare true name sort t

  let declare_low name sort t =
    Logger.debug ~level:1 "[Symbolic state][declare] %s as Low" name;
    declare false name sort t

  let var_load t name size =
    let r_val = VarStore.load t.var_store name size
                |> Untainting.subst_rbv t.untainter in
    Logger.debug ~level:1 "[Symbolic state][var_load] %s (%a bits) -> %a"
      name Size.Bit.pp size (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_val;
    r_val 
      
  let var_assign t name size r_val =
    Logger.debug ~level:1 "[Symbolic state][var_assign] %s (%a bits) := %a"
      name Size.Bit.pp size (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_val;
    let formula, var_store =
      VarStore.assign name size r_val t.formula t.var_store
    in { t with formula; var_store }

  
  (* { Path Constraint } *)

  let path_constraint t =
    let pc = PC.get t.pc |> Untainting.subst_bl t.untainter in
    Logger.debug ~level:1 "[Symbolic state][path_constraint] result: %a"
      Formula_pp.pp_bl_term pc; pc
  
  let pc_update t r_cond =
    Logger.debug ~level:1 "[Symbolic state][pc_update] pc := pc /\\ %a"
     (Rel_expr.pp_rexpr Formula_pp.pp_bl_term) r_cond;
        (* if Relse_options.Untainting.get () && Rel_expr.is_relational r_value then
         *   raise (Rel_expr.Should_not_be_relational ("in update_pc: " ^ pp_value)); *)
    let formula, pc = PC.update r_cond t.formula t.pc
    in { t with formula; pc; }


  (* { Memory } *)

  let memory_select t size r_index =
    (* if Relse_options.Untainting.get () && Rel_expr.is_relational rel_index then
     *   raise (Rel_expr.Should_not_be_relational ("in memory_select: " ^ pp_index)); *)
    let r_value = Memory.select t.memory ~size r_index
                  |> Untainting.subst_rbv t.untainter in
    Logger.debug ~level:1 "[Symbolic state][memory_select] mem@%a * %d -> %a"
      (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_index size
      (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_value;
    r_value

  let memory_store t size r_index r_value =
    Logger.debug ~level:1 "[Symbolic state][memory_store] mem@%a * %d := %a"
      (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_index size
      (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_value;
    (* if Relse_options.Untainting.get () && Rel_expr.is_relational r_index then
     *   raise (Rel_expr.Should_not_be_relational ("in update_mem: " ^ pp_index)); *)
    let formula, memory =
      Memory.store ~size r_index r_value t.formula t.memory
    in { t with formula; memory; }

  
  (* { Untainting } *)
  
  (* let untaint_bl r_bl st =
   *   if Relse_options.Untainting.get () then
   *     (\* Untainting option is set and the variable is relational *\) 
   *     let bl, untainter = Untainting.untaint_bl st.untainter r_bl in
   *     Rel_expr.mk_simple bl, { st with untainter }
   *   else r_bl, st             (\* Otherwise, do nothing *\) *)

  let untaint_bv r_bv st =
    if Relse_options.Untainting.get () then
      (* Untainting option is set and the variable is relational *) 
      { st with untainter = Untainting.untaint_bv st.untainter r_bv}
    else st (* Otherwise, do nothing *)
end
