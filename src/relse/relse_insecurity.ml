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
open Relse_options

(** Status of an insecurity formula *)
type status = Secure | Insecure | Unknown

let dedup () = Relse_options.Dedup.get ()

module Path_state = Relse_path.Path_state
module Sym_state = Relse_symbolic.State
module Solver = Relse_smt.Solver

module Insecurity_formula :
sig
  type t
  val empty : t
  val add_checks : Formula.bl_term -> Formula.bv_term Rel_expr.t list -> t -> t
  val solve : Relse_stats.query_type -> Path_state.t -> t -> Path_state.t
end = struct
  
  type t = {
    
    check_list: Formula.bv_term Rel_expr.t list;    
    (** The list of insecurity checks *)

    pc : Formula.bl_term option;
    (** The path-constraint under which the insecurty formula must be
       satifsaible *) 
 }

  let empty =
    let check_list = []
    and pc = None
    in { check_list; pc }

  let check_to_formula rexpr = Rel_expr.fold0 Formula.mk_bv_distinct rexpr


  (** [add_pc pc t] Adds the paths contraint [pc] to the insecurity
      formula [t]. Fails if the insecurity formula already has a
      different path constraint. *)
  let add_pc pc t =
    Logger.debug ~level:10 "[Insecurity] Add pc to insecurity formula: %a"
      Formula_pp.pp_bl_term pc;
    match t.pc with
    | Some pc' when not @@ Formula.equal_bl_term pc pc' ->
      (Format.printf "[Insecurity] %a / %a"
         Formula_pp.pp_bl_term pc
         Formula_pp.pp_bl_term pc';
       failwith "[Relse][Insecurity] Cannot add path constraint to \
                 insecurity formula.")
    | _ -> { t with pc = Some pc }

  
  let add_checks pc new_checks t =
    let append_unique current_list new_check =
      (* Check if the expression is relational *)
      if not (Rel_expr.(deduplicate_eq Formula.equal_bv_term new_check |> is_relational))
      then
        begin
          (* Spared check *)
          Relse_stats.add_spared_check ();
          Logger.debug ~level:3
            "[Insecurity][spared] %a is simple" 
            (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) new_check;
          current_list
        end
      else
        (* Add the query to the list if it is not already there (backtracking
           according to dedup parameter).
           Record the number of checks spared / checks added *)
        let equal_term t t' = Rel_expr.equal Formula.equal_bv_term t t' in
        let rec duplicate l fuel =
          if fuel = 0 then false
          else match l with
            | [] -> false
            | x :: xs ->
              if equal_term x new_check then true
              else duplicate xs (fuel-1)
        in
        (* Add the formula to the list of insecurity formulas to check *)
        if not (duplicate current_list (dedup ()))
        then
          begin
            Relse_stats.add_done_check ();
            Logger.debug ~level:9
              "[Insecurity][miss] Check %a added to the list"
              (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) new_check;
            new_check :: current_list
          end
        else
          begin
            Relse_stats.add_spared_check ();
            Logger.debug ~level:9
              "[Insecurity][dedup] Hit: check %a already in the list"
              (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) new_check;
            current_list
          end
    in
    let t = add_pc pc t in
    { t with check_list = List.fold_left append_unique t.check_list new_checks } 


  (* Add the negation of the insecurity checks to ps *)
  let untaint_checks t ps =
    let untaint_symstate symstate =
        List.fold_left (fun symstate bv -> Sym_state.untaint_bv bv symstate)
          symstate t.check_list in
     function
      | Secure ->
        (* Check is secure, we do not have to add it to the formula
           but we have to untaint it in ps. *)
        Relse_path.Path_state.on_symbolic_state untaint_symstate ps
      | Insecure | Unknown as status ->
        match Relse_options.InstrLeak with
        | HaltLeak -> failwith "Should not happen"
        | InstrLeak ->
          (* Do not constraint the insecurity check for the rest of
             the execution *) ps
        | UniqueLeaks when status = Unknown -> ps
        | UniqueLeaks ->
          (* Constraint the insecurity check for the rest of the execution *)
          (* Add hypothesis [not insec] to [ps] *)
          let add_asserts symstate =
            List.fold_left
              (fun symstate check -> Sym_state.add_assertion
                  (check_to_formula check |> Formula.mk_bl_not)
                  symstate)
              symstate t.check_list
          in
          Path_state.on_symbolic_state add_asserts ps |>
          Path_state.on_symbolic_state untaint_symstate  
  
  (** Call the solver to check if the insecurity formula is satisfiable  *)
  let call_solver qtype asserts ps =
    match Solver.check_sat_with_asserts qtype asserts ps with
    | Formula.UNSAT, _ ->
      Logger.debug ~level:2 "[Insecurity][Checked] The insecurity query is unsatisfiable";
      Secure
    | Formula.SAT, ps ->
      (* Handle satisfiable insecurity queries TODO: move to relse.ml *)
      let print_insecurity model msg =
        Logger.result "[Insecurity][Violation] %s" msg;
        Logger.result "Address %a" Path_state.pp_loc ps;
        match model with
        | Some(model) -> Logger.result "@[<v 0>Time: %f| Model:@ %a@]"
                           (Relse_stats.get_time ())
                           Smt_model.pp model;
        | None -> ()
      in
      let exit_se () =
        Relse_stats.add_path ();
        Relse_stats.print_stats ();
        Path_state.pp_path ps max_int;
        exit (Relse_stats.get_exit_code ())
      in
      let model =
        if Relse_options.PrintModel.get ()
        then Some (Solver.get_model_with_asserts asserts ps)
        else None
      in 
      let msg = match qtype with
        | Relse_stats.Memory_Insecurity -> "Insecure memory access";
        | Relse_stats.Control_Insecurity -> "Insecure jump";
        | Relse_stats.Insecurity -> "Insecurity query is satisfiable";
        | _ -> failwith "Undefined violation";
      in
      print_insecurity model msg;
      if Relse_options.LeakInfo.get () == HaltLeak then exit_se ();  
      Insecure
    | _ ->
      Logger.debug ~level:2 "[Insecurity][Checked] Could not determine satisfiability of \
                             insecurity formula"; (* TODO should be logged in the stats *)
      Unknown
  

  (** Creates an insecurity formula from a relational expression to check *)
  let mk_insec_formula t =
    let append_check_to_formula f rexpr =
      Formula.mk_bl_or f (check_to_formula rexpr)
    in
    List.fold_left append_check_to_formula Formula.mk_bl_false t.check_list

  (** Check that the insecurity formula is not satisfiable*)
  let solve qtype ps t =
    match t.check_list with
    | [] -> (* Nothing to check *) ps
    | _ ->
      let asserts =
        match t.pc with
        | None -> [mk_insec_formula t]
        | Some pc -> [pc; mk_insec_formula t]
      in
      let secure = call_solver qtype asserts ps in
      untaint_checks t ps secure 
end


(** Modules of type [PROPERTY] correspond to the definition of a
   property. They implement a function [eval] which adds
   insecurity checks corresponding to the current instruction,
   according to the property. *)
module type PROPERTY = sig
  type t
  val create : unit -> t
  val add_checks : Relse_path.Path_state.t -> t -> t
  val solve : Relse_stats.query_type -> Relse_path.Path_state.t -> t -> Path_state.t * t
end

module Dummy : PROPERTY with type t = unit = struct
  type t = unit
  let create () = ()
  let add_checks _ t = t
  let solve _ ps t = ps, t
end

(** Leak memory accesses and control-flow. *)
module CT : PROPERTY = struct

  type t = Insecurity_formula.t

  let create () = Insecurity_formula.empty

  (** Add insecurity checks relative to control-flow statements *)
  let add_cf_checks ps t =
    let open Dba.Instr in
    match Path_state.get_dba_instruction ps with
    | DJump (expr,_) ->
      (* Get the relational expression to leak *)
      let r_expr = Relse_smt.Translate.expr (Path_state.symbolic_state ps) expr in
      Insecurity_formula.add_checks (Path_state.get_pc ps) [r_expr] t
    (* Add it to the insecurity query *)
    | If (condition,_,_)
      when Instruction.is_conditional_jump (Path_state.get_instruction ps) ->
      let r_expr = Relse_smt.Translate.expr (Path_state.symbolic_state ps) condition in
      Insecurity_formula.add_checks (Path_state.get_pc ps) [r_expr] t
    (* Other instructions *)
    | If (_,_,_)
    | Assign (_,_,_) 
    | SJump (_,_)
    | Stop _ 
    | Assert (_,_) 
    | Assume (_,_) 
    | NondetAssume (_,_,_) 
    | Nondet (_,_,_) 
    | Undef (_,_) 
    | Malloc (_,_,_) 
    | Free (_,_) 
    | Print (_,_) -> t

  (** Add insecurity checks relative to memory accesses in expressions *)
  let rec add_mem_checks_expr ps expr t =
  let open Dba.Expr in
  match expr with
  | Var _ | Cst _ -> t
  | Load (_, _, idx) ->
    let r_expr = Relse_smt.Translate.expr (Path_state.symbolic_state ps) idx in
    Insecurity_formula.add_checks (Path_state.get_pc ps) [r_expr] t
    |> add_mem_checks_expr ps idx
  | Binary (_, op1, op2) ->
    add_mem_checks_expr ps op1 t
    |> add_mem_checks_expr ps op2
  | Unary (_, op) ->
    add_mem_checks_expr ps op t
  | Ite (c, then_e, else_e) ->
    add_mem_checks_expr ps c t
    |> add_mem_checks_expr ps then_e
    |> add_mem_checks_expr ps else_e
  
  (** Add insecurity checks relative to memory accesses in lvalues *)
  let add_mem_checks_lval ps lval t = 
    let open Dba.LValue in
    match lval with
    | Var _ | Restrict _ -> t
    | Store (_, _, idx) ->
    let r_expr = Relse_smt.Translate.expr (Path_state.symbolic_state ps) idx in
    Insecurity_formula.add_checks (Path_state.get_pc ps) [r_expr] t

  (** Add insecurity checks relative to memory accesses *)
  let add_mem_checks ps t =
    let open Dba.Instr in
    match Path_state.get_dba_instruction ps with
    | Assign (lval,expr,_) ->
      add_mem_checks_expr ps expr t |> add_mem_checks_lval ps lval
    | DJump (expr,_) -> add_mem_checks_expr ps expr t
    | If (expr,_,_) -> add_mem_checks_expr ps expr t
    | SJump (_,_) 
    | Stop _
    | Assert (_,_)
    | Assume (_,_)
    | NondetAssume (_,_,_)
    | Nondet (_,_,_)
    | Undef (_,_)
    | Malloc (_,_,_)
    | Free (_,_)
    | Print (_,_) -> t

  let add_checks ps t =
    add_cf_checks ps t |> add_mem_checks ps

  let solve qtype ps t =
    let ps = Insecurity_formula.solve qtype ps t in
    ps, (create ())
end


(** Principal module that handles the insecurity state *)
module type INSECURITY_STATE = sig
  type t
  val initialize : Relse_path.Path_state.t -> (Relse_path.Path_state.t * t)
  val eval : Relse_path.Path_state.t -> t -> (Relse_path.Path_state.t * t)
  val force_check : ?qtype:Relse_stats.query_type -> Relse_path.Path_state.t -> t ->
    (Relse_path.Path_state.t * t)
end
module Insecurity_State(Prop:PROPERTY) : INSECURITY_STATE = struct

  type t = Prop.t

  (** Initialize symbolic execution with pairs of symbolic values for
     high variables, distinct in both executions *)
  let initialize_highs_in_stack ps =
    let init_high_x86 offset ps =
      (* @[esp-{offset}] := <s | s'> *)
      let bv_offset = Relse_utils.int_to_bitvector (offset + 4) in
      let name = "@esp-" ^ (string_of_int offset) in
      let msg = "Setting high byte: " ^ name  in
      Logger.debug ~level:5 "[initialisation] %s" msg;
      let ps = Path_state.maybe_add_comment ps msg in
      (* Declare symbolic high byte *)
      let symstate = Path_state.symbolic_state ps in
      let sort = Formula.bv_sort (Natural.to_int Basic_types.Constants.bytesize) in
      let symstate = Sym_state.declare_high name sort symstate in
      let ps = Path_state.set_symbolic_state symstate ps in
      (* Store it in the memory *)
      let tag = Dba.VarTag.register in
      let size = Size.Bit.to_int (Relse_utils.word_size_bits ()) in
      let esp_reg = Dba.Expr.var ~tag "esp" size in
      let addr = Dba.Expr.(sub esp_reg (constant bv_offset)) in
      let lval = Dba.LValue.store (Size.Byte.create 1) (Kernel_options.Machine.endianness ()) addr in
      let rval = Dba.Expr.temporary ~size:(Natural.to_int Basic_types.Constants.bytesize) name in
      Relse_smt.Translate.assignment lval rval ps
    in
    let default _ _ =
      failwith "Init high_in_stack not implemented for this architecture"
    in
    let f = match Kernel_options.Machine.get () with
      | Machine.X86 _ -> init_high_x86
      | _ -> default
    in
    Basic_types.Int.Set.fold f (Relse_options.HighBytes.get ()) ps


  let initialize_high_symbols ps =
    let init_high symbol_name ps =
      let img = (Kernel_functions.get_img ()) in
      let symbol = match Loader_utils.symbol_by_name ~name:symbol_name img with
        | Some symbol -> symbol
        | None -> failwith ("No symbol named " ^ symbol_name) in
      let size = Loader_utils.size_of_symbol symbol in
      let base_addr = Loader_utils.address_of_symbol symbol in
      let base_vaddr = Virtual_address.of_int64 (Int64.of_int base_addr) in
      Logger.debug ~level:3 "[Initialisation][high_symbol] %@%s[%d] at addr %a"
        symbol_name size Virtual_address.pp base_vaddr;
      (* TODO mutualize this function with delare_symbolic_size in relse_stubs.ml *)
      let rec loop offset ps =
        if offset = size then ps
        else
          let var_name = Format.asprintf "h_%s_%d" symbol_name offset in
          Logger.debug ~level:5 "[Initialisation][high_byte] %@%s[%d] := %s"
            symbol_name offset var_name;
          (* Declare symbolic high byte *)
          let sort = Formula.bv_sort (Natural.to_int Basic_types.Constants.bytesize) in
          let ps = Path_state.on_symbolic_state (Sym_state.declare_high var_name sort) ps in
          (* Store it in the memory *)
          let addr = offset + Virtual_address.to_int base_vaddr
                     |> Relse_utils.dba_constant_from_int in
          Logger.debug ~level:5 "[Initialisation] %@[%a] := %s"
            Dba_printer.Ascii.pp_bl_term addr var_name;
          let lval = Dba.LValue.store (Size.Byte.create 1) (Kernel_options.Machine.endianness ()) addr in
          let size = (Natural.to_int Basic_types.Constants.bytesize) in
          let rval = Dba.Expr.temporary ~size var_name in
          let ps = Relse_smt.Translate.assignment lval rval ps
          in loop (offset + 1) ps
      in
      loop 0 ps
    in
    Basic_types.String.Set.fold init_high (Relse_options.HighSymbols.get ()) ps

  let initialize ps =
    let ps = initialize_highs_in_stack ps
             |> initialize_high_symbols in
    ps, Prop.create ()
  
  let force_check ?(qtype=Relse_stats.Insecurity) ps t =
    Logger.debug ~level:3 "[Insecurity][check] Checking insecurity at address %a"
      Virtual_address.pp (Path_state.virtual_address ps);
    match Relse_options.LeakInfo.get () with
    | InstrLeak ->
      (* If an insecurity query has already been found at this address, then abort *)
      let insecure_addresses = Relse_stats.get_insecurity_addresses () in
      let current_stm = Path_state.get_current_statement ps in
      if Relse_utils.AddressList.find current_stm insecure_addresses then        
        begin
          Relse_stats.add_spared_check ();
          Logger.debug ~level:8 "[Insecurity][spared] %a already in the list"
            Virtual_address.pp
            Dba_types.(Caddress.to_virtual_address (Statement.location current_stm));
          (* Drop current insecurity checks *)
          ps, (Prop.create ())
        end
      else Prop.solve qtype ps t
    | _ -> Prop.solve qtype ps t

  let control_flow_instr ps = 
     let open Dba.Instr in
     match Path_state.get_dba_instruction ps with
     | DJump (_,_) | If (_,_,_) -> true
     (* In other cases, the instruction does not change the
        control-flow *)
     | Assign (_,_,_)
     | SJump (_,_)
     | Stop _
     | Assert (_,_)
     | Assume (_,_)
     | NondetAssume (_,_,_)
     | Nondet (_,_,_)
     | Undef (_,_)
     | Malloc (_,_,_)
     | Free (_,_)
     | Print (_,_) -> false

  
  let check ps t =
    match Relse_options.FaultPacking.get ()  with 
    | Instr ->                      (* Instruction level check *)
      let qtype = (if control_flow_instr ps
                   then Relse_stats.Control_Insecurity
                   else Relse_stats.Memory_Insecurity) in
      force_check ~qtype ps t
    | Block ->                      (* Control-Flow level check *)
      let qtype = Relse_stats.Insecurity in
      if control_flow_instr ps  
      then force_check ~qtype ps t
      else ps,t
    | Never -> ps, t

  let eval ps t =
    (* Add insecurity checks *)
    Prop.add_checks ps t |> check ps
end

let init () =
  if (Relse_options.SymbolicStore.get()) = Sse ||
     (Relse_options.FaultPacking.get ()) = Never
  then (module Insecurity_State(Dummy) : INSECURITY_STATE)
  else match Relse_options.Property.get () with
    | Relse_options.CT ->
      (module Insecurity_State(CT) : INSECURITY_STATE)
