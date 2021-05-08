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
  val solve : Relse_stats.query_type -> Path_state.t -> t -> Path_state.t * status
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
        Path_state.on_symbolic_state untaint_symstate ps
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
        | Relse_stats.Terminal_Insecurity -> "Terminal insecurity query is satisfiable";
        | Relse_stats.Exploration | Relse_stats.Model | Relse_stats.Enum -> failwith "Undefined violation";
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

  (** Check that the insecurity formula is not satisfiable *)
  let solve qtype ps t =
    match t.check_list with
    | [] ->
      Logger.debug ~level:2 "[Insecurity][solve] Empty insecurity list.";
      ps, Secure
    | _ ->
      let asserts =
        match t.pc with
        | None -> [mk_insec_formula t]
        | Some pc -> [pc; mk_insec_formula t]
      in
      let status = call_solver qtype asserts ps in
      let ps = untaint_checks t ps status in
      ps, status  
end


(** Modules of type [PROPERTY] correspond to the definition of a
   property. *)
module type PROPERTY = sig
  type t
  val create : unit -> t

  (** Forget all current checks *)
  val reset_formula : t -> t

  (** Add insecurity checks for the current instruction *)
  val add_checks : Path_state.t -> t -> t

  (** Declare high/low inputs  *)
  val declare_input : level:Relse_utils.level -> Dba.LValue.t -> Path_state.t -> t -> t

  (** Indicates that we've reached the end of the program *)
  val terminate : Path_state.t -> t -> Path_state.t * t
  
  (** Send insecurity checks to the solver  *)
  val solve : Relse_stats.query_type -> Path_state.t -> t -> Path_state.t * t
end

module Dummy : PROPERTY with type t = unit = struct
  type t = unit
  let create () = () 
  let reset_formula t = t
  let add_checks _ t = t
  let declare_input ~level:_ _ _ t = t
  let terminate ps t = ps, t
  let solve _ ps t = ps, t
end

(** Leak memory accesses and control-flow. *)
module CT : PROPERTY = struct

  type t = Insecurity_formula.t

  let create () = Insecurity_formula.empty
  let reset_formula _ = create ()
  let terminate ps t = ps, t
  
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
    | Print (_,_)
    | Serialize (_,_) -> t

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
    | Print (_,_)
    | Serialize (_,_) -> t

  let declare_input ~level:_ _ _ t = t
  
  let add_checks ps t =
    add_cf_checks ps t |> add_mem_checks ps

  let solve qtype ps t =
    let ps, _ = Insecurity_formula.solve qtype ps t in
    ps, (reset_formula t)
end


(** Leak modified memory addresses at the end of the target
   function. *)
module SecretErasure : PROPERTY = struct

  module Obs = Rel_expr.RelBvTermHashamt

  (* Location of the store, number of bytes stored *)
  type store_info = (Virtual_address.t * Dba.size)
  
  type t = {
    observable_addr: store_info Obs.t;
    insecurity_formula : Insecurity_formula.t;
  }

  
  let create () = {
    observable_addr = Obs.empty;
    insecurity_formula = Insecurity_formula.empty;
  }


  let reset _ = {
    observable_addr = Obs.empty;
    insecurity_formula = Insecurity_formula.empty;                       
  }

  let reset_formula t = { t with insecurity_formula = Insecurity_formula.empty }

  
  (** If ~level is high, adds dba_idx to the set of addresses to
      check. If it is low, removes dba_idx from the list of addresses
      to check (value at index dba_idx has been overwritten by public
      data). *)
  let add_store ~level dba_idx size ps t =
    let r_idx = Relse_smt.Translate.expr (Path_state.symbolic_state ps) dba_idx in
    let observable_addr =
      let vaddr = Path_state.virtual_address ps in
      (match level with
       | Relse_utils.Low ->
         if Obs.mem r_idx t.observable_addr then
           let vaddr', size' =  Obs.find r_idx t.observable_addr in
           if size' <= size
           then
             begin
               Logger.debug ~level:2 "[Insecurity][Secret-erasure] \
                                      @%a: Removing address %a (size \
                                      %d, vaddr %a) from obserable address."
                 Virtual_address.pp vaddr
                 (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_idx size
                 Virtual_address.pp vaddr';
               Obs.remove r_idx t.observable_addr
             end
           else t.observable_addr
         else t.observable_addr
       | Relse_utils.High ->
         (Logger.debug ~level:2 "[Insecurity][Secret-erasure] @%a: \
                                 Adding address %a (size %d) to \
                                 obserable address."
            Virtual_address.pp vaddr
            (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) r_idx size;
          Obs.add r_idx (vaddr, size) t.observable_addr)) in
    { t with observable_addr }


  (** Add high addresses to the list of addresses to check *)
  let declare_input ~level lvalue ps t =
    let open Dba.LValue in
    match lvalue with
    | Store (size, _, dba_idx) ->
      let t = add_store ~level dba_idx size ps t
      in Logger.debug ~level:2 "[Insecurity][Secret-erasure] @%a: \
                                Cardinal1 of Obs: %d."
        Virtual_address.pp (Path_state.virtual_address ps)
      @@ Obs.cardinal t.observable_addr; t
    | Var _
    | Restrict _ -> t

  
  (** We consider as observable any address in which secret data is
     stored *)
  let add_observable_addresses ps t =
    let open Dba in
    match Path_state.get_dba_instruction ps with
    | Instr.Assign (LValue.Store (size, _, dba_idx), dba_val, _) ->
      let r_val = Relse_smt.Translate.expr (Path_state.symbolic_state ps) dba_val in
      let level = if Rel_expr.is_relational r_val
        then Relse_utils.High
        else Relse_utils.Low in
      add_store ~level dba_idx size ps t
    | Instr.Assign (_,_,_)
    | Instr.DJump (_,_)
    | Instr.If (_,_,_)
    | Instr.SJump (_,_) 
    | Instr.Stop _
    | Instr.Assert (_,_)
    | Instr.Assume (_,_)
    | Instr.NondetAssume (_,_,_)
    | Instr.Nondet (_,_,_)
    | Instr.Undef (_,_)
    | Instr.Malloc (_,_,_)
    | Instr.Free (_,_)
    | Instr.Print (_,_)
    | Instr.Serialize (_,_) -> t

  let solve qtype ps t =
    let ps, status = Insecurity_formula.solve Relse_stats.Insecurity ps t.insecurity_formula in
    let open Relse_stats in
    if qtype =  Terminal_Insecurity && status = Insecure then
      begin
        Logger.result "[Insecurity][Secret-erasure] from one of the stores at address:";
        Obs.iter (fun _ (vaddr, _) ->
            Logger.result "[Insecurity][Secret-erasure] \t @%a" Virtual_address.pp vaddr)
          t.observable_addr;
          ps, reset ()
      end
    else ps, reset_formula t


  (** Creates the insecurity formula by leaking the content of all
      addresses in the list of observable addresses *)
  let terminate ps t =
    Logger.debug ~level:2 "[Insecurity][Secret-erasure] Terminating program at address %a"
      Virtual_address.pp @@ Path_state.virtual_address ps;
    (* Leaks the value [Load r_index] *)
    let add_addr_check r_index (_,size) insec_fml =
      let load_value = Sym_state.memory_select
          (Path_state.symbolic_state ps) size r_index in
      Logger.debug ~level:2 "[Insecurity][Secret-erasure] Leaking %a"
        (Rel_expr.pp_rexpr Formula_pp.pp_bv_term) load_value;
      Insecurity_formula.add_checks (Path_state.get_pc ps) [load_value] insec_fml
    in
    let insecurity_formula =
      Obs.fold add_addr_check t.observable_addr Insecurity_formula.empty in
    solve Relse_stats.Terminal_Insecurity ps { t with insecurity_formula }

  let add_checks ps t = add_observable_addresses ps t
end

(** Principal module that handles the insecurity state *)
module type INSECURITY_STATE = sig
  type t
  val initialize : Path_state.t -> (Path_state.t * t)
  val declare_input: ?level:Relse_utils.level -> Dba.LValue.t -> string -> Path_state.t -> t ->
    Relse_smt.Path_state.t * t
  val declare_input_addr: ?level:Relse_utils.level -> Dba.Expr.t -> string -> Path_state.t -> t ->
    Relse_smt.Path_state.t * t
  val eval : Path_state.t -> t -> (Path_state.t * t)
  val end_path : ?qtype:Relse_stats.query_type -> terminated:bool -> Path_state.t -> t ->
    (Path_state.t * t)
end
module Insecurity_State(Prop:PROPERTY) : INSECURITY_STATE = struct

  type t = Prop.t

  (** [declare_input ?level lval name ps] Declares a variable [name]
     with security level [level] and assigns the lvalue [lval := name]
     in [ps] *)
  let declare_input ?(level=Relse_utils.High) lval name ps t =
    if level = Relse_utils.High || LowDecl.get () then
      let msg = Format.sprintf "Setting %s variable %s" (Relse_utils.level_to_string level) name  in
      Logger.debug ~level:5 "[Initialisation] %s" msg;
      let ps = Path_state.maybe_add_comment ps msg in
      (* Declare fresh symbolic variable *)
      let sort = Formula.bv_sort (Natural.to_int Basic_types.Constants.bytesize) in
      let ps = Path_state.on_symbolic_state (Sym_state.declare level name sort) ps in
      (* Store it in the memory *)
      let rval = Dba.Expr.temporary ~size:(Natural.to_int Basic_types.Constants.bytesize) name in
      Relse_smt.Translate.assignment lval rval ps, Prop.declare_input ~level lval ps t
    else ps, t

  
  (** [declare_sec_addr ?level addr name ps] Declares a variable
     [name] with secuirty level [level] and stores it a address [addr]
     in ps: [@[addr] := name] *)
  let declare_input_addr ?(level=Relse_utils.High) addr name ps t =
    let lval = Dba.LValue.store (Size.Byte.create 1)
        (Kernel_options.Machine.endianness ()) addr in
    declare_input ~level lval name ps t

  
  (** Initializes addresses of high input in memory according to the
      option [Relse_options.HighBytes.get ()] *)
  let initialize_highs_in_stack (ps, t) =
    let init_high offset (ps,t) =
      (* @[esp-{offset}] := <h_l|h_r> *)
      let bv_offset = Relse_utils.int_to_bitvector (offset + 4) in
      let name = Format.sprintf "@%s-%d"
          (Kernel_options.Machine.stack_register ()) offset in
      let stack_register = Relse_utils.get_stack_register () in
      let addr = Dba.Expr.(sub stack_register (constant bv_offset)) in
      declare_input_addr addr name ps t
    in
    Basic_types.Int.Set.fold init_high (Relse_options.HighBytes.get ()) (ps,t)


  (** Initializes addresses of high input in memory according to the
     option [Relse_options.HighSymbols.get ()] *)
  let initialize_high_symbols (ps, t) =
    let init_high_symbol symbol_name (ps,t) =
      let img = (Kernel_functions.get_img ()) in
      let symbol = match Loader_utils.symbol_by_name ~name:symbol_name img with
        | Some symbol -> symbol
        | None -> failwith ("No symbol named " ^ symbol_name) in
      let size = Loader_utils.size_of_symbol symbol in
      let base_vaddr = Virtual_address.create (Loader_utils.address_of_symbol symbol) in
      Logger.debug ~level:3 "[Initialisation][high_symbol] %@%s[%d] at addr %a"
        symbol_name size Virtual_address.pp base_vaddr;
      let rec init_symbol_offset offset (ps,t) =
        if offset = size then ps,t
        else
          let name = Format.asprintf "h_%s_%d" symbol_name offset in
          let addr = offset + Virtual_address.to_int base_vaddr
                     |> Relse_utils.dba_constant_from_int in
          let ps,t = declare_input_addr addr name ps t in
          init_symbol_offset (offset + 1) (ps,t)
      in
      init_symbol_offset 0 (ps,t)
    in
    Basic_types.String.Set.fold init_high_symbol (Relse_options.HighSymbols.get ()) (ps,t)
      


  (** Initializes the insecurity state *)
  let initialize ps =
    let state = ps, Prop.create () in
    initialize_highs_in_stack state 
    |> initialize_high_symbols
  

  let do_check ?(qtype=Relse_stats.Insecurity) ps t =
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
          ps, (Prop.reset_formula t)
        end
      else Prop.solve qtype ps t
    | _ -> Prop.solve qtype ps t


  let end_path ?(qtype=Relse_stats.Insecurity) ~terminated ps t =
    let ps, t = if terminated then Prop.terminate ps t else ps, t in
    do_check ~qtype ps t


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
     | Print (_,_)
     | Serialize (_,_) -> false


  let check ps t =
    match Relse_options.FaultPacking.get ()  with 
    | Instr ->                      (* Instruction level check *)
      let qtype = (if control_flow_instr ps
                   then Relse_stats.Control_Insecurity
                   else Relse_stats.Memory_Insecurity) in
      do_check ~qtype ps t
    | Block ->                      (* Control-Flow level check *)
      let qtype = Relse_stats.Insecurity in
      if control_flow_instr ps  
      then do_check ~qtype ps t
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
    | Relse_options.SecretErasure ->
      (module Insecurity_State(SecretErasure) : INSECURITY_STATE)
