open Relse_options

module Path_state = Relse_path.Path_state
module S = Bitvector.Collection.Set
module Sym_state = Relse_symbolic.State

module Solver = struct

  type result =
    | CheckSat of Formula.status * S.t
    | GetModel of Smt_model.t
    | Enum of Bitvector.t list * S.t
  
  let do_optimisation ?(keep=Formula.VarSet.empty) fm =
    let cst = Formula_options.(OptimAll.get () || OptimCst.get ()) in
    let itv = Formula_options.(OptimAll.get () || OptimItv.get ()) in
    let prn = Formula_options.(OptimAll.get () || OptimPrn.get ()) in
    let rbs = Formula_options.(OptimAll.get () || OptimRbs.get ()) in
    let row = Formula_options.(OptimAll.get () || OptimRow.get ()) in
    let ssa = Formula_options.(OptimAll.get () || OptimSsa.get ()) in
    let lst =
      let i = Formula_options.OptimLst.get () in
      if i = 0 then None else Some i
    in
    Formula_transformation.optimize ~keep ?lst ~cst ~itv ~prn ~rbs ~row ~ssa fm

  let mk_comment qtype =
    let open Relse_stats in
    match qtype with
    | Exploration -> "Exploration query"
    | Control_Insecurity -> "Insecurity (CF) query"
    | Memory_Insecurity -> "Insecurity (MEM) query"
    | Insecurity -> "Insecurity query"
    | Model -> "Model query"
    | Relse_stats.Enum -> "Enumeration query"
  
  let prepare_solver_in_state ?keep ps assertions solver qtype =
    let symbolic_state = Path_state.symbolic_state ps in
    let append en = Solver.Session.put_entry solver en in
    let add_assertions formula term = Formula.push_front_assert term formula in
    let final_formula =
      let fml = Formula.push_front_comment "Assertions to check" (Sym_state.formula symbolic_state) in
      List.fold_left add_assertions fml assertions
      |> do_optimisation ?keep
      |> Formula.push_front_comment (mk_comment qtype)
    in
    Relse_stats.add_query_size (Formula.formula_size final_formula);
    Formula.iter_forward append final_formula

  let log_result time result qtype =
    let r = match result with
      | CheckSat (result, _) -> result;
      | GetModel _ -> Formula.SAT
      | Enum _ -> Formula.SAT in
    let log = match r with
      | Formula.SAT | Formula.UNSAT -> Logger.debug ~level:4
      | _ -> Logger.warning ~level:0
    in log "[Solver] SMT query resulted in %a" Formula_pp.pp_status r;
    Relse_stats.add_query time r qtype
    
  let with_solver ?keep qtype ps formulas f =
    let timeout = Formula_options.Solver.Timeout.get () in
    let file = 
      if Sse_options.SMT_dir.is_set() then
        let filename = Sse_utils.temp_file () in
        let vaddr = Path_state.virtual_address ps in
        Logger.debug ~level:3 "@[<h>Using SMT script file %s %@ %a@]"
          filename Virtual_address.pp vaddr ;
        Some filename
      else None in
    let solver = Formula_options.Solver.get () in
    let session = Solver.Session.create ?file ~timeout solver in
    Logger.debug ~level:5 "Running %s %@ %a"
      (Prover.name_of solver) Path_state.pp_loc ps;
    let prepare_solver = prepare_solver_in_state ?keep ps formulas in
    try
      prepare_solver session qtype;
      let start_time = Unix.gettimeofday () in
      let res = f session in
      Solver.Session.destroy session;
      log_result (Unix.gettimeofday() -. start_time) res qtype;
      Some res
    with
    | Failure msg ->
      Logger.warning "SMT solver failed on %s" msg;
      Solver.Session.destroy session;
      if not (Sse_options.KeepGoing.get ())
      then begin
        Logger.error
          "@[<v 0>\
           @[SMT solver failed in %a@ with message:@].@ \
           @[%s@]@ \
           @[Aborting. Use -keep-going to ignore@]@]"
          (Print_utils.pp_opt Format.pp_print_string) file msg;
        failwith msg
      end;
      None
    | e ->
      Solver.Session.destroy session;
      raise e

  let no_address = S.empty

  (** Load the addresses of specified sections *)
  let get_addresses_to_load session path_state =
    if not (Sse_options.LoadSections.is_set () || Sse_options.LoadROSections.get())
    then no_address
    else
      let model = Solver.Session.get_model session in
      let addresses = Smt_model.memory_addresses model in
      let keep_addr (addr:Bitvector.t) =
        not (Path_state.address_belongs_to_init ~addr path_state) &&
        Relse_utils.is_loadable addr 
      in
      List.fold_left
        (fun set bv -> if keep_addr bv then S.add bv set else set)
        S.empty addresses

  let maybe_load_and_recurse f result path_state to_load =
    if S.is_empty to_load then result, path_state
    else begin
      Logger.debug ~level:1 "[load_section] %a:@ loading addresses @ %a"
        Path_state.pp_loc path_state
        (fun ppf ->
           S.iter (fun x -> Format.fprintf ppf "%a@ " Bitvector.pp_hex x))
        to_load;
      let path_state =
        S.fold
          (fun addr ps -> Path_state.with_init_mem_at ps ~addr ~size:1)
          to_load path_state
      in
      f path_state
    end

  let check_sat_with_asserts qtype formulas path_state  =
    let rec check_sat_with_asserts qtype formulas path_state =
      let do_check session =
        let result = Solver.Session.check_sat session in
        let to_load =
          match result with
          | Formula.SAT -> get_addresses_to_load session path_state
          | _ -> no_address in
        CheckSat (result, to_load)
      in
      let result, to_load =
        match with_solver qtype path_state formulas do_check with
        | Some (CheckSat (result, to_load)) -> result, to_load
        | _ -> Formula.UNKNOWN, no_address
      in
      maybe_load_and_recurse
        (check_sat_with_asserts qtype formulas)
        result path_state to_load
    in
    let result, ps = check_sat_with_asserts qtype formulas path_state in
    Relse_stats.update_status result (Path_state.get_current_statement ps) qtype;
    result, ps
  
  let check_sat path_state =
    let pc = Sym_state.path_constraint
        (Path_state.symbolic_state path_state) in
    check_sat_with_asserts Relse_stats.Exploration [pc] path_state

  let get_model_with_asserts formula path_state = (* TODO: remove, UGLY *)
    let do_check session =
      let result = Solver.Session.check_sat session in
      let model =
        match result with
        |  Formula.SAT -> Solver.Session.get_model session;
        | _ -> failwith "We ask the solver a model when formula is unsat"
      in GetModel model
    in
    match with_solver Relse_stats.Model path_state formula do_check with
      | Some (GetModel m) -> m
      | _ -> failwith "We ask the solver a model when formula is unsat"

  let get_model path_state =
    let pc = Sym_state.path_constraint
        (Path_state.symbolic_state path_state) in
    get_model_with_asserts [pc] path_state

  let rec enumerate_values n expr path_state =
    let rec loop acc to_load n solver =
      match n with
      | 0 -> Enum (acc, to_load)
      | _ ->
        begin
          begin match acc with
            | [] -> ()
            | x::_ ->
              Formula.(mk_bv_distinct (mk_bv_cst x) expr)
              |> Formula.mk_assert
              |> Solver.Session.put_entry solver
          end;
          match Solver.Session.check_sat solver with
          | Formula.SAT ->
            let bv = Solver.Session.get_value solver (Formula.mk_bv_term expr) in
            Logger.debug ~level:5 "Solver returned %a ; \
                                   %d solutions still to be found"
              Bitvector.pp_hex bv
              (n-1);
            let to_load' = get_addresses_to_load solver path_state in
            loop (bv::acc) (S.union to_load to_load') (n - 1) solver
          | res ->
            begin
              Logger.debug ~level:4 "Solver returned %a"
                Formula_pp.pp_status res;
              Enum (acc, to_load)
            end
        end
    in
    (* We need to avoid removal of the variables that are used in the
       enumeration. Since the enumerated term does not change --- only the
       distinct constant values do --- it is enough to compute the set of
       keep variables before any solver call.
     *)
    let keep = Formula_utils.bv_term_variables expr in
    let values, to_load =
      match with_solver ~keep Relse_stats.Exploration path_state [] (loop [] no_address n) with
      | Some (Enum (values, to_load)) -> values, to_load
      | _ -> [], no_address in
    let values, path_state =
      maybe_load_and_recurse (enumerate_values n expr) values path_state to_load
    in
    if List.length values = n then
      Logger.warning "Found as many solutions for@ %a@ as asked.@ \
                      Possibly incomplete solution set."
        Formula_pp.pp_bv_term expr;
    values, path_state
end

module Translate = struct
  open Dba
  
  (** Get the function to construct the formula for unary operators *)
  let unary e = function
    | Unary_op.Not    -> Formula.mk_bv_not
    | Unary_op.UMinus -> Formula.mk_bv_neg
    | Unary_op.Sext n -> Formula.mk_bv_sign_extend (n - Dba.Expr.size_of e)
    | Unary_op.Uext n -> Formula.mk_bv_zero_extend (n - Dba.Expr.size_of e)
    | Unary_op.Restrict interval -> Formula.mk_bv_extract interval
  
  
  let as_bv bop e1 e2 =
    Formula.(mk_bv_ite (bop e1 e2) (mk_bv_one) (mk_bv_zero))
  
  let rotate_right_const n = Formula.mk_bv_rotate_right n
  let rotate_left_const n = Formula.mk_bv_rotate_left n

  let rotate shift_func rev_shift_func const_rot_func value shift =
    let open Formula in
    match shift.bv_term_desc with
    | BvCst x ->
      let op = Bitvector.value_of x |> Bigint.int_of_big_int |> const_rot_func in
      op value
    | _ ->
      let part1 = shift_func value shift
      and shift_size = Formula_utils.bv_size shift
      and value_size = Formula_utils.bv_size value |> Bigint.big_int_of_int in
      let value_size = Bitvector.create value_size shift_size |> mk_bv_cst in
      let offset = mk_bv_sub value_size shift in
      let part2 = rev_shift_func value offset in
      mk_bv_or part1 part2
        
  let rotate_right = rotate Formula.mk_bv_lshr Formula.mk_bv_shl rotate_right_const
  let rotate_left = rotate Formula.mk_bv_shl Formula.mk_bv_lshr rotate_left_const


  (** Get the function to construct the formula for unary operators *)
  let binary op =
    let open Binary_op in
    match op with
    | Plus   -> Formula.mk_bv_add
    | Minus  -> Formula.mk_bv_sub
    | Mult   -> Formula.mk_bv_mul
    | DivU   -> Formula.mk_bv_udiv
    | DivS   -> Formula.mk_bv_sdiv
    | ModU   -> Formula.mk_bv_urem
    | ModS   -> Formula.mk_bv_srem
    | Eq     -> as_bv (Formula.mk_bv_equal)
    | Diff   -> as_bv (Formula.mk_bv_distinct)
    | LeqU   -> as_bv (Formula.mk_bv_ule)
    | LtU    -> as_bv (Formula.mk_bv_ult)
    | GeqU   -> as_bv (Formula.mk_bv_uge)
    | GtU    -> as_bv (Formula.mk_bv_ugt)
    | LeqS   -> as_bv (Formula.mk_bv_sle)
    | LtS    -> as_bv (Formula.mk_bv_slt)
    | GeqS   -> as_bv (Formula.mk_bv_sge)
    | GtS    -> as_bv (Formula.mk_bv_sgt)
    | Xor    -> Formula.mk_bv_xor
    | And    -> Formula.mk_bv_and
    | Or     -> Formula.mk_bv_or
    | Concat -> Formula.mk_bv_concat
    | LShift -> Formula.mk_bv_shl
    | RShiftU -> Formula.mk_bv_lshr
    | RShiftS -> Formula.mk_bv_ashr
    | LeftRotate -> rotate_left
    | RightRotate -> rotate_right

  
  (** [expr symbolic_state e high] Returns the two formulas representing
     the evaluation of the expressions [e] on the state
     [symbolic_state] in the original program and its renamed version.
     If [e] uses undeclared variables, those variables are declared as
     high if [high=true] or low otherwise.  *)
  let rec expr ?(high=false) (* ?(iv=true) *) symbolic_state e = (* RELSE *)
    let smt_unary = unary and smt_binary = binary in
    let open Dba.Expr in
    let open Rel_expr in
    match e with
    | Var { name; size = bitsize; _ } ->
      Sym_state.var_load symbolic_state name (Size.Bit.create bitsize)

    | Cst (_, bv) -> mk_simple (Formula.mk_bv_cst bv)

    | Load (bytes, _endianness, index) ->
      (* It seems that the weak untaint here makes the performances
         worse *)
      let r_index = (expr symbolic_state index) in
      (* let r_index = Rel_expr.weak_untaint r_index in *)
      Sym_state.memory_select symbolic_state bytes r_index

    | Binary (bop, op1, op2) as e ->
      Logger.debug ~level:6 "Translating binary %a" Dba_printer.Ascii.pp_bl_term e;
      let rel_e1 = expr symbolic_state op1 ~high
      and rel_e2 = expr symbolic_state op2 ~high in
      let do_binary e1 e2 =
        (smt_binary bop e1 e2)
      in apply2 do_binary rel_e1 rel_e2

    | Unary (uop, e) ->
      let rel_e = expr symbolic_state e ~high
      in apply (fun x -> smt_unary e uop x) rel_e

    | Ite (c, then_e, else_e) ->
      let rel_c = expr symbolic_state c ~high
      and rel_then = (expr symbolic_state then_e ~high) 
      and rel_else = (expr symbolic_state else_e ~high) in
      let do_ite c e_then e_else =
        Formula.(mk_bv_ite (mk_bv_equal c (mk_bv_one)) e_then e_else)
      in
      apply3 do_ite rel_c rel_then rel_else
      

  let restrict_to_formula concat_lo concat_hi max_bit rvar rval =
    match concat_lo < 0, concat_hi > max_bit with
    | false, false ->
      Formula.(mk_bv_concat
                 (mk_bv_extract {Interval.lo=concat_hi; Interval.hi=max_bit} rvar)
                 (mk_bv_concat
                    rval
                    (mk_bv_extract {Interval.lo=0; Interval.hi=concat_lo} rvar)))
    | true, false ->
      Formula.(mk_bv_concat
                 (mk_bv_extract {Interval.lo=concat_hi; Interval.hi=max_bit} rvar)
                 rval)
    | false, true ->
      Formula.(mk_bv_concat
                 rval
                 (mk_bv_extract {Interval.lo=0; Interval.hi=concat_lo} rvar))
    | true, true -> rval

  (** [lvalue_with_rval_update symbolic_state rel_val high] Returns
      the function that updates [rval] in [symbolic_state]. If
      [high=true], any undeclared variable is declared high, otherwise,
      it is low *)
  let lvalue_with_rval_update symbolic_state rel_val =
    function
    | LValue.Var { name; size = bitsize;  _ } ->
      let size = Size.Bit.create bitsize in
      Relse_utils.Var (name, size, rel_val)

    | LValue.Restrict ({name; size = bitsize; _}, {Interval.lo; Interval.hi}) ->
      let size = Size.Bit.create bitsize in
      let rel_var = Sym_state.var_load symbolic_state name size in
      let concat_lo = lo - 1 and concat_hi = hi + 1 in
      let max_bit = bitsize - 1 in
      let do_restrict svar rval =
        restrict_to_formula concat_lo concat_hi max_bit svar rval
      in
      let rel_term = Rel_expr.apply2 do_restrict rel_var rel_val in
      Relse_utils.Var (name, size, rel_term)
           
    | LValue.Store (size, _, e) ->
      let r_index = expr symbolic_state e in
      (* let r_index = Rel_expr.weak_untaint r_index in *)
      Relse_utils.Mem (size, r_index, rel_val) 

  let assignment ?(high = false) lvalue rvalue ps =
    let symstate = Path_state.symbolic_state ps in
    let rval = expr symstate rvalue ~high in
    Path_state.assign (lvalue_with_rval_update symstate rval lvalue) ps
end
