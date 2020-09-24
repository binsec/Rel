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

(** Enumerate jumps targets *)
let get_entry_point () =
  match Kernel_functions.get_ep () with
  | Some v -> v
  | None ->
    Logger.warning "No entrypoint: starting from main.";
    Relse_utils.get_main_symbol ()
    |> Loader_utils.address_of_symbol
    |> Virtual_address.create

module Path_state = Relse_path.Path_state
module Sym_state = Relse_symbolic.State

module type SSE_RUNNER = sig val start: unit -> unit end

module Env_make(IS:Relse_insecurity.INSECURITY_STATE): SSE_RUNNER =
struct 
  module State  = struct
    type t = {
      path : Path_state.t;
      insecurity : IS.t;
    }

    (* { Initialization } *)

    (** Initialize the registers to comply with the ABI (DF:=1) *)
    let initialize_registers ps =
      let open Dba in
      (* DF *)
      let df_value = Bitvector.zero in
      Logger.debug ~level:5 "[initialisation] Setting DF to %s" (Bitvector.to_hexstring df_value);
      let rval = Expr.constant df_value in
      let tag = Dba.VarTag.register in
      let lval = LValue.var ~tag ~bitsize:Size.Bit.bits1 "DF" in
      Relse_smt.Translate.assignment lval rval ps


    (** Initialize the initial memory of path state [ps] from [filename]
        (default memory.txt) *)
    let init_from_file ps =
      let filename = Sse_options.MemoryFile.get () in
      if not (Sys.file_exists filename) then begin
        Logger.warning "Cannot find relse configuration file %s" filename;
        ps
      end
      else
        let initials =
          Logger.debug "Reading initialization from %s" filename;
          let parser = Parser.initialization
          and lexer = Lexer.token in
          Parse_utils.read_file ~parser ~lexer ~filename
        in
        let f ps init =
          let open Parse_helpers.Initialization in
          match init.operation with
          | Mem_load (addr, size) ->
            (* Read these addresses from the binary file. *)
            Path_state.with_init_mem_at ps ~addr ~size
          | Universal _ -> failwith "Not implemented"
          | Assignment (lval, rval, _) ->
            match rval with
            | Singleton rv -> Relse_smt.Translate.assignment lval rv ps
            | _ -> failwith "Not implemented"
        in List.fold_left f ps initials
    
    let make_initial_state ~entrypoint =
      let decode vaddress = Disasm_core.decode vaddress |> fst in
      let initial_instruction = decode entrypoint in
      let initial_symstate = Sym_state.create () in
      let initial_ps, initial_is =
        Path_state.create initial_symstate initial_instruction
        |> init_from_file
        |> initialize_registers
        |> IS.initialize in
      { path=initial_ps; insecurity=initial_is }

    let path state = state.path
    let insecurity state = state.insecurity
    let symbolic_state state = Path_state.symbolic_state state.path

    let set_path state path = { state with path }

    (* let set_insecurity state insecurity = { state with insecurity } *)

    let on_path f state =
      { state with path = (f state.path) }

    (* let on_insecurity f state =
     *   { state with insecurity = (f state.insecurity) } *)

    let update_state f state =
      let (path, insecurity) = f state.path state.insecurity
      in { path; insecurity }
  end

  module State_stack = Fstack.Make(State)

  module Env = struct

    type t = {
      (* The stack of states to explore *)
      worklist : State_stack.t;

      (* The current state *)
      current_state : State.t option;

      (* The number of paths currently explored *)
      paths : int;

      (* The stub context which contains all the stub informations *)
      stub_ctx: Relse_stubs.t;
    }

    let empty = 
      let worklist = State_stack.empty in
      let initial_state = None in
      let stub_ctx = Relse_stubs.empty in      
      { worklist; current_state=initial_state; paths=0; stub_ctx; }

    (* Current environment *)
    let env = ref empty

    (* Initialize the environment  *)
    let initialize_env ~entrypoint =
      let initial_state = State.make_initial_state ~entrypoint in
      let worklist = State_stack.singleton initial_state in
      let stub_ctx = Relse_stubs.init () in
      env := { empty with worklist; stub_ctx }

    (* Chose a new path from then environment *)
    let next_state () =
      let worklist, current_state =
        match State_stack.pop !env.worklist with
        | current_state, worklist ->
          Logger.debug ~level:3 "[Exploration] Choose new path from environment :@ %a"
            Path_state.pp_loc (State.path current_state);
          worklist, Some current_state
        | exception Not_found -> !env.worklist, None
      in env := {!env with worklist; current_state }

    let push_state st =
      let worklist = State_stack.push st !env.worklist in
      env := { !env with worklist }

    let incr_paths () =
      env := { !env with paths=(!env.paths + 1) }

    let current_state () =
      match !env.current_state with
      | Some current_state -> current_state
      | None -> failwith "No current state in the environment"

    let current_state_option () = !env.current_state

    let set_current_state state =
      env := { !env with current_state = Some state }

    let get_nb_paths () = !env.paths

    let stub_ctx () = !env.stub_ctx

    (** Mark the current state as satisfiable *)
    let mark_sat () =
      let current_state = current_state () in
      set_current_state (State.on_path Path_state.mark_sat current_state)

    (** Set the current path state *)
    let set_current_path_state ps =
      let current_state = current_state () in
      set_current_state (State.set_path current_state ps)

    (* (\** Get the current path state *\)
     * let get_current_path_state () =
     *   State.path (current_state ()) *)
  end

  module Terminator = struct
    type t = {
      end_path   : Path_state.t -> bool;
      end_search : Path_state.t -> bool;
    }

    let create ~end_path ~end_search () =
      { end_path; end_search }

    let dfs ~goals ~avoids =
      let in_set st = Virtual_address.Set.mem (Path_state.virtual_address st) in
      let end_path =
        (fun st ->
           if in_set st avoids
           then (Logger.debug ~level:2 "[Exploration] End of path : to be avoided"; true)
           else if Sse_options.MaxDepth.get () > 0 &&
                   Path_state.depth st >= Sse_options.MaxDepth.get ()
           then
             begin
               Relse_stats.(set_status Max_Depth);
               Logger.warning "[Exploration] Max depth exceeded (%d)" (Path_state.depth st);
               true
             end
           else false)
      and end_search st = in_set st goals in
      create ~end_search ~end_path ()
  end

  (** Terminates the current path *)
  let end_path () =
    Env.incr_paths ();
    let state = Env.current_state () in
    let path_number = Env.get_nb_paths () in
    Logger.debug ~level:4 "[Exploration] Path %d explored." path_number;
    Path_state.pp_path (State.path state) path_number;
    Relse_stats.add_path ()

  (** End the RelSE *)
  let end_relse msg =
    Logger.result msg;
    Relse_stats.print_stats ();
    exit (Relse_stats.get_exit_code ())

  (** Terminates the current path and the RelSE *)
  let end_path_and_relse msg =
    end_path () |> ignore;
    end_relse msg

  (* Check the remaining insecurity queries *)
  let end_path_and_check () =
    end_path ();
    let state = Env.current_state () in
    (* Check remaining insecurity queries *)
    ignore (IS.force_check
              (State.path state)
              (State.insecurity state));
    if Relse_options.MaxPaths.get () > 0 &&
       Env.get_nb_paths () >= Relse_options.MaxPaths.get ()
    then
      begin
        Relse_stats.(set_status Max_Paths);
        end_relse "Maximum number of paths reached."
      end

  let get_avoid_address () =
    let addresses = Sse_options.AvoidAddresses.get () in

    let add_function_end func_name addresses =
      let img = Kernel_functions.get_img () in
      let _, end_fun = Loader_utils.symbol_by_name ~name:func_name img 
                       |> Utils.unsafe_get_opt
                       |> Loader_utils.symbol_interval in
      let end_fun = (Virtual_address.pred end_fun) in
      Logger.debug ~level:2 "[Initialization] Adding end of %s at %a to avoids."
        func_name Virtual_address.pp end_fun;
      Basic_types.Int.Set.add (Virtual_address.to_int end_fun) addresses
    in

    (* If entrypoint is a function, add ret from function as avoid address *)
    let addresses =
      match Kernel_options.Entry_point.get_opt () with
      | None -> add_function_end "main" addresses (* Means we start from main *)
      | Some s ->
        match Loader_utils.Binary_loc.of_string s with
        | Loader_utils.Binary_loc.Name func_name ->
          add_function_end func_name addresses
        | _ -> addresses
    in
    let pp_address addr =
      Logger.debug ~level:2 "[Initialization] Avoids %a"
        Virtual_address.pp (Virtual_address.create addr)
    in
    Basic_types.Int.Set.iter pp_address addresses;
    addresses


  let check_sat_current_state () =            (* TODO Move somewhere else *)
    let ps = State.path (Env.current_state ()) in
    match Relse_path.Path_state.status ps with
    | Formula.SAT ->
      (Logger.debug ~level:2 "[Exploration] Spared pc check: sat"; Env.mark_sat (); true)
    | Formula.UNSAT ->
      (Logger.debug ~level:2 "[Exploration] Spared pc check: unsat"; false)
    | _ ->
      Logger.debug ~level:2 "[Exploration] Running exploration query";
      match Relse_smt.Solver.check_sat ps with
      | Formula.SAT, ps -> Env.set_current_path_state ps; Env.mark_sat (); true
      | _ -> false


  (** Pick the next satisfiable branch in the worklist *)
  let rec choose_next_state () =
    Env.next_state ();
    match Env.current_state_option () with
    | Some _ ->
      (match check_sat_current_state () with
       | true -> ()
       | false -> choose_next_state ())
    | None -> (* No more paths *)
      end_relse "[Exploration] End of the RelSE"



  module Eval = struct  

    let assignment ~lvalue ~rvalue state =
      State.on_path (Relse_smt.Translate.assignment lvalue rvalue) state


    let static_jump ~jump_target state =
      match jump_target with
      | Dba.JInner idx ->
        State.on_path (Path_state.set_block_index idx) state
      | Dba.JOuter addr ->
        let vaddr = Dba_types.Caddress.to_virtual_address addr in
        Logger.debug ~level:5 "Jumping to new address %a"
          Virtual_address.pp vaddr;
        State.on_path (Path_state.goto_vaddr vaddr) state


    let fork_path ~r_cond ~jump_target ~local_target state =
      Relse_stats.add_fork ();

      (* Prepare conditional expressions *)
      let mk_then_cond expr = Formula.(mk_bv_equal expr mk_bv_one) in
      let then_expr = Rel_expr.apply mk_then_cond r_cond in
      let mk_else_cond expr = Formula.mk_bl_not (mk_then_cond expr) in
      let else_expr = Rel_expr.apply mk_else_cond r_cond in

      (* Expand current path with assert condition and go to jump_target *)
      let then_state =
        State.on_path (Path_state.update_pc_cond then_expr) state
        |> static_jump ~jump_target
      in
      (* Push path with negation of the condition and go to local_target *)
      let else_state =
        let update_pc ps = (Path_state.update_pc_cond else_expr) ps
                           |> Path_state.set_block_index local_target in
        State.on_path update_pc state
      in
      Env.push_state then_state; Env.push_state else_state; choose_next_state ()


    let ite ~condition ~jump_target ~local_target =
      let state = Env.current_state () in
      Logger.debug ~level:2 "[Exploration][Ite] Condition %a"
        Dba_types.Expr.pp condition;

      match Dba.Expr.constant_value condition with
      | Some bv when Bitvector.is_zeros bv ->
        (* Condition evaluates to false *)
        static_jump ~jump_target state
        |> Env.push_state;
        choose_next_state ()
      | Some _ ->
        (* Condition evaluates to true *)
        State.on_path (Path_state.set_block_index local_target) state
        |> Env.push_state;
        choose_next_state ()
      | None ->
        let r_cond = Relse_smt.Translate.expr (State.symbolic_state state) condition in
        fork_path ~r_cond ~jump_target ~local_target state

    let dynamic_jump ~jump_expr =
      let state = Env.current_state () in
      let ps = State.path state in
      let img = Kernel_functions.get_img () in
      let r_expr = Relse_smt.Translate.expr (Path_state.symbolic_state ps) jump_expr in

      let n = Sse_options.JumpEnumDepth.get () in
      (* Jump only on the target of the left side *)
      let target = Rel_expr.left r_expr in
      let concretes, ps = match Formula_utils.is_bv_cst target with
        | Some bv -> (* If the target expression is concrete, goto jump target *)
          [bv], ps
        | None -> (* If the target expression is symbolic, enumerate jump target *)
          let concretes, ps = Relse_smt.Solver.enumerate_values n target ps in
          if List.length concretes >= n then Relse_stats.(set_status EnumLimit);
          concretes, ps
      in
      let with_bv path_state bv =
        let mk_cond expr = Formula.(mk_bv_equal (mk_bv_cst bv) expr) in
        let condition = Rel_expr.apply mk_cond r_expr in
        let addr = Virtual_address.of_bitvector bv in
        let invalid bv =
          Logger.warning
            "@[<hov>Dynamic jump@ %a@ could have led to invalid address %a;@ \
             skipping@]"
            Path_state.pp_loc path_state Bitvector.pp_hex bv
        in
        Logger.debug ~level:4 "[Exploration] Dynamic jump@ %a@ could lead to %a"
          Path_state.pp_loc path_state
          Bitvector.pp_hex bv;
        let address = Virtual_address.to_int addr in
        let section = Loader_utils.find_section_by_address ~address img in
        match section with
        | Some s when
            Loader.Section.has_flag Loader_types.Read s &&
            Loader.Section.has_flag Loader_types.Exec s ->
          let ps = (Path_state.update_pc_dynamic ~checked:true condition) path_state
                   |> Path_state.goto_vaddr addr in
          Env.push_state (State.set_path state ps)
        | Some _ | None -> invalid bv
      in
      (* Add the SRNI check only in the first branch *)
      List.iter (with_bv ps) concretes;
      choose_next_state ()

    let skip instruction idx =
      Logger.debug ~level:3 "[Exploration] Skipping %a" Dba_printer.Ascii.pp_instruction instruction;
      Path_state.set_block_index idx

    (* If comment is activated, this will add, for every formula entry, a
       comment about where it comes from.
       This can be usefull to debug the path predicate translation.  *)
    let maybe_add_comment ps =
      let comment =
        Print_utils.string_from_pp
          (Formula_pp.pp_as_comment Path_state.pp_loc) ps
      in Path_state.maybe_add_comment ps comment

    (** Evaluation of a DBA instuction in the symbolic environment *)
    let eval () =
      let state = State.on_path maybe_add_comment (Env.current_state ()) in
      let ps = State.path state in
      Logger.debug ~level:5 "@[Evaluating@ %a@]" Path_state.pp_loc ps;

      (* Check for stubs *)
      match Relse_stubs.check (Env.stub_ctx ()) ps with
      | Relse_stubs.Halt ->
        end_path_and_check (); choose_next_state ()
      | Relse_stubs.Skip ps ->
        Env.set_current_state (State.set_path state ps)
      | Relse_stubs.Continue ps ->
        Env.set_current_state (State.set_path state ps);      
        let dba_instr = Path_state.get_dba_instruction ps in

        (* Gather (and possibly verifies) the insecurity checks at the
           current instruction *)
        let state = State.update_state (IS.eval) state in

        (* Evaluate the instruction
           TODO: harmonization of functions / style *)
        match dba_instr with
        | Dba.Instr.Assign (lvalue, rvalue, idx) ->
          let state = state
                      |> assignment ~lvalue ~rvalue
                      |> State.on_path (Path_state.set_block_index idx) in
          Env.set_current_state state;

        | Dba.Instr.SJump (jump_target, _) ->
          let state = state
                      |> static_jump ~jump_target in
          Env.set_current_state state;

        | Dba.Instr.If (condition, jump_target, local_target) ->
          ite ~condition ~jump_target ~local_target

        | Dba.Instr.DJump (e, _) ->

          dynamic_jump ~jump_expr:e

        | Dba.Instr.Stop (Some Dba.KO) ->
          (* Discard current path, choose a new one *) 
          end_path_and_check (); choose_next_state ()

        | Dba.Instr.Undef (_, idx) as instruction ->
          (* Instruction [lval := undef] -> Ignores the instruction *)
	        let state = state
	                    |> State.on_path (skip instruction idx) in
	        Env.set_current_state state

        | Dba.Instr.Stop _
        | Dba.Instr.Assert _
        | Dba.Instr.Assume _
        | Dba.Instr.Nondet _
        | Dba.Instr.NondetAssume _
        | Dba.Instr.Malloc _
        | Dba.Instr.Free _
        | Dba.Instr.Print _ as dba_instruction ->
          let vaddress = Path_state.location ps in
          let msg =
            Format.asprintf "instruction %a at address %a"
              Dba_printer.Ascii.pp_instruction dba_instruction
              Virtual_address.pp (Dba_types.Caddress.to_virtual_address vaddress) 
          in Errors.not_yet_implemented msg
  end


  (** Explores all the symbolic paths *)
  let loop_until ~p ~halt =
    let rec loop_aux () =
      let open Terminator in
      let state = Env.current_state () in
      let ps = (State.path state) in
      (* Reached the goal (and the end of the path) *)  
      if p.end_search ps then
        begin
          end_path_and_check ();
          halt state
        end
        (* Reached the end of the path *)
      else if p.end_path ps then
        begin
          end_path_and_check ();
          choose_next_state ();
          loop_aux ()
        end
        (* Continue along this path *)
      else
        begin
          Eval.eval ();
          loop_aux ()
        end
    in
    choose_next_state (); loop_aux ()


  (** Halts the relationalSE along a path and returns a model.
      - Generate a output file with the stats,  *)
  let halt state =
    let ps = State.path state in
    let model = Relse_smt.Solver.get_model ps in
    Logger.result "@[<v 0>[Exploration] RelSE ended with the following model:@ %a@]"
      Smt_model.pp model;
    end_path_and_relse "[Exploration] Halt"


  (** Run the relational symbolic execution on the specified file *)
  let do_relse ~filename =
    Logger.debug ~level:2 "[Initialization] Running RelSE with %s" filename;
    let entrypoint = get_entry_point () in
    Logger.debug ~level:2 "[Initialization] Starting from %a" Virtual_address.pp
      entrypoint;
    let ints_to_vaddresses iset =
      Basic_types.Int.Set.fold
        (fun i vset -> Virtual_address.(Set.add (create i) vset))
        iset
        Virtual_address.Set.empty
    in
    let p =
      Terminator.dfs
        ~goals:(Sse_options.GoalAddresses.get () |> ints_to_vaddresses)
        ~avoids:(get_avoid_address () |> ints_to_vaddresses) in
    Env.initialize_env ~entrypoint;
    loop_until ~p ~halt

  let timeout_handler _ =
    Relse_stats.set_status Relse_stats.Timeout;
    end_path_and_relse "[Exploration] Timeout of the RelSE"


  (** Setup timeout fo the RelSE and interrupt redirection *)
  let prepare_interrupt_state () =
    (* Redirect interrupts to handler *)
    Sys.(set_signal sigalrm (Signal_handle timeout_handler));
    Sys.(set_signal sigint (Signal_handle timeout_handler));
    (* Set gloabl timeout *)
    let set_timeout t = if t > 0 then Unix.alarm t |> ignore in 
    set_timeout @@ Relse_options.Timeout.get ()
  
  let start () =
    let filename = Kernel_options.ExecFile.get () in
    prepare_interrupt_state ();      
    do_relse ~filename
end

(** Run the relational symbolic execution *)
let run () =
  if Relse_options.is_enabled () && Kernel_options.ExecFile.is_set () then
    let (module IS) = Relse_insecurity.init () in
    let module S = Env_make(IS) in S.start ()

let _ =
  Cli.Boot.enlist ~name:"RelSE" ~f:run
