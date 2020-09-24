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
module Path = Relse_utils.AddressList
  
let decode vaddress = Disasm_core.decode vaddress |> fst

module Path_state =
struct

  module Sym_state =  Relse_symbolic.State

  type t  = {
    depth : int;
    path : Path.t option;
    symbolic_state : Sym_state.t;
    instruction : Instruction.t;
    block_index : int;
    status : Formula.status; (* we know that this path is satisfiable *)
  }

  let depth ps = ps.depth

  let status ps = ps.status

  let get_instruction ps =
    ps.instruction

  let get_block_index ps =
    ps.block_index
  
  let get_dba_instruction ps =
    let block = ps.instruction.Instruction.dba_block in
    Dhunk.inst block ps.block_index |> Utils.unsafe_get_opt

  let get_pc ps = Sym_state.path_constraint ps.symbolic_state
  
  let symbolic_state ps = ps.symbolic_state

  let set_symbolic_state symstate ps = { ps with symbolic_state = symstate }

  let on_symbolic_state f ps =
    { ps with symbolic_state = (f ps.symbolic_state) }

  let assign value ps =
    let symbolic_state = 
    match value with
      | Relse_utils.Var (name, size, r_val) ->
        Sym_state.var_assign ps.symbolic_state name size r_val
      | Relse_utils.Mem (size, r_index, r_val) ->
          Sym_state.memory_store ps.symbolic_state size r_index r_val
    in { ps with symbolic_state }
  
  let update_pc ?(checked=false) r_cond ps =
    let current_status = ps.status in
    let symbolic_state = ps.symbolic_state in
    (* It seems that the weak untaint here makes the performances
       worse *)
    (* let rel_cond = Rel_expr.weak_untaint rel_cond in *)
    let cond = match r_cond with
      | Rel_expr.Simple c -> c
      | Rel_expr.Rel (c,c') -> Formula.mk_bl_and c c' in

    if current_status = Formula.UNSAT then
      (* Don't need to add the condition since the formula is UNSAT *) ps
    else if checked = true
    then
      let symbolic_state = Sym_state.pc_update symbolic_state r_cond in
      let status = Formula.SAT in
      { ps with symbolic_state; status }
    else
      match Relse_utils.solving_attempt cond with
      | Relse_utils.TRUE ->
        (* The condition is True, status doesn't change *) ps
      | Relse_utils.FALSE ->
        (* The condition is False, status becomes UNSAT *)
        let symbolic_state = Sym_state.pc_update symbolic_state r_cond
        in { ps with symbolic_state; status = Formula.UNSAT }
      | _ ->
        (* The condition is unknown *)
        let symbolic_state = Sym_state.pc_update symbolic_state r_cond in
        (* Try to solve the new pc *)  
        let new_pc = Sym_state.path_constraint symbolic_state in
        let status = Relse_utils.(solving_attempt new_pc |> formula_status) in
        { ps with symbolic_state; status }

  let update_pc_cond ?(checked=false) condition ps =
    update_pc ~checked condition ps

  let update_pc_dynamic ?(checked=false) r_expr ps =
    update_pc ~checked r_expr ps

  let with_init_mem_at ~addr ~size path_state =
    Sym_state.init_mem_at path_state.symbolic_state ~addr ~size;
    { path_state with status = Formula.UNKNOWN; }

  exception Found

  let address_belongs_to_init ~addr path_state =
    let add_int bv n =
      let size = Bitvector.size_of bv in
      let bv_n = Bitvector.of_int ~size n in
      Bitvector.add bv bv_n in
    try
      Bitvector.Collection.Htbl.iter
        (fun kaddr vsize ->
           if
             Bitvector.compare addr kaddr >= 0 &&
             let end_addr = add_int kaddr vsize in
             Bitvector.compare addr end_addr < 0
           then raise Found
        ) (Sym_state.initialisations path_state.symbolic_state);
      false
    with Found -> true

  let virtual_address st =
    let open Instruction in
    st.instruction.address

  let location st =
    let caddress =
      virtual_address st |> Dba_types.Caddress.of_virtual_address in
    Dba_types.Caddress.reid caddress st.block_index

  (** Get the current statement (Caddr + DBA instuction) *)
  let get_current_statement st =
    get_dba_instruction st
    |> Dba_types.Statement.create (location st)
 
  let goto address ps =
    (* let prev = location st in *)
    let vaddr = Dba_types.Caddress.to_virtual_address address in
    Relse_stats.add_dba_instruction ();
    let instruction, depth =
      if Virtual_address.compare vaddr (virtual_address ps) <> 0 then
        begin
          (* New x86 instruction *)
          Relse_stats.add_instruction ();
          decode vaddr, ps.depth + 1
        end
      else
        ps.instruction, ps.depth
    in
    let block_index = address.Dba.id in
    let ps = { ps with instruction; block_index } in
    let statement = get_current_statement ps in
    let path = match ps.path with
      | None -> None
      | Some x -> Some (Path.extend statement x)
    in
    { ps with path; depth; }

  let goto_vaddr address ps =
    goto (Dba_types.Caddress.of_virtual_address address) ps 

  let set_block_index idx st =
    goto (Dba_types.Caddress.reid (location st) idx) st 

  (** {2 Printers} *)
    
  (** [pp_path ps] Pretty print the current location of [ps] *)
  let pp_loc ppf st =
    let dba_instruction = get_dba_instruction st in
    let vaddress = virtual_address st in
    Format.fprintf ppf "(%a, %d)@ :@ @[%a@]"
      Virtual_address.pp vaddress
      st.block_index
      Dba_printer.Ascii.pp_instruction dba_instruction

  (** [pp_path ps path_number] Pretty print the path leading to [ps]*)
  let pp_path ps path_number = match ps.path with
    | None -> ()
    | Some x -> Path.pp_address_trace_to_file x path_number

  let mark_sat ps = { ps with status = Formula.SAT }

  let create
      ?(depth=0) ?(path=Some (Path.create ~name:"path"))
      ?(block_index=0)
      symbolic_state instruction =
    assert(block_index >= 0 &&
           block_index <= Dhunk.length instruction.Instruction.dba_block);
    let status = Formula.SAT in
    { depth; path; symbolic_state; instruction; block_index; status; }

  let maybe_add_comment ps comment =
    if Sse_options.Comment.get () then
      let symstate = Sym_state.comment comment ps.symbolic_state in
      set_symbolic_state symstate ps
    else ps

end
