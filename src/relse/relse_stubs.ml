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
module Make(IS:Relse_insecurity.INSECURITY_STATE) =
struct
  open Relse_options

  module PS = Relse_path.Path_state

  type return_type =
    | Continue of PS.t * IS.t

    | Skip of PS.t * IS.t
    | Terminated

  module type STUB = sig
    val name : string
    val do_stub : return:Virtual_address.t option -> PS.t -> IS.t -> return_type
  end

  type stub = (module STUB)
  type t = {
    stubs: (Virtual_address.t, stub) Hashtbl.t;
  }

  let jump_to_ret_addr ret_addr ps =
    Logger.debug ~level:4 "[Stub] End of stub. Return to %@%a"
      Virtual_address.pp ret_addr;
    PS.goto_vaddr ret_addr ps


  let declare_symbolic_input ~level ~size ret_addr ps is =
    (* Get the address of the variable from the stack *)
    (* This address is located at @[esp] *)
    let base_addr =
      let size = Relse_utils.word_size_bytes () in
      let endianness = Kernel_options.Machine.endianness () in
      let stack_register = Relse_utils.get_stack_register () in
      Dba.Expr.load size endianness stack_register
      in
    (* Loop over bytes *)
    let rec init_symbol_offset offset (ps,is) =
      if offset = size then ps,is
      else
        let name = Format.asprintf "%c_%a_%d"
            (Relse_utils.level_to_char level)
            Virtual_address.pp (PS.virtual_address ps) offset in
        let addr = Dba.Expr.(add base_addr (Relse_utils.dba_constant_from_int offset)) in
        let ps, is = IS.declare_input_addr ~level addr name ps is in
        init_symbol_offset (offset + 1) (ps,is)
    in
    let ps, is = init_symbol_offset 0 (ps,is) in
    jump_to_ret_addr ret_addr ps, is


  (* Terminate the execution when the function is met *)
  module Terminate(S: sig val value:String.t end):STUB =
  struct
    let name = S.value
    let do_stub ~return:_ _ _ = Terminated
  end

  module Memset:STUB =
  struct
    let name = "memset"
    let redirect = "__memset_ia32"

    (* Redirecting memset to __memset_ia32 *)
    let do_stub ~return:_ ps is =
      let img = Kernel_functions.get_img () in
      match Loader_utils.address_of_symbol_by_name ~name:redirect img with
      | Some addr ->
        let addr = Virtual_address.create addr in
        Logger.debug ~level:6 "[Stub][Memset] jump to addr %a" Virtual_address.pp addr;
        Skip (Relse_path.Path_state.goto_vaddr addr ps, is)
      | None -> failwith ("[Stub][Memset] " ^ redirect ^ " not found")
  end


  module Memset_ifunc:STUB =
  struct
    let name = "memset_ifunc"
    let redirect = "__memset_ia32"

    (* Redirecting memset to __memset_ia32 *)
    let do_stub ~return:_ ps is =
      let img = Kernel_functions.get_img () in
      match Loader_utils.address_of_symbol_by_name ~name:redirect img with
      | Some addr ->
        let addr = Virtual_address.create addr in
        Logger.debug ~level:6 "[Stub][Memset_ifunc] jump to addr %a" Virtual_address.pp addr;
        Skip (Relse_path.Path_state.goto_vaddr addr ps, is)
      | None -> failwith ("[Stub][Memset_ifunc] " ^ redirect ^ " not found")
  end

  module Bzero:STUB =
  struct
    let name = "bzero"
    let redirect = "__bzero_ia32"

    (* Redirecting bzero to __bzero_ia32 *)
    let do_stub ~return:_ ps is =
      let img = Kernel_functions.get_img () in
      match Loader_utils.address_of_symbol_by_name ~name:redirect img with
      | Some addr ->
        let addr = Virtual_address.create addr in
        Logger.debug ~level:6 "[Stub][Bzero] jump to addr %a" Virtual_address.pp addr;
        Skip (Relse_path.Path_state.goto_vaddr addr ps, is)
      | None -> failwith ("[Stub][Bzero] " ^ redirect ^ " not found")
  end


  let return_from_stubbed_call ps =
    let size = Relse_utils.word_size_bytes () in
    let endianness = Kernel_options.Machine.endianness () in
    let stack_register = Relse_utils.get_stack_register () in
    (* Get the return address from the stack *)
    (* This address is located at @[esp] *)
    let ret_addr =
      Dba.Expr.load size endianness stack_register |>
      Relse_smt.Translate.expr (PS.symbolic_state ps) |>
      Rel_expr.value |>
      Formula_utils.is_bv_cst |>
      Utils.unsafe_get_opt |>
      Virtual_address.of_bitvector
    in
    (* Undo the call: esp := esp + 4 *)
    let ps =
      let incr_4 expr =
        let size = Relse_utils.word_size_bits () |> Size.Bit.to_int in
        Relse_smt.Translate.assignment
          (Dba.LValue.of_expr expr)
          (Dba.Expr.add expr (Dba.Expr.constant (Bitvector.of_int ~size 4)))
          ps
      in
      incr_4 stack_register
    in
    Logger.debug ~level:4 "[Stub] Return from stubbed call, ret_address=%a"
      Virtual_address.pp ret_addr;
    ret_addr, ps

  module HighSize(Bytes : sig val value:Size.Byte.t end):STUB =
  struct
    let name = Format.asprintf"high_input_%a" Size.Byte.pp Bytes.value

    let do_stub ~return ps is =
      let level = if Relse_utils.is_sse ()
        then Relse_utils.Low
        else Relse_utils.High in
      let ret_addr, ps = match return with
      | Some ret_addr -> ret_addr, ps
      | None -> return_from_stubbed_call ps
      in
      let ps, is = declare_symbolic_input ~level
          ~size:(Size.Byte.to_int Bytes.value) ret_addr ps is in
      Skip (ps, is)
  end

  module LowSize(Bytes : sig val value:Size.Byte.t end):STUB =
  struct
    let name = Format.asprintf"low_input_%a" Size.Byte.pp Bytes.value
    let do_stub ~return ps is =
      let ret_addr, ps = match return with
        | Some ret_addr -> ret_addr, ps
        | None -> return_from_stubbed_call ps
      in
      let ps, is = declare_symbolic_input ~level:Relse_utils.Low
          ~size:(Size.Byte.to_int Bytes.value) ret_addr ps is in
      Skip (ps, is)
  end

  (* Get list of the the low/high_input functions used in the binary +
     stack protector failure + *)
  let get_sym_stub name =
    let high_regexp = Str.regexp "^high_input_[0-9]+$" in
    let low_regexp = Str.regexp "^low_input_[0-9]+$" in
    let stack_chk_fail_regexp = Str.regexp "^_*stack_chk_fail.*$" in
    let assert_fail_regexp = Str.regexp "^_*assert_fail" in
    if Str.string_match high_regexp name 0 then
      let list = String.split_on_char '_' name in
      Some (module HighSize(
          struct
            let value = Size.Byte.of_string (List.nth list 2)
          end):STUB)
    else if Str.string_match low_regexp name 0 then
      let list = String.split_on_char '_' name in
      Some (module LowSize(
          struct
            let value = Size.Byte.of_string (List.nth list 2)
          end):STUB)
      (* Stop when stack protector fails *)
    else if Str.string_match stack_chk_fail_regexp name 0 then
      Some (module Terminate(
          struct
            let value = name
          end):STUB)
      (* Stop when assert fails *)
    else if Str.string_match assert_fail_regexp name 0 then
      Some (module Terminate(
          struct
            let value = name
          end):STUB)
    else None

  let find_sym_stubs img =
    let symbols = Loader.Img.symbols img in
    let add_sym_stub list symbol = 
      let name = Loader.Symbol.name symbol in
      match get_sym_stub name with
      | Some stub -> stub :: list
      | None -> list in
    Array.fold_left add_sym_stub [] symbols

  (* [add_stub ctx stub img] Add the module [stub] to the table [ctx.table]
     with the virtual addess specified in [img] *)
  let add_stub stubs stub img =
    let module Stub = (val stub : STUB) in
    let name = Stub.name in
    Loader_utils.address_of_symbol_by_name ~name img
    |> (function
        | Some addr ->
          Logger.debug ~level:1 "[Stub] Add symbol %s to stubs" name;
          let addr = Virtual_address.create addr in
          Hashtbl.add stubs addr stub
        | None -> Logger.warning "[Stub] Symbol %s not found" name)

  (* Create the table mapping addresses to their stubs *)
  let empty =
    let size = 2 in
    let stubs = Hashtbl.create size in
    { stubs; }

  (* Create the table mapping addresses their stubs *)
  let init () =
    let size = 2 in
    let img = Kernel_functions.get_img () in
    let stubs = Hashtbl.create size in
    add_stub stubs (module Memset) img;
    add_stub stubs (module Memset_ifunc) img;
    add_stub stubs (module Bzero) img;
    let symbols_to_stub =  find_sym_stubs img in
    List.iter (fun stub -> add_stub stubs stub img) symbols_to_stub;
    { stubs; }

  (* Eval the call if it is stubbed *)
  let eval_call ctx addr ret_addr ps is =
    match Hashtbl.find_opt ctx.stubs addr with
    | None -> None                (* The call is not stubbed *)
    | Some stub ->                (* Eval the stubbed call *)
      let module Stub = (val stub : STUB) in
      Logger.debug ~level:5 "[Stub] Call %s at address @%a with return address at @%a"
        Stub.name Virtual_address.pp addr Virtual_address.pp ret_addr;
      Some (Stub.do_stub ~return:(Some ret_addr) ps is)

  (* Should be somewhere else *)
  let check_call ctx ps is =
    (* Check if the instruction is a call *)
    let hunk = Instruction.hunk @@ PS.get_instruction ps in
    match Kernel_options.Machine.get () with
    | Machine.X86 _ ->
      begin
        match Dhunk.inst hunk 2 with
        | Some instr ->
          (match instr with
           | Dba.Instr.SJump ( Dba.JOuter addr, Some (Dba.Call ret_addr)) ->
             (* Function call, check if it is stubbed *)
             let addr = Dba_types.Caddress.to_virtual_address addr in
             let ret_addr = Dba_types.Caddress.to_virtual_address ret_addr in
             Logger.debug ~level:9 "[Stub] Call address @%a with return address at @%a"
               Virtual_address.pp addr Virtual_address.pp ret_addr;
             eval_call ctx addr ret_addr ps is
           | _ -> None (* Not a call *))
        | _ -> None
      end
    | Machine.ARM _ ->
      begin
        match Dhunk.inst hunk 1 with
        | Some instr ->
          (match instr with
           | Dba.Instr.SJump (Dba.JOuter addr, _) ->
             (* Function call, check if it is stubbed *)
             let addr = Dba_types.Caddress.to_virtual_address addr in
             let ret_addr = Virtual_address.add_int 4 (PS.virtual_address ps) in
             Logger.debug ~level:9 "[Stub] Call address @%a with return address at @%a"
               Virtual_address.pp addr Virtual_address.pp ret_addr;
             eval_call ctx addr ret_addr ps is
           | _ -> None (* Not a call *))
        | _ -> None
      end
    | _ -> failwith "This machine is unsupported"

  (* Check if the address should be redirected to another address (typically
     indirect function like memset_ifunc) *)
  let check_redirections ctx ps is =
    let addr = PS.virtual_address ps in
    match Hashtbl.find_opt ctx.stubs addr with
    | None -> None                (* No redirections *)
    | Some stub ->                (* Redirect address *)
      let module Stub = (val stub : STUB) in
      Logger.debug ~level:5 "[Stub] Redirection (%s) from address @%a"
        Stub.name Virtual_address.pp addr;
      Some (Stub.do_stub ~return:None ps is)


  let is_ret_from_main ps =
    let addr = PS.virtual_address ps in
    (* Check if the instruction is a return *)
    if Instruction.is_return (PS.get_instruction ps) then
      (* Check if it belongs to the main *)
      Loader_utils.belongs_to_symbol (Relse_utils.get_main_symbol ()) addr
    else false

  (** [check ctx ps] Updates the stub context [ctx] and the path state
      [ps] according to the current instruction. *)
  let check ctx ps is =
    let default = Continue (ps, is) in
    if PS.get_block_index ps <> 0 then default
    else
      (* Check if the instruction is a return in the main *)
      match is_ret_from_main ps with
      | true -> Terminated
      | false -> 
        (* Check if the instruction is a stubbed call *)
        match check_call ctx ps is with
        | Some ret -> ret
        | None ->
          match check_redirections ctx ps is with
          | Some ret -> ret
          | None -> default

end
