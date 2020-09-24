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

module PS = Relse_path.Path_state
module SS = Relse_symbolic.State

type return_type =
  | Continue of PS.t
  | Skip of PS.t
  | Halt

module type STUB = sig
  val name : string
  val do_stub : Virtual_address.t -> PS.t -> return_type
end

type stub = (module STUB)
type t = {
  stubs: (Virtual_address.t, stub) Hashtbl.t;
}

let jump_to_ret_addr ret_addr ps =
  Logger.debug ~level:4 "[Stub] End of stub. Return to %@%a"
    Virtual_address.pp ret_addr;
  PS.goto_vaddr ret_addr ps


let declare_symbolic_size_x86 ~size ~high ret_addr ps =
  if not high && not (LowDecl.get ()) then
    jump_to_ret_addr ret_addr ps
  else
    (* Get the address of the store *)    
    let addr =
      (* Address of argument is located at address esp *)
      let tag = Dba.VarTag.register in
      let size = Relse_utils.word_size_bits () |> Size.Bit.to_int in
      let var_to_load = Dba.Expr.var ~tag "esp" size in
      Dba.Expr.load (Relse_utils.word_size_bytes ()) (Kernel_options.Machine.endianness ()) var_to_load
    in
    Logger.debug ~level:4 "[Stub] %s input of size %d at %@[%a]"
      (if high then "High" else "Low") size Dba_printer.Ascii.pp_bl_term addr;
    (* Add high bytes *)
    let rec init_high_loop n ps =
      if n = size then ps
      else
        let name = Format.asprintf "%c_%a_%d"
            (if high then 'h'else 'l')
            Virtual_address.pp (PS.virtual_address ps) n  in
        let msg = Printf.sprintf "Setting %s byte: %s"
            (if high then "high" else "low") name in
        Logger.debug ~level:5 "[Initialisation] %s" msg;
        let ps = PS.maybe_add_comment ps msg in
        (* Declare symbolic high byte *)
        let symstate = PS.symbolic_state ps in
        let sort = Formula.bv_sort (Natural.to_int Basic_types.Constants.bytesize) in
        let symstate =
          (if high then SS.declare_high else SS.declare_low)
            name sort symstate in
        let ps = PS.set_symbolic_state symstate ps in
        (* Store it in the memory *)
        let addr = Dba.Expr.(add addr (Relse_utils.dba_constant_from_int n)) in
        Logger.debug ~level:4 "[Stub] %@[%a] := %s"
          Dba_printer.Ascii.pp_bl_term addr name;
        let lval = Dba.LValue.store (Size.Byte.create 1) (Kernel_options.Machine.endianness ()) addr in
        let size = (Natural.to_int Basic_types.Constants.bytesize) in
        let rval = Dba.Expr.temporary ~size name in
        let ps = Relse_smt.Translate.assignment lval rval ps
        in init_high_loop (n+1) ps
    in
    let ps = init_high_loop 0 ps in
    jump_to_ret_addr ret_addr ps

let declare_symbolic_size_armv7 ~size ~high ret_addr ps =
  if not high && not (LowDecl.get ()) then
    jump_to_ret_addr ret_addr ps
  else
    let addr =
      (* Address of argument is located at address esp *)
      let tag = Dba.VarTag.register in
      let size = Relse_utils.word_size_bits () |> Size.Bit.to_int in
      let var_to_load = Dba.Expr.var ~tag "sp" size in
      Dba.Expr.load (Relse_utils.word_size_bytes ()) (Kernel_options.Machine.endianness ()) var_to_load
    in
    Logger.debug ~level:4 "[Stub] %s input of size %d at %@[%a]"
      (if high then "High" else "Low") size Dba_printer.Ascii.pp_bl_term addr;
    (* Add high bytes *)
    let rec init_high_loop n ps =
      if n = size then ps
      else
        let name = Format.asprintf "%c_%a_%d"
            (if high then 'h'else 'l')
            Virtual_address.pp (PS.virtual_address ps) n  in
        let msg = Printf.sprintf "Setting %s byte: %s"
            (if high then "high" else "low") name in
        Logger.debug ~level:5 "[Initialisation] %s" msg;
        let ps = PS.maybe_add_comment ps msg in
        (* Declare symbolic high byte *)
        let symstate = PS.symbolic_state ps in
        let sort = Formula.bv_sort ((Natural.to_int Basic_types.Constants.bytesize)) in
        let symstate =
          (if high then SS.declare_high else SS.declare_low)
            name sort symstate in
        let ps = PS.set_symbolic_state symstate ps in
        (* Store it in the memory *)
        let addr = Dba.Expr.(add addr (Relse_utils.dba_constant_from_int n)) in
        Logger.debug ~level:4 "[Stub] %@[%a] := %s"
          Dba_printer.Ascii.pp_bl_term addr name;
        let lval = Dba.LValue.store (Size.Byte.create 1) (Kernel_options.Machine.endianness ()) addr in
        let rval = Dba.Expr.temporary ~size:(Natural.to_int Basic_types.Constants.bytesize) name in
        let ps = Relse_smt.Translate.assignment lval rval ps
        in init_high_loop (n+1) ps
    in
    let ps = init_high_loop 0 ps in
    jump_to_ret_addr ret_addr ps

(* Halts the execution when the function is met *)
module Halt(S: sig val value:String.t end):STUB =
struct
  let name = S.value
  let do_stub _ _ = Halt
end

module Memset:STUB =
struct
  let name = "memset"
  let redirect = "__memset_ia32"

  (* Redirecting memset to __memset_ia32 *)
  let do_stub _ ps =
    let img = Kernel_functions.get_img () in
    match Loader_utils.address_of_symbol_by_name ~name:redirect img with
    | Some addr ->
      let addr = Virtual_address.create addr in
      Logger.debug ~level:6 "[Stub][Memset] jump to addr %a" Virtual_address.pp addr;
      Skip (Relse_path.Path_state.goto_vaddr addr ps)
    | None -> failwith ("[Stub][Memset] " ^ redirect ^ " not found")
end

module HighSize(Bytes : sig val value:Size.Byte.t end):STUB =
struct
  let name = Format.asprintf"high_input_%a" Size.Byte.pp Bytes.value
  let do_stub ret_addr ps =
    let high = not (Relse_utils.is_sse ()) in
    let call_stub = match Kernel_options.Machine.get () with
      | Machine.X86 _ -> declare_symbolic_size_x86
      | Machine.ARM _ -> declare_symbolic_size_armv7
      | _ -> failwith "Stub not implemented for this architecture"
    in
    Skip (call_stub ~size:(Size.Byte.to_int Bytes.value) ~high ret_addr ps)
end

module LowSize(Bytes : sig val value:Size.Byte.t end):STUB =
struct
  let name = Format.asprintf"low_input_%a" Size.Byte.pp Bytes.value
  let do_stub ret_addr ps =
    let call_stub = match Kernel_options.Machine.get () with
      | Machine.X86 _ -> declare_symbolic_size_x86
      | Machine.ARM _ -> declare_symbolic_size_armv7
      | _ -> failwith "Stub not implemented for this architecture"
    in 
    Skip (call_stub ~size:(Size.Byte.to_int Bytes.value) ~high:false ret_addr ps)
end

(* Get list of the the low/high_input functions used in the binary  *)
let get_sym_stub name =
  let high_regexp = Str.regexp "^high_input_[0-9]+$" in
  let low_regexp = Str.regexp "^low_input_[0-9]+$" in
  let stack_chk_fail_regexp = Str.regexp "^_*stack_chk_fail.*$" in
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
  else if Str.string_match stack_chk_fail_regexp name 0 then
    Some (module Halt(
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

(* Create the table mapping addresses their stubs *)
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
  let symbols_to_stub =  find_sym_stubs img in
  List.iter (fun stub -> add_stub stubs stub img) symbols_to_stub;
  { stubs; }

(* Eval the call if it is stubbed *)
let eval_call ctx addr ret_addr ps =
  match Hashtbl.find_opt ctx.stubs addr with
  | None -> None                (* The call is not stubbed *)
  | Some stub ->                (* Eval the stubbed call *)
    let module Stub = (val stub : STUB) in
    Logger.debug ~level:5 "[Stub] Call %s at address @%a with return address at @%a"
      Stub.name Virtual_address.pp addr Virtual_address.pp ret_addr;
    let ps = Stub.do_stub ret_addr ps
    in Some ps

(* Should be somewhere else *)
let check_call ctx ps =
  (* Check if the instruction is a call *)
  let hunk = Instruction.hunk @@ PS.get_instruction ps in
  (* Format.printf "INSTRUCTION: %a\n"
   *   Dhunk.pp hunk; *)
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
           eval_call ctx addr ret_addr ps
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
           Logger.debug ~level:5 "[Stub] Call address @%a with return address at @%a"
            Virtual_address.pp addr Virtual_address.pp ret_addr;
           eval_call ctx addr ret_addr ps
         | _ -> None (* Not a call *))
      | _ -> None
    end
  | _ -> failwith "Stubs not implemented for this architecture"

let is_ret_from_main ps =
  let addr = PS.virtual_address ps in
  (* Check if the instruction is a return *)
  if Instruction.is_return (PS.get_instruction ps) then
    (* Check if it belongs to the main *)
    Loader_utils.belongs_to_symbol (Relse_utils.get_main_symbol ()) addr
  else false

(** [check ctx ps] Updates the stub context [ctx] and the path state
    [ps] according to the current instruction. *)
let check ctx ps =
  let default = Continue ps in
  if PS.get_block_index ps <> 0 then default
  else
    (* Check if the instruction is a return in the main *)
    match is_ret_from_main ps with
    | true -> Halt
    | false -> 
      (* Check if the instruction is a stubbed call *)
      match check_call ctx ps with
      | Some ret -> ret
      | None -> default
