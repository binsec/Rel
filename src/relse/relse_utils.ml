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

type level = Low | High

let level_to_string = function
  | Low -> "low"
  | High -> "high"

let level_to_char = function
  | Low -> 'l'
  | High -> 'h'

type assignment_t =
  | Var of string * Size.Bit.t * Rel_expr.rel_bv (* name, value *)
  | Mem of Dba.size * Rel_expr.rel_bv * Rel_expr.rel_bv (* size, index, value *)

let word_size_bits () = Kernel_options.Machine.bits ()
                     |> Machine.Bitwidth.bitsize    (* 32 bits *)
let word_size_bytes () = Kernel_options.Machine.bits ()
                         |> Machine.Bitwidth.bytesize  (* 4 bytes *)
             
let int_to_bitvector i = Bitvector.of_int ~size:(Size.Bit.to_int (word_size_bits ())) i

let dba_constant_from_int n =
  Dba_utils.Expr.constant_from_int
    ~size:(word_size_bits () |> Size.Bit.to_int )
    ~value:n

let get_stack_register () =
  let register_name = Kernel_options.Machine.stack_register () in
  let size = Kernel_options.Machine.word_size () in
  Dba.Expr.var ~tag:Dba.VarTag.register register_name size

let is_loadable addr =
  let loader = Kernel_functions.get_img () in
  let address = Bitvector.value_of addr |> Bigint.int_of_big_int in
  match Loader_utils.find_section_by_address ~address loader with
  | None -> false
  | Some section ->
    let name = Loader.Section.name section in
    Basic_types.String.Set.mem name (Sse_options.LoadSections.get()) ||
    Sse_options.LoadROSections.get() &&
    Loader.Section.(has_flag Loader_types.Read section &&
                    not (has_flag Loader_types.Write section))


let read_bitvector addr sz =
  let b = Buffer.create (2 * sz) in
  (* The loop below is little-endian *)
  let rec loop offset =
    if offset < 0 then
      let v = Bigint.big_int_of_string ("0x" ^ Buffer.contents b) in
      let bv = Bitvector.create v ((Natural.to_int Basic_types.Constants.bytesize) * sz) in
      Logger.debug ~level:5 "[read_bitvector] Reading image at addr=%a, size=%d. Result=%a"
        Bitvector.pp_hex addr sz Bitvector.pp_hex bv;
      bv
    else
      let off_bv = int_to_bitvector offset in
      let load_addr = Bitvector.add addr off_bv in
      let img = Kernel_functions.get_img () in
      let byte = Loader_utils.get_byte_at img load_addr in
      let byte_str = Format.sprintf "%02x" byte in
      Buffer.add_string b byte_str;
      loop (offset - 1)
  in loop (sz - 1)

let is_sse () =
  match Relse_options.SymbolicStore.get () with
  | Sse -> true
  | _ -> false


(* Warning: works because image never changes *)
let get_main_symbol =
  let sym = ref None in
  (fun () ->
     match !sym with
     | None ->
       let img = Kernel_functions.get_img () in  
       Loader_utils.symbol_by_name ~name:"main" img
       |> Utils.unsafe_get_opt
     | Some s -> s)

let relse_dirname = "binsec_relse"

let temp_file =
  let n = ref 0 in
  fun () ->
    incr n;
    let tmpdir = Sse_options.SMT_dir.get () in
    let temp_dir = Filename.concat tmpdir relse_dirname in
    if not (Sys.file_exists temp_dir) then
      begin
        Logger.debug ~level:6 "Creating directory %s" temp_dir;
        Unix.mkdir temp_dir 0o700
      end;
    let suffix = Format.sprintf "_%d.smt2" !n in
    let prefix = "relse" in
    let filename = Filename.temp_file ~temp_dir prefix suffix in
    Logger.debug ~level:5 "Creating temporary %s" filename;
    filename

let mk_var_name basename idx =
  Format.sprintf "%s_%d" basename idx

type status =
  | TRUE
  | FALSE
  | UNKNOWN

let formula_status status =
  match status with
  | TRUE -> Formula.SAT
  | FALSE -> Formula.UNSAT
  | UNKNOWN -> Formula.UNKNOWN
  
let solving_attempt fml =
  if Formula.(equal_bl_term (mk_bl_comp BlEqual fml mk_bl_true) mk_bl_true)
  then TRUE
  else if Formula.(equal_bl_term (mk_bl_comp BlEqual fml mk_bl_false) mk_bl_true)
  then FALSE
  else UNKNOWN
         
(* TODO comparator-like module ? *)
type comparison_type = Equal | Distinct | NotComparable

let compare_bv bv1 bv2 =
    if Formula.(equal_bl_term (mk_bv_comp BvEqual bv1 bv2) mk_bl_true)
    then Equal
    else if Formula.(equal_bl_term (mk_bv_comp BvDistinct bv1 bv2) mk_bl_true)
    then Distinct (* TODO: Not working... *)
    else NotComparable

let is_const expr =
  let open Formula in
  match expr.bv_term_desc with
  | BvCst _ -> true
  | _ -> false

let is_var expr =
  let open Formula in
  match expr.bv_term_desc with
  | BvFun (_, []) -> true
  | _ -> false

let normalize_simple expr iv =
  let open Formula in
  match expr.bv_term_desc with
  | BvCst _ -> expr
  | BvFun (_, []) -> expr
  | BvFun _ -> iv
  | BvLet _ -> iv
  | BvUnop (_, bv) when is_const bv || is_var bv -> expr
  | BvUnop _-> iv
  | BvBnop (_, bv1, bv2) (* Check the operator (only if add ?)*)
    when (is_const bv1 && (is_const bv2 || is_var bv2)) ||
         (is_const bv2 && (is_var bv1)) -> expr
  | BvBnop _ -> iv
  | BvIte _ -> iv
  | Select _ -> iv
  
let normalize_rel r_expr r_iv =
  Rel_expr.apply2 normalize_simple r_expr r_iv

module AddressList = struct
  type t = {
    list: Dba_types.Statement.t Sequence.t;
    name : string;
  }

  let virtual_address stmt =
    let open Dba_types in
    Statement.location stmt |> Caddress.to_virtual_address
  
  let create ~name = { list = Sequence.empty; name }
  let extend i al =
    match Sequence.peek_front al.list with
    | None -> { al with list = Sequence.push_front i al.list }
    | Some stmt ->
      let last_vaddres = virtual_address stmt in
      let v = virtual_address i in
      if v <> last_vaddres then
        { al with list = Sequence.push_front i al.list }
      else
        al

  let find target al =
    let target = virtual_address target in
    let found addr result =
      let addr = virtual_address addr in
      result || (addr = target)
    in
    Sequence.fold_forward found al.list false

  let pp_as_address_trace ppf list =   
    match Sequence.peek_front list with
    | None -> ()
    | Some _ ->
      Sequence.iter_forward
        (fun i ->
           let v = virtual_address i in
           Format.fprintf ppf "@[<h>%a@]@ " Virtual_address.pp v
        )
        list;
      Format.fprintf ppf "@]"
  
  let pp_address_trace_to_file al path_number =
    if Sse_options.AddressTraceFile.is_set () then
      let filename = (Sse_options.AddressTraceFile.get ()) ^ al.name ^ "_" ^
                     (string_of_int path_number) in
      Print_utils.pp_to_file ~filename pp_as_address_trace al.list
end
