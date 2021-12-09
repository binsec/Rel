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

module Statistics = struct

  type h = (int, unit) Hashtbl.t
  type opcode_tbl = (Instruction.Generic.t, unit) Hashtbl.t

  type t = {
    decoded : opcode_tbl;
    mutable n_instr : int;
    parse_failed : h;
    other_errors : h;
    not_implemented : h;
  }

  let empty = { decoded = Hashtbl.create 17;
                n_instr = 0;
                parse_failed = Hashtbl.create 7;
                other_errors = Hashtbl.create 7;
                not_implemented = Hashtbl.create 7;
              }

  let size h = Hashtbl.length h
  let size_unique h =
    let open Basic_types in
    let s = Int.Set.empty in
    Hashtbl.fold (fun k _ s -> Int.Set.add k s) h s
    |> Int.Set.cardinal

  let pp_h ppf h =
    Format.fprintf ppf
      "@[<v 0>%d (%d)@ @[<hov 0>%a@]@]"
      (size h)
      (size_unique h)
      (fun ppf -> Hashtbl.iter (fun k _ -> Format.fprintf ppf "%x;@ " k))
      h


  let pp ppf t =
    Format.fprintf ppf
      "@[<v 0>\
       ARM decoding statistics@ \
       ----@ \
       Decoded (unique): %d (%d)@ \
       Failed parsing (unique): %a@ \
       Not implemented (unique): %a @ \
       Misc errors (unique): %a@ \
       @]"
      t.n_instr (Hashtbl.length t.decoded)
      pp_h t.parse_failed
      pp_h t.not_implemented
      pp_h t.other_errors

end

let stats = Statistics.empty

let show_stats ppf () = Statistics.pp ppf stats


let find key kvs =
  try List.assoc key kvs
  with
  | Not_found ->
    Arm_options.Logger.fatal "Decoder message has no %s field. Aborting." key


(* Some conversion functions from parsed categorized value to the expected types
   in Instruction.Generic.create *)
let to_hex_opcode = function
  | Parse_helpers.Message.Value.Hex h -> Format.sprintf "%x" h
  | _ -> assert false


let to_mnemonic = function
  | Parse_helpers.Message.Value.Str s ->
    Mnemonic.supported s Format.pp_print_string
  | _ -> assert false

let just_integer = function
  | Parse_helpers.Message.Value.Int n -> n
  | _ -> assert false


let compare_labeled_instruction (caddr1, _i1) (caddr2, _i2) =
  Dba_types.Caddress.compare caddr1 caddr2


let to_block addr_instr_list =
  (* Blocks returned by Unisimi's ARM decoded are not necessarily ordered.
     We need to do it here. The specific comparison functions explicits
     assumptions about what is expected (same virtual addresses and differences
     of identifiers).
  *)
  List.sort compare_labeled_instruction addr_instr_list
  |> List.map snd
  |> Dhunk.of_list


let mk_instruction (kvs, instructions) =
  let opcode = find "opcode" kvs |> to_hex_opcode in
  let mnemonic = find "mnemonic" kvs |> to_mnemonic in
  let size = find "size" kvs |> just_integer in
  let block = to_block instructions in
  let ginstr = Instruction.Generic.create size opcode mnemonic in
  ginstr, block

(* Create a dummy instruction.
   This is used for "unfailing" mode where something is always returned, even in
   cases of Parser.Error.
*)
let dummy_instruction kvs =
  let block = Dba.Instr.stop (Some Dba.KO) |> Dhunk.singleton in
  let opcode   = find "opcode" kvs |> to_hex_opcode in
  let mnemonic = find "mnemonic" kvs |> to_mnemonic in
  let size = 4 in
  let ginstr = Instruction.Generic.create size opcode mnemonic in
  ginstr, block


let empty_instruction =
  let block = Dba.Instr.stop (Some Dba.KO) |> Dhunk.singleton in
  let opcode = "" in
  let mnemonic = Mnemonic.unsupported () in
  let size = 4 in
  let ginstr = Instruction.Generic.create size opcode mnemonic in
  ginstr, block


type error_type =
  | ESize
  | EParser
  | EMnemonic


let dummy_parse ?(etype=EParser) s =
  let lexbuf = Lexing.from_string s in
  match Parser.decoder_base Lexer.token lexbuf |> dummy_instruction with
  | i -> Error (etype, i)
  | exception Failure _ -> Error (EMnemonic, empty_instruction)

let parse_result s =
  Arm_options.Logger.debug ~level:1 "@[<v 0>Parsing %s@]" s;
  let open Lexing in
  let lexbuf = from_string s in
  try
    let i = Parser.decoder_msg Lexer.token lexbuf |> mk_instruction in
    Ok i
  with
  | Errors.Mismatched_instruction_size _  ->
    dummy_parse ~etype:ESize s
  | Failure _ ->
    dummy_parse s
  | Parser.Error  ->
    let pos = lexeme_start_p lexbuf in
    Arm_options.Logger.error
      "@[<v 0>Probable parse error at line %d, column %d@ \
       Lexeme was: %s@ \
       Entry was: %s@ \
       Getting basic infos only ... \
       @]"
      pos.pos_lnum pos.pos_cnum (Lexing.lexeme lexbuf) s;
    dummy_parse s


let decode_arm addr bytes =
  Arm32dba.decode ~thumb:false ~addr bytes |> parse_result

let decode_from_reader_arm addr reader =
  if addr mod 4 <> 0 then Error (ESize, empty_instruction)
  else
    match Lreader.Peek.u32 reader with
    | exception _ ->
        Error (ESize, empty_instruction)
    | bytes -> decode_arm (Int32.of_int addr) (Int32.of_int bytes)

let merge_itblock addr itblock =
  let n = Array.length itblock in
  let sizes = Array.make (n + 1) 0 in
  for i = 1 to n do
    sizes.(i) <- sizes.(i - 1) + Dhunk.length itblock.(i - 1)
  done;
  let inner j i = i + sizes.(j) in
  let outer =
    let rec iter i map =
      if i = n then map
      else
        iter (i + 1)
          (Dba_types.Caddress.Map.add
             (Dba_types.Caddress.block_start_of_int (addr + (2 * i)))
             (Dba.Jump_target.inner sizes.(i))
             map)
    in
    let map = iter 1 Dba_types.Caddress.Map.empty in
    fun j -> function
      | Dba.JInner goto -> Dba.Jump_target.inner (inner j goto)
      | Dba.JOuter caddr as jo -> (
          try Dba_types.Caddress.Map.find caddr map with Not_found -> jo)
  in
  let open Dhunk in
  let j = ref 0 in
  init sizes.(n) (fun i ->
      if i >= sizes.(!j + 1) then incr j;
      Dba_types.Instruction.reloc ~outer:(outer !j) ~inner:(inner !j)
        (Utils.unsafe_get_opt (inst itblock.(!j) (i - sizes.(!j)))))

let pp_opcode ppf bv =
  for i = 0 to (Bitvector.size_of bv / 8) - 1 do
    Format.fprintf ppf "%02x"
      (Z.to_int
         Bitvector.(
           value_of (extract bv { Interval.lo = 8 * i; hi = (8 * (i + 1)) - 1 })))
  done

let itstate w i =
  if i = 0 then 0 else w land 0xe0 lor ((w lsl (i - 1)) land 0x1f)

let isitw w =
  let w = w land 0xffff in
  0xbf00 < w && w <= 0xbfff

let decode_thumb itstate addr bytes =
  Arm32dba.decode ~thumb:true ~itstate ~addr bytes |> parse_result

let decode_from_reader_thumb addr reader =
  let addr =
    if addr mod 2 = 1 then (
      Lreader.rewind reader 1;
      addr - 1)
    else addr
  in
  match try Lreader.Peek.u32 reader with _ -> Lreader.Peek.u16 reader with
  | exception _ ->
      Error (ESize, empty_instruction)
  | bytes when not @@ isitw bytes ->
      decode_thumb 0 (Int32.of_int addr) (Int32.of_int bytes)
  | word -> (
      (* it block *)
      let n =
        1
        +
        if word land 0x01 <> 0 then 4
        else if word land 0x02 <> 0 then 3
        else if word land 0x04 <> 0 then 2
        else if word land 0x08 <> 0 then 1
        else assert false
      in
      let itblock = Array.make n (fst empty_instruction)
      and ithunks = Array.make n (snd empty_instruction) in
      let rec init i offset =
        if i = n then offset
        else
          match
            try Lreader.Peek.peek reader 4
            with _ -> Lreader.Peek.peek reader 2
          with
          | exception _ -> raise @@ Failure ""
          | bytes -> (
              match
                decode_thumb (itstate word i)
                  (Int32.of_int (addr + offset))
                  (Int32.of_int (Bitvector.to_int bytes))
              with
              | Error _ -> raise @@ Failure ""
              | Ok (instr, dhunk) ->
                  itblock.(i) <- instr;
                  ithunks.(i) <- dhunk;
                  let size = Size.Byte.to_int instr.Instruction.Generic.size in
                  Lreader.advance reader size;
                  init (i + 1) (offset + size))
      in
      try
        let size = init 0 0 in
        Lreader.rewind reader size;
        let bytes = Lreader.Peek.peek reader size in
        let opcode = Format.asprintf "%a" pp_opcode bytes in
        let mnemonic =
          Mnemonic.supported itblock (fun ppf it ->
              Array.iteri
                (fun i g ->
                  if i <> 0 then Format.pp_print_string ppf "; ";
                  Instruction.Generic.pp_mnemonic ppf g)
                it)
        in
        Ok
          ( Instruction.Generic.create size opcode mnemonic,
            merge_itblock addr ithunks )
      with Failure _ -> Error (ESize, empty_instruction))

let unwrap_result = function Error (_, i) -> i | Ok x -> x

let decode_from_reader addr reader =
  match Arm_options.SupportedMode.get () with
  | Arm_options.Arm -> decode_from_reader_arm addr reader |> unwrap_result
  | Arm_options.Thumb -> decode_from_reader_thumb addr reader |> unwrap_result


let decode reader (addr:Virtual_address.t) =
  let res = decode_from_reader (addr:>int) reader in
  Arm_options.Logger.debug ~level:3 "@[%a@]" show_stats ();
  res
;;

let cached_decode reader =
  let h = Virtual_address.Htbl.create 7 in
  fun (addr:Virtual_address.t) ->
  match Virtual_address.Htbl.find h addr with
  | res -> res
  | exception Not_found ->
     let res = decode reader addr in
     Virtual_address.Htbl.add h addr res;
     res
;;
