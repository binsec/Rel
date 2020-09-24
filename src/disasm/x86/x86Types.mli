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

(* The X86 machine language *)

type simd_size = S32 | S64 | S128

type xmm_mm = XMM | MM

type xmm_pos = Left | Right

type mode = [ `M32 | `M16 ]

type sizeMode = [ mode | `M8 ]

type address_size_mode = A16 | A32

type rep = NoRep | Rep | RepE | RepNE

(** {6 Register sets} *)

(** General-purpose 8-bit registers *)
type reg8 =
  | AL
  | CL
  | DL
  | BL
  | AH
  | CH
  | DH
  | BH

(** General-purpose 16-bit registers *)
type reg16 =
  | AX
  | CX
  | DX
  | BX
  | SP
  | BP
  | SI
  | DI

(** General-purpose 32-bit registers *)
type reg32 =
  | EAX
  | ECX
  | EDX
  | EBX
  | ESP
  | EBP
  | ESI
  | EDI

(** Segment registers *)
type segment_reg =
  | ES
  | CS
  | SS
  | DS
  | FS
  | GS

(** Floating-point registers *)
type float_reg =
  | ST0
  | ST1
  | ST2
  | ST3
  | ST4
  | ST5
  | ST6
  | ST7

(** MMX registers *)
type mmx_reg =
  | MM0
  | MM1
  | MM2
  | MM3
  | MM4
  | MM5
  | MM6
  | MM7

(** XMM registers *)
type xmm_reg =
  | XMM0
  | XMM1
  | XMM2
  | XMM3
  | XMM4
  | XMM5
  | XMM6
  | XMM7

(** Control registers *)
type control_reg =
  | CR0
  | CR2
  | CR3
  | CR4

(** Debug registers *)
type debug_reg =
  | DR0
  | DR1
  | DR2
  | DR3
  | DR6
  | DR7

(** Test registers *)
type test_reg =
  | TR3
  | TR4
  | TR5
  | TR6
  | TR7

(** {6 Flags} *)

(** {6 Condition codes} *)

(** Flags *) (* signification des flags? *)
type flag =
  | ID
  | VIP
  | VIF
  | AC
  | VM
  | RF
  | NT
  | IOPL
  | OF   (* Overflow: Overflow in signed calculation*)
  | DF   (* Direction: sprecifies the direction of string manipulations *)
  | IF
  | TF
  | SF   (* Sign: The highest bit is 1 or 0 *)
  | ZF   (* Zero: the result is zero *)
  | AF   (* Adjust/Auxilary: overflow or under flow on the 8 lower bits, used in
            BCD calculation *)
  | PF   (* Parity: number of set bits in lower byte is even or odd *)
  | CF   (* Carry: Overflow in unsigned calculation *)

(** Basic conditions *)
type condition =
  | O
  | B
  | Z
  | BE
  | S
  | P
  | L
  | LE


type cc = {
  truth_value : bool;
  condition : condition;
}
(** A condition code is a pair of a basic condition and a boolean indicating
    whether that condition is true. *)

(** SSE tests *)
type sse =
  | SseEQ
  | SseLT
  | SseLE
  | SseUNORD
  | SseNEQ
  | SseNLT
  | SseNLE
  | SseORD

(** {6 Addresses} *)

(** Scales for integer operations *)
type scale =
  | Scale1
  | Scale2
  | Scale4
  | Scale8

(** The memory address format supported by the machine language *)
type address = {
  addrMode : address_size_mode;          (** Address size attribute *)
  addrDisp : int64;                      (** Constant displacement *)
  addrBase : reg32 option;               (** Optional base register *)
  addrIndex : (scale * reg32) option     (** Optional index register, along
                                              with a scaling factor by which to
                                              multiply it *)
}

(** {6 Operands} *)

(** Generic instruction operands, indexed by the relevant register set *)
type 'a genop =
  | Imm of int64       (** A constant machine integer -no immediate values of more than 64 bits- *)
  | Reg of 'a          (** A register *)
  | Address of address (** A memory dereference *)

type genopxmm = xmm_reg genop
type genop32 = reg32 genop
type genop16 = reg16 genop
type genop8 = reg8 genop

(** Specializations to particular register sets *)


(** {6 Operations} *)

(** Arithmetic operations *)
type arith_op =
  | Add
  | Adc
  | And
  | Or
  | Xor
  | Sub
  | Sbb

(** Bitwise shift operations *)
type shift_op =
  | Shl
  | Shr
  | Sar

type shiftd_op =
  | Shld
  | Shrd


(** Rotate operations *)
type rotate_op =
  | Rol
  | Ror
  | Rcl
  | Rcr

type ('a, 'b) ar2 = {
    mode : sizeMode;
    dst : 'a;
    src : 'b;
}

(** Standard x86 instruction set *)
type instruction_kind =
  | Arith of sizeMode * arith_op * genop32 * genop32
  | Call of int64
  | DCall of genop32
  | Cmp of sizeMode * genop32 * genop32
  | CmpXchg of sizeMode * genop32 * genop32
  | CmpXchg8b of xmm_mm * simd_size * genopxmm
  | Test of sizeMode * genop32 * genop32
  | Inc of sizeMode * genop32
  | Dec of sizeMode * genop32
  | Jcc of cc * int64
  | Jcxz of mode * int64
  | Jmp of int64
  | DJmp of genop32
  | Lea of sizeMode * reg32 * address
  | Leave
  | CMovcc of sizeMode * cc * genop32 * genop32
  | CBW of mode
  | CWD of mode
  | Cmps of sizeMode
  | Mov of sizeMode * genop32 * genop32
  | MovSegRight of genop16 * segment_reg
  | MovSegLeft of segment_reg * genop16
  | Movzx of mode * reg32 * genop8
  | Movzx16 of mode * reg32 * genop16
  | Movsx of mode * reg32 * genop8
  | Movsx16 of mode * reg32 * genop16
  | Xadd of sizeMode * genop32 * genop32
  | Movs of sizeMode
  | Lods of sizeMode
  | Stos of sizeMode
  | Scas of sizeMode
  | Bt  of (genop32, genop32) ar2
  | Bts of (genop32, genop32) ar2
  | Btr of (genop32, genop32) ar2
  | Btc of (genop32, genop32) ar2
  | Nop
  | Not of sizeMode * genop32
  | Neg of sizeMode * genop32
  | Pop of sizeMode * genop32
  | PopS of segment_reg
  | PopA of mode
  | Push of sizeMode * genop32
  | PushS of segment_reg
  | PushA of mode
  | Pushfd of mode
  | Popfd of mode
  | Bswap of mode * reg32
  | Bsr of mode * reg32 * genop32
  | Bsf of mode * reg32 * genop32
  | Ret
  | Reti of int
  | Retf
  | Retfi of int
  | Shift of sizeMode * shift_op * genop32 * genop8
  | Rotate of sizeMode * rotate_op * genop32 * genop8
  | Shiftd of sizeMode * shiftd_op * genop32 * genop32 * genop8
  | SetCc of cc * genop8
  | Halt
  | Clc
  | Stc
  | Cld
  | Std
  | Cmc
  | Xchg of sizeMode * genop32 * genop32
  | Mul of sizeMode * genop32
  | IMul of sizeMode * genop32
  | IMul2 of sizeMode * genop32 * genop32
  | IMul3 of sizeMode * genop32 * genop32 * genop32
  | Div of sizeMode * genop32
  | IDiv of sizeMode * genop32
  | Unsupported of string
  | Bad
  | Loopnz of mode * address_size_mode * int64
  | Loopz of mode * address_size_mode * int64
  | Loop of mode * address_size_mode * int64
  | Pshufw of xmm_mm * simd_size * xmm_reg * genopxmm * int
  | Pshuflw of xmm_mm * simd_size * xmm_reg * genopxmm * int
  | Pshufhw of xmm_mm * simd_size * xmm_reg * genopxmm * int
  | Pshufd of xmm_mm * simd_size * xmm_reg * genopxmm * int
  | Movaps of simd_size * genopxmm * genopxmm
  | Movlpd of simd_size * genopxmm * genopxmm
  | Movhpd of simd_size * genopxmm * genopxmm
  | Movlps of simd_size * genopxmm * genopxmm
  | Movhps of simd_size * genopxmm * genopxmm
  | Movhlps of simd_size * genopxmm * genopxmm
  | Movlhps of simd_size * genopxmm * genopxmm
  | Movsldup of simd_size * genopxmm * genopxmm
  | Movshdup of simd_size * genopxmm * genopxmm
  | Movddup of simd_size * genopxmm * genopxmm
  | Movntq of xmm_mm * simd_size * genopxmm * genopxmm
  | Movd of xmm_mm * xmm_pos * genopxmm * genop32
  | MovQ of xmm_mm * simd_size * genopxmm * genopxmm
  | MovdQA of xmm_mm * simd_size * genopxmm * genopxmm
  | MovdQU of xmm_mm * simd_size * genopxmm * genopxmm
  | Palignr of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Pcmpeqb of xmm_mm * simd_size * genopxmm * genopxmm
  | Pcmpeqw of xmm_mm * simd_size * genopxmm * genopxmm
  | Pcmpeqd of xmm_mm * simd_size * genopxmm * genopxmm
  | Pcmpgtb of xmm_mm * simd_size * genopxmm * genopxmm
  | Pcmpgtw of xmm_mm * simd_size * genopxmm * genopxmm
  | Pcmpgtd of xmm_mm * simd_size * genopxmm * genopxmm
  | PmovMSKB of xmm_mm * simd_size * genop32 * genopxmm
  | Pminu of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Pmins of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Pxor of xmm_mm * simd_size * genopxmm * genopxmm
  | Por of xmm_mm * simd_size * genopxmm * genopxmm
  | Pand of xmm_mm * simd_size * genopxmm * genopxmm
  | Pandn of xmm_mm * simd_size * genopxmm * genopxmm
  | Pmaxu of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Pmaxs of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Punpckl of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Punpckh of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Packus of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Packss of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Pmaddwd of xmm_mm * simd_size * genopxmm * genopxmm
  | Padd of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Padds of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Paddus of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Psub of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Psubs of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Psubus of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Pmulhw of xmm_mm * simd_size * genopxmm * genopxmm
  | Pmullw of xmm_mm * simd_size * genopxmm * genopxmm
  | Psrl of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Psll of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Psra of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Psrldq of genopxmm * int
  | Pslldq of genopxmm * int
  | Pclmulqdq of xmm_mm * simd_size * genopxmm * genopxmm * int
  | Ptest of xmm_mm * simd_size * genopxmm * genopxmm
  | Movups of genopxmm * genopxmm
  | Movupd of genopxmm * genopxmm
  | Xlat of address_size_mode
  | Aas
  | Aam of int
  | Aad of int
  | Lsl of sizeMode * genop32 * genop32
  | Fld
  | Fxch of float_reg
  | Lahf
  | Sahf
  | Salc
  | Wait
  | Emms
  | Popcnt of sizeMode * genop32 * genop32
  | Lzcnt of sizeMode * genop32 * genop32
  | Prefetch of string
