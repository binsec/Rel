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

(** {Definition of command-line & programmatic options for RelSE} *)

include Cli.S

(** { Verbose options }  *)

module PrintModel : Cli.BOOLEAN
module StatFile : Cli.STRING_OPT
module StatPrefix : Cli.STRING
module LowDecl : Cli.BOOLEAN

(** { Timeouts }  *)

module Timeout : Cli.INTEGER
module MaxPaths : Cli.INTEGER

(** { Optimizations } *)
type fault_packing =
  | Instr
  | Block
  | Never
module FaultPacking : Cli.GENERIC with type t = fault_packing
module Dedup : Cli.INTEGER
module Untainting : Cli.BOOLEAN
module Canonical : Cli.BOOLEAN

type store_type =
  | Sse
  | SelfComposed
  | Shadow
module SymbolicStore : Cli.GENERIC with type t = store_type

type memory_type =
  | MemStd
  | MemList
  | MemMap
module MemoryType : Cli.GENERIC with type t = memory_type


(** { Specification options } *)                    

module HighBytes : Cli.INTEGER_SET
module HighSymbols : Cli.STRING_SET
(* module CriticalFunction : Cli.STRING *)

type property =
  | CT             (** Check for constant-time *)
  | SecretErasure  (** Check for safe erasure *)


module Property : Cli.GENERIC with type t = property

type leak_info =
  | HaltLeak    (** Halts at first leak *)
  | InstrLeak   (** Report leaky instructions (instructions are
                    reported only once) *)
  | UniqueLeaks (** Repors unique expressions that are leaked along a
                    path *)

module LeakInfo : Cli.GENERIC with type t = leak_info
