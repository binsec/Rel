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

include Cli.Options(
struct
  let name = "arm"
  let shortname = name
end
)

type supported_mode = Thumb | Arm

module SupportedMode = Builder.Variant_choice (struct
  type t = supported_mode

  let name = "supported-mode"

  let default = Arm

  let doc =
    "Can be used to decode thumb instructions or arm instructions \
     (default: arm)."

  let to_string = function Thumb -> "thumb" | Arm -> "arm"

  let of_string = function
    | "thumb" -> Thumb
    | "arm" -> Arm
    | x ->
        raise
          (Invalid_argument
             (x
            ^ " is not a valid arm decoding mode. Expected one of both, thumb \
               or arm."))

  let choices = [ "thumb"; "arm" ]
end)
