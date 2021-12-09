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

open Kernel_options

let _ =
  let len = Array.length Sys.argv in
  let rec loop i =
    if i < len then
      if Sys.argv.(i) = "-config" then
        let config_filename = Sys.argv.(i + 1) in
        Kernel_options.Config_file.set config_filename
      else loop (i + 1)
  in loop 0

let read_configuration_file () =
  match Kernel_options.Config_file.get_opt () with
  | None -> () (* Maybe_TODO : Use default file ? *)
  | Some filename ->
     ignore @@ Cli.parse_configuration_file ~filename


let binary_descr () =
  if Describe_binary.get () && ExecFile.is_set () then
     Logger.result "@\n%a"
      Kernel_functions.Loader.pp_loader_summary (ExecFile.get ())

let version () =
  if Version.get () then begin
      Logger.set_log_level "result";
      Format.printf "Binsec version %%VERSION%%";
      exit 0
    end
;;

let _ =
  Cli.Boot.enlist ~name:"binary description" ~f:binary_descr;
  Cli.Boot.enlist ~name:"version description" ~f:version;
;;
