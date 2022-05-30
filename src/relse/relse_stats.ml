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

module AL = Relse_utils.AddressList

(* Status *)
(* TODO: I should change this by a module status *)
type status = Insecure | Max_Depth | Max_Paths | Solver_Timeout | Timeout | EnumLimit
let max_depth =      0b00001
and max_paths =      0b00010
and solver_timeout = 0b00100
and timeout =        0b01000
and enum_limit =     0b10000

(* Exit code *)
let exit_secure = 0
and exit_error = 1
and exit_insecure = 7
and exit_unknown = 8

type query_type = Exploration | Control_Insecurity | Memory_Insecurity | Terminal_Insecurity | Insecurity | Model | Enum

type query_record = {
  total : int;
  sat   : int;
  unsat : int;
  err   : int;
  time  : float ;
}

let empty_query_record = {
  total = 0;
  sat = 0;
  unsat = 0;
  err = 0;
  time = 0.0;
}

let print_query_record_csv fmt qr =
  Format.fprintf fmt "%d,%d,%d,%d,%f,"
    qr.sat
    qr.unsat
    qr.err
    qr.total
    qr.time

let print_query_record fmt label qr =
  Format.fprintf fmt "%s: @[<v 2>@ \
                      SAT:\t%d@ UNSAT:\t%d@ Other:\t%d@ Total:\t%d@ Time:\t%f@ Time avg:\t%f@]@\n"
    label
    qr.sat
    qr.unsat
    qr.err
    qr.total
    qr.time
    (qr.time /. (float_of_int qr.total))

let get_solver () =
  Formula_options.(
    match Solver.get () with
    | Boolector -> "boolector"
    | Z3 -> "z3"
    | CVC4 -> "cvc4"
    | Yices -> "yices")
    
let get_store () =
  match SymbolicStore.get () with
  | Sse -> "sse"
  | SelfComposed -> "self-comp"
  | Shadow -> "shadow"

let get_mem () =
  match MemoryType.get () with
  | MemStd -> "std"
  | MemList -> "row-list"
  | MemMap -> "row-map"

type t = {
  exploration_queries : query_record ref;
  control_insecurity_queries : query_record ref;
  memory_insecurity_queries : query_record ref;
  terminal_insecurity_queries : query_record ref;
  insecurity_queries : query_record ref;
  model_queries : query_record ref;
  enum_queries : query_record ref;
  
  insecurity_checks : int * int;

  total_query_size : int;
  max_query_size : int;
  
  paths : int;
  conditional_instructions : int;
  forks : int;
  instructions : int;
  dba_instructions : int;
  
  start_time : float;
  nb_violations : int;
  status : int;

  exploration_query_addr : AL.t;
  insecure_query_addr : AL.t;
  secure_query_addr : AL.t;
  timeout_query_addr : AL.t;
}

let empty = {
  exploration_queries = ref empty_query_record;
  control_insecurity_queries = ref empty_query_record;
  memory_insecurity_queries = ref empty_query_record;
  terminal_insecurity_queries = ref empty_query_record;
  insecurity_queries = ref empty_query_record;
  model_queries = ref empty_query_record;
  enum_queries = ref empty_query_record;
  
  insecurity_checks = (0,0);

  total_query_size = 0;
  max_query_size = 0;
  
  paths = 0;
  conditional_instructions = 0;
  forks = 0;
  instructions = 0;
  dba_instructions = 0;
  
  start_time = Unix.gettimeofday();
  nb_violations = 0;
  status = 0;

  exploration_query_addr = AL.create ~name:"explor";
  insecure_query_addr = AL.create ~name:"insecure";
  secure_query_addr = AL.create ~name:"secure";
  timeout_query_addr = AL.create ~name:"timeout";
}

let stat = ref empty

let get_total () =
  let exploration_queries = !(!stat.exploration_queries)
  and insecurity_queries = !(!stat.insecurity_queries)
  and model_queries = !(!stat.model_queries)
  and enum_queries = !(!stat.enum_queries) in
  let total = exploration_queries.total + insecurity_queries.total + model_queries.total + enum_queries.total
  and sat = exploration_queries.sat + insecurity_queries.sat + model_queries.sat + enum_queries.sat
  and unsat = exploration_queries.unsat + insecurity_queries.unsat + model_queries.unsat + enum_queries.unsat
  and err = exploration_queries.err + insecurity_queries.err + model_queries.err + enum_queries.err
  and time = exploration_queries.time +. insecurity_queries.time +. model_queries.time +. enum_queries.time
  in { total; sat; unsat; err; time }

let get_insecurity_addresses () =
  !stat.insecure_query_addr

let get_exit_code () =
  if !stat.nb_violations > 0
  then exit_insecure
  else if !stat.status = 0
  then exit_secure
  else exit_unknown

let get_status () =
  (if (!stat.status land max_depth) = 0 then "0" else "1") ^
  (if (!stat.status land max_paths) = 0 then "0" else "1") ^
  (if (!stat.status land solver_timeout) = 0 then "0" else "1") ^
  (if (!stat.status land timeout) = 0 then "0" else "1")

let print_exit_code () =
  let exit_code = get_exit_code () in
  if exit_code = exit_secure then "Secure" else
  if exit_code = exit_insecure then "Insecure" else
  if exit_code = exit_error then "Error"
  else "Unknown"

let print_status () =
  let str =
    (if (!stat.status land max_depth) <> 0 then "_max depth reached_" else "") ^
    (if (!stat.status land max_paths) <> 0 then "_max paths reached_" else "") ^
    (if (!stat.status land solver_timeout) <> 0 then "_solver timeout_" else "") ^
    (if (!stat.status land timeout) <> 0 then "_timeout of RelSE_" else "") ^
    (if (!stat.status land enum_limit) <> 0 then "_enum limit reached_" else "")
  in
  if str = "" then "None" else str
    
let set_status status =
  match status with
  | Insecure ->
    let nb_violations = !stat.nb_violations + 1 in
    stat := { !stat with nb_violations }
  | Max_Depth ->
    let status = !stat.status lor max_depth in
    stat := { !stat with status }
  | Max_Paths ->
    let status = !stat.status lor max_paths in
    stat := { !stat with status }
  | Solver_Timeout ->
    let status = !stat.status lor solver_timeout in
    stat := { !stat with status }
  | Timeout ->
    let status = !stat.status lor timeout in
    stat := { !stat with status }
  | EnumLimit ->
    let status = !stat.status lor enum_limit in
    stat := { !stat with status }

let add_query_size sz =
  let max_query_size =
    if sz > !stat.max_query_size
    then sz
    else !stat.max_query_size
  and total_query_size = sz + !stat.total_query_size
  in stat := { !stat with max_query_size ; total_query_size }

let update_status result addr query_type =
  let update_insec_query () =
    match result with
    | Formula.SAT ->
      set_status Insecure;
      let insecure_query_addr = AL.extend addr !stat.insecure_query_addr in
      stat := { !stat with insecure_query_addr }
    | Formula.UNSAT ->
      let secure_query_addr = AL.extend addr !stat.secure_query_addr in
      stat := { !stat with secure_query_addr }
    | Formula.TIMEOUT | Formula.UNKNOWN ->
      set_status Solver_Timeout;
      let timeout_query_addr = AL.extend addr !stat.timeout_query_addr in
      stat := { !stat with timeout_query_addr }
  in
  let update_exploration_query () =
    let exploration_query_addr = AL.extend addr !stat.exploration_query_addr in
    stat := { !stat with exploration_query_addr }
  in
  match query_type with
  | Exploration -> update_exploration_query ()
  | Control_Insecurity -> update_insec_query ()
  | Memory_Insecurity -> update_insec_query ()
  | Terminal_Insecurity -> update_insec_query ()
  | Insecurity -> update_insec_query ()
  | Model -> ()
  | Enum -> update_exploration_query ()
  
let add_query time result query_type =
  let update_queries queries =
    queries := {
      unsat = !queries.unsat + if result = Formula.UNSAT then 1 else 0;
      sat   = !queries.sat + if result = Formula.SAT then 1 else 0;
      err   = !queries.err + if result <> Formula.SAT && result <> Formula.UNSAT then 1 else 0;
      total = !queries.total + 1;
      time = !queries.time +. time;
    }
  in
  match query_type with
  | Exploration ->
    update_queries !stat.exploration_queries;
  | Control_Insecurity ->
    update_queries !stat.control_insecurity_queries;
    update_queries !stat.insecurity_queries
  | Memory_Insecurity ->
    update_queries !stat.memory_insecurity_queries;
    update_queries !stat.insecurity_queries
  | Terminal_Insecurity ->
    update_queries !stat.terminal_insecurity_queries;
    update_queries !stat.insecurity_queries
  | Insecurity -> update_queries !stat.insecurity_queries
  | Model -> update_queries !stat.model_queries
  | Enum ->
    update_queries !stat.enum_queries

let add_done_check () =
  let checked = fst !stat.insecurity_checks + 1
  and spared = snd !stat.insecurity_checks in
  stat := { !stat with insecurity_checks = (checked, spared) }
                
let add_spared_check () =
  let checked = fst !stat.insecurity_checks
  and spared = snd !stat.insecurity_checks + 1 in
  stat := { !stat with insecurity_checks = (checked, spared) }

(* let avg_insecurity_length () =
 *   let n = fst !stat.insecurity_length
 *   and sum = snd !stat.insecurity_length in
 *   if n <> 0
 *   then (float_of_int sum) /. (float_of_int n)
 *   else 0.0 *)

let add_path () =
  stat := { !stat with paths = !stat.paths + 1 }

let add_conditional () =
  stat := { !stat with conditional_instructions = !stat.conditional_instructions + 1 }

let add_fork () =
  stat := { !stat with forks = !stat.forks + 1 }

let add_instruction () =
  stat := { !stat with instructions = !stat.instructions + 1 }

let add_dba_instruction () =
  stat := { !stat with dba_instructions = !stat.dba_instructions + 1 }

let set_start () =
  stat := { !stat with start_time = Unix.gettimeofday() }

let get_time () =
  (Unix.gettimeofday() -. !stat.start_time)
  
let get () = !stat

let pp fmt stat =
  let total_qr = get_total () in
  Format.fprintf fmt "RelSE stats:@[<v 2>@\n";
   print_query_record fmt "Total queries" (total_qr);
  print_query_record fmt "Exploration queries" !(stat.exploration_queries); 
  print_query_record fmt "CF Insecurity queries" !(stat.control_insecurity_queries);
  print_query_record fmt "Mem Insecurity queries" !(stat.memory_insecurity_queries);
  print_query_record fmt "Term Insecurity queries" !(stat.terminal_insecurity_queries);
  print_query_record fmt "Insecurity queries" !(stat.insecurity_queries);
  print_query_record fmt "Model queries" !(stat.model_queries);
  print_query_record fmt "Enum queries" !(stat.enum_queries);
  print_query_record fmt "Total queries" (total_qr);
  Format.fprintf fmt "Query size avg/max:\t%f / %d@ \
                      Checks done/spared:\t%d / %d@ \
                      Coverage: @[<v 2>@ Paths:\t\t%d@ Conditionals:\t%d@ Forks:\t%d@ DBA Instructions:\t%d@ x86 Instructions:\t%d@]@ \
                      Violations:\t\t%d@ \
                      Status:\t%s@ \
                      Result:\t%s@ \
                      Elapsed time:\t%f\n \
                      @]"
    (float_of_int stat.total_query_size /. (float_of_int total_qr.total))
    stat.max_query_size
    (fst stat.insecurity_checks)
    (snd stat.insecurity_checks)
    stat.paths
    stat.conditional_instructions
    stat.forks
    stat.dba_instructions
    stat.instructions
    stat.nb_violations
    (print_status ())
    (print_exit_code ())
    (get_time ())

let pp_csv ~with_header ~label ~fp ~dd ~untainting fmt stat =
  let total_qr = get_total () in
  if with_header then
    Format.fprintf fmt "label,fp,dd,untainting,\
                        Explor SAT,Explor UNSAT,Explor other,Explor total,\
                        Explor time,CF SAT,CF UNSAT,CF other,CF total,CF \
                        time,Mem SAT,Mem UNSAT,Mem other,Mem total,Mem \
                        time,Term SAT,Term UNSAT,Term other,Term total,Term \
                        time,Insec SAT,Insec UNSAT,Insec other,Insec total,\
                        Insec time,Model total,Model time,Enum total,Enum \
                        time,Total SAT,Total UNSAT,Total other,Total total,\
                        Total time,average query size,max query size,done \
                        checks,spared checks,paths,conditions,forks,\
                        dba_instructions,x86instructions,wall time,\
                        violations,status,exit code,MAX_DEPTH,MAX_PATH,\
                        TIMEOUT,store,canonical,mem_type,solver,bopt\n";
  Format.fprintf fmt "%s,%s,%d,%d," label fp dd untainting;
  print_query_record_csv fmt !(stat.exploration_queries); 
  print_query_record_csv fmt !(stat.control_insecurity_queries);
  print_query_record_csv fmt !(stat.memory_insecurity_queries);
  print_query_record_csv fmt !(stat.terminal_insecurity_queries);
  print_query_record_csv fmt !(stat.insecurity_queries);
  Format.fprintf fmt "%d,%f,%d,%f,"
  !(stat.model_queries).total
  !(stat.model_queries).time
  !(stat.enum_queries).total
  !(stat.enum_queries).time;
  print_query_record_csv fmt (total_qr);
  Format.fprintf fmt "%f,%d,%d,%d,%d,%d,%d,%d,%d,%f,%d,%s,%d,%d,%d,%f,%s,%d,%s,%s,%d\n"
    (float_of_int stat.total_query_size /. (float_of_int total_qr.total))
    stat.max_query_size
    (fst stat.insecurity_checks)
    (snd stat.insecurity_checks)
    stat.paths
    stat.conditional_instructions
    stat.forks
    stat.dba_instructions
    stat.instructions
    (get_time ())
    stat.nb_violations
    (get_status ())
    (get_exit_code ())
    (Sse_options.MaxDepth.get ())
    (Relse_options.MaxPaths.get ())
    (Sse_options.Timeout.get ())
    (get_store ())
    (if Relse_options.Canonical.get () then 1 else 0)
    (get_mem ())
    (get_solver ())
    (if Formula_options.OptimAll.get () then 1 else 0)

(* Save address trace *)
let addresses_to_file stat =
  AL.pp_address_trace_to_file stat.exploration_query_addr 0;
  AL.pp_address_trace_to_file stat.insecure_query_addr 0;
  AL.pp_address_trace_to_file stat.secure_query_addr 0;
  AL.pp_address_trace_to_file stat.timeout_query_addr 0

(* Converts fault-packing option to string *)
let fault_packing_to_string () =
  match Relse_options.FaultPacking.get () with
  | Relse_options.Instr -> "Instr"
  | Relse_options.Block -> "Block"
  | Relse_options.Never -> "Never"

(** Print the smt statistics at the end of the execution*)
let print_stats () =
  let stats = get() in
  addresses_to_file stats;
  if Relse_options.StatFile.is_set() then
    let open Unix in
    let fname = Relse_options.StatFile.get() in
    let with_header, file =
      try
        Logger.debug ~level:4 "open %s mode x" fname;
        true, openfile fname [O_WRONLY; O_CREAT; O_EXCL] 0o644
      with Unix_error (EEXIST, _, _) ->
        Logger.debug ~level:4 "open %s mode a" fname;
        false, openfile fname [O_WRONLY; O_APPEND] 0o644
    in
    let out = out_channel_of_descr file in
    let fmt = Format.formatter_of_out_channel out in
    (* Options *)
    let fp = fault_packing_to_string ()
    and dd = Relse_options.Dedup.get()
    and untainting = (if Untainting.get () then 1 else 0)
    and label = Relse_options.StatPrefix.get() in
    Format.fprintf
      fmt
      "%a"
      (pp_csv ~with_header ~label ~fp ~dd ~untainting) stats;
    Format.pp_print_flush fmt ();
    close_out out
  else
    Logger.info "%a" pp stats
