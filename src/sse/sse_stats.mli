type t = {
  queries : int;
  queries_unsat : int;
  queries_sat : int;
  queries_err : int;
  enumerations : int;
  query_time : float;
  branches_explored : int;
  instructions : int;
  merged_paths : int;
  refused_mergers : int;
  aborted_mergers : int;
  start_time : float;
}

val empty: t

val add_query : float -> unit
val add_sat_check : Formula.status -> unit
val add_enumeration : unit -> unit
val add_instruction : unit -> unit
val add_branch : unit -> unit
val add_merged_path : unit -> unit
val add_refused_merger : unit -> unit
val add_aborted_merger : unit -> unit
val set_start : unit -> unit

val get : unit -> t

val pp : Format.formatter -> t -> unit
val pp_csv : with_header:bool -> Format.formatter -> string * t -> unit

