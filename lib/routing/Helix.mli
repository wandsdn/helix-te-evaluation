open Yates_types.Types

val init_te_optimisation : unit -> unit

val solve : topology -> demands -> scheme

val initialize : scheme -> unit

val local_recovery : scheme -> topology -> failure -> demands -> scheme

val check_congestion : topology -> edge -> (edge array * int * float) list -> float -> unit

val resolve_congestion : topology -> demands -> scheme

val can_optimise : unit -> bool

val get_stats : unit -> (int * int * (helix_count_stats HelixCountStatsMap.t))

val get_scheme : unit -> scheme

val get_tm_churn_start_scheme : unit -> scheme
