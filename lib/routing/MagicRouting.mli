open Yates_types.Types

val initialize : scheme -> unit

val solve : topology -> demands -> scheme

val local_recovery : scheme -> topology -> failure -> demands -> scheme
