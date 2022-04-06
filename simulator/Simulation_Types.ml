open Core
open Yates_routing
open Yates_types.Types

(* State used during simulation of a single TM *)
type network_iter_state = {
  ingress_link_traffic  : ((edge Array.t * int * float) list) EdgeMap.t;
  delivered       : float SrcDstMap.t;
  latency         : (float LatencyMap.t) SrcDstMap.t;
  tm_churn        : float;
  utilization     : (float list) EdgeMap.t;
  scheme          : scheme;
  failures        : failure;
  failure_drop    : float;
  congestion_drop : float;
  real_tm         : demands;
  predict_tm      : demands;
}

(* Statistics returned after simulating each TM *)
type sim_tm_stats = {
  throughput    : throughput SrcDstMap.t;
  latency       : throughput LatencyMap.t;
  congestion    : (congestion * congestion) EdgeMap.t;
  failure_drop  : throughput;
  congestion_drop   : throughput;
  tm_churn          : float;
  flash_throughput  : throughput;
  aggregate_demand  : demand;
  recovery_churn    : float;
  scheme            : scheme;
  solver_time       : float;
  te_opti_success   : int;
  te_opti_fail      : int;
  te_opti_mctrl     : helix_count_stats HelixCountStatsMap.t;
  ing_change_mctrl  : helix_count_stats HelixCountStatsMap.t;
}
