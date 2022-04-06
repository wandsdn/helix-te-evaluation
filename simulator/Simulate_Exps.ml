(* Experiments that we want to run with the Simulator *)
open Yates_types.Types

type iter_vs_time = { iteration : int ; time : float; time_dev : float; }

let iter_vs_time_to_string (r:iter_vs_time) : string =
  Printf.sprintf "%d\t%f\t%f" r.iteration r.time r.time_dev

type iter_vs_congestion = { iteration : int ; congestion : float; congestion_dev : float; }

let iter_vs_congestion_to_string (r:iter_vs_congestion) : string =
  Printf.sprintf "%d\t%f\t%f" r.iteration r.congestion r.congestion_dev

type iter_vs_churn = { iteration : int ; churn : float; churn_dev : float; }

let iter_vs_churn_to_string (r:iter_vs_churn) : string =
  Printf.sprintf "%d\t%f\t%f" r.iteration r.churn r.churn_dev

type iter_vs_num_paths = { iteration : int ; num_paths : float; num_paths_dev : float; }

let iter_vs_num_paths_to_string (r:iter_vs_num_paths) : string =
  Printf.sprintf "%d\t%f\t%f" r.iteration r.num_paths r.num_paths_dev

type iter_vs_throughput = { iteration : int ; throughput : float; throughput_dev : float; }

let iter_vs_throughput_to_string (r:iter_vs_throughput) : string =
  Printf.sprintf "%d\t%f\t%f" r.iteration r.throughput r.throughput_dev

type iter_vs_edge_congestions = { iteration : int ; edge_congestions : float EdgeMap.t; }

let iter_vs_edge_congestions_to_string (topo:topology) (r:iter_vs_edge_congestions) : string =
  Printf.sprintf "%d\t" r.iteration ^
  EdgeMap.fold ~init:"" ~f:(fun ~key:e ~data:c acc -> acc ^ "\n\t\t" ^ "(" ^
    (Node.name (Net.Topology.vertex_to_label topo (fst (Net.Topology.edge_src e)))) ^ "," ^
    (Node.name (Net.Topology.vertex_to_label topo (fst (Net.Topology.edge_dst e)))) ^ ") : " ^
    string_of_float c) r.edge_congestions

type iter_vs_latency_percentiles = { iteration : int ; latency_percentiles : float LatencyMap.t; }

let iter_vs_latency_percentiles_to_string (r:iter_vs_latency_percentiles) : string =
  Printf.sprintf "%d\t" r.iteration ^
  LatencyMap.fold r.latency_percentiles
    ~init:""
    ~f:(fun ~key:latency ~data:percentile acc ->
      acc ^ "\n\t\t" ^ string_of_float latency ^ " : " ^ string_of_float percentile)

(* Helix TE optimisation success and failure totals *)
type iter_vs_te_opti_count = { iteration: int ; te_opti_success : int; te_opti_fail : int; }

let iter_vs_te_opti_count_to_string (r:iter_vs_te_opti_count) : string =
  Printf.sprintf "%d\t%d\t%d" r.iteration r.te_opti_success r.te_opti_fail

(* Helix Multi-Controller TE optimisation success and failures *)
type iter_vs_mctrl_te_opti_count = { iteration: int ;
            te_opti: helix_count_stats HelixCountStatsMap.t }

let iter_vs_mctrl_te_opti_count_to_string (r:iter_vs_mctrl_te_opti_count) : string =
  Printf.sprintf "%d\t" r.iteration ^
  HelixCountStatsMap.fold r.te_opti ~init:("") ~f:(fun ~key:(cid) ~data:(c) acc ->
    acc ^ (Printf.sprintf "\n\t\t(%s) : %d\t%d" cid c.success c.failure)
  )

(* Helix multi-controller ingress change success and failure *)
type iter_vs_mctrl_ing_change_count = { iteration: int ;
            ing_change: helix_count_stats HelixCountStatsMap.t }

let iter_vs_mctrl_ing_change_count_to_string (r:iter_vs_mctrl_ing_change_count) : string =
  Printf.sprintf "%d\t" r.iteration ^
  HelixCountStatsMap.fold r.ing_change ~init:("") ~f:(fun ~key:(cid) ~data:(c) acc ->
    acc ^ (Printf.sprintf "\n\t\t(%s) : %d\t%d" cid c.success c.failure)
  )
