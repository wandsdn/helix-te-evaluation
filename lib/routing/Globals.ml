open Core
(**************************************************************************)
(* Global variables to store configuration *)
(**************************************************************************)

(* Network *)
let budget = ref Int.max_value
let deloop = ref false
let er_mode = ref false
let nbins : int option ref = ref None

(* Simulator tweaks *)
let tm_sim_iters = ref 1000
let rand_seed : int option ref = ref None
let gurobi_method = ref Int.minus_one

(* Failures and recovery *)
let failure_time = ref Int.max_value
let local_recovery_delay  = ref Int.max_value
let global_recovery_delay = ref Int.max_value
let flash_recover = ref false

(* Routing algorithm specific *)
let ffc_max_link_failures = ref 1

(* Helix specific parameters (only applies to Helix algorithm) *)
let helix_te_thresh = ref 0.0                   (* TE Threshold *)
let helix_te_opti_wait = ref 0                  (* Wait n-iterations before applying update *)
let helix_poll_wait = ref 0                     (* Poll interval (n-iterations) *)
let helix_recomp_scheme = ref false             (* Clear original TE changes for each run *)
let helix_sw_ctrl_map_file = ref ""             (* Switch controller mapping for HelixMC *)
let helix_te_opti_method = ref ""               (* Helix TE algorithm to use *)
let helix_te_candidate_sort_rev = ref true      (* Reverse candidate sort order *)
let helix_te_pot_path_sort_rev = ref false      (* Reverse potential paths sort order *)
let helix_te_paccept = ref false                (* Accept partial solutions *)
