(*
    ------------------------------------------------------------
    Helix single controller YATES module that calls the YATES wrapper
    from the Helix repo to simulate Helix's behaivour and perform TE.

    NOTE: This module improves simulation process time, however, it is
        partially deprecated. Use HelixMC with a switch-to-controller
        mapping that assigns all switches to a single local controller
        to collect single controller results !
    ------------------------------------------------------------
*)
open Core

open Apsp
open Util
open Yates_types.Types

open Yojson
open Yojson.Basic.Util
open Helix_Helper


(* Flag that specifies if the module outputs debug info *)
let debug_output = false

(* Previous routing scheme *)
let prev_scheme = ref SrcDstMap.empty

(* Over utilised links *)
let over_util_links = ref []


(* Poll timer to simulate stats collection *)
let poll_timer = ref (-20)

(* Optimisation lock timer simulates processing and installation time *)
let te_opti_timer = ref 0
let te_opti_lock = ref false

(* Host and link statistics map *)
let host_traffic = ref SrcDstMap.empty
let link_traffic = ref EdgeMap.empty

(* Stats counters which are destroyed every iteration *)
let te_opti_success = ref 0
let te_opti_fail = ref 0


(* Initiate the TE optimisation variables to default values. This should be called
 * before starting a simulation for the algorithm. Output the algorithm attributes
 * for debugging purposes.
 *)
let init_te_optimisation unit : unit =
    (* XXX: Subtract 1 from the poll interval to ensure running every nth iteration! *)
    poll_timer := (!Globals.helix_poll_wait - 1);

    Printf.printf "\n\nTE Thresh: %f | TE Opti Wait: %d | Poll Wait: %d\n"
        !Globals.helix_te_thresh !Globals.helix_te_opti_wait !Globals.helix_poll_wait;
    Printf.printf "Recompute scheme for each iteration: %s\n"
        (Bool.to_string !Globals.helix_recomp_scheme);
    ()

(* Get the currently computed routing scheme *)
let get_scheme unit : scheme =
    !prev_scheme


(* Return the current routing scheme (initial scheme) to allow computing the path change churn
 * metric adjustment value. Behaves similar to ``get_scheme``, however, if `recomp_scheme`
 * is true (recompute the routing scheme for every iteration) returns an empty routing scheme to
 * account for complete recomputation of every paths (i.e. we start out with an empty routing scheme.
 *
 * NOTE: For the other YATES algorithms (which recompute paths every matrix row, TM churn does not
 * account for paths being re-installed (if they are the same) just for differences. Based on this
 * we assume that if the path is the same, there is some mechanism which sees this and does not
 * re-install the paths. For the Helix algorithms, this method will force including all paths
 * as we assume a clean slate every iteration.
 *)
let get_tm_churn_start_scheme unit : scheme =
    if !Globals.helix_recomp_scheme = true then
        SrcDstMap.empty
    else
        !prev_scheme

(* Solve the routing scheme by generating the required paths for the topology. Method calls the
 * Helix controller wrapper script that computes the required paths, which are then installed.
 *
 * Args:
 *  topo (topology): Topology to generate routing scheme
 *  _ (demands) : Current demand TE matrix to use for generating routing scheme
 *
 * Returns:
 *  scheme: Routing scheme for the current topo
 *)
let solve (topo:topology) (dem:demands) : scheme =
    (* If recomp for every iteration is false and we already have a scheme do not recompute *)
    if (SrcDstMap.is_empty !prev_scheme) = false && !Globals.helix_recomp_scheme = false then
        !prev_scheme
    else
    begin
        if debug_output then
            Printf.printf "Computing topology by calling Helix wrapper\n";
        serialize_topo topo "topo.json";

        (* Call the Helix wrapper and tell it to generate topology paths *)
        let topo_path = Filename.realpath "topo.json" in
        let data = call_helix_wrapper "yates_wrapper.py" ("topo" ^
            " --topo " ^ topo_path) in
        let json = Yojson.Basic.from_string data in

        (* Process the returned paths, update the routing scheme and return the new paths *)
        let scheme = json_path_to_scheme topo json in
        prev_scheme := scheme;
        scheme
    end


(* Initialize the modules routing scheme *)
let initialize (s:scheme) : unit =
  prev_scheme := s;
  ()


(* Check if a link is congested (over the TE threshold). Method simulates
 * polling for stats by delaying congestion check for the specified poll
 * wait interval (n-iterations). Every iteration, the current edge usage
 * is added to the poll interval total traffic map. Every statistics poll
 * interval, the total is averaged and the method checks for congestion.
 *
 * Args:
 *  topo (topology): Current topology
 *  e (edge): Current link to check if congested or store usage info
 *  traf ((edge array * int * float) list): Amount of traffic on edge/link (bits)
 *  c (float): Capacity of the link bits per seconds
 *)
let check_congestion (topo:topology) (e:edge) (traf:(edge array * int * float) list) (c:float) : unit =
    (* Go through the traffic and compute total *)
    let total_traf = List.fold traf ~init:(0.0) ~f:(fun acc (_,_,d) -> acc +. d) in

    (* Update the link traffic with the current TX rates *)
    let link_tx = match EdgeMap.find !link_traffic e with
        | None -> total_traf
        | Some tx -> (tx +. total_traf) in
    link_traffic := EdgeMap.set !link_traffic ~key:(e) ~data:(link_tx);

    if debug_output then
        Printf.printf "%s Traf: %f | Total Traf: %f | Capacity: %f\n"
            (string_of_edge topo e) total_traf link_tx c;

    (* Check if the poll interval has elapsed (check stats for congestion) *)
    if !poll_timer <= 0 then begin
        let avg_tx = link_tx /. (Int.to_float !Globals.helix_poll_wait) in
        let usage = avg_tx /. c in

        if debug_output then
            Printf.printf "\tPoll interval | AVG TX: %f | Usage %f\n"
                    avg_tx usage;

        if usage > !Globals.helix_te_thresh then begin
            let src,src_p = Topology.edge_src e in
            let dst,dst_p = Topology.edge_dst e in
    
            if debug_output then
                Printf.printf "Over util link: %s-%s %f\n"
                    (Node.name (Topology.vertex_to_label topo src))
                    (Node.name (Topology.vertex_to_label topo dst))
                    (usage);
    
            (* Add the details of the link to the list of over-utilised ports *)
            over_util_links := !over_util_links @ [(src, src_p, usage)];
        end;
    end;
    ()


(* Check if the module can perform TE optimisation, no TE optimisation in
 * progress and congested list of links is not empty
 *
 * Returns:
 *  bool: True if module can perform TE optimisation, false otherwise
 *)
let can_optimise unit : bool =
    ((!te_opti_lock = false) && (not (List.is_empty !over_util_links)))


(* Get the current TE optimisation failure and success count. Calling method
 * will reset the success/failure conouters so only call once per iteration!
 *
 * Returns:
 *  int * int * HelixCountStatsMap: success count, failure count, per controller counts
 *)
let get_stats unit : int * int * (helix_count_stats HelixCountStatsMap.t) =
    let old_success = !te_opti_success in
    let old_fail = !te_opti_fail in

    te_opti_success := 0;
    te_opti_fail := 0;

    old_success, old_fail, HelixCountStatsMap.empty


(* Fix all congested links in the topology. If the module detected congested
 * ports the method calls the Helix wrapper to perform TE optimisation and
 * update the routing scheme. If wrapper returns a routing scheme update queue
 * the update and return once the wait interval has elapsed (simulate latency).
 * Method returns an empty routing scheme if no path changes available.
 *
 * Note: Method also ticks the statics poll interval as this method should be
 *  called once every iteration.
 *
 * Args:
 *  topo (topology): Topology of the network
 *  dem (demands): Current host demands (send matrix) on topology
 *
 * Returns:
 *  scheme: Updated routing scheme or empty scheme if no change occured.
 *)
let resolve_congestion (topo:topology) (dem:demands) : scheme =
    (* Add the current host demand to the list *)
    SrcDstMap.iteri dem
        ~f:(fun ~key:(src,dst) ~data:(d) ->
            let traff = match (SrcDstMap.find !host_traffic (src, dst)) with
                | None -> d
                | Some x -> (x +. d) in
            host_traffic := SrcDstMap.set !host_traffic ~key:(src, dst) ~data:(traff);
        );

    (* Tick the poll timer *)
    let poll_reset = if !poll_timer <= 0 then begin
        (* Poll timer has expired, queue a congestion check process *)
        poll_timer := (!Globals.helix_poll_wait - 1);
        true
    end else begin
        (* Decrement timer, did not retrieve stats *)
        poll_timer := !poll_timer - 1;
        false
    end in

    (* Are we on a poll stats interval ? *)
    if poll_reset then begin
        (* Dump the current average link traffic and average host traffic *)
        if debug_output then begin
            Printf.printf "AVERAGE TRAFFIC:\n";
            EdgeMap.iteri !link_traffic
            ~f:(fun ~key:(e) ~data:(total_traf) ->
                let avg_traf = total_traf /. (Int.to_float !Globals.helix_poll_wait) in
                Printf.printf "\t[L] %s AVG TX: %f\n" (string_of_edge topo e) avg_traf;
            );

            SrcDstMap.iteri !host_traffic
            ~f:(fun ~key:(src,dst) ~data:(total_traf) ->
                let avg_traf = total_traf /. (Int.to_float !Globals.helix_poll_wait) in
                Printf.printf "\t[H] %s-%s AVG TX: %f\n" (string_of_vertex topo src)
                    (string_of_vertex topo dst) avg_traf;
            );
        end;

        (* Do we need to optimise the network ? *)
        if can_optimise () then begin
            (* Average the TX rates of the topology per poll interval *)
            link_traffic := EdgeMap.map !link_traffic
            (fun link_tx : float ->
                link_tx /. (Int.to_float !Globals.helix_poll_wait)
            );

            (* Average the host demands of the topology per poll interval *)
            host_traffic := SrcDstMap.map !host_traffic
            (fun host_tx : float ->
                host_tx /. (Int.to_float !Globals.helix_poll_wait)
            );

            (* Serialize the over-util links, demands and link usage *)
            serialize_over_util_links topo !over_util_links "over_util.json";
            serialize_demand topo !host_traffic "demand.json";
            serialize_topo_traffic topo !link_traffic "topo_traffic.json";

            (* Reset the poll timers that need to be reset *)
            over_util_links := [];

            (* Call the Helix wrapper and tell it to generate topology paths *)
            let topo_path = Filename.realpath "topo.json" in
            let over_util_path = Filename.realpath "over_util.json" in
            let demand_path = Filename.realpath "demand.json" in
            let topo_traffic_path = Filename.realpath "topo_traffic.json" in

            let data = call_helix_wrapper "yates_wrapper.py" ("te" ^
                " --topo " ^ topo_path  ^
                " --flow_demand " ^ demand_path ^
                " --topo_traffic " ^ topo_traffic_path ^
                " --over_util " ^ over_util_path ^
                " --te_thresh " ^ (string_of_float !Globals.helix_te_thresh)) in
            let json = Yojson.Basic.from_string data in

            let opti_success = if (List.length (to_assoc json)) > 0 then
                true
            else
                false in

            (* Iterate through the source array *)
            List.iter (to_assoc json)
                (fun (src, src_d) ->
                    (* Iterate through the destination array *)
                    List.iter (to_assoc src_d)
                        (fun (dst, dst_d) ->
                            let pMap = ref PathMap.empty in
                            let prim_list = dst_d |> member "primary" |> to_list in
                            (*let sec_list = dst_d |> member "secondary" |> to_list in*)

                            let prim = json_list_to_path topo prim_list in
                            (*let sec = json_list_to_path topo sec_list in*)

                            pMap := PathMap.set !pMap ~key:(prim) ~data:(1.0);
                            (*pMap := PathMap.set !pMap ~key:(sec) ~data:(0.0);*)

                            (* Go through the path splices to add to the path map (path prob set) *)
                            (*List.iter (dst_d |> member "splice" |> to_list)
                                (fun sp ->
                                    let sp_path = json_list_to_path topo (sp |> to_list) in
                                    pMap := PathMap.set !pMap ~key:(sp_path) ~data:(0.0);
                                );
                            *)
                            (* Find the host src and destination vertex to update the routing scheme *)
                            let src_vert = match find_vertex_by_name topo src with 
                                | None -> failwith "Could not find host name vertex for path"
                                | Some x -> x
                            in
                            let dst_vert = match find_vertex_by_name topo dst with 
                                | None -> failwith "Could not find host name vertex for path"
                                | Some x -> x
                            in
                            prev_scheme := SrcDstMap.set !prev_scheme
                                                ~key:(src_vert, dst_vert)
                                                ~data:(!pMap);
                    );
            );

            (* If the optimisation was a success initiate the update wait and increment stats *)
            if opti_success then begin
                (* XXX: Subtract one from timer to ensure executing on nth iteration after update *)
                te_opti_timer := (!Globals.helix_te_opti_wait - 1);
                te_opti_lock := true;
                te_opti_success := !te_opti_success + 1;
            end else begin
                te_opti_fail := !te_opti_fail + 1;
            end

        end;

        (* Reset the poll variables (for the next poll interval) *)
        link_traffic := EdgeMap.empty;
        host_traffic := SrcDstMap.empty;
    end;

    (* Is there an optimisation that needs to be applied ? *)
    if !te_opti_lock then
        (* Check if we need to apply the optimisation *)
        if !te_opti_timer <= 0 then begin
            te_opti_timer := 0;
            te_opti_lock := false;
            !prev_scheme

        (* Otherwise just tick the optimisation timer *)
        end else begin
            te_opti_timer := !te_opti_timer - 1;
            SrcDstMap.empty
        end
    else
        (* No optimisation is avaible *)
        SrcDstMap.empty


(* Perform local recovery when failures occur in the network *)
let local_recovery = normalization_recovery
