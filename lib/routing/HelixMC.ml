(*
    ------------------------------------------------------------
    Helix multi-controller YATES module that calls the wrappers and
    emulates Helix's behavior. The module acts as a coordinator that
    maintains system state.
    ------------------------------------------------------------
    
    PREFACE NOTE:
        Local controller TE updates adhere to a delay mechanism that simulates
        latency in updates. Root controller TE updates use the same mechanism.

        Local controllers immediately notify the root controller of inter-domain
        instruction modifications caused by a TE update.

        Other domains are notified of ingress changes on merge of the path
        segments which occur after applying the update. This enforce ingress
        change detection to occur when traffic uses the new path.

        Root path modifications take precedence over local TE updates, any paths
        re-computed via a root instruction update removes any local controller
        path changes that need to be applied later.
*)
open Core

open Apsp
open Util
open Yates_types.Types

open Yojson
open Yojson.Basic.Util
open Helix_Helper
open HelixMC_Helper


(* Should we output debug information *)
let debug_output = false

(* Map switches, hosts and inter-domain links to local controllers *)
let sw_ctrl_map = ref NodeCtrlMap.empty
let host_ctrl_map = ref NodeCtrlMap.empty
let inter_dom_links = ref IDLMap.empty

(* Previous routing scheme for the entire topology *)
let prev_scheme = ref SrcDstMap.empty

(* Root controller update information *)
let root_te_opti_timer = ref 0
let root_te_opti_lock = ref false

let root_te_scheme_update = ref SrcDstMap.empty
let root_te_idi_update = ref ""

(* Stats counters which are destroyed every iteration *)
let root_te_opti_success = ref 0
let root_te_opti_fail = ref 0

(* Stats counter for inter-domain ingress/egress change notification of root controller *)
let root_idi_change_success = ref 0
let root_idi_change_fail = ref 0


(* Inform the root controller that inter-domain instructions were modified by a local
 * controller instance (local controller change of inter-domain path)
 *)
let root_idi_local_change (cid:string) (topo:topology) (json:Yojson.Basic.json): unit =
    if debug_output then
        Printf.printf "Notify root of local inter-dom path change on %s\n" cid;

    (* Serialize the inter-domain instructions to send to root ctrl *)
    let buf = Buffer.create 400 in
    Printf.bprintf buf "{\"%s\": %s}" cid (Yojson.Basic.to_string json);
    let oc = Out_channel.create ("root.interdominst.json") ~append:false in
    fprintf oc "%s\n" (Buffer.contents buf);
    Out_channel.close oc;

    (* Call the root controller Helix wrapper to notify of the change *)
    serialize_topo topo "topo.all.json";
    let topo_path = Filename.realpath "topo.all.json" in
    let idp_inst_path = Filename.realpath "root.interdominst.json" in
    let data = call_helix_wrapper "yates_root_wrapper.py" ("ing_egg_change" ^
        " --sw_ctrl_map " ^ !Globals.helix_sw_ctrl_map_file ^
        " --path_inst " ^ idp_inst_path ^
        " --topo " ^ topo_path) in

    if data = "\n{}" then begin
        root_idi_change_success := !root_idi_change_success + 1;
        if debug_output then
            Printf.printf "Completed root controller inter-domain path change operation\n"
    end else begin
        root_idi_change_fail := !root_idi_change_fail + 1;
        if debug_output then
            Printf.printf "Strange result returned from inter-domain path change operation (%s)\n" data;
    end;
    ()

(* Module that stores local controller information *)
module CtrlInfo : CtrlInfo = struct
    type t = {
        (* ID of the controller *)
        cid : string;
        (* List triple for over-utilised links (vertex, port, usage) *)
        mutable over_util_links : (vertex * int32 * float) list;
        (* Traffic from hosts for controller *)
        mutable host_traffic : demands;
        (* Link traffic observed for poll interval *)
        mutable link_traffic : demand EdgeMap.t;

        (* Last poll interval average observed traffic for inter-domain links
         * NOTE: this field only updates every poll interval expiry, any other time
         * contains old poll interval-value
         *)
        mutable avg_inter_dom_link_traffic : demand EdgeMap.t;

        (* Number of TE optimisation successes *)
        mutable te_opti_success: int;
        (* Number of TE optimisation failures *)
        mutable te_opti_fail : int;
        (* Number of ing changes requests that were successful (resulted in idi mod) *)
        mutable ing_change_success : int;
        (* Number of ing change requests that failed *)
        mutable ing_change_fail : int;

        (* Link traffic poll timer, counts down from `Globals.helix_poll_wait` *)
        mutable poll_timer : int;
        (* TE update delay timer, counts down from `Globals.helix_te_opti_wait` *)
        mutable te_opti_timer : int;

        (* Is there a TE optimisation in progress (waiting for updated to be applied) *)
        mutable te_opti_lock : bool;
        (* Computed routing scheme for the controller *)
        mutable prev_scheme : scheme;
        (* TE update scheme that needs to be applied after `te_opti_timer` expires *)
        mutable te_scheme_update : scheme;
        (* Failed inter-domain link TE optimisation information *)
        mutable failed_opti_idl : string;

        (* Local controller inter-domain instruction list *)
        mutable inter_dom_inst : IDI.t list HKeyMap.t;
    }

    let default = {
        cid = "";
        over_util_links = [];
        host_traffic = SrcDstMap.empty;
        link_traffic = EdgeMap.empty;
        avg_inter_dom_link_traffic = EdgeMap.empty;
        te_opti_success = 0;
        te_opti_fail = 0;
        ing_change_success = 0;
        ing_change_fail = 0;
        poll_timer = -20;
        te_opti_timer = 0;
        te_opti_lock = false;
        prev_scheme = SrcDstMap.empty;
        te_scheme_update = SrcDstMap.empty;
        failed_opti_idl = "[]";
        inter_dom_inst = HKeyMap.empty;
    }

    (* Initiate a new controller object with default values *)
    let create (cid:string) : t =
        { default with cid = cid; }

    (* Get the current controller routing scheme *)
    let get_scheme (t:t) : scheme =
        t.prev_scheme

    (* Get the current failed inter-area TE opti links as a JSON string.
     * NOTE: Calling this method will clear the variable ('{}').
     *)
    let get_failed_opti_idl (t:t) : string = 
        let data = t.failed_opti_idl in
        t.failed_opti_idl <- "[]";
        data

    (* Reset the poll timer to the default tick value *)
    let reset_poll_timer (t:t) : unit =
        t.poll_timer <- (!Globals.helix_poll_wait - 1)

    (* Dump controller info to console *)
    let dump (t:t) : unit =
        Printf.printf "<Controller> CID %s, Con-Links: %d, Poll-Time: %d (%d fail TE | %d succ TE)\n"
                t.cid (List.length t.over_util_links) t.poll_timer t.te_opti_fail t.te_opti_success

    (* Return the current inter-domain instruction map *)
    let get_inter_dom_inst (t:t) : (IDI.t list HKeyMap.t) = 
        t.inter_dom_inst

    (* Update the inter-domain instructions of local controller from JSON
     * object. If the first instruction in the JSON object contains a delete
     * action, all instructions associated with that src-dest pair are removed
     * from the map. If the action is not delete overwrite the current
     * instruction list with the JSON object list for that src-dest pair.
     *)
    let update_inter_dom_inst (t:t) (json:Yojson.Basic.json) : unit = 
        t.inter_dom_inst <- List.fold (json |> to_list)
            ~init:(t.inter_dom_inst)
            ~f:(fun acc (obj) ->
                let src = obj |> member "keysrc" |> to_string in
                let dst = obj |> member "keydst" |> to_string in
                let insts = obj |> member "instructions" in

                let action = (List.nth_exn (insts |> to_list) 0) |> member "action" |> to_string in
                if action = "delete" then begin
                    if debug_output then
                        Printf.printf "Inter-dom inst is delete %s-%s\n" src dst;

                    if HKeyMap.mem acc (src, dst) then
                        HKeyMap.remove acc (src, dst)
                    else
                        acc
                end else begin
                    let new_instruction_list = deserialize_idi_list insts in
                    (* Output the instructions we are updating with
                    List.iter new_instruction_list
                        ~f:(fun inst ->
                            Printf.printf "# [%s-%s] -> %s\n" src dst (IDI.to_str inst);
                        );
                    *)
                    HKeyMap.set acc ~key:(src, dst) ~data:(new_instruction_list)
                end
            )

    (* Serialize and return as a JSON string the current inter-domain
     * instructions of the local controller. The instruction map is encoded as
     * a list of objects with two fields for the keys representing the src-dest
     * pair of the inter-domain (area) instructions.
     *)
    let get_serialized_inter_dom_inst (t:t) : string =
        (* Create a buffer to use for serialization and output start bracket *)
        let buf = Buffer.create 400 in
        Printf.bprintf buf "[";
        let first = ref true in

        (* Iterate through the inter-dom instructions and serialize to JSON *)
        HKeyMap.iteri t.inter_dom_inst
            ~f:(fun ~key:(src, dst) ~data:(inst_list) ->
                if !first then
                    first := false
                else
                    Printf.bprintf buf ", ";

                Printf.bprintf buf "{\"keysrc\": \"%s\", " src;
                Printf.bprintf buf "\"keydst\": \"%s\", " dst;
                Printf.bprintf buf "\"instructions\": [";
                let first_inst = ref true in
                List.iter inst_list
                    ~f:(fun inst ->
                        if !first_inst then
                            first_inst := false
                        else
                            Printf.bprintf buf ", ";
                        Printf.bprintf buf "%s" (IDI.to_json inst);
                    );
                Printf.bprintf buf "]}";
            );

        (* Close the main bracket and return JSON string *)
        Printf.bprintf buf "]";
        Buffer.contents buf

    (* Serialize the current inter-domain instructions of the local controller
     * to a JSON file `out_file`. Method calls
     * `CtrlInfo.get_serialized_inter_dom_inst` to retrieve JSON string of
     * inter-domain instructions for the controller.
     *)
    let serialize_old_inter_dom_inst (t:t) (out_file:string) : unit =
        let oc = Out_channel.create (out_file) ~append:false in
        fprintf oc "%s\n" (get_serialized_inter_dom_inst t);
        Out_channel.close oc

    (* Print current local controller inter-domain instructions to console *)
    let dump_old_inter_dom_inst (t:t) : unit =
        Printf.printf "CID %s\n" t.cid;
        HKeyMap.iteri t.inter_dom_inst
            ~f:(fun ~key:(src, dst) ~data:(insts) ->
                Printf.printf " * (%s - %s)\n" src dst;
                List.iter insts
                    ~f:(fun inst ->
                        Printf.printf "\t%s\n" (IDI.to_str inst);
                    )
            )

    (* Compute the local intra-domain paths for a controller *)
    let compute_paths (t:t) (topo:topology) (dem:demands) : scheme =
        if (SrcDstMap.is_empty t.prev_scheme) = false && !Globals.helix_recomp_scheme = false then
            (* Return the old scheme if not recomputing for every iteration and have old paths *)
            t.prev_scheme
        else begin
            (* Serialize the local controller topology and inter-domain link information *)
            let ctrl_topo = get_cid_topo topo t.cid !sw_ctrl_map !host_ctrl_map !inter_dom_links in
            let topo_fname = t.cid ^ ".topo.json" in
            let inter_dom_fname = t.cid ^ ".interdom.json" in
            serialize_topo ctrl_topo topo_fname;
            serialize_inter_dom_links topo t.cid !inter_dom_links inter_dom_fname;

            (* Call the Helix wrapper to compute the local intra-domain paths *)
            let topo_path = Filename.realpath topo_fname in
            let inter_dom_path = Filename.realpath inter_dom_fname in
            let data = call_helix_wrapper "yates_mctrl_wrapper.py" ("topo" ^
                " --topo " ^ topo_path ^
                " --inter_dom_links " ^ inter_dom_path ^
                " --cid " ^ t.cid) in
            let json = Yojson.Basic.from_string data in
            let scheme = json_path_to_scheme topo json in

            (* Save the computed path and return the result *)
            t.prev_scheme <- scheme;
            scheme
        end

    (* Compute paths from inter-domain path instructions received from the root controller *)
    let compute_inter_dom_paths (t:t) (topo:topology) (inst:Yojson.Basic.json) : scheme =
        (* Serialize the controller topology and write the path instructions to a JSON *)
        let ctrl_topo = get_cid_topo topo t.cid !sw_ctrl_map !host_ctrl_map !inter_dom_links in
        let topo_fname = t.cid ^ ".topo.json" in
        let inter_dom_fname = t.cid ^ ".interdom.json" in
        let inter_dom_inst_fname = t.cid ^ ".interdominst.json" in
        serialize_topo ctrl_topo topo_fname;
        serialize_inter_dom_links topo t.cid !inter_dom_links inter_dom_fname;
        serialize_json_inter_dom_inst inst inter_dom_inst_fname;

        (* Call the Helix wrapper and tell it to generate the required paths *)
        let topo_path = Filename.realpath topo_fname in
        let inter_dom_path = Filename.realpath inter_dom_fname in
        let inter_dom_inst_path = Filename.realpath inter_dom_inst_fname in
        let data = call_helix_wrapper "yates_mctrl_wrapper.py" ("inter-dom" ^
            " --topo " ^ topo_path ^
            " --inter_dom_inst " ^ inter_dom_inst_path ^
            " --inter_dom_links " ^ inter_dom_path ^
            " --cid " ^ t.cid) in
        let json = Yojson.Basic.from_string data in

        (* Process the paths from the wrapper and update the local routing scheme *)
        let scheme = json_path_to_scheme topo json in
        if debug_output then
            Printf.printf "\nCOMP PATH SCHEME %s DUMP:\n^^%s^^\n" t.cid (dump_scheme topo scheme);

        t.prev_scheme <- (scheme_merge scheme t.prev_scheme);

        (* If a local TE optimisation is in progress (to be applied) remove paths re-computed by
         * the root controller. Prevents overwriting root updates when local update is later applied
         *)
        if ((SrcDstMap.is_empty scheme) = false) && (t.te_opti_lock = true) then begin
            let new_te_scheme_update = SrcDstMap.fold t.te_scheme_update
                ~init:(SrcDstMap.empty)
                ~f:(fun ~key:(src,dst) ~data:(pmap) acc ->
                    (* If hkey covered by root updte do not add it to new scheme update list *)
                    if SrcDstMap.mem scheme (src, dst) then
                        acc
                    else
                        SrcDstMap.set ~key:(src, dst) ~data:(pmap) acc;
                ) in

            if (SrcDstMap.is_empty new_te_scheme_update) then begin
                (* There is no local TE update so we should clear the process *)
                t.te_opti_timer <- 0;
                t.te_opti_lock <- false;
                t.te_scheme_update  <- SrcDstMap.empty;
            end else
                t.te_scheme_update <- new_te_scheme_update;
        end;

        (* Update the old inter-domain instruction with the new instruction object and 
         * return the computed paths
         *)
        update_inter_dom_inst t inst;
        scheme

    (* Notify the controller that an ingress change occured (modify primary path)
     *
     * Args:
     *  topo (topology): Current topology
     *  src_v (vertex): Path key source part
     *  dst_v (vertex): Path key destination part
     *  sw (string): New ingress switch of path
     *  pn (int32): New ingress port on the switch
     *
     * Returns:
     *  path: New resulting path produced by the ingress change
     *)
    let ingress_change (t:t) (topo:topology) (src_v:vertex) (dst_v:vertex) (sw:string) (pn:int32) : path =
        let src = Node.name (Topology.vertex_to_label topo src_v) in
        let dst = Node.name (Topology.vertex_to_label topo dst_v) in

        (* Output information *)
        if debug_output then
            Printf.printf "Performing ingress change call (%s-%s) sw %s pn %ld\n" src dst sw pn;

        (* Call the Helix wrapper and tell it to perform an ingress change operation *)
        let ctrl_topo = get_cid_topo topo t.cid !sw_ctrl_map !host_ctrl_map !inter_dom_links in
        let topo_fname = t.cid ^ ".topo.json" in
        let inter_dom_fname = t.cid ^ ".interdom.json" in
        let inter_dom_inst_fname = t.cid ^ ".interdominst.json" in
        serialize_topo ctrl_topo topo_fname;
        serialize_inter_dom_links topo t.cid !inter_dom_links inter_dom_fname;
        serialize_old_inter_dom_inst t inter_dom_inst_fname;

        (* Call the Helix wrapper and tell it to generate topology paths *)
        let topo_path = Filename.realpath topo_fname in
        let inter_dom_path = Filename.realpath inter_dom_fname in
        let inter_dom_inst_path = Filename.realpath inter_dom_inst_fname in
        let data = call_helix_wrapper "yates_mctrl_wrapper.py" ("ing-change" ^
            " --topo " ^ topo_path ^
            " --inter_dom_links " ^ inter_dom_path ^
            " --inter_dom_inst " ^ inter_dom_inst_path ^
            " --ing_change_key_src " ^ src ^
            " --ing_change_key_dst " ^ dst ^
            " --ing_change_sw " ^ sw ^
            (Printf.sprintf " --ing_change_pn %ld" pn) ^
            " --cid " ^ t.cid) in
        let json = Yojson.Basic.from_string data in

        (* Process any inter-domain path instruction changes *)
        if debug_output then
            Printf.printf "Ingress Change: %s %s\n" t.cid data;

        (* Process any inter-domain path instruction changes *)
        let idp_change = json |> member "idp_change" in
        if (List.length (to_list idp_change)) > 0 then begin
            if debug_output then
                Printf.printf "Ingress change for controller %s\n" t.cid;

            t.ing_change_success <- t.ing_change_success + 1;
            update_inter_dom_inst t idp_change;
            root_idi_local_change t.cid topo idp_change;
        end else
            t.ing_change_fail <- t.ing_change_fail + 1;

        (* Process the ingress change path from the controller *)
        let path = json |> member "path" |> to_list in
        let path_mod = json_list_to_path topo path in

        if (List.is_empty path_mod) then
            get_path_from_map topo (SrcDstMap.find_exn t.prev_scheme (src_v, dst_v))
        else begin
            t.prev_scheme <- SrcDstMap.set ~key:(src_v, dst_v) ~data:(
                PathMap.set ~key:(path_mod) ~data:(1.0) PathMap.empty
            ) t.prev_scheme;
            path_mod
        end

    (* Can we perform congested link checks *)
    let can_check_congestion (t:t) : bool =
        (t.te_opti_lock = false)

    (* Can we perform TE optimisations *)
    let can_optimise (t:t) : bool =
        ((t.te_opti_lock = false) && (not (List.is_empty t.over_util_links)))

    (* Add ingress host traffic for a local controller *)
    let update_host_traffic (t:t) (topo:topology) (src:vertex) (dst:vertex) (d:float) : unit =
        let host_traf = match (SrcDstMap.find t.host_traffic (src, dst)) with
            | None -> d
            | Some x -> (x +. d) in

        if debug_output then
            Printf.printf "\t%s Updating host %s-%s traffic %f\n"
                t.cid (Node.name (Topology.vertex_to_label topo src))
                (Node.name (Topology.vertex_to_label topo dst)) host_traf;

        t.host_traffic <- SrcDstMap.set t.host_traffic ~key:(src, dst) ~data:(host_traf);
        ()

    (* Check if a link is over-utilised, see ``check_congestion``. *)
    let check_congestion (t:t) (topo:topology) (e:edge) (traf:float) (c:float) : unit =
        (* Update the link traffic with the current TX rates *)
        let link_tx = match EdgeMap.find t.link_traffic e with
            | None -> traf
            | Some tx -> (tx +. traf) in
        t.link_traffic <- EdgeMap.set t.link_traffic ~key:(e) ~data:(link_tx);

        if debug_output then
            Printf.printf "%s Traf: %f | Total Traf: %f | Capacity: %f\n"
                (string_of_edge topo e) traf link_tx c;

        (* If we are on a poll interval check if the link is congested (otherwise just add valuie for avg) *)
        if ((t.poll_timer <= 0) && (can_check_congestion t)) then begin
            let avg_tx = link_tx /. (Int.to_float !Globals.helix_poll_wait) in
            let usage = avg_tx /. c in
            if usage > !Globals.helix_te_thresh then begin
                let src,src_p = Topology.edge_src e in

                if debug_output then begin
                    let dst,dst_p = Topology.edge_dst e in
                    Printf.printf "Over util link: %s-%s %f\n"
                        (Node.name (Topology.vertex_to_label topo src))
                        (Node.name (Topology.vertex_to_label topo dst))
                        (usage);
                end;

                (* Add the details of the link to the list of over-utilised links *)
                t.over_util_links <- t.over_util_links @ [(src, src_p, usage)];
            end;
        end

    (* Resolve any local controller congestion in the topology. See ``resolve_congestion`` for details.
     *
     * NOTE: Method automatically updates the inter-domain path instruction map `inter_dom_path_inst`
     *       and notifies the root controller of any ingress/egress changes caused by the TE update.
     *       Inter-domain links that failed to be optimised are added to `t.te_failed_opti_idl` as a
     *       JSON string.
     *
     * Returns:
     *  scheme: TE path modifications when an update needs to be applied. Method uses a delay mechanism
     *      to simulate controller latency in decision making process.
     *)
    let resolve_congestion (t:t) (topo:topology) : scheme =
        (* Tick the poll timer *)
        let poll_reset = if t.poll_timer <= 0 then begin
            (* Poll timer has expired, queue a congestion check process *)
            t.poll_timer <- (!Globals.helix_poll_wait - 1);
            true
        end else begin
            (* Decrement timer, did not retrieve stats *)
            t.poll_timer <- t.poll_timer - 1;
            false
        end in

        (* Are we on a poll stats interval ? *)
        if poll_reset then begin
            (* Average the TX rates of the topology per poll interval *)
            t.link_traffic <- EdgeMap.map t.link_traffic
                (fun link_tx : float ->
                    link_tx /. (Int.to_float !Globals.helix_poll_wait)
                );

            (* Average the host traffic for the domain *)
            t.host_traffic <- SrcDstMap.map t.host_traffic
                (fun host_tx : float ->
                    host_tx /. (Int.to_float !Globals.helix_poll_wait)
                );

            (* Update average inter-domain link traffic from domain link traffic map *)
            t.avg_inter_dom_link_traffic <- EdgeMap.fold t.link_traffic
                ~init:(EdgeMap.empty)
                ~f:(fun ~key:(edge) ~data:(traf) acc ->
                    let src_v,src_pn = Topology.edge_src edge in
                    let src_name = Node.name (Topology.vertex_to_label topo src_v) in
                    if is_inter_dom_link src_name src_pn !inter_dom_links then
                        EdgeMap.set ~key:(edge) ~data:(traf) acc
                    else
                        acc
                );

            (* Dump the current average traffic information for the CID *)
            if debug_output then begin
                Printf.printf "%s AVERAGE TRAFFIC:\n" t.cid;
                EdgeMap.iteri t.link_traffic
                ~f:(fun ~key:(e) ~data:(avg_traf) ->
                    Printf.printf "\t[L] %s AVG TX: %f\n" (string_of_edge topo e) avg_traf;
                );

                EdgeMap.iteri t.avg_inter_dom_link_traffic
                ~f:(fun ~key:(e) ~data:(avg_traf) ->
                    Printf.printf "\t[IDL] %s AVG TX: %f\n" (string_of_edge topo e) avg_traf;
                );

                SrcDstMap.iteri t.host_traffic
                ~f:(fun ~key:(src,dst) ~data:(avg_traf) ->
                    Printf.printf "\t[H] %s-%s AVG TX: %f\n" (string_of_vertex topo src)
                        (string_of_vertex topo dst) avg_traf;
                );
            end;

            (* Output the average host traffic information if debug out is on *)
            if debug_output then
                SrcDstMap.iteri t.host_traffic
                    ~f:(fun ~key:(src,dst) ~data:(avg_traf) ->
                        let src_name = Node.name (Topology.vertex_to_label topo src) in
                        let dst_name = Node.name (Topology.vertex_to_label topo dst) in
                        Printf.printf "%s HK %s-%s avg traffic %f\n" t.cid src_name dst_name avg_traf;
                    );

            (* Do we need to optimise the network ? *)
            if can_optimise t then begin
                (* Output the congested ports list if debug out is on *)
                if debug_output then begin
                    Printf.printf "CID %s congested ports list:\n" t.cid;
                    List.iter t.over_util_links
                        ~f:(fun (src, src_p, usage) ->
                            Printf.printf "\t%s %ld %f\n"
                                (Node.name (Topology.vertex_to_label topo src))
                                src_p usage
                        )
                end;

                (* Serialize the over-util links, demands and link usage *)
                let topo_fname = t.cid ^ ".topo.json" in
                let over_util_fname = t.cid ^ ".over_util.json" in
                let demand_fname = t.cid ^ ".demand.json" in
                let usage_fname = t.cid ^ ".topo_traffic.json" in
                let inter_dom_fname = t.cid ^ ".interdom.json" in
                let inter_dom_inst_fname = t.cid ^ ".interdominst.json" in
                serialize_over_util_links topo t.over_util_links over_util_fname;
                serialize_demand topo t.host_traffic demand_fname;
                serialize_topo_traffic topo t.link_traffic usage_fname;
                serialize_inter_dom_links topo t.cid !inter_dom_links inter_dom_fname;
                serialize_old_inter_dom_inst t inter_dom_inst_fname;

                t.over_util_links <- [];

                (* Call the helix wrapper and tell it to generate topology paths *)
                let topo_path = Filename.realpath topo_fname in
                let over_util_path = Filename.realpath over_util_fname in
                let demand_path = Filename.realpath demand_fname in
                let topo_traffic_path = Filename.realpath usage_fname in
                let inter_dom_path = Filename.realpath inter_dom_fname in
                let inter_dom_inst_path = Filename.realpath inter_dom_inst_fname in
                let data = call_helix_wrapper "yates_mctrl_wrapper.py" ("te" ^
                    " --topo " ^ topo_path ^
                    " --flow_demand " ^ demand_path ^
                    " --topo_traffic " ^ topo_traffic_path ^
                    " --over_util " ^ over_util_path ^
                    " --te_thresh " ^ (string_of_float !Globals.helix_te_thresh) ^
                    " --te_opti_method " ^ !Globals.helix_te_opti_method ^
                    " --te_candidate_sort_rev " ^ (string_of_bool !Globals.helix_te_candidate_sort_rev) ^
                    " --te_pot_path_sort_rev " ^ (string_of_bool !Globals.helix_te_pot_path_sort_rev) ^
                    " --te_paccept " ^ (string_of_bool !Globals.helix_te_paccept) ^
                    " --inter_dom_links " ^ inter_dom_path ^
                    " --inter_dom_inst " ^ inter_dom_inst_path ^
                    " --cid " ^ t.cid) in
                let json =
                    try
                        Yojson.Basic.from_string data
                    with _ as e ->
                        begin
                            Printf.printf "--- EXCEPTION BAD JSON STRING ---:\n%s\n" data;
                            raise e
                        end
                    in

                let res = json |> member "res" in

                (* Process the failed inter-domain link optimisation field *)
                let failed_opti_idl = json |> member "failed_inter_dom_links" in
                let failed_opti_idl_str = (Yojson.Basic.to_string failed_opti_idl) in
                t.failed_opti_idl <- if failed_opti_idl_str = "[]" then
                        "[]"
                    else
                        failed_opti_idl_str;

                (* Process any inter-domain path instruction changes *)
                let idp_change = json |> member "idp_change" in
                if (List.length (to_list idp_change)) > 0 then begin
                    if debug_output then
                        Printf.printf "Potential egress change occurred for domain %s\n" t.cid;

                    update_inter_dom_inst t idp_change;
                    root_idi_local_change t.cid topo idp_change;
                end;

                (* Check if a port was optimised (success or partial success) *)
                let opti_success = if (List.length (to_assoc res)) > 0 then begin
                    if debug_output then
                        Printf.printf "Fixed congestion via local controller %s\n" t.cid;
                    true
                end else begin
                    if debug_output then
                        Printf.printf "Failed to fix congestion on local controller %s\n" t.cid;
                    false 
                end in

                t.te_scheme_update <- json_path_to_scheme topo res;

                (* If TE operation sucesful initiate update wait, increment stats and fix root instructions *)
                if opti_success then begin
                    (* XXX: Subtract one from timer to ensure executing on nth iteration after update *)
                    t.te_opti_timer <- (!Globals.helix_te_opti_wait - 1);
                    t.te_opti_lock <- true;
                    t.te_opti_success <- t.te_opti_success + 1;

                    if debug_output then begin
                        Printf.printf "LOCAL FIX SCHEME:\n%s\n" (dump_scheme topo t.te_scheme_update);
                        Printf.printf "TE opti timer: %d | Success/Fail: %d/%d\n"
                                        t.te_opti_timer t.te_opti_success t.te_opti_fail;
                        dump_old_inter_dom_inst t;
                    end;
                end else begin
                    t.te_opti_fail <- t.te_opti_fail + 1;
                end;
            end;

            (* Reset the poll variables (for the next poll interval) *)
            t.link_traffic <- EdgeMap.empty;
            t.host_traffic <- SrcDstMap.empty;
        end;

        (* Is there an optimisation that needs to be applied ? *)
        if t.te_opti_lock then
            (* Check if we need to apply the optimisation *)
            if t.te_opti_timer <= 0 then begin
                t.te_opti_timer <- 0;
                t.te_opti_lock <- false;
                t.prev_scheme <- (scheme_merge t.te_scheme_update t.prev_scheme);
                t.te_scheme_update

            (* Otherwise just tick the optimisation timer *)
            end else begin
                t.te_opti_timer <- t.te_opti_timer - 1;
                SrcDstMap.empty
            end
        else
            (* No optimisation is avaible *)
            SrcDstMap.empty

    (* Get the current te optimisation failure and success count.
     * This method will reset the counters so they should be called only after
     * every iteration !
     *
     * Returns:
     *  int * int: Success count, failure count
     *)
    let get_stats (t:t) : int * int =
        let old_success = t.te_opti_success in
        let old_fail = t.te_opti_fail in
        t.te_opti_success <- 0;
        t.te_opti_fail <- 0;
        old_success, old_fail

    (* Get the current ingress change request success and failure counts.
     * Method will reset the counters so it should be called once every
     * iteration (TE matrix row) !
     *
     * Returns:
     *  int * int: Success count, failure count
     *)
    let get_ing_change_stats (t:t) : int * int =
        let old_success = t.ing_change_success in
        let old_fail = t.ing_change_fail in
        t.ing_change_success <- 0;
        t.ing_change_fail <- 0;
        old_success, old_fail

    (* Get the average inter-domain link traffic for the local controller. Stats contain
     * average traffic for the last poll interval that passed. Note that some inter-domain
     * links are not present in the result if they are not used in a YATES paths.
     *)
    let get_inter_dom_link_traffic (t:t) : (demand EdgeMap.t) =
        t.avg_inter_dom_link_traffic

end (* END CtrlInfo struct *)


(* List of local controller instances *)
let local_controllers = ref CIDMap.empty

(* --------------------------------------------- *)


(* Output all controller inter-domain path instructions to console *)
let dump_all_inter_dom_inst unit : unit =
    Printf.printf "Dump inter domain path instructions\n";
    CIDMap.iteri !local_controllers
        ~f:(fun ~key:(cid) ~data:(ctrl) ->
            CtrlInfo.dump_old_inter_dom_inst ctrl;
        );
    Printf.printf "--------\n";
    ()

(* Read and process the switch-to-controller mapping file. The switch-to-controller information is
 * stored in `sw_ctrl_map`, while host to controller mappings are stored in `host_ctrl_map`.
 * `inter_dom_links` stores a mapping of inter-domain switch-port pairs to CID, destination
 * switch and neighbour CID. Controller instances are initiated and stored in `local_controllers`.
 *)
let read_sw_ctrl_map_file unit : unit =
    let json = Yojson.Basic.from_file !Globals.helix_sw_ctrl_map_file in
    (*let root_instances = json |> member "root" |> to_assoc in*)
    let ctrl_instances = json |> member "ctrl" |> to_assoc in
    List.iter (ctrl_instances)
        (fun (ctrl, ctrl_d) ->
            let switch_list = ctrl_d |> member "sw" |> to_list in
            let host_list = ctrl_d |> member "host" |> to_list in

            if debug_output then begin
                Printf.printf "\nLocal Controller: %s\n" ctrl;
                Printf.printf "SW: %s\n" (String.concat ~sep:", " (switch_list |> filter_string));
                Printf.printf "HOST: %s\n" (String.concat ~sep:", " (host_list |> filter_string));
            end;

            (* Initiate a new controller info instance and add to list of local controllers *)
            let new_ctrl = CtrlInfo.create ctrl in
            local_controllers := CIDMap.set !local_controllers ~key:(ctrl) ~data:(new_ctrl);

            (* Process the switch and host mappings *)
            List.iter switch_list (fun s ->
                sw_ctrl_map := NodeCtrlMap.set !sw_ctrl_map ~key:(s |> to_string) ~data:(ctrl);
            );
            (* Add the hosts to the mapping *)
            List.iter host_list (fun s ->
                host_ctrl_map := NodeCtrlMap.set !host_ctrl_map ~key:(s |> to_string) ~data:(ctrl);
            );

            (* Process the inter-domain mappings *)
            List.iter (ctrl_d |> member "dom" |> to_assoc)
                (fun (ncid, ncid_d) ->
                    if debug_output then
                        Printf.printf "NCID: %s\n" ncid;

                    (* Iterate through list of neighbour domains *)
                    List.iter (ncid_d |> to_list)
                        (fun (inter_dom_link) ->
                            let sw = inter_dom_link |> member "sw" |> to_string in
                            let port = inter_dom_link |> member "port" |> to_string in
                            let sw_to = inter_dom_link |> member "sw_to" |> to_string in
                            if debug_output then
                                Printf.printf "\tCID: %s | SW: %s | PORT: %s | SW_TO: %s | NCID: %s\n"
                                                ctrl sw port sw_to ncid;

                            let port_int = Int32.of_string port in
                            inter_dom_links := IDLMap.set !inter_dom_links ~key:(sw, port_int)
                                                ~data:(ctrl, sw_to, ncid);
                        );
                );
        );
    ()

(* Process local controller inter-domain path segments to end-to-end inter-domain paths. Only paths with
 * keys from `path_keys` will be processed. Paths are merged recursively and any ingress changes processed,
 * An ingress change will cause a notification to the local controller of the  modification which updates
 * the controllers path.
 *
 * NOTE: We assume that path segments contain the neighbour domain border node as the final element of the
 *       path (were applicable) and processing starts with a CID path that contains the source node.
 *       All path maps should contain a single path with probability 1.0!
 *
 * Args:
 *  topo (topology): Network topology representation
 *  path_keys ([vertex * vertex]): List of src-dest vertex pairs to process
 *
 * Returns:
 *  scheme: Merged end-to-end path for pairs in `path_keys`
 *)
let merge_inter_dom_path_segments (topo:topology) (path_keys:(vertex * vertex) list) : scheme =
    if debug_output then
        Printf.printf "Merge inter domain paths called\n";

    List.fold path_keys
        ~init:(SrcDstMap.empty)
        ~f:(fun acc (src, dst) ->
            (* Find the vertex information and CIDs of controllers that contain the hosts *)
            let src_n = (Node.name (Topology.vertex_to_label topo src)) in
            let dst_n = (Node.name (Topology.vertex_to_label topo dst)) in
            let src_cid = get_host_ctrl_cid src_n !host_ctrl_map in
            let dst_cid = get_host_ctrl_cid dst_n !host_ctrl_map in

            (* Recursively merge the path segments*)
            let path = ref [] in
            let exp_ing_sw = ref "" in
            let exp_ing_pn = ref 0l in

            let rec merge_path ccid = begin
                (* Get the controller instan and local path *)
                let ctrl = match CIDMap.find !local_controllers ccid with
                    | None -> failwith (Printf.sprintf "Could not find ctrl %s for path merging" ccid)
                    | Some x -> x in
                let tmp_pmap = SrcDstMap.find_exn (CtrlInfo.get_scheme ctrl) (src, dst) in
                let tmp_path = ref (get_path_from_map topo tmp_pmap) in

                if debug_output then
                    Printf.printf "Merge %s -> [%s]\n" ccid (dump_edges topo !tmp_path);

                (* Find the primary inter-dom inst ingress switch and port for the path segment *)
                let idpi_map = CtrlInfo.get_inter_dom_inst ctrl in
                let idpi_list = HKeyMap.find_exn idpi_map (src_n, dst_n) in
                let idpi_prim = List.nth_exn idpi_list 0 in
                let idpi_ing_sw = IDI.get_ingress_sw idpi_prim in
                let idpi_ing_pn = IDI.get_ingress_port idpi_prim in

                (* If there is an ingress change notify the local controller and update the segment *)
                (if ((not (!exp_ing_sw = "")) && (
                        (not (!exp_ing_sw = idpi_ing_sw)) ||
                        (not (!exp_ing_pn = idpi_ing_pn))
                )) then begin

                    (* Try to perform ingress change and catch errors dumping current path *)
                    try
                        let ingchange_path = CtrlInfo.ingress_change ctrl topo src dst !exp_ing_sw !exp_ing_pn in
                        if not (List.is_empty ingchange_path) then begin
                            if debug_output then
                                Printf.printf "Found ingress change path %s %s-%s\n" ccid src_n dst_n;
                            tmp_path := ingchange_path;
                        end else
                            if debug_output then
                                Printf.printf "No ingress change found for path %s %s-%s\n" ccid src_n dst_n;

                    with _ as e -> begin
                        Printf.printf "INGRESS CHANGE ERROR: ";
                        List.iter !path ~f:(fun ed ->
                            let ed_src,_ = Topology.edge_src ed in
                            let ed_dst,_ = Topology.edge_dst ed in
                            let ed_src_name = Node.name (Topology.vertex_to_label topo ed_src) in
                            let ed_dst_name = Node.name (Topology.vertex_to_label topo ed_dst) in
                            Printf.printf "%s-%s " ed_src_name ed_dst_name;
                        );
                        Printf.printf "\n";
                        dump_all_inter_dom_inst ();
                        exit 1;
                    end
                end);

                if debug_output then
                    Printf.printf "\tMerge %s -> [%s]\n" ccid (dump_edges topo !tmp_path);

                (* Update the next expected ingress and iterate to the next segment (if exists) *)
                let final_edge = List.nth_exn !tmp_path ((List.length !tmp_path) - 1) in
                let vend_vert,vend_pn = Topology.edge_dst final_edge in
                let vend_name = Node.name (Topology.vertex_to_label topo vend_vert) in
                exp_ing_sw := vend_name;
                exp_ing_pn := vend_pn;

                path := !path @ !tmp_path;

                if debug_output then
                    Printf.printf "\tNEW PATH [%s]\n" (dump_edges topo !path);

                if (not (ccid = dst_cid)) then begin
                    let ncid = get_node_ctrl_cid vend_name !sw_ctrl_map !host_ctrl_map in

                    (* Prevent infinite loops from occurring *)
                    (* TODO: Extend check to a list of visited CIDs to fully prevent loops *)
                    if ncid = ccid then begin
                        if debug_output then
                            Printf.printf "\nController Scheme:\n|%s|\n" (dump_scheme topo 
                                                                        (CtrlInfo.get_scheme ctrl));
                        failwith ("Can't merge path " ^ src_n ^ "-" ^ dst_n ^
                                    ", encountered infinite loop on controller " ^ ccid);
                    end;

                    (* Iterate on next domain in path *)
                    merge_path ncid;
                end;
            end in
            merge_path src_cid;

            (* Save the merged path *)
            let pmap = PathMap.set PathMap.empty ~key:(!path) ~data:(1.0) in
            SrcDstMap.set acc ~key:(src, dst) ~data:(pmap)
        )

(* Go through root path instructions, computing required segments and paths by calling local
 * controller instances. Produces a routing scheme that contains end-to-end paths for all
 * inter-domain pairs.
 *
 * NOTE: we assume that each path map will only contain a single path element with prob 1.0!
 *
 * Args:
 *  topo (topology): Network topology
 *  json (Yojson.Basic.json): Dictionary of path instructions for each local controller
 *
 * Returns:
 *  scheme: Routing scheme that contains end-to-end paths.
 *)
let proc_root_path_instructions (topo:topology) (json:Yojson.Basic.json) : scheme = 
    let path_keys = List.fold (to_assoc json)
        ~init:([])
        ~f:(fun acc (cid, cid_d) ->
            let new_scheme = match CIDMap.find !local_controllers cid with
                | None -> failwith ("Can't find controller instance " ^ cid)
                | Some x -> CtrlInfo.compute_inter_dom_paths x topo cid_d in

            (* Accumulate any missing path keys from the list of modified paths*)
            SrcDstMap.fold new_scheme
                ~init:(acc)
                ~f:(fun ~key:(u,v) ~data:(pmap) acc2 ->
                    if (List.mem acc2 (u, v) ~equal:(=)) then
                        acc2
                    else
                        acc2 @ [(u, v)]
                );
        ) in

    (* Generate the routing scheme for the inter-domain paths *)
    merge_inter_dom_path_segments topo path_keys

(* Go through all local controller inter-domain instructions and serialize to
 * a JSON file `out_file` as a dictionary of CIDs mapping to a instruction
 * source-destination key map (dictionary).
 *)
let serialize_all_inter_dom_inst (out_file:string) : unit =
    (* Create a buffer to use for the serialization write start bracket *)
    let buf = Buffer.create 400 in
    Printf.bprintf buf "{";
    let first_cid = ref true in

    (* Iterate through controllers and get serialized instruction string *)
    CIDMap.iteri !local_controllers
        ~f:(fun ~key:(cid) ~data:(ctrl) ->
            if !first_cid then
                first_cid := false
            else
                Printf.bprintf buf ", ";

            Printf.bprintf buf "\"%s\": %s" cid
                    (CtrlInfo.get_serialized_inter_dom_inst ctrl);
        );
    Printf.bprintf buf "}";

    (* Write the topo data to the JSON file *)
    let oc = Out_channel.create (out_file) ~append:false in
    fprintf oc "%s\n" (Buffer.contents buf);
    Out_channel.close oc;
    ()

(* Serialize root controller inter-domain traffic by combining all local controllers
 * average inter-domain link traffic map.
 *
 * Args:
 *  topo (topology): Topology of the network
 *  map (CIDMap): List of local controller instances to build root link traffic from
 *  out_file (string): File to save serialized inter-domain root controller traffic
 *)
let serialize_root_link_traffic (topo:topology) (map:CtrlInfo.t CIDMap.t) (out_file:string) : unit =
    (* Collate the inter-domain traffic for all controllers *)
    let inter_dom_link_traffic = CIDMap.fold map
        ~init:(EdgeMap.empty)
        ~f:(fun ~key:(cid) ~data:(ctrl) acc -> 
            EdgeMap.fold (CtrlInfo.get_inter_dom_link_traffic ctrl)
                ~init:(acc)
                ~f:(fun ~key:(edge) ~data:(traf) acc2 ->
                    EdgeMap.set ~key:(edge) ~data:(traf) acc2
                )
        ) in

    serialize_topo_traffic topo inter_dom_link_traffic out_file

(* Serialize root controller TE optimisation request information.
 *
 * Args:
 *  info (CIDMap): Map of TE optimisation requests
 *  out_file (string): File to save serialized data
 *)
let serialize_root_te_opti_request (info:string CIDMap.t) (out_file:string) : unit =
    let buf = Buffer.create 400 in
    Printf.bprintf buf "{";
    let first = ref true in

    CIDMap.iteri info
        ~f:(fun ~key:(cid) ~data:(info_str) ->
            if !first then
                first := false
            else
                Printf.bprintf buf ", ";

            Printf.bprintf buf "\"%s\": %s" cid info_str;
        );
    Printf.bprintf buf "}";

    (* Write the topo data to the JSON file *)
    let oc = Out_channel.create (out_file) ~append:false in
    fprintf oc "%s\n" (Buffer.contents buf);
    Out_channel.close oc;
    ()

(* Initiate TE optimisation variables for all local controllers and load the domain description file *)
let init_te_optimisation unit : unit =
    (* Process the sw-ctrl map file *)
    read_sw_ctrl_map_file ();

    (* Reset the poll timer of all local controller instances *)
    CIDMap.iteri !local_controllers
        ~f:(fun ~key:(cid) ~data:(ctrl) ->
            CtrlInfo.reset_poll_timer ctrl;

            if debug_output then
                CtrlInfo.dump ctrl;
        );

    (* Output execution attributes of the controller *)
    Printf.printf "\n\nTE Thresh: %f | TE Opti Wait: %d | Poll Wait: %d\n"
                    !Globals.helix_te_thresh !Globals.helix_te_opti_wait
                    !Globals.helix_poll_wait;
    Printf.printf "TE Opti Method: %s\n" !Globals.helix_te_opti_method;
    Printf.printf "TE Candidate Sort Rev: %s\n"
                    (string_of_bool !Globals.helix_te_candidate_sort_rev);
    Printf.printf "TE Pot Path Sort Rev: %s\n"
                    (string_of_bool !Globals.helix_te_pot_path_sort_rev);
    Printf.printf "Recompute scheme for each iteration: %s\n"
                    (string_of_bool !Globals.helix_recomp_scheme);
    ()

(* Get the currently computed routing scheme *)
let get_scheme unit : scheme =
    !prev_scheme

(* Get the initial scheme to compute the initial path churn value. Behaves similar to get_scheme. however,
 * if recomp_scheme is true, returns an empty routing scheme to allow accounting for recomputing all paths.
 *
 * NOTE XXX: For the other algorithms that recompute each iteration, TM churn dosen't account for paths being
 * reinstalled in the next iterations, just for differences. I guess we assume that if the path is the same,
 * there is some mechanism which sees this and does not modify the existing path. For the reactive controller,
 * this method accounts for recomputing and reinstalling.
 *)
let get_tm_churn_start_scheme unit : scheme =
    if !Globals.helix_recomp_scheme = true then
        SrcDstMap.empty
    else
        !prev_scheme

(* Compute the required paths for the topology by calling all relevant controller instances. The
 * final scheme is a combination of all schemes merged into one.
 *
 * Args:
 *  topo (topology): Topology to generate routing scheme
 *  dem (demands): Current demand TE matrix to use for generating routing scheme
 *
 * Returns:
 *  scheme: Routing scheme for the current topology
 *)
let solve (topo:topology) (dem:demands) : scheme =
    if (SrcDstMap.is_empty !prev_scheme) = false && !Globals.helix_recomp_scheme = false then begin
        (* Return old scheme (do not recompute for every iteration and already have scheme) *)
        !prev_scheme
    end else begin
        (* Compute local controler intra-domain path *)
        let new_scheme = ref SrcDstMap.empty in
        CIDMap.iteri !local_controllers
            ~f:(fun ~key:(cid) ~data:(ctrl) ->
                let scheme = CtrlInfo.compute_paths ctrl topo dem in
                new_scheme := scheme_merge !new_scheme scheme;
            );

        (* Call root controller to compute inter-domain paths *)
        serialize_topo topo "topo.all.json";
        let topo_path = Filename.realpath "topo.all.json" in
        let data = call_helix_wrapper "yates_root_wrapper.py" ("topo" ^
            " --sw_ctrl_map " ^  !Globals.helix_sw_ctrl_map_file ^
            " --topo " ^ topo_path) in
        let json = Yojson.Basic.from_string data in
        let root_scheme = proc_root_path_instructions topo json in

        (* Merge the inter-domain paths *)
        new_scheme := scheme_merge !new_scheme root_scheme;
        if debug_output then begin
            Printf.printf "\nSOLVE SCHEME DUMP:\n|%s|\n\n" (dump_scheme topo !new_scheme);
            dump_all_inter_dom_inst ();
        end;

        (* Save the computed path and return the result *)
        prev_scheme := !new_scheme;
        !new_scheme
    end

(* Initialize the modules routing scheme *)
let initialize (s:scheme) : unit =
  prev_scheme := s;
  ()

(* Check if a link is over-utilised (over TE threshold). Method finds the controller instance
 * that manages the switch and calls its check congestion method. The traffic matrix `traf` is
 * used to compute the total traffic on the link and save host pair ingress traffic amounts for
 * the domain.
 *
 * Args:
 *  topo (topology): Topology to generate routing scheme
 *  e (edge): Link to check if its congested
 *  traf ((edge array * int * float) list): Amount of traffic on edge/link (bits)
 *  c (float): Capacity of the link bits per seconds
 *)
let check_congestion (topo:topology) (e:edge) (traf:(edge array * int * float) list) (c:float) : unit =
    (* Find the controllers and end details of the link to check for con gestion *)
    let src_v,src_pn = Topology.edge_src e in
    let dst_v,dst_pn = Topology.edge_dst e in
    let src_name = (Node.name (Topology.vertex_to_label topo src_v)) in
    let dst_name = (Node.name (Topology.vertex_to_label topo dst_v)) in
    let src_cid = get_node_ctrl_cid src_name !sw_ctrl_map !host_ctrl_map in
    let dst_cid = get_node_ctrl_cid dst_name !sw_ctrl_map !host_ctrl_map in
    let src_ctrl = match CIDMap.find !local_controllers src_cid with
        | Some x -> x
        | None -> failwith ("Can't find controller with ID " ^ src_cid) in
    let dst_ctrl = match CIDMap.find !local_controllers dst_cid with
        | Some x -> x
        | None -> failwith ("Can't find controller with ID " ^ dst_cid) in

    (* Go through the traffic, compute total and add ingress host-pair traffic to controllers *)
    let total_traf = List.fold traf
        ~init:(0.0)
        ~f:(fun acc (path,_,d) ->
            (* Find the source and destination nodes of the current path in the queue *)
            let hkey_src,hkey_dst = get_hkey_from_path topo path in
            let hkey_src_name = Node.name (Topology.vertex_to_label topo hkey_src) in

            (* Check if the host pair is generating traffic *)
            (if d > 0.0 then
                (* Check if this is a ingress link from a host managed by a controller *)
                if (
                    (hkey_src_name = src_name) &&
                    (is_cid_host src_cid hkey_src_name !host_ctrl_map)
                ) then
                    CtrlInfo.update_host_traffic src_ctrl topo hkey_src hkey_dst d
                (* Check if this is an inter-domain link for a controller *)
                else if (
                    is_cid_inter_dom_link dst_cid dst_name dst_pn !inter_dom_links
                ) then
                    CtrlInfo.update_host_traffic dst_ctrl topo hkey_src hkey_dst d
            );

            (* Cumulate the total traffic on the link *)
            acc +. d
    ) in

    CtrlInfo.check_congestion src_ctrl topo e total_traf c

(* Get the current te optimisation failure and success count.
 * This method will reset the counters so they should be called only after
 * every iteration !
 *
 * Returns:
 *  int * int * HelixCounStatstMap: success count, failure count, per controller counts
 *)
let get_stats unit : int * int * (helix_count_stats HelixCountStatsMap.t) =
    let old_success = !root_te_opti_success in
    let old_fail = !root_te_opti_fail in
    root_te_opti_success := 0;
    root_te_opti_fail := 0;

    (* Get the stats total and create the opti count map *)
    let success, fail, mctrl = CIDMap.fold !local_controllers
        ~init:(old_success, old_fail, HelixCountStatsMap.empty)
        ~f:(fun ~key:(cid) ~data:(ctrl) (acc_suc, acc_fail, opti_count) -> 
            let cid_suc, cid_fail = CtrlInfo.get_stats ctrl in
            (
                (acc_suc + cid_suc),
                (acc_fail + cid_fail),
                (HelixCountStatsMap.set opti_count ~key:(cid) ~data:(
                    {success = cid_suc; failure = cid_fail;}
                ))
            )
        ) in
    (success, fail, (HelixCountStatsMap.set mctrl ~key:("Root") ~data:(
        {success = old_success; failure = old_fail;}))
    )

(* Get the current ingress change success and failure count. This method will
 * reset the counters so it should only be called once for every iteration
 * (TE matrix row)!
 *
 * Returns:
 *  HelixCountStatsMap: success and failure count per controller
 *)
let get_ing_change_stats unit : (helix_count_stats HelixCountStatsMap.t) =
    let old_success = !root_idi_change_success in
    let old_fail = !root_idi_change_fail in
    root_idi_change_success := 0;
    root_idi_change_fail := 0;

    (* Accumulate the local controller ingress change stats to a map *)
    let mctrl = CIDMap.fold !local_controllers
        ~init:(HelixCountStatsMap.empty)
        ~f:(fun ~key:(cid) ~data:(ctrl) acc ->
            let cid_success, cid_fail = CtrlInfo.get_ing_change_stats ctrl in
            HelixCountStatsMap.set acc ~key:(cid) ~data:({
                success = cid_success; failure = cid_fail;
            })
        ) in

    HelixCountStatsMap.set mctrl ~key:("Root") ~data:({
        success = old_success; failure = old_fail;
    })

(* Resolve all detected congestion in the topology. Method should be called every simulation
 * tick (iteration) as it progresses all time-out timers. Local controller is called to
 * resolve congestion. If local controller fails to resolve congestion for inter-domain links
 * root controller wrapper is notified. When a change occurs the old routing scheme is
 * updated and a new path is returned. An empty scheme is returned if there are no updates.

 * Args:
 *  topo (topology): Current topology of network
 *  dem (demands): Current host demands on topology
 *
 * Returns:
 *  scheme: Updated routing scheme or empty scheme if no path changes occured
 *)
let resolve_congestion (topo:topology) (dem:demands) : scheme =
    (* Result scheme to merge root and local updates *)
    let res_scheme = ref SrcDstMap.empty in

    (* Call all local controllers to resolve congestion *)
    let idp_keys, failed_opti_idl = CIDMap.fold !local_controllers
        ~init:([], CIDMap.empty)
        ~f:(fun ~key:(cid) ~data:(ctrl) (acc1, acc2) ->
            let new_scheme = CtrlInfo.resolve_congestion ctrl topo in

            (* Go through the local controller path modifications *)
            let tmp_idp_keys = SrcDstMap.fold new_scheme
                ~init:(acc1)
                ~f:(fun ~key:(src, dst) ~data:(paths) acc_key ->
                    let src_n = Node.name (Topology.vertex_to_label topo src) in
                    let dst_n = Node.name (Topology.vertex_to_label topo dst) in
                    let src_cid = get_host_ctrl_cid src_n !host_ctrl_map in
                    let dst_cid = get_host_ctrl_cid dst_n !host_ctrl_map in
                    if src_cid = dst_cid then begin
                        (* For intra-domain path changes apply them to the scheme *)
                        res_scheme := SrcDstMap.set !res_scheme ~key:(src, dst) ~data:(paths);
                        acc_key
                    end else
                        (* Add updated inter-domain path key to list of modified paths if not already
                         * in the list, otherwise just accumulate the old list.
                         *)
                        if (List.mem acc_key (src, dst) ~equal:(=)) = false then
                            acc_key @ [(src, dst)]
                        else
                            acc_key
                ) in

            (* Retrieve any failed inter-domain link optimisations from the controller *)
            let idl_str = CtrlInfo.get_failed_opti_idl ctrl in
            let idl_acc = match idl_str with
                | "[]" -> acc2
                | _ -> CIDMap.set acc2 ~key:(cid) ~data:(idl_str) in

            (* Accumulate the keys of modified inter-domain paths and inter-domain links which failed
             * to be optimised by the local controller
             *)
            (
                tmp_idp_keys,
                idl_acc
            )
        ) in

    (* Merge and process local controller inter-domain path updates to the result scheme *)
    if (List.is_empty idp_keys) = false then
        res_scheme := SrcDstMap.fold (merge_inter_dom_path_segments topo idp_keys)
            ~init:(!res_scheme)
            ~f:(fun ~key:(src, dst) ~data:(paths) acc ->
                SrcDstMap.set acc ~key:(src, dst) ~data:(paths)
            );

    (* Notify the root controller to resolve congestion on inter-domain links that failed to optimise *)
    if ((not (CIDMap.is_empty failed_opti_idl)) && (not !root_te_opti_lock)) then begin
        if debug_output then
            Printf.printf "Requesting inter-domain link congestion fix from root controller\n";

        serialize_topo topo "topo.all.json";
        serialize_root_link_traffic topo !local_controllers "root.topo_traffic.json";
        serialize_all_inter_dom_inst "root.inst.json";
        serialize_root_te_opti_request failed_opti_idl "root.te.json";

        let topo_path = Filename.realpath "topo.all.json" in
        let te_inst_path = Filename.realpath "root.inst.json" in
        let te_opti_req_path = Filename.realpath "root.te.json" in
        let topo_traffic_path = Filename.realpath "root.topo_traffic.json" in
        let data = call_helix_wrapper "yates_root_wrapper.py" ("te" ^
            " --sw_ctrl_map " ^ !Globals.helix_sw_ctrl_map_file ^
            " --path_inst " ^ te_inst_path ^
            " --te_opti_req " ^ te_opti_req_path ^
            " --topo_traffic " ^ topo_traffic_path ^
            " --te_candidate_sort_rev " ^ (string_of_bool !Globals.helix_te_candidate_sort_rev) ^
            " --te_paccept " ^ (string_of_bool !Globals.helix_te_paccept) ^
            " --topo " ^ topo_path) in

        if List.is_empty (to_assoc (Yojson.Basic.from_string data)) then begin
            if debug_output = false then
                Printf.printf "Failed to resolve congestion via root controller |%s|\n" data;
            root_te_opti_fail := !root_te_opti_fail + 1;
        end else begin
            if debug_output then
                Printf.printf "\nROOT FIX IDI:\n^^%s^^\n" data;

            root_te_opti_success := !root_te_opti_success + 1;
            root_te_idi_update := data;
            root_te_opti_timer := (!Globals.helix_te_opti_wait -1);
            root_te_opti_lock := true;
        end
    end;

    (* Check if we need to apply a root controller TE update *)
    if !root_te_opti_lock then
        if !root_te_opti_timer <= 0 then begin
            (* Apply the root TE update on expiry of the timer *)
            let json = Yojson.Basic.from_string !root_te_idi_update in
            let root_scheme = proc_root_path_instructions topo json in
            if debug_output then
                Printf.printf "\nROOT FIX SCHEME:\n^^%s^^\n" (dump_scheme topo root_scheme);

            res_scheme := SrcDstMap.fold root_scheme
                ~init:(!res_scheme)
                ~f:(fun ~key:(src, dst) ~data:(paths) acc ->
                    SrcDstMap.set acc ~key:(src, dst) ~data:(paths)
                );
            root_te_idi_update := "";
            root_te_opti_timer := 0;
            root_te_opti_lock := false;
		end else
            (* Tick the root controller timer *)
            root_te_opti_timer := !root_te_opti_timer - 1;

    (* Check if we have a update result that we need to apply *)
    if SrcDstMap.is_empty !res_scheme then
        SrcDstMap.empty
    else begin
        (* Merge changes to the current routing scheme and return the update *)
        prev_scheme := SrcDstMap.fold !res_scheme
            ~init:(!prev_scheme)
            ~f:(fun ~key:(src, dst) ~data:(paths) acc ->
                SrcDstMap.set acc ~key:(src, dst) ~data:(paths)
            );

        if debug_output then begin
            Printf.printf "\nFIX SCHEME:\n^^%s^^\n" (dump_scheme topo !prev_scheme);
            dump_all_inter_dom_inst ();
        end;

        !prev_scheme
    end

(* Perform local recovery when failures occur in the network *)
let local_recovery = normalization_recovery
