(*
    ------------------------------------------------------------
    Helix multi-controller helper module
    ------------------------------------------------------------

TYPES FOR ALL INFORMATION STORED
    
Switch to Ctrl:
    NodeCtrlMap | sw:str -> CID:str
Host to Ctrl:
    NodeCtrlMap | host:str -> CID:str
Inter-dom link to Ctrl:
    IDLMap | (sw:str, port:int32) -> (CID:str, sw_to:str, neighbour CID:str)
List of local controllers
    ControllerList | CID:str -> CtrlInfo
    CIDInfoMap | CID:str -> CtrlInfo

Inter-domain instructions maps
    CIDMap | CID:str ->
        HKeyMap | (src:str, dest:str) -> [IDI, ...]
*)
open Core

open Apsp
open Util
open Yates_types.Types

open Yojson
open Yojson.Basic.Util
open Helix_Helper


(* Map a controller ID (string) to a object *)
module CIDMap = Map.Make(String)

(* Map node name (switch or host string) to a object *)
module NodeCtrlMap = Map.Make(String)

(* A map of inter-domain links (switch port) to destination info *)
(* Map a inter-domain links (switch port tuples) to a a object *)
module IDLOrd = struct
    type t = (string * int32) [@@deriving sexp]
    let compare = Pervasives.compare
end

module IDLMap = Map.Make(IDLOrd)

(* Map a path source destination key (host key) to a object *)
module HKeyOrd = struct
    type t = (string * string) [@@deriving sexp]
    let compare = Pervasives.compare
end

module HKeyMap = Map.Make(HKeyOrd)


(* Signature of type that contains inter domain instruction information *)
module type IDI = sig
    type t

    val create : Yojson.Basic.json -> t
    val get_type : t -> int
    val to_str : t -> string
    val to_json : t -> string
    val get_ingress_sw : t -> string
    val get_ingress_port : t -> int32
    val get_egress_sw : t -> string
    val get_egress_port : t -> int32
end

(* Module that stores inter-dom instructions sent to local controllers from root *)
module IDI : IDI = struct
    type t = {
        (* Input switch, can be empty if src host in domain *)
        mutable in_sw : string;
        (* Input port, can be -1 if src host in domain *)
        mutable in_pn : int32;
        (* Output switch, can be empty if dest host in domain *)
        mutable out_sw : string;
        (* Output port, can be -1 if dest host in domain *)
        mutable out_pn : int32;
    }

    (* Initiate a new inter-domain instruction object from a json instruction *)
    let create (inst:Yojson.Basic.json) : t =
        let in_sw, in_pn = match (inst |> member "in") with
            | `Int x -> ("", (Int32.of_int_exn x))
            | `List x -> (
                ((List.nth_exn x 0) |> to_string),
                (Int32.of_int_exn ((List.nth_exn x 1) |> to_int)))
            | _ -> failwith "Invalid inter-dom instruction in port type" in

        let out_sw, out_pn = match (inst |> member "out") with
            | `Int x -> ("", (Int32.of_int_exn x))
            | `List x -> (
                ((List.nth_exn x 0) |> to_string),
                (Int32.of_int_exn ((List.nth_exn x 1) |> to_int)))
            | _ -> failwith "Invalid inter-dom instruction out port type" in

        {in_sw = in_sw; in_pn = in_pn; out_sw = out_sw; out_pn = out_pn}

    (* Find the type of the instruction, -1 for source, 0 for transit and 1 for destination *)
    let get_type (t:t) : int =
        if t.in_sw = "" then
            -1
        else if t.out_sw = "" then
            1
        else
            0

    (* Get the ingress swich field of the instruction *)
    let get_ingress_sw (t:t) : string =
        t.in_sw

    (* Get the ingress port field of the instruction *)
    let get_ingress_port (t:t) : int32 =
        t.in_pn

    (* Get the egress switch field of the instruction *)
    let get_egress_sw (t:t) : string =
        t.out_sw

    (* Get the egress port field of the instruction *)
    let get_egress_port (t:t) : int32 =
        t.out_pn

    (* Return a string representation of the object *)
    let to_str (t:t) : string =
        Printf.sprintf "<InterDomInst> in_sw %s in_pn %ld out_sw %s out_pn %ld"
            t.in_sw t.in_pn t.out_sw t.out_pn

    (* Return a JSON string representation of the object *)
    let to_json (t:t): string =
        let in_str = if t.in_sw = "" then
                Printf.sprintf "%ld" t.in_pn
            else
                Printf.sprintf "[\"%s\", %ld]" t.in_sw t.in_pn in
        let out_str = if t.out_sw = "" then
                Printf.sprintf "%ld" t.out_pn
            else
                Printf.sprintf "[\"%s\", %ld]" t.out_sw t.out_pn in
        Printf.sprintf "{\"action\": \"add\", \"in\": %s, \"out\": %s}" in_str out_str;

end (* END IDI struct *)

(* Signature of type that contains local controller instance information *)
module type CtrlInfo = sig
    type t

    val create : string -> t
    val get_scheme : t -> scheme
    val get_failed_opti_idl : t -> string
    val reset_poll_timer : t -> unit
    val dump : t -> unit

    val get_inter_dom_inst : t -> IDI.t list HKeyMap.t
    val update_inter_dom_inst : t -> Yojson.Basic.json -> unit
    val get_serialized_inter_dom_inst : t -> string
    val serialize_old_inter_dom_inst : t -> string -> unit
    val dump_old_inter_dom_inst : t -> unit

    val compute_paths : t -> topology -> demands -> scheme
    val compute_inter_dom_paths : t -> topology -> Yojson.Basic.json -> scheme
    val ingress_change : t -> topology -> vertex -> vertex -> string -> int32 -> path

    val can_check_congestion : t -> bool
    val can_optimise : t -> bool

    val update_host_traffic : t -> topology -> vertex -> vertex -> float -> unit

    val check_congestion : t -> topology -> edge -> float -> float -> unit
    val resolve_congestion : t -> topology -> scheme

    val get_stats : t -> int * int
    val get_ing_change_stats : t -> int * int
    val get_inter_dom_link_traffic : t -> (demand EdgeMap.t)
end


(* Check if a switch belongs to a controller *)
let is_cid_switch (cid:string) (sw:string) (map) : bool =
    match NodeCtrlMap.find map sw with
        | None -> false
        | Some x -> (x = cid)

(* Check if a host belongs to a controller *)
let is_cid_host (cid:string) (host:string) (map) : bool =
    match NodeCtrlMap.find map host with
        | None -> false
        | Some x -> (x = cid)

(* Check if a element (host or switch) belongs to a controller *)
let is_cid_element (cid:string) (element:string) (map_sw) (map_host) : bool =
    let is_sw = is_cid_switch cid element map_sw in
    let is_host = is_cid_host cid element map_host in
    (is_sw || is_host)

(* Check if an inter-domain link belongs to a controller *)
let is_cid_inter_dom_link (cid:string) (sw:string) (port:int32) (map) : bool =
    match IDLMap.find map (sw, port) with
        | None -> false
        | Some (x,_,_) -> (x = cid)

(* Check if a link is an inter-domain link *)
let is_inter_dom_link (switch:string) (port:int32) (map) : bool =
    match IDLMap.find map (switch, port) with
        | None -> false
        | Some x -> true

(* Get a list of all hosts associated to a controller *)
let get_ctrl_hosts (ctrl_id:string) (map) : (string list) =
    NodeCtrlMap.fold map
        ~init:([])
        ~f:(fun ~key:(host) ~data:(cid) acc ->
            if cid = ctrl_id then
                acc @ [host]
            else
                acc
        )

(* Get a list of all switches associated to a controller *)
let get_ctrl_sw (ctrl_id:string) (map) : (string list) =
    NodeCtrlMap.fold map
        ~init:([])
        ~f:(fun ~key:(sw) ~data:(cid) acc ->
            if cid = ctrl_id then
                acc @ [sw]
            else
                acc
        )

(* Find the CID of a controller that manages a switch *)
let get_sw_ctrl_cid (sw:string) (map) : string =
    match NodeCtrlMap.find map sw with
        | None -> failwith ("Could not find CID for switch " ^ sw)
        | Some x -> x

(* Find the CID of the controller that manages a host *)
let get_host_ctrl_cid (host:string) (map) : string =
    match NodeCtrlMap.find map host with
        | None -> failwith ("Could not find CID for host " ^ host)
        | Some x -> x

(* Find the CID of the controller that managed a element *)
let get_node_ctrl_cid (n:string) (map_sw) (map_host) : string =
    (* Try and find the node in the host map *)
    let res = match NodeCtrlMap.find map_host n with
        | None -> "-"
        | Some x -> x in
    match res with
        | "-" -> begin
            (* Try to find the node in the switch map, if not found raise an error *)
            match NodeCtrlMap.find map_sw n with
                | Some x -> x
                | None -> failwith ("Could not find CID of node " ^ n)
        end
        | _ -> res

(* Get a list of border switches for a controller (inter-domain edge switches) *)
let get_ctrl_border_sw (ctrl_id:string) (map): (string list) =
    IDLMap.fold map
        ~init:([])
        ~f:(fun ~key:(sw, port) ~data:(cid, neigh_sw, _) acc ->
            if cid = ctrl_id then
                acc @ [sw]
            else
                acc
        )

(* Retrieve a list of nodes connected to via inter-domain links for a controller *)
let get_inter_dom_neighbour_sw (ctrl_id:string) (map): (string list) =
    IDLMap.fold map
        ~init:([])
        ~f:(fun ~key:(sw, port) ~data:(cid, neigh_sw, _) acc ->
            if cid = ctrl_id then
                acc @ [neigh_sw]
            else
                acc
        )

(* Get the CID of the neighbouring domain that an inter-domain link connects to *)
let get_inter_dom_neighbour_cid (sw:string) (port:int32) (map) : string =
    match IDLMap.find map (sw, port) with
        | None -> failwith (
            Printf.sprintf "Could not find neighbour CID for inter-domain link %s %ld" sw port)
        | Some (_,_,y) -> y

(* Get a list of inter-domain links of a controller. Format of list is (sw, port, sw_to, ncid) *)
let get_inter_dom_links (ctrl_id:string) (map) : ((string * int32 * string * string) list) =
    IDLMap.fold map
        ~init:([])
        ~f:(fun ~key:(sw, port) ~data:(cid, neigh_sw, ncid) acc ->
            if cid = ctrl_id then
                acc @ [(sw, port, neigh_sw, ncid)]
            else
                acc
        )

(* Return the topology for a local controller. Returned object contains nodes managed by the
 * controller as well as any neighbouring inter-domain link nodes. Only inter-domain edges
 * and intra domain edges are presents.
 *
 * Args:
 *  topo (topology): Complete topology object for entire network
 *  cid (string): ID of controller to get topolog for
 *
 * Returns:
 *  scheme: Local controller restricted topology
 *)
let get_cid_topo (topo:topology) (cid:string) (map_sw) (map_host) (inter_dom_map): topology =
    (* Remove verticies and implicitly edges that do not belong to the controller. The neighbouring
     * nodes for inter-domain links are kept in the topology, however, non-inter-domain edges
     * of these nodes will be removed
     *)
    let border_sw_neigh = get_inter_dom_neighbour_sw cid inter_dom_map in
    let new_topo = Topology.fold_vertexes
        (fun vert acc ->
            let vert_name = (Node.name (Topology.vertex_to_label topo vert)) in

            if not (is_cid_element cid vert_name map_sw map_host) then
                if (List.mem border_sw_neigh vert_name ~equal:(=)) then
                    acc
                else
                    Topology.remove_vertex acc vert
            else
                acc
        ) topo (Topology.copy topo) in

    Topology.fold_edges
        (fun edge acc ->
            let src_v,src_pn = Topology.edge_src edge in
(*            let dst_v,dst_pn = Topology.edge_dst edge in*)
            let src_n = Node.name (Topology.vertex_to_label topo src_v) in
(*            let dst_n = Node.name (Topology.vertex_to_label topo dst_v) in*)

            if (List.mem border_sw_neigh src_n ~equal:(=)) then
                Topology.remove_edge acc edge
                (*if not (is_inter_dom_link dst_n dst_pn inter_dom_map) then
                    Topology.remove_edge acc edge
                else
                    acc
                *)
            else
                acc

        ) new_topo (new_topo)

(* Merge two routing schemes. If a src_dst key exists in both `s1` and `s2`
 * then the S1 key and value pair is stored in the final result.
 *)
let scheme_merge (s1:scheme) (s2:scheme) : scheme =
    let merged = SrcDstMap.fold s1
        ~init:(SrcDstMap.empty)
        ~f:(fun ~key:(u,v) ~data:(pmap) acc ->
            SrcDstMap.set ~key:(u,v) ~data:(pmap) acc
        ) in
    SrcDstMap.fold s2
        ~init:(merged)
        ~f:(fun ~key:(u,v) ~data:(pmap) acc ->
            match SrcDstMap.find acc (u,v) with
                | Some x -> acc
                | None -> SrcDstMap.set ~key:(u,v) ~data:(pmap) acc
        )

(* Parse a deserialized JSON object to a list of inter-domain instruction objects *)
let deserialize_idi_list (json:Yojson.Basic.json) : (IDI.t list) =
    List.fold (json |> to_list)
        ~init:([])
        ~f:(fun acc inst ->
            acc @ [(IDI.create inst)]
        )

(* Parse a deserialized JSON object to a src-dest key map of inter-domain
 * instruction lists. Calls `deserialize_idi_list` to process list of
 * instructions.
 *)
let deserailize_idi_map (json:Yojson.Basic.json) : (IDI.t list HKeyMap.t) =
    List.fold (json |> to_list)
        ~init:(HKeyMap.empty)
        ~f:(fun acc (obj) ->
            let src = obj |> member "keysrc" |> to_string in
            let dst = obj |> member "keydst" |> to_string in
            let insts = obj |> member "instructions" in

            HKeyMap.set acc ~key:(src, dst) ~data:(
                deserialize_idi_list insts
            )
        )

(* Serialize inter-domain path-instructions to a file for sending to the local
 * controller wrapper.
 *
 * Args:
 *  topo (topology): Topology object to serialize to a JSON object
 *  out_file (string): Path of serilized JSON output file
 *)
let serialize_json_inter_dom_inst (json:Yojson.Basic.json) (out_file:string) : unit =
    let oc = Out_channel.create (out_file) ~append:false in
    Yojson.Basic.to_channel oc json;
    Out_channel.close oc;
    ()

(* Serialize the list of inter-domain links for a particular controller. *)
let serialize_inter_dom_links (topo:topology) (ctrl_id:string) (map) (out_file:string) : unit =
    let buf = Buffer.create 400 in
    let first = ref true in
    let idls = get_inter_dom_links ctrl_id map in

    Printf.bprintf buf "[";
    List.iter idls
        (fun (sw, pn, sw_to, ncid) ->
            if !first = true then
                first := false
            else
                Printf.bprintf buf ", ";

            Printf.bprintf buf "[\"%s\", %ld, \"%s\", \"%s\"]" sw pn sw_to ncid;
        );

    Printf.bprintf buf "]";

    (* Write the topo data to the JSON file *)
    let oc = Out_channel.create (out_file) ~append:false in
    fprintf oc "%s\n" (Buffer.contents buf);
    Out_channel.close oc;
    ()

(* Get the first path entry from a path map object *)
let get_path_from_map (topo:topology) (pps:probability PathMap.t) : path =
    let sorted_paths = PathMap.keys pps
                        |> List.sort ~compare:(fun p1 p2 ->
                            Pervasives.compare
                                (get_path_weight topo p1) (get_path_weight topo p2)
                            )in
    List.nth_exn sorted_paths 0

(* Get the host pair that form a complete end-to-end path. The method returns the
 * source of the first edge and the destination of the last edge.
 *
 * Args:
 *  topo (topology): Topology instance of the simulation
 *  path (edge array): List of edges
 *
 * Returns:
 *  vertex * vertex: Source vertex, destination vertex
 *)
let get_hkey_from_path (topo:topology) (path:edge array) : vertex * vertex =
    let first_edge = Array.get path 0 in
    let last_edge = Array.get path ((Array.length path) - 1) in
    let src,_ = Topology.edge_src first_edge in
    let dst,_ = Topology.edge_dst last_edge in
    src, dst
