(*
    ------------------------------------------------------------
    Helix (single controller / shared) helper module
    ------------------------------------------------------------
*)

open Core

open Util
open Yates_types.Types

open Yojson
open Yojson.Basic.Util



(* ========== PATH THAT POINTS TO THE HELIX REPO SOURCE CODE ========= *)
let helix_source_path = "../helix/"
(* =================================================================== *)




(* Output a topology to the console
 *
 * Args:
 *  topo (topology): Topology to output to the console
 *)
let print_topo (topo:topology) =
    Topology.iter_edges (fun e ->
        let src,_ = Topology.edge_src e in
        let dst,_ = Topology.edge_dst e in
        Printf.printf "(%s-%s [%Ld]) "
            (Node.name (Topology.vertex_to_label topo src))
            (Node.name (Topology.vertex_to_label topo dst))
            (Link.cost (Topology.edge_to_label topo e));
    ) topo;
    Printf.printf "\n";
    ()

(* Basic comparator that checks for equality
 *
 * Args
 *  a (obj): First object to compare
 *  b (obj): Second object to compare
 *
 * Returns:
 *  bool: True if the objects are equal, False otherwise
 *)
let cmp a b : bool =
    if a = b then
        true
    else
        false

(* Comparator that checks if two edges are equal by checking if the src and
 * destination vertexes and ports are the same.
 *
 * Args
 *  a (edge): First edge to compare
 *  b (edge): Second edge to compare
 *
 * Returns
 *  bool: True if the edges are equal, False otherwise
 *)
let edge_cmp a b : bool =
    let a_src,_ = Topology.edge_src a in
    let a_dst,_ = Topology.edge_dst a in
    let b_src,_ = Topology.edge_src b in
    let b_dst,_ = Topology.edge_dst b in
    if a_src = b_src && a_dst = b_dst then
        true
    else
        false

(* Find the instance of an edge based on the names of the two vertices that
 * make up the src and destination. If the edge can't be found method
 * returns None
 *
 * Args:
 *  topo (topology): topology of network to find edge in
 *  a (str): Src vertex name of edge to find
 *  b (str): Destination vertex name of edge to find
 *
 * Returns:
 *  edge option: Edge instance or none if we can't find edge with specified
 *               src and dst name.
 *)
let find_edge_by_name (topo:topology) (a:string) (b:string) : edge option =
    let edge = ref None in
    Topology.iter_edges
        (fun e ->
            let src,_ = Topology.edge_src e in
            let dst,_ = Topology.edge_dst e in
            let src_name = (Node.name (Topology.vertex_to_label topo src)) in
            let dst_name = (Node.name (Topology.vertex_to_label topo dst)) in
            if src_name = a && dst_name = b then
                edge := Some e;
         ) topo;
    !edge

(* Find a vertex in a topology based on its name. If the vertex can't be
 * gound None is returned.
 *
 * Args:
 *  topo (topology): Topology to find vertex in
 *  a (str): Name of the vertex to look for in topo
 *
 * Returns:
 *  vertex option: Vertex with the specified name or None if the vertex
 *                 can't be found in topo
 *)
let find_vertex_by_name (topo:topology) (a:string) : vertex option =
    let node = ref None in
    Topology.iter_vertexes
        (fun v ->
            let v_name = (Node.name (Topology.vertex_to_label topo v)) in
            if v_name = a then
                node := Some v;
         ) topo;
    !node

(* Convert a YoJson list to a path.
 *
 * Args:
 *  topo (topology): Topology of network
 *  json)list (Basic.json list): JSON list to convert to path
 *
 * Returns:
 *  path: Converted path which is a list made up edges (or empty list)
 *)
let json_list_to_path (topo:topology) (json_list:Basic.json list) : path =
    let vert_a = ref "" in
    let vert_b = ref "" in
    List.fold json_list
        ~init:([])
        ~f:(fun acc n_json ->
            let n = n_json |> to_string in
            if !vert_a = "" then begin
                vert_a := n;
                acc
            end
            else begin
                if !vert_b = "" then
                    vert_b := n
                else begin
                    vert_a := !vert_b;
                    vert_b := n;
                end;
                (* If the edge produced by the two vertices does not exist do
                 * not append it to the path
                 *)
                match find_edge_by_name topo !vert_a !vert_b with
                    | None -> acc
                    | Some x -> acc @ [x]
            end
        )

(* Convert a json dictionary to a routing scheme. The json dict takes the format:
 *          src: {dst: {primary: [], secondary: [], splice: [[], []]}}.
 * NOTE: only the primary path is added to the scheme while other fields are ignored.
 *)
let json_path_to_scheme (topo:topology) (json:Yojson.Basic.json) : scheme =
    List.fold (to_assoc json)
        ~init:(SrcDstMap.empty)
        ~f:(fun acc (src, src_d) ->
            List.fold (to_assoc src_d)
                ~init:(acc)
                ~f:(fun acc2 (dst, dst_d) ->
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
                    (* Find the host src and dest vertex to update the routing scheme *)
                    let src_vert = match find_vertex_by_name topo src with 
                        | None -> failwith "Could not find host name vertex for path"
                        | Some x -> x
                    in
                    let dst_vert = match find_vertex_by_name topo dst with 
                        | None -> failwith "Could not find host name vertex for path"
                        | Some x -> x
                    in
                    SrcDstMap.set acc2 ~key:(src_vert, dst_vert) ~data:(!pMap)
                )
        )

(* Serialize a topology object and save it as a JSON file
 *
 * Args:
 *  topo (topology): Topology object to serialize to a JSON object
 *  out_file (string): Path of serialized JSON output file
 *)
let serialize_topo (topo:topology) (out_file:string) : unit =
    (* Serialize the topology to a JSON object *)
    let buf = Buffer.create 400 in
    let first = ref true in

    Printf.bprintf buf "{\"topo\": [";
    Topology.iter_edges
        (fun e ->
            let src,src_p = Topology.edge_src e in
            let dst,dst_p = Topology.edge_dst e in
            let cost = Link.cost (Topology.edge_to_label topo e) in
            (*let weight = Link.weight (Topology.edge_to_label topo e) in*)
            let speed = Int64.of_float (capacity_of_edge topo e) in

            (* Set the cost to 1 if it equals nothing *)
            let cost = if cost = 0L then 1L else cost in

            if !first = true then
                first := false
            else
                Printf.bprintf buf ", ";

            Printf.bprintf buf "{\"src\": \"%s\", \"srcPort\": %ld, "
                (Node.name (Topology.vertex_to_label topo src)) src_p;
            Printf.bprintf buf "\"dest\": \"%s\", \"destPort\": %ld, \"cost\": %Ld, \"speed\": %Ld}"
                (Node.name (Topology.vertex_to_label topo dst)) dst_p cost speed;

            (*
            if (Node.device (Topology.vertex_to_label topo src)) = Node.Host then
            begin
                let ip = (Node.ip (Topology.vertex_to_label topo src)) in
                let mac = (Node.mac (Topology.vertex_to_label topo src)) in
                Printf.bprintf buf ", \"ip\": \"%ld\", \"mac\": \"%Ld\"} "
                    ip mac;
            end
            else
                Printf.bprintf buf "}";
            *)
        ) topo;

    (* Generate the host information list JSON object *)
    Printf.bprintf buf "],\n\"hosts\": [";

    let first = ref true in
    Topology.iter_vertexes
        (fun v ->
            let label = Topology.vertex_to_label topo v in
            if (Node.device label) = Node.Host then
                if !first = true then begin
                    first := false;
                    Printf.bprintf buf " \"%s\" " (Node.name label);
                end else
                    Printf.bprintf buf ", \"%s\" " (Node.name label);
        ) topo;

    Printf.bprintf buf "]}";

    (* Write the topo data to the JSON file *)
    let oc = Out_channel.create (out_file) ~append:false in
    fprintf oc "%s\n" (Buffer.contents buf);
    Out_channel.close oc;
    ()

(* Serialize a demand object and save it to a output JSON file
 *
 * Args:
 *  topo (topology): Topology object of current topo
 *  dem (demands): Demand to serailize to file
 *  out_file (string): Path of serialized JSON output file
 *)
let serialize_demand (topo:topology) (dem:demands) (out_file:string) : unit =
    (* Serialize the topology to a JSON object *)
    let buf = Buffer.create 400 in
    let first = ref true in

    Printf.bprintf buf "[";
    SrcDstMap.iteri dem
        ~f:(fun ~key:(src,dst) ~data:(d) ->
            if !first = false then
                Printf.bprintf buf ","
            else
                first := false;
            Printf.bprintf buf "{\"src\": \"%s\", \"dest\": \"%s\", \"val\": %f}"
                (Node.name (Topology.vertex_to_label topo src))
                (Node.name (Topology.vertex_to_label topo dst))
                d;
        );

    Printf.bprintf buf "]";

    (* Write the topo data to the JSON file *)
    let oc = Out_channel.create (out_file) ~append:false in
    fprintf oc "%s\n" (Buffer.contents buf);
    Out_channel.close oc;
    ()


(* Serialize the topology traffic (link traffic object) to a JSON file.
 *
 * Args:
 *  topo (topology): Topology object of current topo
 *  links (EdgeMap): Link traffic object
 *  out_file (string): Path of serialized JSON output file
 *)
let serialize_topo_traffic (topo:topology) (links:(float EdgeMap.t)) (out_file:string) : unit =
    (* Serialize the topology to a JSON object *)
    let buf = Buffer.create 400 in
    let first = ref true in

    Printf.bprintf buf "[";
    EdgeMap.iteri links
        ~f:(fun ~key:(e) ~data:(usage) ->
            (* Get the source node and port of link *)
            let src,src_p = Topology.edge_src e in

            (* Output the serialized information *)
            if !first = false then
                Printf.bprintf buf ","
            else
                first := false;
            Printf.bprintf buf "{\"src\": \"%s\", \"src_port\": \"%ld\", \"val\": %f}"
                (Node.name (Topology.vertex_to_label topo src)) src_p usage
        );
    Printf.bprintf buf "]";

    (* Write the topo data to the JSON file *)
    let oc = Out_channel.create (out_file) ~append:false in
    fprintf oc "%s\n" (Buffer.contents buf);
    Out_channel.close oc;
    ()

(* Serialize the list over utilised links as a JSON object (over_util_links).
 *
 * Args:
 *  topo (topology): Topology object of current topo
 *  out_file (string): Path of serialized JSON output file
 *)
let serialize_over_util_links (topo:topology) (links:(vertex * int32 * float) list) (out_file:string) : unit =
    (* Serialize the topology to a JSON object *)
    let buf = Buffer.create 400 in
    let first = ref true in

    Printf.bprintf buf "[";
    List.iter links
        ~f:(fun (src, src_p, usage) ->
            if !first = false then
                Printf.bprintf buf ","
            else
                first := false;
            Printf.bprintf buf "{\"src\": \"%s\", \"src_port\": %ld, \"usage\": %f}"
                (Node.name (Topology.vertex_to_label topo src)) src_p usage;
        );
    Printf.bprintf buf "]";

    (* Write the topo data to the JSON file *)
    let oc = Out_channel.create (out_file) ~append:false in
    fprintf oc "%s\n" (Buffer.contents buf);
    Out_channel.close oc;
    () 

(* Call the Helix controller wrapper and return the result generated 
 * by the script.
 *
 * Args:
 *  script (string): Script name to call (wrapper)
 *  operation (string): Action and arguments to pass to script
 *
 * Returns:
 *  string: Response from the script
 *)
let call_helix_wrapper (script:string) (operation:string) : string =
    let cmd = ("python " ^ helix_source_path ^ script ^ " --action " ^ operation) in
    (*Printf.printf "Running Helix wrapper command '%s'\n" cmd;*)

    (* Run the command and check if completed *)
    let proc = Unix.open_process_in (cmd) in
    let rec read_output proc data = 
        try
            let line = In_channel.input_line_exn proc in
            let data = data ^ "\n" ^ line in
            read_output proc data
        with End_of_file -> data in

    let data = match (read_output proc "") with
        | "" -> failwith("Helix wrapper script failed to complete request")
        | x -> x

    in
    ignore (Unix.close_process_in proc);
    data
