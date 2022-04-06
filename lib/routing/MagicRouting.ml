(*
    ------------------------------------------------------------
    Magic routing scheme that demonstrates that YATES dosen't perform
    checking for paths to ensure that they make sense, i.e. the paths
    connect and are of valid links

    NOTE: THIS ALGORITHM DOSEN'T WORK SO DON'T USE IT FOR RESULT
          COMPARISON. IT'S PURPOSE IS TO ASSERT IF YATES VALIDATES
          PATHS WHEN FORWARDING TRAFFIC !
    ------------------------------------------------------------
*)
open Core

open Apsp
open Util
open Yates_types.Types


(* Previous routing scheme *)
let prev_scheme = ref SrcDstMap.empty

(* Initialize the modules routing scheme *)
let initialize (s:scheme) : unit =
  prev_scheme := s;
  ()

(* Solve the routing scheme by finding the ingress and egress links to and from the required
 * hosts.
 *)
let solve (topo:topology) (dem:demands) : scheme =
    let scheme = SrcDstMap.fold dem
        ~init:(SrcDstMap.empty)
        ~f:(fun ~key:(src, dst) ~data:(d) acc ->
            if not (src = dst) then begin
                let src_name = Node.name (Topology.vertex_to_label topo src) in
                let dst_name = Node.name (Topology.vertex_to_label topo dst) in
                let path = match NetPath.shortest_path topo src dst with
                    | None -> failwith (Printf.sprintf "Can't compute path for %s-%s" src_name dst_name)
                    | Some x -> x in
                let magic_path = [List.nth_exn path 0] @ [List.nth_exn path ((List.length path) -1)] in
                SrcDstMap.set ~key:(src, dst) ~data:(
                    PathMap.set ~key:(magic_path) ~data:(1.0) PathMap.empty
                ) acc
            end else
                acc
        ) in

    prev_scheme := scheme;
    !prev_scheme

(* Perform local recovery when failures occur in the network *)
let local_recovery = normalization_recovery
