#!/usr/bin/python

""" Script that generates a switch controller mapping from a topology file
and a simple switch to controller mappig file.

Usage:
    ./sw_ctrl_map_gen.py --topo topo --map map --output output
        topo - YATES topology file in either .dot or .gml format
        map - Text file with simple controller to switch list. See ``Map Input Syntax ``
        output - Output file to save generated switch to controller map JSON

    Optional Attributes:
        --speed - Float that defines scale factor to apply to link speeds for output (defaults 1.0)

Map Input Syntax:
    The map input file defines the number of local controllers as well as the switches that
    it manages. A controller per line and each line follows the syntax "<CID>|<SW>,<SW>" where
    <CID> is the local controller ID or name and <SW> is the switch ID that it manages.

    Note, we assume that every switch specified has a host connected to it with the same ID so
    <SW> should be the ID and not include any prefix. For example if we have a host H1 and
    switch SW1 that belongs to controller C1, we would write a line: "C1|1".
"""

import sys
import json
from networkx.drawing.nx_pydot import read_dot as dot
from argparse import ArgumentParser


def get_node_cid(node, ctrl_map):
    """ Retrieve the CID for a specifiec node by going through a mapping file

    Args:
        node (str): Name of the node (either host or switch)
        ctrl_map (dict): Switch to controller map object

    Returns:
        str: ID of the controller or None if object can't be located
    """
    for cid,cid_d in ctrl_map["ctrl"].iteritems():
        if node in cid_d["host"]:
            return cid
        if node in cid_d["sw"]:
            return cid
    return None


if __name__ == "__main__":
    # Process the arguments
    parser = ArgumentParser()
    parser.add_argument("--topo", required=True, type=str, help="DOT file to process")
    parser.add_argument("--map", required=True, type=str,
                            help="Path to simple map file where each line has format <CID>|<sw>,<sw>,...")
    parser.add_argument("--output", required=True, type=str, help="Output sw controller map file")
    args = parser.parse_args()

    RES = {"root": {"r1": {}}, "ctrl": {}}

    # Parse the map attributes
    with open(args.map, "r") as f:
        for line in f:
            tmp_split = line.strip().split("|")
            cid = tmp_split[0]
            RES["ctrl"][cid] = {"sw": [], "host": [], "dom": {}}

            for tok in tmp_split[1].split(","):
                RES["ctrl"][cid]["sw"].append("s%s" % tok)
                RES["ctrl"][cid]["host"].append("h%s" % tok)

    # Read the dot file and process
    g = dot(path=args.topo)
    nodes = g.nodes.items()
    edges = g.edges.items()

    # Go through and add the inter-domain links to each domain
    for edge,info in edges:
        nfrom,nto,disc = edge
        nfrom_cid = get_node_cid(nfrom, RES)
        nto_cid = get_node_cid(nto, RES)
        if not nfrom_cid == nto_cid:
            if nto_cid not in RES["ctrl"][nfrom_cid]["dom"]:
                RES["ctrl"][nfrom_cid]["dom"][nto_cid] = []

            RES["ctrl"][nfrom_cid]["dom"][nto_cid].append({
                "sw": nfrom, "sw_to": nto,
                "port": info["src_port"], "port_to": info["dst_port"]
            })

            # Add a capacity attribute to the result dictionary (WIP)
#            if "capacity" in info:
#                cap_str = info["capacity"].lower()
#                if "gbps" in cap_str:
#                    print(cap_str)
#                    print("GBPS")
#                elif "mbps" in cap_str:
#                    print(cap_str)
#                    print("MBPS")
#                else:
#                    print(cap_str)

    with open(args.output, "w+") as f:
        json.dump(RES, f)
