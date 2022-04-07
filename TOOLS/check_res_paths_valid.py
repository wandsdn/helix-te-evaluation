#!/usr/bin/python

""" Script that ensures the path dump files produced by YATES (stored in the
results folder) are valid. Paths for each source-destination pair should
contain valid end-to-end links for every hop (no packet teleportation).

Usage:
    ./check_yates_paths.py --topo <topo> --path [path] --folder_path \
        [folder_paths]

        <topo> - YATES topology file in either .dot or .gml format
        [path] - Validate a single YATES path file
        [folder_path] - Validate a folder of YATES path files

    Either a [path] or [folder_path] has to be specified !
"""

import os
import sys
import re
from networkx.drawing.nx_pydot import read_dot as dot
from networkx import read_gml as gml
from networkx.exception import NetworkXError
from argparse import ArgumentParser


def __validate_path_file(g, fpath):
    """ Validate a path file. If file contains invalid paths, information is
    printed to the console.

    Args:
        g (networkx.Graph): Topology of network, used to ensure links exist
        fpath (str): YATES path dump file we are checking

    Returns:
        int, int: Total num of checked paths, num of invalid paths
    """
    # Process the paths
    total_paths = 0
    error_paths = 0
    with open(fpath, "r") as f:
        hkey = None
        path = None
        for line in f:
            line = line.strip()
            if line == "":
                continue

            # If this is a path key line extract it
            res = re.search("(\w+)\s+->\s+(\w+)\s+:", line)
            if res:
                hkey = (res.groups()[0], res.groups()[1])
                continue

            # If this is a path line process it
            res = re.search("\[([^\]]+)\]\s+@", line)
            if res:
                total_paths += 1
                path = res.groups()[0]
                path = path.split(")")
                prev_n = None
                first_node = None
                is_error = False

                # Go through the links in the path
                for n in path:
                    if n == "":
                        continue

                    n = n.split("(")[1]
                    n = n.strip().split(",")
                    n = (n[0], n[1])
                    if prev_n:
                        # If first node in current link does not match previous
                        # raise an error (invalid link)
                        if prev_n != n[0]:
                            print("%s | Path %s-%s dosen't connect at (%s, %s)" %
                                        (fpath, hkey[0], hkey[1], n[0], n[1]))
                            error_paths += 1
                            is_error = True
                            break
                    else:
                        first_node = n[0]

                    # Make sure the link specified by the tuple exists
                    if n[1] not in g.neighbors(n[0]):
                        print("%s | Path %s-%s link (%s, %s) dosen't exist" %
                                        (fpath, hkey[0], hkey[1], n[0], n[1]))
                        error_paths += 1
                        is_error = True
                        break

                    prev_n = n[1]

                # Make sure that the path starts at src and ends at dst
                if not is_error:
                    if not first_node == hkey[0]:
                        print("%s | Path %s-%s dosen't start with src host" %
                                                    (fpath, hkey[0], hkey[1]))
                        error_paths += 1
                    elif not prev_n == hkey[1]:
                        print("%s | Path %s-%s dosen't end with dest host" %
                                                    (fpath, hkey[0], hkey[1]))
                        error_paths += 1

    # Return the stats
    return total_paths, error_paths


def guess_topo_format(fpath):
    """ Try to guess the topology file `fpath` format. First try to load
    topo as a '.dot' file and then a '.gml' file (if first load fails).

    Args:
        fpath (str): Path to the topology file we want to load

    Returns:
        networkx.Graph: Loaded graph object or None if invalid
    """
    # Try to load the topology as a dot file format
    g = None
    try:
        g = dot(path=fpath)
    except:
        g = None

    # Try to load the topology as GML
    if g is None:
        try:
            g = gml(path=fpath)
        except NetworkXError:
            g = None
    return g


if __name__ == "__main__":
    # Process and validate required arguments
    parser = ArgumentParser()
    parser.add_argument("--topo", required=True, type=str,
                            help="Topology file (either .dot or .gml)")
    parser.add_argument("--path", required=False, type=str, default=None,
                            help="Check single YATES path dump file")
    parser.add_argument("--folder_path", required=False, type=str, default=None,
                            help="Check all YATES path dumps in folder")
    args = parser.parse_args()
    if args.path is None and args.folder_path is None:
        print("Please provide either a path or folder_path argument")
        exit(1)

    # Load the topology
    g = guess_topo_format(args.topo)
    if g is None:
        print("Topology format is invalid (need valid .dot or .gml file)!")
        exit(1)

    files_processed = 0
    total_paths = 0
    error_paths = 0
    if args.folder_path is None:
        # Process a single file
        if not os.path.isfile(args.path):
            print("Path '%s' is invalid (not a file)!" % args.path)
            exit(1)

        files_processed += 1
        total_paths, error_paths = __validate_path_file(g, args.path)
    else:
        if not os.path.isdir(args.folder_path):
            print("Folder path '%s' is invalid (not a folder)!" %
                                                    args.folder_path)
            exit(1)

        # Process multiple files in a folder
        for name in os.listdir(args.folder_path):
            fpath = os.path.join(args.folder_path, name)
            if not os.path.isfile(fpath):
                continue

            tp, ep = __validate_path_file(g, fpath)
            files_processed += 1
            total_paths += tp
            error_paths += ep

    # Output the returned stats
    print("-" * 50)
    print("FILES PROCESSED      : %d" % files_processed)
    print("TOTAL NUMBER OF PATHS: %d" % total_paths)
    print("TOTAL INVALID PATHS  : %d" % error_paths)

    # Compute and output the error rate
    err_rate = 0.0
    if total_paths > 0:
        err_rate = float(error_paths) / float(total_paths) * 100
    print("ERROR RATE           : %.2f%%" % err_rate)
