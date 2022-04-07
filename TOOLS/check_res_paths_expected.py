#!/usr/bin/python

""" Script that ensures the path dump files produced by YATES (stored in the
results folder) are as expected. Dump path files are compared against a JSON
object of expected paths which defines the paths for each iteration (TE
matrix line).

Use this script on result folders generated using the YATES 'log-paths' flag.
The file will output current paths after each iteration of the simulator.
Dumped path files stored in result folder under directory 'paths/'. Syntax
of dump path file name is "<algorithm name>_<iteration>".

NOTE: YATES logs paths at the begining of each iteration (before optimisations
      are triggered. Reactive TE algorithms will also log paths at the end of
      the simulation. For example, iteration 0 of reactive TE algos contain
      paths generated at the start of the run, while iteration 1 represents
      path produced after optimising for the first TE matrix line.

Usage:
    ./check_res_paths_expected.py --expected <expec> --folder_path <fold_path>
            <expec> - Path to expected state JSON file
            <fold_path> - Path to folder containg YATES path dump files
"""

import os
import sys
import re
import json
from argparse import ArgumentParser


def __validate_path_file(fpath, expected):
    """ Ensure that a YATES path dump file `fpath` matches the expected
    state `expected`.

    NOTE: This method dosen't ensure paths are complete end-to-end, but
          rather that they match the expected state. We do this to allow
          checking for failure scenarios as well as sucesfull scenarios.

    Args:
        fpath (str): Path to the file we are checking
        expected (dict): Expected state of all paths

    Returns:
        int, int: Total number of paths checked, number of invalid paths
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

                # Clean the path string
                last_node = None
                path_tmp = []
                for n in path:
                    if n == "":
                        continue
                    n = n.split("(")[1]
                    n = n.strip().split(",")
                    path_tmp.append(n[0])
                    last_node = n[1]

                if last_node is not None:
                    path_tmp.append(last_node)
                path = path_tmp

                hkey_str = "%s-%s" % hkey
                if hkey_str not in expected:
                    print("\t(%s) Can't find expected key %s" %
                                                    (fpath, hkey_str))
                    error_paths += 1
                    continue
                if not path == expected[hkey_str]:
                    print("(%s) Path %s dosen't match expected" %
                                                    (fpath, hkey_str))
                    print("\tACTUAL  : %s !=" % path)
                    print("\tEXPECTED: %s" % expected[hkey_str])
                    error_paths += 1

    if total_paths != len(expected):
        print ("(%s) Error, mismatched number of paths (%s != %s)" %
                    (path, total_paths, len(expected)))

    # Return the stats
    return total_paths, error_paths


if __name__ == "__main__":
    # Process the arguments and validate them
    parser = ArgumentParser()
    parser.add_argument("--expected", required=True, type=str,
                            help="Path to expected state JSON file")
    parser.add_argument("--folder_path", required=True, type=str,
                            help="Path to YATES dump folder that contains state")
    args = parser.parse_args()

    # Load the expected state file
    expected = {}
    with open(args.expected, "r") as fin:
        expected = json.load(fin)

    files_processed = 0
    total_paths = 0
    error_paths = 0
    for ctrl_name,ctrl_Data in expected.iteritems():
        # Sort the iterations
        num_scenarios = len(expected[ctrl_name])
        iters = expected[ctrl_name].keys()
        iters.sort(key=int)

        # Iterate through the sorted iteration and process/check files
        for iter in iters:
            data = expected[ctrl_name][iter]
            fname = "%s_%s" % (ctrl_name, iter)
            fpath = os.path.join(args.folder_path, fname)
            if not os.path.isfile(fpath):
                print("Can't find path file %s" % fpath)
                continue

            # Check the file and add to the global stats
            print("Checking iteration %s (%s)" % (iter, fname))
            tp, ep = __validate_path_file(fpath, data)
            files_processed += 1
            total_paths += tp
            error_paths += ep

    # Output the returned stats for the checked files
    print("-" * 50)
    print("FILES PROCESSED      : %d" % files_processed)
    print("TOTAL NUMBER OF PATHS: %d" % total_paths)
    print("TOTAL INVALID PATHS  : %d" % error_paths)

    # Calculate and output the error rate
    err_rate = 0.0
    if total_paths > 0:
        err_rate = float(error_paths) / float(total_paths) * 100
    print("ERROR RATE           : %.2f%%" % err_rate)
