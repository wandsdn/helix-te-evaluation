#!/bin/bash


# ------------------------------------------------------------
# Helper script that executes all Helix multi-controller test scenarios
# for YATES. The script first collects all required results and then
# uses the path check script to validate the results. All config files
# for YATES as well as the expected state should follow the convetion
# '<mc_name>.<file>.<ext>' and be stored in the BASE_FOLDER folder.
# Output from the YATES simulation are stored in LOG_FOLDER in a follow
# named '<mc_name>.out.txt'. YATES results should all be in the folder
# RESULT_FOLDER in a sub-directory with the scenario name.
# ------------------------------------------------------------


# Base path to folder that contains all MDC scenario files
BASE_FOLDER="HelixMC_TEST"
# Path that will hold YATES simulation log output files
LOG_FOLDER="."
# Path that will contain YATES results folders for all simualtions
RESULT_FOLDER="data/results"
# YATES common attributes to use for collecting results for the tests
YATES_ATTR="-helixMC -helix-poll-wait 100 -helix-te-opti-wait 10"
YATES_ATTR="$YATES_ATTR -helix-te-opti-method FirstSol"
YATES_ATTR="$YATES_ATTR -helix-te-candidate-sort-rev false"
YATES_ATTR="$YATES_ATTR -helix-te-pot-path-sort-rev false"
YATES_ATTR="$YATES_ATTR -helix-te-paccept false"
YATES_ATTR="$YATES_ATTR -log-paths"

run_test() {
    # Delete result directory if it alreayd exists
    if [ -d "$RESULT_FOLDER/$1/" ]; then
        echo "Result folder $RESULT_FOLDER/$1/ already exists. Deleting!";
        rm -r "$RESULT_FOLDER/$1";
    fi;

    echo "Collecting results, log path: $LOG_FOLDER/$1.out.txt";
    yates "$BASE_FOLDER/$1.dot" "$BASE_FOLDER/$1.te.txt" \
            "$BASE_FOLDER/$1.te.txt" "$BASE_FOLDER/$1.hosts" $YATES_ATTR \
            -helix-sw-ctrl-map "$BASE_FOLDER/$1.sw_ctrl_map.json" \
            -helix-te-thresh $2 -out $1 > "$LOG_FOLDER/$1.out.txt" 2>&1

    echo "Running check script ...";
    ./TOOLS/check_res_paths_expected.py --expected "$BASE_FOLDER/$1.expected.json" \
        --folder_path "$RESULT_FOLDER/$1/paths";
}



echo "===== MC V2 =====";
run_test "mc_v2" 0.90;
echo "===================";
echo -e "\n";

echo "===== MC V3 =====";
run_test "mc_v3" 0.90;
echo "===================";
echo -e "\n";

echo "===== MC V4 =====";
run_test "mc_v4" 0.80;
echo "===================";
echo -e "\n";

echo "===== MC V5 =====";
run_test "mc_v5" 0.90;
echo "===================";
echo -e "\n";

echo "===== MC V6 =====";
run_test "mc_v6" 0.90;
echo "===================";
echo -e "\n";

echo "===== MC V7 =====";
run_test "mc_v7" 0.90;
echo "===================";
echo -e "\n";
