# README #

This repo contains a modified version of [YATES](https://cornell-netlab.github.io/yates/)
that adds support for evaluating reactive TE optimisation systems that
collect statics and perform real-time decisions during a YATES simulation.

This framework was used to evaluate Helix’s TE optimisation performance. Helix
is a hierarchical multi-controller SDN (MCSDN) system. Part of the results
contained in this repo (AT&T MPLS and Abilene topology results) were discussed
in the SOSR2021 paper 
["Helix: Traffic Engineering for Multi-Controller SDN"](https://doi.org/10.1145/3482898.3483354).

The base YATES code was forked from the current 
[master](https://github.com/cornell-netlab/yates/tree/138242186975a8f5bcfe5518903108e6a6666f22) 
branch (12 Sep 2019).



## Repo Overview ##

This section contains an overview of the items contained in this repo. For a
description of YATES, please refer to the [YATES user documentation](https://cornell-netlab.github.io/yates/).

* `HelixMC_TEST`
    * Helix multi-controller test scenario files (_see "Helix Module and
      Wrapper Integration Tests" section for more details_).
* `run_helixMC_path_tests.sh`
    * Helix integration test scripts using FirstSol TE optimisation method
* `run_helixMC_CSPF_path_tests.sh`
    * Helix intergration tests scripts using CSPFRecomp TE optimisation method

* `TOOLS/`
    * Folder that contains tools that simplify the process to create Helix
      switch-to-controller map files and allow checking if a TE system
      generates valid paths.
* `TOOLS/sw_ctrl_map_gen.py`
    * Script that simplifies the process to create switch-to-controller map
      files. The script resolves inter-area links based on a provided topology
      and a simple controller switch association text file that specifies what
      controller manages a device. _Please refer to the "Switch-to-Controller
      Map File" section for a description of the required association text
      file and switch-to-controller map file syntax._
* `TOOLS/check_res_path_valid.py`
    * Script that checks if a TE algorithm is generating valid paths. A path is
      valid if its edges exist in the topology file (nodes of edges are
      adjacent and interconnected).
    * **NOTE: When providing YATES with a routing scheme, YATES does not check
      if two nodes of an edge are available in the topology.** As such, a TE
      system can specify edges that do not exist in the topology (e.g. contain
      the source and destination nodes) causing packets to seemingly teleport.
      Refer to the MagicRouting YATES algorithm module for an example of this
      behaviour. This script was built to validate the collected results by
      ensuring that a TE system is forwarding traffic on nodes that exist in
      the topology.
    * During an experiment, YATES outputs the routing scheme generated by a TE
      system in the `paths/` output results folder. This script can either
      validate a single path file, or a complete folder. Please use command
      `./check_res_paths_valid.py --help` to view script arguments. This script
      should be used in conjunction with the `-log-path` YATES argument which
      causes YATES to dump the routing scheme used during each experiment run.


### TE Evaluation Results Folder ###

* `RESULTS/`
    * Folder that contains raw and processed results used to evaluate Helix’s
      TE optimisation performance. _Please refer to the "Helix TE Experiment
      Results" section for a full description of the files and experiment
      collection script_.
* `RESULTS/ConLoss/`
    * Folder that contains processed congestion loss rates for all three
      topologies. The sub folders contain the data CDF files used by the
      GNUPLOT scripts to create the TE evaluation results graphs. Each set of
      results is selected into a sub-folder that follows the naming convention
      `<topo_name>_<scale>` where `<topo_name>` represents the name of the
      topology used in the simulation and `<scale>` the TE matrix traffic
      multiplier used in the experiments. For example, the `ATTMPLS_500`
      sub-folder contains the processed results for the AT&T MPLS topology
      experiments collected using a 500x traffic multiplier.
* `RESULTS/PathChurn/`
    * Folder that contains the processed path change churn metrics for all
      three topologies. Sub-folders follow the same naming convention as the
      processed congestion loss results.
* `RESULTS/RAW/`
    * Folder that contains the unprocessed congestion loss and path change
      churn YATES metric files collected for the TE evaluation. Results are
      separated into sub-folders that contain the data collected for a single
      YATES experiment. The sub-folders follow the naming convention
      `<topo_name>_<scale>_<algorithms>` where `<algorithms>` represents a
      underscore separated list of algorithms that specify what system’s
      results are constrained in the YATES metric files. For example,
      `ATTMPLS_550_ECMP_VLB_Raeke` contains the results collected for the ECMP,
      VLB and Raeke systems on the AT&T MPLS topology using a x550 traffic
      multiplier.
    * The `CongestionLossVsIterations.dat` contains the RAW congestion loss
      metrics collected during the simulation and `TMChurnVsIterations.dat` the
      raw path change churn metric.
    * _NOTE: For the Hibernia result sets, Helix is used to denote the
      single-controller Helix instance and HelixMC the multi-controller
      deployment results collected on the hibernia network._
* `RESULTS/Experiments/`
    * Folder that contains the topology, demand, host and map files used to
      collect results. The files contained in the folder are referenced by the
      collection script present in the root of the repo.
* `RESULTS/<topo_name>_<scale>.p`
    * GNUPLOT script that uses the processed congestion loss and path change
      churn data to generate the TE evaluation result graphs.

* `run_helix_TE_evaluation.sh`
    * Collection script that executes all experiments used in the Helix TE
      evaluation. This script uses the experiment files contained in the
      `RESULTS` folder. _Please refer to the "Helix TE Experiment Results"
      section for a full description of the metric files generated by the script.


### Helix TE Algorithm Modules ###

* `lib/routing/Helix.ml _DEPRECATED_`
    * YATES Helix single-controller module that simulates Helix’s behaviour
      by calling the Helix wrapper from the Helix controller repo. All Helix
      modules maintain local system state and perform operations such as 
      compute paths and optimise the network by calling the wrapper.
     * Module is partially deprecated. To collect single-controller Helix
       results use the HelixMC and provide the module a switch-to-controller
       map that assigns all switches in the network to a single LC.
* `lib/routing/Helix_Helper.ml`
    * Helix helper module that contains usefull methods to serialise state and
      call the wrapper scripts.
* `lib/routing/HelixMC.ml`
    * YATES Helix multi-controller module. Calls the Helix multi-controller
      wrappers to perform TE optimisation and compute paths. Requires
      a switch-to-controller map file.
* `lib/routing/HelixMC_Helper.ml`
    * Helix multi-controller helper module.



## Installation and Dependecies ##

Please refer to [YATES repo](https://github.com/cornell-netlab/yates) for
YATES dependencies and installation procedure.

To use the Helix algorithms (Helix, HelixMC, and HelixNoOpti), this repo
requires the [Helix controller source code](https://github.com/wandsdn/helix).

After cloning the repo, you need to update the Helix wrapper base folder path
and recompile the modules. The installation process is as follows:

1) Install YATES depedencies based on [instructions](https://github.com/cornell-netlab/yates).

2) Clone the Helix repo:

```
git clone https://github.com/wandsdn/helix.git
```

3) Open `lib/routing/Helix_Helper.ml` and update the `helix_source_path`
   variable with the current path to the cloned Helix source-code repo.
   For example, if Helix source-code repo was cloned in same parent folder
   as this repo, modify the variable to:

```ocaml
let helix_source_path = "../helix/"
```

4) Recompile YATES:

```
make clean install
```



## Extra YATES Arguments ##

This section contains an overview of the extra Helix YATES arguments added to
the simulator to evaluating Helix's TE optimisation performance.
d
Display YATES help: `yates -help`

| Argument                              | Description
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| `-helix`                              | (_DEPRECATED_) Run Helix single-controller |
| `-helixMC`                            | Run Helix multi-controller |
| `-helixNoOpti`                        | Run Helix single-controller with no TE optimisation |
| `-helix-sw-ctrl-map-file <path>`      | HelixMC switch to controller mapping file path |
| `-helix-te-thresh <float>`            | Helix TE-threshold (_default 0.95_ for 95%) |
| `-helix-poll-wait <int>`              | Poll interval length. Wait n-iterations before checking for congestion (_default 1_) |
| `-helix-te-opti-wait <int>`           | Wait n-iterations before applying a TE routing scheme update (_default 100_) |
| `-helix-te-opti-method <name>`        | Helix TE optimisation method: CSPFRecomp (_default_), FirstSol, BestSolUsage, BestSolPLen |
| `-helix-te-paccept <bool>`            | Should Helix accept partial TE solutions (_default true_) |
| `-helix-te-candidate-sort-rev <bool>` | Should Helix sort candidates in descending order (_default True_, consider heavy hitters first) |
| `-helix-te-pot-path-sort-rev <bool>`  | Should Helix sort potential path changes in descending order (_defaults False_). Only applies to some TE optimisaiton methods. |
| `-helix-recomp-scheme <bool>`         | Should Helix recompute all its paths every run and start with a clean slate during the simulation (_default false_) |


For example, if we wish to evaluate Helix on topology `t.dot` using demand
`d.txt` and applying the following Helix TE arguments:

* `a.json` switch-to-controller map file
* TE threshold of 95%
* Consider heavy hitter first (candidates generating most traffic)
* Use a 100 iterations poll interval

We will use the command:


```
yates t.dot d.txt d.txt t.hosts -helixMC -helix-sw-ctrl-map-file a.json \
    -helix-te-thresh 0.95 -helix-te-candidate-sort-rev true \
    -helix-poll-wait 100
```



## Helix TE Experiment Results ##

All raw and processed TE evaluation results can be found in the `RESULTS`
directory. The results present in the folder were collected using the
`run_helix_TE_evaluation.sh` script. After performing a simulation YATES stores
all metrics in the `data/results/` folder. The metric folders follow the same
naming convetion used for the `RESULTS/RAW/` sub-folders.

The TE evaluation results results contains nine GNUPLOT scripts that use the
processed YATES metrics (from the `ConLoss/` and `PathChurn/` folders) to
generate the TE evaluation results graphs. To generate the graphs use the
command `gnuplog <topo_name>_<scale_factor>.p`. For example to generate the
graphs for the Hibernia 400x scale factor evaluation experiments, use the
command `gnuplot Hibernia_400.p`. Running these GNUPLOT files will generate
a SVG image.



## Helix Module and Wrapper Integration Tests ##

The repo contains several integration tests that ensure that the Helix module
and wrappers behave as expected. The tests contained in the repo use the
outlined [“Helix Multi-Controller Test Scenarios”](https://github.com/wandsdn/helix/tree/master/MCTestScenario_Config).
The tests will run several simulations using Helix, collecting results and
logging the routing scheme after each experiment run (uses the `-log-path`
YATES attribute). After the simulation finishes, the tests use the
`TOOLS/check_res_paths_expected.py` script to validate that the produced Helix
routing scheme after each run is correct. For more information on how the check
results path scripts works, please refer to the TOOLS folder read-me file.

All topology, demand and expected path files for the test scenarios are
available in the `HelixMC_TEST` folder contained in the root of the folder.
The scenario files use the naming convention `<mc_scenario>.<type>.<ext>`
where `<mc_scenario>` represents the multi-controller scenario (e.g. `mc_v2`)
and `<type>.<ext>` the particular file type. For example, `mc_v4.dot` contains
the topology used for the V4 multi-controller tests scenario.

To run the tests use the scripts `run_helixMC_path_tests.sh` and
`run_helixMC_CSPF_path_tests.sh`. The output of the scripts will specify the
number of incorrect paths for each scenario.

_NOTE_: The tests generate several temporary log files and YATES serialisation
files. The log files of the experiments are written to the root of the repo in
files that follow the naming convention `<mc_scenario>.out.txt` and
`<mc_scenario>.cspf.out.txt` for the CSPFRecomp TE method tests.



## Helix Temporary Files ##

The Helix YATES module generates several temporary JSON files  in the root of
the repo which are used to pass state to the wrappers. The files follow the
naming convention `<CID>.<type>.json` for state passed to a particular
controller instance. For example, `c1.topo.json` contains the local topology
visible to the c1 controller (Area 1 topology).


## Switch-to-Controller Map File ##

The HelixMC module requires a switch to controller mapping file to perform
multi-controller evaluation experiments. The file follows the same syntax as
the multi-controller map file used by the [Helix emulation frameworks[(https://github.com/wandsdn/helix#switch-to-controller-mapping-file-syntax).
Because the YATES module does not start multiple instances, the Helix YATES
mapping file does not need a "extra_instances" field under the local-controller
information object.

The `TOOLS/sw_ctrl_map_gep.py` script simplifies the process of generating map
files. The script requires a DOT formated topology file (used by YATES for
experiments) and a simple text file that specifies controller IDs and the
switches they manage. The script will automatically resolve any inter-area
links based on the provided topology and controller information. To view the
arguments of the script run command `./sw_ctrl_map_gep.py --help`.

For example if we have a simple topology containing three nodes `sw1, sw2, sw3`
(connected in sequence) and we wish to map `c1` to `sw1` and `c2` to
`sw2 and sw3`, we provide the map script with the following map text file:

```text
c1|sw1
c2|sw2,sw3
```


The script will automatically resolve the inter-area link (`sw2 <-> sw3`) and
produce the following JSON switch-to-controller map file:


```json
{
    "root": {"r1": {}},
    "ctrl": {
        "c1": {"sw": ["sw1"], "host": ["h1"],
            "dom": {
                "c2": [{"sw" "sw1", "port": 1, "sw_to": "sw2", "port_to": 1}] 
            }
        },
        "c2": {"sw": ["sw2", "sw3"], "host": ["h2", "h3"],
            "dom": {
                "c1": [{"sw" "sw2", "port": 1, "sw_to": "sw1", "port_to": 1}] 
            }
        }
    }
}
```

