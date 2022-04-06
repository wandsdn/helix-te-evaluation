# README #

This repo contains a modified version of [YATES](https://cornell-netlab.github.io/yates/)
that adds support for evaluating reactive TE optimisation systems that
collect statics and perform real-time decisions during a YATES simulation.

This framework was used to evaluate Helix’s TE optimisation performance. Helix
is a hierarchical multi-controller SDN (MCSDN) system. Part of the results
contained in this repo (AT&T MPLS and Abilene topology results) were discussed
in the SOSR2021 paper 
[“Helix: Traffic Engineering for Multi-Controller SDN”](https://doi.org/10.1145/3482898.3483354).

The base YATES code was forked from the current 
[master](https://github.com/cornell-netlab/yates/commit/138242186975a8f5bcfe5518903108e6a6666f22) 
branch (12 Sep 2019).



## Repo Overview ##

This section contains an overview of the items contained in this repo. For a
description of YATES, please refer to the [YATES user documentation](https://cornell-netlab.github.io/yates/).
 

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

2) Clone the Helix repo: `git clone https://github.com/wandsdn/helix.git`

3) Open `lib/routing/Helix_Helper.ml` and update the `helix_source_path`
   variable with the current path to the cloned Helix source-code repo.
   For example, if Helix source-code repo was cloned in same parent folder
   as this repo, modify the variable to: `let helix_source_path = "../helix/"`

4) Recompile YATES: `make clean install`.
