#!/bin/bash

# AT&T MPLS topology evaluation experiments
yates RESULTS/Experiments/ATTMPLS.dot RESULTS/Experiments/ATTMPLS.demand.txt RESULTS/Experiments/ATTMPLS.demand.txt \
	RESULTS/Experiments/ATTMPLS.hosts -out ATTMPLS_500_Helix -budget 1 \
	-simtime 500 -scale 500 -helix-sw-ctrl-map-file RESULTS/Experiments/ATTMPLS.map.json \
	-helix-te-opti-method CSPFRecomp -helix-te-thresh 0.95 \
	-helix-te-opti-wait 1 -helix-poll-wait 100 -helix-te-paccept true \
	-helixMC;

yates RESULTS/Experiments/ATTMPLS.dot RESULTS/Experiments/ATTMPLS.demand.txt RESULTS/Experiments/ATTMPLS.demand.txt \
	RESULTS/Experiments/ATTMPLS.hosts -out ATTMPLS_550_Helix -budget 1 \
	-simtime 500 -scale 550 -helix-sw-ctrl-map-file RESULTS/Experiments/ATTMPLS.map.json \
	-helix-te-opti-method CSPFRecomp -helix-te-thresh 0.95 \
	-helix-te-opti-wait 1 -helix-poll-wait 100 -helix-te-paccept true \
	-helixMC;

yates RESULTS/Experiments/ATTMPLS.dot RESULTS/Experiments/ATTMPLS.demand.txt RESULTS/Experiments/ATTMPLS.demand.txt \
	RESULTS/Experiments/ATTMPLS.hosts -out ATTMPLS_600_Helix -budget 1 \
	-simtime 500 -scale 600 -helix-sw-ctrl-map-file RESULTS/Experiments/ATTMPLS.map.json \
	-helix-te-opti-method CSPFRecomp -helix-te-thresh 0.95 \
	-helix-te-opti-wait 1 -helix-poll-wait 100 -helix-te-paccept true \
	-helixMC;



yates RESULTS/Experiments/ATTMPLS.dot RESULTS/Experiments/ATTMPLS.demand.txt RESULTS/Experiments/ATTMPLS.demand.txt \
	RESULTS/Experiments/ATTMPLS.hosts -out ATTMPLS_500_CSPF_MCF -budget 1 \
	-simtime 500 -scale 500 -cspf -mcf;

yates RESULTS/Experiments/ATTMPLS.dot RESULTS/Experiments/ATTMPLS.demand.txt RESULTS/Experiments/ATTMPLS.demand.txt \
	RESULTS/Experiments/ATTMPLS.hosts -out ATTMPLS_550_CSPF_MCF -budget 1 \
	-simtime 500 -scale 550 -cspf -mcf;

yates RESULTS/Experiments/ATTMPLS.dot RESULTS/Experiments/ATTMPLS.demand.txt RESULTS/Experiments/ATTMPLS.demand.txt \
	RESULTS/Experiments/ATTMPLS.hosts -out ATTMPLS_600_CSPF_MCF -budget 1 \
	-simtime 500 -scale 600 -cspf -mcf;



yates RESULTS/Experiments/ATTMPLS.dot RESULTS/Experiments/ATTMPLS.demand.txt RESULTS/Experiments/ATTMPLS.demand.txt \
	RESULTS/Experiments/ATTMPLS.hosts -out ATTMPLS_500_ECMP_VLB_Raeke -budget 4 \
	-simtime 500 -scale 500 -ecmp -raeke -vlb;

yates RESULTS/Experiments/ATTMPLS.dot RESULTS/Experiments/ATTMPLS.demand.txt RESULTS/Experiments/ATTMPLS.demand.txt \
	RESULTS/Experiments/ATTMPLS.hosts -out ATTMPLS_550_ECMP_VLB_Raeke -budget 4 \
	-simtime 500 -scale 550 -ecmp -raeke -vlb;

yates RESULTS/Experiments/ATTMPLS.dot RESULTS/Experiments/ATTMPLS.demand.txt RESULTS/Experiments/ATTMPLS.demand.txt \
	RESULTS/Experiments/ATTMPLS.hosts -out ATTMPLS_600_ECMP_VLB_Raeke -budget 4 \
	-simtime 500 -scale 600 -ecmp -raeke -vlb;



yates RESULTS/Experiments/ATTMPLS.dot RESULTS/Experiments/ATTMPLS.demand.txt RESULTS/Experiments/ATTMPLS.demand.txt \
	RESULTS/Experiments/ATTMPLS.hosts -out ATTMPLS_500_SemiMCFKSP -budget 2 \
	-simtime 500 -scale 500 -semimcfksp

yates RESULTS/Experiments/ATTMPLS.dot RESULTS/Experiments/ATTMPLS.demand.txt RESULTS/Experiments/ATTMPLS.demand.txt \
	RESULTS/Experiments/ATTMPLS.hosts -out ATTMPLS_550_SemiMCFKSP -budget 2 \
	-simtime 500 -scale 550 -semimcfksp;

yates RESULTS/Experiments/ATTMPLS.dot RESULTS/Experiments/ATTMPLS.demand.txt RESULTS/Experiments/ATTMPLS.demand.txt \
	RESULTS/Experiments/ATTMPLS.hosts -out ATTMPLS_600_SemiMCFKSP -budget 2 \
	-simtime 500 -scale 600 -semimcfksp;


# --------------------------------------------------------------------------------------


# Abilene topology evaluation experiments
yates RESULTS/Experiments/Abilene.dot RESULTS/Experiments/Abilene.demand.txt RESULTS/Experiments/Abilene.demand.txt \
	RESULTS/Experiments/Abilene.hosts -out Abilene_2.2_Helix_CSPF_MCF -scale 2.2 -budget 1 \
	-simtime 500 -helix-sw-ctrl-map-file RESULTS/Experiments/Abilene.map.json \
	-helix-te-opti-method CSPFRecomp -helix-te-thresh 0.95 -helix-te-opti-wait 1 \
	-helix-poll-wait 100 -helix-te-paccept true -helixMC -cspf -mcf

yates RESULTS/Experiments/Abilene.dot RESULTS/Experiments/Abilene.demand.txt RESULTS/Experiments/Abilene.demand.txt \
	RESULTS/Experiments/Abilene.hosts -out Abilene_2.8_Helix_CSPF_MCF -scale 2.8 -budget 1 \
	-simtime 500 -helix-sw-ctrl-map-file RESULTS/Experiments/Abilene.map.json \
	-helix-te-opti-method CSPFRecomp -helix-te-thresh 0.95 -helix-te-opti-wait 1 \
	-helix-poll-wait 100 -helix-te-paccept true -helixMC -cspf -mcf

yates RESULTS/Experiments/Abilene.dot RESULTS/Experiments/Abilene.demand.txt RESULTS/Experiments/Abilene.demand.txt \
	RESULTS/Experiments/Abilene.hosts -out Abilene_3.0_Helix_CSPF_MCF -scale 3.0 -budget 1 \
	-simtime 500 -helix-sw-ctrl-map-file RESULTS/Experiments/Abilene.map.json \
	-helix-te-opti-method CSPFRecomp -helix-te-thresh 0.95 -helix-te-opti-wait 1 \
	-helix-poll-wait 100 -helix-te-paccept true -helixMC -cspf -mcf



yates RESULTS/Experiments/Abilene.dot RESULTS/Experiments/Abilene.demand.txt RESULTS/Experiments/Abilene.demand.txt \
    RESULTS/Experiments/Abilene.hosts -out Abilene_2.2_ECMP_VLB_Raeke -scale 2.2 -budget 4 \
    -simtime 500 -ecmp -vlb -raeke;

yates RESULTS/Experiments/Abilene.dot RESULTS/Experiments/Abilene.demand.txt RESULTS/Experiments/Abilene.demand.txt \
    RESULTS/Experiments/Abilene.hosts -out Abilene_2.8_ECMP_VLB_Raeke -scale 2.8 -budget 4 \
    -simtime 500 -ecmp -vlb -raeke;

yates RESULTS/Experiments/Abilene.dot RESULTS/Experiments/Abilene.demand.txt RESULTS/Experiments/Abilene.demand.txt \
    RESULTS/Experiments/Abilene.hosts -out Abilene_3.0_ECMP_VLB_Raeke -scale 3.0 -budget 4 \
    -simtime 500 -ecmp -vlb -raeke;



yates RESULTS/Experiments/Abilene.dot RESULTS/Experiments/Abilene.demand.txt RESULTS/Experiments/Abilene.demand.txt \
    RESULTS/Experiments/Abilene.hosts -out Abilene_2.2_SemiMCFKSP -scale 2.2 -budget 2 \
    -simtime 500 -semimcfksp;

yates RESULTS/Experiments/Abilene.dot RESULTS/Experiments/Abilene.demand.txt RESULTS/Experiments/Abilene.demand.txt \
    RESULTS/Experiments/Abilene.hosts -out Abilene_2.8_SemiMCFKSP -scale 2.8 -budget 2 \
    -simtime 500 -semimcfksp;

yates RESULTS/Experiments/Abilene.dot RESULTS/Experiments/Abilene.demand.txt RESULTS/Experiments/Abilene.demand.txt \
    RESULTS/Experiments/Abilene.hosts -out Abilene_3.0_SemiMCFKSP -scale 3.0 -budget 2 \
    -simtime 500 -semimcfksp;



# --------------------------------------------------------------------------------------


# Hibernia Global topology evaluation experiments
yates RESULTS/Experiments/Hibernia.dot RESULTS/Experiments/Hibernia.demand.txt \
    RESULTS/Experiments/Hibernia.demand.txt RESULTS/Experiments/Hibernia.hosts \
    -budget 1 -simtime 500 -scale 300 -helix-sw-ctrl-map RESULTS/Experiments/Hibernia.Helix.map.json \
    -helix-te-opti-method CSPFRecomp -helix-te-thresh 0.95 -helix-te-opti-wait 1 \
    -helix-poll-wait 100 -helix-te-paccept true -out Hibernia_300_Helix_CSPF -cspf -helixMC

yates RESULTS/Experiments/Hibernia.dot RESULTS/Experiments/Hibernia.demand.txt \
    RESULTS/Experiments/Hibernia.demand.txt RESULTS/Experiments/Hibernia.hosts \
    -budget 1 -simtime 500 -scale 350 -helix-sw-ctrl-map RESULTS/Experiments/Hibernia.Helix.map.json \
    -helix-te-opti-method CSPFRecomp -helix-te-thresh 0.95 -helix-te-opti-wait 1 \
    -helix-poll-wait 100 -helix-te-paccept true -out Hibernia_350_Helix_CSPF -cspf -helixMC

yates RESULTS/Experiments/Hibernia.dot RESULTS/Experiments/Hibernia.demand.txt \
    RESULTS/Experiments/Hibernia.demand.txt RESULTS/Experiments/Hibernia.hosts \
    -budget 1 -simtime 500 -scale 400 -helix-sw-ctrl-map RESULTS/Experiments/Hibernia.Helix.map.json \
    -helix-te-opti-method CSPFRecomp -helix-te-thresh 0.95 -helix-te-opti-wait 1 \
    -helix-poll-wait 100 -helix-te-paccept true -out Hibernia_400_Helix_CSPF -cspf -helixMC



yates RESULTS/Experiments/Hibernia.dot RESULTS/Experiments/Hibernia.demand.txt \
    RESULTS/Experiments/Hibernia.demand.txt RESULTS/Experiments/Hibernia.hosts \
    -budget 1 -simtime 500 -scale 300 -helix-sw-ctrl-map RESULTS/Experiments/Hibernia.HelixMC.map.json \
    -helix-te-opti-method CSPFRecomp -helix-te-thresh 0.95 -helix-te-opti-wait 1 \
    -helix-poll-wait 100 -helix-te-paccept true -out Hibernia_300_HelixMC -helixMC

yates RESULTS/Experiments/Hibernia.dot RESULTS/Experiments/Hibernia.demand.txt \
    RESULTS/Experiments/Hibernia.demand.txt RESULTS/Experiments/Hibernia.hosts \
    -budget 1 -simtime 500 -scale 350 -helix-sw-ctrl-map RESULTS/Experiments/Hibernia.HelixMC.map.json \
    -helix-te-opti-method CSPFRecomp -helix-te-thresh 0.95 -helix-te-opti-wait 1 \
    -helix-poll-wait 100 -helix-te-paccept true -out Hibernia_350_HelixMC -helixMC

yates RESULTS/Experiments/Hibernia.dot RESULTS/Experiments/Hibernia.demand.txt \
    RESULTS/Experiments/Hibernia.demand.txt RESULTS/Experiments/Hibernia.hosts \
    -budget 1 -simtime 500 -scale 400 -helix-sw-ctrl-map RESULTS/Experiments/Hibernia.HelixMC.map.json \
    -helix-te-opti-method CSPFRecomp -helix-te-thresh 0.95 -helix-te-opti-wait 1 \
    -helix-poll-wait 100 -helix-te-paccept true -out Hibernia_400_HelixMC -helixMC



yates RESULTS/Experiments/Hibernia.dot RESULTS/Experiments/Hibernia.demand.txt \
    RESULTS/Experiments/Hibernia.demand.txt RESULTS/Experiments/Hibernia.hosts \
    -budget 1 -simtime 500 -scale 300 -out Hibernia_300_MCF -mcf

yates RESULTS/Experiments/Hibernia.dot RESULTS/Experiments/Hibernia.demand.txt \
    RESULTS/Experiments/Hibernia.demand.txt RESULTS/Experiments/Hibernia.hosts \
    -budget 1 -simtime 500 -scale 350 -out Hibernia_350_MCF -mcf

yates RESULTS/Experiments/Hibernia.dot RESULTS/Experiments/Hibernia.demand.txt \
    RESULTS/Experiments/Hibernia.demand.txt RESULTS/Experiments/Hibernia.hosts \
    -budget 1 -simtime 500 -scale 400 -out Hibernia_400_MCF -mcf
