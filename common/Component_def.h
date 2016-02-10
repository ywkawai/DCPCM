#ifndef COMPONENT_DEF_H
#define COMPONENT_DEF_H

#define NUM_DCPCM_COMP 2
#define COUPLER_LOG_LEVEL 0
#define COUPLER_LOG_STDERROR .false.

#define AGCM_COMP_NAME  "DCPAM"
#define AGCM_NX         64
#define AGCM_NY         32
#define AGCM_NZ         26
#define AGCM_NMAX       21
#define AGCM_DELTIME    1800d0

#define OGCM_COMP_NAME  "Dennou-OGCM"
#define OGCM_NX         1
#define OGCM_NY         64
#define OGCM_NZ         61
#define OGCM_NMAX       42
#define OGCM_DELTIME    14400d0

#define ATMOCN_COUPLING_CYCLE 14400d0

#define GRIDMAPFILE_AO_NAME "/home/ykawai/workspace/DCPCM/run/com_dir/common/gmap-ATM_T21-OCN_Pl42.dat"
#define GRIDMAPFILE_OA_NAME "/home/ykawai/workspace/DCPCM/run/com_dir/common/gmap-OCN_Pl42-ATM_T21.dat"

#endif
