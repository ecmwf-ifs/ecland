#!/bin/bash
# (C) Copyright 2023- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# Script to run the ecland model on pre-downloaded forcing and initial conditions.

#====== Define paths to libraries and executables

# The following are environment variables that might be required to set for running ecLand on external machines
#export PATH=<Path_to_your_ecland_installation>/ecland-build/bin:$PATH
#export NETCDF_DIR=
#export NETCDF_LIB=
#export NETCDF_INCLUDE=
#export LD_LIBRARY_PATH=

# For ecmwf hpc and intel compiler, the following can be used:
module load prgenv/intel intel/2021.4 python3/3.10.10-01
module load hpcx-openmpi/2.9 netcdf4/4.9.1
#======
export PATH=../../ecland-build/bin:$PATH

#====== Setup (change these as required)
EXPERIMENT_NAME=${1:-"CA-001"}
GROUP=LS-PRACTICALS
FORCING_TYPE=era5
INPUT_DIR=./ecland_input_practicals/
OUTPUT_DIR=./
WORK_DIR=./work/
PERIOD="2022-2022" # this is used in combination to $EXPERIMENT_NAME in the call to ecland-run-experiment

#===============================================

ecland-run-experiment -g ${GROUP}\
                      -t ${FORCING_TYPE}\
                      -i ${INPUT_DIR}\
                      -o ${OUTPUT_DIR}\
                      -s "${EXPERIMENT_NAME}_${PERIOD}"\
                      -w ${WORK_DIR}
