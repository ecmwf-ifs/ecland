#!/usr/bin/env bash
# (C) Copyright 2021- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


set -eu
set -o pipefail
set +v

SCRIPTS_DIR="$( cd $( dirname "${BASH_SOURCE[0]}" ) && pwd -P )"
if [[ $(basename ${SCRIPTS_DIR}) == "bin" ]]; then
  SCRIPTS_DIR="$( cd $( dirname "${SCRIPTS_DIR}/$(readlink "${BASH_SOURCE[0]}")" ) && pwd -P )"
fi

function abs_path {
  builtin echo $(cd "$(dirname -- "${1}")" >/dev/null; pwd -P)/$(basename -- "${1}")
}
function trace () {
  builtin echo -e "+ $@"
  eval "$@"
}


#====== SETUP
source ${SCRIPTS_DIR}/ecland_runtime.sh
echo $PATH
TEST_DIR=$(abs_path ${1})
echo $TEST_DIR
SETUP_FILE=${TEST_DIR}/setup
NAMELIST=${TEST_DIR}/namelist
CONTROL_DIR=${TEST_DIR}/control
WORK_DIR=${WORK_DIR:-$(pwd)/work}
OUTPUT_DIR=${OUTPUT_DIR:-$(pwd)/output}
INPUT_DIR=${INPUT_DIR:-$(pwd)/input}

echo "SETUP_FILE: ${SETUP_FILE}"

source ${SETUP_FILE}

NPROC=${NPROC:-1}

echo "FORCING_TYPE  : ${FORCING_TYPE}"
echo "FORCING_DIR   : ${FORCING_DIR}"
echo "SITE          : ${SITE}"
echo "INICLM_DIR    : ${INICLM_DIR}"
echo "NAMELIST      : ${NAMELIST}"
echo "NPROC         : ${NPROC}"
echo "WORK_DIR      : ${WORK_DIR}"
echo "OUTPUT_DIR    : ${OUTPUT_DIR}"
echo "INPUT_DIR     : ${INPUT_DIR}"
echo "ECLAND_MASTER : ${ECLAND_MASTER}"

#====== END SETUP

#***************************
# Start:
#***************************

# Create folders
trace mkdir -p ${WORK_DIR}
trace mkdir -p ${OUTPUT_DIR}
trace mkdir -p ${INPUT_DIR}

#****************************** Retrieve input files *****************************#

declare -a INPUT_FILES

INPUT_FILES+=(${FORCING_DIR}/met_${FORCING_TYPE}HT_${SITE}.nc)
INPUT_FILES+=(${INICLM_DIR}/surfinit_${SITE}.nc)
INPUT_FILES+=(${INICLM_DIR}/surfclim_${SITE}.nc)
for INPUT_FILE in ${INPUT_FILES[*]}; do
  trace ${SCRIPTS_DIR}/ecland_retrieve.sh ${INPUT_FILE} ${INPUT_DIR}/$(basename ${INPUT_FILE})
done

#****************************** Run the simulations *****************************#

trace ${SCRIPTS_DIR}/ecland_run_model.sh   \
    -s ${SITE}              \
    -b ${ECLAND_MASTER}     \
    -w ${WORK_DIR}          \
    -o ${OUTPUT_DIR}        \
    -f ${INPUT_DIR}         \
    -i ${INPUT_DIR}         \
    -F ${FORCING_TYPE}      \
    -p ${NPROC}             \
    -t ${NTHREADS:-12}      \
    -n ${NAMELIST}

#******************************** Validate results *******************************#
if [[ -z ${REL_TOL:-} ]]; then
    REL_TOL="1.e-8 1.e-8 1.e-8"
fi
export REL_TOL
if [[ ${VALIDATE} = true  ]]; then
  NLOOP=1
  trace ${SCRIPTS_DIR}/ecland_validate.sh  \
    -s ${SITE}              \
    -n ${NLOOP}             \
    -o ${OUTPUT_DIR}        \
    -w ${WORK_DIR}          \
    -c ${CONTROL_DIR}
fi
unset REL_TOL

echo "+ rm -rf ${WORK_DIR}"
rm -rf ${WORK_DIR}

#EOF
