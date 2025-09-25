#!/usr/bin/env bash

# This script is used to run an experiment in the Ecland project.
# (C) Copyright 2023- ECMWF.
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

# Functions

# This function returns the absolute path of a given file or directory.
# Arguments:
#   - $1: The file or directory path
function abs_path () {
  builtin echo $(cd "$(dirname -- "${1}")" >/dev/null; pwd -P)/$(basename -- "${1}")
}

# This function prints and executes a command.
# Argument:
function trace () {
  builtin echo -e "+ $@"
  eval "$@"
}

# This function finds all sites in a given directory that match a specific pattern.
# Arguments:
#   - $1: The directory path
#   - $2: The pattern to match
# The function returns an array of site names.
function find_sites () {
  declare -a VAR
  flist=$(ls "${1}"/*${2}*)
  for ff in $flist; do
    VAR+=(" $(basename $ff .nc |  sed -r 's/([^_]*_){2}//') ")
  done
  echo "${VAR[@]}"
}

# Function to check restart files for ecLand and Cama-flood
check_restart_files() {
  if [[ $LRESTART = true ]]; then
    echo "restart file(s) for ecLand and Cama-flood specified by environmental variables: "
    echo "RESTARTECLAND=${RESTARTECLAND}"
    echo "RESTARTCMF=${RESTARTCMF}"
    if [[ -z "${RESTARTECLAND}" || ! -f "${RESTARTECLAND}" ]]; then
      echo "LRESTART is true, but restart file for ecLand has not been specified or does not exist, export RESTARTECLAND environment variable. Abort"
      exit 0
    fi
    if [[ ${RUN_CMF} = true && ( -z "${RESTARTCMF}" || ! -f "${RESTARTCMF}" ) ]]; then
      echo "LRESTART is true, but restart file for Camaflood has not been specified or does not exist, export RESTARTCMF environment variable. Abort"
      exit 0
    fi
  fi
}

# Help message function
function usage () {
  echo "Usage: $0 [options]"
  echo "Options:"
  echo "  -g, GROUP               Specify group of sites (e.g. ESM-SnowMIP,TERRENO...) to be run sequentially"
  echo "  -t, FORCING_TYPE        Specify forcing type (insitu, era5)"
  echo "  -i, INPUT_DIR           Specify clim/forcing directory of experiment"
  echo "  -o, OUTPUT_DIR          Specify output directory of experiment"
  echo "  -w, WORK_DIR            Specify working directory of experiment"
  echo "  -x, ECLAND_MASTER       Specify location of ECLAND master exe"
  echo "  -s, SITE (opt)          If present, specify the site to run from the GROUP (default = all sites in GROUP)"
  echo "  -l, NLOOP (opt)         If present, specify the number of loops to run (default = 1)"
  echo "  -R, LRESTART (opt)      If present, specify if restart files are present (default = false)"
  echo "  -n, NAMELIST_FILE (opt) If present, Specify location of namelist to use for the experiment (default in namelists/namelist_ecland_48R1)"
  echo "  -c, NAMELIST_CMF (opt)  If present, specify location of namelist to use for the cama-flood model (default in namelists/namelist_cmf_48R1)"
  echo "  -h, --help              Display this help message"
  echo "Example:"
  echo " ecland_run_experiment.sh -g ESM-SnowMIP -t insitu -i /path/to/input -o /path/to/output -w /path/to/workdir -x /path/to/ecland-master <-n /path/to/namelist_ecland> <-s site1> <-l 1> <-R false> <-c /path/to/namelist_cmf>"
}


#====== Read arguments

# Default values for site, if not provided run all sites in GROUP
SITE=""
NLOOP=1
LRESTART=false
ECLAND_MASTER=ecland-master
NAMELIST_DEFAULT="$(pwd)/namelists/namelist_ecland_48R1"
NAMELIST_CMF_DEFAULT="$(pwd)/namelists/namelist_cmf_48R1"
# Parse command-line arguments
[ $# -eq 0 ] && usage
while getopts ":hg:t:i:o:w:x:s:n:c:l:R:" opt; do
  case $opt in
    g)
      GROUP="$OPTARG"
      ;;
    t)
      FORCING_TYPE="$OPTARG"
      ;;
    i)
      INPUT_DIR="$OPTARG"
      ;;
    o)
      OUTPUT_DIR="$OPTARG"
      ;;
    w)
      WORK_DIR="$OPTARG"
      ;;
    x)
      ECLAND_MASTER="$OPTARG"
      ;;
    n)
      NAMELIST_FILE="$OPTARG"
      ;;
    c)
      NAMELIST_CMF="$OPTARG"
      ;;
    R)
      LRESTART="$OPTARG"
      ;;
    s)
      SITE="$OPTARG"
      ;;
    l)
      NLOOP="$OPTARG"
      ;;
    h| *)
      usage
      exit 0
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done

export DR_HOOK_ASSERT_MPI_INITIALIZED=0
SCRIPTS_DIR="$( cd $( dirname "${BASH_SOURCE[0]}" ) && pwd -P )"
if [[ $(basename ${SCRIPTS_DIR}) == "bin" ]]; then
  SCRIPTS_DIR="$( cd $( dirname "${SCRIPTS_DIR}/$(readlink "${BASH_SOURCE[0]}")" ) && pwd -P )"
fi

#====== SETUP
source ${SCRIPTS_DIR}/ecland_runtime.sh
WORK_DIR=${WORK_DIR:-$(pwd)/work}
OUTPUT_DIR=${OUTPUT_DIR:-$(pwd)/output}
INPUT_DIR=${INPUT_DIR:-$(pwd)/input}
FORCING_DIR=${INPUT_DIR}/forcing/${GROUP}/
INICLM_DIR=${INPUT_DIR}/clim/${GROUP}/
NAMELIST_FILE=${NAMELIST_FILE:-${NAMELIST_DEFAULT}}
NAMELIST_CMF=${NAMELIST_CMF:-${NAMELIST_CMF_DEFAULT}}

mkdir -p ${WORK_DIR}

# Check number of sites to run
if [[ $SITE = "" ]]; then
  SITES=($(find_sites ${FORCING_DIR} ${FORCING_TYPE}))
else
  SITES=( ${SITE} )
fi
#====== END SETUP


for cs in ${SITES[@]}; do

# Create namelist files for ecLand and Cama-flood based on the template provided
# and the information in the input files site-specifics (length of the forcing files, etc.)
trace ${SCRIPTS_DIR}/ecland_create_namelist.py -g ${GROUP} \
                         -n ${NAMELIST_FILE} \
                         -c ${NAMELIST_CMF} \
                         -s ${cs} \
                         -d ${INPUT_DIR} \
                         -w ${WORK_DIR} \
                         -t ${FORCING_TYPE}
NAMELIST="${WORK_DIR}/namelist_${cs}"
# Check if cama-flood is active. We use the creation of the namelist
# as indicator of cama-flood on/off
RUN_CMF=false
NAMELIST_CMF_OPT=""
if [[ -f ${WORK_DIR}/namelist_cmf_${cs} ]]; then
    NAMELIST_CMF_OPT="-c ${WORK_DIR}/namelist_cmf_${cs}"
    RUN_CMF=true
fi

# Print setup
echo "EcLand setup:"
echo "FORCING_TYPE  : ${FORCING_TYPE}"
echo "SITE          : ${cs}"
echo "NLOOP         : ${NLOOP}"
echo "INICLM_DIR    : ${INICLM_DIR}"
echo "FORCING_DIR   : ${FORCING_DIR}"
echo "NAMELIST      : ${NAMELIST}"
echo "WORK_DIR      : ${WORK_DIR}"
echo "OUTPUT_DIR    : ${OUTPUT_DIR}"
echo "INPUT_DIR     : ${INPUT_DIR}"
echo "ECLAND_MASTER : ${ECLAND_MASTER}"
echo "RUN CMF       : ${RUN_CMF}"
echo "ISRESTART     : ${LRESTART}"
[[ ${RUN_CMF} = true ]] && echo "NAMELIST_CMF  : ${NAMELIST_CMF}"


# If restart is true, check if restart files are present.
check_restart_files

#***************************
# Start:
#***************************

# Create folders
trace mkdir -p ${WORK_DIR}
trace mkdir -p ${INPUT_DIR}
trace mkdir -p ${OUTPUT_DIR}

trace ${SCRIPTS_DIR}/ecland_run_model.sh   \
    -s "${cs}"           \
    -b "${ECLAND_MASTER}"\
    -w "${WORK_DIR}"     \
    -o "${OUTPUT_DIR}"   \
    -f "${FORCING_DIR}"  \
    -i "${INICLM_DIR}"   \
    -F "${FORCING_TYPE}" \
    -n "${NAMELIST}"     \
    ${NAMELIST_CMF_OPT}  \
    -l ${NLOOP}          \
    -R ${LRESTART}       
done

# Clean up the working directory
echo "+ rm -rf ${WORK_DIR}"
rm -rf ${WORK_DIR}/

#EOF
