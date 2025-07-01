#!/usr/bin/env bash
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

SCRIPTS_DIR="$( cd $( dirname "${BASH_SOURCE[0]}" ) && pwd -P )"
export PATH=${SCRIPTS_DIR}:${PATH}
if [[ $(basename ${SCRIPTS_DIR}) == "bin" ]]; then
  SCRIPTS_DIR="$( cd $( dirname "${SCRIPTS_DIR}/$(readlink "${BASH_SOURCE[0]}")" ) && pwd -P )"
fi
export PATH=${SCRIPTS_DIR}:${PATH}

function abs_path {
  builtin echo $(cd "$(dirname -- "${1}")" >/dev/null; pwd -P)/$(basename -- "${1}")
}

function trace () {
  builtin echo -e "+ $@"
  eval "$@"
}

# Helper functions
function log () {
  verbose=${VERBOSE}
  if [[ "$*" =~ (gdb|sanitizer|valgrind) ]]; then
      verbose=true
  fi
  if [[ ${verbose} == true ]]; then
    # Send stdout/stderr both to file and terminal
    ("$@" | tee -a stdout.log) 3>&1 1>&2 2>&3 | tee -a stderr.log
  else
    "$@" 1> stdout.log 2>&1
  fi
}

# Script to run ECLand on sites or regional domains
# Arguments for the ECLand model run
# -------------------------------

# set defaults
STA=""
WORKDIR="./"
OUTPUTDIR="./"
FORCINGDIR=""
INICLMDIR=""
FORCINGTYPE="insitu"
NAMELIST="namelist"
NAMELIST_CMF=""
RUN_CMF=false
ECLAND_MASTER="ecland-master"
LRESTART=false
NLOOP=1
VERBOSE=false
while getopts ":s:b:w:o:f:i:F:n:R:c:l:v:" opt; do
  case $opt in
    s) STA="$OPTARG" ;;
    b) ECLAND_MASTER="$OPTARG" ;;
    w) WORKDIR="$OPTARG" ;;
    o) OUTPUTDIR="$OPTARG" ;;
    f) FORCINGDIR="$OPTARG" ;;
    i) INICLMDIR="$OPTARG" ;;
    F) FORCINGTYPE="$OPTARG" ;;
    n) NAMELIST=("$OPTARG") ;;
    c) NAMELIST_CMF=("$OPTARG") ;;
    R) LRESTART="$OPTARG" ;;
    l) NLOOP="$OPTARG" ;;
    v) VERBOSE=true ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      exit 1
      ;;
  esac
done

NAMELIST=$(abs_path ${NAMELIST})
INICLMDIR=$(abs_path ${INICLMDIR})
FORCINGDIR=$(abs_path ${FORCINGDIR})
RESTDIR=$(abs_path ${RESTDIR:-"${INICLMDIR}"})

start_year="${STA#*_}"
start_year="${start_year%-*}"
end_year="${STA##*_}"
end_year=${end_year:5:9}

# Check if namelist_cmf is defined and use it to define RUN_CMF variable
# and copy in workdir river network and initial conditions.
if [[ -n ${NAMELIST_CMF} ]]; then
    RUN_CMF=true
    NAMELIST_CMF=$(abs_path ${NAMELIST_CMF})
fi

# Usage example
echo "STA: $STA"
echo "WORKDIR: $WORKDIR"
echo "OUTPUTDIR: $OUTPUTDIR"
echo "FORCINGDIR: $FORCINGDIR"
echo "INICLMDIR: $INICLMDIR"
echo "FORCINGTYPE: $FORCINGTYPE"
echo "NAMELIST: $NAMELIST"
echo "RUN CMF: $RUN_CMF"
[[ ${RUN_CMF} = true ]] && echo "NAMELIST_CMF: $NAMELIST_CMF"
echo "NLOOP: $NLOOP"


# Setting up variables and create folders
mkdir -p ${WORKDIR}
RDIR=${WORKDIR}/RUN/${STA}
ODIR=${OUTPUTDIR}/${STA}
FORCING=${FORCINGDIR}
INICLM=${INICLMDIR}

ODIR=$(abs_path ${ODIR})

mkdir -p ${OUTPUTDIR}
rm -rf ${ODIR}
mkdir -p ${ODIR}

rm -rf ${RDIR}
mkdir -p ${RDIR}
cd ${RDIR}

# Initialise RLOOP (loop counter between 1 and NLOOP)
RLOOP=1

# Link executable and forcing and clim/ini files
cp ${NAMELIST} input
ln -s ${FORCING}/met_${FORCINGTYPE}HT_${STA}.nc forcing
cp ${INICLM}/surfinit_${STA}.nc soilinit
cp ${INICLM}/surfclim_${STA}.nc surfclim

# If camaflood is active, copy river initial conditions and network and namelist
if [[ $RUN_CMF = true ]]; then
  ln -sf ${INICLM}/inpmat_${STA}.nc inpmat.nc
  ln -sf ${INICLM}/rivpar_${STA}.nc rivpar.nc
  ln -sf ${INICLM}/rivclim_${STA}.nc rivclim.nc
  ln -sf ${INICLM}/bifprm_${STA}.txt bifprm.txt

  cp ${NAMELIST_CMF} input_cmf.nam
  cp ${INICLM}/diminfo_${STA}.txt diminfo.txt

  eyear=$(grep "EYEAR=" input_cmf.nam | awk '{print $1}' | cut -d "=" -f 2)
  emon=$(grep "EMON=" input_cmf.nam | awk '{print $1}' | cut -d "=" -f 2)
  eday=$(grep "EDAY=" input_cmf.nam | awk '{print $1}' | cut -d "=" -f 2)
  ehour=$(grep "EHOUR=" input_cmf.nam | awk '{print $1}' | cut -d "=" -f 2)
fi

# If a restart file is provided, link it in the rundir
if [[ $LRESTART = true ]]; then
  rm -f soilinit
  rm -f surfclim
  ln -sf ${RESTDIR}/restartin.nc restartin.nc
  ln -sf ${RESTARTECLAND} soilinit
  ln -sf ${RESTARTECLAND} surfclim
  [[ $RUN_CMF = true ]] && ln -sf ${RESTARTCMF} restartin_cmf.nc
fi
# Loop
if [[ "${LAUNCH}" =~ "ecland-launch" ]]; then
  LAUNCH_CMD=$(DRY_RUN=true ${LAUNCH} ${ECLAND_MASTER})
else
  LAUNCH_CMD="${LAUNCH} ${ECLAND_MASTER}"
fi
for RLOOP in $(seq 1 ${NLOOP}); do

  # do the run
  #
  ls -ltarh
  echo "---------------------------------------------"
  echo "Executable built:  $(which ${ECLAND_MASTER})"
  echo "Forcing:           ${FORCING}/met_${FORCINGTYPE}HT_${STA}.nc"
  echo "Rundir:            ${RDIR}"
  echo "RLOOP / NLOOP:     ${RLOOP} / ${NLOOP}"
  echo "---------------------------------------------"
  echo "*******************************************************************************"
  echo "ECLAND START"
  echo -e "\n+ ${LAUNCH_CMD} \n"
  echo "*******************************************************************************"
  START=$(date +%s)
  log ${LAUNCH} ${ECLAND_MASTER} || {
    sleep 1
    echo
    echo "*******************************************************************************"
    echo "ECLAND FAILED EXECUTION"
    echo "*******************************************************************************"
    exit 1
  }

  END=$(date +%s)
  DIFF=$(( $END - $START ))
  echo -e "\n\n\t Running ${LAUNCH_CMD} took $DIFF seconds\n"
  echo "---------------------------------------------"

  # Spinup run output are labelled with RLOOP. Final run is kept without label.
  trace mv stdout.log run.log
  if [[ $RLOOP != $NLOOP ]]; then
    trace mv run.log run_S${RLOOP}.log
    trace mv o_gg.nc o_gg_S${RLOOP}.nc
    trace mv restartout.nc restartout_S${RLOOP}.nc
    if [[ $RUN_CMF = true ]]; then
      trace mv o_totout.nc o_totout_S${RLOOP}.nc
      trace mv restart${eyear}${emon}${eday}${ehour}.nc restartout_cmf_S${RLOOP}.nc
    fi
  fi
  # Overwrite soilinit and surfclim from previous run with the new one,
  # so next RLOOP uses initial conditions from end of previous RLOOP.
  if [[ $NLOOP -gt 1 ]]; then
    ln -sf restartout_S${RLOOP}.nc soilinit
    ln -sf restartout_S${RLOOP}.nc surfclim
    [[ $RUN_CMF = true ]] && ln -sf restartout_cmf_S${RLOOP}.nc restartin_cmf.nc
  fi

done # end of loop

# Move output and logs to the output directory
trace mv o_*.nc ${ODIR}/
trace mv restartout*.nc ${ODIR}/
trace mv run*.log ${ODIR}/
trace mv input ${ODIR}/input.namelist
if [[ $RUN_CMF = true ]]; then
  trace mv restart${eyear}${emon}${eday}${ehour}.nc ${ODIR}/restartout_cmf.nc
fi

echo "Output, logs and namelist in: ${ODIR}"
echo
echo "+ rm -rf ${RDIR}"
rm -rf ${RDIR}

exit 0
