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


# set defaults

while getopts ":s:n:w:o:c:" opt; do
  case $opt in
    s) SITE="$OPTARG" ;;
    n) NLOOP="$OPTARG" ;;
    w) WORK_DIR="$OPTARG" ;;
    o) OUTPUT_DIR="$OPTARG" ;;
    c) CONTROL_DIR="$OPTARG" ;;
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

vars_to_test="SoilTemp SoilMois AvgSurfT"

# paths
ctlres=${CONTROL_DIR}/${SITE}
#expres=${OUTPUT_DIR}/${SITE}/run_S${NLOOP}.log
expres=${OUTPUT_DIR}/${SITE}/run.log

echo "Validating SITE=${SITE}, NLOOP=${NLOOP}, VARS=[${vars_to_test}], CONTROL=${ctlres}"

VDIR=${WORK_DIR}/validate/${SITE}/${NLOOP}
mkdir -p ${VDIR}
cd ${VDIR}

# Start validation for prognostic variables
for var in ${vars_to_test}; do
  # Extract MEAN,min and max from the log files of the experiment
  ${SCRIPTS_DIR}/ecland_extract_stats.py -i ${expres} -v ${var}
done

# Check now bit-identical values compared to control
${SCRIPTS_DIR}/ecland_validate_stats.py -c ${ctlres} -e ${VDIR} -t ${REL_TOL} -v ${vars_to_test}

# Cleaning
rm -rf ${VDIR}
