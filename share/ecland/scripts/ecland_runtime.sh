# (C) Copyright 2023- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

SCRIPTS_DIR="$( cd $( dirname "${BASH_SOURCE[0]}" ) && pwd -P )"

ECLAND_CACHE_PATH_DEFAULT=${HOME}/cache/ecland
[[ ${HPCPERM:-unset} != "unset" ]] && ECLAND_CACHE_PATH_DEFAULT=${HPCPERM}/cache/ecland

export ECLAND_CACHE_PATH=${ECLAND_CACHE_PATH:-${ECLAND_CACHE_PATH_DEFAULT}}
export ecland_ROOT=${ecland_ROOT:-${SCRIPTS_DIR}/../../..}
export DOWNLOAD_DIR=${DOWNLOAD_DIR:-$(pwd)}

# Names of executables
ECLAND_PROJECT_NAME=$(basename $(cd ${SCRIPTS_DIR}/.. && pwd -P) )
export ECLAND_MASTER=${ECLAND_MASTER:-${ECLAND_PROJECT_NAME}-master}

# Executable runtime environment
export LAUNCH=${LAUNCH:-}
if [[ ${LAUNCH} == "" && $(uname) == "Darwin" && -x "$(command -v mpirun)" ]]; then
  # See README: Avoids SIGILL in debug builds during MPI_INIT
  export LAUNCH="ecland-launch"
fi

export MBX_SIZE=${MBX_SIZE:-150000000}
export MPL_MBX_SIZE=${MPL_MBX_SIZE:-${MBX_SIZE}}

[[ ! (-d ${ecland_ROOT}/bin) ]] || export PATH=${ecland_ROOT}/bin:${ecland_ROOT}/share/${ECLAND_PROJECT_NAME}/scripts:${PATH}

export prec=@prec@
