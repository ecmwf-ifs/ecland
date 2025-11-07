#!/usr/bin/env bash

# (C) Copyright 2023- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

set -ea
set -o pipefail

FILE_PATH=$1
FILE=$(basename ${FILE_PATH})
TARGET_FILE_PATH=${2:-$(pwd)/${FILE_PATH}}
if [[ "${TARGET_FILE_PATH}" == */ ]] ; then
  TARGET_FILE_PATH=${TARGET_FILE_PATH}/${FILE}
fi
TARGET_FILE=$(basename ${TARGET_FILE_PATH})
LOCATION=$(dirname ${TARGET_FILE_PATH})

SCRIPTS_DIR="$( cd $( dirname "${BASH_SOURCE[0]}" ) && pwd -P )"

source ${SCRIPTS_DIR}/ecland_runtime.sh

################################################################################################
# 1. Return if FILE is already available

if [ -f "${TARGET_FILE_PATH}" ]; then
  echo "File is available in LOCATION: ${TARGET_FILE_PATH}"
  exit
fi

################################################################################################
# 2. Find FILE in search locations on disk

if [ -f "${FILE_PATH}" ]; then
  echo "File was found in: ${FILE_PATH}"
  mkdir -p ${LOCATION}
  ln -sf ${FILE_PATH} ${TARGET_FILE_PATH}
  echo "Symbolic link is available in LOCATION: ${TARGET_FILE_PATH}"
  exit
fi

declare -a SEARCH_LOCATIONS

# Add search location in ECLAND_CACHE_PATH, a writeable directory
if [[ ! -z ${ECLAND_CACHE_PATH} ]] ; then
  SEARCH_LOCATIONS+=(${ECLAND_CACHE_PATH})
fi

# Add search locations from ':' separated list "ECLAND_CACHE_PATH"
if [[ ! -z ${ECLAND_DATA_PATH} ]] ; then
  for d in $(echo ${ECLAND_DATA_PATH} | tr ":" "\n"); do
    SEARCH_LOCATIONS+=(${d})
  done
fi

SEARCH_LOCATIONS+=(${ecland_ROOT}/share/ecland)
SEARCH_LOCATIONS+=(${PWD})

for SEARCH_LOCATION in ${SEARCH_LOCATIONS[*]}; do
  if [ -f "${SEARCH_LOCATION}/${FILE_PATH}" ]; then
    echo "File was found in: ${SEARCH_LOCATION}/${FILE_PATH}"
    mkdir -p ${LOCATION}
    ln -sf ${SEARCH_LOCATION}/${FILE_PATH} ${TARGET_FILE_PATH}
    echo "Symbolic link is available in LOCATION: ${TARGET_FILE_PATH}"
    exit
  fi
  if [ -f "${SEARCH_LOCATION}/${FILE}" ]; then
    echo "File was found in: ${SEARCH_LOCATION}/${FILE}"
    mkdir -p ${LOCATION}
    ln -sf ${SEARCH_LOCATION}/${FILE} ${TARGET_FILE_PATH}
    echo "Symbolic link is available in LOCATION: ${TARGET_FILE_PATH}"
    exit
  fi
done

################################################################################################
# 3. FILE is not found in any path hierarchy. Download from sites.ecmwf.int

ECLAND_URL=https://sites.ecmwf.int/repository/ecland

function command_exists () {
    type "$1" &> /dev/null ;
}

function trace () {
  echo "+ $@"
  eval "$@"
}


function url_exists () {
  curl -o/dev/null -sfI "$1"
}

function decompress () {
  case ${1} in
    *.gz)  echo "+ gzip  -ckd ${1} > ${2}.decompress"; gzip  -ckd ${1} > ${2}.decompress ;;
    *.bz2) echo "+ bzip2 -ckd ${1} > ${2}.decompress"; bzip2 -ckd ${1} > ${2}.decompress ;;
    *.zst) echo "+ zstd  -ckd ${1} > ${2}.decompress"; zstd  -ckd ${1} > ${2}.decompress ;;
    *)     trace cp ${1} ${2}.decompress ;;
  esac
  trace mv ${2}.decompress ${2}
}

function url() {
  if   ( command_exists zstd  ) && ( url_exists ${ECLAND_URL}/${1}.zst ); then
    echo ${ECLAND_URL}/${1}.zst
  elif ( command_exists bzip2 ) && ( url_exists ${ECLAND_URL}/${1}.bz2 ); then
    echo ${ECLAND_URL}/${1}.bz2
  elif ( command_exists gzip  ) && ( url_exists ${ECLAND_URL}/${1}.gz  ); then
    echo ${ECLAND_URL}/${1}.gz
  elif ( url_exists ${ECLAND_URL}/${1} ); then
    echo ${ECLAND_URL}/${1}
  else
    echo "URL_NOT_FOUND"
  fi
}

if [ ! -f "${DOWNLOAD_DIR}/${FILE}" ]; then
  mkdir -p ${DOWNLOAD_DIR} && cd ${DOWNLOAD_DIR}

  URL=$(url ${FILE_PATH})
  url_exists ${URL} || {
    echo "${FILE_PATH} or ${FILE} were not found in any search location."
    echo "A suitable download URL is also not available."
    echo "Tried: ${ECLAND_URL}/${FILE_PATH}"
    exit 1
  }
  FILE_COMPRESSED=$(basename ${URL})
  if [ ! -f ${FILE_COMPRESSED} ]; then
    echo "Downloading from URL: ${URL}"
    trace curl -L -C - "${URL}" --output ${FILE_COMPRESSED}.download
    trace mv ${FILE_COMPRESSED}.download ${FILE_COMPRESSED}
    echo "Download completed"
  fi
  if [ "${FILE}" != "${FILE_COMPRESSED}" ]; then
    echo "Decompressing ${FILE_COMPRESSED}"
    decompress ${FILE_COMPRESSED} ${FILE}
    echo "Decompression completed"
    rm ${FILE_COMPRESSED}
  fi
fi

cmp -s ${DOWNLOAD_DIR}/${FILE} ${TARGET_FILE_PATH} || {
  echo "Moving file to LOCATION"
  mkdir -p ${LOCATION}
  trace mv ${DOWNLOAD_DIR}/${FILE} ${TARGET_FILE_PATH}
}

echo "File is available in LOCATION: ${TARGET_FILE_PATH}"
