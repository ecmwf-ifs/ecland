#!/usr/bin/env bash

HDF5_VERSION=1.14.6
TEMPORARY_FILES="${TMPDIR:-/tmp}"
USE_CONFIGURE_BUILD=false

export HDF5_INSTALL_DIR=$(pwd)/hdf5-install
while [ $# != 0 ]; do
    case "$1" in
    "--prefix")
        export HDF5_INSTALL_DIR="$2"; shift
        ;;
    "--tmpdir")
        TEMPORARY_FILES="$2"; shift
        ;;
    "--version")
        HDF5_VERSION="$2"; shift
        ;;
    "--use-configure")
       USE_CONFIGURE_BUILD=true ; shift
        ;;
    *)
        echo "Unrecognized argument '$1'"
        exit 1
        ;;
    esac
    shift
done

# Choose HDF5

VERSION_PARTS=($(echo ${HDF5_VERSION} | tr "." "\n"))
MAJOR__VERSION=${VERSION_PARTS[0]}\_${VERSION_PARTS[1]}
FULL_VERSION=${VERSION_PARTS[0]}\_${VERSION_PARTS[1]}\_${VERSION_PARTS[2]}

URL="https://support.hdfgroup.org/releases/hdf5/v${MAJOR__VERSION}/v${FULL_VERSION}/downloads/hdf5-${HDF5_VERSION}.tar.gz"
FOLDER="hdf5-${HDF5_VERSION}"

# Download hdf5

if [ ! -d "${TEMPORARY_FILES}/${FOLDER}" ]; then
  echo "Downloading ${TEMPORARY_FILES}/${FOLDER} from URL [${URL}]"
  mkdir -p ${TEMPORARY_FILES}
  curl --location \
       "${URL}" | tar zx -C "${TEMPORARY_FILES}"
else
   echo "Download already present in ${TEMPORARY_FILES}/${FOLDER}"
fi

# Build hdf5

if [ "$USE_CONFIGURE_BUILD" = true ]; then
   cd ${TEMPORARY_FILES}/hdf*
   mkdir -p "${HDF5_INSTALL_DIR}"
   ./configure --prefix="${HDF5_INSTALL_DIR}" --enable-shared --enable-fortran --enable-hl
   make -j
   make install
else
   cmake -G Ninja -S  ${TEMPORARY_FILES}/${FOLDER} -B "build-${FOLDER}" -DHDF5_BUILD_FORTRAN=ON -DHDF5_BUILD_HL_LIB=ON -DBUILD_TESTING=OFF 
   cmake --build  "build-${FOLDER}" --config Release
   cmake --install "build-${FOLDER}" --prefix ${HDF5_INSTALL_DIR}
fi

exit 0

