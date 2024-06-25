#!/bin/bash 
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set -eu

##################################################################
#
# Script to create site information for ecLand
# SURFCLIM provides climate fields based on lat and lon
# SURFINIT provides initialization field based on lat and lon
#
##################################################################

# Functions
get_lsm() {
    xdata_dir=$1
    lsmfile=$2
    if [[ $XDATA_DIR == *rdx* ]]; then
      grib_copy -w shortName=lsm ${XDATA_DIR}/lsmoro $lsmgrib
    else
      if [[ -f "${XDATA_DIR}/lsm" ]]; then
        cp -f ${XDATA_DIR}/lsm $lsmgrib
      else
        python3 ${scriptsdir}/osm_pyutils/download_clim_cds.py -v lsm -r ${MRESOL} -o $lsmgrib
      fi
    fi
}

gridT(){
resol=$1
gtype=$2  
case ${gtype} in
  "l_2"|"_3") GT=N;;
  "_4") GT=O;;
  *) print "GTYPE not available!"; exit -1;;
esac
NLAT=$( sh ${scriptsdir}/gaussgr -r $1 -g $2 )
echo ${GT}${NLAT}
}

#
odir=$1
scriptsdir=$2


# Definition of sites
sitelist=($(cat ${inidir}/site_list.txt))
nsites=${#sitelist[@]}

iniDates=($(cat ${inidir}/iniDate_list.txt))
endDates=($(cat ${inidir}/endDate_list.txt))
lats=($(cat ${inidir}/lat_list.txt))
lons=($(cat ${inidir}/lon_list.txt))

# location of default input file
surfclimgrb=${inidir}/clim.grb

MRESOL=$(gridT $RESOL $GTYPE )

mkdir -p $odir

# Loop over files
i=0
while (( $i < $nsites )); do
    echo $(pwd)
    site=${sitelist[$i]}

    iniDate=${iniDates[$i]}
    endDate=${endDates[$i]}
    iniYear=${iniDate:0:4}
    endYear=${endDate:0:4}
    lat=${lats[$i]}
    lon=${lons[$i]}
    surfinitgrb=${inidir}/init_${iniDate}.grb

    echo $iniDate $endDate $surfinitgrb
    echo $site $lat $lon 

    surfclimcdf=$odir/surfclim_${site}_${iniYear}-${endYear}.nc
    surfinitcdf=$odir/surfinit_${site}_${iniYear}-${endYear}.nc

    # Extract only land points for ini:
    lsmgrib=${inidir}/lsm
    get_lsm ${XDATA_DIR} ${lsmgrib}

    # Get surfclim information
    ${scriptsdir}/osm_pyutils/create_sites.py -i ${surfclimgrb} -l ${lsmgrib} -t ${which_surface} "-p ${lat},${lon}" -m 'clim' -o ${surfclimcdf}
    echo $surfclimcdf "created "

    ${scriptsdir}/osm_pyutils/create_sites.py -i ${surfinitgrb} -l ${lsmgrib} -t ${which_surface} "-p ${lat},${lon}" -m 'init' -o ${surfinitcdf}
    echo $surfinitcdf "created "

i=$(( i+1 ))
done


exit 0
