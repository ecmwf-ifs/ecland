#!/bin/bash
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set -eux

#** Script to extract era5 forcing fields needed for OSM run **#
#**************************************************************#


# Set output dir of concatenated forcing fields
forcingdir=$1

# Output Directory
odir=$2
mkdir -p $odir

# Forcing type, 2D or 1D (point)
forcingType=$3

# Define iniDate and end Date: add option for monthly chuncks
iniDates=($(cat ${forcingdir}/iniDate_list.txt))
endDates=($(cat ${forcingdir}/endDate_list.txt))
sitelist=($(cat ${forcingdir}/site_list.txt))

nsites=${#sitelist[@]}
varlist=( "PSurf" "Tair" "Qair" "Wind_E" "Wind_N" "SWdown" "LWdown" "Rainf" "Snowf" "Ctpf")
varlen=${#varlist[@]}

if [[ $forcingType = '1D' ]]; then
    ftypeStr='era5'
elif [[ $forcingType = '2D' ]]; then
    ftypeStr='2D'
fi
# LOOP SITES, concatenate monthly forcing produced.
i=0
while [[ $i -lt $nsites ]]; do
  site=${sitelist[$i]}
  siteini=${iniDates[$i]}
  siteend=${endDates[$i]}
  siteiniYear=${siteini:0:4}
  siteendYear=${siteend:0:4}
  rm -f $odir/met_2DHT_${site}_${siteiniYear}-${siteendYear}.nc
  cdo -O -f nc4c -z zip_6 mergetime $forcingdir/${site}/${site}_*.nc $odir/met_${ftypeStr}HT_${site}_${siteiniYear}-${siteendYear}.nc
  #rm -rf $forcingdir/${site}/
  i=$((i+1))
done
