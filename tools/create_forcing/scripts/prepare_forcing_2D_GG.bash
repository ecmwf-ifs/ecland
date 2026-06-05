#!/bin/bash
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set -eux

# Script to extract ERA5 forcing fields for ecLand on REDUCED GAUSSIAN GRID
# - Extract forcing fields from ERA5 (on native Gaussian)
# - Regrid to target Gaussian grid
# - Convert to NetCDF using grb2nc_gaussian.py
#**************************************************************#

# Function to add num of days (num_days) to an input_date
add_days_to_date() {
    input_date="$1"
    num_days="$2"
    timestamp=$(date -d "$input_date" +%s)
    new_timestamp=$((timestamp + num_days * 24 * 60 * 60))
    new_date=$(date -d "@$new_timestamp" "+%Y%m%d")
    echo "$new_date"
}

# Set output dir of concatenated forcing fields
forcingdir=$1
idate=$2
edate=$3
climoutdir=${4}

# Target Gaussian grid from environment
TARGET_GRID=${TARGET_GAUSSIAN_GRID:-N32}

echo "=== Gaussian Grid Forcing Preparation ==="
echo "Target grid: $TARGET_GRID"
echo "Date range: $idate - $edate"

mkdir -p $forcingdir
export MARS_READANY_BUFFER_SIZE=4132480200
export MIR_GRIB_INPUT_BUFFER_SIZE=7688961960

# Read dates and sites
iniDates=($(cat ${forcingdir}/iniDate_list.txt))
endDates=($(cat ${forcingdir}/endDate_list.txt))
sitelist=($(cat ${forcingdir}/site_list.txt))

nsites=${#sitelist[@]}

# Variables required by ECLand:
varlist=( "PSurf" "Tair" "Qair" "Wind_E" "Wind_N" "SWdown" "LWdown" "Rainf" "Snowf" "Ctpf" )
varlen=${#varlist[@]}

# Reference climate file for grid info
climfile=${climoutdir}/surfclim_${sitelist[0]}_${iniDates[0]:0:4}-${endDates[0]:0:4}.nc

date=$idate
first_dateMonth=${date:0:6}00

while [[ $date -le $edate ]]; do
  echo "Processing: $date"

  d=${date:0:6}00
  forcingdirDate=${forcingdir}_${d}
  mkdir -p $forcingdirDate

  # Download forcing data from CDS/MARS
  case "${RETRIEVE_WITH}" in
      "mars") ${scriptsdir}/extract_forcing_mars.bash -e ${d} -d $forcingdirDate -i 1 -f 1 -c ${FORCINGCLASS} -s oper 
          ;;
      "cds")  ${scriptsdir}/extract_forcing_cds.bash -e ${d} -d $forcingdirDate -i 1 -f 1 -c ${FORCINGCLASS} -s oper -P false
          ;;
      *) echo "download method not implemented"; exit 1 ;;
  esac

  # Process each site
  i=0
  while [[ $i -lt $nsites ]]; do
    siteini=${iniDates[$i]}
    siteend=${endDates[$i]}
    site=${sitelist[$i]}
    forcingdirSite=${forcingdir}/${site}/
    mkdir -p $forcingdirSite

    # Move forcing files
    for var in "${varlist[@]}"; do
      if [[ -f $forcingdirDate/${var}.grb ]]; then
        mv $forcingdirDate/${var}.grb $forcingdir/${var}_${d}.grb
      fi
    done

    # Process if within date range
    if [[ $siteini -le $date && $siteend -ge $date ]]; then
      flistMerge=""
      
      for cvar in "${varlist[@]}"; do
        echo "Processing variable: $cvar"
        
        # Regrid to target Gaussian grid using metview
        python3 ${scriptsdir}/osm_pyutils/interp_gg.py -t con -g ${TARGET_GRID} \
          -i ${forcingdir}/${cvar}_${d}.grb \
          -o ${forcingdirSite}/${cvar}_${d}.grb
        
        rm -f ${forcingdir}/${cvar}_${d}.grb

        # Remove last timestep for concatenation (except last month)
        testDate=$(add_days_to_date $date 33 | cut -c1-6)"01"
        testDate=$(add_days_to_date $testDate -1)
        if [[ $testDate -lt $siteend ]]; then
          ncount=$(grib_ls -p count ${forcingdirSite}/${cvar}_${d}.grb | tail -n -1 | awk '{print $1}')
          grib_copy -w count!=${ncount} ${forcingdirSite}/${cvar}_${d}.grb ${forcingdirSite}/tmp_${d}.grb
          mv -f ${forcingdirSite}/tmp_${d}.grb ${forcingdirSite}/${cvar}_${d}.grb
        fi
        
        # Convert to NetCDF using Gaussian converter
        python3 ${scriptsdir}/osm_pyutils/grb2nc_gaussian.py \
          ${forcingdirSite}/${cvar}_${d}.grb \
          ${forcingdirSite}/${cvar}_${d}.nc
        
        flistMerge="${flistMerge} ${forcingdirSite}/${cvar}_${d}.nc"
      done

      # Merge all variables into single file
      # Note: For Gaussian grids we use ncks instead of cdo merge
      first_file=$(echo $flistMerge | awk '{print $1}')
      cp $first_file ${forcingdirSite}/${site}_${d}.nc
      for ncfile in $flistMerge; do
        if [[ $ncfile != $first_file ]]; then
          ncks -A $ncfile ${forcingdirSite}/${site}_${d}.nc
        fi
      done
      rm -f $flistMerge
    fi
  
    i=$((i+1))
  done

  # Cleanup
  rm -f ${forcingdirSite}/*_${d}.grb 2>/dev/null || true
  rm -rf ${forcingdirDate}/
  
  date=$(add_days_to_date $date 33 | cut -c1-6)"01"
done

echo ""
echo "=== Gaussian Grid Forcing Complete ==="
echo "Output: ${forcingdir}/${sitelist[0]}/"
