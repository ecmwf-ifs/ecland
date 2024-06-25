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
# - extract tar data from ecfs era5land forcing fields NB at the moment it works only for data on ecfs
# - forcing fields are in daily chuncks from 1 to 25 hour of the following day
#   --> data are overlapping of 1 hour
# - to create a continuous time series, the last forcing timestep has to be removed from each file (not last day)
# - Concatenate over the year
#**************************************************************#

# Function to add num of days (num_days) to an input_date. Works with positive and negative num_days
add_days_to_date() {
    input_date="$1"   # Input date in the format "YYYYMMDD"
    num_days="$2"     # Number of days to add
    # Convert the input date to seconds since the Unix epoch
    timestamp=$(date -d "$input_date" +%s)
    # Calculate the new timestamp 
    new_timestamp=$((timestamp + num_days * 24 * 60 * 60))
    # Format the new timestamp back to the "YYYYMMDD" format
    new_date=$(date -d "@$new_timestamp" "+%Y%m%d")
    echo "$new_date"
}

extract_preProc_forcing() {
    local preProcForcdir="$1"
    local forcingCommonName="$2"
    local d="$3"
    
    local forcingFileName
    forcingFileName=$(find "$preProcForcdir" -name "${forcingCommonName}_${d}*" -print -quit)
    forcingFileName=$(basename "$forcingFileName")
    local fileExt="${forcingFileName##*.}"

    case "$fileExt" in
        gz)
            tar -zxf "${preProcForcdir}/${forcingFileName}" -C "${forcingdirDate}/" ;;
        tar)
            tar -xf "${preProcForcdir}/${forcingFileName}" -C "${forcingdirDate}/" ;;
        zip)
            unzip "${preProcForcdir}/${forcingFileName}" -d "${forcingdirDate}/" ;;
        grb)
            cp -f "${preProcForcdir}"/*_"${d}".grb "${forcingdirDate}/"
            for file in "${forcingdirDate}"/*_${d}.grb; do
              base_name=$(basename "$file" "_${d}.grb")
              new_name="${forcingdirDate}/${base_name}.grb"
              mv "$file" "$new_name"
            done ;;
        *)
            echo "Unsupported file extension: $fileExt" ;;
    esac
}


# Set output dir of concatenated forcing fields
forcingdir=$1

# some fields
idate=$2
edate=$3

# Output Directory
#odir=$4
#mkdir -p $odir

#climoutdir=${5}
climoutdir=${4}

mkdir -p $forcingdir
export MARS_READANY_BUFFER_SIZE=4132480200
export MIR_GRIB_INPUT_BUFFER_SIZE=7688961960
# Define iniDate and end Date: add option for monthly chuncks
iniDates=($(cat ${forcingdir}/iniDate_list.txt))
endDates=($(cat ${forcingdir}/endDate_list.txt))
sitelist=($(cat ${forcingdir}/site_list.txt))

nsites=${#sitelist[@]}

# Variables required by OSM:
varlist=( "PSurf" "Tair" "Qair" "Wind_E" "Wind_N" "SWdown" "LWdown" "Rainf" "Snowf" "Ctpf" )
varlen=${#varlist[@]}


# Start
    # Define exact lat/lon box over which forcing is processed.
    climfile=${climoutdir}/surfclim_${sitelist[0]}_${iniDates[0]:0:4}-${endDates[0]:0:4}.nc
    clonw_r=$(ncdump -v lon ${climfile} | grep "lon" | tail -n1 | awk '{print $3}' | sed 's/,//')
    clatn_r=$(ncdump -v lat ${climfile} | grep "lat" | tail -n1 | awk '{print $3}' | sed 's/,//')
    tmp_lon=$(ncdump -v lon ${climfile} | tail -n2 | head -n1 )
    tmp_lon=${tmp_lon%%[[:space:]]}; tmp_lon=${tmp_lon%%;*}
    clone_r=$(echo $tmp_lon | awk '{print $(NF)}')

    tmp_lat=$(ncdump -v lat ${climfile} | tail -n2 | head -n1 )
    tmp_lat=${tmp_lat%%[[:space:]]}; tmp_lat=${tmp_lat%%;*}
    clats_r=$(echo $tmp_lat | awk '{print $(NF)}')

date=$idate
first_dateMonth=${date:0:6}00
while [[ $date -le $edate ]]; do
  echo $date

  d=${date:0:6}00
  forcingdirDate=${forcingdir}_${d}
  mkdir -p $forcingdirDate
  preProcCDS=false
  gridInfo=""
  if [[ ${preProcCDS:- false} = true ]]; then
    gridInfo="-g ${dx},${clats_r},${clatn_r},${clone_r},${clonw_r}"
  fi
  # Extract/Download forcing data
  test -e ${preProcForcdir}/${forcingCommonName}_${d}* && retM=0 || retM=1
  if [[ $retM = 1 ]]; then
     case "${RETRIEVE_WITH}" in
         "mars") ${scriptsdir}/extract_forcing_mars.bash -e ${d} -d $forcingdirDate -i 1 -f 1 -c ${FORCINGCLASS} -s oper 
             ;;
         "cds")  ${scriptsdir}/extract_forcing_cds.bash  -e ${d} -d $forcingdirDate -i 1 -f 1 -c ${FORCINGCLASS} -s oper -P ${preProcCDS:-false} ${gridInfo}
             ;;
         *) echo "download method not implemented " ;;
     esac
  else
    # Assuming filename of the compressed file is forcing_*_${d}.tar.gz
    # Assuming filename of forcing files ${varname}_${d}.grb

    extract_preProc_forcing "${preProcForcdir}" "${forcingCommonName}" "${d}"

  fi
  # Save forcing files if SAVE_FORCING_GRIB is true/True
  if [[ ${SAVE_FORCING_GRIB:-false} = true ]]; then
     mkdir -p ${preProcForcdir}
     tar -zcf ${preProcForcdir}/${forcingCommonName}_${d}.tar.gz -C ${forcingdirDate} ${varlist[@]/%/.grb}
  fi

# extract and process forcing
  ciDate=$date
  ceDate=$(add_days_to_date $date 33 | cut -c1-6)"01"
  ceDate=$(add_days_to_date $ceDate -1)
  i=0
  while [[ $i -lt $nsites ]]; do
    siteini=${iniDates[$i]}
    siteend=${endDates[$i]}
    site=${sitelist[$i]}
    sdir=${forcingdir}/${site}
    echo $sdir
    if [[ ! -d $sdir ]]; then
      mkdir -p $sdir
    fi

# Using multi region extraction
#**    # Define exact lat/lon box over which forcing is processed.
#**    climfile=${climoutdir}/surfclim_${site}_${siteini:0:4}-${siteend:0:4}.nc
#**    clonw_r=$(ncdump -v lon ${climfile} | grep "lon" | tail -n1 | awk '{print $3}' | sed 's/,//')
#**    clatn_r=$(ncdump -v lat ${climfile} | grep "lat" | tail -n1 | awk '{print $3}' | sed 's/,//')
#**    tmp_lon=$(ncdump -v lon ${climfile} | tail -n2 | head -n1 )
#**    tmp_lon=${tmp_lon%%[[:space:]]}; tmp_lon=${tmp_lon%%;*}
#**    clone_r=$(echo $tmp_lon | awk '{print $(NF)}')
#**
#**    tmp_lat=$(ncdump -v lat ${climfile} | tail -n2 | head -n1 )
#**    tmp_lat=${tmp_lat%%[[:space:]]}; tmp_lat=${tmp_lat%%;*}
#**    clats_r=$(echo $tmp_lat | awk '{print $(NF)}')

# mars-grib extraction preprocessing...
    if [[ $FORCINGCLASS = 'l5' && ${preProcForcdir} = *"era5land_monthly"* ]]; then
    varlist+=("lapseM")
    varlen=${#varlist[@]}
    fi
    n=0
    while [[ $n -lt ${varlen} ]]; do
      echo "copying variable:" ${varlist[$n]}
      var=${varlist[$n]}
      mv $forcingdirDate/${var}.grb $forcingdir/${var}_${d}.grb 
   
      n=$((n+1))
    done # end loop in $n

  
    # CREATE_SITE_FORCING OR CAT-INTO if within the range of the site period
    if [[ $siteini -le $date && $siteend -ge $date ]]; then
      n=0
      forcingdirSite=${forcingdir}/${site}/
      mkdir -p $forcingdirSite
      # loop in forcing variables, if more than 1 month, remove last step in each month
      # as it is repeated in the first step of the following month.
      flistMerge=""
      for cvar in "${varlist[@]}"; do
        echo "copying variable:" $cvar
        
        # Interpolate and cut domain if regional run.
        if [[ ${preProcCDS:-false} = false ]]; then
          # interpolate to lat/lon and cut domain using metview-python
          python3 ${scriptsdir}/osm_pyutils/interp_ll.py -t con -d ${dx} -s ${clats_r} -n ${clatn_r} -w ${clonw_r} -e ${clone_r} -i ${forcingdir}/${cvar}_${d}.grb -o  ${forcingdirSite}/${cvar}_${d}.grb
          rm -f ${forcingdir}/${cvar}_${d}.grb

          testDate=$(add_days_to_date $date 33 | cut -c1-6)"01"
          testDate=$(add_days_to_date $testDate -1 )
          if [[ $testDate -lt $siteend ]]; then
            if [[ ${cvar} != 'lapseM' && ${FORCINGCLASS} != 'l5' ]]; then
            # Extract only first month without last hour so months can be concanated.
            ncount=$(grib_ls -p count ${forcingdirSite}/${cvar}_${d}.grb | tail -n -1 | awk '{print $1}')
            grib_copy -w count!=${ncount} ${forcingdirSite}/${cvar}_${d}.grb ${forcingdirSite}/tmp_${d}.grb
            mv -f ${forcingdirSite}/tmp_${d}.grb ${forcingdirSite}/${cvar}_${d}.grb
            fi
          fi
        else # Data have been already preProcessed, skip this.
          mv ${forcingdir}/${cvar}_${d}.grb ${forcingdirSite}/${cvar}_${d}.grb
        fi 
        YYYY=$(echo ${siteini} | cut -c1-4)
        MM=$(echo ${siteini} | cut -c5-6)
        DD=$(echo ${siteini} | cut -c7-8)
        python3 << EOF
import sys
sys.path.append("${scriptsdir}/osm_pyutils")
import convert_osm_forcing_grb2nc as conv
conv.process_LL("${climfile}","${forcingdirSite}/${cvar}_${d}.grb","seconds since ${YYYY}-${MM}-${DD} 00:00:00",1)
EOF
        [[ ${cvar} != 'lapseM' ]] && flistMerge="${flistMerge} ${forcingdirSite}/${cvar}_${d}.nc"
      done
      if [[ ${lapseCorr:-false} = true ]]; then
        # Interpolate and cut the orography difference file - create once then copy over
        if [[ ! -r "${forcingdirSite}/orodiff_interp_${first_dateMonth}.nc" ]] ; then
          python3 ${scriptsdir}/osm_pyutils/interp_ll.py -t con -d ${dx} -s ${clats_r} -n ${clatn_r} -w ${clonw_r} -e ${clone_r} -i ${ORODIFF_FILE} -o  ${forcingdirSite}/orodiff_interp_${d}.grb
          cdo -f nc4c -z zip_6 -t ecmwf copy ${forcingdirSite}/orodiff_interp_${d}.grb ${forcingdirSite}/orodiff_interp_${d}.nc
        else
          cp -f ${forcingdirSite}/orodiff_interp_${first_dateMonth}.nc ${forcingdirSite}/orodiff_interp_${d}.nc
        fi
        # Apply forcing lapse rate correction:
        ${scriptsdir}/test_corr.bash -T ${forcingdirSite}/Tair_${d}.nc -Q ${forcingdirSite}/Qair_${d}.nc -P ${forcingdirSite}/PSurf_${d}.nc -L ${forcingdirSite}/lapseM_${d}.nc -E ${forcingdirSite}/orodiff_interp_${d}.nc -W ${forcingdirSite} 
      fi
      # End lapse rate correction
     
      cdo -O -f nc4c -z zip_6 merge $flistMerge ${forcingdirSite}/${site}_${d}.nc
      rm -f $flistMerge
    fi
  
    i=$((i+1))
  done

  rm -f ${forcingdir}/forcing_ea_1_oper_1_${d}.tar.gz
  rm ${forcingdirSite}/*_${d}.grb
  rm -rf ${forcingdirDate}/
  
  date=$(add_days_to_date $date 33 | cut -c1-6)"01"
done
# ENDYEARLOOP

