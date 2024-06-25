#!/bin/bash
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set -eux

#** Script to extract era5 forcing fields needed for ecLand run **#
# - Extract forcing fields from ERA5
# - Create forcing fields for each site
# - Save forcing fields in netcdf format

#**************************************************************#


get_lsm() {
    xdata_dir=$1
    lsmgrib=$2
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

# process grib variable for given date extracted from mars or local disk
# - reduce file size to speed up nearest neighbour in extract_site
# - extract only one month of forcing if not the end date to avoid repetitions
process_variable() {
    fileIn=$1
    date=$2
    siteend=$3
    lat=$4
    lon=$5
    fileOut=$6
    testDate=$(add_days_to_date $date 33 | cut -c1-6)"01"
    testDate=$(add_days_to_date $testDate -1 )
    # Reduce file size to speed up nearest neighbour in extract_site
    ${scriptsdir}/osm_pyutils/sel_region_point_forcing.py -i  ${fileIn} \
                                                           -o ${fileOut} \
                                                           -l ${lat} -L ${lon}

    if [[ $testDate -lt $siteend && ${fileOut} != *"lsm"* ]]; then
        # Extract only one month of forcing
        ncount=$(grib_ls -p count ${fileOut} | tail -n -1 | awk '{print $1}')
        grib_copy -w count!=${ncount} ${fileOut} tmp.grb
        mv -f tmp.grb ${fileOut}
    fi
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
        "")
            cp -f "${preProcForcdir}"/*_"${d}".grb "${forcingdirDate}/" ;;
        *)
            echo "Unsupported file extension: $fileExt" ;;
    esac
}


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

# First initial and last end date of all sites
idate=$1
edate=$2

# Output forcing Directory
odir=$3

# Script dir
scriptsdir=$4

# Define iniDate and end Date: add option for monthly chuncks
iniDates=($(cat ${forcingdir}/iniDate_list.txt))
endDates=($(cat ${forcingdir}/endDate_list.txt))
sitelist=($(cat ${forcingdir}/site_list.txt))
lats=($(cat ${forcingdir}/lat_list.txt))
lons=($(cat ${forcingdir}/lon_list.txt))

nsites=${#sitelist[@]}
MRESOL=$(gridT $RESOL $GTYPE )

# Forcing Variables required by ecLand:
varlist=("PSurf" "Tair" "Qair" "Wind_E" "Wind_N" "SWdown" "LWdown" "Rainf" "Snowf" "Ctpf")
mkdir -p $odir
mkdir -p $forcingdir

# CLEAR SITE FOLDERS
i=0
for site in ${sitelist[@]}; do
  sdir=${forcingdir}/${site}
  rm -rf ${forcingdir}/${site}
done

# Start

date=$idate
while [[ $date -le $edate ]]; do
  echo $date

  d=${date:0:6}00
  forcingdirDate=${forcingdir}_${d}
  mkdir -p $forcingdirDate

  #------------------------------
  # Extract/Download forcing data
  test -e ${preProcForcdir}/${forcingCommonName}_${d}* && retM=0 || retM=1
  #retM=1
  if [[ $retM = 1 ]]; then # File does not exist, download it
     case "${RETRIEVE_WITH}" in
         "mars") ${scriptsdir}/extract_forcing_mars.bash -e ${d} -d $forcingdirDate -i 1 -f 1 -c ${FORCINGCLASS} -s ${FORCINGSTREAM} 
             ;;
         "cds")  time ${scriptsdir}/extract_forcing_cds.bash  -e ${d} -d $forcingdirDate -i 1 -f 1 -c ea -s oper -P true
             ;;
         *) echo "download method not implemented " ;;
     esac
  else # File already exists, no need to download
    # Assuming filename of the compressed file is forcing_*_${d}.tar.gz
    # Assuming filename of forcing files ${varname}_${d}.grb

    extract_preProc_forcing "${preProcForcdir}" "${forcingCommonName}" "${d}"

  fi
  # Save forcing files if SAVE_FORCING_GRIB is true/True
  if [[ ${SAVE_FORCING_GRIB} = true ]]; then
     mkdir -p ${preProcForcdir}
     tar -zcf ${preProcForcdir}/${forcingCommonName}_${d}.tar.gz -C ${forcingdirDate} ${varlist[@]/%/.grb}
  fi
  for  var in ${varlist[@]}; do
    echo "copying variable:" ${var}
    mv $forcingdirDate/${var}.grb $forcingdir/${var}_${d}.grb 
  done
  # End of download
  #------------------------------


  #----------------------------------------------------
  # SITE LOOP, extract and create forcing for each site
  ciDate=$date
  ceDate=$(add_days_to_date $date 33 | cut -c1-6)"01"
  ceDate=$(add_days_to_date $ceDate -1)
  i=0
  while [[ $i -lt $nsites ]]; do
    siteini=${iniDates[$i]}
    siteend=${endDates[$i]}
    site=${sitelist[$i]}
    sdir=${forcingdir}/${site}
    lat=${lats[$i]}
    lon=${lons[$i]}
    if [[ ! -d $sdir ]]; then
      mkdir -p $sdir
    fi
    flistNcVar=() # initialise list of forcing files for each site
  
    # create site forcing for the time period if within the range of the site period
    if [[ $siteini -le $date && $siteend -ge $date ]]; then
      forcingdirSite=${forcingdir}/${site}/
      mkdir -p $forcingdirSite
      # Only grib processing allowed at the moment
        # List files that will be pcreated
        flistNcVar=("${varlist[@]/#/${forcingdirSite}/}") ; flistNcVar=("${flistNcVar[@]/%/_${d}.nc}")
        lsmgrib=${forcingdir}/lsm_${d}
        get_lsm ${XDATA_DIR} ${lsmgrib}
        ${scriptsdir}/osm_pyutils/process_site_var.py  -v ${varlist[@]} -w ${forcingdir} -d ${date} -e ${siteend} -o ${forcingdirSite} -l ${lat} -L ${lon} -s ${scriptsdir}
        cdo -f nc4 -z zip_6 merge ${forcingdirSite}/*_${d}.nc ${forcingdirSite}/${site}_${d}.nc

        rm -f ${forcingdirSite}/*_${d}.grb
        rm -f ${flistNcVar[@]}
    fi
  
    i=$((i+1))
  done
  #----------------------------------------------------

  rm -f ${forcingdir}/forcing_*_${d}.tar.gz
  rm -f ${forcingdir}/*_${d}.grb
  rm -rf ${forcingdirDate}/
  
  date=$(add_days_to_date $date 33 | cut -c1-6)"01"
done
# End loop in time (from first_init_date to last_end_date)
