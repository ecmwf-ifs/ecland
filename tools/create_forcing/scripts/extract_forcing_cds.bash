#!/bin/bash
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

#
#   Job to retrieve archived near-surface meteorological forcing
# 

set -eux 

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


##===================================
## HARDCODED variables 
##===================================
EPS="0.000001"                     # small number for precip in kg.m-2.s-1 == 0.086 mm/day
NFPDAY=2                           # NFPDAY : # forecast per day 
MARS_CMD=mars
##=====================================

##===================================
## Variables: can be changes vi environment in calling script 
##===================================
FORCINGLEV=${FORCINGLEV:-0}      # model level to extract data: 0: lowest model level, 1: 2nd lowest model level..
GRID=${GRID:-XXX}                 # grid  
LEXTRACT_ML=${LEXTRACT_ML:-true}  # extract model level: t/u/v/q 
LEXTRACT_LNSP=${LEXTRACT_LNSP:-true}  # extract model level: t/u/v/q 
LEXTRACT_SFC=${LEXTRACT_SFC:-true} # extract surface : SSRD/STRD/LSP/CP/SF/SP
BINS=${BINS:-'.'}                 # location of binaries / scripts / 


##===================================
## Default values - can be changes via command line input
##==================================
EXTDATE="20100101" # extraction date: YYYYMMDD for daily / YYYMMDD00 for monthly chunk
EXPVER='1'         # experiment version
CLASS=xxx          # class: ERA5 only 
FREQ=3             # hourly frequency only  
DATADIR='.'        # directory to save output 
STREAM='oper'      # forecast stream to extract  
preProc=true       # Preprocess data on a regular lat/lon grid
gridInfo=      # dx,clatn,clats,clone,clonw ; in this order
##===================================
## Change default via command line options
##===================================


while getopts e:i:d:f:c:s:m:P:g: option
do
  case $option in
    e) EXTDATE=$OPTARG;;
    i) EXPVER=$OPTARG;;
    d) DATADIR=$OPTARG;;
    f) FREQ=$OPTARG;;
    c) CLASS=$OPTARG;;
    s) STREAM=$OPTARG;;
    P) preProc=$OPTARG;;
    g) gridInfo=$OPTARG;;
    \?) exit 1;;
  esac
done

echo '============================='
echo '        OPTIONS              '
echo "-e EXTDATE=$EXTDATE" 
echo "-i EXPVER=$EXPVER" 
echo "-d DATADIR=$DATADIR "
echo "-f FREQ=$FREQ"
echo "-c CLASS=$CLASS"
echo "-s STREAM=$STREAM"
echo "-P preProc=$preProc"
[[ ${preProc} = true ]] && echo "-g gridInfo=$gridInfo"
echo ''

mkdir -p $DATADIR
cd $DATADIR

EXPVER0=$EXPVER



##====================================
## define date/time variables 
##===================================
YYYY=$(echo ${EXTDATE} | cut -c1-4)
MM=$(echo ${EXTDATE} | cut -c5-6)
DD=$(echo ${EXTDATE} | cut -c7-8)

D1=01
D2=$( add_days_to_date $( add_days_to_date ${YYYY}${MM}${D1} 33 | cut -c1-6)"01" -1 | cut -c 7-8)

  
DATE1=${YYYY}${MM}${D1}  # 1st date
DATE2=${YYYY}${MM}${D2}  # las date 
  case $FREQ in
    1|3)
      echo "FREQ=${FREQ}";;
    *)
      echo "Attention, for ERA5 forcing only hourly or 3-hourly analysis are available"
      echo "Check your FREQ variable"
      exit -1
    ;;
  esac

  TIME="06/18"  # time to extract 

  STEPS_VAR=$( echo $(seq -w $FREQ  $FREQ 12 ) | sed "s/ /\//g")  # steps to retrieve ML
  STEPS_FLX=$STEPS_VAR   # steps to retrieve SFC
  STEPS_FLXn=$STEPS_FLX  # steps to retrieve SFC   
  STEPS_FLXp=$STEPS_FLX  # steps to retrieve SFC   

  ## special case for 1st 6 hours, we need the previous day forecast     
  STEPS_VAR0=$( echo $(seq -w 6 $FREQ 12 ) | sed "s/ /\//g")  # ML
  STEPS_FLX0=$STEPS_VAR0 # SFC
  STEPS_FLX0n=$STEPS_VAR0
  STEPS_FLX0p=$STEPS_VAR0

  DATE0=$( add_days_to_date ${DATE1} -1)
  TIME0="18"
  CLASS0=$CLASS



# Assume 137 levels for most of experiment types: 
FORCINGLEV0=137
FORCINGLEV=$(( $FORCINGLEV0 - $FORCINGLEV )) 
   
FSAVE=""  # list of variables to save 

##====================================
## Extract SFC
##====================================
if [[ $LEXTRACT_SFC = true ]]; then
echo "Extracting SFC" 

gridInfoOption=""
if [[ ${gridInfo} != "" ]]; then
	gridInfoOption="--gridInfo "${dx},${clatn},${clats},${clonw},${clone}""
fi
${scriptsdir}/osm_pyutils/extract_process_cds.py --date0 ${DATE0} --time0 ${TIME0} --date1 ${DATE1} --date2 ${DATE2} --time ${TIME}\
                                             	 --steps_var0 ${STEPS_FLX0} --steps_var ${STEPS_FLX}  --stream ${STREAM} --freq ${FREQ}\
						 --dataPreProc ${preProc} --dataVar "flux" ${gridInfoOption}

fi # LEXTRACT_SFC

##====================================
## Extract model level data 
#  Data are on a spherical harmonic grid, it requires
#  a grib template on gaussian grid (--gribTemplate) to be used for regridding.
##===================================
if [[ $LEXTRACT_ML = true ]] ; then

echo "Extracting ML"

${scriptsdir}/osm_pyutils/extract_process_cds.py --date0 ${DATE0} --time0 ${TIME0} --date1 ${DATE1} --date2 ${DATE2} --time ${TIME}\
	                                         --steps_var0 ${STEPS_VAR0} --steps_var ${STEPS_VAR}  --stream ${STREAM} --freq ${FREQ}\
						 --dataPreProc ${preProc} --dataVar "ml" --gribTemplate "LWdown.grb" ${gridInfoOption}


fi # LEXTRACT_ML

# ##=================================
### Final clean
