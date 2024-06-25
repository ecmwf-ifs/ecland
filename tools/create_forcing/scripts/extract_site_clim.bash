#!/bin/bash
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# This script extracts the climate fields needed for ecLand from ERA5
# and the initial conditions for each site.


set -eu

## functions 
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


check_ClimFiles_exist() {
    local climFolder="$1"
    shift
    local climFiles=("$@")

    # Initialize a variable to track if all files exist
    local climFilesPresent=true

    # Check if all files exist
    for file in "${climFiles[@]}"; do
        if [[ ! -f "$climFolder/$file" ]]; then
            #echo "File $file does not exist in $climFolder"
            climFilesPresent=false
            break
        fi
    done

    # Return true or false
    $climFilesPresent && return 0 || return 1
}

#----
scriptsdir=$1
MARS_CMD=mars

INIDATES=(`cat ${inidir}/iniDate_list.txt`)

ndates=${#INIDATES[@]}
ndates=$((ndates-1))
MRESOL=$(gridT $RESOL $GTYPE )

# Static "climate" fields required by ecLand:
# land-sea mask, orography, lake cover, soil type, angle of subgrid orog, anisotropy of subgrid orog, slope of subgrid orog, standard deviation of subgrid orog, high vegetation cover, low vegetation cover, type of high vegetation, type of low vegetation, lake depth, monthly LAI high veg, monthly LAI low veg, monthly wetland fract, urban coverage, c3/c4 vegetation type.
ClimFiles=("lsm" "oro" "clake" "slt" "anor" "isor" "slor" "sdor" "cvh" "cvl" "tvh" "tvl" "lakedl" "month_laih" "month_lail" "wetlandf" "urban" "c3_c4_mask.grb")

mkdir -p $inidir                                            
cd $inidir # we work in inidir                              
##===============================================
## 1.1 GET CLIMATE FIELDS: static 

rm -f clim.grb 
rm -f lsmoro clake slt sfc lakedl wetlandf urban c3_c4_mask.grb
rm -f month_alnid month_aluvd month_laih month_lail
if [[ $XDATA_DIR == *rdx* ]]; then
  for ff in lsmoro clake slt sfc lakedl wetlandf urban c3_c4_mask.grb month_laih month_lail; do 
    cp $XDATA_DIR/${ff} ./
    cat ${ff} >> clim.grb 
  done
  for ff in month_alnid month_aluvd; do
    cp $XDATA_DIR/${ff} ./
  done
else
  # Check if data already on disk. Otherwise download from cds
  if check_ClimFiles_exist "$XDATA_DIR" "${ClimFiles[@]}"; then
    for ff in lsm oro clake slt anor isor slor sdor cvh cvl tvh tvl lakedl wetlandf urban c3_c4_mask.grb month_laih month_lail ; do
      cp -f "$XDATA_DIR/$ff" ./
      cat ${ff} >> clim.grb 
    done
    for ff in month_alnid month_aluvd; do
      cp $XDATA_DIR/${ff} ./
    done
  else # Download from cds
    python3 ${scriptsdir}/osm_pyutils/download_clim_cds.py -v lsm oro clake slt sfc lakedl  -r ${MRESOL} -o clim.grb
    python3 ${scriptsdir}/osm_pyutils/download_clim_cds.py -v month_laih month_lail  -r ${MRESOL} -o lai.grb --clim
    cat lai.grb >> clim.grb
    python3 ${scriptsdir}/osm_pyutils/download_clim_cds.py -v wetlandf urban c3_c4_mask.grb -r ${MRESOL} -o tmpGribCode.grb -m "tmpGrib"
    cat tmpGribCode.grb >> clim.grb
    python3 ${scriptsdir}/osm_pyutils/download_clim_cds.py -v month_alnid month_aluvd -r ${MRESOL} -o albedo_all.grb --clim
    grib_copy -w paramId=16 albedo_all.grb month_aluvd 
    grib_copy -w paramId=17 albedo_all.grb month_alnid 
    rm -f albedo_all.grb tmpGribCode.grb
  fi
fi

##==============================================
## Climatologies albedo

rm -f bar
cat >> bar << EOF
import metview as mv
alnid=mv.read("month_alnid")
aluvd=mv.read("month_aluvd")
tmp = 0.45976*aluvd+0.54024*alnid
tmp = mv.grib_set(tmp, ["shortName","al"])
mv.write("month_alb",tmp)
EOF
python3 < bar

##  Merge with clim
cat month_alb >> clim.grb 
rm -f month_lai month_alb 


##===============================================
## 2.  Extract ini conditions 
## 2.1 Default data (should be always present)
PPARAM=SWVL1/SWVL2/SWVL3/SWVL4/STL1/STL2/STL3/STL4/SKT/ASN/RSN/SD/TSN
# Assume that ice fields and lake fields are always present - otherwise needs a check.
PPARAM="$PPARAM/ISTL1/ISTL2/ISTL3/ISTL4/SRC/CI"
PPARAM="$PPARAM/8.228/9.228/10.228/11.228/12.228/13.228/14.228"

i=0
while (( $i <= $ndates )); do
INIDATE=${INIDATES[$i]}

#if [[ ! -e init_${INIDATE}.grb ]]; then

  if [[ ${RETRIEVE_WITH} == 'mars' ]]; then
  $MARS_CMD << EOF
  retrieve, stream=${INISTREAM}, class=$INICLASS, expver=$INIEXPVER,date=$INIDATE,$ensmem
  time=${INIHOUR},type=an,levtype=sfc,
  param=$PPARAM,grid=$MRESOL,
  target="init.grb"
EOF
  elif [[ ${RETRIEVE_WITH} == 'cds' ]]; then
    cat > bar << EOF
import cdsapi
c = cdsapi.Client()
EOF
   cat >> bar << EOF
c.retrieve('reanalysis-era5-complete', 
    { 
    'date'    : "$INIDATE",            
    'levtype' : "sfc",
    'param'   : "${PPARAM}",
    'stream'  : "${INISTREAM}",
    'time'    : "${INIHOUR}",
    'type'    : "an", 
    'grid'    : "$MRESOL",
    'class'   : "$INICLASS",
    'format'  : 'grib',
    }, 
    'init.grb')
EOF
   python3 < bar  #
   rm -f bar
fi
  
  mv -f init.grb init_${INIDATE}.grb
#fi

i=$((i+1))
done



#EOF
