#!/bin/bash
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


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

#----
scriptsdir=$1
MARS_CMD=mars

INIDATES=(`cat ${inidir}/iniDate_list.txt`)

ndates=${#INIDATES[@]}
ndates=$((ndates-1))
MRESOL=$(gridT $RESOL $GTYPE )

mkdir -p $inidir
cd $inidir # we work in inidir
##===============================================
## 1.1 GET CLIMATE FIELDS: static 

rm -f clim.grb 
rm -f lsmoro clake slt sfc lakedl wetlandf urban c3_c4_mask.grb
for ff in lsmoro clake slt sfc lakedl wetlandf urban c3_c4_mask.grb
do 
  if [[ $XDATA_DIR == *rdx* ]]; then
    cp $XDATA_DIR/${ff} ./
  else
    wget $XDATA_DIR/${ff} 
  fi
  cat ${ff} >> clim.grb 
done

##==============================================
## 1.2. Get Climate fields: climatologies

## Albedo
rm -f month_alnid month_aluvd month_laih month_lail
for ff in month_alnid month_aluvd month_laih month_lail
do
  if [[ ! -e ${ff} ]]; then
    if [[ $XDATA_DIR == *rdx* ]]; then
      cp $XDATA_DIR/${ff} ./
    else
      wget $XDATA_DIR/${ff} 
    fi
  fi
done
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
#rm -f bar

## LAIH
rm -f month_lai 
cat month_laih >> month_lai
cat month_lail >> month_lai

##  Merge with clim
cat month_lai >> clim.grb 
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
