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
MARS_CMD=mars

outdir=${1}

INIDATES=(`cat ${inidir}/iniDate_list.txt`)
ENDDATES=(`cat ${inidir}/endDate_list.txt`)
sitenames=(`cat ${inidir}/site_list.txt`)

ndates=${#INIDATES[@]}
MRESOL=$(gridT $RESOL $GTYPE )

export MARS_READANY_BUFFER_SIZE=4132480200
export MIR_GRIB_INPUT_BUFFER_SIZE=7688961960

mkdir -p $inidir
mkdir -p $outdir
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
tmp = aluvd*0.45976+0.54024*alnid
mv.write("tmp_alb",tmp)
EOF
python3 < bar
rm -f bar
grib_set -s shortName=al tmp_alb month_alb
rm -f tmp_alb
## LAIH
rm -f month_lai 
cat month_laih >> month_lai
cat month_lail >> month_lai

##  Merge with clim
cat month_lai >> clim.grb 
cat month_alb >> clim.grb 
rm -f month_lai month_alb 
# Regular lat/lon
if [[  ${regional:-false} = true && ${RUN_CMF:-false} = false  ]];   then
  clatn_g=$clatn ; clats_g=$clats
  clonw_g=$clonw ; clone_g=$clone
else # clipping is done afterward, in prepare_basin
  clatn_g=$(echo "90-${dx}/2.0" | bc -l ); clats_g=$(echo "-90+${dx}/2.0" | bc -l )
  clonw_g=$(echo "-180+${dx}/2.0" | bc -l ); clone_g=$(echo "180-${dx}/2.0" | bc -l )
fi

# Soil type, Type of Low/High vegetation and lsmgrd" are interpolated with nearest-neighbour
field_nn="slt/tvl/tvh/lsmgrd"
grib_copy -w shortName=cl clim.grb _cl
grib_copy -w shortName!=cl clim.grb _nocl
grib_copy -w shortName=${field_nn} _nocl nn_interp.grb
grib_copy -w shortName!=${field_nn} _nocl gridbox_avg.grb
# Use metview-python for interpolation to regular lat/lon
python3 ${scriptsdir}/osm_pyutils/interp_ll.py -t bil -d ${dx} -s ${clats_g} -n ${clatn_g} -w ${clonw_g} -e ${clone_g} -i gridbox_avg.grb -o clim_2.grb
python3 ${scriptsdir}/osm_pyutils/interp_ll.py -t bil -d ${dx} -s ${clats_g} -n ${clatn_g} -w ${clonw_g} -e ${clone_g} -i _cl -o clim_3.grb
python3 ${scriptsdir}/osm_pyutils/interp_ll.py -t nn -d ${dx} -s ${clats_g} -n ${clatn_g} -w ${clonw_g} -e ${clone_g} -i  nn_interp.grb -o clim_4.grb

cat clim_3.grb clim_4.grb >> clim_2.grb
mv clim_2.grb clim.grb

##===================================================
## 2.  Extract ini conditions either from MARS or CDS
PPARAM=SWVL1/SWVL2/SWVL3/SWVL4/STL1/STL2/STL3/STL4/SKT/ASN/RSN/SD/TSN/ISTL1/ISTL2/ISTL3/ISTL4/SRC/CI/8.228/9.228/10.228/11.228/12.228/13.228/14.228

i=0
#while (( $i < $ndates )); do
INIDATE=${INIDATES[$i]}
ENDDATE=${ENDDATES[$i]}
site=${sitenames[$i]}

if [[ ${RETRIEVE_WITH} == 'mars' ]]; then
  $MARS_CMD << EOF
  retrieve, stream=${INISTREAM}, class=ea, expver=$INIEXPVER,date=$INIDATE,
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
  
python3 ${scriptsdir}/osm_pyutils/interp_ll.py -t bil -s ${clats_g} -n ${clatn_g} -w ${clonw_g} -e ${clone_g}  -d ${dx} -i init.grb  -o init_2.grb
grib_set -w shortName=cl -s shortName=ci init_2.grb init.grb

rm -rf init_2.grb

## Defaults defining points:
LLAND=${LLAND-True}
LLAKE=${LLAKE-True}
LOCEAN=${LOCEAN-False}
BBOX=${BBOX-"-190,190,-91,91"}

##============================================
## convert to netcdf 

cp clim.grb clim_LL.grb
cp init.grb init_LL.grb

# Define and compute mask grib for land point only:
lland=1
llake=1
llocean=0
grib_copy -w shortName=lsm clim_LL.grb lsm_LL.grb
grib_copy -w shortName=cl clim_LL.grb cl_LL.grb

rm -f bar
cat >> bar << EOF
import metview as mv
lsm=mv.read("lsm_LL.grb")
cl=mv.read("cl_LL.grb")
ldland=lsm>=0.5
ldlake=(cl>=0.5*(1.0-lsm))
ldocean=(lsm<0.5)*(ldlake==0)
ldmask=(ldland*${lland}) + (ldlake * ${llake}) + (ldocean*${llocean})
ldmask_norm=(ldmask>=1.0)*1 + (ldmask<1)*0
mv.write("mask_LL.grb", ldmask_norm)
EOF
python3 < bar
rm -f bar

rm -f lsm_LL.grb cl_LL.grb

# convert in netcdf files, mask in a separate file.
grib_set -s edition=1 init_LL.grb pppp; mv -f pppp init_LL.grb
grib_set -s edition=1 mask_LL.grb pppp; mv -f pppp mask_LL.grb

cdo -f nc -t ecmwf copy init_LL.grb init_LL.nc
cdo -f nc -t ecmwf copy mask_LL.grb mask_LL.nc

# Temporary parameter!!
grib_copy -w paramId=200026 clim_LL.grb wetlandf.grb
grib_copy -w paramId=200199 clim_LL.grb urban.grb

# Check if any grib1 field in clim.grb
grib_ed=$(grib_ls -p edition clim.grb | tail -n+3 | head -n-3)
grib1_exist=false
for ed in ${grib_ed}; do
  if [[ $ed = 1 ]]; then
      grib1_exist=true
      break
  fi
done
if [[ $grib1_exist = true ]]; then
  grib_copy -w edition=1 clim.grb ed1.grb
fi
grib_copy -w edition=2 clim.grb ed2.grb

grib_copy -w paramId=66 ed2.grb lai_lv.grb
grib_copy -w paramId=67 ed2.grb lai_hv.grb
grib_copy -w shortName=al ed2.grb albedo.grb
grib_copy -w paramId!=66/67/174/200026 ed2.grb clim_LL_notime.grb
# workaround to avoid seg fault for sotype=0 as clake is interp with bilin
rm -f bar
cat >> bar << EOF
import metview as mv
clim=mv.read("clim_LL_notime.grb")
slt=clim.select(shortName="slt")
slt_new=slt*(slt>0)+3*(slt==0)
mv.write("tmp_slt",slt_new)
EOF
python3 < bar
rm -f bar

grib_copy -w shortName!=slt clim_LL_notime.grb clim_noslt
cat clim_noslt tmp_slt > clim_LL_notime.grb
rm -f tmp_slt clim_noslt

cdo -f nc -t ecmwf copy clim_LL_notime.grb clim_LL_notime.nc

if [[ $grib1_exist = true ]]; then
  cdo -f nc -t ecmwf copy ed1.grb ed1.nc
  # Workaround when using cdo/2.2.0
  if [[ $XDATA_DIR = *"v015"* ]]; then 
  ncrename -v TVH,tvh ed1.nc; ncrename -v TVL,tvl ed1.nc
  fi
fi

cdo -f nc -t ecmwf copy lai_lv.grb lai_lv.nc
cdo -f nc -t ecmwf copy lai_hv.grb lai_hv.nc
cdo -f nc -t ecmwf copy albedo.grb albedo.nc
# Workaround when using some cdo versions...
albFileVar=$(cdo infon albedo.nc | head -n2 | tail -n-1 | awk '{print $13 }')
if [[ $albFileVar != 'al' ]]; then
  ncrename -v param193.19.0,al albedo.nc tmp.nc ; mv -f tmp.nc albedo.nc
fi

# Temporary parameters!!
cdo -f nc -t ecmwf copy wetlandf.grb wetlandf.nc
cdo -f nc -t ecmwf copy urban.grb urban.nc
#cdo -f nc -t ecmwf copy c3_c4.grb c3_c4.nc
#ncwa -a time c3_c4.nc c3_c4_tmp.nc ; mv -f c3_c4_tmp.nc c3_c4.nc
#ncwa -a time urban.nc urban_tmp.nc ; mv -f urban_tmp.nc urban.nc
#ncks -A c3_c4.nc clim_LL.nc
#ncks -A urban.nc clim_LL.nc

# Remove time and height dimensions if presents
ncwa -a time init_LL.nc init_LL2.nc ; mv -f init_LL2.nc init_LL.nc
ncwa -a time clim_LL_notime.nc clim_LL_notime2.nc ; mv -f clim_LL_notime2.nc clim_LL_notime.nc
ncwa -a time mask_LL.nc mask_LL2.nc ; mv -f mask_LL2.nc mask_LL.nc

mv -f clim_LL_notime.nc clim_LL.nc
ncks -x -v time clim_LL.nc clim_LL2.nc ; mv clim_LL2.nc clim_LL.nc
[[ $grib1_exist = true ]] && ncks -A ed1.nc clim_LL.nc
ncks -A lai_lv.nc clim_LL.nc
ncks -A lai_hv.nc clim_LL.nc
ncks -A albedo.nc clim_LL.nc
ncks -A wetlandf.nc clim_LL.nc

ncrename -v LSM,Mask mask_LL.nc

# Fix initial condition files to follow ecLand standard
python3 <<eof
import sys
sys.path.append("${scriptsdir}/osm_pyutils")
from iniclim_forcing_LL import *
fclim='clim_LL.nc'
finit='init_LL.nc'
fmask='mask_LL.nc'

fix_iniclim_LL(finit,fmask,'soilinit',ftype='ini')
fix_iniclim_LL(fclim,fmask,'surfclim',ftype='clim')
eof

ncks -x -v x surfclim surfclim.tmp ; mv -f surfclim.tmp surfclim
ncks -A mask_LL.nc surfclim
if [[  ${regional:-false} = true && ${RUN_CMF:-false} = false  ]];   then
  # No extra operations, get final file in target dir
  mv -f soilinit ${outdir}/surfinit_${site}_${INIDATE:0:4}-${ENDDATE:0:4}.nc
  cp -f surfclim ${outdir}/surfclim_${site}_${INIDATE:0:4}-${ENDDATE:0:4}.nc
fi

#i=$((i+1))
#done Not yet looping on sites/regions.


#EOF
