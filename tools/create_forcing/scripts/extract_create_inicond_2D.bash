#!/bin/bash
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# This script extracts the climate fields needed for ecLand from ERA5 
# and the initial conditions for each site in regional (2D) configuration.

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
            climFilesPresent=false
            break
        fi
    done

    # Return true or false
    $climFilesPresent && return 0 || return 1
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


# Static "climate" fields required by ecLand:
# land-sea mask, orography, lake cover, soil type, angle of subgrid orog, anisotropy of subgrid orog, slope of subgrid orog, standard deviation of subgrid orog, high vegetation cover, low vegetation cover, type of high vegetation, type of low vegetation, lake depth, monthly LAI high veg, monthly LAI low veg, monthly wetland fract, urban coverage, c3/c4 vegetation type.
ClimFiles=("lsm" "oro" "clake" "slt" "anor" "isor" "slor" "sdor" "cvh" "cvl" "tvh" "tvl" "lakedl" "month_laih" "month_lail" "wetlandf" "urban" "c3_c4_mask.grb")


mkdir -p $inidir
mkdir -p $outdir
cd $inidir # we work in inidir
##===============================================
## 1.1 Get climate fields: static 

rm -f clim.grb 
rm -f lsmoro clake slt sfc lakedl wetlandf urban c3_c4_mask.grb
rm -f month_alnid month_aluvd month_laih month_lail
# Check if ecmwf machine
if [[ $XDATA_DIR == *rdx* ]]; then
  for ff in lsmoro clake slt sfc lakedl wetlandf urban c3_c4_mask.grb month_laih month_lail; do 
    cp $XDATA_DIR/${ff} ./
    if [[ ${ff} == 'wetlandf' ]]; then 
        grib_set -s edition=1,paramId=200026 ${ff} tmp.grb; 
        mv -f tmp.grb ${ff}
    fi
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
grib_set -s paramId=33,edition=2 tmp_alb tmp_alb2
grib_set -s paramIdECMF=174      tmp_alb2 month_alb
rm -f tmp_alb tmp_alb2

##  Merge with clim
cat month_alb >> clim.grb 
rm -f month_alb 

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

# Define and compute mask grib for land point only:
lland=1
llake=1
llocean=0
grib_copy -w shortName=lsm clim.grb lsm.grb
grib_copy -w shortName=cl clim.grb cl.grb

rm -f bar
cat >> bar << EOF
import metview as mv
lsm=mv.read("lsm.grb")
cl=mv.read("cl.grb")
ldland=lsm>=0.5
ldlake=(cl>=0.5*(1.0-lsm))
ldocean=(lsm<0.5)*(ldlake==0)
ldmask=(ldland*${lland}) + (ldlake * ${llake}) + (ldocean*${llocean})
ldmask_norm=(ldmask>=1.0)*1 + (ldmask<1)*0
mv.write("mask.grb", ldmask_norm)
EOF
python3 < bar
rm -f bar

rm -f lsm.grb cl.grb

# convert in netcdf files, mask in a separate file. Init and mask in grib1
grib_set -s edition=1 init.grb pppp; mv -f pppp init.grb
grib_set -s edition=1 mask.grb pppp; mv -f pppp mask.grb

cdo -f nc -t ecmwf copy init.grb init.nc
cdo -f nc -t ecmwf copy mask.grb mask.nc

# Extract Temporary parameter from clim.grb
grib_copy -w paramId=200026 clim.grb wetlandf.grb
grib_copy -w paramId!=200026 clim.grb tmp.grb; mv -f tmp.grb clim.grb
#grib_copy -w paramId=200199 clim.grb urban.grb

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

# Extract time-varying (climatological) fields from ed2
grib_copy -w paramId=66 ed2.grb lai_lv.grb
grib_copy -w paramId=67 ed2.grb lai_hv.grb
grib_copy -w shortName=al ed2.grb albedo.grb
grib_copy -w paramId!=66/67/174 ed2.grb clim_notime.grb
# workaround to avoid seg fault for sotype=0 as clake is interp with bilin
rm -f bar
cat >> bar << EOF
import metview as mv
clim=mv.read("clim_notime.grb")
slt=clim.select(shortName="slt")
slt_new=slt*(slt>0)+3*(slt==0)
mv.write("tmp_slt",slt_new)
EOF
python3 < bar
rm -f bar

# Remove old slt and put new slt with the fix for clake
grib_copy -w shortName!=slt clim_notime.grb clim_noslt
cat clim_noslt tmp_slt > clim_notime.grb
rm -f tmp_slt clim_noslt

# Convert in netcdf the first set of fields "clim_notime" in grib2
cdo -f nc -t ecmwf copy clim_notime.grb clim_notime.nc

# Separate handling of grib1
if [[ $grib1_exist = true ]]; then
  cdo -f nc -t ecmwf copy ed1.grb ed1.nc
  # Workaround when using cdo/2.2.0
  if [[ $XDATA_DIR = *"v015"* ]]; then 
  ncrename -v TVH,tvh ed1.nc; ncrename -v TVL,tvl ed1.nc
  fi
fi

# Convert to netCDF time-varying (climatological) fields from ed2
cdo -f nc -t ecmwf copy lai_lv.grb lai_lv.nc
cdo -f nc -t ecmwf copy lai_hv.grb lai_hv.nc
cdo -f nc -t ecmwf copy albedo.grb albedo.nc
# Workaround when using some cdo versions...
albFileVar=$(cdo infon albedo.nc | head -n2 | tail -n-1 | awk '{print $13 }')
if [[ $albFileVar != 'al' ]]; then
  ncrename -v param193.19.0,al albedo.nc tmp.nc ; mv -f tmp.nc albedo.nc
fi

# Wetlands handled separately - in grib1
cdo -f nc -t ecmwf copy wetlandf.grb wetlandf.nc
wetlandVar=$(cdo infon wetlandf.nc | tail -n1 | awk '{print $13}')
if [[ ${wetlandVar} != 'cldiff' ]]; then
    ncrename -v ${wetlandVar},'cldiff' wetlandf.nc 
fi

# Remove time and height dimensions if presents in init, clim_notime and mask
ncwa -a time init.nc init2.nc ; mv -f init2.nc init.nc
ncwa -a time clim_notime.nc clim.nc
ncwa -a time mask.nc mask2.nc ; mv -f mask2.nc mask.nc

# Remove time variable from clim.nc
ncks -x -v time clim.nc clim_2.nc ; mv clim_2.nc clim.nc

# Append grib1 and time-varying fields to clim.nc
[[ $grib1_exist = true ]] && ncks -A ed1.nc clim.nc
ncks -A lai_lv.nc clim.nc
ncks -A lai_hv.nc clim.nc
ncks -A albedo.nc clim.nc
ncks -A wetlandf.nc clim.nc

# Rename LSM field to "Mask"
ncrename -v LSM,Mask mask.nc

# Workaround for some eccodes setting of lake depth parameter in the clim.grb
dlVar=$(cdo infon clim.nc | grep "param0." | awk '{print $13}')
if [[ $dlVar != '' ]]; then
  ncrename -v param0.2.1,dl clim.nc tmp.nc ; mv -f tmp.nc clim.nc
fi

# Fix initial condition files to follow ecLand standard
python3 <<eof
import sys
sys.path.append("${scriptsdir}/osm_pyutils")
from iniclim_forcing_LL import *
fclim='clim.nc'
finit='init.nc'
fmask='mask.nc'

fix_iniclim_LL(finit,fmask,'soilinit',ftype='ini')
fix_iniclim_LL(fclim,fmask,'surfclim',ftype='clim')
eof

ncks -x -v x surfclim surfclim.tmp ; mv -f surfclim.tmp surfclim
ncks -A mask.nc surfclim
if [[  ${regional:-false} = true && ${RUN_CMF:-false} = false  ]];   then
  # No extra operations, get final file in target dir
  mv -f soilinit ${outdir}/surfinit_${site}_${INIDATE:0:4}-${ENDDATE:0:4}.nc
  cp -f surfclim ${outdir}/surfclim_${site}_${INIDATE:0:4}-${ENDDATE:0:4}.nc
fi

#i=$((i+1))
#done Not yet looping on sites/regions.


#EOF
