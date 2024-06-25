#!/bin/bash
# (C) Copyright 2021- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

#
#   Job to  get and transform to netcdf initial and clim files for osm CaMa-Flood
#
#  Author: G. Arduini 2024, adapted from camaflood model calib 
#

set -eux

#
OUTDIR=${1}
sitelist=($(cat ${WORKDIR}/site_list.txt))
iniDlist=($(cat ${WORKDIR}/iniDate_list.txt))
endDlist=($(cat ${WORKDIR}/endDate_list.txt))

site=${sitelist[0]}_${iniDlist[0]:0:4}-${endDlist[0]:0:4}

##===============================================
## 0. DEFAULTS
FHRES_CATMXY="${FMAP}/1min.catmxy.nc"
FHRES_GRDARE="${FMAP}/1min.grdare.nc"

echo ${dx} ${clatn} ${clats} ${clonw} ${clone}

case ${CMF_RES} in
  glb_15min) case ${dx} in
                 "0.25")  nmaxRC=48; nmaxIRC=42;   nmaxR=40; nmaxIR=125 ;;
                 "0.10") nmaxRC=48; nmaxIRC=42;   nmaxR=30; nmaxIR=40 ;;
                 *)      nmaxRC=48; nmaxIRC=42;   nmaxR=156; nmaxIR=40 ;;
             esac 
             ;;
  glb_06min) case ${dx} in
                 "0.25") nmaxRC=22; nmaxIRC=67;   nmaxR=20; nmaxIR=302 ;;
                 "0.10") nmaxRC=22; nmaxIRC=67;   nmaxR=23; nmaxIR=78  ;;
                 *)      nmaxRC=22; nmaxIRC=67;   nmaxR=23; nmaxIR=78  ;;
             esac 
             ;;
  glb_03min) case ${dx} in
                 "0.25")  nmaxRC=13; nmaxIRC=109;  nmaxR=13; nmaxIR=615 ;;
                 "0.10") nmaxRC=13; nmaxIRC=109;  nmaxR=25; nmaxIR=135 ;;
                 "0.05") nmaxRC=13; nmaxIRC=109;  nmaxR=48; nmaxIR=152 ;;
                 *)      nmaxRC=13; nmaxIRC=109;  nmaxR=48; nmaxIR=152 ;;
             esac
             ;;
esac

cd $WORKDIR

##===============================================

##===============================================
# check if you want to ecp from ecfs or cp from perm
FNCDATA_L=ncdata.nc 
FBIFP_L=bifprm.txt
FINPMAT_L=inpmat.nc
cp -f ${FIXDIR}/$FBIFP_L   ${WORKDIR}/glob_$FBIFP_L
cp -f ${FIXDIR}/$FNCDATA_L ${WORKDIR}/glob_$FNCDATA_L
cp -f ${FIXDIR}/$FINPMAT_L ${WORKDIR}/glob_$FINPMAT_L

clip_bounding_htessel=1
nlev=100
bbox="${clonw} ${clone} ${clats} ${clatn}" #west east south north
python3 -u ${scriptsdir}/osm_pyutils/sel_region.py   -d $bbox \
                          -t clip \
                          -i glob_$FNCDATA_L \
                          -o $FNCDATA_L \
                          -pi glob_$FBIFP_L \
                          -po $FBIFP_L \
                          -inpmat glob_$FINPMAT_L \
                          -cb $clip_bounding_htessel 

## cut HTESSEL domain 
cp -f $WORKDIR/surfclim  $WORKDIR/glob_surfclim
cp -f $WORKDIR/soilinit  $WORKDIR/glob_soilinit
cdo_sel=$(cat cdo_clip_htessel.txt )
cdo -f nc4 -z zip_6 selindexbox,$cdo_sel glob_surfclim surfclim_${site}.nc
cdo -f nc4 -z zip_6 selindexbox,$cdo_sel glob_soilinit surfinit_${site}.nc

## create new inpmat based on
cp -f $FHRES_CATMXY hres_catmxy.nc 
cp -f $FHRES_GRDARE hres_grdare.nc  

ix0=$(cat clip_cama_ix0_iy0.txt | awk '{print $1}')
iy0=$(cat clip_cama_ix0_iy0.txt | awk '{print $2}')
CINV=""
#if [[ $CMF_RES == 'glb_01min' && $LECMF2LAKEC == "0"  ]] then  
if [[ ${compute_inv:-false} = false ]]; then  
    CINV="-cinv"
    nmaxIRC=$nmaxRC
    nmaxIR=$nmaxR
fi
python3 -u ${scriptsdir}/osm_pyutils/gen_inpmat.py -igrid surfinit_${site}.nc \
                        -iriv glob_$FNCDATA_L \
                        -ihcat hres_catmxy.nc \
                        -iharea hres_grdare.nc \
                        -o inpmat_tmp.nc \
                        -m pmask.nc \
                        ${CINV}  \
                        -ix0 $ix0 -iy0 $iy0 -n $nmaxR -nI $nmaxIR


cdo_sel=$(cat cdo_clip_cama.txt )
cdo -f nc4 -z zip_6 -selindexbox,$cdo_sel -selvar,inpa,inpx,inpy,nlev inpmat_tmp.nc inpmat.nc 
ncks -A -v lonin,latin,inpaI,inpxI,inpyI,nlevI inpmat_tmp.nc inpmat.nc 

ncks -O -v ctmare,elevtn,nxtdst,rivlen,fldhgt,lat,lon,nextx,nexty $FNCDATA_L rivclim.nc 
cdo -f nc4 -z zip_6  -selindexbox,$cdo_sel $FIXDIR/rivpar.nc rivpar.nc
cdo -f nc4 -z zip_6  -selindexbox,$cdo_sel $FIXDIR/outclm.nc outclm.nc 


NX=$(ncdump -h rivclim.nc | grep "^.lon = " |  head -1 | awk '{print $3}')
NY=$(ncdump -h rivclim.nc | grep "^.lat = " |  head -1 | awk '{print $3}')
NLFP=$(ncdump -h rivclim.nc | grep "^.lev = " |  head -1 | awk '{print $3}')
NXIN=$(ncdump -h inpmat.nc | grep "^.lonin = " |  head -1 | awk '{print $3}')
NYIN=$(ncdump -h inpmat.nc | grep "^.latin = " |  head -1 | awk '{print $3}')
INPN=$(ncdump -h inpmat.nc | grep "^.lev = " |  head -1 | awk '{print $3}')

cat > diminfo.txt << EOF
$NX       !! nXX
$NY       !! nYY
$NLFP     !! floodplain layer
$NXIN     !! input nXX 
$NYIN     !! input nYY 
$INPN     !! input num
NONE
0     !! west  edge
0     !! east  edge
0     !! north edge
0     !! south edge
EOF

cp -f surfinit_${site}.nc $OUTDIR/
cp -f surfclim_${site}.nc $OUTDIR/
for ff in bifprm.txt cdo_clip_htessel.txt pmask.nc clip_cama_ix0_iy0.txt inpmat.nc cdo_clip_cama.txt  $FNCDATA_L rivclim.nc rivpar.nc outclm.nc diminfo.txt
do 
  ff_b=$(echo $ff | cut -d '.' -f 1)
  ext=$(echo $ff | cut -d '.' -f 2)
  cp -f $ff $OUTDIR/${ff_b}_${site}.${ext}
done


exit 0 
