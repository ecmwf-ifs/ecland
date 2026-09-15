#!/bin/bash
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# This script extracts climate fields and initial conditions for ecLand
# and outputs on REDUCED GAUSSIAN GRID (TL or TCO).
#
# Key difference from extract_create_inicond_2D.bash:
# - No interpolation to lat-lon
# - Data stays on Gaussian grid throughout
# - Uses grb2nc_gaussian.py for netCDF conversion (not CDO)

set -eu

## functions 
gridT(){
resol=$1
gtype=$2  
case ${gtype} in
  "l_2"|"_3") GT=N;;
  "_4") GT=O;;
  *) echo "GTYPE not available!"; exit -1;;
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
# Source grid (ERA5 native)
MRESOL=$(gridT $RESOL $GTYPE)

# Target Gaussian grid from environment
TARGET_GRID=${TARGET_GAUSSIAN_GRID:-$MRESOL}  # Default to source resolution

export MARS_READANY_BUFFER_SIZE=4132480200
export MIR_GRIB_INPUT_BUFFER_SIZE=7688961960

echo "=== Gaussian Grid Processing ==="
echo "Source grid: $MRESOL"
echo "Target grid: $TARGET_GRID"

mkdir -p $inidir
mkdir -p $outdir
cd $inidir

##===============================================
## 1. Get climate fields: download on native Gaussian, regrid to target Gaussian

rm -f clim.grb month_alb

# Download climate data from CDS on native Gaussian grid
if [[ ${RETRIEVE_WITH} == 'cds' ]]; then
    echo "Downloading climate data from CDS..."
    python3 ${scriptsdir}/osm_pyutils/download_clim_cds.py -v lsm oro clake slt sfc lakedl -r ${MRESOL} -o clim_src.grb
    python3 ${scriptsdir}/osm_pyutils/download_clim_cds.py -v month_laih month_lail -r ${MRESOL} -o lai_src.grb --clim
    python3 ${scriptsdir}/osm_pyutils/download_clim_cds.py -v wetlandf urban c3_c4_mask.grb -r ${MRESOL} -o misc_src.grb -m "tmpGrib"
    python3 ${scriptsdir}/osm_pyutils/download_clim_cds.py -v month_alnid month_aluvd -r ${MRESOL} -o albedo_src.grb --clim
    
    # Combine climate files
    cat clim_src.grb lai_src.grb misc_src.grb > clim_native.grb
    
    # Process albedo (combine UV and NIR)
    grib_copy -w paramId=16 albedo_src.grb month_aluvd
    grib_copy -w paramId=17 albedo_src.grb month_alnid
    
    python3 << EOF
import metview as mv
alnid = mv.read("month_alnid")
aluvd = mv.read("month_aluvd")
tmp = aluvd * 0.45976 + 0.54024 * alnid
mv.write("tmp_alb", tmp)
EOF
    grib_set -s paramId=33,edition=2 tmp_alb tmp_alb2
    grib_set -s paramIdECMF=174 tmp_alb2 month_alb_native
    cat month_alb_native >> clim_native.grb
    rm -f tmp_alb tmp_alb2 month_aluvd month_alnid albedo_src.grb clim_src.grb lai_src.grb misc_src.grb month_alb_native
fi

##===============================================
## 2. Regrid climate to target Gaussian grid

echo "Regridding climate to ${TARGET_GRID}..."

# Separate fields by interpolation method
# Soil type, vegetation types need nearest-neighbour
field_nn="slt/tvl/tvh/lsmgrd"
grib_copy -w shortName=cl clim_native.grb _cl_native
grib_copy -w shortName!=cl clim_native.grb _nocl_native
grib_copy -w shortName=${field_nn} _nocl_native nn_native.grb
grib_copy -w shortName!=${field_nn} _nocl_native bil_native.grb

# Regrid using metview to target Gaussian grid
python3 ${scriptsdir}/osm_pyutils/interp_gg.py -t bil -g ${TARGET_GRID} -i bil_native.grb -o clim_bil.grb
python3 ${scriptsdir}/osm_pyutils/interp_gg.py -t bil -g ${TARGET_GRID} -i _cl_native -o clim_cl.grb
python3 ${scriptsdir}/osm_pyutils/interp_gg.py -t nn -g ${TARGET_GRID} -i nn_native.grb -o clim_nn.grb

cat clim_cl.grb clim_nn.grb >> clim_bil.grb
mv clim_bil.grb clim.grb
rm -f _cl_native _nocl_native nn_native.grb bil_native.grb clim_cl.grb clim_nn.grb clim_native.grb

##===============================================
## 3. Extract initial conditions

PPARAM=SWVL1/SWVL2/SWVL3/SWVL4/STL1/STL2/STL3/STL4/SKT/ASN/RSN/SD/TSN/ISTL1/ISTL2/ISTL3/ISTL4/SRC/CI/8.228/9.228/10.228/11.228/12.228/13.228/14.228

i=0
INIDATE=${INIDATES[$i]}
ENDDATE=${ENDDATES[$i]}
site=${sitenames[$i]}

echo "Extracting initial conditions for ${INIDATE}..."

if [[ ${RETRIEVE_WITH} == 'mars' ]]; then
  $MARS_CMD << EOF
  retrieve, stream=${INISTREAM}, class=ea, expver=$INIEXPVER,date=$INIDATE,
  time=${INIHOUR},type=an,levtype=sfc,
  param=$PPARAM,grid=$MRESOL,
  target="init_native.grb"
EOF
elif [[ ${RETRIEVE_WITH} == 'cds' ]]; then
  cat > bar << EOF
import cdsapi
c = cdsapi.Client()
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
    'init_native.grb')
EOF
   python3 < bar
   rm -f bar
fi

# Regrid initial conditions to target Gaussian grid
python3 ${scriptsdir}/osm_pyutils/interp_gg.py -t bil -g ${TARGET_GRID} -i init_native.grb -o init_regrid.grb
grib_set -w shortName=cl -s shortName=ci init_regrid.grb init.grb
rm -f init_native.grb init_regrid.grb

##===============================================
## 4. Create mask (on target Gaussian grid)

echo "Creating land-sea mask..."
grib_copy -w shortName=lsm clim.grb lsm.grb
grib_copy -w shortName=cl clim.grb cl.grb

python3 << EOF
import metview as mv
lsm = mv.read("lsm.grb")
cl = mv.read("cl.grb")
ldland = lsm >= 0.5
ldlake = (cl >= 0.5 * (1.0 - lsm))
ldocean = (lsm < 0.5) * (ldlake == 0)
# Include land and lake points
ldmask = (ldland * 1) + (ldlake * 1) + (ldocean * 0)
ldmask_norm = (ldmask >= 1.0) * 1 + (ldmask < 1) * 0
mv.write("mask.grb", ldmask_norm)
EOF
rm -f lsm.grb cl.grb

##===============================================
## 5. Convert GRIB to NetCDF using our Gaussian converter

echo "Converting to NetCDF (Gaussian grid)..."

# Separate static and time-varying fields
grib_copy -w paramId=66 clim.grb lai_lv.grb
grib_copy -w paramId=67 clim.grb lai_hv.grb
grib_copy -w shortName=al clim.grb albedo.grb
grib_copy -w paramId=200026 clim.grb wetlandf.grb || echo "No wetland field"
grib_copy -w paramId!=66/67/174/200026 clim.grb clim_static.grb

# Convert each component using our Gaussian-aware converter
python3 ${scriptsdir}/osm_pyutils/grb2nc_gaussian.py init.grb init.nc
python3 ${scriptsdir}/osm_pyutils/grb2nc_gaussian.py mask.grb mask.nc
python3 ${scriptsdir}/osm_pyutils/grb2nc_gaussian.py clim_static.grb clim_static.nc
python3 ${scriptsdir}/osm_pyutils/grb2nc_gaussian.py lai_lv.grb lai_lv.nc
python3 ${scriptsdir}/osm_pyutils/grb2nc_gaussian.py lai_hv.grb lai_hv.nc
python3 ${scriptsdir}/osm_pyutils/grb2nc_gaussian.py albedo.grb albedo.nc
[[ -f wetlandf.grb ]] && python3 ${scriptsdir}/osm_pyutils/grb2nc_gaussian.py wetlandf.grb wetlandf.nc

##===============================================
## 6. Merge and finalize NetCDF files

echo "Merging NetCDF files..."

# Merge all climate fields
ncks -A lai_lv.nc clim_static.nc
ncks -A lai_hv.nc clim_static.nc
ncks -A albedo.nc clim_static.nc
[[ -f wetlandf.nc ]] && ncks -A wetlandf.nc clim_static.nc

# Rename mask variable
ncrename -v lsm,Mask mask.nc || ncrename -v LSM,Mask mask.nc || echo "Mask already named"

# Add mask to climate file
ncks -A mask.nc clim_static.nc

# Final output files
mv clim_static.nc ${outdir}/surfclim_${site}_${INIDATE:0:4}-${ENDDATE:0:4}.nc
mv init.nc ${outdir}/surfinit_${site}_${INIDATE:0:4}-${ENDDATE:0:4}.nc

# Cleanup
rm -f *.grb lai_lv.nc lai_hv.nc albedo.nc wetlandf.nc mask.nc

echo ""
echo "=== Gaussian Grid Output Complete ==="
echo "Climate file: ${outdir}/surfclim_${site}_${INIDATE:0:4}-${ENDDATE:0:4}.nc"
echo "Init file: ${outdir}/surfinit_${site}_${INIDATE:0:4}-${ENDDATE:0:4}.nc"
echo "Grid: ${TARGET_GRID} (reduced Gaussian)"
