#!/bin/bash 
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set -eu
#
##################################################################
#
# README - Script to create forcing from oper/era for OSM
#
# To edit start time 		: set year
# To edit forcing frequency : t1, while nt, @nn
# To edit forcing file		: forcinggrb
#
##################################################################


# Define timesteps for use in forcing
#iniDates=($(cat iniDate_list.txt))
#endDates=($(cat endDate_list.txt))



# location forcing files
forcingdir=$1

# location on cca of climate fields
XDATA_DIR=$2

# site index name
site=$3

# forcingdate
fDate=$4

# indate
iniDate=$5

#oudate
endDate=$6

siteend=$7

lat=$8
lon=$9

# Prepare some python scripts
cat > compute_time.py << EOF
from pylab import *
import calendar
import datetime as dt
import sys
iniDate = str(sys.argv[1])
endDate = str(sys.argv[2])

date_format = "%Y%m%d"
a = dt.datetime.strptime(iniDate, date_format)
b = dt.datetime.strptime(endDate, date_format)
dyear = b - a 
dyear = dyear.days + 1

dt=3600
t1 = arange(0, 86400*dyear+dt, dt)
nt = 0
while nt < (len(t1)-1):
    print('%s,'%t1[nt])
    nt += 1
if (nt == (len(t1)-1)):
    print('%s;'%t1[nt])
EOF

cat > leap_year.py << EOF
import calendar
import sys
import datetime as dt
iniDate = str(sys.argv[1])
endDate = str(sys.argv[2])

date_format = "%Y%m%d"
a = dt.datetime.strptime(iniDate, date_format)
b = dt.datetime.strptime(endDate, date_format)
dyear = b - a 
dyear = dyear.days + 1

print(dyear)
EOF

#******
# START
#******

# clean the directory
rm -f time.asc temp toto
rm -f ndays.txt

#site=${sitelist[$i]}

cat << EOF >time.asc
time=
EOF

#source ./sites_details.txt

echo "Processing" $site
forcingcdf=${forcingdir}/${site}_${fDate}.nc
forcingasc=${forcingdir}/${site}_${fDate}.asc

rm -f $forcingcdf
rm -f $forcingasc

python3 leap_year.py $iniDate $endDate > ndays.txt
dyear=$(cat ndays.txt)

tsteps=$((86400 * dyear / 3600 + 1))

echo $tsteps

nowdate=$(date)

iniYear=${iniDate:0:4}
iniMonth=${iniDate:4:2}
iniDay=${iniDate:6:2}

# Generate forcing.cdf
cat << EOF > $forcingasc
netcdf surfclim_${site} {
dimensions:
        lat = 1 ;
        lon = 1 ;
        time = UNLIMITED ; // (${tsteps} currently)
variables:
    float time(time) ;
        time:units = "seconds since ${iniYear}-${iniMonth}-${iniDay} 00:00:00.0" ;
    double lat(lat) ;
        lat:units = "degrees_north" ;
        lat:long_name = "latitude" ;
    double lon(lon) ;
        lon:units = "degrees_east" ;
        lon:long_name = "longitude" ;
    float LWdown(time, lat, lon) ;
        LWdown:ALMA_name = "LWdown" ;
        LWdown:CMIP_name = "rlds" ;
        LWdown:long_name = "Surface downward longwave radiation" ;
        LWdown:units = "W/m2" ;
    float PSurf(time, lat, lon) ;
        PSurf:ALMA_name = "Psurf" ;
        PSurf:CMIP_name = "ps" ;
        PSurf:long_name = "Surface pressure" ;
        PSurf:units = "Pa" ;
    float Qair(time, lat, lon) ;
        Qair:ALMA_name = "Qair" ;
        Qair:CMIP_name = "hus" ;
        Qair:long_name = "Near-surface specific humidity" ;
        Qair:units = "kg/kg" ;
    float Rainf(time, lat, lon) ;
        Rainf:ALMA_name = "Rainf" ;
        Rainf:CMIP_name = "prra" ;
        Rainf:long_name = "Rainfall rate" ;
        Rainf:units = "kg/m2/s" ;
    float Ctpf(time, lat, lon) ;
        Ctpf:ALMA_name = "Ctpf" ;
        Ctpf:CMIP_name = "prra" ;
        Ctpf:long_name = "Convective total precipitation fraction (convective + stratiform)" ;
        Ctpf:units = "-" ;
    float Snowf(time, lat, lon) ;
        Snowf:ALMA_name = "Snowf" ;
        Snowf:CMIP_name = "prsn" ;
        Snowf:long_name = "Snowfall rate" ;
        Snowf:units = "kg/m2/s" ;
    float SWdown(time, lat, lon) ;
        SWdown:ALMA_name = "SWdown" ;
        SWdown:CMIP_name = "rsds" ;
        SWdown:long_name = "Surface downward shortwave radiation" ;
        SWdown:units = "W/m2" ;
    float Tair(time, lat, lon) ;
        Tair:ALMA_name = "Tair" ;
        Tair:CMIP_name = "ta" ;
        Tair:long_name = "Near-surface air temperature" ;
        Tair:units = "K" ;
    float Wind_E(time, lat, lon) ;
        Wind_E:ALMA_name = "Wind_E" ;
        Wind_E:CMIP_name = "wind_e" ;
        Wind_E:long_name = "Near-surface wind velocity eastward" ;
        Wind_E:units = "m/s" ;
    float Wind_N(time, lat, lon) ;
        Wind_N:ALMA_name = "Wind_N" ;
        Wind_N:CMIP_name = "wind_n" ;
        Wind_N:long_name = "Near-surface wind velocity northward" ;
        Wind_N:units = "m/s" ;

// global attributes:
                :Production_time = "${nowdate}" ;
                :Production_source = "ECMWF" ;
                :dataset_name = "ECMWF OSM" ;
                :version = "1.0" ;
                :Contact = "gabriele.arduini@ecmwf.int" ;

data:

 lat = $lat ;

 lon = $lon ;

EOF


# Create time
python3 compute_time.py $iniDate $endDate >> time.asc

# Remove last time value
echo $fDate
echo $fDate
echo $siteend
echo $siteend
testDate=$fDate
testDate=$((testDate + 33))

if [[ $testDate -lt $siteend ]]; then
  echo "hello"
  sed -i '$d' time.asc
  sed -i '$ s/,/;/' time.asc
fi

# Get surfclim information
cat time.asc >> $forcingasc

cat << EOF >> $forcingasc



EOF
# Variables required by OSM:
varlist=("PSurf" "Tair" "Qair" "Wind_E" "Wind_N" "SWdown" "LWdown" "Rainf" "Snowf" "Ctpf")

n=0
while [[ $n -lt 10 ]]; do
  var=${varlist[$n]}
  echo "Attaching : $var"
  forcinggrb=${forcingdir}/${var}_${fDate}.grb

  # Extract only land points for forcing:
  lsmgrib=${forcingdir}/lsm
  if [[ ! -e $lsmgrib ]]; then
    if [[ $XDATA_DIR == *rdx* ]]; then
      cp -f $XDATA_DIR/lsmoro $forcingdir
    else
      wget $XDATA_DIR/lsmoro -P ${forcingdir}/
    fi
    grib_copy -w shortName=lsm ${forcingdir}/lsmoro $lsmgrib
  fi

  # Extract grid point information
  grib_ls -p shortName,date,step -l $lat,$lon,1,$lsmgrib $forcinggrb > temp

  echo $dyear

  if [[ $testDate -lt $siteend ]]; then
    # Regular month
    nn=$((24 * dyear + 2))
  else
    # Last month
    nn=$((24 * dyear + 3))
  fi

  head -$nn temp > toto
  tail toto

  if [[ $var == "PSurf" ]]; then
    awk '$4>=0. {print exp($4)","}' toto | sed '$ s/\,/;/g' | sed '1 s/1\,/'$var'\=/g' >> $forcingasc
  elif [[ $var != "Wind_E" && $var != "Wind_N" ]]; then
    awk '$4>=0. {print $4","}' toto | sed '$ s/\,/;/g' | sed '1 s/value\,/'$var'\=/g' >> $forcingasc
  else
    tail -n +2 toto | awk '{print $4","}' | sed '$ s/\,/;/g' | sed '1 s/value\,/'$var'\=/g' >> $forcingasc
  fi
  rm -rf toto titi

  cat << EOF >> $forcingasc

EOF

  n=$(( n+1))
done

cat << EOF >> $forcingasc
}
EOF

ncgen -o $forcingcdf $forcingasc

