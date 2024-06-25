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

set -eux

gridTF(){

NLAT=$(grib_ls -x -p N -w count=1 $1 | head -n3 | tail -n1 | awk '{print $1}' )
isO=$(grib_ls -x -p isOctahedral -w count=1 $1 | head -n3 | tail -n1 | awk '{print $1}')
GT=N
if [[ $isO == 1 ]] ; then GT=O ; fi
echo ${GT}${NLAT}
}

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
##===================================
## Change default via command line options
##===================================


while getopts e:i:d:f:c:s: option
do
  case $option in
    e) EXTDATE=$OPTARG;;
    i) EXPVER=$OPTARG;;
    d) DATADIR=$OPTARG;;
    f) FREQ=$OPTARG;;
    c) CLASS=$OPTARG;;
    s) STREAM=$OPTARG;;
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
echo ''

cd $DATADIR

EXPVER0=$EXPVER
##=======================================
## Resolution related 
##========================================

if [[ $GRID == XXX ]]; then
  MGRID=""
else
  MGRID="grid=${GRID},"
fi

##=======================================
## PATHs
##========================================


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
  if [[ $FREQ != @(1|3) ]]; then
      echo "Attention, for ERA5 forcing only hourly or 3-hourly analysis are available!"
      echo "Check your FREQ variable in prepIFS"
      exit -1
   fi

if [[ $CLASS = "ea" ]]; then
  TIME="06/18"  # time to extract 

  FORMULA_ssrd="mv.max(0,(ssrd))/(${FREQ}*3600)"
  FORMULA_strd="(strd)/(${FREQ}*3600)"
  FORMULA_lsp="1000.*mv.max(0,(lsp))/(${FREQ}*3600)"
  FORMULA_cp="1000.*mv.max(0,(cp))/(${FREQ}*3600)"
  FORMULA_sf="1000.*mv.max(0,(sf))/(${FREQ}*3600)"

  STEPS_VAR=$( echo $(seq -w $FREQ  $FREQ 12 ) | sed "s/ /\//g")  # steps to retrieve ML
  STEPS_FLX=$STEPS_VAR  # steps to retrieve SFC
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
else
    TIME="00/12"  # time to extract 


    FORMULA_ssrd="max(0,(ssrd2-ssrd1))/(${FREQ}*3600)"
    FORMULA_strd="(strd2-strd1)/(${FREQ}*3600)"
    FORMULA_lsp="1000.*max(0,(lsp2-lsp1))/(${FREQ}*3600)"
    FORMULA_cp="1000.*max(0,(cp2-cp1))/(${FREQ}*3600)"
    FORMULA_sf="1000.*max(0,(sf2-sf1))/(${FREQ}*3600)"

    STEPS_VAR=$( echo $(seq -w $FREQ  $FREQ 12 ) | sed "s/ /\//g")  # steps to retrieve ML
    STEPS_FLX=$( echo $(seq -w 0  $FREQ 12 ) | sed "s/ /\//g")  # steps to retrieve SFC
    STEPS_FLXn=$( echo $(seq -w $FREQ $FREQ 12 ) | sed "s/ /\//g")  # steps to retrieve SFC   
    STEPS_FLXp=$( echo $(seq -w 0 $FREQ 11 ) | sed "s/ /\//g")  # steps to retrieve SFC   

    ## special case for 1st 6 hours, we need the previous day forecast     
    STEPS_VAR0=$( echo $(seq -w 12 $FREQ 12 ) | sed "s/ /\//g")  # ML
    STEPS_FLX0=$( echo "$(( 12 -$FREQ)) 12" | sed "s/ /\//g") # SFC
    STEPS_FLX0n=$( echo "12" | sed "s/ /\//g") # SFC
    STEPS_FLX0p=$( echo "$(( 12 -$FREQ))" | sed "s/ /\//g") # SFC

    DATE0=$( add_days_to_date ${DATE1} -1)
    TIME0="12"
    CLASS0=$CLASS
fi


# Assume 137 levels for most of experiment types: 
FORCINGLEV0=137
FORCINGLEV=$(( $FORCINGLEV0 - $FORCINGLEV )) 
   
FSAVE=""  # list of variables to save 

##====================================
## Extract SFC
##====================================
if [[ $LEXTRACT_SFC = true ]]; then
echo "Extracting SFC" 
rm -f bar
touch bar 
if [[ $DATE0 != NONE ]]; then  ## special data0
cat > bar << EOF
retrieve,
stream=$STREAM,
class=$CLASS0,
expver=$EXPVER0,
date=$DATE0,
time=$TIME0,
step=$STEPS_FLX0,
type=fc,
$MGRID
levtype=sfc,
param=SSRD/STRD/CP/LSP/SF,
target="tmp0.grb"
EOF
$MARS_CMD < bar  #

rm -f bar
touch bar
fi

rm -f tmp1.grb
for pp in SSRD STRD CP LSP SF; do
rm -f tmp1_${pp}.grb
cat >> bar << EOF
retrieve, 
stream=$STREAM,
class=$CLASS,
expver=$EXPVER,
date=$DATE1/TO/$DATE2,
time=${TIME}, 
step=$STEPS_FLX,
type=fc,
levtype=sfc,
$MGRID
param=${pp},
target="tmp1_${pp}"
EOF
$MARS_CMD < bar  #

cat tmp1_${pp} >> tmp1.grb

rm -f bar tmp1_${pp}
touch bar
done



# ERA5 data are relatively easy as fluxes are already deaccumulated. 
# Therefore only units conversion is needed
rm -f bar
for pp in strd ssrd lsp cp sf
do
formula_var="FORMULA_${pp}" 
typeset -n formula="${formula_var}"
cat >> bar << EOF
import metview as mv
data=mv.read("tmp0.grb")
_class="${CLASS}"
if _class != 'od':
  ${pp}=data.select(shortName="${pp}")
else:
  steps_p="${STEPS_FLX0p}".split("/")
  steps_n="${STEPS_FLX0n}".split("/")
  ${pp}1=data.select(shortName="${pp}",step=steps_p)
  ${pp}2=data.select(shortName="${pp}",step=steps_n)

${pp}=${formula}
fname={"ssrd":"SWdown",
       "strd":"LWdown",
       "sf":"Snowf",
       "lsp":"lsp",
       "cp":"cp",
      }
if "${pp}" in ["strd","ssrd","sf"]:
  mv.write(fname["${pp}"]+"0.grb",${pp})
else:
  mv.write(fname["${pp}"]+"0.tmp",${pp})
EOF
done
python3 < bar

rm -f bar

cat >> bar << EOF
import metview as mv
CPf=mv.read("cp0.tmp")
LSPf=mv.read("lsp0.tmp")
SFf=mv.read("Snowf0.grb")
tp=mv.max(0.0, CPf+LSPf-SFf)
mv.write("Rainf0.grb",tp)

ctpf=mv.max(0.0, mv.min(1.0, (CPf>${EPS})*((CPf+$EPS)/($EPS+CPf+LSPf))+0*(CPf<=$EPS)))
mv.write("Ctpf0.grb",ctpf)
EOF
python3 < bar

for pp in strd ssrd lsp cp sf
do
rm -f bar
formula_var="FORMULA_${pp}" 
typeset -n formula="${formula_var}"
cat >> bar << EOF
import metview as mv
fname={"ssrd":"SWdown",
       "strd":"LWdown",
       "sf":"Snowf",
       "lsp":"lsp",
       "cp":"cp",
      }
data=mv.read("tmp1.grb")
_class="${CLASS}"
if _class != 'od':
  ${pp}=data.select(shortName="${pp}")
else:
  steps_p="${STEPS_FLXp}".split("/")
  steps_n="${STEPS_FLXn}".split("/")
  ${pp}1=data.select(shortName="${pp}",step=steps_p)
  ${pp}2=data.select(shortName="${pp}",step=steps_n)

${pp}=${formula}

if "${pp}" in ["strd","ssrd","sf"]:
  mv.write(fname["${pp}"]+"1.grb",${pp})
else:
  mv.write(fname["${pp}"]+"1.tmp",${pp})
EOF
  python3 < bar
  rm -f bar
done

rm -f Rainf1_*.grb Ctpf1_*.grb
cat >> bar << EOF
import metview as mv
CPf=mv.read("cp1.tmp")
LSPf=mv.read("lsp1.tmp")
SFf=mv.read("Snowf1.grb")
tp=mv.max(0.0, CPf+LSPf-SFf)
ctpf=mv.max(0.0, mv.min(1.0, (CPf>${EPS})*((CPf+$EPS)/($EPS+CPf+LSPf))+0*(CPf<=$EPS)))
mv.write("Rainf1.grb",tp)
mv.write("Ctpf1.grb",ctpf)
EOF
python3 < bar


FSAVE="${FSAVE} Rainf Snowf LWdown SWdown Ctpf"
  for ff in $FSAVE 
  do
    if [[ $FORCINGCLASS == 'ea' ]]; then
      cat ${ff}0.grb ${ff}1.grb > ${ff}.grb
      ncount=$(grib_ls -p count ${ff}.grb | tail -n1 | awk '{print $1}')
      tmp=$(seq $((ncount-5)) $ncount)
      message_remove=$(echo $tmp | sed 's/ /\//g')
      grib_copy -w count!=${message_remove} ${ff}.grb tmp.grb
      mv -f tmp.grb ${ff}.grb
    else
      cat ${ff}0.grb ${ff}1.grb > ${ff}.grb
    fi
    rm -f ${ff}0.grb ${ff}1.grb
  done


# clean 
rm -f tmp0.grb tmp1.grb 
rm -f forcing_sfc_aux.grb bar
fi # LEXTRACT_SFC


##====================================
## Extract model level data 
##===================================
if [[ $LEXTRACT_ML = true ]] ; then

MGRID="grid=$( gridTF LWdown.grb ),"


echo "Extracting ML"
rm -f bar 
touch bar
if [[ $DATE0 != NONE ]]; then  ## special data0
cat >> bar << EOF
retrieve, 
stream=$STREAM,
class=$CLASS0,
expver=$EXPVER0,
date=$DATE0,
time=${TIME0}, 
step=$STEPS_VAR0,
type=fc,
levtype=ml,
levelist=$FORCINGLEV,
$MGRID
param=u/v/t/q,
target="forcing_ml_aux.grb"
EOF
if [[ $LEXTRACT_LNSP = true ]]; then
cat >> bar << EOF 
retrieve,levelist=1,param=lnsp
EOF
fi 

fi
cat >> bar << EOF
retrieve, 
stream=$STREAM,
class=$CLASS,
expver=$EXPVER,
date=$DATE1/TO/$DATE2,
time=${TIME}, 
step=$STEPS_VAR,
type=fc,
levtype=ml,
levelist=$FORCINGLEV,
$MGRID
param=u/v/t/q,
target="forcing_ml_aux.grb"
EOF
if [[ $LEXTRACT_LNSP = true ]]; then
cat >> bar << EOF 
retrieve,levelist=1,param=lnsp
EOF
fi 
$MARS_CMD < bar
if [[ $LEXTRACT_LNSP = true ]]; then
grib_copy -w shortName=lnsp forcing_ml_aux.grb PSurf.grb
fi 
grib_copy -w shortName=t forcing_ml_aux.grb Tair.grb
grib_copy -w shortName=q forcing_ml_aux.grb  Qair.grb
grib_copy -w shortName=u forcing_ml_aux.grb  Wind_E.grb
grib_copy -w shortName=v forcing_ml_aux.grb  Wind_N.grb

FSAVE="${FSAVE} Wind_E Wind_N Tair Qair"
if [[ $LEXTRACT_LNSP = true ]]; then
    FSAVE="${FSAVE} PSurf"
fi

rm -f forcing_ml_aux.grb bar bar1 

fi # LEXTRACT_MET


cat >filter.rules << EOF
transient hours  = dataTime / 100 + stepRange ;

if ( (dataDate == $DATE0 && hours >=24 )        || 
     (dataDate >= $DATE1 && dataDate < $DATE2 ) || 
     (dataDate == $DATE2 && hours <=24 )          )
  { write; }
EOF

for ff in $FSAVE
do 
  grib_filter -o tmp.grb filter.rules ${ff}.grb
  mv tmp.grb ${ff}.grb
done
rm -f filter.rules


### save
#*for ff in $FSAVE
#*do
#*  mv ${ff}.grb $DATADIR/${ff}.grb 
#*done


###=================================
### Final clean
