#!/usr/bin/env python
# (C) Copyright 2020- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# create OSM netcdf forcing from grib input.
# Emanuel Dutra, October 2020
# G Arduini, 2022 adapted for lat/lon runs

from netCDF4 import Dataset,date2num
import time
import numpy as np
import eccodes as ec
import os
import datetime as dt
import re


vinfo={}
vinfo['Wind']={'lname':'Wind speed','units':'m/s'}
vinfo['Wind_E']={'lname':'Wind speed U component','units':'m/s'}
vinfo['Wind_N']={'lname':'Wind speed V component co','units':'m/s'}
vinfo['Tair']={'lname':'Temperature','units':'K'}
vinfo['Qair']={'lname':'Specific humidity','units':'kg/kg'}
vinfo['PSurf']={'lname':'Surface Pressure','units':'Pa'}
vinfo['SWdown']={'lname':'Downward shortwave radiation','units':'W/m2'}
vinfo['LWdown']={'lname':'Downward longwave radiation','units':'W/m2'}
vinfo['Rainf']={'lname':'Rainfall','units':'kg/m2/s'}
vinfo['Snowf']={'lname':'Snowfall','units':'kg/m2/s'}
vinfo['Ctpf']={'lname':'Convective total precipitation fraction','units':'-'}
vinfo['Rhair']={'lname':'Relative humidity','units':'-'}
vinfo['lapseM']={'lname':'Daily lapse rate','units':'-'}

#CONSTANTS:
RLVTT=2.5008E+6
RG=9.80665            # Gravitational Acceleration [m/s2]
RKBOL=1.380658E-23    # Boltzmann constant (k) [J/K]
RNAVO=6.0221367E+23   # Avogadro constant (N_A) [1/mol]
R=RNAVO*RKBOL         # Perfect gas constant (= 8.314511211948600)
RMD=28.9644           # Dry air mass
RMV=18.0153           # Vapour  mass
RD=1000.0*R/RMD       # Dry air cst. (= 287.0596736665907 J/kg/K)
RV=1000.0*R/RMV       # Vapour  cst. (= 461.5249933083879 J/kg/K)
RETV=RV/RD-1.0        # Rv/Rd-1 (= 0.608)
RTT=273.16            # Temperature of water fusion at normal pressure
RTWAT=RTT             # Criteria for temperature with respect to liquid water
RTICE=RTT-23.0        # Criteria for temperature with respect to ice
R2ES=611.21           # Constant in Teten's formula: eq(7.5)
R3LES=17.5020         # Mixing Ratio Over Liquid Water in Teten's formula: eq(7.5)
R4LES=32.190         # Constant in Teten's formula for liquid water: eq(7.5)
R3IES=22.5870        # Mixing Ratio Over Ice in Teten's formula: eq(7.5)
R4IES=-0.70          # Constant in Teten's formula for ice: eq(7.5)
PRHMAX=0.999          # max relative humidity
PRHMIN=0.001          # Min relative humidiy

def extract_date(fin):
  # Input filename
  input_filename = fin

  # Define a regular expression pattern to match the date (assuming YYYYMMDD format)
  date_pattern = r'(\d{8})'

  # Search for the date pattern in the input filename
  match = re.search(date_pattern, input_filename)

  # Check if a match was found
  if match:
      # Extract the matched date
      date = match.group(1)

  return date


def calc_rh_t_td(t,td):
  return np.maximum(PRHMIN,np.minimum(PRHMAX,np.exp(-RLVTT/(RV*t*td)*(t-td))))

def calc_qair(rh,t,p):
  pes=calc_PES(t)
  return rh*pes / ( p*(RETV+1.)-rh*pes*RETV)

def calc_rh_p_q_t(p,q,t):
  pes = calc_PES(t)
  return np.maximum(PRHMIN,np.minimum(PRHMAX,(p*q*(RETV+1.)) / ((1.+RETV*q)*pes) ) )

def calc_PES(Tair):

  # ratio between water and ice : Alpha in eq(7.73) for IFS Document Cy38R1e staturation vapor
  FOEALFA = np.minimum(1.0,( (np.maximum(
                       RTICE,np.minimum(RTWAT,Tair))-RTICE) /(RTWAT-RTICE))**2.0)
  #pressure with Teten's formula, eq. 7.15 for IFS doc CY38R1
  FOEEWM = R2ES *(FOEALFA*np.exp(R3LES*(Tair-RTT)/(Tair-R4LES))+
           (1.0-FOEALFA)*np.exp(R3IES*(Tair-RTT)/(Tair-R4IES)))
  return FOEEWM

def read_grid_LL(fname):
  nc = Dataset(fname,'r')
  lat = nc.variables['lat'][:]
  lon = nc.variables['lon'][:]
  gpoints = np.arange(len(lat)*len(lon)+1)
  nc.close()
  return gpoints,lat,lon

def gen_output_LL(dirout,dateout,cvar,tunits,gpoints,lat,lon):
  nlat=len(lat)
  nlon=len(lon)
  fout=dirout+'/'+cvar+'_'+dateout+'.nc'
  print("Generating",fout)
  try:
    os.remove(fout)
  except:
    pass
  nc = Dataset(fout,'w',format='NETCDF4')

  nc.createDimension('x',len(gpoints))
  nc.createDimension('lat',nlat)
  nc.createDimension('lon',nlon)
  nc.createDimension('time',0)

  ncvar = nc.createVariable('x','i4',('x',))
  ncvar.long_name = 'Grid points 1-npoints'
  ncvar.standard_name = 'Grid points 1-npoints'
  ncvar.units = '-'
  ncvar[:] = gpoints

  ncvar = nc.createVariable('lat','f4',('lat',))
  ncvar.long_name = 'latitude'
  ncvar.standard_name = 'latitude'
  ncvar.units = 'degrees_north'
  ncvar._CoordinateAxisType = "Lat"
  ncvar[:] = lat

  ncvar=nc.createVariable('lon','f4',('lon',))
  ncvar.long_name = 'longitude'
  ncvar.standard_name = 'longitude'
  ncvar.units = 'degrees_east'
  ncvar._CoordinateAxisType = "Lon"
  ncvar[:] = lon

  ncvar=nc.createVariable('time','f8',('time',))
  ncvar.long_name = 'time'
  ncvar.standard_name = 'time'
  ncvar.units = tunits
  ncvar._CoordinateAxisType = "time"
  ncvar[0] = 0


  ncvar=nc.createVariable(cvar,'f4',('time','lat','lon'),zlib=True,complevel=6)
  ncvar.long_name = vinfo[cvar]['lname']
  ncvar.standard_name = vinfo[cvar]['lname']
  ncvar.units = vinfo[cvar]['units']
  ncvar._CoordinateAxisType = "Time Lat Lon"
  ncvar[0,:] = -1
  return nc


def read_grib_LL(FGRBIN,Sname,gpoints,nlat,nlon):
  FGRB=FGRBIN[:]
  if '[shortName]' in FGRB:
    FGRB=FGRBIN.replace('[shortName]',Sname)
  print("Reading:",Sname,FGRB)
  grb = open(FGRB)
  ifld=0
  xdata=[]
  xtime=[]
  while True:
    gid = ec.codes_grib_new_from_file(grb)
    if gid is None: break
    sname = ec.codes_get(gid,'shortName')
    if Sname == sname:
      xdata.append(ec.codes_get_values(gid))
      xdd = ec.codes_get(gid,'validityDate')
      xdT = ec.codes_get(gid,'validityTime')
      xtime.append(dt.datetime.strptime(str(xdd)+str(xdT).zfill(4),'%Y%m%d%H%M'))
      ifld=ifld+1
    ec.codes_release(gid)
  grb.close()
  xtime = np.array(xtime)
  xdata = np.array(xdata)
  print("Read",xdata.shape)
  ntime=xdata.shape[0]
  #xdata[:] = xdata[:,gpoints-1]
  xdata_ll=np.reshape(xdata[:],(ntime,nlat,nlon))
  return xdata_ll,xtime

def process_LL(SURFCLIM,INPUTGRB,tunits,FCML):
  # 1. Get grid info
  gpoints,lat,lon = read_grid_LL(SURFCLIM)
  nlat=len(lat)
  nlon=len(lon)

  dirout=os.path.dirname(INPUTGRB)
  dateout=extract_date(INPUTGRB)
  CVAR='Wind_E'
  if CVAR in INPUTGRB:
    xdata,xtime = read_grib_LL(INPUTGRB,'u',gpoints,nlat,nlon)
    nc=gen_output_LL(dirout,dateout,CVAR,tunits,gpoints,lat,lon)
    nc.variables[CVAR][:]=xdata
    nc.variables['time'][:]=date2num(xtime,tunits)
    nc.close()

  CVAR='Wind_N'
  if CVAR in INPUTGRB:
    xdata,xtime = read_grib_LL(INPUTGRB,'v',gpoints,nlat,nlon)
    nc=gen_output_LL(dirout,dateout,CVAR,tunits,gpoints,lat,lon)
    nc.variables[CVAR][:]=xdata
    nc.variables['time'][:]=date2num(xtime,tunits)
    nc.close()

  CVAR='PSurf'
  if CVAR in INPUTGRB:
    xdata,xtime = read_grib_LL(INPUTGRB,'lnsp',gpoints,nlat,nlon)
    nc=gen_output_LL(dirout,dateout,CVAR,tunits,gpoints,lat,lon)
    nc.variables[CVAR][:]=np.exp(xdata)
    nc.variables['time'][:]=date2num(xtime,tunits)
    nc.close()

  CVAR='Tair'
  if CVAR in INPUTGRB:
    xdata,xtime = read_grib_LL(INPUTGRB,'t',gpoints,nlat,nlon)
    nc=gen_output_LL(dirout,dateout,CVAR,tunits,gpoints,lat,lon)
    nc.variables[CVAR][:]=xdata
    nc.variables['time'][:]=date2num(xtime,tunits)
    nc.close()

  CVAR='Qair'
  if CVAR in INPUTGRB:
    xdata,xtime = read_grib_LL(INPUTGRB,'q',gpoints,nlat,nlon)
    nc=gen_output_LL(dirout,dateout,CVAR,tunits,gpoints,lat,lon)
    nc.variables[CVAR][:]=xdata
    nc.variables['time'][:]=date2num(xtime,tunits)
    nc.close()



  CVAR='SWdown'
  if CVAR in INPUTGRB:
    xdata,xtime = read_grib_LL(INPUTGRB,'ssrd',gpoints,nlat,nlon)
    nc=gen_output_LL(dirout,dateout,CVAR,tunits,gpoints,lat,lon)
    nc.variables[CVAR][:]=np.maximum(0,xdata)
    nc.variables['time'][:]=date2num(xtime,tunits)
    nc.close()

  CVAR='LWdown'
  if CVAR in INPUTGRB:
    xdata,xtime = read_grib_LL(INPUTGRB,'strd',gpoints,nlat,nlon)
    nc=gen_output_LL(dirout,dateout,CVAR,tunits,gpoints,lat,lon)
    nc.variables[CVAR][:]=np.maximum(0,xdata)
    nc.variables['time'][:]=date2num(xtime,tunits)
    nc.close()

  CVAR='Rainf'
  if CVAR in INPUTGRB:
    rainf,xtime = read_grib_LL(INPUTGRB,'cp',gpoints,nlat,nlon)
    #rainf = tp - sf

    nc=gen_output_LL(dirout,dateout,CVAR,tunits,gpoints,lat,lon)
    nc.variables[CVAR][:]=np.maximum(0,rainf)
    nc.variables['time'][:]=date2num(xtime,tunits)
    nc.close()

  CVAR='Snowf'
  if CVAR in INPUTGRB:
    sf,xtime = read_grib_LL(INPUTGRB,'sf',gpoints,nlat,nlon)
    nc=gen_output_LL(dirout,dateout,CVAR,tunits,gpoints,lat,lon)
    nc.variables[CVAR][:]=np.maximum(0,sf)
    nc.variables['time'][:]=date2num(xtime,tunits)
    nc.close()

  CVAR='Ctpf'
  if CVAR in INPUTGRB:
    xdata,xtime = read_grib_LL(INPUTGRB,'cp',gpoints,nlat,nlon)
    nc=gen_output_LL(dirout,dateout,CVAR,tunits,gpoints,lat,lon)
    nc.variables[CVAR][:]=xdata
    nc.variables['time'][:]=date2num(xtime,tunits)
    nc.close()

  CVAR='lapseM'
  if CVAR in INPUTGRB:
    xdata,xtime = read_grib_LL(INPUTGRB,'lnsp',gpoints,nlat,nlon)
    nc=gen_output_LL(dirout,dateout,CVAR,tunits,gpoints,lat,lon)
    nc.variables[CVAR][:]=xdata
    nc.variables['time'][:]=date2num(xtime,tunits)
    nc.close()


  return

