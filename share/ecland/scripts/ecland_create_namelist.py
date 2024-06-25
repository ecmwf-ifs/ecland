#!/usr/bin/env python3

# A python script to create the model namelist based on some entry file
# Extract information required:
    # NSTART
    # NSTOP
    # TSTEP model (default same as forcing)
    # NLAT
    # NLON
    # NDFORC
    # NDIMCDF
    # ZPHISTA (if insitu)
    # ZDTFORC
    # NDIMFORC
# (C) Copyright 2023- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.



from netCDF4 import Dataset,num2date
import numpy as np
import argparse
from datetime import datetime
import re


def read_args():
    parser = argparse.ArgumentParser(description="Script to create a namelist file from a site clim/forcing  ")
    parser.add_argument("-g", "--group", type=str, help="Name of the site", required=True)
    parser.add_argument("-n", "--namelist", type=str, help="Path to the namelist", required=True)
    parser.add_argument("-c", "--namelist_cmf", type=str, help="Path to the cama-flood namelist", required=False)
    parser.add_argument("-s", "--site", type=str, help="Name of the site", required=True)
    parser.add_argument("-t", "--ftype", type=str, help="type of forcing, insitu or era5/oper", required=True)
    parser.add_argument("-d", "--datadir",  type=str, help="datadir", required=True)
    parser.add_argument("-w", "--workpath",  type=str, help="path where namelist will be stored", required=True)

    return parser.parse_args()

# From the ecland namelist, extract the value of LECMF1WAY to check if running
# with Cama active. Return it in upper case to avoid too many cases
def extract_cmf_value(namelist_ecland):
    match = re.search(r'LECMF1WAY=(\.\w+\.)', namelist_ecland)
    if match:
        cmf_value = match.group(1)
        return cmf_value.upper()
    else:
        return None

def generate_cmf_namelist():
    # Read namelist base file and populate the entries specific to the
    # experiment, if any
    with open(namelist_cmf_base, 'r') as file:
      file_content = file.read()
      file_content = file_content.format(yy=yy,
                                         mm=mm,
                                         dd=dd,
                                         yye=yye,
                                         mme=mme,
                                         dde=dde)

    return file_content


def generate_ecland_namelist():
    # Read namelist base file and populate the entries specific to the experiment.
    with open(namelist_base, 'r') as file:
      file_content = file.read()
      file_content = file_content.format(nstart=nstart,
                                         nstop=nstop,
                                         nfreq_post=nfreq_post,
                                         tstep=tstep,
                                         nlat=nlat,
                                         nlon=nlon,
                                         nforcing=nforcing,
                                         iniDate=iniDate,
                                         iniSec=iniSec,
                                         ndimcdf=ndimcdf,
                                         zphista=zphista,
                                         zuv=zuv,
                                         forcing_step=forcing_step)

    return file_content

if __name__ == "__main__":

    args=read_args()
    namelist_base=args.namelist
    namelist_cmf_base=args.namelist_cmf
    site=args.site
    forcing_type=args.ftype
    datadir=args.datadir
    group=args.group
    wpath=args.workpath

# Get the filenames, dates and paths from site and datadir
    work_folder=forcing_type+"_"+site
    parts = site.split("_")
    iniY=parts[1].split("-")[0]
    endD=parts[1].split("-")[1]

    # Read climate file
    climFileName=f'{datadir}/clim/{group}/surfclim_{site}.nc'
    climFile=Dataset(climFileName,format='NETCDF4')

    # Read forcing file
    forcingFileName=f'{datadir}/forcing/{group}/met_{forcing_type}HT_{site}.nc'
    forcingFile=Dataset(forcingFileName,format='NETCDF4')
    units = forcingFile.variables['time'].units

    # Namelist info
    if forcing_type=='2D':
      iniDate=datetime.strptime(units, 'seconds since %Y-%m-%d %H:%M:%S').strftime('%Y%m%d')
      yy=iniDate[0:4]
      mm=iniDate[4:6]
      dd=iniDate[6:8]
      time_variable = forcingFile.variables['time']
      # Get the last time value
      last_time_data = time_variable[-1]
      last_time = num2date(last_time_data, units)
      # Convert the last time to YYYYMMDD format (assuming it's in a standard datetime format)
      last_time_str = last_time.strftime('%Y%m%d')
      yye=last_time_str[0:4]
      mme=last_time_str[4:6]
      dde=last_time_str[6:8]
      iniSec=0
    else:
      time_variable = num2date(forcingFile.variables['time'][0], units)
      iniDate=time_variable.strftime('%Y%m%d')
      iniSec=int((time_variable - time_variable.replace(hour=0, minute=0, second=0, microsecond=0)).total_seconds())

    nstart=0
    tstep=1800.0
    forcing_step=(forcingFile.variables['time'][1].data - forcingFile.variables['time'][0].data)
    #if forcing_type=='insitu':
    #  forcing_step=(forcingFile.variables['time'][1].data - forcingFile.variables['time'][0].data)*3600.0
    nforcing=len(forcingFile.variables['time'])
    nstop=int(nforcing*(forcing_step/tstep)-2)
    nlat=len(forcingFile.dimensions['lat'])
    nlon=len(forcingFile.dimensions['lon'])
    ndimcdf=2
    if forcing_type=='insitu':
      zphista=climFile.variables['zphista'][0].data[0]
      zuv=climFile.variables['zuv'][0].data[0]
    else:
      zphista=10.0
      zuv=10.0
    nfreq_post=int(np.maximum(1,3600.0/tstep))

    # Generate namelist:
    namelist=generate_ecland_namelist()
    namelistFileName='namelist'
    # Write the content to the file
    with open(f'{wpath}/{namelistFileName}_{site}', "w") as file:
        file.write(namelist)


    if extract_cmf_value(namelist) in ['TRUE','.T.','.TRUE.']:
      namelist_cmf=generate_cmf_namelist()
      #namelistFileName='input_cmf.nam'
      namelistFileName='namelist_cmf'
      # Write the content to the file
      with open(f'{wpath}/{namelistFileName}_{site}', "w") as file:
          file.write(namelist_cmf)

