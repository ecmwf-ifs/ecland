#!/usr/bin/env python3
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import metview as mv
import multiprocessing
from multiprocessing import Pool
import numpy as np
import argparse
import datetime
from datetime import datetime as dt
import subprocess
import os
from create_sites import create_forcing, vattrib
from functools import partial
from extract_process_cds import gridpoint_info


vtable=dict()
vtable['lnsp']=vattrib('PSurf','Pa', 'Surface pressure')
vtable['q']=vattrib('Qair','kg/kg', 'Near-surface specific humidity')
vtable['t']=vattrib('Tair','K', 'Near-surface air temperature')
vtable['u']=vattrib('Wind_E','m/s', 'Near-surface wind velocity eastward')
vtable['v']=vattrib('Wind_N','m/s', 'Near-surface wind velocity northward')
vtable['cp']=vattrib('Ctpf','-', 'Fraction of convective precipitation (convective + stratiform)')
vtable['tp']=vattrib('Rainf','kg/m2/s', 'Total precipitation rate')
vtable['sf']=vattrib('Snowf','kg/m2/s', 'Snowfall rate')
vtable['strd']=vattrib('LWdown','W/m2', 'Surface downward longwave radiation')
vtable['ssrd']=vattrib('SWdown','W/m2', 'Surface downward shortwave radiation')


def read_args():
    parser = argparse.ArgumentParser(description='preprocess monthly forcing to concatenate them ')

# Add a --help argument with a custom help message
    parser.add_argument('-v', '--vars', nargs='+', required=True, help='list of variables')
    parser.add_argument('-w', '--workDir',required=True, help='forcing directory')
    parser.add_argument('-d', '--date',required=True, help='current date')
    parser.add_argument('-e', '--endDate',required=True, help='site end date')
    parser.add_argument('-o', '--output',required=True, help='output forcing file directory')
    parser.add_argument('-l', '--lat', type=float, required=True, help='lat of point')
    parser.add_argument('-L', '--lon', type=float, required=True, help='lon of point')
    parser.add_argument('-s', '--sdir', type=str, required=True, help='scriptsdir')

    args = parser.parse_args()
    return args

def crop_data(fin, lat, lon):
    area=[lat-1.0, lon-1.0, lat+1.0, lon+1.0] # S,W,N,E
    fcrop=mv.read(data=fin,area=area)
    return fcrop

def process_forcing(forcingDir, endDate, lat, lon, fdate, testDate, vars, forcingDirOut, scriptsdir):
    print(f"var: {vars}, date: {fdate}")
    forcingFile=f"{forcingDir}/{vars}_{fdate}.grb"
    fin=mv.read(f"{forcingFile}")
    # Read lsm and use it to mask forcing and get nearest land gridpoint
    lsm=mv.read(f"{forcingDir}/lsm_{fdate}")
    # check lsm and forcing on same grid and interpolate otherwise
    TgridType, TNLat = gridpoint_info(lsm) # Target grid  info
    FgridType, FNLat = gridpoint_info(fin) # Forcing grid info
    if TNLat+TgridType != FNLat+FgridType:
        print(f"forcing and lsm grid points do not match, forcing will be interpolated")
        print(f"forcing grid: {FgridType} {FNLat}, lsm grid: {TgridType} {TNLat}")
        fin=mv.read(data=fin, grid=TgridType+TNLat)

    fin=mv.bitmap(fin, mv.bitmap(lsm>=0.5,0) )
    # Crop around the area of interest
    forcing=crop_data( fin, lat, lon)
    # Check for total rainfall!
    if vars == 'Rainf':
       varname=mv.grib_get_string(forcing, 'shortName')[0]
       if varname != 'tp':
          forcing=mv.grib_set(forcing, ['shortName', 'tp'])
    if (testDate < dt.strptime(endDate,"%Y%m%d")):
      forcing = forcing[:-1]

    outFile=f"{forcingDirOut}/{vars}_{fdate}.nc"
    create_forcing(forcing, lat, lon, outFile)


def main():
    args=read_args()

    vars=args.vars
    forcingDir=args.workDir
    date=args.date
    endDate=args.endDate
    lat=args.lat
    lon=args.lon
    outDir=args.output
    scriptsdir=args.sdir

    fdate=date[0:6]+"00"

    # compute last date of the month
    date_obj = dt.strptime(date, '%Y%m%d')
    if date_obj.month == 12: # if end of the year
        testDate = (date_obj.replace(day=1, month=1, year=date_obj.year+1) - datetime.timedelta(days=1))
    else:
        testDate = (date_obj.replace(day=1, month=date_obj.month+1) - datetime.timedelta(days=1))
    for var in vars:
        process_forcing(forcingDir, endDate, lat, lon, fdate, testDate, var, outDir, scriptsdir)

# Multiprocessing does not work yet on MacOS, still under investigation...
    ## Get the number of CPU cores
    #num_proc = np.minimum(multiprocessing.cpu_count(), len(vars))
    ## Create a pool of worker processes
    #pool = multiprocessing.Pool(processes=num_proc)
    ## List to keep track of the async results
    #async_results = []
    #for var in vars:
    #    async_result = pool.apply_async(process_forcing, (forcingDir, endDate, lat, lon, fdate, testDate, var, outDir, scriptsdir))
    #    async_results.append(async_result)
    ## Wait for all processes to complete
    #for async_result in async_results:
    #    async_result.get()
    #
    #pool.close()
    #pool.join()


if __name__ == '__main__':
    main()
