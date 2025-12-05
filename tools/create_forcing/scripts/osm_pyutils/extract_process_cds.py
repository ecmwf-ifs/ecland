#!/usr/bin/env python3
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import cdsapi
import metview as mv
import numpy as np
import argparse
import os
from multiprocessing import Pool
from datetime import datetime as dt
from datetime import timedelta

def download_from_cds(DATE, STREAM, TIME, STEPS_VAR, dataName, type_data, dataFormat, grbVars, grbLevType, grbLev,outFile):
    c = cdsapi.Client()
    request = {
        'date'    : DATE,
        'time'    : TIME,
        'param'   : grbVars,
        'levtype' : grbLevType,
        'stream'  : STREAM,
        'step'    : STEPS_VAR,
        'type'    : type_data,
        'data_format'  : dataFormat,
    }
    # Only include levelist if it's not None (surface fields don't have levelist)
    if grbLev is not None:
        request['levelist'] = grbLev
    c.retrieve(dataName, request, outFile)

# Function to deaccumulate flux variables, derive total precipitation and convert units
def deacc_variable(data, pp, FREQ):
    if pp == "ssrd":
        var_tmp=data.select(shortName=f"{pp}")
        return mv.max(0, var_tmp) / (FREQ * 3600)
    elif pp == "strd":
        var_tmp=data.select(shortName=f"{pp}")
        return var_tmp / (FREQ * 3600)
    elif pp == "sf":
        var_tmp=data.select(shortName=f"{pp}")
        return 1000. * mv.max(0, var_tmp) / (FREQ * 3600)
    elif pp == "tp":
        var_lsp=1000.* mv.max(0, 1000. * mv.max(0,data.select(shortName="lsp") ) / (FREQ * 3600))
        var_cp=1000.* mv.max(0, 1000. * mv.max(0, data.select(shortName="cp")) / (FREQ * 3600))
        var_sf=1000.* mv.max(0, 1000. * mv.max(0, data.select(shortName="sf")) / (FREQ * 3600))

        return mv.max(0.0, var_cp + var_lsp - var_sf)
    elif pp == "cp":
        zeps=0.0000001
        var_lsp=1000.* mv.max(0, 1000. * mv.max(0,data.select(shortName="lsp") ) / (FREQ * 3600))
        var_cp=1000.* mv.max(0, 1000. * mv.max(0, data.select(shortName="cp")) / (FREQ * 3600))
        return mv.max(0.0, mv.min(1.0, (var_cp>zeps)*((var_cp+zeps)/(zeps+var_cp+var_lsp))+0*(var_cp<=zeps)))

# Function to process data in regular lat/lon grid
def processDataRegLL(var_f,dx,clatn,clats,clone,clonw,interp_type='con'):
    target_grid={'grid':[dx,dx], 'area':[clats, clonw, clatn, clone]} # S,W,N,E}
    if interp_type == 'bil':
       outputData=mv.regrid(target_grid, data=var_f)
    else:
       if interp_type =='nn':
          interp_method='nearest_neighbour'
       if interp_type =='con':
          interp_method='grid_box_average'
       outputData=mv.regrid(target_grid, data=var_f,interpolation=interp_method)
    return outputData

# Function to get grid point grid information from a grib file (number of latitudes and grid type)
def gridpoint_info(data):
    NLat = mv.grib_get_string(data, 'N')
    if isinstance(NLat, list):
        NLat = NLat[0]
    isOcta=(int(mv.grib_get_string(data, 'isOctahedral')[0]) == 1)
    if isOcta:
       gridType='O'
    else:
       gridType='N'
    return gridType, NLat


def parse_arguments():
    parser = argparse.ArgumentParser(description='Script description')
    parser.add_argument('--date0', type=str, help='Date 0')
    parser.add_argument('--time0', type=str, help='Time 0')
    parser.add_argument('--date1', type=str, help='Date 1')
    parser.add_argument('--date2', type=str, help='Date 2')
    parser.add_argument('--stream', type=str, help='Stream')
    parser.add_argument('--time', type=str, help='Time')
    parser.add_argument('--steps_var0', type=str, help='Steps variables for date0 and time0')
    parser.add_argument('--steps_var', type=str, help='Steps variables for date1 and time1')
    parser.add_argument('--freq', type=int, help='Frequency')
    parser.add_argument('--dataName', type=str,default=None, help='name of data to download')
    parser.add_argument('--dataFormat', type=str, default=None,help='format of data, e.g. grib or nc')
    parser.add_argument('--dataType', type=str, default=None, help='data type, e.g fc, default is None')
    parser.add_argument('--dataPreProc', type=bool, default=False, help='data preprocessing for 2D (regional), default is False')
    parser.add_argument('--dataVar', type=str, default="flux", help='data variables to process, accepted values: "flux", "ml", "sfc"; default is flux')
    parser.add_argument('--gridInfo', type=str, default=None, help='information for data processing in case regional run, default is None')
    parser.add_argument('--gribTemplate', type=str, default="LWdown.grb", help='grib file with the template for gaussian grid point grid, default is "LWdown.grb"')

    args = parser.parse_args()
    return args

def main():
    args = parse_arguments()

    DATE0 = args.date0
    DATE1 = args.date1
    DATE2 = args.date2
    STREAM = args.stream
    TIME0 = args.time0
    TIME = args.time
    STEPS_VAR0 = args.steps_var0
    STEPS_VAR = args.steps_var
    FREQ = args.freq
    dataVariables = args.dataVar
    gribTemplate=args.gribTemplate

    # Default
    dataName='reanalysis-era5-complete'
    type_data='fc'
    dataFormat='grib'
    dataPreProc=False
    # Check for optional arguments
    if args.dataName is not None:
       dataName=args.dataName
    if args.dataType is not None:
       type_data=args.dataType
    if args.dataFormat is not None:
       dataFormat=args.dataFormat
    if args.dataPreProc is not None:
       datapreproc=args.dataPreProc
    if args.gridInfo is not None:
       dx=(args.gridInfo).split(',')[0]
       clatn=(args.gridInfo).split(',')[1]
       clats=(args.gridInfo).split(',')[2]
       clone=(args.gridInfo).split(',')[3]
       clonw=(args.gridInfo).split(',')[4]
       interp_type='con' # Hard-coded!


    # grib to netcdf filename
    if dataVariables == 'flux':
       grbVars=['SSRD', 'STRD', 'CP', 'LSP', 'SF']
       grbLevType="sfc"
       grbLev=None
       fname={"ssrd":"SWdown",
              "strd":"LWdown",
              "sf":"Snowf",
              "cp":"Ctpf",
              "lsp":"lsp",
              "tp":"Rainf", # Derived from cp, lsp and sf
             }
    elif dataVariables == 'ml':
       grbVars=['u', 'v', 't', 'q']
       grbLevType="ml"
       grbLev=137
       grbPressure=['lnsp']
       grbLevPressure=1
       fname={"u":"Wind_E",
              "v":"Wind_N",
              "t":"Tair",
              "q":"Qair",
              "lnsp":"PSurf",}
    elif dataVariables == 'sfc':
       grbVars=['U10', 'V10', 'T2M', 'Q2M', 'SP']
       grbLevType="ml"
       grbLev=None

    if dataVariables == 'sfc':
        raise NotImplementedError("Processing for 'sfc' data variables is not yet implemented.")
    # Start

    # Retrieve data for the initial date
    download_from_cds(DATE0, STREAM, TIME0, STEPS_VAR0, dataName, type_data, dataFormat, grbVars, grbLevType, grbLev,outFile="tmp0.grib")
    if dataVariables == 'ml':
        download_from_cds(DATE0, STREAM, TIME0, STEPS_VAR0, dataName, type_data, dataFormat, grbPressure, grbLevType, grbLevPressure,outFile="tmp0_pressure.grib")

    # Retrieve flux data for the entire period
    download_from_cds(f"{DATE1}/to/{DATE2}", STREAM, TIME, STEPS_VAR, dataName, type_data, dataFormat, grbVars, grbLevType, grbLev, outFile="tmp1.grib")
    if dataVariables == 'ml':
        download_from_cds(f"{DATE1}/to/{DATE2}", STREAM, TIME, STEPS_VAR, dataName, type_data, dataFormat, grbPressure, grbLevType, grbLevPressure, outFile="tmp1_pressure.grib")


    # Process data for 'flux' case
    if dataVariables == 'flux':
        os.system("cat tmp0.grib tmp1.grib > forcing_flux.grb")
        data=mv.read("forcing_flux.grb")
        var0=dict()
        for var in fname.keys():
          if var != 'lsp':
            var_f=deacc_variable(data, var, FREQ)
            validDate=mv.valid_date(var_f)
            # metview likes indeces, and the where function returns a tuple
            mask_idx=np.where([(vv>=dt.strptime(DATE1,"%Y%m%d")) & (vv<= (dt.strptime(DATE2,"%Y%m%d") + timedelta(days=1)))
                                for vv in validDate])[0]
            var_f=var_f[mask_idx]
            # Preprocessing in regular lat/lon grid if requested
            if dataPreProc:
              var_f=processDataRegLL(var_f,dx,clatn,clats,clone,clonw,interp_type)
              var_f=var_f[:-1]
            mv.write(f"{fname[var]}.grb", var_f)
        os.remove("forcing_flux.grb")

    # Process data for 'ml case
    if dataVariables == 'ml':
        # Concatenate tmp0.grib and tmp1.grib into total file
        os.system("cat tmp0.grib tmp1.grib > forcing_ml_aux.grb")
        os.system("cat tmp0_pressure.grib tmp1_pressure.grib > tmp_PSurf.grb")
        for var in fname.keys():
            if var not in ['PSurf','lnsp']:
               os.system(f"grib_copy -w shortName={var} forcing_ml_aux.grb tmp_{fname[var]}.grb")

            data=mv.read(f"tmp_{fname[var]}.grb")
            validDate=mv.valid_date(data)
            # metview likes indeces, and the where function returns a tuple
            mask_idx=np.where([(vv>=dt.strptime(DATE1,"%Y%m%d")) & (vv<= (dt.strptime(DATE2,"%Y%m%d") + timedelta(days=1)))
                           for vv in validDate])[0]
            var_f=data[mask_idx]
            dataTemplate=mv.read(gribTemplate)
            gridType, NLat = gridpoint_info(dataTemplate)
            var_f=mv.read(data=var_f, grid=gridType+NLat)
            # Preprocessing in regular lat/lon grid....
            if dataPreProc:
               var_f=processDataRegLL(data,dx,clatn,clats,clone,clonw,interp_type)
               var_f=var_f[:-1]
            mv.write(f"{fname[var]}.grb", var_f)
            os.remove(f"tmp_{fname[var]}.grb")

        os.remove("forcing_ml_aux.grb")
        os.remove("tmp0_pressure.grib")
        os.remove("tmp1_pressure.grib")


    # Clean
    os.remove("tmp0.grib")
    os.remove("tmp1.grib")

if __name__ == '__main__':
    main()
