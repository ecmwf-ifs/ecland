#!/usr/bin/env python3
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import argparse
import metview as mv
import xarray as xr
import numpy as np


class vattrib:

    def __init__(self, grb2nc=None, Units=None, LongName=None):
        self.grb2nc   = grb2nc
        self.Units    = Units
        self.LongName = LongName

vtable=dict()

# Dictionary containing variable attributes and info. A new variable should be
# added to this table as well.
vtable['slt']=vattrib('sotype','-', 'soil type')
vtable['z']=vattrib('geopot','m2/m', 'surface geopotential')
vtable['sdor']=vattrib('sdor','m', 'm')
vtable['tvl']=vattrib('tvl','-', 'low vegetation type')
vtable['tvh']=vattrib('tvh','-', 'high vegetation type')
vtable['dl']=vattrib('LDEPTH','m', 'lake depth')
vtable['al']=vattrib('Malbedo','-', 'monthly surface albedo')
vtable['lai_hv']=vattrib('Mlail','-', 'monthly high veg lai')
vtable['lai_lv']=vattrib('Mlaih','-', 'monthly low veg lai')
vtable['cldiff']=vattrib('fwet','-', 'monthly wetland fraction')
vtable['lsm']=vattrib('landsea','-', 'land sea mask')
vtable['cl']=vattrib('CLAKE','-', 'lake cover fraction')
vtable['cvl']=vattrib('cvl','-', 'low vegetation coverage')
vtable['cvh']=vattrib('cvh','-', 'high vegetation coverage')
vtable['vegdiff']=vattrib('cu','-', 'urban coverage')
vtable['lsmgrd']=vattrib('Ctype','-', 'c3/c4 vegetation type')

vtable['lat']=vattrib(None,'degree_north', 'latitude')
vtable['lon']=vattrib(None,'degree_east', 'longitude')
vtable['ntiles']=vattrib(None,'-', 'surface tiles')
vtable['nvtiles']=vattrib(None,'-', 'vegetation tiles')
vtable['nlevs']=vattrib(None,'-', 'soil level centre')
vtable['zuv']=vattrib(None,'m', 'reference level for temperature')
vtable['zphista']=vattrib(None,'m', 'reference level for wind')
vtable['r0vt']=vattrib(None,'kg CO2/m2/s', 'reference respiration')
vtable['Mask']=vattrib(None,'-', 'Catchment mask')
vtable['time']=vattrib(None,'seconds since', 'time since start of the run')
vtable['month']=vattrib(None,'-', 'months in year')

vtable['swvl']=vattrib('SoilMoist','m3/m3', 'soil moisture content per layer')
vtable['stl']=vattrib('SoilTemp','K', 'soil temperature')
vtable['istl']=vattrib('icetemp','K', 'sea ice temperature')
vtable['skt']=vattrib('AvgSurfT','K', 'average skin temperature')
vtable['asn']=vattrib('SAlbedo','-', 'snow albedo')
vtable['rsn']=vattrib('snowdens','kg/m3', 'snow density')
vtable['sd']=vattrib('SWE','kg/m2', 'snow water equivalent')
vtable['tsn']=vattrib('SnowT','K', 'snow temperature')
vtable['src']=vattrib('CanopInt','kg/m2', 'interception layer depth')
vtable['lmld']=vattrib('HLML','m', 'lake mixed layer thickness')
vtable['licd']=vattrib('HLICE','m', 'lake ice thickness')
vtable['lblt']=vattrib('TLBOT','K', 'lake bottom temperature')
vtable['lmlt']=vattrib('TLWML','K', 'lake mixed layer temperature')
vtable['ltlt']=vattrib('TLMNW','K', 'lake mean water temperature')
vtable['lict']=vattrib('TLICE','K', 'lake ice temperature')
vtable['lshf']=vattrib('TLSF','-', 'lake temperature shape factor')

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


# Function to read the arguments
def read_args():
    parser = argparse.ArgumentParser(description='Read a file and a string')
    parser.add_argument('-i', '--input', type=str, help='surfclim/soilinit file')
    parser.add_argument('-l', '--lsm', type=str, help='land sea mask file')
    parser.add_argument('-t', '--which_surface', type=str, help='type of land cover', nargs='?', default=None)
    parser.add_argument('-p', '--latlon', type=str, help='string with lat,lon coordinates (e.g. 40.0,-10.1)')
    parser.add_argument('-m', '--mode', type=str, help='clim or init creation')
    parser.add_argument('-o', '--output', type=str, help='Output file')
    parser.add_argument('-v', '--verbose', action='store_true', help='Enable verbose mode')
    args = parser.parse_args()
    return args

def get_time_since_start(forcing):
    time_dt=mv.valid_date(forcing)
      # Get the first date
    first_date = time_dt[0]
      # Convert the dates to seconds since the first date
    time_var = [(date - first_date).total_seconds() for date in time_dt]
    return first_date.strftime('%Y-%m-%d'),time_var

def create_clim(sfcType, lat, lon, fout, ntiles, nmonths, climVar, climVarSfcType, surfclim):
    """
    Create surface climate data based on the given parameters.

    Args:
        sfcType (str): Surface type. Possible values are 'orig', 'land', 'lake', 'bare', 'grass', 'forest', 'urban'.
        lat (float): Latitude of the location.
        lon (float): Longitude of the location.
        fout (str): Output file path for the created surface climate data.
        ntiles (int): Number of tiles.
        nmonths (int): Number of months.
        climVar (list): List of climate variables.
        climVarSfcType (list): List of climate variables specific to surface type.
        surfclim (object): Surface climate data object.

    Returns:
        None
    """
    dsOut=create_empty_surfclim(climVar,climVarSfcType, lat, lon, ntiles, nmonths)

    # Dictionary to make conversion from variable name to grib code shortName (in particular for temporary grib...)
    var2grbCode=dict()
    for var in climVar+climVarSfcType:
        var2grbCode[var]=var

    var2grbCode['cl']='clake'
    var2grbCode['vegdiff']='urb'
    var2grbCode['cldiff']='fwet'

    # Variables that depend on sfcType
    if sfcType == 'orig':
        lsm=mv.nearest_gridpoint(surfclim.select(shortName='lsm'),lat,lon)
        clake=mv.nearest_gridpoint(surfclim.select(shortName='cl'),lat,lon)
        cvl=mv.nearest_gridpoint(surfclim.select(shortName='cvl'),lat,lon)
        cvh=mv.nearest_gridpoint(surfclim.select(shortName='cvh'),lat,lon)
        urb=mv.nearest_gridpoint(surfclim.select(shortName='vegdiff'),lat,lon)
    elif sfcType == 'land':
        lsm=1.0; clake=0.0
        cvl=mv.nearest_gridpoint(surfclim.select(shortName='cvl'),lat,lon)
        cvh=mv.nearest_gridpoint(surfclim.select(shortName='cvh'),lat,lon)
        urb=mv.nearest_gridpoint(surfclim.select(shortName='vegdiff'),lat,lon)
    elif sfcType == 'lake':
        lsm=0.0; clake=1.0
        cvl=mv.nearest_gridpoint(surfclim.select(shortName='cvl'),lat,lon)
        cvh=mv.nearest_gridpoint(surfclim.select(shortName='cvh'),lat,lon)
        urb=mv.nearest_gridpoint(surfclim.select(shortName='vegdiff'),lat,lon)
    elif sfcType == 'bare':
        lsm=1.0; clake=0.0; cvl=0.0; cvh=0.0; urb=0.0
    elif sfcType == 'grass':
        lsm=1.0; clake=0.0; cvl=1.0; cvh=0.0; urb=0.0
    elif sfcType == 'forest':
        lsm=1.0; clake=0.0; cvl=0.0; cvh=1.0; urb=0.0
    elif sfcType == 'urban':
        lsm=1.0; clake=0.0; cvl=0.0; cvh=0.0; urb=1.0
    else:
        print("Invalid which_surface")

    for var in climVarSfcType:
        if var in mv.grib_get_string(surfclim,"shortName"):
           dsOut[vtable[var].grb2nc].values[0,0]=locals()[var2grbCode[var]]
        else:
              print(f"{var} not found")

    for var in climVar:
        if var in mv.grib_get_string(surfclim,"shortName"):
            varname=var
            if var == 'fwet':
                varname='cldiff'
            varValue=mv.nearest_gridpoint(surfclim.select(shortName=varname), lat, lon)
            if var in ['al','lai_lv','lai_hv','cldiff']:
              dsOut[vtable[var].grb2nc].values[:,0,0]=varValue
            else:
              dsOut[vtable[var].grb2nc].values[0,0]=varValue

        else:
            print(f"{var} not found")

    dsOut.to_netcdf(fout)

def create_init(lat, lon, fout, ntiles, levs, nmonths, initVar, surfinit):
    """
    Create initial conditions for a given latitude and longitude.

    Args:
        lat (float): Latitude of the location.
        lon (float): Longitude of the location.
        fout (str): Output file path for the created initial conditions.
        ntiles (int): Number of tiles.
        levs (list): List of levels.
        nmonths (int): Number of months.
        initVar (list): List of variables for initialization.
        surfinit (object): Object containing surface initialization data.

    Returns:
        None
    """
    dsOut = create_empty_surfinit(initVar, lat, lon, levs, ntiles, nmonths)

    for var in initVar:
        varname = var
        if var in ['swvl', 'stl', 'istl']:
            for lev in np.arange(1, len(levs) + 1):
                if f"{varname}{lev}" in mv.grib_get_string(surfinit, "shortName"):
                    varValue = mv.nearest_gridpoint(surfinit.select(shortName=f"{varname}{lev}"), lat, lon)
                    dsOut[vtable[var].grb2nc].values[lev - 1, 0, 0] = varValue
                else:
                    print(f"{var} not found")
        else:
            if var in (mv.grib_get_string(surfinit, "shortName")):
                varValue = mv.nearest_gridpoint(surfinit.select(shortName=varname), lat, lon)
                dsOut[vtable[var].grb2nc].values[0, 0] = varValue
            else:
                print(f"{var} not found")

    dsOut.to_netcdf(fout)


def create_forcing(forcing, lat, lon, fout):
    """
    Create a forcing dataset for a given latitude and longitude.

    Parameters:
    - forcing: The input forcing dataset.
    - lat: The latitude value.
    - lon: The longitude value.
    - fout: The output file path.

    Returns:
    None
    """
    first_time, time_forcing = get_time_since_start(forcing)
    forcingVar = mv.grib_get_string(forcing, "shortName")[0]
    dsOut = create_empty_forcing(forcingVar, lat, lon, time_forcing, first_time)

    if forcingVar in mv.grib_get_string(forcing, "shortName"):
        if forcingVar in ['lnsp']:
            varValue = np.round(np.exp(np.array(mv.nearest_gridpoint(forcing.select(shortName=forcingVar), lat, lon))), 3)
        else:
            varValue = np.array(mv.nearest_gridpoint(forcing.select(shortName=forcingVar), lat, lon))
        dsOut[vtable[forcingVar].grb2nc].values[:, 0, 0] = varValue
    else:
        print(f"{forcingVar} not found")

    dsOut.to_netcdf(fout)

# Function to create an empty surfclim data array to be filled with the values
def create_empty_surfclim(climVar, climVarSfc, lat, lon, ntiles, nmonths):

    variables = climVar + climVarSfc

    data = {}
    nlat=1
    nlon=1

    data['lat']     = xr.DataArray([lat], dims=['lat'])
    data['lon']     = xr.DataArray([lon], dims=['lon'])
    data['ntiles'] = xr.DataArray(np.arange(1, ntiles+1, dtype=int), dims=['ntiles']).astype(dtype='int32')
    data['nvtiles'] = xr.DataArray(np.arange(1,3, dtype=int), dims=['nvtiles']).astype(dtype='int32')
    data['month']   = xr.DataArray(np.arange(1,nmonths+1, dtype=int), dims=['month']).astype(dtype='int32')
    data['zuv']     = xr.DataArray(np.float32(10.0), dims=[])
    data['zphista'] = xr.DataArray(np.float32(10.0), dims=[])
    data['r0vt']    = xr.DataArray(np.zeros((nlat, nlon)), dims=['lat', 'lon'])
    data['Mask']    = xr.DataArray(np.ones((nlat, nlon)), dims=['lat', 'lon'])

    for var in variables:
        if var not in ['al','lai_lv','lai_hv','cldiff']:
          data[vtable[var].grb2nc] = xr.DataArray(np.empty((nlat, nlon)), dims=['lat', 'lon'])
        else:
          data[vtable[var].grb2nc] = xr.DataArray(np.empty((nmonths,nlat, nlon)), dims=['month','lat', 'lon'])

        try:
            data[vtable[var].grb2nc].attrs['units'] = vtable[var].Units
            data[vtable[var].grb2nc].attrs['long_name'] = vtable[var].LongName
        except AttributeError:
            pass

    dataset = xr.Dataset(data)

    for var in ['lat','lon','ntiles','nvtiles','month','zuv','zphista','r0vt','Mask']:
        try:
           dataset[var].attrs['units'] = vtable[var].Units
           dataset[var].attrs['long_name'] = vtable[var].LongName
        except AttributeError:
           pass

    return dataset


# Function to create an empty surfclim data array to be filled with the values
def create_empty_surfinit(initVar, lat, lon, levs, ntiles, nmonths):

    data = {}
    nlat=1
    nlon=1
    nlevs=len(levs)

    data['lat']     = xr.DataArray([lat], dims=['lat'])
    data['lon']     = xr.DataArray([lon], dims=['lon'])
    data['ntiles']  = xr.DataArray(np.arange(1, ntiles+1, dtype=int), dims=['ntiles']).astype(dtype='int32')
    data['nvtiles'] = xr.DataArray(np.arange(1,3, dtype=int), dims=['nvtiles']).astype(dtype='int32')
    data['nlevs']   = xr.DataArray(levs, dims=['nlevs']).astype(dtype='float32')
    data['month']   = xr.DataArray(np.arange(1,nmonths+1, dtype=int), dims=['month']).astype(dtype='int32')
    data['time']    = xr.DataArray([0], dims=['time'])

    for var in initVar:
        if var not in ['swvl','stl','istl']:
          data[vtable[var].grb2nc] = xr.DataArray(np.empty((nlat, nlon)), dims=['lat', 'lon'])
        else:
          data[vtable[var].grb2nc] = xr.DataArray(np.empty((nlevs, nlat, nlon)), dims=['nlevs','lat', 'lon'])

        try:
            data[vtable[var].grb2nc].attrs['units']     = vtable[var].Units
            data[vtable[var].grb2nc].attrs['long_name'] = vtable[var].LongName
        except AttributeError:
            pass

    dataset = xr.Dataset(data)

    for var in ['lat','lon','ntiles','nlevs','nvtiles','time','month']:
        try:
            dataset[var].attrs['units'] = vtable[var].Units
            dataset[var].attrs['long_name'] = vtable[var].LongName
        except AttributeError:
            pass

    return dataset

# Function to create an empty surfclim data array to be filled with the values
def create_empty_forcing(Fvar, lat, lon, time, first_date):

    data = {}
    nlat=1
    nlon=1
    ntime=len(time)

    data['lat']     = xr.DataArray([lat], dims=['lat'])
    data['lon']     = xr.DataArray([lon], dims=['lon'])
    data['time']    = xr.DataArray(time, dims=['time'])

    data[vtable[Fvar].grb2nc] = xr.DataArray(np.empty((ntime, nlat, nlon)), dims=['time','lat', 'lon'])

    try:
        data[vtable[Fvar].grb2nc].attrs['units']     = vtable[Fvar].Units
        data[vtable[Fvar].grb2nc].attrs['long_name'] = vtable[Fvar].Units
    except AttributeError:
        pass

    dataset = xr.Dataset(data)

    for var in ['lat','lon']:
        try:
            dataset[var].attrs['units'] = vtable[var].Units
            dataset[var].attrs['long_name'] = vtable[var].LongName
        except AttributeError:
            pass
    dataset['time'].attrs['units'] = vtable['time'].Units+' '+first_date+' 00:00:00'
    dataset['time'].attrs['long_name'] = vtable['time'].LongName

    return dataset


def main():
    args=read_args()

    sfcType=args.which_surface
    lat    =float(args.latlon.split(',')[0])
    lon    =float(args.latlon.split(',')[1])
    mode   =args.mode
    fout   =args.output

    # Dimension information
    ntiles  = 10
    nvtiles = 2
    levs    = np.array([0.035, 0.175, 0.64, 1.945])
    nmonths = 12
    ntime   = 1

    climVar=['slt','z','sdor','tvl','tvh','dl',
             'al','lai_lv','lai_hv', 'cldiff','lsmgrd'
            ]
    climVarSfcType=['lsm','cl','cvl','cvh','vegdiff']

    initVar=['swvl', 'stl', 'istl',
             'skt', 'asn', 'rsn',
             'sd', 'tsn', 'src',
             'lmld', 'licd', 'lblt',
             'lmlt', 'ltlt', 'lict', 'lshf',
            ]

    forcingVar=['lnsp',
                'q',
                't',
                'u',
                'v',
                'cp',
                'tp',
                'sf',
                'strd',
                'ssrd']

    # Read input and mask sea points
    surfFile=mv.read(args.input)
    lsm=mv.read(args.lsm)

    if mode == 'clim':
      surfclim=mv.bitmap(surfFile, mv.bitmap(lsm>=0.5,0) )
      create_clim(sfcType, lat, lon, fout, ntiles, nmonths, climVar, climVarSfcType, surfclim)

    if mode == 'init':
      surfinit=mv.bitmap(surfFile, mv.bitmap(lsm>=0.5,0) )
      create_init(lat, lon, fout, ntiles, levs, nmonths, initVar, surfinit)

    if mode == 'forcing':
      forcing=mv.bitmap(surfFile, mv.bitmap(lsm>=0.5,0) )
      create_forcing(forcing, lat, lon, fout)


if __name__ == '__main__':
    main()
