#!/usr/bin/env python
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""
Convert GRIB files on reduced Gaussian grid to NetCDF.

This handles the special case where CDO cannot convert reduced Gaussian
grid data to NetCDF. Uses eccodes to read GRIB and netCDF4 to write.

Based on EC-Earth3 routines from ifs-scripts.

Usage:
    python grb2nc_gaussian.py input.grb output.nc
"""

import sys
import numpy as np
from netCDF4 import Dataset
import eccodes as ec
import time
import argparse


def read_grib_to_dict(grib_file):
    """
    Read all GRIB messages and group by variable/time.
    
    Returns dict: {shortName: {'values': [...], 'lats': ..., 'lons': ..., 'N': ..., 'pl': ..., 'times': [...]}}
    """
    var_data = {}
    
    with open(grib_file, 'rb') as f:
        while True:
            msg = ec.codes_grib_new_from_file(f)
            if msg is None:
                break
            
            try:
                shortName = ec.codes_get(msg, 'shortName')
                values = ec.codes_get_values(msg)
                
                # Get grid info (only need once per variable)
                if shortName not in var_data:
                    lats = ec.codes_get_array(msg, 'latitudes')
                    lons = ec.codes_get_array(msg, 'longitudes')
                    
                    try:
                        N = ec.codes_get(msg, 'N')
                    except:
                        N = None
                    
                    try:
                        pl = ec.codes_get_array(msg, 'pl')
                    except:
                        pl = None
                    
                    var_data[shortName] = {
                        'values': [],
                        'lats': lats,
                        'lons': lons,
                        'N': N,
                        'pl': pl,
                        'times': [],
                    }
                
                # Get time information (for forcing data)
                try:
                    date = ec.codes_get(msg, 'dataDate')  # YYYYMMDD
                    time_val = ec.codes_get(msg, 'dataTime')  # HHMM
                    step = ec.codes_get(msg, 'step', ktype=int)  # forecast step in hours
                    # Convert to hours since reference
                    hour = time_val // 100 + step
                    var_data[shortName]['times'].append((date, hour))
                except:
                    var_data[shortName]['times'].append(None)
                
                var_data[shortName]['values'].append(values)
                
            finally:
                ec.codes_release(msg)
    
    return var_data


def write_netcdf_gaussian(var_data, output_file):
    """
    Write NetCDF file for reduced Gaussian grid data.
    
    Structure:
    - Dimension 'rgrid' for the flattened reduced Gaussian grid points
    - 'latitude' and 'longitude' as 1D variables (size rgrid)
    - 'N' and 'pl' as grid description variables
    - Data variables with dimension (rgrid,), (month, rgrid), or (time, rgrid)
    """
    
    if not var_data:
        print("No data to write")
        return
    
    # Get grid info from first variable
    first_var = list(var_data.values())[0]
    lats = first_var['lats']
    lons = first_var['lons']
    N = first_var['N']
    pl = first_var['pl']
    npoints = len(lats)
    nsteps = len(first_var['values'])
    
    # Determine if this is climatology (12 monthly) or forcing (many timesteps)
    is_monthly_clim = (nsteps == 12)
    is_forcing = (nsteps > 12)
    
    # Create NetCDF
    nc = Dataset(output_file, 'w', format='NETCDF4')
    nc.history = f'Created {time.ctime(time.time())} by grb2nc_gaussian.py'
    nc.Conventions = 'CF-1.6'
    nc.grid_type = 'reduced_gaussian'
    if N is not None:
        nc.gaussian_N = N
    
    # Create dimensions
    nc.createDimension('rgrid', npoints)
    
    # Time dimension handling
    if nsteps > 1:
        if is_monthly_clim:
            nc.createDimension('month', 12)
            month_var = nc.createVariable('month', 'i4', ('month',))
            month_var.long_name = 'month of year'
            month_var.units = 'month'
            month_var[:] = np.arange(1, 13)
            time_dim = 'month'
        else:
            # Forcing data with time dimension
            nc.createDimension('time', nsteps)
            time_var = nc.createVariable('time', 'f8', ('time',))
            time_var.long_name = 'time'
            
            # Create time values (hours since first timestep)
            times = first_var['times']
            if times and times[0] is not None:
                first_date, first_hour = times[0]
                time_var.units = f'hours since {str(first_date)[:4]}-{str(first_date)[4:6]}-{str(first_date)[6:8]} 00:00:00'
                # Calculate hours since start
                time_vals = []
                for t in times:
                    if t is not None:
                        date, hour = t
                        # Simple: assume all same month for now, just use hour index
                        time_vals.append(len(time_vals))
                    else:
                        time_vals.append(len(time_vals))
                time_var[:] = np.arange(nsteps)
            else:
                time_var.units = 'hours since reference'
                time_var[:] = np.arange(nsteps)
            time_dim = 'time'
    
    # Store number of latitudes (for Gaussian grid description)
    if pl is not None:
        nc.createDimension('lat', len(pl))
        pl_var = nc.createVariable('pl', 'i4', ('lat',))
        pl_var.long_name = 'number of points per latitude'
        pl_var[:] = pl
    
    # Coordinate variables
    lat_var = nc.createVariable('latitude', 'f8', ('rgrid',), zlib=True)
    lat_var.long_name = 'latitude'
    lat_var.units = 'degrees_north'
    lat_var.standard_name = 'latitude'
    lat_var[:] = lats
    
    lon_var = nc.createVariable('longitude', 'f8', ('rgrid',), zlib=True)
    lon_var.long_name = 'longitude'
    lon_var.units = 'degrees_east'
    lon_var.standard_name = 'longitude'
    lon_var[:] = lons
    
    # Data variables
    for varname, vdata in var_data.items():
        nv = len(vdata['values'])
        
        if nv == 1:
            # Single timestep - dimension (rgrid,)
            var = nc.createVariable(varname, 'f4', ('rgrid',), zlib=True, complevel=6)
            var[:] = vdata['values'][0]
        else:
            # Multiple timesteps - dimension (time/month, rgrid)
            var = nc.createVariable(varname, 'f4', (time_dim, 'rgrid'), zlib=True, complevel=6)
            for i, vals in enumerate(vdata['values']):
                var[i, :] = vals
        
        var.coordinates = 'latitude longitude'
    
    nc.close()
    print(f"Written: {output_file} ({npoints} grid points, {nsteps} timesteps, {len(var_data)} variables)")


def main():
    parser = argparse.ArgumentParser(description='Convert Gaussian GRIB to NetCDF')
    parser.add_argument('input', help='Input GRIB file')
    parser.add_argument('output', help='Output NetCDF file')
    args = parser.parse_args()
    
    var_data = read_grib_to_dict(args.input)
    write_netcdf_gaussian(var_data, args.output)


if __name__ == '__main__':
    main()
