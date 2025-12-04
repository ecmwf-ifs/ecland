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
    
    Returns dict: {shortName: {'values': [...], 'lats': ..., 'lons': ..., 'N': ..., 'pl': ..., 'months': [...]}}
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
                        'months': [],
                    }
                
                # Get month for climatological fields
                try:
                    month = ec.codes_get(msg, 'month')
                except:
                    month = None
                
                var_data[shortName]['values'].append(values)
                var_data[shortName]['months'].append(month)
                
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
    - Data variables with dimension (rgrid,) or (month, rgrid)
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
    
    # Create NetCDF
    nc = Dataset(output_file, 'w', format='NETCDF4')
    nc.history = f'Created {time.ctime(time.time())} by grb2nc_gaussian.py'
    nc.Conventions = 'CF-1.6'
    nc.grid_type = 'reduced_gaussian'
    if N is not None:
        nc.gaussian_N = N
    
    # Create dimensions
    nc.createDimension('rgrid', npoints)
    
    # Check if any variable has multiple timesteps (climatological)
    has_monthly = any(len(v['values']) > 1 for v in var_data.values())
    if has_monthly:
        nc.createDimension('month', 12)
        month_var = nc.createVariable('month', 'i4', ('month',))
        month_var.long_name = 'month of year'
        month_var.units = 'month'
        month_var[:] = np.arange(1, 13)
    
    # Store number of latitudes
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
        nsteps = len(vdata['values'])
        
        if nsteps == 1:
            # Single timestep - dimension (rgrid,)
            var = nc.createVariable(varname, 'f4', ('rgrid',), zlib=True, complevel=6)
            var[:] = vdata['values'][0]
        else:
            # Multiple timesteps (monthly climatology) - dimension (month, rgrid)
            var = nc.createVariable(varname, 'f4', ('month', 'rgrid'), zlib=True, complevel=6)
            for i, vals in enumerate(vdata['values']):
                var[i, :] = vals
        
        var.coordinates = 'latitude longitude'
    
    nc.close()
    print(f"Written: {output_file} ({npoints} grid points, {len(var_data)} variables)")


def main():
    parser = argparse.ArgumentParser(description='Convert Gaussian GRIB to NetCDF')
    parser.add_argument('input', help='Input GRIB file')
    parser.add_argument('output', help='Output NetCDF file')
    args = parser.parse_args()
    
    var_data = read_grib_to_dict(args.input)
    write_netcdf_gaussian(var_data, args.output)


if __name__ == '__main__':
    main()
