#!/usr/bin/env python
# (C) Copyright 2018- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

from __future__ import print_function
from netCDF4 import Dataset
import time
import numpy as np
import eccodes as ec
import os

import sys

imiss = -9999
rmiss = 1e20

def fix_iniclim_LL(fin,fmask,fout,ftype=None):
    if ftype == 'ini':
      init_var={"lat":"lat",
                "lon":"lon",
                "STL1":"SoilTemp",
                "SWVL1":"SoilMoist",
                "ISTL1":"iceTemp",
                "SD":"SWE",
                "RSN":"snowdens",
                "ASN":"SAlbedo",
                "TSN":"SnowT",
                "SRC":"CanopInt",
                "SKT":"AvgSurfT",
                "var8":"TLWML",
                "var9":"HLML",
                "var10":"TLBOT",
                "UDVW":"TLMNW",
                "VDVW":"TLSF",
                "URTW":"TLICE",
                "VRTW":"HLICE",
           }
    if ftype == 'clim':
      init_var={
                'lon':'lon',
                'lat':'lat',
                'slor':'sst',
                'cl':'CLAKE',
                'cvh':'cvh',
                'cvl':'cvl',
                'lsm':'landsea',
                'lsrh':'lz0h',
                'sdor':'sdor',
                'slt':'sotype',
                'sr':'z0m',
                'tvh':'tvh',
                'tvl':'tvl',
                'Z':'geopot',
                'dl':'LDEPTH',
                'lai_lv':'Mlail',
                'lai_hv':'Mlaih',
                'al':'Malbedo',
                'cldiff':'fwet',
                'lsmgrd':'Ctype',
                'vegdiff':'cu',
                'ice':"seaice",
      }

    ncIN=Dataset(fin,'r')
    ncMask=Dataset(fmask,'r')
    ncOUT=Dataset(fout,mode='w',format='NETCDF4')
    ncOUT.history = 'Created ' + time.ctime(time.time()) + 'based on' + fin
    # copy dimensions
    if ftype == 'ini':
      nlevs=0
    else:
      #nlevs=nlevs_input
      nlevs=4
    for cdim in ncIN.dimensions.keys():
      if ('depth' in cdim):
        nlevs=nlevs+1
      elif ('time' in cdim):
        dlen=len(ncIN.dimensions[cdim])
        cdim='month'
        ncOUT.createDimension(cdim, dlen)
      else:
        if ncIN.dimensions[cdim].isunlimited():
            dlen = None
        else:
            dlen = len(ncIN.dimensions[cdim])
        ncOUT.createDimension(cdim, dlen)

    ncOUT.createDimension('nlevs', nlevs)

    mask=ncMask.variables['Mask']


    for vname, varin in ncIN.variables.items():
      vnameOrig = vname
      if vname in ['TVH', 'TVL']:
        vname=vname.lower()

      if vname in init_var.keys():

         if vname == 'STL1':
           stl_vtype = varin.dtype
           if hasattr(varin, '_FillValue'):
             stl_vfill = varin._FillValue
           continue
         if vname == 'ISTL1':
           istl_vtype = varin.dtype
           if hasattr(varin, '_FillValue'):
             istl_vfill = varin._FillValue
           continue
         elif vname == 'SWVL1':
           swvl_vtype = varin.dtype
           if hasattr(varin, '_FillValue'):
             swvl_vfill = varin._FillValue
           continue
         else:
           vtype = varin.dtype
           vdim = varin.dimensions
           if (('time' in vdim) & (fin=='clim_LL.nc')):
               vdim=('month','lat','lon')
           vfill = None
           zlib = True
           complevel = 6
           if hasattr(varin, '_FillValue'):
               vfill = varin._FillValue
           # if hasattr(varin,'_Shuffle'):
               #zlib = True
           # if hasattr(varin,'_DeflateLevel'):
               #complevel = getattr(varin,'_DeflateLevel')
           print(vname, vtype, vdim, vfill, zlib, complevel)
           cdfvar = ncOUT.createVariable(init_var[vname], vtype, vdim, fill_value=vfill,
                                         zlib=zlib, complevel=complevel)
        # copy attributes
           for catt in varin.ncattrs():
             if catt == '_FillValue':
                continue
             ccat = getattr(varin, catt)
             setattr(cdfvar, catt, ccat)


           xIN = ncIN.variables[vnameOrig][:]
           if vname in ['SD', 'SRC']:
             xIN=np.maximum(0., 1000.0*xIN)
           if init_var[vname] in ['sst']:
             xIN[:]=280.0

           if vname == 'lat':
             nlat=len(xIN)
           if vname == 'lon':
             nlon=len(xIN)

           ndim = xIN.ndim
           if ndim == mask.ndim:
               cdfvar[:,:] = xIN[:,:]
           elif ndim == mask.ndim + 1:
               cdfvar[:,:] = xIN[:, :]
           elif (ndim == mask.ndim - 1):
               cdfvar[:] = xIN[:]


    if ftype == 'ini':
      stl_vdim = ('nlevs', 'lat', 'lon')
      swvl_vdim = ('nlevs', 'lat', 'lon')
      vfill = None
      zlib = True
      complevel = 6
      cdfvar = ncOUT.createVariable(init_var['STL1'], stl_vtype, stl_vdim, fill_value=vfill,
                                           zlib=zlib, complevel=complevel)
      for catt in ncIN.variables['STL1'].ncattrs():
         if catt == '_FillValue':
            continue
         ccat = getattr(ncIN.variables['STL1'], catt)
         setattr(cdfvar, catt, ccat)
      for lev in np.arange(nlevs):
        if lev == 0:
          xIN = ncIN.variables['STL1'][:]
        if lev == 1:
          xIN = ncIN.variables['STL2'][:]
        if lev == 2:
          xIN = ncIN.variables['STL3'][:]
        if lev == 3:
          xIN = ncIN.variables['STL4'][:]

        cdfvar[lev,:,:] = xIN[:, :]

      cdfvar = ncOUT.createVariable(init_var['SWVL1'], swvl_vtype, swvl_vdim, fill_value=vfill,
                                           zlib=zlib, complevel=complevel)
      for catt in ncIN.variables['SWVL1'].ncattrs():
         if catt == '_FillValue':
            continue
         ccat = getattr(ncIN.variables['SWVL1'], catt)
         setattr(cdfvar, catt, ccat)
      for lev in np.arange(nlevs):
        if lev == 0:
          xIN = ncIN.variables['SWVL1'][:]
        if lev == 1:
          xIN = ncIN.variables['SWVL2'][:]
        if lev == 2:
          xIN = ncIN.variables['SWVL3'][:]
        if lev == 3:
          xIN = ncIN.variables['SWVL4'][:]

        cdfvar[lev,:,:] = np.maximum(0.01, np.minimum(1.0, xIN[:, :]) )
    else:
      cvar = ncOUT.createVariable('month', 'f4', ('month',))
      cvar.long_name = 'month'
      cvar.units = 'months since 2000-01-01'
      ncOUT.variables['month'][:] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

      ncOUT.createDimension('x', (nlat*nlon))
      cvar = ncOUT.createVariable('x', 'i4', ('x',))
      cvar.long_name = 'Grid points 1-npoints'
      cvar.standard_name = 'Grid points 1-npoints'
      cvar.units = '-'
      cvar[:] = np.arange(nlat*nlon) + 1

    ncOUT.close()
    ncIN.close()

def fix_forcing_LL(fin,fout,iniDate,dtforc):
    forcing_var={
            "time":"time",
            "lat":"lat",
            "lon":"lon",
            "lnsp":"PSurf",
            "q":"Qair",
            "t":"Tair",
            "STRD":"LWdown",
            "SSRD":"SWdown",
            "u":"Wind_E",
            "v":"Wind_N",
            "CP":"Ctpf",
            "LSP":"Rainf",
            "SF":"SnowF"
       }

    ncIN=Dataset(fin,'r')
    ncOUT=Dataset(fout,mode='w',format='NETCDF4')
    ncOUT.history = 'Created ' + time.ctime(time.time()) + 'based on' + fin

    # Create dimensions
    for cdim in ncIN.dimensions.keys():
      if ('nh' in cdim):
        continue
      else:
        if ncIN.dimensions[cdim].isunlimited():
            dlen = None
        else:
            dlen = len(ncIN.dimensions[cdim])
        ncOUT.createDimension(cdim, dlen)

    for vname, varin in ncIN.variables.items():
      if vname in forcing_var.keys():
         print(vname)
         vtype = varin.dtype
         vdim = varin.dimensions
         vfill = None
         zlib = True
         complevel = 6
         if hasattr(varin, '_FillValue'):
             vfill = varin._FillValue
         print(vname, vtype, vdim, vfill, zlib, complevel)
         cdfvar = ncOUT.createVariable(forcing_var[vname], vtype, vdim, fill_value=vfill,
                                       zlib=zlib, complevel=complevel)

         # copy attributes
         for catt in varin.ncattrs():
             if catt == '_FillValue':
                 continue
             ccat = getattr(varin, catt)
             setattr(cdfvar, catt, ccat)
         if vname == 'lnsp':
            setattr(cdfvar,'long_name',"Surface atmospheric pressure")
            setattr(cdfvar,'units',"Pa")
         if vname == 'LSP':
            setattr(cdfvar,'long_name',"Rainfall rate (convective + stratiform)")
         if vname == 'CP':
            setattr(cdfvar,'long_name',"Convective total precipitation fraction (convective + stratiform)")
         if vname == 'time':
            setattr(cdfvar,'units',"seconds since "+iniDate)

         xIN = ncIN.variables[vname][:]
         ndim = xIN.ndim
         if vname == 'lnsp':
           cdfvar[:,:] = np.exp(xIN[:,:])
         elif vname == 'time':
           cdfvar[:]=dtforc*(xIN-6.)
         else:
           cdfvar[:] = xIN


    ncOUT.close()
    ncIN.close()

