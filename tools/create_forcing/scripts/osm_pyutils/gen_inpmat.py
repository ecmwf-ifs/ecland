# Module/program to generate runoff input interpolation
# (C) Copyright 2018- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# adapted from gen_inpmat.F90 by Dai. Yamazaki
# E. Dutra Oct 2018

from __future__ import print_function
from netCDF4 import Dataset
import numpy as np
import sys
import os
import time

import cmflood_pylib as cfl
import cython_ext as ce


def gen_output(fout,ginfo_riv,ginfo_inp,inpn,inpx,inpy,inpa,
               inpnI,inpxI,inpyI,inpaI,zmiss):
  t0 = time.time()
  print('generating',fout)
  if os.path.isfile(fout):
    os.remove(fout)

  ncfile = Dataset(fout,mode='w',format='NETCDF4_CLASSIC')
  ncfile.history = "Created "+time.ctime(time.time())

  ## dimensions
  nlev=np.max(inpn)
  nlevI=np.max(inpnI)
  ncfile.createDimension('lev',nlev)
  ncfile.createDimension('lat',ginfo_riv['nlat'])
  ncfile.createDimension('lon',ginfo_riv['nlon'])
  if (ginfo_inp['nlon'] == 0):
    ncfile.createDimension('latin',1)
    ncfile.createDimension('lonin',ginfo_inp['nptot'])
  else:
    ncfile.createDimension('latin',ginfo_inp['nlat'])
    ncfile.createDimension('lonin',ginfo_inp['nlon'])
  ncfile.createDimension('levI',nlevI)

  cvar=ncfile.createVariable('lat','f8',('lat'))
  cvar.long_name = 'latitude'
  cvar.standard_name = 'latitude'
  cvar.units = 'degrees_north'
  cvar.comment = 'river network dimension'
  cvar[:] = ginfo_riv['latr']

  cvar=ncfile.createVariable('lon','f8',('lon'))
  cvar.long_name = 'longitude'
  cvar.standard_name = 'longitude'
  cvar.units = 'degrees_east'
  cvar.comment = 'river network dimension'
  cvar[:] = ginfo_riv['lonr']
  if (ginfo_inp['nlon'] == 0):
    cvar=ncfile.createVariable('latin','f8',('lonin'))
    cvar.long_name = 'latitude'
    cvar.standard_name = 'latitude'
    cvar.units = 'degrees_north'
    cvar.comment = 'input dimension'
    cvar[:] = ginfo_inp['lat']

    cvar=ncfile.createVariable('lonin','f8',('lonin'))
    cvar.long_name = 'longitude'
    cvar.standard_name = 'longitude'
    cvar.units = 'degrees_east'
    cvar.comment = 'input dimension'
    cvar[:] = ginfo_inp['lon']
  else:
    cvar=ncfile.createVariable('latin','f8',('latin'))
    cvar.long_name = 'latitude'
    cvar.standard_name = 'latitude'
    cvar.units = 'degrees_north'
    cvar.comment = 'input dimension'
    cvar[:] = ginfo_inp['latr']

    cvar=ncfile.createVariable('lonin','f8',('lonin'))
    cvar.long_name = 'longitude'
    cvar.standard_name = 'longitude'
    cvar.units = 'degrees_east'
    cvar.comment = 'input dimension'
    cvar[:] = ginfo_inp['lonr']


  ## remap runoff to cama-flood

  cvar=ncfile.createVariable('lev','i4',('lev'))
  cvar.long_name = 'link_level'
  cvar.units = '-'
  cvar.comment = 'Max number of links'
  cvar[:] = np.arange(nlev)

  cvar=ncfile.createVariable('inpa','f8',('lev','lat','lon'),
                             zlib=True,complevel=6,fill_value=zmiss)
  cvar.long_name = 'input area'
  cvar.units = 'm3'

  cvar=ncfile.createVariable('inpx','i4',('lev','lat','lon'),
                             zlib=True,complevel=6,fill_value=zmiss)
  cvar.long_name = 'x index'
  cvar.units = '-'

  cvar=ncfile.createVariable('inpy','i4',('lev','lat','lon'),
                             zlib=True,complevel=6,fill_value=zmiss)
  cvar.long_name = 'y index'
  cvar.units = '-'

  cvar=ncfile.createVariable('nlev','i4',('lat','lon'),
                             zlib=True,complevel=6,fill_value=zmiss)
  cvar.long_name = 'number of levels'
  cvar.units = '-'

  ## remap cama-flood to runoff

  cvar=ncfile.createVariable('levI','i4',('levI'))
  cvar.long_name = 'link_level'
  cvar.units = '-'
  cvar.comment = 'Max number of links'
  cvar[:] = np.arange(nlevI)

  cvar=ncfile.createVariable('inpaI','f8',('levI','latin','lonin'),
                             zlib=True,complevel=6,fill_value=zmiss)
  cvar.long_name = 'input area inverse'
  cvar.units = 'm3'

  cvar=ncfile.createVariable('inpxI','i4',('levI','latin','lonin'),
                             zlib=True,complevel=6,fill_value=zmiss)
  cvar.long_name = 'x index inverse'
  cvar.units = '-'

  cvar=ncfile.createVariable('inpyI','i4',('levI','latin','lonin'),
                             zlib=True,complevel=6,fill_value=zmiss)
  cvar.long_name = 'y index inverse'
  cvar.units = '-'

  cvar=ncfile.createVariable('nlevI','i4',('latin','lonin'),
                             zlib=True,complevel=6,fill_value=zmiss)
  cvar.long_name = 'number of levels inverse'
  cvar.units = '-'

  ## write
  ncfile.variables['nlev'][:,:] = inpn
  ncfile.variables['inpa'][:,:] = inpa[1:nlev+1,:,:]
  ncfile.variables['inpy'][:,:] = inpy[1:nlev+1,:,:]
  ncfile.variables['inpx'][:,:] = inpx[1:nlev+1,:,:]

  ncfile.variables['nlevI'][:,:] = inpnI
  ncfile.variables['inpaI'][:,:] = inpaI[1:nlevI+1,:,:]
  ncfile.variables['inpyI'][:,:] = inpyI[1:nlevI+1,:,:]
  ncfile.variables['inpxI'][:,:] = inpxI[1:nlevI+1,:,:]


  ncfile.close()
  print("Saving data in %6.2f seconds"%(time.time()-t0))
  return

def get_args():
  from argparse import ArgumentParser,ArgumentDefaultsHelpFormatter

  description = 'Select a domain using a mask'
  parser = ArgumentParser(description=description,formatter_class=ArgumentDefaultsHelpFormatter)
  parser.add_argument('-igrid',dest='frunoff',type=str,default=None,
                     help="Input runoff grid")
  parser.add_argument('-iriv',dest='friv',type=str,default=None,
                     help="river network file")
  parser.add_argument('-ihcat',dest='fhres',type=str,default=None,
                     help="Input high resolution catmxy file")
  parser.add_argument('-iharea',dest='fhres_grdare',type=str,default=None,
                     help="Input high resolution area")
  parser.add_argument('-o',dest='fout',type=str,default=None,
                     help="Output file")
  parser.add_argument("-p", dest='fplot',help="plot map of differences",default=False,
                    action="store_true")
  parser.add_argument("-f", dest='fix_area',help="do not apply fix to area",default=True,
                    action="store_false")
  parser.add_argument("-cinv", dest='cinv',help="True for 2-way coupling",default=True,
                    action="store_false")
  parser.add_argument('-n',dest='nmax',type=int,default=100,
                     help="Maximum number of levels direct interpolation")
  parser.add_argument('-nI',dest='nmaxI',type=int,default=100,
                     help="Maximum number of levels inverse interpolation")
  parser.add_argument('-z',dest='zmiss',type=float,default=-9999,
                     help="missing value")
  parser.add_argument('-m',dest='fpmask',type=str,default=None,
                      help='Input file with pmask')
  parser.add_argument('-ix0',dest='ix0',type=int,default=0,
                      help='lon index of clip start')
  parser.add_argument('-iy0',dest='iy0',type=int,default=0,
                      help='lat index of clip start')


  opts = parser.parse_args()
  return opts

##===========================================
## Load grid information
def read_grid_data(friv,fhres,frunoff):

  t0 = time.time()
  ginfo_riv = cfl.get_nc_grid(friv)     # ouput river network, target grid
  if fhres == 'None':
    ginfo_hre = ginfo_riv # at 01 min the files are identical !
  else:
    ginfo_hre = cfl.get_nc_grid(fhres)    # high-res river network catchments

  try:
    ginfo_inp = cfl.get_nc_grid(frunoff)  # input runoff grid to be interpolated
  except:
    ginfo_inp = cfl.get_grb_grid(frunoff)

  print("Loading grid-information in %6.2f seconds"%(time.time()-t0))
  return ginfo_riv,ginfo_hre,ginfo_inp

##===========================================
## Load high-res data
def read_hres(fhres,fhres_grdare,friv):
  t0 = time.time()

  #read grdare
  if fhres_grdare == "None":
    nc = Dataset(friv,'r')
    grdare = nc.variables['ctmare'][:].filled()
    nc.close()
  else:
    nc = Dataset(fhres_grdare,'r')
    grdare = nc.variables['grdare'][:].filled()
    nc.close()
    grdare=grdare*1.e6


  #read catmXX
  if fhres == "None":
    ny,nx = grdare.shape
    catmXX,catmYY = np.meshgrid(np.arange(1,nx+1),np.arange(1,ny+1))
    catmXX[grdare<0]= -9999
    catmYY[grdare<0]= -9999
    catmXX=catmXX.astype('i2')
    catmYY=catmYY.astype('i2')
  else:
    nc = Dataset(fhres,'r')
    catmXX = nc.variables['catmXX'][:].filled()
    catmYY = nc.variables['catmYY'][:].filled()
    nc.close()


  print("Loading high resolution data in %6.2f seconds"%(time.time()-t0))
  return catmXX,catmYY,grdare

##===========================================
## Main calculations
##===========================================
def find_inpn(ginfo_riv,ginfo_hre,ginfo_inp,catmXX,catmYY,grdare,nmax,zmiss,fpmask):

  t0 = time.time()
  ## allocate arrays
  inpn = np.zeros((ginfo_riv['nlat'],ginfo_riv['nlon']),dtype='i4')
  inpa = np.zeros((nmax,ginfo_riv['nlat'],ginfo_riv['nlon']),dtype='f8')
  inpy = np.zeros((nmax,ginfo_riv['nlat'],ginfo_riv['nlon']),dtype='i4')+zmiss
  inpx = np.zeros((nmax,ginfo_riv['nlat'],ginfo_riv['nlon']),dtype='i4')+zmiss

  if fpmask is None:
    pmask = np.zeros((ginfo_riv['nlat'],ginfo_riv['nlon']),dtype='i2')+1
  else:
    nc = Dataset(fpmask,'r')
    pmask = np.ma.filled(nc.variables['pmask'][:],0).astype('i2')


  ## Calculations
  if (ginfo_inp['nlon'] == 0):
    ce.gen_inpmat_inp2riv_hres_gg(catmXX[:,:],
                                  catmYY[:,:],
                                  ginfo_hre['latr'][:],
                                  cfl.wrapTo360(ginfo_hre['lonr'][:]),
                                  grdare[:,:],
                                  inpn,inpx,inpa,
                                  ginfo_inp['latr'],
                                  ginfo_inp['nplon'].astype('i4'),
                                  ginfo_inp['nplonC'].astype('i4'))
    inpy[inpx!=zmiss]=1
  else:
    ce.gen_inpmat_inp2riv_hres_reg(catmXX[:,:],
                                  catmYY[:,:],
                                  ginfo_hre['latr'][:],
                                  ginfo_hre['lonr'][:],
                                  grdare[:,:],pmask,
                                  inpn,inpx,inpy,inpa,
                                  ginfo_inp['latr'][0],ginfo_inp['dlat'],
                                  ginfo_inp['lonr'][0],ginfo_inp['dlon'],
                                  ginfo_inp['nlat'],ginfo_inp['nlon'])

  print("Computing interpolation in %6.2f seconds"%(time.time()-t0))
  print("Maximum number of levels was:%i"%np.max(inpn))
  nlev=np.max(inpn)
  return inpn,inpx[0:nlev+1,:,:],inpy[0:nlev+1,:,:],inpa[0:nlev+1,:,:]

##===========================================
## Main calculations Inverse
##===========================================
def find_inpnI(ginfo_inp,nmax,zmiss,inpn,inpx,inpy,inpa):

  t0 = time.time()
  ## allocate arrays
  if (ginfo_inp['nlon'] == 0):
    asize=(ginfo_inp['nptot'],1)
    asizel=(nmax,ginfo_inp['nptot'],1)
  else:
    asize=(ginfo_inp['nlat'],ginfo_inp['nlon'])
    asizel=(nmax,ginfo_inp['nlat'],ginfo_inp['nlon'])

  inpnI = np.zeros(asize,dtype='i4')
  inpaI = np.zeros(asizel,dtype='f8')
  inpyI = np.zeros(asizel,dtype='i4')+zmiss
  inpxI = np.zeros(asizel,dtype='i4')+zmiss

  ## Calculations
  nlev=np.max(inpn)
  ce.gen_inpmatI_reg(inpn,inpx[1:nlev+1,:,:],inpy[1:nlev+1,:,:],inpa[1:nlev+1,:,:],
                    inpnI,inpxI,inpyI,inpaI)

  print("Computing interpolation inverse in %6.2f seconds"%(time.time()-t0))
  print("Maximum number of levelsI was:%i"%np.max(inpnI))
  nlev=np.max(inpnI)
  return inpnI,inpxI[0:nlev+1,:,:],inpyI[0:nlev+1,:,:],inpaI[0:nlev+1,:,:]

##================================================
## Fill Inverse with dummy values (1-way coupling only and 1arcmin)
##================================================
def fill_dummy_inpnI(ginfo_inp,nmax,zmiss,inpn,inpx,inpy,inpa):

  t0 = time.time()
  ## allocate arrays
  if (ginfo_inp['nlon'] == 0):
    asize=(ginfo_inp['nptot'],1)
    asizel=(nmax,ginfo_inp['nptot'],1)
  else:
    asize=(ginfo_inp['nlat'],ginfo_inp['nlon'])
    asizel=(nmax,ginfo_inp['nlat'],ginfo_inp['nlon'])

  inpnI = np.zeros(asize,dtype='i4')
  inpaI = np.zeros(asizel,dtype='f4')
  inpyI = np.zeros(asizel,dtype='i4')+zmiss
  inpxI = np.zeros(asizel,dtype='i4')+zmiss

  ## Fill with dummy values
  nlev=np.max(inpn)
  inpnI[:,:]=np.maximum(1, nmax-1)
  inpxI[:,:,:]=-9999
  inpyI[:,:,:]=-9999
  inpaI[:,:,:]=-9999
  nlev=nmax


  return inpnI,inpxI[0:nlev+1,:,:],inpyI[0:nlev+1,:,:],inpaI[0:nlev+1,:,:]


##================================
## diagnostics
def run_diag(ctmare_riv,inpa):
  inpaS = np.sum(inpa,axis=0)  # total area in mapping
  npoints_missing = np.sum((ctmare_riv > 0) & (inpaS == 0) )
  print("Number of points with area problems:  %i"%npoints_missing)
  # normalized difference
  norm_diff=100.*(inpaS-ctmare_riv)/ctmare_riv
  npvalid=1.*len(norm_diff.compressed())
  bins=[-100000.,-100.,-50.,-5.,-1.,1.,5.,50.,100.,100000.]
  count,bb=np.histogram(norm_diff.compressed(),bins)
  for i in range(len(bins)-1):
    print("%% points with errors %6i:%6i: %6.2f"%(bins[i],bins[i+1],100*count[i]/npvalid))
  total_area_diff=100.*(np.sum(inpaS)-np.sum(ctmare_riv))/np.sum(ctmare_riv)
  print("Total area difference:%6.2f %%:"%total_area_diff)

##============================================
## Fix inpa to match river network
def fix_area(inpa,ctmare_riv):
  print('Applying area correction')
  inpaS = np.sum(inpa,axis=0)  # total area in mapping
  npoints_missing = np.sum((ctmare_riv > 0) & (inpaS == 0) )
  npok = (ctmare_riv > 0) & (inpaS > 0 )
  fcorr = np.zeros((ctmare_riv.shape))+1
  fcorr[npok] = ctmare_riv[npok] / inpaS[npok]
  inpa_out = (inpa * fcorr)
  return inpa_out

def main():

  ##===========================================
  ## Constants and command line arguments

  tstart=time.time()
  #nmax=100
  #zmiss=-9999

  opts = get_args()
  frunoff=opts.frunoff
  friv=opts.friv
  fhres=opts.fhres
  fhres_grdare=opts.fhres_grdare
  fout=opts.fout
  Lplot=opts.fplot
  nmax=opts.nmax
  nmaxI=opts.nmaxI
  zmiss=opts.zmiss
  Lfix_area=opts.fix_area
  fpmask=opts.fpmask
  ix0=opts.ix0
  iy0=opts.iy0
  cinv=opts.cinv



  ##===========================================
  ## Load grid information
  ginfo_riv,ginfo_hre,ginfo_inp = read_grid_data(friv,fhres,frunoff)

  ##===========================================
  ## Load high-res data
  catmXX,catmYY,grdare = read_hres(fhres,fhres_grdare,friv)

  ##===========================================
  ## Calculations
  inpn,inpx,inpy,inpa = find_inpn(ginfo_riv,ginfo_hre,ginfo_inp,
                                  catmXX,catmYY,grdare,nmax,zmiss,fpmask)

  ##===========================================
  ## diagnostics
  nc = Dataset(friv,'r')
  ctmare_riv = nc.variables['ctmare'][:]
  nc.close()
  run_diag(ctmare_riv,inpa)

  ##============================================
  ## Fix inpa to match river network
  if Lfix_area:
    inpa=fix_area(inpa,ctmare_riv)
    run_diag(ctmare_riv,inpa)

  ##============================================
  ## Compute inverse matrix, if cinv=False fill with dummy values
  if cinv:
    inpnI,inpxI,inpyI,inpaI = find_inpnI(ginfo_inp,nmaxI,zmiss,
                                         inpn,inpx,inpy,inpa)
  else:
    inpnI,inpxI,inpyI,inpaI = fill_dummy_inpnI(ginfo_inp,nmaxI,zmiss,
                                               inpn,inpx,inpy,inpa)

  inpxI[inpxI!=zmiss]=inpxI[inpxI!=zmiss]-ix0
  inpyI[inpyI!=zmiss]=inpyI[inpyI!=zmiss]-iy0
  ##===========================================
  ## Save Output
  ncfile = gen_output(fout,ginfo_riv,ginfo_inp,inpn,inpx,inpy,inpa,
                      inpnI,inpxI,inpyI,inpaI,zmiss)


  if Lplot:
    import matplotlib as mplt
    #mplt.use('Agg')
    import matplotlib.pyplot as plt
    plt.figure()
    levels=[-100,-50,-25,-10,-5,5,10,25,50,100]
    cmap=plt.cm.BrBG
    norm=mplt.colors.BoundaryNorm(levels,cmap.N)
    plt.pcolormesh(ginfo_riv['lonr'],ginfo_riv['latr'],
              norm_diff,
              cmap=cmap,norm=norm,
              rasterized=True);
    #plt.clim([-100,100]);
    plt.colorbar(extend='both',)
    plt.title("100 x (inpa-ctmare)/ctmare")
    fname="area_diff_%i.pdf"%(ginfo_riv['nptot'])
    plt.savefig(fname)
    plt.close()

  print("Total time taken: %6.2f seconds"%(time.time()-tstart))

if __name__ == "__main__":
    sys.exit(main())
