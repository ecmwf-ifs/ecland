# cama-flood related python utilities
# (C) Copyright 2018- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# E. Dutra Oct 2018

from __future__ import print_function
import numpy as np
from netCDF4 import Dataset

PLANET_RADIUS=6371000.  # default radius for area calculations

try:
  import cython_ext as ce
except:
  print('cmflood_pylib:could not load cython_ext: some functions might not work')

def calc_slope_mask(i1next,dslope,PMAXSLP):
  ## compute rivseq
  nseqall = i1next.shape[0]
  rivseq =  np.ma.copy(i1next)*0
  # 0 - start of channel (no upstream)
  # 1 - some upstream..
  # -1 end of channel
  for iseq in range(nseqall):
    if i1next[iseq,0] >0:
      rivseq[i1next[iseq,0]]=1
    else:
      rivseq[iseq]=-1

  Smask = np.zeros((nseqall,1),dtype=int)-1
  Smask[rivseq==-1]=0  # false in outlet
  for iseq in range(nseqall):
    if rivseq[iseq] == 0: # start on first cells only
      iseqk=iseq
      isok=1
      while (i1next[iseqk,0] > 0 and Smask[iseqk] == -1):
        if (dslope[iseqk]>PMAXSLP and isok== 1):
          # continuity applies
          Smask[iseqk]=1
        else:
          # no more continuity : no mask from here downstream
          isok=0
          Smask[iseqk]=0
        iseqk=i1next[iseqk,0]
  return rivseq,Smask

def vec2map(xin,i1seqx,i1seqy,i2vector):

  xout = np.ma.masked_all(i2vector.shape,xin.dtype)
  xout[i1seqy,i1seqx] = xin

  return xout


def map2vec(xin,i1seqx,i1seqy):
  #nx,ny = xin.shape
  #nseqall,1 = i1seqx
  #xout = np.zeros((nseqall,1))
  #for iseq in range(nseqall):
    #xout[iseq] = xin[i1seqy[iseq],i1seqx[iseq]]

  return xin[i1seqy,i1seqx]

def calc_1d_seq(i2nextx,i2nexty,debug=False):

  nlat,nlon = i2nextx.shape
  nseqall=np.sum(~i2nextx.mask)
  nseqriv=np.sum(i2nextx>0)
  if debug:
    print("cmflood_pylib:calc_1d_seq:nlat,nlon,nseqall,nseqriv",nlat,nlon,nseqall,nseqriv)

  i2vector = np.zeros((nlat,nlon),dtype=int)-1
  i1seqx = np.zeros((nseqall,1),dtype=int)
  i1seqy = np.zeros((nseqall,1),dtype=int)
  i1next = np.zeros((nseqall,1),dtype=int)

  iseq=-1
  for iy in range(nlat):
    for ix in range(nlon):
      if (i2nextx[iy,ix]> 0 ):
        iseq=iseq+1
        i1seqx[iseq]=ix
        i1seqy[iseq]=iy
        i2vector[iy,ix]=iseq
  nseqriv1=iseq+1
  for iy in range(nlat):
    for ix in range(nlon):
      if (i2nextx[iy,ix]< 0 ):
        iseq=iseq+1
        i1seqx[iseq]=ix
        i1seqy[iseq]=iy
        i2vector[iy,ix]=iseq
  nseqall1=iseq+1
  assert(nseqriv==nseqriv1)
  assert(nseqall==nseqall1)

  for iseq in range(nseqall):
    ix=i1seqx[iseq]
    iy=i1seqy[iseq]
    if (i2nextx[iy,ix]>0):
      jx=i2nextx[iy,ix]-1  ## caution fortran vs python
      jy=i2nexty[iy,ix]-1  ## caution fortran vs python
      i1next[iseq]=i2vector[jy,jx]
    else:
      i1next[iseq]=-9

  if debug:
    iy=15
    ix=45
    print('iy,ix:',iy,ix)
    iseq=i2vector[iy,ix]
    print('i2vector:iseq',iseq)
    print('iseqy:',i1seqy[iseq])
    print('iseqx:',i1seqx[iseq])
    print('i1next:',i1next[iseq])
    print('i2nexty,i2nextx',i2nexty[iy,ix]-1,i2nextx[iy,ix]-1)
    print('iseqy[i1next],iseqx[i1next]',i1seqy[i1next[iseq]],i1seqx[i1next[iseq]])

  return i2vector,i1seqx,i1seqy,i1next,nseqriv,nseqall


def compute_area(rlon1,rlon2,rlat1,rlat2):
  """ adapted from Dai's code with comments:
  to   calculate area of 1 degree longitude box at each latitude
  by   algorithm by T. Oki, mathematics by S. Kanae, mod by nhanasaki
  on   26th Oct 2003
  at   IIS,UT
  rlat1, rlat2 : latitude -90.0 (south pole) to 90.0 (north pole)
  returns arealat : in m^2
  by approximated equation
  """
  de=np.sqrt(0.00669447)
  drad=PLANET_RADIUS #6378136

  dsin1 = np.sin(rlat1 * np.pi/180.)
  dsin2 = np.sin(rlat2 * np.pi/180.)

  dfnc1 = dsin1*(1+(de*dsin1)**2. /2.)
  dfnc2 = dsin2*(1+(de*dsin2)**2. /2.)

  return np.abs((np.pi*drad**2*(1.-de**2)/180.*(dfnc1-dfnc2))*(rlon2-rlon1))

def get_nc_grid(fin,verbose=True):
  """
  Get grib grid information
  grid_info = get_grib_grid(fin,verbose)
  fin - netcdf file (will read the first field only)
  verbose - if true prints some information
  grid_info: dictionary with the output:
    grid_info['nptot'] = npG # total number of grid points
    grid_info['nlat'] = nlatG # number of latitude lines
    grid_info['nlon'] = 0 # to identify if it is gaussian
    grid_info['nplon'] = nplonG # number longitudes in each latitude
    grid_info['lat'] = latG # global lat
    grid_info['lon'] = lonG # global lon
    grid_info['nplonC'] = nplonGC  # cumulative number of points in each latitude line
    grid_info['latr'] = latrG  # gaussian regular latitude
    grid_info['lonr'] = lonrG  # gaussian regular longitude
    grid_info['dlon'] = dlon  # longitude increment in each latitude line
  """
  if verbose:
    print('get_nc_grid: reading file',fin)
  nc = Dataset(fin,'r')


  ## information from grib file
  nlat=None
  for cdim in ['lat','latitude']:
    if cdim in nc.dimensions:
      nlat = len(nc.dimensions[cdim])
      latr = nc.variables[cdim][:]
      break
  nlon=None
  for cdim in ['lon','longitude']:
    if cdim in nc.dimensions:
      nlon = len(nc.dimensions[cdim])
      lonr = nc.variables[cdim][:]
      break
  if nlat is None or nlon is None:
    raise ValueError('get_nc_grid: Could not find latitude or longitude nlat,nlon',nlat,nlon)

  npG = nlat*nlon
  nplonG = np.zeros((nlat),dtype='i4')+nlon  # number of point in each latitude
  nplonGC = np.cumsum(nplonG,dtype='i4')
  nc.close()

  lonG,latG = np.meshgrid(lonr,latr)
  #lonG = lonG.reshape(npG)
  #latG = latG.reshape(npG)
  dlonG = (lonr[-1]-lonr[0])/(nlon-1)
  dlatG = (latr[-1]-latr[0])/(nlat-1)

  grid_info = {}
  grid_info['nptot'] = npG # total number of grid points
  grid_info['nlat'] = nlat # number of latitude lines
  grid_info['nlon'] = nlon # to identify we'r gaussian !

  grid_info['nplon'] = nplonG # number longitudes in each latitude
  grid_info['lat'] = latG # global lat
  grid_info['lon'] = lonG # global lon
  grid_info['nplonC'] = nplonGC  # cumulative number of points in each latitude line
  grid_info['latr'] = latr  # regular latitude
  grid_info['lonr'] = lonr  # regular longitude
  grid_info['dlon'] = dlonG  # longitude increments
  grid_info['dlat'] = dlatG  # latitude increments

  if verbose:
    print("nptot,nlat,nlon,dlat,dlon:",' ',
          grid_info['nptot'],grid_info['nlat'],grid_info['nlon'],
          grid_info['dlat'],grid_info['dlon'])
  return grid_info


def get_grb_grid(fin,verbose=True):
  import eccodes as ec
  """
  Get grib grid information
  grid_info = get_grib_grid(fin,verbose)
  fin - grib file (will read the first field only)
  verbose - if true prints some information
  grid_info: dictionary with the output:
    grid_info['nptot'] = npG # total number of grid points
    grid_info['nlat'] = nlatG # number of latitude lines
    grid_info['nlon'] = 0 # to identify if it is gaussian
    grid_info['nplon'] = nplonG # number longitudes in each latitude
    grid_info['lat'] = latG # global lat
    grid_info['lon'] = lonG # global lon
    grid_info['nplonC'] = nplonGC  # cumulative number of points in each latitude line
    grid_info['latr'] = latrG  # gaussian regular latitude
    grid_info['lonr'] = lonrG  # gaussian regular longitude
    grid_info['dlon'] = dlon  # longitude increment in each latitude line
  """
  if verbose:
    print('get_grib_grid: reading file',fin)
  f = open(fin)
  gid = ec.codes_grib_new_from_file(f)


  ## information from grib file
  nlatG = ec.codes_get(gid,'Nj') # number of latitudes
  nplonG = np.empty(nlatG,dtype='i4')
  nplonG = ec.codes_get_array(gid,'pl')  # number of point in each latitude
  ec.codes_release(gid)
  f.close()
  ## derived
  nplonGC = np.cumsum(nplonG,dtype='i4')
  npG = np.sum(nplonG,dtype='i4')
  lonr = np.linspace(0.,360.,np.max(nplonG),endpoint=False)
  dlonG = 360./nplonG
  ## gen gaussian lat and weights
  latr,weights = get_reduced_lats_weights(nlatG)

  latG = np.empty(npG,dtype='f8')
  lonG = np.empty(npG,dtype='f8')
  istart=0
  for ilat in range(nlatG):
    iend = istart+nplonG[ilat]
    latG[istart:iend] = latr[ilat]
    lonG[istart:iend] = np.linspace(0.,360.,nplonG[ilat],endpoint=False)
    istart = iend


  grid_info = {}
  grid_info['nptot'] = npG # total number of grid points
  grid_info['nlat'] = nlatG # number of latitude lines
  grid_info['nlon'] = 0 # to identify we'r gaussian !
  grid_info['nplon'] = nplonG # number longitudes in each latitude
  grid_info['lat'] = latG # global lat
  grid_info['lon'] = lonG # global lon
  grid_info['nplonC'] = nplonGC  # cumulative number of points in each latitude line
  grid_info['latr'] = latr  # gaussian regular latitude
  grid_info['lonr'] = lonr  # gaussian regular longitude
  grid_info['dlon'] = dlonG  # longitude increment in each latitude line
  grid_info['gweights'] = weights  # longitude increment in each latitude line


  ## area computations
  area= np.empty(npG,dtype='f8')
  #area = np.empty(npG,dtype='f8')
  istart=0
  for ilat in range(nlatG):
    iend = istart+nplonG[ilat]
    #xarea = 0.5*np.abs(np.sin(np.deg2rad(ybounds[istart,0]))-
                       #np.sin(np.deg2rad(ybounds[istart,2])) )/nplonG[ilat]
    xarea = 4*np.pi*PLANET_RADIUS**2*weights[ilat] / (nplonG[ilat]*np.sum(weights))
    area[istart:iend] = xarea
    #area1[istart:iend] = xarea1
    istart = iend

  grid_info['area'] = area  # longitude increment in each latitude line
  #grid_info['area'] = area  # longitude increment in each latitude line

  if verbose:
    print("nptot,nlat,nlon:",' ',grid_info['nptot'],grid_info['nlat'],grid_info['nlon'])
    print('get_grib_grid: finished')
  return grid_info

def get_reduced_lats_weights(N):
  """
  Compute the latitudes and weigths of a gaussian grid with N latitudes:
  lats,weights = get_reduced_lats_weights(N)

  Using:
  Legendre quadrature points on [-1,1]. Returns the roots of Nth order Legendre polynomial P_N(x) and
  weights (w) to use in Gaussian Quadrature over [-1,1] with weighting function 1.

  Adapted from  Anna Agusti-Panareda
  """
  #import scipy.special
  #sinlat,weight = scipy.special.orthogonal.p_roots(N)
  OUT=np.zeros((N,2))
  OUT[:,0],OUT[:,1] = np.polynomial.legendre.leggauss(N)

  lat=(np.arcsin(OUT[:,0])*180.0/np.pi)[::-1] # invert to be between 90: -90
  weight = OUT[:,1][::-1]
  return lat,weight

def wrapTo360(xin):
  """
  wrap longitues between 0 to 360
  xout = wrapTo360(xin)
  xin: input np.array with longitudes
  xout: output np.array with longitudes
  """
  xout = xin.copy()
  if np.size(xout) > 1 :
      xout[xin<0.] = 360. + xin[xin<0.]
  else:
      if xin<0. :
          xout = 360. + xin
  return xout

def grid_des(cgrid,verbose=False):
  """
  lat,lon = grid_des(cgrid)
  Returns the lat,lon of a specific grid :cgrid
  """
  if cgrid in ["glb_0.25d","glb_15min"]:
    dd=np.double(15./60.)
    dchunk=1
  elif cgrid in ["glb_0.1d","glb_06min"]:
    dd=np.double(6./60.)
    dchunk=1
  elif cgrid in ["glb_05min"]:
    dd=np.double(5./60.)
    dchunk=1
  elif cgrid in ["glb_03min"]:
    dd=np.double(3./60.)
    dchunk=1
  elif cgrid in ["1min","glb_01min"]:
    dd=np.double(1./60.)
    dchunk=3
  elif cgrid in ["30sec","glb_30sec"]:
    dd=np.double(1./120)
    dchunk=6
  else:
    raise ValueError('grid_des: input cgrid not defined: '+cgrid)
  lon=np.linspace(-180.+0.5*dd,180.-0.5*dd,360./dd)
  lat=np.linspace(90.-0.5*dd,-90.+0.5*dd,180./dd)
  if verbose:
    print("carea         west    east   south   north      nx      ny        csizecgrid")
    print("%9s %7.3f %7.3f %7.3f %7.3f %7i %7i %17.15f"%(cgrid,lon[0],lon[-1],lat[0],lat[-1],len(lon),len(lat),dlon))
  return lat,lon,dchunk
