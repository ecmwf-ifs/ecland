# (C) Copyright 2018- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# Script to select a region or basin, creating a mask or clipping the domain
# Original: E. Dutra Nov 2018

from __future__ import print_function
from netCDF4 import Dataset
import numpy as np
import sys
import os
import time
import datetime as dt

def get_args():
  from argparse import ArgumentParser,ArgumentDefaultsHelpFormatter

  description = 'Select a domain using a mask'
  parser = ArgumentParser(description=description,formatter_class=ArgumentDefaultsHelpFormatter)
  parser.add_argument('-i',dest='finput',type=str,default=None,
                     help="Input netcdf file ncdata.nc")
  parser.add_argument('-o',dest='foutput',type=str,default=None,
                     help="Output file, will have nextx,nexty variables in the case of mask or all for clipping")
  parser.add_argument('-b',action='store', dest='lbasin',
                      type=int, nargs='*', default=lbasin_dflt,
                      help="Sequence of basins,-1 for all, or list of basins")
  parser.add_argument('-d',action='store', dest='domain',
                      type=float, nargs=4, default=domain_dflt,
                      help="Domain : west east south north")
  parser.add_argument('-t',action='store', dest='ptype',required=True,
                      type=str,choices=['mask','clip'],
                      help="Type of action: mask or clip ")
  parser.add_argument('-pi',action='store',dest='bifIN',required=False,
                      type=str,default=None,
                      help="Input bifurcation file")
  parser.add_argument('-po',action='store',dest='bifOUT',required=False,
                      type=str,default=None,
                      help="Output bifurcation file")
  parser.add_argument('-inpmat',action='store',dest='finpmat',required=False,
                      type=str,default=None,
                      help="Input inpmat file ")
  parser.add_argument("-e", dest='dextend',help="extend domain to fit all basins",default=False,
                    action="store_true")
  parser.add_argument("-cb", action='store',dest='cborder',required=False,type=int,
                      help="#pixel to extend clip border for htessel",default=1)



  opts = parser.parse_args()
  return opts

def fix_limits(lon,lat,domainI):
  slon = np.sort(lon)
  slat = np.sort(lat)

  if ( domainI[1]<domainI[0] ):
    tmp=domainI[0]
    domainI[0]=domainI[1]
    domainI[1]=tmp

  if ( domainI[3]<domainI[2] ):
    tmp=domainI[2]
    domainI[2]=domainI[3]
    domainI[3]=tmp

  domain_ori=[np.min(slon),np.max(slon),np.min(slat),np.max(slat)]
  domain1=domain_ori[:]
  # west
  for ilon in range(len(slon)):
    if domainI[0]==slon[ilon]: domain1[0]=slon[ilon];break
    if slon[ilon] > domainI[0]:
      domain1[0] = slon[np.maximum(0,ilon-1)]
      break
  #east
  for ilon in range(len(slon)):
    if domainI[1] == slon[ilon]: domain1[1]=slon[ilon];break
    if slon[ilon] > domainI[1]  :
      domain1[1] = slon[np.minimum(len(slon)-1,ilon)]
      break
  # South
  for ilat in range(len(slat)):
    if domainI[2]==slat[ilat]: domain1[2]=slat[ilat];break
    if slat[ilat] > domainI[2]:
      domain1[2] = slat[np.maximum(0,ilat-1)]
      break
  # North
  for ilat in range(len(slat)):
    if domainI[3]==slat[ilat]: domain1[3]=slat[ilat];break
    if slat[ilat] > domainI[3]:
      domain1[3] = slat[np.minimum(len(slat)-1,ilat)]
      break
  return domain1,domain_ori

def write_pmask(pmask,ncIN,cvartemplate='lsmask',foutput='pmask.nc'):
  print('generating:',foutput)
  if os.path.isfile(foutput):
    os.remove(foutput)

  ncOUT = Dataset(foutput,mode='w',format='NETCDF4_CLASSIC')

  ## copy dimensions
  varin=ncIN.variables[cvartemplate]
  for cdim in varin.dimensions:
    dlen=len(ncIN.dimensions[cdim])
    ncOUT.createDimension(cdim,dlen)

  cdfvar = ncOUT.createVariable('pmask',varin.dtype,varin.dimensions,
                                fill_value=-9999,zlib=True,complevel=6)
  cdfvar[:]=pmask

  ## copy dimensions variables
  for cvar in varin.dimensions:
    varin=ncIN.variables[cvar]
    cdfvar = ncOUT.createVariable(cvar,varin.dtype,varin.dimensions,
                                fill_value=-9999,zlib=True,complevel=6)
    cdfvar[:] = varin[:]
  ncOUT.close()

def gen_output(foutput,ncIN,copy_vars=None,dclip=None):
  ## create output file
  print('generating',foutput)
  if os.path.isfile(foutput):
    os.remove(foutput)

  ncOUT = Dataset(foutput,mode='w',format='NETCDF4_CLASSIC')

  ## copy global attributes
  for catt in ncIN.ncattrs():
    setattr(ncOUT,catt,getattr(ncIN,catt))
  try:
    hist0 = getattr(ncIN,'history')
  except:
    hist0=''
  dnow=time.ctime(time.time())
  hist=dnow+': sel_region.py '+' '.join(sys.argv[1:])+' \n'+hist0
  setattr(ncOUT,'history',hist)
  setattr(ncOUT,'date_modified',dnow)

  ## copy dimensions
  for cdim in ncIN.dimensions.keys():
      if ncIN.dimensions[cdim].isunlimited():
          dlen=None
      else:
          dlen = len(ncIN.dimensions[cdim])
          if dclip is not None:
            if cdim in ['lon','Lon','Longitude','longitude','x']:
              dlen=dclip['nlonN']
            if cdim in ['lat','Lat','Latitude','latitude','y']:
              dlen=dclip['nlatN']
      ncOUT.createDimension(cdim,dlen)

  ## copy variables:
  for vname, varin in ncIN.variables.items():
    if copy_vars is not None:
      if vname not in copy_vars :
        continue
    vtype = varin.dtype
    vdim = varin.dimensions
    vfill = None
    if hasattr(varin,'_FillValue'):
        vfill = varin._FillValue
    zlib = True
    complevel=6
    #print (vname,vtype,vdim,vfill,zlib)
    cdfvar = ncOUT.createVariable(vname,vtype,vdim,fill_value=vfill,
                  zlib=zlib,complevel=complevel)

    ## copy attributes
    for catt in varin.ncattrs():
      if catt == '_FillValue': continue
      ccat = getattr(varin,catt)
      setattr(cdfvar,catt,ccat)

  return ncOUT

def find_clip_domain(pmask,cborder=1):

  nlat,nlon = pmask.shape

  latmask = np.sum(pmask[:,:].mask,axis=1)
  lonmask = np.sum(pmask[:,:].mask,axis=0)
  ix0=-1
  ix1=-1
  for ilon in range(0,nlon,1):
    if lonmask[ilon] < nlat:
      ix0=ilon
      break
  for ilon in range(nlon-1,0,-1):
    if lonmask[ilon] < nlat:
      ix1=ilon
      break
  # add +/- cborder
  ix0=np.maximum(ix0-cborder,0)
  ix1=np.minimum(ix1+cborder,nlon-1)
  nlonN=ix1-ix0+1

  iy0=-1
  iy1=-1
  for ilat in range(0,nlat,1):
    if latmask[ilat] < nlon:
      iy0=ilat
      break
  for ilat in range(nlat-1,0,-1):
    if latmask[ilat] < nlon:
      iy1=ilat
      break
  # add +/- cborder
  iy0=np.maximum(iy0-cborder,0)
  iy1=np.minimum(iy1+cborder,nlat-1)
  nlatN=iy1-iy0+1
  return {'ix0':ix0,'ix1':ix1,'iy0':iy0,'iy1':iy1,'nlonN':nlonN,'nlatN':nlatN}

def process_mask(foutput,ncIN,pmask):

  ncOUT = gen_output(foutput,ncIN,copy_vars=None)
  # apply to the variables and write output
  for vname, varin in ncOUT.variables.items():
    if vname in ['nextx','nexty']:
      ncOUT.variables[vname][:] = np.ma.masked_where(pmask.mask,ncIN.variables[vname][:])
    else:
      ncOUT.variables[vname][:] = ncIN.variables[vname][:]
  return ncOUT

def mask_domain(domain,rivdata,pmask_in,extend=False):

    pmask = pmask_in.copy()
    llon,llat = np.meshgrid(rivdata['lon'],rivdata['lat'])
    area_in = ( (llon>=domain[0])&
                (llon<=domain[1])&
                (llat>=domain[2])&
                (llat<=domain[3])  )
    pmask[~area_in] =  0
    pp_domain = np.sum(pmask)

    ## filter basins crossing the lat/lon border
    ibb = np.unique(rivdata['basin'][(pmask==1) & ( (llon==domain[0]) |  (llon==domain[1]) |
                                                   (llat==domain[2]) | (llat==domain[3])) ])
    ibM = 0
    print('Masking domain: checking %i basins'%len(ibb))
    for ik,ib in enumerate(ibb):
       bmask = rivdata['basin']==ib
       ntot=np.sum(bmask)
       nclip=np.sum(bmask[pmask==1])
       if (ntot != nclip):
         #print(ib,ntot,nclip)
         ibM = ibM + 1
         if extend:
           pmask[bmask] = 1
         else:
          pmask[bmask]=0
       print('\r processing %i of %i'%(ik,len(ibb)), end="\r")
    if ibM > 0 :
      print(ibM,' basins crossing the domain. Before there where',pp_domain,' points')
    return pmask

def process_clip(foutput,ncIN,pmask,rivdata):

  nlat,nlon = pmask.shape

  dclip = find_clip_domain(pmask)
  print('Clipping, LON:',dclip['ix0'],dclip['ix1'],
        rivdata['lon'][dclip['ix0']],rivdata['lon'][dclip['ix1']],dclip['nlonN'])
  print('Clipping, LAT:',dclip['iy0'],dclip['iy1'],
        rivdata['lat'][dclip['iy0']],rivdata['lat'][dclip['iy1']],dclip['nlatN'])
  print("%i,%i,%i,%i"%(dclip['ix0']+1,
                       dclip['ix1']+1,
                       dclip['iy0']+1,
                       dclip['iy1']+1),
       file=open('cdo_clip_cama.txt','w') )
  print("%i %i"%(dclip['ix0'],dclip['iy0']),file=open('clip_cama_ix0_iy0.txt','w'))

  ncOUT = gen_output(foutput,ncIN,copy_vars=None,dclip=dclip)

  # write clipping information to netcdf file
  for  catt,cval in dclip.items():
    setattr(ncOUT,catt,cval)

  # copy all data just clipping the domain
  for vname, varin in ncOUT.variables.items():
    if varin.dimensions == ('lat',) :
      varin[:] = ncIN.variables[vname][dclip['iy0']:dclip['iy1']+1]
    elif varin.dimensions == ('lon',) :
      varin[:] = ncIN.variables[vname][dclip['ix0']:dclip['ix1']+1]
    elif varin.dimensions == ('lev',) :
      varin[:] = ncIN.variables[vname][:]
    elif varin.dimensions == ('lat', 'lon') :
      xIN = np.ma.masked_where(pmask.mask,ncIN.variables[vname])
      varin[:,:] = xIN[dclip['iy0']:dclip['iy1']+1,dclip['ix0']:dclip['ix1']+1]
    elif varin.dimensions == ('lev','lat', 'lon') :
      for ilev in range(varin.shape[0]):
        xIN = np.ma.masked_where(pmask.mask,ncIN.variables[vname][ilev,:,:])
        varin[ilev,:,:] = xIN[dclip['iy0']:dclip['iy1']+1,dclip['ix0']:dclip['ix1']+1]
    else:
      print(varin)
      raise ValueError("dimensions not coded, please check")

  ## special case for basin
  xin = ncOUT.variables['basin'][:,:]
  xout = xin.copy()
  ib0=1
  for ibb in (np.unique(xin).filled(1e9)):
    xout[xin==ibb]=ib0
    ib0=ib0+1
  ncOUT.variables['basin'][:,:] = xout

  ## special case for nextx, nexty
  for cvar in ['nextx','nexty']:
    xraw=ncOUT.variables[cvar][:,:]
    isok=xraw>=1
    if cvar == 'nextx': x0=dclip['ix0']
    if cvar == 'nexty': x0=dclip['iy0']
    xraw[isok] = xraw[isok] - x0
    ncOUT.variables[cvar][:,:] = xraw

  return ncOUT,dclip

def process_bifurcation(ncOUT,ncIN,bifIN,bifOUT,dclip):
    nextx=ncOUT.variables['nextx'][:,:]
    print("Reading:",bifIN)
    fid = open(bifIN,'r')
    il=0
    npathB=0
    for line in fid:
      if il == 0:
        npath,nlev=[int(line.split()[0]),int(line.split()[1])]
        xdata=[]
      else:
        xtmp=[float(val) for val in line.split()]
        iXX2=int(xtmp[0]-dclip['ix0'])
        iYY2=int(xtmp[1]-dclip['iy0'])
        jXX2=int(xtmp[2]-dclip['ix0'])
        jYY2=int(xtmp[3]-dclip['iy0'])

        if ( iXX2>0 and iXX2<dclip['nlonN'] and iYY2>0 and iYY2<dclip['nlatN'] ):
          if ( jXX2>0 and jXX2<dclip['nlonN'] and jYY2>0 and jYY2<dclip['nlatN'] ):
            if( nextx[iYY2-1,iXX2-1] != -9999 and nextx[jYY2-1,jXX2-1] != -9999 ):
              xtmp[0]=iXX2
              xtmp[1]=iYY2
              xtmp[2]=jXX2
              xtmp[3]=jYY2
              xdata.append(xtmp)
      il=il+1
    fid.close()
    print("Generating:",bifOUT)
    fid = open(bifOUT,'w')
    fid.write("%i %i npath, nlev, (ix,iy), (jx,jy), length, elevtn, depth, (width1, width2, ... wodth_nlev), (lon,lat))\n"%(len(xdata),nlev))
    for il in range(len(xdata)):
      fid.write((" %8i"*4+" %12.2f"*(nlev+3)+" %10.3f"*2+" \n")%(tuple(xdata[il][:])))
    fid.close()
    print("npath full:%i, npath clipped:%i"%(npath,len(xdata)))

def find_inpmat_domain(pmask,finpmat,cborder=1):

  import sys
  sys.path.append('/home/paga/projects/ecland_opensource/ecland/tools/create_forcing/scripts/osm_pyutils/')
  import cython_ext as ce
  print('loading inpmat')
  nlat,nlon = pmask.shape
  nc = Dataset(finpmat,'r')
  zmiss=-9999
  inpa = nc.variables['inpa'][:]
  inpx = nc.variables['inpx'][:]
  inpy = nc.variables['inpy'][:]
  inpn = nc.variables['nlev'][:]

  #inpaI = nc.variables['inpaI'][:]
  #inpxI = nc.variables['inpxI'][:]
  #inpyI = nc.variables['inpyI'][:]
  #inpnI = nc.variables['nlevI'][:]

  nlev,nlat1,nlon1 = inpa.shape
  # double check we're on the same domain
  if ( nlat != nlat1 or nlon != nlon1):
    raise ValueError('**ERROR** "find_inpmat nlat:%i,nlat1:%i,nlon%i,nlon1%i'
                     %(nlat,nlat1,nlon,nlon1))
  # mask area
  inpa[:,pmask.mask]=0
  inpn[pmask.mask]=0

  asize=(len(nc.dimensions['latin']),len(nc.dimensions['lonin']))
  asizel=(len(nc.dimensions['levI'])+2,len(nc.dimensions['latin']),len(nc.dimensions['lonin']))
  inpnI = np.zeros(asize,dtype='i4')
  inpaI = np.zeros(asizel,dtype='f8')
  inpyI = np.zeros(asizel,dtype='i4')+zmiss
  inpxI = np.zeros(asizel,dtype='i4')+zmiss
  print('Computing inverse')
  ce.gen_inpmatI_reg(inpn,inpx,inpy,inpa,
                       inpnI,inpxI,inpyI,inpaI)
  imask=np.ma.masked_where(inpnI==0,inpnI)
  clip=find_clip_domain(imask,cborder=cborder)
  print("find_ipmat_domain:",clip)
  print("%f,%f,%f,%f"%(nc.variables['lonin'][clip['ix0']],
                       nc.variables['lonin'][clip['ix1']],
                       nc.variables['latin'][clip['iy0']],
                       nc.variables['latin'][clip['iy1']]),
       )
  print("%i,%i,%i,%i"%(clip['ix0']+1,
                       clip['ix1']+1,
                       clip['iy0']+1,
                       clip['iy1']+1),
       file=open('cdo_clip_htessel.txt','w') )

  nc.close()


domain_dflt=[-1,-1,-1,1]
lbasin_dflt=[- 1,]
def main():
  ## Default options


  # get comand line arguments
  opts = get_args()

  if opts.domain != domain_dflt and opts.lbasin != lbasin_dflt:
    raise ValueError("Can only apply domain or basin changes, not both at the same time: only -b or -d options allowed")


  ## open input file
  ncIN = Dataset(opts.finput,'r')

  ## Read river data necessary
  rivdata = {}
  for vname in ['nextx','nexty','basin','lat','lon']:
    rivdata[vname] = ncIN.variables[vname][:]

  nptot_ori=np.sum(~rivdata['nextx'].mask)

  ## Generate Mask:
  pmask = np.ma.filled(rivdata['nextx']*0+1,0) # start with all

  lbasin=False

  ##==========================================================
  ##1 mask by area
  if opts.domain == domain_dflt :
    print("No domain changes")
  else:
    # fix limits
    domain1,domain_ori = fix_limits(rivdata['lon'],rivdata['lat'],opts.domain)

    if domain1 == domain_ori:
      print("No domain changes")
    else:
      print('Input domain changed from',opts.domain,' to',domain1)
      pmask = mask_domain(domain1,rivdata,pmask,opts.dextend)

  #===============================================
  #2 Mask by basin:

  if opts.lbasin == lbasin_dflt :
    print("No basin changes")
  else:
    pmask = pmask*0  # mask everything
    lbasin=True
    for ib in opts.lbasin:
      print("Applying mask to basin:",ib)
      pmask[rivdata['basin']==ib]=1 # activate mask for each basin selected

  # create final mask
  pmask = np.ma.masked_where(pmask==0,pmask)
  write_pmask(pmask,ncIN,cvartemplate='lsmask',foutput='pmask.nc')

  if opts.ptype == 'mask':
    ncOUT = process_mask(opts.foutput,ncIN,pmask)

  if opts.ptype == 'clip':
    ncOUT,dclip = process_clip(opts.foutput,ncIN,pmask,rivdata)

    ## Read bifurcation info:
    if opts.bifIN is not None:
      process_bifurcation(ncOUT,ncIN,opts.bifIN,opts.bifOUT,dclip)

    if opts.finpmat is not None:
      find_inpmat_domain(pmask,opts.finpmat,opts.cborder)

  nptot_final=np.sum(~ncOUT.variables['nextx'][:].mask)

  print("Npoints original:",nptot_ori," Npoints final:",nptot_final)

  ncIN.close()
  ncOUT.close()

if __name__ == "__main__":
    sys.exit(main())


