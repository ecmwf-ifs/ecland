#cython: boundscheck=False, wraparound=False, cdivision=True
# (C) Copyright 2018- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


## To compile: 
## python setup_cython.py build_ext --inplace



from __future__ import print_function
import numpy as np
cimport numpy as np


ctypedef np.float64_t dtype_f8
ctypedef np.float32_t dtype_f4
ctypedef np.int32_t dtype_i4
ctypedef np.int16_t dtype_i2

def accumulate_river(np.ndarray[dtype=dtype_i4,ndim=1] i1seqx,
                     np.ndarray[dtype=dtype_i4,ndim=1] i1seqy,
                     np.ndarray[dtype=dtype_i4,ndim=2] i2nextx,
                     np.ndarray[dtype=dtype_i4,ndim=2] i2nexty,
                     np.ndarray[dtype=dtype_f8,ndim=2] xin):
  cdef:
    int nseq,iseq,ix,iy,jx,jy,nx,ny
  
  ny = i2nextx.shape[0]
  nx = i2nextx.shape[1]
  nseq = i1seqx.shape[0]
  
  cdef np.ndarray[dtype_f8,ndim=2] xout = np.zeros((ny,nx),dtype=np.double)-9999
  
  for iseq in range(nseq):
    ix = i1seqx[iseq]
    iy = i1seqy[iseq]
    xout[iy,ix] = xin[iy,ix]
  
  for iseq in range(nseq):
    ix = i1seqx[iseq]
    iy = i1seqy[iseq]
    if (i2nextx[iy,ix] > 0):
      jx = i2nextx[iy,ix] -1 
      jy = i2nexty[iy,ix] -1 
      xout[jy,jx] = xout[jy,jx]+xout[iy,ix]
  return xout 

def calc_1d_seq_rivseq(np.ndarray[dtype=dtype_i4,ndim=2] rivseq ):
  
  cdef:
    int ny,nx,ix,iy,jx,jy
    int iseq,iseqnow,iseqmax,nseq

  ny = rivseq.shape[0]
  nx = rivseq.shape[1]
  
  cdef np.ndarray[dtype_i4,ndim=1] i1seqx = np.zeros((ny*nx),dtype=np.int32)-99
  cdef np.ndarray[dtype_i4,ndim=1] i1seqy = np.zeros((ny*nx),dtype=np.int32)-99
  
  iseqmax = np.max(rivseq) 
  iseq=0
  for iseqnow in range(iseqmax + 1):
    for ix in range(nx):
      for iy in range(ny):
        if ( rivseq[iy,ix] == iseqnow ):
          i1seqx[iseq] = ix
          i1seqy[iseq] = iy
          iseq = iseq + 1 
  nseq = iseq - 1
  print('calc_1d_seq_rivseq iseqmax:%i nseq:%i'%(iseqmax,nseq))
  return i1seqx[0:nseq+1],i1seqy[0:nseq+1]
  
                                  
def gen_rivseq(np.ndarray[dtype=dtype_i4,ndim=2] i2nextx,
               np.ndarray[dtype=dtype_i4,ndim=2] i2nexty,):
  
  cdef:
    int ny,nx,ix,iy,jx,jy
    int iseqnow,inext
    
  ny = i2nextx.shape[0]
  nx = i2nextx.shape[1]
  
  cdef np.ndarray[dtype_i4,ndim=2] rivseq = np.ones((ny,nx),dtype=np.int32)
  
  ## calculate river sequence
  
  # set 0 in all nodes with upstream drining here
  # set -9999 to missing 
  # keep 1 for all starting nodes 
  for ix in range(nx):
    for iy in range(ny):
      if ( i2nextx[iy,ix] > 0 ):
        jx = i2nextx[iy,ix] -1 # caution fortran index
        jy = i2nexty[iy,ix] -1 # caution fortran index
        rivseq[jy,jx]=0 
      elif ( i2nextx[iy,ix] == -9999 ):
        rivseq[iy,ix]=-9999
  ## now compute actual sequence
  iseqnow=0
  inext=1
  while ( inext > 0 ):
    iseqnow = iseqnow + 1
    inext = 0 
    for ix in range(nx):
      for iy in range(ny):
        if ( rivseq[iy,ix] == iseqnow ):
          if ( i2nextx[iy,ix] > 0 ):
            jx = i2nextx[iy,ix] - 1
            jy = i2nexty[iy,ix] - 1 
            if ( rivseq[jy,jx] <= iseqnow ):
              inext=inext+1
              rivseq[jy,jx] = iseqnow + 1 
      
  
  return rivseq

def calc_slope_mask(np.ndarray[dtype=dtype_i4,ndim=2] i1next,
                    np.ndarray[dtype=dtype_f4,ndim=2] dslope,
                    dtype_f4 PMAXSLP):
  
  cdef:
    int iseq,nseqall,isok,iseqk
     
  ## compute rivseq 
  nseqall = i1next.shape[0]
  cdef np.ndarray[dtype_i4,ndim=2] rivseq = np.zeros((nseqall,1),dtype=np.int32)
  
  # 0 - start of channel (no upstream)
  # 1 - some upstream.. 
  # -1 end of channel 
  for iseq in range(nseqall):
    if i1next[iseq,0] >0:
      rivseq[i1next[iseq,0]]=1
    else:
      rivseq[iseq]=-1

  cdef np.ndarray[dtype_i4,ndim=2] Smask = np.zeros((nseqall,1),dtype=np.int32)-1
  Smask[rivseq==-1]=0  # false in outlet 
  for iseq in range(nseqall):
    if rivseq[iseq,0] == 0: # start on first cells only 
      iseqk=iseq
      isok=1
      while (i1next[iseqk,0] > 0 and Smask[iseqk,0] == -1):
        if (dslope[iseqk,0]>PMAXSLP and isok== 1):
          # continuity applies
          Smask[iseqk,0]=1
        else:
          # no mote continuity : no mask from here downstream 
          isok=0
          Smask[iseqk,0]=0
        iseqk=i1next[iseqk,0]
        
  return rivseq,Smask

def calc_1d_seq(np.ndarray[dtype=dtype_i4,ndim=2] i2nextx,
                np.ndarray[dtype=dtype_i4,ndim=2] i2nexty,
                debug=False):
  cdef:
    int nlat,nlon,nseqall,nseqriv
    int ix,iy,iseq,nseqall1,jx,jy
    
  nlat = i2nextx.shape[0]
  nlon = i2nextx.shape[1]
  nseqall=np.sum(i2nextx>=-10) 
  nseqriv=np.sum(i2nextx>0)
  if debug:
    print("cython_ext:calc_1d_seq:nlat,nlon,nseqall,nseqriv",nlat,nlon,nseqall,nseqriv)    
  cdef np.ndarray[dtype_i4,ndim=2] i2vector = np.zeros((nlat,nlon),dtype=np.int32)-1
  cdef np.ndarray[dtype_i4,ndim=2] i1seqx = np.zeros((nseqall,1),dtype=np.int32)
  cdef np.ndarray[dtype_i4,ndim=2] i1seqy = np.zeros((nseqall,1),dtype=np.int32)
  cdef np.ndarray[dtype_i4,ndim=2] i1next = np.zeros((nseqall,1),dtype=np.int32)
  
  iseq=-1
  for iy in range(nlat):
    for ix in range(nlon):
      if (i2nextx[iy,ix]> 0 ):
        iseq=iseq+1
        i1seqx[iseq,0]=ix
        i1seqy[iseq,0]=iy
        i2vector[iy,ix]=iseq
  nseqriv1=iseq+1
  for iy in range(nlat):
    for ix in range(nlon):
      if (i2nextx[iy,ix] == -10 or  i2nextx[iy,ix] == -9 ):
        iseq=iseq+1
        i1seqx[iseq,0]=ix
        i1seqy[iseq,0]=iy
        i2vector[iy,ix]=iseq
  nseqall1=iseq+1
  assert(nseqriv==nseqriv1)
  assert(nseqall==nseqall1)

  for iseq in range(nseqall):
    ix=i1seqx[iseq,0]
    iy=i1seqy[iseq,0]
    if (i2nextx[iy,ix]>0):
      jx=i2nextx[iy,ix]-1  ## caution fortran vs python
      jy=i2nexty[iy,ix]-1  ## caution fortran vs python
      i1next[iseq,0]=i2vector[jy,jx]
    else:
      i1next[iseq,0]=-9


  return i2vector,i1seqx,i1seqy,i1next,nseqriv,nseqall

def remap(
      np.ndarray[dtype_i4,ndim=2] inpn,
      np.ndarray[dtype_i4,ndim=3] inpx,
      np.ndarray[dtype_i4,ndim=3] inpy,
      np.ndarray[dtype_f8,ndim=3] inpa,
      np.ndarray[dtype_f8,ndim=2] dataIN):
   
  cdef:
    int nlev,nx,ny,ilev,ix,iy,ixin,iyin
    
  nlev = inpa.shape[0]
  ny   = inpa.shape[1]
  nx   = inpa.shape[2]
  
  cdef np.ndarray[dtype_f8,ndim=2] dataOUT = np.zeros((ny,nx),dtype=np.double)
  
  for iy in range(ny):
    for ix in range(nx):
      #print('aa',iy,ix)
      if inpn[iy,ix] > 0: 
        for ilev in range(inpn[iy,ix]):
          ixin = inpx[ilev,iy,ix] - 1  #fortran to python
          iyin = inpy[ilev,iy,ix] - 1  # fortran to python
        
          dataOUT[iy,ix] = dataOUT[iy,ix] + dataIN[iyin,ixin] * inpa[ilev,iy,ix]
  return dataOUT 

  
def gen_inpmatI_reg(
      np.ndarray[dtype_i4,ndim=2] inpn,
      np.ndarray[dtype_i4,ndim=3] inpx,
      np.ndarray[dtype_i4,ndim=3] inpy,
      np.ndarray[dtype_f8,ndim=3] inpa,
      np.ndarray[dtype_i4,ndim=2] inpnI,
      np.ndarray[dtype_i4,ndim=3] inpxI,
      np.ndarray[dtype_i4,ndim=3] inpyI,
      np.ndarray[dtype_f8,ndim=3] inpaI,):
  
  
  cdef:
    int nx_riv,ny_riv,nmax,np_ok
    int ix_riv,iy_riv,ilev_riv,ilevI
    int iy_ro,ix_ro,new
 
  nmax=inpaI.shape[0]  # max number of levels in input
  ny_riv=inpa.shape[1]  # nlon in input
  nx_riv=inpa.shape[2]  # nlat in input 
  
  np_ok = 0  # just to count # of valie pixel
  
  for iy_riv in range(ny_riv): 
    
    for ix_riv in range(nx_riv):
      
      if ( inpn[iy_riv,ix_riv] == 0 ):
        # nothing to do, just continue
        continue
      np_ok = np_ok + 1
      
      for ilev_riv in range(inpn[iy_riv,ix_riv]):
        iy_ro = inpy[ilev_riv,iy_riv,ix_riv] - 1  # go to python index
        ix_ro = inpx[ilev_riv,iy_riv,ix_riv] - 1  # go to python index
        #print(iy_riv,ix_riv,iy_ro,ix_ro)
               
        new=1
        if ( inpnI[iy_ro,ix_ro] >= 1 ):
          for ilevI in range(1,inpnI[iy_ro,ix_ro]+1):
            if ( inpxI[ilevI,iy_ro,ix_ro] == iy_riv + 1 and 
                 inpyI[ilevI,iy_ro,ix_ro] == iy_riv + 1 ):
              new=0
              inpaI[ilevI,iy_ro,ix_ro] = inpaI[ilevI,iy_ro,ix_ro] + inpa[ilev_riv,iy_riv,ix_riv]
              break
        if ( new == 1 ):
          # it's a new level:
          ilevI=inpnI[iy_ro,ix_ro] + 1
          if ( ilevI >= nmax ):
             raise ValueError('**ERROR** nmax overflow %i, %i'%(ilevI,nmax))
          inpnI[iy_ro,ix_ro] = ilevI
          inpxI[ilevI,iy_ro,ix_ro] = ix_riv + 1 # to fortran index 
          inpyI[ilevI,iy_ro,ix_ro] = iy_riv + 1 # to fortran index
          inpaI[ilevI,iy_ro,ix_ro] = inpa[ilev_riv,iy_riv,ix_riv]
      
  print('cython_ext-gen_inpmatI_reg: Total pixels processed:%i'%np_ok)

def gen_inpmat_inp2riv_hres_reg(
               np.ndarray[dtype_i2,ndim=2] catmXX, 
               np.ndarray[dtype_i2,ndim=2] catmYY,
               np.ndarray[dtype_f8,ndim=1] lat_hre,
               np.ndarray[dtype_f8,ndim=1] lon_hre,
               np.ndarray[dtype_f4,ndim=2] carea,
               np.ndarray[dtype_i2,ndim=2] pmask,
               np.ndarray[dtype_i4,ndim=2] inpn,
               np.ndarray[dtype_i4,ndim=3] inpx,
               np.ndarray[dtype_i4,ndim=3] inpy,
               np.ndarray[dtype_f8,ndim=3] inpa,
               double lat0_ro,
               double dlat_ro,
               double lon0_ro,
               double dlon_ro,
               int ny_ro,
               int nx_ro,
               ):
  cdef:
    int ix_hre,iy_hre,ix_riv,iy_riv
    int nx_hre,ny_hre,np_hre_ok,new
    int ik, nmax, iy_ro , ix_ro 
    
  nx_hre=catmXX.shape[1]
  ny_hre=catmXX.shape[0]
  nmax=inpa.shape[0]
  
  np_hre_ok=0 # just to count # of valid pixels 
  ### main loop on high resolution 
  for iy_hre in range(ny_hre): # 
    #print(iy_hre,ny_hre)
    
    iy_ro =  int( ( lat_hre[iy_hre] - (lat0_ro-0.5*dlat_ro) ) / dlat_ro + 1) # +1 fortran index
    
    for ix_hre in range(nx_hre):
      # only if 
      if ( catmXX[iy_hre,ix_hre] > 0  and catmYY[iy_hre,ix_hre] > 0  ): 
        
        ix_riv = catmXX[iy_hre,ix_hre]-1 # -1 fortran index
        iy_riv = catmYY[iy_hre,ix_hre]-1 # -1 fortran index
        
        if ( pmask[iy_riv,ix_riv] == 1 ): 
        
          ix_ro = int( ( lon_hre[ix_hre] - (lon0_ro-0.5*dlon_ro) ) / dlon_ro + 1 )# +1 fortran index
          if ( ix_ro < 1 or ix_ro > nx_ro) :
            print('dgb',lon_hre[ix_hre],ix_hre,lon0_ro,0.5*dlon_ro,dlon_ro,)
            raise ValueError('**ERROR** ix_in out of bounds ix_ro=%i'%(ix_ro))
          if ( iy_ro < 1 or iy_ro > ny_ro) :
            raise ValueError('**ERROR** iy_in out of bounds iy_ro=%i'%(iy_ro))
          
          np_hre_ok=np_hre_ok+1

          new=1
          if ( inpn[iy_riv,ix_riv] >= 1 ):
            # already found this pixel
            for ik in range(1,inpn[iy_riv,ix_riv]):
              #print('se',ik,inpy[ik,iy_riv,ix_riv],inpx[ik,iy_riv,ix_riv],
                    #iy_in[iy_hre,ix_hre],ix_in[iy_hre,ix_hre] )
              # find the location
              if (inpx[ik,iy_riv,ix_riv] == ix_ro and 
                  inpy[ik,iy_riv,ix_riv] == iy_ro ):
                new=0
                inpa[ik,iy_riv,ix_riv]=inpa[ik,iy_riv,ix_riv]+carea[iy_hre,ix_hre]
                break
          if (new == 1 ) : 
            #its a new level
            ik = inpn[iy_riv,ix_riv] + 1 
            if ( ik >= nmax ):
              raise ValueError('**ERROR** nmax overflow %i, %i'%(ik,nmax))
            inpn[iy_riv,ix_riv] = ik
            inpx[ik,iy_riv,ix_riv] = ix_ro
            inpy[ik,iy_riv,ix_riv] = iy_ro
            inpa[ik,iy_riv,ix_riv] = carea[iy_hre,ix_hre]
            
  print('cython_ext-gen_inpmat_inp2riv_hres: Total hres pixels processed:%i'%np_hre_ok)
  
  
def gen_inpmat_inp2riv_hres_gg(
               np.ndarray[dtype_i2,ndim=2] catmXX, 
               np.ndarray[dtype_i2,ndim=2] catmYY,
               np.ndarray[dtype_f8,ndim=1] lat_hre,
               np.ndarray[dtype_f8,ndim=1] lon_hre,
               np.ndarray[dtype_f4,ndim=2] carea,
               np.ndarray[dtype_i4,ndim=2] inpn,
               np.ndarray[dtype_i4,ndim=3] inpy,
               np.ndarray[dtype_f8,ndim=3] inpa,
               np.ndarray[dtype_f8,ndim=1] latr_gg,
               np.ndarray[dtype_i4,ndim=1] nplon_gg,
               np.ndarray[dtype_i4,ndim=1] nplon_ggC,
               ):
  cdef:
    int ix_hre,iy_hre,ix_riv,iy_riv
    int nx_hre,ny_hre,np_hre_ok,new
    int ik, nmax
    int ilat_gg, ilon_gg, igp_gg
    float dlon_gg
    
  nx_hre=catmXX.shape[1]
  ny_hre=catmXX.shape[0]
  nmax=inpa.shape[0]
  
  np_hre_ok=0 # just to count # of valid pixels 
  ### main loop on high resolution 
  for iy_hre in range(ny_hre): # 
    #print(iy_hre,ny_hre)
    ilat_gg =  np.abs(latr_gg - lat_hre[iy_hre]).argmin()
    dlon_gg = (360./nplon_gg[ilat_gg])
    #print('lat',iy_hre,ilat_gg,lat_hre[iy_hre],latr_gg[ilat_gg],dlon_gg,nplon_gg[ilat_gg])
    for ix_hre in range(nx_hre):
      # only if 
      if ( catmXX[iy_hre,ix_hre] > 0  and catmYY[iy_hre,ix_hre] > 0 ): 
        #ilon_gg= int ( (lon_hre[ix_hre] - (-0.5*dlon_gg))/ dlon_gg  )
        ilon_gg = int(( 0.5*dlon_gg+lon_hre[ix_hre] )/dlon_gg)
        if (ilon_gg == nplon_gg[ilat_gg]):
          ilon_gg=0
        if ( ilon_gg < 0 or ilon_gg >= nplon_gg[ilat_gg]) :
          raise ValueError('**ERROR** ilon_gg out of bounds ilon_gg=%i,max=%i'%(ilon_gg,nplon_gg[ilat_gg]-1))
        igp_gg = nplon_ggC[ilat_gg] - nplon_gg[ilat_gg]+ilon_gg + 1 # +1 fortran index
        
        ##dgb
        #if (igp_gg == 127472+1):
          #print(lat_hre[iy_hre],lon_hre[ix_hre])
        
        ix_riv = catmXX[iy_hre,ix_hre]-1 # -1 fortran index
        iy_riv = catmYY[iy_hre,ix_hre]-1 # -1 fortran index
        np_hre_ok=np_hre_ok+1

        new=1
        if ( inpn[iy_riv,ix_riv] >= 1 ):
          # already found this pixel
          for ik in range(1,inpn[iy_riv,ix_riv]):
            #print('se',ik,inpy[ik,iy_riv,ix_riv],inpx[ik,iy_riv,ix_riv],
                  #iy_in[iy_hre,ix_hre],ix_in[iy_hre,ix_hre] )
            # find the location
            if (inpy[ik,iy_riv,ix_riv] == igp_gg ):
              new=0
              inpa[ik,iy_riv,ix_riv]=inpa[ik,iy_riv,ix_riv]+carea[iy_hre,ix_hre]
              break
        if (new == 1 ) : 
          #its a new level
          ik = inpn[iy_riv,ix_riv] + 1 
          if ( ik >= nmax ):
            raise ValueError('**ERROR** nmax overflow %i, %i'%(ik,nmax))
          inpn[iy_riv,ix_riv] = ik
          inpy[ik,iy_riv,ix_riv] = igp_gg
          inpa[ik,iy_riv,ix_riv] = carea[iy_hre,ix_hre]
            
  print('cython_ext-gen_inpmat_inp2riv_hres: Total hres pixels processed:%i'%np_hre_ok)
  

  
  
