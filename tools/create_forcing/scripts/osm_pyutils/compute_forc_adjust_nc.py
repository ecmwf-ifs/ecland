#!/usr/bin/env python3
# (C) Copyright 2015- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


# compute lapse rate from model level data

from __future__ import print_function
import eccodes as ec
import xarray as xr
import numpy as np
from netCDF4 import Dataset
import sys
import os
import datetime
import time


def read_args():
    import argparse
    parser = argparse.ArgumentParser(description='adjst forcing')
    parser.add_argument('-iQ', dest='fqin', default='NONE', type=str, metavar='fqin',
                        help='input Q')
    parser.add_argument('-iT', dest='ftin', default='NONE', type=str, metavar='ftin',
                        help='input quair file ')
    parser.add_argument('-iP', dest='fpin', default='NONE', type=str, metavar='fpin',
                        help='input lnsp file ')
    parser.add_argument('-iDZ', dest='fdzin', default='NONE', type=str, metavar='fdzin',
                        help='input dz')
    parser.add_argument('-iDTDZ', dest='fdtdzin', default='NONE', type=str, metavar='fdtdzin',
                        help='input dtdz')
    args = parser.parse_args()
    return args


def calc_PES(Tair):

    # ratio between water and ice : Alpha in eq(7.73) for IFS Document Cy38R1e
    # staturation vapor
    FOEALFA = np.minimum(1.0, ((np.maximum(
                         RTICE, np.minimum(RTWAT, Tair)) - RTICE) / (RTWAT - RTICE))**2.0)
    # pressure with Teten's formula, eq. 7.15 for IFS doc CY38R1
    FOEEWM = R2ES * (FOEALFA * np.exp(R3LES * (Tair - RTT) / (Tair - R4LES)) +
                     (1.0 - FOEALFA) * np.exp(R3IES * (Tair - RTT) / (Tair - R4IES)))
    return FOEEWM


def compute_correction(Tin, Qin, Pin, DTDZin, DZ):
    # 1. Saturation vapor pressure with Teten's formula: eq (7.5)
    PESin = calc_PES(Tin)

    # 2. relative humidity
    RHin = (Pin * Qin * (RETV + 1.)) / ((1. + RETV * Qin) * PESin)
    RHin = np.maximum(PRHMIN, np.minimum(RHin, PRHMAX))

    # 3. New temperature
    Tnew = Tin + DZ * DTDZin
    PESnew = calc_PES(Tnew)  # new staturation vapor pressure

    # 4. update PSurf
    Tv = 0.5 * (Tin + Tnew) * (1. + RETV * Qin)  # not considering avg Q
    Pnew = Pin / np.exp(RG * DZ / RD / Tv)

    # 5. new Qair
    Qnew = RHin * PESnew / (Pnew * (RETV + 1.) - RHin * PESnew * RETV)


    return Tnew, Qnew, Pnew


args = read_args()


FQIN    = args.fqin
FTIN    = args.ftin
FPIN    = args.fpin
FDZIN   = args.fdzin
FDTDZIN = args.fdtdzin

# CONSTANTS:
RG = 9.80665            # Gravitational Acceleration [m/s2]
RKBOL = 1.380658E-23    # Boltzmann constant (k) [J/K]
RNAVO = 6.0221367E+23   # Avogadro constant (N_A) [1/mol]
R = RNAVO * RKBOL         # Perfect gas constant (= 8.314511211948600)
RMD = 28.9644           # Dry air mass
RMV = 18.0153           # Vapour  mass
RD = 1000.0 * R / RMD       # Dry air cst. (= 287.0596736665907 J/kg/K)
RV = 1000.0 * R / RMV       # Vapour  cst. (= 461.5249933083879 J/kg/K)
RETV = RV / RD - 1.0        # Rv/Rd-1 (= 0.608)
RTT = 273.16            # Temperature of water fusion at normal pressure
RTWAT = RTT             # Criteria for temperature with respect to liquid water
RTICE = RTT - 23.0        # Criteria for temperature with respect to ice
R2ES = 611.21           # Constant in Teten's formula: eq(7.5)
# Mixing Ratio Over Liquid Water in Teten's formula: eq(7.5)
R3LES = 17.5020
R4LES = 32.190         # Constant in Teten's formula for liquid water: eq(7.5)
R3IES = 22.5870        # Mixing Ratio Over Ice in Teten's formula: eq(7.5)
R4IES = -0.70          # Constant in Teten's formula for ice: eq(7.5)
PRHMAX = 0.999          # max relative humidity
PRHMIN = 0.001          # Min relative humidiy
DTDZMIN = -0.0098     # min DTDZ
DTDZMAX = 0           # max DTDZ

# open input files
FNAMES = {}
FNAMES['Q'] = FQIN
FNAMES['T'] = FTIN
FNAMES['P'] = FPIN
FNAMES['DZ'] = FDZIN
FNAMES['DTDZ'] = FDTDZIN
FIN = {}

for kk in FNAMES.keys():
    FIN[kk] = xr.open_dataset(FNAMES[kk])

# Set all lat/lon values == to Q
for kk in ['T','P','DZ','DTDZ']:
      FIN[kk]['lat']=FIN['Q']['lat']
      FIN[kk]['lon']=FIN['Q']['lon']

# output files
FOUT = dict()
filenameout=dict()
for kk in ['Q', 'T', 'P']:
    dname = os.path.dirname(FNAMES[kk])
    if len(dname) > 0:
        dname = dname + '/'
    else:
        dname = "./"
    filenameout[kk] = dname + 'corr_' + os.path.basename(FNAMES[kk])
    FOUT[kk] = FIN[kk].copy(deep=True)
    try:
        hist0 = getattr(ncIN, 'history')
    except:
        hist0 = ''
    dnow = datetime.datetime.now().strftime("%c")
    hist = (dnow + ': compute_forc_adjust.py using: ' + os.path.basename(FNAMES['DTDZ']) + ' and ' +
            os.path.basename(FNAMES['DZ'] + ' \n' + hist0))
    #setattr(FOUT[kk], 'history', hist)
    #setattr(FOUT[kk], 'date_modified', dnow)

# load lat lon just for plot (debug)
lat = FIN['Q']['lat'][:]
lon = FIN['Q']['lon'][:]

# find variable names
CVAR = {}
for kk, ff in FIN.items():
    for vname, vv in ff.items():
        if kk == 'DZ':
            nmin = 2
        else:
            nmin = 3
        if len(vv.dims) >= nmin:
            CVAR[kk] = vname
            break

nt = len(FIN['Q']['time'])

DZ = FIN['DZ'][CVAR['DZ']][0,:, :]  # load elevation

ntdtdz = len(FIN['DTDZ']['time'])
dtdz0 = FIN['DTDZ'][CVAR['DTDZ']][0, :, :]


GID = {}
CID = {}
idtdz=0
t0 = time.time()
for ik in range(nt):
    print(ik)
    Qin = FIN['Q'][CVAR['Q']][ik, :, :]
    Tin = FIN['T'][CVAR['T']][ik, :, :]
    Pin = FIN['P'][CVAR['P']][ik, :, :]
    if ntdtdz == 1:
        DTDZin = dtdz0
    else:
        DTDZin = FIN['DTDZ'].variables[CVAR['DTDZ']][idtdz, :, :]
        if ((ik+1) % 24 == 0):
            idtdz=idtdz+1

    LLNSP = False
    if (np.max(Pin) < 20.):
        LLNSP = True
    if LLNSP:
        Pin = np.exp(Pin)
    # Main calculations
    Tnew, Qnew, Pnew = compute_correction(Tin, Qin, Pin, DTDZin, DZ)

    # to output
    if LLNSP:
        Pnew = np.log(Pnew)
    FOUT['Q'][CVAR['Q']][ik, :, :] = Qnew
    FOUT['T'][CVAR['T']][ik, :, :] = Tnew
    FOUT['P'][CVAR['P']][ik, :, :] = Pnew

t1 = time.time()

print('time to correct forcing: '+str(np.round((t1-t0),2)))
for kk in ['Q','T','P']:
  FOUT[kk].to_netcdf(f"{filenameout[kk]}")

sys.stdout.flush()

# close input files
for kk, ff in FIN.items():
    ff.close()
for kk, ff in FOUT.items():
    ff.close()
