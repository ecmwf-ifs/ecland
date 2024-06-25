#!/usr/bin/env python3
# (C) Copyright 2023- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


import matplotlib.pyplot as plt
import xarray as xr
from datetime import datetime
import matplotlib.dates as mdates
import plot_utils


expColor=["k","r","b","y","g","m"]
class variable:

    def __init__(self, ncfile=None, ncvar=None,nlev=None,
                 mfact=1., isDeriv=False):
        self.ncfile  = ncfile
        self.ncvar   = ncvar
        self.nlev    = nlev
        self.mfact   = mfact
        self.isDeriv = isDeriv

vartable={}

# To plot a new variable, add a new entry to the vartable list:
# vartable[<variable_name_for_plotting>] = variable(<file_containing_the_variable>,<netcdf_var_name>,<vertical_level_to_plot (def=1)>, <scaling_factor (def=1)>, <is_derived_from_combination_of_other_var (def=False)>)
vartable['soilMoist_1']  = variable('o_gg.nc', 'SoilMoist',nlev=1,) # mfact=1./(0.07 * 1000.))
vartable['soilMoist_2']  = variable('o_gg.nc', 'SoilMoist',nlev=2,) # mfact=1./(0.07 * 1000.))
vartable['soilTemp_1']   = variable('o_gg.nc', 'SoilTemp', nlev=1,)
vartable['soilTemp_2']   = variable('o_gg.nc', 'SoilTemp', nlev=2,)
vartable['Tskin']       = variable('o_gg.nc', 'AvgSurfT',)
vartable['Tsnow_top']   = variable('o_gg.nc', 'SnowTML',nlev=1)
vartable['SWE_total']   = variable('o_gg.nc', 'SWE', mfact=1)
vartable['SnowDensity'] = variable('o_gg.nc', 'snowdens')
vartable['SnowAlbedo']  = variable('o_gg.nc', 'SAlbedo')
vartable['SnowDepth']  = variable('o_gg.nc', ["SWE","snowdens"],isDeriv=True)

vartable['SensibleHeat']  = variable('o_efl.nc', 'Qh')
vartable['LatentHeat']  = variable('o_efl.nc', 'Qle')
vartable['SoilHeat']  = variable('o_efl.nc', 'Qg',mfact=-1)
vartable['LW_net']  = variable('o_efl.nc', 'LWnet')
vartable['SW_net']  = variable('o_efl.nc', 'SWnet')

vartable['T2m']     = variable('o_d2m.nc', 'T2m')
vartable['D2m']     = variable('o_d2m.nc', 'D2m')


# Script to plot timeseries of output from ECLand
def plot_timeseries(expnames,expdir,plotname,plotdir,varlist=None):
    # List of possible output files from ECLand:

    # List of variables to plot:
    if varlist is None:
      varlist=['Tskin',
               'soilTemp_1',
               'soilTemp_2',
               'Tsnow_top',
               'SnowDensity',
               'SnowDepth',
               'SWE_total',
               'SnowAlbedo',
               'soilMoist_1',
               'soilMoist_2',
               'SensibleHeat',
               'LatentHeat',
               'SoilHeat',
               'LW_net',
               'SW_net',
               'T2m',
               'D2m',
      ]

    Nplot=len(varlist)
    fig, axes = plot_utils.create_subplot_grid(Nplot)
    # Trick to plot when varlist=1
    if len(varlist)==1:
        axes=[axes]
    for var,ax in zip(varlist,axes):
        fname=vartable[var].ncfile
        varnc=vartable[var].ncvar
        mfact=vartable[var].mfact
        for ee,exp in enumerate(expnames):
           fdata=xr.open_dataset(f"{expdir}/{exp}_2022-2022/{fname}")
           if not vartable[var].isDeriv:
             if vartable[var].nlev is not None:
                varplot=mfact*fdata[varnc][:,vartable[var].nlev,0,0]
             else:
                varplot=mfact*fdata[varnc][:,0,0]
             units=fdata[varnc].units
           else:
             if var=='SnowDepth':
                varplot=fdata["SWE"][:,0,0]/fdata["snowdens"][:,0,0]
                units="m"

           time=fdata['time'][:]
           ax.plot(time, varplot, color=expColor[ee],label=exp)
           ax.set_ylabel(f'{var} ({units})')
           # Customize the x-axis tick labels to show only the hours
           interv=2
           if len(time)>24:
               interv=4
           hours = mdates.HourLocator(interval=interv)  # Set tick every 1 hour
           ax.xaxis.set_major_locator(hours)
           ax.xaxis.set_major_formatter(mdates.DateFormatter('%H'))
           ax.legend()

    plt.tight_layout()
    plt.savefig(f'{plotdir}/{plotname}.pdf')


def plot_forcing(expnames,expdir,varlist=None):
    # List of variables to plot:
    if varlist is None:
      varlist=['LWdown',
               'PSurf',
               'Tair',
               'Qair',
               'Wind_E',
               'Wind_N',
      ]

    Nplot=len(varlist)
    fig, axes = plot_utils.create_subplot_grid(Nplot)
    # Trick to plot when varlist=1
    if len(varlist)==1:
        axes=[axes]
    for var,ax in zip(varlist,axes):
        for ee,exp in enumerate(expnames):
           fdata=xr.open_dataset(f"{expdir}/met_era5HT_{exp}_2022-2022.nc")
           varplot=fdata[var][:,0,0]
           units=fdata[var].units

           time=fdata['time'][:]
           ax.plot(time, varplot, color=expColor[ee],label=exp)
           ax.set_ylabel(f'{var} ({units})')
           # Customize the x-axis tick labels to show only the hours
           hours = mdates.HourLocator(interval=2)  # Set tick every 1 hour
           ax.xaxis.set_major_locator(hours)
           ax.xaxis.set_major_formatter(mdates.DateFormatter('%H'))
           ax.legend()

    plt.tight_layout()
    plt.show()



if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Plot selected surface variables from ecLand output files for a series of experiments.")
    parser.add_argument("-e","--expnames", nargs="+", help="list of experiment names")
    parser.add_argument("-d","--expdir", help="path to experiment data", default="./")
    #parser.add_argument("--plot", help="type of plot to produce (time series,...)", default="./")
    parser.add_argument("-p","--plotname", help="plotname", default="plot_ECLand")
    parser.add_argument("-D","--dstdir", help="Destination directory for plots", default="./")
    args = parser.parse_args()

    plot_timeseries(args.expnames, args.expdir, args.plotname,args.dstdir)



