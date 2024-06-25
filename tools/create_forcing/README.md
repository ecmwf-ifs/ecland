ecLand create input tool
******

A tool to create initial conditions and meteorological forcing data to run ecLand using ERA5 data. 

Introduction
============

EcLand requires a set of input fields to run for a given `<site>`:
- a file containing the initial conditions for the model prognostic variables (name convention `surfinit_<site>.nc`)
- a file containing the static information on the site to run, e.g. fraction of low/high vegetation, soil type etc. (called `surfclim_<site>.nc`)
- a file containing the time-varying atmospheric forcing variables, e.g. wind speed, surface pressure, air temperature, solar radiation etc. (called `met_era5HT_<site>.nc`)

This tool automates the retrieval and creation initial condition and forcing files on a site or region from era5 data for ECLand.
The script can retrieve either from MARS (Meterological Archival and Retrieval System) or CDS (Climate Data Store) the ERA5 data. At the moment the tool only works on ERA5 data 
in GRIB format.  It allows the "raw" global forcing to be downloaded and stored on a user defined path in GRIB for future re-use.

For point extraction, the nearest gridpoint of ERA5 to the selected lat/lon location is used.
For regional (2D) extraction, the data at native ERA5 resolutions are interpolated using bilinear and nearest neighbour for the initial and static fields (e.g. soil type is interpolated with nearest neighbour);
forcing fields are interpolated using conservative interpolation.

For creation of Cama-Flood river maps for regional maps, additional static fields are required. These can be accessed through the Cama-Flood developer's page at this [website](http://hydro.iis.u-tokyo.ac.jp/~yamadai/cama-flood/).


License
=======

(C) Copyright 2023- ECMWF.

This software is licensed under the terms of the Apache Licence Version 2.0
which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

In applying this licence, ECMWF does not waive the privileges and immunities
granted to it by virtue of its status as an intergovernmental organisation nor
does it submit to any jurisdiction.



General description and requirements
===========

Supported Platforms
-------------------

- Linux
- Apple MacOS

Other UNIX-like operating systems may work too out of the box.


Requirements
------------

- eccodes
- metview
- metview-python
- cds api (if not using MARS for data download)
- pyYAML
- netcdf4
- cdo 2.2.0 or newer
- nco

#### Quick instructions for ECMWF HPC

Load modules in the environment:

    module load prgenv/intel
    module load ecmwf-toolbox/new python3/new netcdf4/new cdo/2.2.0 nco eclib/new
    # to run the optional tests the following is required
    module load ecbuild/new

For processing of large amount of data and/or using parallelised processing, it is recommended to run the tool 
in ecinteractive node, for instance to use 2 processors and 32GB of memory:

    ecinteractive -c2 -m32GB -s40GB
    export TMPDIR=$SCRATCHDIR  # For metview tmpdir

#### Quick instructions for MacOS

Given the multiple libraries used, it is recommended to use a package manager like conda and create an environment
containing the necessary code. E.g. for macOS, assuming `conda-forge` is in the list of channels:

    conda create -n ecLand-env # Create environment
    conda activate ecLand-env
    conda install metview  -c conda-forge # metview binaries  
    conda install metview-python  -c conda-forge # metview-python 
    conda install anaconda::wget # wget 
    conda install coreutils gnu-coreutils
    conda install netcdf4   # netcdf4
    conda install nco # nco
    conda install cdo # cdo
    pip install cdsapi # cdsapi 
        
if installing cdo in conda has issues, you can try installing cdo binaries outside of conda environment, and then add it manually to the path.  E.g. using homebrew on macOS outside the conda environment:

    brew install cdo
    # assuming installation path /opt/homebrew/bin :
    export PATH=${PATH}:/opt/homebrew/bin


Run tests (optional)
---------

To run the optional test environment, ecbuild (>=3.7.0) should be installed and in the path.

If any modification to the scripts is done, or to test successfull installation and setting of the environment, 
tests can be run. From the parent folder, `tools/create_forcing`:
 
    mkdir build
    cmake -B build
    cd build   
    ctest [-VV]


Run the tool
------------

To run the tool:

`./ecland_create_forcing.py -c <config.yaml> [--clean]"`

where `config.yaml` is a configuration file containing the setup for the extraction and creation of the input data.

Configuration file
-------------------

To run the tool a yaml configuration file must be provided. The repository contains two examples for a "1D" (i.e. site) and "2D" (i.e. regional) data extraction.

### Description of the configuration file:

The block `sites` contains general information on the type of extraction. If it is a "1D" or "2D" extraction, the `GROUP` name defining sites under a common project.
This block also specifies the list of sites and the time period for which the forcing fields should be created. Multiple entries of `sitenames`, `iniDate` and `endDate`
can be provided simultaneously for 1D extractions; for 2D, only one sitenames at the time can be processed.

    sites:
        ftype: 1D                                 # 1D or 2D -- note that 2D allows only 1 "sitenames" (region) at the time 
                                                  #             (and so iniDate, endDate, dx, clat*,clon* ...)
        group: TEST                               # Group the sites extracted under a common label/project, e.g. TERRENO,PLUMBER2 etc.
        sitenames:                                # site name code, MUST comply to XX-YYY for 1D, more flexible for 2D
            - TE-001
        iniDate:                                  # iniDate, one entry for each entry of sitename
            - 20220101
        endDate:                                  # endDate, one entry for each entry of sitename
            - 20220228


The block `point` is only relevant for "1D" (i.e. `ftype=1D`) extraction. It defines the list of lat/lon points, the type of surface to specify (original, e.g. same as ERA5, 100% land, 100% lake etc.),
and the "actions" (i.e. the scripts) to execute. The data creation process can be divided in three main steps, by setting `true` or `false` 
the items of the `actions` keyword. The actions are the following:
   extract_initial_conditions) only extract initial conditions and static fields 
   create_initial_conditions) create the netCDF files containing the initial conditions and static fields
to run ecLand. 
   extract_create_forcing) extract the ERA5 forcing fields and create the netCDF files to run ecLand on the selected list of sites.

    point: 
        lat_lon:         # lat/lon pairs, one entry for each entry of sitename, applicable for 1D (site) simulations
            - 50.866,6.447
    
        which_surface: 
            - land       # Values accepted: orig (keep lsm/lake fraction from initial conditions, e.g. era5),
                         #                  land (set 100% land fraction), 
                         #                  lake (set 100% lake fraction)
        actions: # Note order of the following items is important!
            - extract_initial_conditions: true   # Extract initial condition from database (mars or cds)
            - create_initial_conditions: true   # Create initial condition site netcdf
            - extract_create_forcing: true       # Extract and create forcing for site netcdf

The block `regional` is only relevant for `ftype=2D`. It contains information on the 2D domain for which initial conditions and forcing fields will be created.
2D simulations are by default run on a regular lat/lon grid and so only the 4 corners of the lat/lon box and the grid spacing (dx) are required.
The `actions` variable specifies which scripts to execute. The process can be divided in two (or three, if Cama-Flood is requested) steps: 
  prepare_initialconditions_2D) extract and create initial conditions and static field information on the 2D domain 
  prepare_cmf_basin) (optional) extract and create river network map and conditions for cama-flood
  prepare_forcing_2D) extract and create the forcing fields to run ecLand on the domain over the specified time period 

    regional:
        regional: true # true if a regional run, false if global
        dx:     0.25   # grid spacing in degrees
        clatn:  59.75  # lat north corner of the domain
        clats:  34.75  # lat south corner of the domain
        clone:  25.75  # lon east corner of the domain 
        clonw:  -10.75 # lon west corner of the domain
        actions:       # Note order of the following items is important!
            - prepare_initialconditions_2D: true
            - prepare_cmf_basin           : true
            - prepare_forcing_2D          : true

The block `config` contains a list of paths and folders used during the execution of the create_forcing tool. 
It is important to set correctly the `scriptsdir` variable as it defines the path containing the scripts used by the tool.
The variable `retrieve_with` defines if the data should be extracted from MARS or the CDS, set it accordingly. If you use this tool outside ECMWF or you do not have access
to the MARS database, you should get a cds api access token first (more information on [CDS website](https://cds.climate.copernicus.eu/api-how-to))

    config:
        workingdir: /scratch/${USER}/era5_data                # working dir forcing
        inidir:     /scratch/${USER}/era5_data                # working dir initial cond
        cmfdir:     /perm/${USER}/ecland_input/cmf_aux/       # directory with cama-flood auxillary files
                                                              # subfolders are expected to follow this naming convention:
                                                              # <cmf_res>_<dx>_<CLIMVERSION>
                                                              
        sodir:      /perm/${USER}/ecland_input/clim/          # output dir for initial cond and static fields
        fodir:      /perm/${USER}/ecland_input/forcing/       # output dir for forcing
        scriptsdir: /home/paga/projects/ecland_opensource/ecland/tools/create_forcing/scripts/ # Directory with create_forcing scripts
        retrieve_with: mars         


The block `initial_conditions` allows to specify some variables to select what type of data to download.
For users outside of ecmwf, these variables should not be modified.

    initial_conditions:
        INICLASS: "ea"      # the class of the initial condition data
        INISTREAM: oper     # the stream of the initial condition data
        INIEXPVER: 1        # the experiment version of the initial condition data
        INIHOUR: "00:00:00" # the time of the initial condition data
        RESOL: "639"        # the resolution used for the initial condition data and climate data, on the original gaussian grid used by ERA5. For external user only native ERA5 resolution (639l_2 ~ 28km) is available.
        GTYPE: "l_2"        # the type of gaussian grid used by ERA5, "l_2" for linear reduced gaussian grid, 
                            # "_4" for cubic reduced gaussian grid
        ensmem: ""
        CLIMVERSION: "v020"    # the version of the climate data used for the initial condition data. 
                               # ERA5 uses the v015; also available v020 (operational with CY48R1)
        clim_data_loc: "nexus" # the location of the climate data used for the initial condition data. 
                               # external users should set it to "nexus"
                               # emcwf users can set it to internal "ecmwf" path.


The block `forcing` allows to specify some variables to select what type of forcing data to download.
Currently, only ERA5 data can be used either from CDS or MARS archives.
The `nthreads` parameter controls the number of processes to use in the `prepare_forcing_2D` action.
The `lapseCorrection` allows to do a lapse rate correction of air temperature, specific humidity and pressure based on height. 
Only works with grib format at the moment.
It requires the path to a grib file with the orography height difference between ERA5 (interpolated on the regular lat/lon 0.25 grid) 
and target resolution to be provided. The path to this file should be set in `ORODIFF_FILE`.
The `SAVE_FORCING_GRIB` parameter allows to save the forcing downloaded from cds/mars in grib format to `forcingdir` for future use. forcingCommonName is the name of the preProcessed forcing file to read or store, filename convention accepted by the forcing extraction scripts: `<forcingCommonName>_<YYYYMM00>.tar.gz`

    forcing:
        FORCINGCLASS: "ea"                                    # the class of the forcing data, not yet supported
        FORCINGSTREAM: oper                                   # the stream of the forcing data, not yet supported
        FORCINGEXPVER: 1                                      # the experiment version of the forcing data, not yet supported
        FORCINGHOUR: "00:00:00"                               # the time of the forcing data, not yet supported
        lapseCorrection: False                                # Apply lapse rate correction to temperature, Pressure and specific humidity; default:False
                                                              # Require a ORODIFF_FILE to be provided at the target resolution (e.g. dx=0.25 deg)
        ORODIFF_FILE: None                                    # Path to the orography difference file used for lapse rate correction
        nthreads: 2                                           # Number of threads (parallel tasks) used for the forcing calculation. 
        forcingCommonName: "forcing_ea_1_oper_1"              # Name of the forcing file, filename convention accepted by the forcing extraction scripts: <forcingCommonName>_<YYYYMM00>.tar.gz
        forcingdir: ""                                        # Directory with pre-processed forcing (empty if no forcing is available)
                                                              # This is also the directory where forcing will be saved if SAVE_FORCING_GRIB is true
        SAVE_FORCING_GRIB: false                              # Save forcing downloaded from cds/mars in grib format to <forcingdir> for future use
    

LICENCE
=======

(C) Copyright 2024- ECMWF.

This software is licensed under the terms of the Apache Licence Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

In applying this licence, ECMWF does not waive the privileges and immunities granted to it by virtue of its status as an intergovernmental organisation nor does it submit to any jurisdiction. 

