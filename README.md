ecLand
******

The ECMWF surface model ecLand

Introduction
============

The ECMWF land surface model (ecLand) is the land-surface model developed at ECMWF. 
ecLand is a physically-based land-surface model, describing the energy and water movement into the soil column 
and the exchange of fluxes (energy, water, momentum, carbon) with the atmosphere. 
The model can be used fully coupled to the atmospheric model or in stand-alone mode, 
forced at the interface with external atmospheric variables without accounting for feedbacks.
The model includes a number of physical sub-modules aiming at representing the main features of the land-surface.
A full scientific and technical description of ecLand, CY48R1, can be found in
[Boussetta et al. (2021)](https://www.mdpi.com/2073-4433/12/6/723). An up-to-date detailed description of ecLand as used in IFS cycles can be found in Chapter 8 of the [IFS documentation](https://www.ecmwf.int/en/publications/ifs-documentation).


Installing ecLand
================

Supported Platforms
-------------------

- Linux
- Apple MacOS

Other UNIX-like operating systems may work too out of the box.

Requirements
------------

- Fortran and C compiler, and optionally C++ compiler
- open-mpi
- CMake 3.24 or newer (see <https://cmake.org>)
- ecbuild 3.8 or newer (see <https://github.com/ecmwf/ecbuild>)
- fiat (see <https://github.com/ecmwf-ifs/fiat>)
- eccodes (see <https://github.com/ecmwf/eccodes>)
- netcdf-fortran (see <https://github.com/Unidata/netcdf-fortran>)

Some driver scripts to run tests and validate results rely on availability of:

- Python 3.8 or newer

When any of the software dependencies "ecbuild", "fiat", "eccodes" are not found or not installed, they will
be downloaded on demand and built as part of the usual build procedure. This is not recommended but is convenient.
In that case an internet connection is required.

Building ecLand
--------------

#### Quick instructions for ECMWF HPC

Intel build:

    module load prgenv/intel intel/2021.4 cmake/3.25 ninja/1.11.1 hpcx-openmpi/2.9 netcdf4/4.9.1 ecbuild/3.8.0 ecmwf-toolbox/new python3/3.11.8-01

GNU build:

    module load prgenv/gnu gcc/new cmake/3.25 ninja/1.11.1 hpcx-openmpi/2.9 netcdf4/new ecbuild/3.8.0 ecmwf-toolbox/new python3/3.11.8-01

Download from Github:

	git clone git@github.com:ecmwf-ifs/ecland --branch main

Common:
    
    cmake -S ecland -B ecland-build -G Ninja
    srun -c 64 --mem 40g cmake --build ecland-build --parallel 64

#### Quick instructions for MacOS

Relies on already having installed correct versions of cmake, ninja, open-mpi, netcdf, ecbuild, ...

    unzip ecland.zip # if downloaded from the repository
    cmake -S ecland -B ecland-build -G Ninja
    cmake --build ecland-build --parallel 16

#### General instructions

The ecLand build system is based on CMake. If CMake has issues detecting required software dependencies,
following environment variables may be defined:

    export ecbuild_ROOT=<path-to-ecbuild>
    export MPI_HOME=<path-to-MPI>
    export fiat_ROOT=<path-to-fiat>
    export eccodes_ROOT=<path-to-eccodes>
    export NetCDF_Fortran_ROOT=<path-to-netcdf-fortran>
    export CC=<path-to-C-compiler>
    export FC=<path-to-Fortran-compiler>
    export CXX=<path-to-C++-compiler>

The procedure is as follows:

1. Configure ecland:

    cmake -S `<path-to-source>` -B `<path-to-build>`

2. Build ecland:

    cmake --build `<path-to-build>` --parallel `<nthreads>`

3. Install ecland:

    cmake --install `<path-to-build>` --parallel `<nthreads>`

Extra options can be added to the `cmake` command in step 1. to control the build (default in bold)

- `-DCMAKE_BUILD_TYPE=<Debug|**RelWithDebInfo**|Release|Bit>` default=RelWithDebInfo (typically `-O2 -g`)
- `-DENABLE_TESTS=<**ON**|OFF>` : Turn on/off tests
- `-DENABLE_MPI=<**ON**|OFF>` : Turn on/off MPI distributed memory parallelism
- `-DENABLE_OMP=<**ON**|OFF>` : Turn on/off OpenMP threaded parallelism
- `-DENABLE_SINGLE_PRECISION=<ON|**OFF**>` : Turn ON single precision build
- `-DCMAKE_INSTALL_PREFIX=<install-prefix>` : Install location
- `-DFETCHCONTENT_DEPENDENCIES=<ON|OFF>` : Turn on/off download/compilation of dependencies eccodes and/or fiat if not found

More options to control compilation flags, only when defaults are not sufficient

- `-DOpenMP_Fortran_FLAGS=<flags>`
- `-DCMAKE_Fortran_FLAGS=<fortran-flags>`
- `-DCMAKE_C_FLAGS=<c-flags>`

Once this has finished successfully, run `make` and `make install`.

An informational tool `ecland [--help] [--info] [--version] [--git]` is available upon compilation
and can be used the to verify compilation options and version information of ecLand.

Optionally, tests can be run to check succesful compilation, when the feature TESTS is enabled (`-DENABLE_TESTS=ON`, default ON).
In the build folder (e.g. `ecland-build`), run:

    ctest -R ecland [-VV]

Running ecLand
=============

Following are instructions to run ecLand as a standalone tool.

#### Input data

ecLand requires initial condition and static (physiographic) files, meteorological forcing files to run.
These can be prepared using the ecland create forcing tool (see below, "Prepare input data from ERA5") 
provided with this package, 
using the ERA5 data available on the Climate Data Store (CDS) or the ECMWF MARS archive.
Forcing data can be provided from other sources, given that the metadata and file content complies with ecLand IO.
Example data are provided through the get.ecmwf.int (see below, "Insitu and test data" section).

Users are encouraged to look at the [ERA5 documentation page](https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-DataorganisationandhowtodownloadERA5), Table 1, for a description of the static climate field required by ecLand and how to retrieve them 
from the CDS. Additional information can also be found on the [ERA5-Land documentation page](https://confluence.ecmwf.int/display/CKB/ERA5-Land%3A+data+documentation).

#### Run ecLand
Required libraries: python3/3.10.10-01 or newer, netCDF4, openmpi, 

The following are environment variables that might be required to set for running ecLand on external machines: \
`export NETCDF_DIR=` \
`export NETCDF_LIB=` \
`export NETCDF_INCLUDE=` \
`export LD_LIBRARY_PATH=` 

Include path to ecland executable to the list of `PATH`:

    export PATH=/path/to/ecland-install/bin:$PATH

E.g., if using the default installation as described in previous steps:

    export PATH=../ecland-build/bin:$PATH

Command line executable script:

    ecland-run-experiment -g ${GROUP} -t ${FORCING_TYPE} -i ${INPUT_DIR} -o ${OUTPUT_DIR} -w ${WORK_DIR} [-x ${ECLAND_EXE}] [-l ${NLOOP}]
    [-R ${LRESTART}] [-n ${NAMELIST_ECLAND}] [-c ${NAMELIST_CMF}] [-s ${SITENAME}]

Description of options:
- `-g <GROUP>` : string specifying the folder grouping all sites or regions to be run sequentially. It contains all sites or regions belonging to the same project, e.g. if running ESM-SnowMIP sites, they can be contained in the same folder.

- `-t <FORCING_TYPE>` : type of forcing to be used (site, 2D). By default, the following can be used:
    - insitu : point simulation (site), using forcing and initial conditions from in-situ measurements, like observations from fluxnet sites gathered in the [PLUMBER2](https://geonetwork.nci.org.au/geonetwork/srv/eng/catalog.search#/metadata/f7075_4625_2374_0846) projects, or in the [ESM-SnowMIP](https://climate-cryosphere.org/esm-snowmip/#:~:text=ESM%2DSnowMIP%20is%20an%20intercomparison,and%20RCM%20land%20surface%20modules) project. Example data can be downloaded from [get.ecmwf.int](https://get.ecmwf.int/#browse/browse:ecland) .
    - era5 : point simulation (site), using forcing and initial conditions from a reanalysis, e.g. ERA5 and prepared with create forcing tool, or another meteorological model. Example provided for a site of [TERENO network](https://www.tereno.net/joomla/index.php/observatories) on [get.ecmwf.int](https://get.ecmwf.int/#browse/browse:ecland).
    - 2D : regional simulations (2D), using forcing and initial conditions from reanalysis, e.g. ERA5 and prepared with create forcing tool, or another meteorological model.

- `-i <INPUT_DIR>` : path to the directory containing the input files (initial conditions, static and forcing). 
`INPUT_DIR` must follow the following tree structure:
   - `<INPUT_DIR>/clim/<GROUP>/` :  initial conditions, static fields for the sites or region under <GROUP> to be run
   - `<INPUT_DIR>/forcing/<GROUP/` : forcing files for the sites or region under `GROUP` to be run
- `-o <OUTPUT_DIR>` : path to the directory where output files of the simulations will be stored.
- `-w <WORK_DIR>` : path to the working directory where simulations will be executed.

Optional arguments:

- `-x <ECLAND_EXE>` : (optional) ecland executable path, default assumes `ECLAND_EXE=../ecland-build/bin/ecland-master`
- `-s <SITENAME>`     : (optional) single site or "region" to run among the ones in the `${GROUP}` folder
- `-n <NAMELIST>`     : (optional) namelist file used for ecLand (default is located in `namelists/namelist_ecland_48R1`)
- `-c <NAMELIST_CMF>` : (optional) namelist file used for cama-flood (default is located in `namelists/namelist_cmf_48R1`);
note that cama-flood activation is controlled by ecLand namelist parameter: `LECMF1WAY=.T.`
- `-l <NLOOP>` : (optional) Number of "loops" to do for spinup: the entire period for each site will be run NLOOP times;
the last step of each loop is used as initial conditions for the following loop (default is 1)
- `-R <LRESTART>` : (optional) true for a restart run. Path to the (restart) initial conditions should be provided setting 
the following environment variables:
    - `export RESTARTECLAND=/path/to/restart_files_for_ecLand`
    - (if Cama-flood is active)  `export RESTARTCMF=/path/to/restart_files_for_cmf`

#### Running with MPI

The arguments `-np <NSTASKS>` and `-nt <NTHREADS>` can be passed to the `ecland-run-test` and `ecland-run-experiment` scripts to run with MPI
and OpenMP. This uses the internal `ecland-launch` "smart" launcher that chooses a good launcher depending on availability and the used platform.
It will also set export OMP_NUM_THREADS=<NTHREADS> for you. The unit-tests are automatically configured to use this when invoked via ctest.

A custom launch command for greater control can be constructed using the `--launch` argument, e.g. --launch="srun -n <NTASKS> -c <NTHREADS>".
Note that this does not automatically export the OMP_NUM_THREADS variable.



#### Example script for ECMWF HPC and intel installation, single point (1D) on ESM-SnowMIP data,
This example only performs 1 loop (default) on all sites in the ESM-SnowMIP group folder.
It uses the default namelist file that is contained in namelists/namelist_ecland_48R1 .

    # Assuming data have been already downloaded from get.ecmwf.int
    module load prgenv/intel intel/2021.4 python3/3.10.10-01
    module load hpcx-openmpi/2.9 netcdf4/4.9.1

    GROUP=ESM-SnowMIP
    FORCING_TYPE=insitu
    INPUT_DIR=/perm/${USER}/ecland_input/
    OUTPUT_DIR=/scratch/${USER}/
    WORK_DIR=./work/
    export PATH=${ecland_ROOT:-../ecland-build}/bin:$PATH

    ecland-run-experiment -g ${GROUP}\
                          -t ${FORCING_TYPE}\
                          -i ${INPUT_DIR}\
                          -o ${OUTPUT_DIR}\
                          -w ${WORK_DIR}

#### Example script for ECMWF HPC and intel installation, single point (1D) for a TERENO point
This example performs 3 loops on a point. Forcing and initial conditions were extracted from ERA5 at the nearest gridpoint to a TERENO site.
A run for a single site could be specified by adding the option `-s <sitename>` to the option list of the call to ecland-run-experiment script.
It uses the default namelist file that is contained in namelists/namelist_ecland_48R1 .


    # Assuming data have been already downloaded from get.ecmwf.int
    module load prgenv/intel intel/2021.4 python3/3.10.10-01
    module load hpcx-openmpi/2.9 netcdf4/4.9.1

    GROUP=TERENO
    SITE="TE-001_2022-2022"
    FORCING_TYPE=era5
    INPUT_DIR=/perm/${USER}/ecland_input/
    OUTPUT_DIR=/scratch/${USER}/
    WORK_DIR=./work/
    export PATH=${ecland_ROOT:-../ecland-build}/bin:$PATH
    NLOOP=3

    ecland-run-experiment -g ${GROUP}\
                          -s ${SITE}\
                          -t ${FORCING_TYPE}\
                          -i ${INPUT_DIR}\
                          -o ${OUTPUT_DIR}\
                          -l ${NLOOP}\
                          -w ${WORK_DIR}



#### Example script for ECMWF HPC and intel installation, regional (2D)
This example runs the "EU-001_2022-2022" domain that has been created with the create_forcing tool.
It runs 2 loops and uses 2 MPI tasks and 8 openMP threads. It uses the default namelist file contained in namelists folder.

    # Assuming data have been created with the create_forcing tool
    module load prgenv/intel intel/2021.4 python3/3.10.10-01
    module load hpcx-openmpi/2.9 netcdf4/4.9.1

    GROUP=TEST_2D
    DOMAIN=EU-001_2022-2022
    FORCING_TYPE=2D
    INPUT_DIR=/perm/${USER}/ecland_input/
    OUTPUT_DIR=/scratch/${USER}/${GROUP}
    WORK_DIR=/scratch/paga/work/
    NAMELIST_FILE="./namelists/namelist_ecland_48R1" 
    NLOOP=2
    export PATH=${ecland_ROOT:-../ecland-build}/bin:$PATH
    export NPROC=2
    export NTHREADS=8
    
    ecland-run-experiment -g ${GROUP}\
                          -t ${FORCING_TYPE}\
                          -i ${INPUT_DIR}\
                          -o ${OUTPUT_DIR}\
                          -n ${NAMELIST_FILE}\
                          -l ${NLOOP}\
                          -s ${DOMAIN}\
                          -w ${WORK_DIR}


#### Namelist file
The namelist file (default located in: `namelists/namelist_ecland_48R1`) controls the execution of EcLand simulations.
Some parameters (e.g. length of the simulation, time step etc.) are set using the script `ecland_create_namelist.py`.
These parameters can be identified by looking at the parameters between curly brackets `{}` in the namelist file.

`ecland_create_namelist.py` set these parameters based on the information in the initial and boundary conditions and in the forcing files provided.

The physics options, the output of more variables in netCDF and other settings can be accessed by modifying the namelist file provided.

#### Running with Cama-Flood river routing
By default the routing of runoff using Cama-Flood is turned off. 
This requires input data that can be accessed from the Cama-flood developers' page at this 
[website](http://hydro.iis.u-tokyo.ac.jp/~yamadai/cama-flood/).
This data can be processed on a regional domain and in the right formats using the tools in `tools/create_forcing`.


Tutorial
============

A Jupyter notebook is provided in `tutorials/` to give a simple example on running ecLand and working with 
the input and forcing files. To run the examples in the jupyter notebook `ecland_practicals.ipynb`, it is assumed
that ecLand has been already correctly installed and tests successfully passed (see previous steps).

The `ecland_run_practicals.sh`

The folder `tutorials` also contains example data to run the model on a case-study, a script to run the simulations and plotting scripts to visualise
the results of the simulations. The notebook contains examples of usage of all the different tools.

Initial conditions and forcing data
============

Initial conditions, static fields and forcing must be contained in folders following a structure.  Assuming `INPUT_DIR` is the parent folder:

- `<INPUT_DIR>/clim/<GROUP>/` :  initial conditions, static fields for the sites or region for a specific `GROUP`. 
- `<INPUT_DIR>/forcing/<GROUP/` : forcing files for the sites or region for a specific `GROUP`.

For site simulations, the netCDF file containing the static "climate" fields (land-sea mask, lake mask etc.) should follow the following naming convention:
    
    surfclim_<SITENAME>.nc

with `SITENAME` having the following structure: `<XY>-<ABC>_<iniYear>-<endYear>.nc` (even if simulation is shorter than 1 year). In this convention:

- `XY` represents a placeholder for an alphanumeric identifier. This can consist of either alphabetical characters, numerical characters, or a combination of both. Example: IT, US, 123, AB1.
- `ABC` represents another alphanumeric identifier. This can also be a sequence of alphabetical characters, numerical characters, or a combination of both. Example: SRo, 001, A1B.

netCDF files containing the initial conditions (soil temperature, soil moisture, snow temperature etc.) follow the convention:

    surfinit_<SITENAME>.nc

netCDF files containing the forcing fields (wind components or wind speed, near-surface air temperature and humidity etc.) follow the convention:

    met_<FORCING_TYPE>HT_<SITENAME>.nc

with <FORCING_TYPE> being:

- insitu : point simulation (site), using forcing and initial conditions from in-situ measurements, like observations from fluxnet sites gathered in the [PLUMBER2](https://geonetwork.nci.org.au/geonetwork/srv/eng/catalog.search#/metadata/f7075_4625_2374_0846) projects, or in the [ESM-SnowMIP](https://climate-cryosphere.org/esm-snowmip/#:~:text=ESM%2DSnowMIP%20is%20an%20intercomparison,and%20RCM%20land%20surface%20modules) project. Example data can be downloaded from [get.ecmwf.int](https://get.ecmwf.int/#browse/browse:ecland) .
- era5 : point simulation (site), using forcing and initial conditions from a reanalysis, e.g. ERA5 and prepared with create forcing tool, or another meteorological model. Example provided for a site of [TERENO network](https://www.tereno.net/joomla/index.php/observatories) on [get.ecmwf.int](https://get.ecmwf.int/#browse/browse:ecland).
- 2D : regional simulations (2D), using forcing and initial conditions from reanalysis, e.g. ERA5 and prepared with create forcing tool, or another meteorological model.


The list of variables contained in each of these files to run ecLand can be accessed by inspecting the test data provided (see below) using tools like `ncdump -h <filename>`.


Creation of initial conditions and forcing from ERA5 
============

A tool is provided, in `tools/create_forcing`, to create input and forcing files for ecLand both in "site" (points) and "2D" (regional) configuration.

A separated `Readme` file with a description of the tool and instructions to use it can be found in `tools/create_forcing`.

Insitu data and test data 
============

"In-situ" forcing are forcing data that uses meteorological measurements from insitu stations. This type of data are widely used for forcing land surface
models, as it reduces the impact of biases in the meteorological forcing on the land component under study (e.g. soil moisture, latent heat, snow depth etc.). This allows
to evaluate the response of the land surface parametrizations under "perfect" conditions.

Popular in-situ forcing dataset include "ESM-SnowMIP" for snow processes and "FLUXNET2015" for fluxes and warm processes. 
Further information on FLUXNET2015 data is reported in [Pastorello et al. 2020](https://www.nature.com/articles/s41597-020-0534-3) and data can be accessed from this [website](https://geonetwork.nci.org.au/geonetwork/srv/eng/catalog.search#/metadata/f7075_4625_2374_0846).
Further information on ESM-SnowMIP data is reported in [Menard et al. 2019](https://essd.copernicus.org/articles/11/865/2019/) and data can be accessed from this [website](https://catalogue.ceda.ac.uk/uuid/b6b6266415634a6ab242702d7ee6be05).

A script to download initial condition and forcing data for the ESM-SnowMIP sites is provided:
  -  `GROUP=ESM-SnowMIP` , with `FORCING_TYPE=insitu`
  
An example of forcing data created with the `create_forcing` tool is also provided:
  - `GROUP=TERENO` , with `FORCING_TYPE=era5`    

To download the data, navigate to `tools/retrieve_data` folder, and use the following command:

    ./retrieve_sites.bash -g <GROUP> -i <OUT_DIR> [-p <path_to_scripts>]

The test data used to perform the `ctest` can be used as a template for custom forcing/initial conditions. Assuming the ctests have been run successfully under `ecland-build`, 
for a specific test the data can be found in: 

    ecland-build/tests/<specific_test>/input

Known issues
============

If the following error message is reported: 
    `perl: warning: Falling back to a fallback locale ("en_US.UTF-8")"`

Export the following environment variables in the running terminal:

    export LC_CTYPE=en_US.UTF-8
    export LC_ALL=en_US.UTF-8

Reporting Bugs
==============

Please report bugs using a [GitHub issue](https://github.com/ecmwf-ifs/ecland/issues).
Support is given on a best-effort basis by package developers.

LICENCE
=======

(C) Copyright 2024- ECMWF.

This software is licensed under the terms of the Apache Licence Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

In applying this licence, ECMWF does not waive the privileges and immunities granted to it by virtue of its status as an intergovernmental organisation nor does it submit to any jurisdiction. 

The ESM-SnowMIP data distributed were based on [Menard, C.; Essery, R. (2020): ESM-SnowMIP meteorological and evaluation datasets at ten reference sites (in situ and bias corrected reanalysis data). Centre for Environmental Data Analysis, 2024-03-20](https://catalogue.ceda.ac.uk/uuid/b6b6266415634a6ab242702d7ee6be05); data were further processed to enable the usage in ecLand.

All data products are distributed under a Creative Commons Attribution 4.0 International (CC BY 4.0). 
To view a copy of this licence, visit https://creativecommons.org/licenses/by/4.0/

See the `LICENSE` file for details on the license and attribution requirements.


Contributing
============

Contributions to ecLand are welcome.
In order to do so, please open a [GitHub issue](https://github.com/ecmwf-ifs/ecland/issues) where
a feature request or bug can be discussed.

Then create a [pull request](https://github.com/ecmwf-ifs/ecland/pulls) with your contribution.

All contributors to the pull request need to sign the
[contributors license agreement (CLA)](https://bol-claassistant.ecmwf.int/ecmwf-ifs/ecland).
