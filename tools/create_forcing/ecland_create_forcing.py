#!/usr/bin/env python3
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


import os
import shutil
import time
import subprocess
from multiprocessing import Pool
from datetime import datetime,timedelta
import argparse
import ecland_create_forcing_utils as utils
import glob

def parse_arguments():
    parser = argparse.ArgumentParser(description='ecland_create_forcing script, add --info for more information')

    # Add a --help argument with a custom help message
    parser.add_argument('--info',  action='store_true', help='print information and requirements and exit')
    parser.add_argument('--clean', action='store_true', help='clean working directory ')
    parser.add_argument('-c', '--config', required=False, help='Path to yaml configuration file')

    return parser.parse_args()

# Main function
def main():
   # Parse the command-line arguments
   args = parse_arguments()
   if args.info:
      utils.print_info()
   else:
      utils.print_info()
      try:
        config_file = args.config
        config_data = utils.load_config(config_file)
      except:
        exit(1)
      print("Current working directory:", os.getcwd())
      config_common, config_point, config_regional, config_init, config_forcing, config_gaussian = utils.process_config_data(config_data)

      print("Settings for initial conditions and forcing extraction.")
      if config_common.ftype == '1D':
        print("Site details:")
        for site,iniDate,endDate,lat,lon in zip(config_common.sitelist,config_common.iniDates,config_common.endDates,config_point.latitudes,config_point.longitudes):
            print(f'Site: {site}, lat:{lat},lon:{lon}, {iniDate} -- {endDate}')
        print("Run extract ini cond script:", config_point.lextract_ini_clim)
        print("Run create ini cond script:", config_point.lcreate_ini_clim)
        print("Run extract and create forcing script:", config_point.lprepare_forcing)
      elif config_common.ftype == '2D':
        print('Region details:')
        print(f'Region: {config_common.sitelist}, lat north:{config_regional.clatn}, lon east:{config_regional.clone}, lat south:{config_regional.clats}, lon west:{config_regional.clonw}, dx={config_regional.dx}')
        print("Run create ini cond script:", config_regional.lprepare_inicond_2D)
        print("Run create cmf basin script:", config_regional.lprepare_cmf_basin)
        print("Run create forcing  script:", config_regional.lprepare_forcing_2D)
      elif config_common.ftype == '2D_GG':
        print('Gaussian Grid details:')
        print(f'Region: {config_common.sitelist}, target grid: {config_gaussian.target_grid}')
        print("Run create ini cond script (Gaussian):", config_gaussian.lprepare_inicond_2D_GG)
        print("Run create forcing script (Gaussian):", config_gaussian.lprepare_forcing_2D_GG)

      print("")
      print("forcing working dir:", config_common.fdir)
      print("input working dir:", config_common.inidir)
      print("output climate dir:", config_common.sodir)
      print("forcing output dir:", config_common.fodir)
      print(f'retrieve from : {config_common.retrieve_with} database')

      # The following are needed by the bash scripts
      first_initial_date = min(config_common.iniDates)
      last_end_date      = max(config_common.endDates)
      if not os.path.exists(config_common.inidir):
        os.makedirs(config_common.inidir)
      if not os.path.exists(config_common.fdir):
        os.makedirs(config_common.fdir)

      if 'point' in config_data:
        utils.create_ascii_files(config_common.sitelist,config_common.iniDates,config_common.endDates,config_common.inidir,config_common.fdir,config_point.latitudes,config_point.longitudes)
      else:
        utils.create_ascii_files(config_common.sitelist,config_common.iniDates,config_common.endDates,config_common.inidir,config_common.fdir)

#---------- Run main shell scripts
      lfail=False
      if config_common.ftype == '1D':

          if config_point.lextract_ini_clim:
              utils.extract_inicond_1D(config_common,config_point,config_init)

      # Create initial conditions
          if config_point.lcreate_ini_clim:
              utils.create_inicond_1D(config_common, config_init, config_point,config_forcing)

      # Prepare Forcing
          if config_point.lprepare_forcing:

             # Paralellise extraction forcing over num_processes processes
             config_common.iniDates_sliced,config_common.endDates_sliced=utils.define_timeSlices(str(first_initial_date),
                                                                                                 str(last_end_date),
                                                                                                 config_forcing.num_processes)

             # Map the function to the iterable (num_processes) to run in parallel, and execute script
             try:
                 with Pool(processes=config_forcing.num_processes) as pool:
                      pool.starmap(utils.prepare_forcing_1D, [(nn,
                                                              config_common,
                                                              config_init,
                                                              config_point,
                                                              config_forcing,
                                                              )
                                                              for nn in
                                                              range(config_forcing.num_processes)])
                      pool.close()
                      pool.join()   # Wait for all subprocesses to complete
             except subprocess.CalledProcessError as e:
                 print("Subprocess failed:", e)
                 pool.terminate()
                 # Raise the exception again to exit the program
                 raise Exception(f"Error running the shell script: {e}")
             # Concatenate forcing:
             concat_forcing_1D_script = f'{config_common.scriptsdir}/concat_forcing.bash {config_common.fdir} {config_common.fodir} {config_common.ftype} '
             subprocess.run(concat_forcing_1D_script, shell=True, check=True)


      elif config_common.ftype == '2D':

          config_regional.nlat_ll  = int((180.0-config_regional.dx)/config_regional.dx +1)
          config_regional.nlon_ll  = int((360.0-config_regional.dx)/config_regional.dx +1)
          if config_regional.lprepare_inicond_2D:

             # Run script
             utils.prepare_inicond_2D(config_common,config_init, config_regional)

             # If cama-flood option is on, create initial conditions for
             # cama-flood configuration
             if config_regional.lprepare_cmf_basin:
                # Compile cython code used by prepare_cmf_basin
                utils.compile_cython_ext(config_common)
                utils.prepare_cmf_basin(config_common,config_init, config_regional)

          # Extract and prepare forcing. This is done in parallel to speed-up
          # download/preprocessing.
          if config_regional.lprepare_forcing_2D:

            # Paralellise extraction forcing over num_processes processes
            config_common.iniDates_sliced,config_common.endDates_sliced=utils.define_timeSlices(str(first_initial_date),
                                                                                                str(last_end_date),
                                                                                                config_forcing.num_processes)

            # Map the function to the iterable (num_processes) to run in parallel, and execute script
            try:
                with Pool(processes=config_forcing.num_processes) as pool:
                     pool.starmap(utils.prepare_forcing_2D, [(nn,
                                                                   config_common,
                                                                   config_init,
                                                                   config_regional,
                                                                   config_forcing,
                                                                  )
                                                                  for nn in
                                                                  range(config_forcing.num_processes)])
                     pool.close()
                     pool.join()   # Wait for all subprocesses to complete
            except subprocess.CalledProcessError as e:
                print("Subprocess failed:", e)
                pool.terminate()
                # Raise the exception again to exit the program
                raise Exception(f"Error running the shell script: {e}")

            # Remove files matching the pattern "prep_forcing_2D_*"
            for file in glob.glob("prep_forcing_2D_*"):
              os.remove(file)

            # Concatenate forcing:
            concat_forcing_2D_script = f'{config_common.scriptsdir}/concat_forcing.bash {config_common.fdir} {config_common.fodir} {config_common.ftype}'
            subprocess.run(concat_forcing_2D_script, shell=True, check=True)

      elif config_common.ftype == '2D_GG':

          if config_gaussian.lprepare_inicond_2D_GG:
             # Run Gaussian grid script
             utils.prepare_inicond_2D_GG(config_common, config_init, config_gaussian)

          # Forcing for Gaussian grid (not yet implemented)
          if config_gaussian.lprepare_forcing_2D_GG:
             print("WARNING: Forcing preparation for Gaussian grid not yet implemented")

      else:
          raise Exception("ecland_create_forcing works only with ftype=1D, 2D, or 2D_GG, please change your configuration file.")

      # Clean temporary files
      if args.clean:
          # deep clean of working dir
          shutil.rmtree(config_common.fdir)
          if (config_common.inidir != config_common.fdir):
            shutil.rmtree(config_common.inidir)
      print('')
      print('Forcing extraction completed successfully.')

#==== End of main script

if __name__ == "__main__":
    main()

