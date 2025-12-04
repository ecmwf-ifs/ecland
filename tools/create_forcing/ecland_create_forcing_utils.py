# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# This module contains methods for running the ecland_create_forcing project.

import yaml
import os
import subprocess
import argparse
import shutil
import time
import subprocess
from multiprocessing import Pool
from datetime import datetime,timedelta

class ConfigCommon:
  """
  A class that represents common configuration settings.

  Attributes:
    ftype (str): The type of initial conditions and forcing to create, point or 2D
    sitelist (list): The list of site names.
    iniDates (list): The list of initial dates.
    endDates (list): The list of end dates.
    group (str): The group name.
    preProcForcDir (str): The directory for pre-processed forcing data.
    fdir (str): The forcing working directory.
    inidir (str): The climate (init conditions) working directory.
    sodir (str): The directory for climate (init conditions) output.
    fodir (str): The directory for forcing output.
    scriptsdir (str): The directory for scripts.
    retrieve_with (str): The retrieval method.
    cmfdir (str): The directory for CMF data (optional).
  """

  def __init__(self, config_data):
    self.ftype = config_data['sites'].get('ftype', '')
    self.sitelist = config_data['sites'].get('sitenames', [])
    self.iniDates = config_data['sites'].get('iniDate', [])
    self.endDates = config_data['sites'].get('endDate', [])
    self.group = config_data['sites'].get('group', '')
    # Folders and paths
    self.fdir = os.path.abspath(config_data['config'].get('workingdir', './work/'))
    self.inidir = os.path.abspath(config_data['config'].get('inidir', './work/'))
    self.sodir = os.path.abspath(config_data['config'].get('sodir', './ecland_input/clim') + "/" + self.group)
    self.fodir = os.path.abspath(config_data['config'].get('fodir', './ecland_input/forcing') + "/" + self.group)
    self.scriptsdir = os.path.abspath(config_data['config'].get('scriptsdir', './scripts/'))
    self.retrieve_with = config_data['config'].get('retrieve_with', 'cds')
    self.endDates_sliced=None
    self.iniDates_sliced=None
    self.METVIEW_PYTHON_START_TIMEOUT=config_data['config'].get('METVIEW_PYTHON_START_TIMEOUT',None)
    if 'regional' in config_data:
      self.cmfdir = os.path.abspath(config_data['config'].get('cmfdir','./ecland_input/cmf_aux/'))
    else:
      self.cmfdir = None


class ConfigPoint:
  """
  Represents a configuration point.

  Attributes:
    lats_lons (list): List of latitude and longitude pairs (string).
    latitudes (list): List of latitudes (float).
    longitudes (list): List of longitudes (float).
    which_surface (str): Surface type information.
    lextract_ini_clim (bool): Flag indicating whether to extract initial conditions.
    lcreate_ini_clim (bool): Flag indicating whether to create initial conditions.
    lprepare_forcing (bool): Flag indicating whether to extract and create forcing.
  """

  def __init__(self, config_data):
    self.lats_lons = config_data.get('point', {}).get('lat_lon', [])
    self.latitudes, self.longitudes = read_latlon(self.lats_lons)
    self.which_surface = config_data['point']['which_surface'][0]
    self.lextract_ini_clim = config_data['point']['actions'][0].get('extract_initial_conditions', False)
    self.lcreate_ini_clim = config_data['point']['actions'][1].get('create_initial_conditions', False)
    self.lprepare_forcing = config_data['point']['actions'][2].get('extract_create_forcing', False)

class ConfigRegional:
  """
  Class representing regional configuration settings.

  Attributes:
    regional (str): The regional setting.
    dx (float): The dx (resolution in degrees) value.
    clatn (float): The clatn value.
    clats (float): The clats value.
    clone (float): The clone value.
    clonw (float): The clonw value.
    nlat_ll (int): number of latitude values in the lat/lon box
    nlon_ll (int): number of longitude values in the lat/lon box
    cmf_res (float): resolution of cama-flood in glob_XXmin format with XX the resolution in minutes.
    lprepare_inicond_2D (bool): prepare initial conditions and boundary conditions for 2D runs.
    lprepare_cmf_basin (bool): prepare cama-flood river routing initial and boundary conditions for 2D runs.
    lprepare_forcing_2D (bool): prepare forcing for 2D processing.
  """

  def __init__(self, config_data):
    self.regional = config_data['regional'].get('regional', "")
    self.dx       = config_data['regional'].get('dx', "")
    self.clatn    = config_data['regional'].get('clatn', "")
    self.clats    = config_data['regional'].get('clats', "")
    self.clone    = config_data['regional'].get('clone', "")
    self.clonw    = config_data['regional'].get('clonw', "")
    self.cmf_res  = config_data['regional'].get('cmf_res', "")
    self.lprepare_inicond_2D = config_data['regional'].get('actions', [{}])[0].get('prepare_initialconditions_2D', False)
    self.lprepare_cmf_basin  = config_data['regional'].get('actions', [{}])[1].get('prepare_cmf_basin', False)
    self.lprepare_forcing_2D = config_data['regional'].get('actions', [{}])[2].get('prepare_forcing_2D', False)
    self.nlat_ll = None
    self.nlon_ll = None


class ConfigGaussian:
  """
  Class representing Gaussian grid configuration settings for 2D_GG runs.

  Attributes:
    grid_type (str): "N" for TL (linear), "O" for TCO (cubic octahedral)
    truncation (int): Truncation number (e.g., 32 for N32, 64 for N64)
    target_grid (str): Combined grid specification (e.g., "N32", "O96")
    clatn, clats, clone, clonw (float): Optional regional clipping bounds
    lprepare_inicond_2D_GG (bool): Prepare initial conditions on Gaussian grid
    lprepare_forcing_2D_GG (bool): Prepare forcing on Gaussian grid
  """

  def __init__(self, config_data):
    self.grid_type   = config_data['gaussian'].get('grid_type', 'N')
    self.truncation  = config_data['gaussian'].get('truncation', 32)
    self.target_grid = f"{self.grid_type}{self.truncation}"
    # Optional regional clipping
    self.clatn = config_data['gaussian'].get('clatn', "")
    self.clats = config_data['gaussian'].get('clats', "")
    self.clone = config_data['gaussian'].get('clone', "")
    self.clonw = config_data['gaussian'].get('clonw', "")
    # Actions
    actions = config_data['gaussian'].get('actions', [{}])
    self.lprepare_inicond_2D_GG = actions[0].get('prepare_initialconditions_2D_GG', False) if len(actions) > 0 else False
    self.lprepare_forcing_2D_GG = actions[1].get('prepare_forcing_2D_GG', False) if len(actions) > 1 else False


class ConfigInit:
  """
  A class that initializes the configuration data for creating forcing files.

  Args:
    config_data (dict): A dictionary containing the configuration data.

  Attributes:
    iniclass (str): The value of 'INICLASS' from the config_data.
    inistream (str): The value of 'INISTREAM' from the config_data.
    iniexpver (str): The value of 'INIEXPVER' from the config_data.
    inihour (str): The value of 'INIHOUR' from the config_data.
    resol (str): The value of 'RESOL' from the config_data.
    gtype (str): The value of 'GTYPE' from the config_data.
    ensmem (str): The value of 'ensmem' from the config_data.
    climv (str): The value of 'CLIMVERSION' from the config_data.
    clim_loc (str): The value of 'clim_data_loc' from the config_data.
    xdata_dir (str): The directory path for the climate data.

  """
  def __init__(self, config_data):
    self.iniclass = config_data['initial_conditions'].get('INICLASS', 'ea')
    self.inistream = config_data['initial_conditions'].get('INISTREAM', 'oper')
    self.iniexpver = str(config_data['initial_conditions'].get('INIEXPVER', '1'))
    self.inihour = str(config_data['initial_conditions'].get('INIHOUR', '00:00:00'))
    self.resol = str(config_data['initial_conditions'].get('RESOL', '639'))
    self.gtype = config_data['initial_conditions'].get('GTYPE', 'l_2')
    self.ensmem = config_data['initial_conditions'].get('ensmem', "")
    self.climv = config_data['initial_conditions'].get('CLIMVERSION', 'v015')
    self.clim_loc = config_data['initial_conditions'].get('clim_data_loc', 'nexus')
    if self.clim_loc == 'ecmwf':
      self.xdata_dir = f"/home/rdx/data/climate/climate.{self.climv}/{self.resol}{self.gtype}"
    else:
      self.xdata_dir = f"cds"

class ConfigForcing:
  """
  A class representing the configuration for forcing data.

  Attributes:
    forcingClass (str): The forcing class to be extracted (ea by default).
    num_processes (int): The number of processes to parallelise the forcing extraction.
    lapseCorr (str): true/false to apply lapse rate correction
    oroDiff_file (str): path to the file containing the orography difference used to compute lapse rate correction.
    forcingSaveDir (str): The directory to save the forcing data in grib format in a tar.gz file.
    forcingCommonName (str): The common name for the forcing data.
  """

  def __init__(self, config_data):
    self.forcingClass      = config_data['forcing'].get('FORCINGCLASS', 'ea')
    self.forcingStream     = config_data['forcing'].get('FORCINGSTREAM', 'oper')
    self.forcingExpver     = config_data['forcing'].get('FORCINGEXPVER', '1')
    self.forcingHour       = config_data['forcing'].get('FORCINGHOUR', '00:00:00')
    self.preProcForcDir    = config_data['forcing'].get('forcingdir', '')
    self.SAVE_FORCING_GRIB = config_data['forcing'].get('SAVE_FORCING_GRIB', False)
    self.forcingCommonName = config_data['forcing'].get('forcingCommonName', '')
    self.num_processes = int(config_data['forcing'].get('nthreads', 1))
    self.lapseCorr = config_data['forcing'].get('lapseCorrection', False)
    self.oroDiff_file = config_data['forcing'].get('ORODIFF_FILE', 'default_oro_diff_file')


def process_config_data(config_data):
  # Access specific configuration values
  config_forcing=dict()
  # Common information
  config_common = ConfigCommon(config_data)

  # Only relevant for point (1D) extraction
  if 'point' in config_data:
    config_point = ConfigPoint(config_data)
  else:
    config_point = None

  if 'regional' in config_data:
    config_regional = ConfigRegional(config_data)
  else:
    config_regional = None

  # Gaussian grid configuration (for 2D_GG type)
  if 'gaussian' in config_data:
    config_gaussian = ConfigGaussian(config_data)
  else:
    config_gaussian = None

  # setup var for init data:
  config_init = ConfigInit(config_data)

  # Setup variable for forcing extraction:
  config_forcing = ConfigForcing(config_data)

  # Return the processed configuration data
  return (config_common, config_point, config_regional, config_init, config_forcing, config_gaussian)



def extract_inicond_1D(config_common, config_point, config_init):
  """
  Extracts initial conditions for 1D simulation.

  Args:
    config_common (object): The configuration object containing common settings.
    config_point (object): The configuration object containing point-specific settings.
    config_init (object): The configuration object containing initialization settings.

  Raises:
    Exception: If there is an error running the shell script.

  """
  # Export required variables
  os.environ['inidir']        = config_common.inidir
  os.environ['INISTREAM']     = config_init.inistream
  os.environ['INIEXPVER']     = config_init.iniexpver
  os.environ['INIHOUR']       = f'{config_init.inihour}'
  os.environ['RESOL']         = f'{config_init.resol}'
  os.environ['GTYPE']         = f'{config_init.gtype}'
  os.environ['XDATA_DIR']     = config_init.xdata_dir
  os.environ['INICLASS']      = config_init.iniclass
  os.environ['ensmem']        = config_init.ensmem
  if config_common.METVIEW_PYTHON_START_TIMEOUT is not None:
     os.environ['METVIEW_PYTHON_START_TIMEOUT'] = f'{config_common.METVIEW_PYTHON_START_TIMEOUT}'
  else:
     os.environ['METVIEW_PYTHON_START_TIMEOUT'] = str(3000)
  os.environ['RETRIEVE_WITH'] = config_common.retrieve_with
  os.environ['TMPDIR']        = config_common.inidir
  os.environ['which_surface'] = config_point.which_surface
  os.environ['which_surface'] = config_point.which_surface

  extract_inicond_script = f'{config_common.scriptsdir}/extract_site_clim.bash {config_common.scriptsdir}'
  try:
    subprocess.run(extract_inicond_script, shell=True, check=True)
  except subprocess.CalledProcessError as e:
    print(f"Error running the shell script: {e}")
    raise Exception(f"Error running the shell script: {e}")


def create_inicond_1D(config_common, config_init, config_point, config_forcing):
  """
  Creates initial conditions for 1D simulation.

  Args:
    config_common (object): The configuration object containing common settings.
    config_init (object): The configuration object containing initialization settings.
    config_point (object): The configuration object containing point settings.
    config_forcing (object): The configuration object containing forcing settings.

  Raises:
    Exception: If there is an error running the shell script.

  Returns:
    None
  """

  # Export required variables
  os.environ['inidir'] = config_common.inidir
  os.environ['INISTREAM'] = config_init.inistream
  os.environ['INIEXPVER'] = config_init.iniexpver
  os.environ['INIHOUR'] = f'{config_init.inihour}'
  os.environ['RESOL'] = f'{config_init.resol}'
  os.environ['GTYPE'] = f'{config_init.gtype}'
  os.environ['XDATA_DIR'] = config_init.xdata_dir
  os.environ['INICLASS'] = config_init.iniclass
  os.environ['ensmem'] = config_init.ensmem
  if config_common.METVIEW_PYTHON_START_TIMEOUT is not None:
    os.environ['METVIEW_PYTHON_START_TIMEOUT'] = f'{config_common.METVIEW_PYTHON_START_TIMEOUT}'
  else:
    os.environ['METVIEW_PYTHON_START_TIMEOUT'] = str(3000)
  os.environ['RETRIEVE_WITH'] = config_common.retrieve_with
  os.environ['TMPDIR'] = config_common.inidir
  os.environ['preProcForcdir'] = config_forcing.preProcForcDir
  os.environ['which_surface'] = config_point.which_surface

  create_inicond_script = f'{config_common.scriptsdir}/create_sites.bash {config_common.sodir} {config_common.scriptsdir}'
  try:
    subprocess.run(create_inicond_script, shell=True, check=True)
  except subprocess.CalledProcessError as e:
    print(f"Error running the shell script: {e}")
    raise Exception(f"Error running the shell script: {e}")


def prepare_forcing_1D(nn, config_common, config_init, config_point,config_forcing):
  """
  Prepare forcing data for 1D land surface model.

  Args:
   nn (int): The index of the process.
   config_common (object): The configuration object for common settings.
   config_init (object): The configuration object for initialization settings.
   config_point (object): The configuration object for point settings.
   config_forcing (object): The configuration object for forcing settings.

  Raises:
   Exception: If there is an error running the shell script.

  """
  # Export required variables
  os.environ['RETRIEVE_WITH']    = config_common.retrieve_with
  os.environ['TMPDIR']           = config_common.inidir
  os.environ['forcingdir']       = config_common.fdir
  os.environ['preProcForcdir']   = config_forcing.preProcForcDir
  os.environ['XDATA_DIR']        = config_init.xdata_dir
  os.environ['XDATA_DIR']        = config_init.xdata_dir
  os.environ['RESOL']            = f'{config_init.resol}'
  os.environ['GTYPE']            = f'{config_init.gtype}'
  os.environ['FORCINGCLASS']     = f'{config_forcing.forcingClass}'
  os.environ['FORCINGSTREAM']    = f'{config_forcing.forcingStream}'
  if config_common.METVIEW_PYTHON_START_TIMEOUT is not None:
    os.environ['METVIEW_PYTHON_START_TIMEOUT'] = f'{config_common.METVIEW_PYTHON_START_TIMEOUT}'
  else:
    os.environ['METVIEW_PYTHON_START_TIMEOUT'] = str(3000)
  if config_forcing.SAVE_FORCING_GRIB  is not None:
    os.environ['SAVE_FORCING_GRIB'] = f'{str(config_forcing.SAVE_FORCING_GRIB).lower()}'
  else:
    os.environ['SAVE_FORCING_GRIB'] = str(False)
  if config_forcing.forcingCommonName  is not None:
    os.environ['forcingCommonName'] = f'{config_forcing.forcingCommonName}'
  os.environ['scriptsdir'] = config_common.scriptsdir

  initial_date = config_common.iniDates_sliced[nn]
  end_date = config_common.endDates_sliced[nn]

  print(f'running process: {nn}, dates: {initial_date}-{end_date}')
  prepare_forcing_script = f'{config_common.scriptsdir}/extract_create_forcing.bash {initial_date} {end_date} {config_common.fodir} {config_common.scriptsdir}'
  logfile_name = f'{config_common.fdir}/prep_forcing_1D_{nn}.log'
  subprocess.run(f'rm -f {logfile_name}', shell=True, check=True)
  try:
   with open(logfile_name, 'w') as logfile:
    subprocess.run(prepare_forcing_script, shell=True, check=True,
          stdout=logfile, stderr=subprocess.STDOUT)
  except subprocess.CalledProcessError as e:
   print(f"Error running the shell script: {e}")
   raise Exception(f"Error running the shell script: {e}")


def prepare_inicond_2D(config_common, config_init, config_regional):
  """
  Prepare ECLand surface model initial and boundary conditions for 2D (regional) case.

  Args:
    config_common (object): The common configuration object.
    config_init (object): The initialization configuration object.
    config_regional (object): The regional configuration object.

  Raises:
    Exception: If there is an error running the shell script.

  """
  # Export variables:
  os.environ['inidir']        = config_common.inidir
  os.environ['RETRIEVE_WITH'] = config_common.retrieve_with
  os.environ['INISTREAM']     = config_init.inistream
  os.environ['INIEXPVER']     = config_init.iniexpver
  os.environ['INIHOUR']       = f'{config_init.inihour}'
  os.environ['RESOL']         = f'{config_init.resol}'
  os.environ['GTYPE']         = f'{config_init.gtype}'
  os.environ['XDATA_DIR']     = config_init.xdata_dir
  os.environ['INICLASS']      = config_init.iniclass
  os.environ['regional']      = f'{str(config_regional.regional).lower()}'
  os.environ['dx']            = f'{config_regional.dx:.2f}'
  os.environ['clatn']         = f'{config_regional.clatn:.2f}'
  os.environ['clats']         = f'{config_regional.clats:.2f}'
  os.environ['clone']         = f'{config_regional.clone:.2f}'
  os.environ['clonw']         = f'{config_regional.clonw:.2f}'
  os.environ['nlat_ll']       = f'{config_regional.nlat_ll}'
  os.environ['nlon_ll']       = f'{config_regional.nlon_ll}'
  os.environ['scriptsdir']    = f'{config_common.scriptsdir}'
  os.environ['RUN_CMF']       = str(config_regional.lprepare_cmf_basin).lower()
  if config_common.METVIEW_PYTHON_START_TIMEOUT is not None:
     os.environ['METVIEW_PYTHON_START_TIMEOUT'] = f'{config_common.METVIEW_PYTHON_START_TIMEOUT}'
  else:
     os.environ['METVIEW_PYTHON_START_TIMEOUT'] = str(3000)

  out_clim_dir=config_common.sodir
  prepare_inicond_2D_script=f'{config_common.scriptsdir}/extract_create_inicond_2D.bash {out_clim_dir}'
  try:
      subprocess.run(prepare_inicond_2D_script, shell=True, check=True)
  except subprocess.CalledProcessError as e:
      print(f"Error running the shell script: {e}")
      raise Exception(f"Error running the shell script: {e}")


def prepare_inicond_2D_GG(config_common, config_init, config_gaussian):
  """
  Prepare ECLand surface model initial and boundary conditions for 2D Gaussian grid case.

  Args:
    config_common (object): The common configuration object.
    config_init (object): The initialization configuration object.
    config_gaussian (object): The Gaussian grid configuration object.

  Raises:
    Exception: If there is an error running the shell script.

  """
  # Export variables:
  os.environ['inidir']        = config_common.inidir
  os.environ['RETRIEVE_WITH'] = config_common.retrieve_with
  os.environ['INISTREAM']     = config_init.inistream
  os.environ['INIEXPVER']     = config_init.iniexpver
  os.environ['INIHOUR']       = f'{config_init.inihour}'
  os.environ['RESOL']         = f'{config_init.resol}'
  os.environ['GTYPE']         = f'{config_init.gtype}'
  os.environ['XDATA_DIR']     = config_init.xdata_dir
  os.environ['INICLASS']      = config_init.iniclass
  os.environ['scriptsdir']    = f'{config_common.scriptsdir}'
  os.environ['TARGET_GAUSSIAN_GRID'] = config_gaussian.target_grid
  if config_common.METVIEW_PYTHON_START_TIMEOUT is not None:
     os.environ['METVIEW_PYTHON_START_TIMEOUT'] = f'{config_common.METVIEW_PYTHON_START_TIMEOUT}'
  else:
     os.environ['METVIEW_PYTHON_START_TIMEOUT'] = str(3000)

  out_clim_dir=config_common.sodir
  prepare_inicond_2D_GG_script=f'{config_common.scriptsdir}/extract_create_inicond_2D_GG.bash {out_clim_dir}'
  try:
      subprocess.run(prepare_inicond_2D_GG_script, shell=True, check=True)
  except subprocess.CalledProcessError as e:
      print(f"Error running the shell script: {e}")
      raise Exception(f"Error running the shell script: {e}")


# Function to run preparation of cama-flood initial and boundary conditions.
def prepare_cmf_basin(config_common, config_init, config_regional):
  """
  Prepare cama-flood river routing initial and boundary conditions for 2D runs.

  Args:
   config_common (object): The common configuration object.
   config_init (object): The initialization configuration object.
   config_regional (object): The regional configuration object.

  Raises:
   Exception: If there is an error running the shell script.

  """

  out_cmf_dir=config_common.sodir
  # Export variables:
  os.environ['inidir']        = config_common.inidir
  os.environ['RETRIEVE_WITH'] = config_common.retrieve_with
  os.environ['INISTREAM']     = config_init.inistream
  os.environ['INIEXPVER']     = config_init.iniexpver
  os.environ['INIHOUR']       = f'{config_init.inihour}'
  os.environ['RESOL']         = f'{config_init.resol}'
  os.environ['GTYPE']         = f'{config_init.gtype}'
  os.environ['XDATA_DIR']     = config_init.xdata_dir
  os.environ['INICLASS']      = config_init.iniclass
  os.environ['regional']      = f'{str(config_regional.regional).lower()}'
  os.environ['dx']            = f'{config_regional.dx:.2f}'
  os.environ['clatn']         = f'{config_regional.clatn:.2f}'
  os.environ['clats']         = f'{config_regional.clats:.2f}'
  os.environ['clone']         = f'{config_regional.clone:.2f}'
  os.environ['clonw']         = f'{config_regional.clonw:.2f}'
  os.environ['nlat_ll']       = f'{config_regional.nlat_ll}'
  os.environ['nlon_ll']       = f'{config_regional.nlon_ll}'
  os.environ['scriptsdir']    = f'{config_common.scriptsdir}'
  os.environ['RUN_CMF']       = str(config_regional.lprepare_cmf_basin).lower()
  os.environ['CMF_RES']       = config_regional.cmf_res
  os.environ['dx']            = f'{config_regional.dx:.2f}'
  os.environ['WORKDIR']       = config_common.inidir
  os.environ['FMAP']          = f'{config_common.cmfdir}/{config_regional.cmf_res}_{config_regional.dx:.2f}_{config_init.climv}'
  os.environ['FIXDIR']        = f"{config_common.cmfdir}/{config_regional.cmf_res}_{config_regional.dx:.2f}_{config_init.climv}"
  if config_common.METVIEW_PYTHON_START_TIMEOUT is not None:
    os.environ['METVIEW_PYTHON_START_TIMEOUT'] = f'{config_common.METVIEW_PYTHON_START_TIMEOUT}'
  else:
    os.environ['METVIEW_PYTHON_START_TIMEOUT'] = str(3000)

  prepare_cmf_basin_script=f'{config_common.scriptsdir}/prepare_basin_ini.bash {out_cmf_dir}'
  try:
    subprocess.run(prepare_cmf_basin_script, shell=True, check=True)

  except subprocess.CalledProcessError as e:
    print(f"Error running the shell script: {e}")
    raise Exception(f"fail in {e}")


# Function to run extraction and preparation forcing script in parallel
def prepare_forcing_2D(nn, config_common, config_init, config_regional, config_forcing):
  """
  Prepare forcing data for 2D processing in parallel using Pool.

  Args:
    nn (int): The process number.
    config_common (object): An object containing common configuration parameters.
    config_init (object): An object containing initialization configuration parameters.
    config_regional (object): An object containing regional configuration parameters.
    config_forcing (object): An object containing forcing configuration parameters.

  Returns:
    None

  Raises:
    Exception: If there is an error running the shell script.
  """

  # Export variables:
  os.environ['scriptsdir'] = config_common.scriptsdir
  os.environ['dx']         = f'{config_regional.dx:.2f}'
  os.environ['clatn']      = f'{config_regional.clatn:.2f}'
  os.environ['clats']      = f'{config_regional.clats:.2f}'
  os.environ['clone']      = f'{config_regional.clone:.2f}'
  os.environ['clonw']      = f'{config_regional.clonw:.2f}'
  os.environ['regional']   = f'{str(config_regional.regional).lower()}'
  os.environ['xdata_dir']  = f'{config_init.xdata_dir}'
  os.environ['climv']      = f'climate.{config_init.climv}'
  os.environ['preProcForcdir']= f'{config_forcing.preProcForcDir}'
  os.environ['RETRIEVE_WITH'] = f'{config_common.retrieve_with}'
  os.environ['RETRIEVE_WITH'] = f'{config_common.retrieve_with}'
  os.environ['FORCINGCLASS'] = f'{config_forcing.forcingClass}'
  if config_common.METVIEW_PYTHON_START_TIMEOUT is not None:
     os.environ['METVIEW_PYTHON_START_TIMEOUT'] = f'{config_common.METVIEW_PYTHON_START_TIMEOUT}'
  else:
     os.environ['METVIEW_PYTHON_START_TIMEOUT'] = str(3000)
  os.environ['ORODIFF_FILE'] = f'{config_forcing.oroDiff_file}'
  os.environ['lapseCorr'] = f'{config_forcing.lapseCorr}'

  if config_forcing.SAVE_FORCING_GRIB  is not None:
     os.environ['SAVE_FORCING_GRIB'] = f'{str(config_forcing.SAVE_FORCING_GRIB).lower()}'
  else:
     os.environ['SAVE_FORCING_GRIB'] = str(False)
  if config_forcing.forcingCommonName  is not None:
     os.environ['forcingCommonName'] = f'{config_forcing.forcingCommonName}'

  initial_date = config_common.iniDates_sliced[nn]
  end_date = config_common.endDates_sliced[nn]

  print(f'running process: {nn}, dates: {initial_date}-{end_date}')
  prepare_forcing_2D_script = f'{config_common.scriptsdir}/prepare_forcing_2D.bash {config_common.fdir} {initial_date} {end_date} {config_common.sodir}'
  logfile_name = f'{config_common.fdir}/prep_forcing_2D_{nn}.log'
  subprocess.run(f'rm -f {logfile_name}', shell=True, check=True)
  try:
    with open(logfile_name, 'w') as logfile:
      subprocess.run(prepare_forcing_2D_script, shell=True, check=True,
               stdout=logfile, stderr=subprocess.STDOUT)

    subprocess.run(f'rm -f {logfile_name}', shell=True, check=True)
  except subprocess.CalledProcessError as e:
    print(f"Error running the shell script: {e}, num_process={nn}, extraction dates {initial_date}-{end_date}")
    subprocess.run(f'cat {logfile_name} | tail -n20', shell=True)
    raise Exception(f"Error running the shell script: {e}")


# Compute time slices over which running the parallel forcing extraction script.
# The function returns two lists of initial and end dates with length of nproc.
def define_timeSlices(iniDate, endDate, nproc):
  """
  Define time slices based on the initial date, end date, and number of processes.

  Args:
    iniDate (str): The initial date in the format 'YYYYMMDD'.
    endDate (str): The end date in the format 'YYYYMMDD'.
    nproc (int): The number of processes.

  Returns:
    tuple: A tuple containing two lists - iniDates and endDates.
      - iniDates: A list of initial dates for each time slice in the format 'YYYYMMDD'.
      - endDates: A list of end dates for each time slice in the format 'YYYYMMDD'.
  """

  # Convert string dates to datetime objects
  iniDate_dt = datetime.strptime(iniDate, '%Y%m%d')
  endDate_dt = datetime.strptime(endDate, '%Y%m%d')
  # Calculate the total number of days and days in each
  total_days = (endDate_dt - iniDate_dt).days

  # Calculate the number of days in each part
  days_per_part = total_days // nproc

  # Initialize lists to store iniDates and endDates
  iniDates = []
  endDates = []

  # Generate iniDates and endDates for each part
  for i in range(nproc):
    # Calculate iniDate for the current part
    if i == 0:
      tmp_iniDate = iniDate_dt + timedelta(days=i * days_per_part)
      if tmp_iniDate.day != 1:
        tmp_2 = tmp_iniDate.replace(day=1)
        current_iniDate = tmp_2
      else:
        current_iniDate = tmp_iniDate
    else:
      current_iniDate = current_endDate + timedelta(days=1)
    iniDates.append(current_iniDate.strftime('%Y%m%d'))

    # Calculate endDates for the current part
    if i == nproc - 1:  # For the last part, use endDate
      current_endDate = endDate_dt
    else:
      current_endDate = current_iniDate + timedelta(days=days_per_part - 1) + timedelta(days=10)
      current_endDate = current_endDate.replace(day=1)
      current_endDate = current_endDate + timedelta(days=-1)

    endDates.append(current_endDate.strftime('%Y%m%d'))

  return iniDates, endDates


# created temporary files used by the various scripts
def create_ascii_files(s1, i1, e1, clim_outdir, forc_outdir, l1=None, l2=None):
  """
  Create ASCII temporary files for site list, initial date list, end date list, latitude list, and longitude list.
  Those are used by a number of the shell scripts.

  Args:
    s1 (list): List of site names.
    i1 (list): List of initial dates.
    e1 (list): List of end dates.
    clim_outdir (str): Directory path for climate (initial conditions) output.
    forc_outdir (str): Directory path for forcing output.
    l1 (list, optional): List of latitudes. Defaults to None.
    l2 (list, optional): List of longitudes. Defaults to None.
  """
  for outdir in [clim_outdir, forc_outdir]:
    txtfile = f"{outdir}/site_list.txt"
    if os.path.exists(txtfile):
      os.remove(txtfile)
    with open(txtfile, "w") as file:
      for item in s1:
        file.write(item + "\n")

    txtfile = f"{outdir}/iniDate_list.txt"
    if os.path.exists(txtfile):
      os.remove(txtfile)
    with open(txtfile, "w") as file:
      for item in i1:
        file.write(str(item) + "\n")

    txtfile = f"{outdir}/endDate_list.txt"
    if os.path.exists(txtfile):
      os.remove(txtfile)
    with open(txtfile, "w") as file:
      for item in e1:
        file.write(str(item) + "\n")

    txtfile = f"{outdir}/lat_list.txt"
    if l1 is not None:
      if os.path.exists(txtfile):
        os.remove(txtfile)
      with open(txtfile, "w") as file:
        for item in l1:
          file.write(str(item) + "\n")

    txtfile = f"{outdir}/lon_list.txt"
    if l2 is not None:
      if os.path.exists(txtfile):
        os.remove(txtfile)
      with open(txtfile, "w") as file:
        for item in l2:
          file.write(str(item) + "\n")
  return


# Clean temporary files created by the various scripts
def clean_tmp_files(forcingType,clim_outdir, forc_outdir ):
    for outdir in [clim_outdir, forc_outdir]:
      if forcingType == '1D':
         tmp_files=["iniDate_list.txt","endDate_list.txt","site_list.txt","lat_list.txt","lon_list.txt"]
         tmp_loc_files=["compute_time.py","leap_year.py","ndays.txt","time.asc","temp"]
      else:
         tmp_files=["iniDate_list.txt","endDate_list.txt","site_list.txt"]
      for ff in tmp_files:
         try:
             if os.path.isfile(outdir+"/"+ff):
                 os.remove(outdir+"/"+ff)
             else:
                 print(f"{outdir}/{ff} does not exist, can't be removed")
         except OSError as e:
             print(f"Error: {e}")
      if forcingType == '1D':
        for ff in tmp_loc_files:
         try:
             if os.path.isfile("./"+ff):
                 os.remove("./"+ff)
             else:
                 print(f"./{ff} does not exist, can't be removed")
         except OSError as e:
             print(f"Error: {e}")
      return


# Read latitude/longitude from the string lat,lon from the YAML file.
def read_latlon(latlon_list):
    lats = []
    lons = []
    for entry in latlon_list:
        lat, lon = entry.split(',')
        lats.append(float(lat))
        lons.append(float(lon))
    return lats, lons


def load_config(config_file):
  """
  Load the configuration data from a YAML file.

  Args:
    config_file (str): The path to the YAML configuration file.

  Returns:
    dict: The loaded configuration data.

  Raises:
    FileNotFoundError: If the specified config file is not found.
    Exception: If there is an error loading the config file.

  """
  try:
    with open(config_file, 'r') as file:
      config_data = file.read()
      # Perform variable substitution
      config_data = config_data.replace('${USER}', os.getenv('USER', 'default_username'))
      config_data = yaml.safe_load(config_data)
    return config_data
  except FileNotFoundError:
    print(f"Config file '{config_file}' not found.")
    print(f"YAML config file should be provided with -c flag:")
    print(f"./ecland_create_forcing.py -c <config.yaml>")
    exit(1)
  except Exception as e:
    print(f"Error loading config file '{config_file}': {e}")
    print(f"YAML config file should be provided with -c flag:")
    print(f"./ecland_create_forcing.py -c <config.yaml>")
    print("For full info:")
    print(f"./ecland_create_forcing.py --info")
    exit(1)

def compile_cython_ext(config_common):
  """
  Compile Cython extensions.

  Args:
    config_common: An instance of the `config_common` class.

  Raises:
    Exception: If there is an error running the script.

  """
  scriptsdir = config_common.scriptsdir
  compile_cython_ext_script = f'python3 {scriptsdir}/osm_pyutils/setup_cython.py build_ext --build-lib={scriptsdir} --path={scriptsdir}/osm_pyutils'
  try:
    subprocess.run(compile_cython_ext_script, shell=True, check=True)

  except subprocess.CalledProcessError as e:
    print(f"Error running the script: {e}")
    raise Exception(f"fail in {e}")


def print_info():
    print("")
    print("=====================")
    print("Running script to retrieve and create initial conditions and forcing")
    print("on a site or region from era5 data for ECLand ")
    print("The script can retrieve either from MARS or CDS the era5 forcing if")
    print("forcing files are not provided")
    print("To run extraction provide/edit the yaml configuration file")
    print("./ecland_create_forcing.py -c <config.yaml> [--clean]")
    print("                     ")
    print("This is a python wrapper, main calculations done by bash scripts:")
    print("- For 1D (site) extraction:")
    print("  - extract_site_clim.bash")
    print("  - create_sites.bash")
    print("  - extract_create_forcing.bash")
    print("    - extract_forcing_cds.bash  ")
    print("    - extract_forcing_mars.bash ")
    print("- For 2D (regional) extraction:")
    print("  - extract_create_inicond_2D.bash")
    print("  - prepare_forcing_2D.bash")
    print("                     ")
    print("External library requirements:")
    print("- eccodes")
    print("- metview")
    print("- metview-python")
    print("- cds api (if not using mars for data download)")
    print("- pyYAML")
    print("- netcdf4")
    print("- cdo")
    print("- nco")
    print("- ecBuild (optional)")
    print("=====================")
    print("                     ")
    print("")

