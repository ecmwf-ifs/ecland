# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import cdsapi
import argparse
import subprocess
import argparse
import metview as mv
import numpy as np

def Grb1toGrb2(var, varsGrb1toGrb2, outf, mode='retrieve'):
  """
  Convert GRIB1 file to GRIB2 format.

  Args:
    var (str): The variable to convert.
    varsGrb1toGrb2 (dict): A dictionary mapping variables from GRIB1 to GRIB2.
    outf (str): The output file path.
    mode (str, optional): The mode of creation of input files. Defaults to 'retrieve'.

  Returns:
    None
  """
  if var in varsGrb1toGrb2.keys():
    if var not in ["clake", "wetlandf", "urban"]:
      subprocess.run(['grib_set', '-s', 'edition=2',
              '-w', f'paramId={varsGrb1toGrb2[var]}', outf, 'tmp.grb'])
    else:
      subprocess.run(['grib_set', '-s', 'paramId=33,edition=2',
              '-w', f'paramId={varsGrb1toGrb2[var]}', outf, 'tmp.grb'])
      subprocess.run(['grib_set', '-s', f'paramIdECMF={varsGrb1toGrb2[var]}',
              '-w', f'paramId=33', 'tmp.grb', 'tmp2.grb'])
      subprocess.run(['mv', '-f', 'tmp2.grb', 'tmp.grb'])

    # Workaround for lake depth and some eccodes versions
    if var == 'lakedl':
      subprocess.run(['grib_set', '-s', 'paramId=228007', '-w', 'paramId=0', 'tmp.grb', 'tmp2.grb'])
      subprocess.run(['mv', '-f', 'tmp2.grb', 'tmp.grb'])
    subprocess.run(['mv', '-f', 'tmp.grb', outf])


def process_tmp_grb(var, tmpTemplate, tmpGrbParam, tmpGrbDefValue, outFile):
  """
  Process temporary GRIB file.

  Args:
    var (str): The variable to process.
    tmpTemplate (dict): A dictionary mapping variables to their corresponding template names.
    tmpGrbParam (dict): A dictionary mapping variables to their corresponding parameter IDs.
    tmpGrbDefValue (dict): A dictionary mapping variables to their corresponding default values.
    outFile (str): The output file name.

  Returns:
    None
  """
  subprocess.run(['grib_copy', '-w', f'shortName={tmpTemplate[var]}', outFile, 'template_tmp.grb'])
  subprocess.run(['grib_set', '-s', f'paramIdECMF={tmpGrbParam[var]}', 'template_tmp.grb', f'tmp.grb'])
  
  # Set the default value. If != 0 use 
  if tmpGrbDefValue[var] == 0:
    subprocess.run(['grib_set', '-s', f'scaleValuesBy=0.0', 'tmp.grb', f'{var}'])
  else:
    fs = mv.read("tmp.grb")
    nval=len(fs[0].values())
    defValue=np.zeros(nval)+tmpGrbDefValue[var]
    fs = mv.set_values(fs, defValue)
    mv.write(var, fs)
  
  subprocess.run(['rm', '-f', 'tmp.grb' 'template_tmp.grb'])


def parse_arguments():
  """
  Parse command line arguments for downloading CDS data.

  Returns:
    argparse.Namespace: Parsed command line arguments.
  """
  parser = argparse.ArgumentParser(description='Download CDS data')
  parser.add_argument('-v', '--variables', nargs='+', help='List of variables to download', required=True)
  parser.add_argument('-r', '--resol', type=str, help='gaussian grid resolution of the data', required=True)
  parser.add_argument('-m', '--mode', type=str, help='tmpGrib mode or retrieve mode', default='retrieve', required=False)
  parser.add_argument('-o', '--output', type=str, help='file output name', required=True)
  parser.add_argument('-c', '--clim', action='store_true', help='Climatological value (yearly)', required=False, default=False)

  return parser.parse_args()

args = parse_arguments()
climVars = args.variables
MRESOL = args.resol
outFile = args.output
mode = args.mode
isClim = args.clim

c = cdsapi.Client()

# Dictionary mapping variable file name to GRIB parameter to retrieve.
grb2Name={"lsm":"172.128",
          "oro":"129.128",
          "clake":"26.128",
          "slt":"43.128",
          "lakedl":"7.228",
          "sfc":"161.128/162.128/163.128/160.128/28.128/27.128/30.128/29.128",
          "month_lail":"66.128",
          "month_laih":"67.128",
          "month_aluvd":"16.128",
          "month_alnid":"17.128",
          }

# "sfc" is a special case, as it contains multiple variables. 
varsInsfc=['anor','isor','slor','sdor','cvh','cvl','tvh','tvl']

# Dictionary mapping variables from GRIB1 to GRIB2
varsGrb1toGrb2={'anor':'162',
                'isor':'161',
                'slor':'163',
                'sdor':'160',
                'cvh':'28',
                'cvl':'27',
                'tvh':'30',
                'tvl':'29',
                'lsm':'172',
                'oro':'129',
                'slt':'43',
                'clake':'26',
                'lakedl':'228007',
                'month_lail':'66',
                'month_laih':'67',
                'urban':'200199',
                'c3_c4_mask.grb':'129172',
               }


# urban, wetland and c3_c4 are not retrieved because not present in era5. 
# if requested will be created from other grib files defined if mode == 'tmpGrib'
tmpGrbName={'urban':"vegdiff",
            'wetlandf':'cldiff',
            'c3_c4_mask.grb':'lsmgrd'}
tmpGrbParam={'urban':"200199",
             'wetlandf':"200026",
             'c3_c4_mask.grb':"129172"}
tmpGrbValue={'urban':0,
             'wetlandf':0,
             'c3_c4_mask.grb':3}
tmpTemplate={'urban':'cvh',
             'c3_c4_mask.grb':'cvh',
             'wetlandf':'month_aluvd'}


# Create a unique string separated by "/" for the climatological variables to retrieve
climStringParams = "/".join([grb2Name[var] for var in climVars if var in grb2Name])

if mode == 'tmpGrib':
    climStringParams="28.128/66.128"
    climVarsTmp=["cvh","month_aluvd"]


# Select date:
Date="2023-01-01"
if isClim:
    Date="2023-01-01/2023-02-01/2023-03-01/2023-04-01/2023-05-01/2023-06-01/2023-07-01/2023-08-01/2023-09-01/2023-10-01/2023-11-01/2023-12-01"

# Date and time are set as those are invariant fields
c.retrieve('reanalysis-era5-complete',
    {
    'date'    : Date,
    'levtype' : "sfc",
    'param'   : climStringParams,
    'stream'  : "oper",
    'time'    : "00:00",
    'type'    : "an",
    'grid'    : MRESOL,
    'class'   : "ea",
    'format'  : 'grib',
    },
    outFile)

# Convert from GRIB1 to GRIB2 a set of parameters.
# Depending on the mode of operation, loop over climVarsTmp or climVars
if mode == 'tmpGrib':
   varGrb2=climVarsTmp
else:
   varGrb2=climVars

# Convert GRIB1 to GRIB2 for selected parameters as defined in varsGrb1toGrb2.
for var in varGrb2:
    if var == 'sfc':
       for subvar in varsInsfc:
         Grb1toGrb2(subvar, varsGrb1toGrb2, outFile,mode=mode)
    else:
       Grb1toGrb2(var, varsGrb1toGrb2, outFile,mode=mode)

# urban, wetlandf, c3_c4_mask are not in era5. For the time being, just create grib containers with default values.
# urban default value = 0
# wetlandf default value = 0
# c3_c4_mask default value = 3



# Call the method for each var in tmpGrbName.keys()
for var in tmpGrbName.keys():
  if var in climVars:
    process_tmp_grb(var, tmpTemplate, tmpGrbParam,tmpGrbValue, outFile)
    with open(outFile, 'ab') as f_out:
      # Open var file in read mode
      with open(var, 'rb') as f_var:
        # Write the contents of var file to outFile
        f_out.write(f_var.read())

# Remove temporary grib parameters that were downloaded as templates and not required.
if mode == 'tmpGrib':
  varTmp = climStringParams.split('/')
  varTmp = [var.split('.')[0] for var in varTmp]
# Join the modified parts back into a single string
  varTmp = '/'.join(varTmp)
  subprocess.run(['grib_copy', '-w', f'paramId!={varTmp}', outFile, 'tmp.grb'])
  subprocess.run(['mv', '-f', 'tmp.grb', outFile])


