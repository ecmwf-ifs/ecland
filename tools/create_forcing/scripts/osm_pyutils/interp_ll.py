# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import metview as mv
import argparse

# Create an argument parser
parser = argparse.ArgumentParser(description='Interpolation Script using metview')

# Add argument for the interpolation type
parser.add_argument('-t', '--type', choices=['bil', 'nn', 'con'], required=True,
                    help='Interpolation type: bilinear (bil), nearest-neigh (nn) or conservative (con)')

parser.add_argument('-d', '--dx', required=True,
                    help='regional grid dx')

parser.add_argument('-s', '--lats', required=True,
                    help='regional lat south')
parser.add_argument('-n', '--latn', required=True,
                    help='regional lat north')
parser.add_argument('-w', '--lonw', required=True,
                    help='regional lon west')
parser.add_argument('-e', '--lone', required=True,
                    help='regional lon east')

# Add argument for the input file
parser.add_argument('-i', '--input', required=True,
                    help='Input file name')

# Add argument for the output file
parser.add_argument('-o', '--output', required=True,
                    help='Output file name')

# Parse the command-line arguments
args = parser.parse_args()

# Access the argument values
interp_type = args.type
dx = args.dx
input_file = args.input
output_file = args.output

iniData=mv.read(input_file)

target_grid={'grid':[dx,dx], 'area':[args.lats, args.lonw, args.latn, args.lone]} # S,W,N,E}
if interp_type == 'bil':
  outputData=mv.regrid(target_grid, data=iniData)
else:
  if interp_type =='nn':
      interp_method='nearest_neighbour'
  if interp_type =='con':
      interp_method='grid_box_average'
  outputData=mv.regrid(target_grid, data=iniData,interpolation=interp_method)

mv.write(f"{output_file}",outputData)
