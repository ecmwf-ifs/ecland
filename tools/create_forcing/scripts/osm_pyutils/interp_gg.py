# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

"""
Interpolation script to reduced Gaussian grid using MetView.

Supports:
  - TL (linear truncation): grid="N32", "N64", "N128", "N320", etc.
  - TCO (cubic octahedral): grid="O96", "O400", "O1280", etc.

Usage:
  python interp_gg.py -t bil -g N32 -i input.grb -o output.grb
  python interp_gg.py -t nn -g O96 -i input.grb -o output.grb
  
  # With regional clipping:
  python interp_gg.py -t bil -g N64 -s 35 -n 60 -w -10 -e 30 -i input.grb -o output.grb
"""

import metview as mv
import argparse

parser = argparse.ArgumentParser(description='Interpolation to reduced Gaussian grid using MetView')

# Interpolation type
parser.add_argument('-t', '--type', choices=['bil', 'nn', 'con'], required=True,
                    help='Interpolation type: bilinear (bil), nearest-neighbour (nn) or conservative (con)')

# Target Gaussian grid specification
parser.add_argument('-g', '--grid', required=True,
                    help='Target Gaussian grid (e.g., N32, N64, N128, N320 for TL; O96, O400 for TCO)')

# Optional regional clipping
parser.add_argument('-s', '--lats', required=False, default=None,
                    help='Regional lat south (optional, for clipping)')
parser.add_argument('-n', '--latn', required=False, default=None,
                    help='Regional lat north (optional, for clipping)')
parser.add_argument('-w', '--lonw', required=False, default=None,
                    help='Regional lon west (optional, for clipping)')
parser.add_argument('-e', '--lone', required=False, default=None,
                    help='Regional lon east (optional, for clipping)')

# Input/output files
parser.add_argument('-i', '--input', required=True,
                    help='Input file name (GRIB)')
parser.add_argument('-o', '--output', required=True,
                    help='Output file name (GRIB)')

args = parser.parse_args()

# Read input data
iniData = mv.read(args.input)

# Build target grid specification for reduced Gaussian grid
# MetView uses "reduced_gg" grid type with the N/O number
target_grid = {'grid': args.grid}

# Add regional clipping if specified
if args.lats is not None and args.latn is not None and args.lonw is not None and args.lone is not None:
    # area format: [S, W, N, E]
    target_grid['area'] = [args.lats, args.lonw, args.latn, args.lone]

# Perform interpolation
if args.type == 'bil':
    outputData = mv.regrid(target_grid, data=iniData)
elif args.type == 'nn':
    outputData = mv.regrid(target_grid, data=iniData, interpolation='nearest_neighbour')
elif args.type == 'con':
    outputData = mv.regrid(target_grid, data=iniData, interpolation='grid_box_average')

# Write output
mv.write(args.output, outputData)

print(f"Interpolated to {args.grid} grid: {args.input} -> {args.output}")
