#!/usr/bin/env python3
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import metview as mv
import argparse

parser = argparse.ArgumentParser(description='preprocess monthly forcing to concatenate them ')

# Add a --help argument with a custom help message
parser.add_argument('-i', '--input', required=True, help='input forcing file')
parser.add_argument('-o', '--output',required=True, help='output forcing file')
parser.add_argument('-l', '--lat', type=float, required=True, help='lat of point')
parser.add_argument('-L', '--lon', type=float, required=True, help='lon of point')


args = parser.parse_args()

fin=mv.read(f"{args.input}")
area=[args.lat-1.0, args.lon-1.0, args.lat+1.0, args.lon+1.0] # S,W,N,E
fout=mv.read(data=fin,area=area)


mv.write(f"{args.output}",fout)





