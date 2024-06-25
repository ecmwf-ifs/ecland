#!/usr/bin/env python3
# (C) Copyright 2023- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


import argparse

def read_ascii_file(file_path):
    data = []
    with open(file_path, 'r') as file:
        lines = file.readlines()
        headers = lines[0].strip().split()
        for line in lines[1:]:
            values = line.strip().split()
            data.append([float(val) for val in values])
    return headers, data

def calculate_differences(data1, data2):
    # compute the difference and relative difference wrt data1
    # between two lists element by element
    diff  = []
    rdiff = []
    for i in range(len(data1)):
        diff_row  = []
        rdiff_row = []
        for j in range(len(data1[i])):
            diff_row.append(abs(data1[i][j] - data2[i][j]))
            rdiff_row.append(abs(data1[i][j] - data2[i][j])/abs(data1[i][j]))
        diff.append(diff_row)
        rdiff.append(rdiff_row)
    return diff,rdiff

def check_relative_tolerance(rel_diff, rel_tol):
    # Check if each element of a list is smaller/larger than a rel_tol value.
    for row in rel_diff:
        for value in row:
            if abs(value) > rel_tol:
                return False
    return True

def read_args():
    parser = argparse.ArgumentParser(description="Verify sections and MEAN-VALUE from a text file.")
    parser.add_argument("-c", dest="path_ctl", help="Input path to ctl")
    parser.add_argument("-e", dest="path_exp", help="Input path to exp")
    parser.add_argument("-t", dest="rel_tol", help="relative tolerance for var")
    parser.add_argument("-v", dest="variables", nargs="+", help="List of variables to verify")
    return parser.parse_args()


args=read_args()

validation_failed = 0
for var in args.variables:
    input_ctl = args.path_ctl+"/"+var+".log"
    input_exp = args.path_exp+"/"+var+".log"

    # Read the input files into lists
    headers,data_ctl = read_ascii_file(input_ctl)
    headers,data_exp = read_ascii_file(input_exp)

    ## Check the difference of each value of each column
    diff,rel_diff = calculate_differences(data_ctl, data_exp)
    print('Validating '+var+ ' with relative tolerance '+args.rel_tol)
    print("    reference  : ",data_ctl)
    print("    experiment : ",data_exp)
    if check_relative_tolerance(rel_diff,float(args.rel_tol)):
        print('    SUCCESS')
    else:
        print('    FAILED')
        validation_failed += 1

if validation_failed == 0:
    print('Validation PASSED')
    exit(0)
else:
    print('Validation FAILED')
    exit(1)
