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

def extract_values(filename,output_var):
    results = []
    with open(filename, 'r') as file:
        flag = False
        for line in file:
            if line.strip() == output_var:
                flag = True
            elif flag and line.startswith(" MEAN-VALUE:"):
                mean = float(line.split()[1])
            elif flag and line.startswith(" MIN -VALUE:"):
                min_value = float(line.split()[2])
            elif flag and line.startswith(" MAX -VALUE:"):
                max_value = float(line.split()[2])
                results.append((mean, min_value, max_value))
                flag = False
    # Write the extracted data into a new text file as a table
    output_filename=f"{output_var}.log"
    with open(output_filename, 'w') as outfile:
        outfile.write("MEAN-VALUE MIN-VALUE MAX-VALUE\n")
        if output_var in ["SoilTemp","SoilMois"]:
          out_results=results[4:]
        else:
          out_results=results
        for mean, min_value, max_value in out_results:
            outfile.write(f"{mean} {min_value} {max_value}\n")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Extract MEAN-VALUE, MIN-VALUE, and MAX-VALUE from the log file of osm experiments for a given variable")
    parser.add_argument("-i", dest="input_filename", help="Input text file name")
    parser.add_argument("-v", dest="variable",       help="Variable to extract statistics")
    args = parser.parse_args()

    filename   = args.input_filename
    variable   = args.variable
    extract_values(filename,variable)


