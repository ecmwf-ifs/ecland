#!/bin/bash
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# Example script to retrieve data for a list of sites from the ESM-SnowMIP, PLUMBER2 and TERENO groups.
# Example usage: retrieve_sites.bash -g ESM-SnowMIP -i /path/to/input_dir [-p /path/to/scripts]
# -g: Specify the group (ESM-SnowMIP, PLUMBER2, TERENO)
# -i: Specify the input directory
# -p: Specify the path to the scripts, using default if not provided

opt=""
while getopts "g:i:p:h" opt; do
    case $opt in
        g)
            GROUP=$OPTARG
            ;;
        i)
            INPUT_DIR=$OPTARG
            ;;
        p)
            PATH_TO_SCRIPTS=$OPTARG
            ;;
        h)
            echo "script to retrieve data for a list of sites from the ESM-SnowMIP, PLUMBER2 and TERENO groups"
            echo "Usage: retrieve_sites.bash -g <group> -i <input_dir> [-p <path_to_scripts>]"
            exit 0
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
    esac
done

# Check if GROUP, INPUT_DIR, and PATH_TO_SCRIPTS are provided
if [[ -z $GROUP || -z $INPUT_DIR ]]; then
        echo "Error: GROUP and INPUT_DIR must be provided."
        echo "Usage: retrieve_sites.bash -g <group> -i <input_dir> [-p <path_to_scripts>]"
        exit 1
fi

# Use default PATH_TO_SCRIPTS if not provided
if [[ -z $PATH_TO_SCRIPTS ]]; then
        PATH_TO_SCRIPTS="${ecland_ROOT:-../../../ecland-build}/share/ecland/scripts/"
        echo "PATH_TO_SCRIPTS not defined, using default: ${PATH_TO_SCRIPTS}"
fi
export PATH=${PATH_TO_SCRIPTS}:$PATH # Add ecland scripts to PATH


# Choose the group and forcing_type
case $GROUP in
    ESM-SnowMIP)
        SITES=("cdp_1994-2014"
               "oas_1997-2010"
               "obs_1997-2010"
               "ojp_1997-2010"
               "rme_1988-2008"
               "sap_2005-2015"
               "snb_2005-2015"
               "sod_2007-2014"
               "swa_2005-2015"
               "wfj_1996-2016" )
        FTYPE='insitu'
        ;;
    PLUMBER2)
        echo "Not implemented yet"
        exit 1
        ;;
    TERENO)
        SITES=("TE-001_2022-2022")
        FTYPE='era5'
        ;;
    *)
        echo "Unknown group: $GROUP"
        exit 1
        ;;
esac

# Create directories if not present
mkdir -p ${INPUT_DIR}/clim/${GROUP}
mkdir -p ${INPUT_DIR}/forcing/${GROUP}

# Retrieve data
for SITE in ${SITES[@]}; do
    echo "Retrieving ${SITE} data from ${GROUP} in ${INPUT_DIR}"
    ecland_retrieve.sh data/clim/${GROUP}/surfclim_${SITE}.nc ${INPUT_DIR}/clim/${GROUP}/surfclim_${SITE}.nc
    ecland_retrieve.sh data/clim/${GROUP}/surfinit_${SITE}.nc ${INPUT_DIR}/clim/${GROUP}/surfinit_${SITE}.nc
    ecland_retrieve.sh data/forcing/${GROUP}/met_${FTYPE}HT_${SITE}.nc ${INPUT_DIR}/forcing/${GROUP}/met_${FTYPE}HT_${SITE}.nc
    echo ""
done

