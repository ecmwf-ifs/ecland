#!/bin/bash
# (C) Copyright 2023- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set -eux

# Remove clim,init, forcing and output files created for experiments CA-002, CA-003, CA-004:
exps="CA-002 CA-003 CA-004 VG-001 VG-002 SM-001 SM-002"

rm -rf CA-001_2022-2022/
for exp in $exps; do
  rm -f ecland_input/clim/LS-PRACTICALS/surfclim_${exp}_2022-2022.nc
  rm -f ecland_input/clim/LS-PRACTICALS/surfinit_${exp}_2022-2022.nc
  rm -f ecland_input/forcing/LS-PRACTICALS/met_era5HT_${exp}_2022-2022.nc

  # Remove output directories
  rm -rf ${exp}_2022-2022/
done
# Remove standard output:
rm stdout*

# Remove plots and plotting directories
rm -rf plots/
rm -f *.pdf
