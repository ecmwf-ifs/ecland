# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# Get all yaml files in the test configuration directory
list(APPEND tests
tests/config_cds.yaml
tests/config_2D_cds.yaml
)
# More tests for ecmwf internal
#*tests/config_2D_ecmwf.yaml
#*tests/config_ecmwf.yaml
#*tests/config_ecmwf_hres.yaml
#*tests/config_2D_ecmwf_cmf.yaml

foreach( test ${tests} )
    get_filename_component(test_name ${test} NAME_WE)
    ecbuild_add_test(
        TARGET ecland_create_forcing_${test_name}
        TYPE PYTHON
        COMMAND ${PROJECT_SOURCE_DIR}/ecland_create_forcing.py
        ARGS -c ${PROJECT_SOURCE_DIR}/${test} --clean
    )
    set_tests_properties(ecland_create_forcing_${test_name} PROPERTIES TIMEOUT 7200)
endforeach()
