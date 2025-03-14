# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# Detect only the system installed Python3 interpreter.
set( Python3_FIND_VIRTUALENV STANDARD )

ecbuild_find_package( Python3 COMPONENTS Interpreter REQUIRED VERSION 3.8 )

set( VENV_NAME "ifsbench_venv" )
set( VENV_PATH "${CMAKE_CURRENT_BINARY_DIR}/${VENV_NAME}" )

# Create a Python virtual environment in the current binary directory.
ecbuild_info( "Create ifsbench virtual environment ${VENV_NAME}" )

execute_process( COMMAND ${Python3_EXECUTABLE} -m venv ${VENV_PATH} )

set( IFSBENCH_PYTHON "${VENV_PATH}/bin/python3")

# Install the missing Python packages.
execute_process(
    COMMAND ${IFSBENCH_PYTHON} -m pip install "ifsbench @ git+https://github.com/ecmwf-ifs/ifsbench.git"
)

configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/ifsbench_run.py
    ${CMAKE_CURRENT_BINARY_DIR}/ifsbench_run.py
)

configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/test.yaml
    ${CMAKE_CURRENT_BINARY_DIR}/test.yaml
)

foreach(TEST_NAME v1_EU_default v1_EU_centered v1_EU_forward
    v1_EU_multiprocess v1_EU_multithread v1_EU_multiprocess_thread)
    ecbuild_add_test(
        TARGET ecland_ifsbench_${TEST_NAME}
        TYPE SCRIPT
        COMMAND ${IFSBENCH_PYTHON}
        ARGS ${CMAKE_CURRENT_BINARY_DIR}/ifsbench_run.py from_yaml
            ${CMAKE_CURRENT_BINARY_DIR}/test.yaml ${TEST_NAME}
            --run-dir=${CMAKE_CURRENT_BINARY_DIR}/ecland_ifsbench_${TEST_NAME}
    )
endforeach()
