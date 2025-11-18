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

ecbuild_add_option( FEATURE IFSBENCH_EDITABLE
                    DEFAULT OFF
                    DESCRIPTION "Perform editable install for ifsbench packages"
)

# Name of the virtual environment
set( VENV_NAME "ifsbench_venv" )
set( VENV_PATH "${CMAKE_CURRENT_BINARY_DIR}/${VENV_NAME}" )

# Create a Python virtual environment in the current binary directory.
ecbuild_info( "Create ifsbench virtual environment ${VENV_NAME}" )

execute_process( COMMAND ${Python3_EXECUTABLE} -m venv ${VENV_PATH} )

set( IFSBENCH_PYTHON "${VENV_PATH}/bin/python3")

if( ENABLE_IFSBENCH_EDITABLE )
  set( PIP_INSTALL_FLAGS "-e")
else()
  set( PIP_INSTALL_FLAGS "")
endif()

# Install the ifsbench_ecland package. This will also install all necessary
# Python dependencies.
execute_process(
  COMMAND ${IFSBENCH_PYTHON} -m pip install ${PIP_INSTALL_FLAGS} ${CMAKE_CURRENT_SOURCE_DIR}
)

set( LAUNCH_COMMAND ${CMAKE_CURRENT_BINARY_DIR}/ifsbench_run.py )

# Link the main runscript into the binary directory.
execute_process(
  COMMAND ${CMAKE_COMMAND} -E create_symlink "${VENV_PATH}/bin/ifsbench_run.py" "${LAUNCH_COMMAND}"
  COMMAND_ERROR_IS_FATAL ANY
)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/test.yaml
  ${CMAKE_CURRENT_BINARY_DIR}/test.yaml
)

foreach(TEST_NAME v1_EU_default v1_EU_centered
    v1_EU_multiprocess v1_EU_multithread v1_EU_multiprocess_thread)
    ecbuild_add_test(
        TARGET ecland_ifsbench_${TEST_NAME}
        TYPE SCRIPT
        COMMAND ${LAUNCH_COMMAND}
        ARGS from_yaml ${CMAKE_CURRENT_BINARY_DIR}/test.yaml ${TEST_NAME}
            --run-dir=${CMAKE_CURRENT_BINARY_DIR}/ecland_ifsbench_${TEST_NAME}
    )
endforeach()
