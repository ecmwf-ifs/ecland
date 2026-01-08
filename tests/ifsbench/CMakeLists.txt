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

ecbuild_add_option( FEATURE ECLAND_IFSBENCH_EDITABLE
                    DEFAULT OFF
                    DESCRIPTION "Perform editable install for ifsbench packages"
)

# Name of the virtual environment
set( VENV_NAME "ifsbench_venv" )
set( VENV_PATH "${CMAKE_CURRENT_BINARY_DIR}/${VENV_NAME}" )

# Create a Python virtual environment in the current binary directory.
ecbuild_info( "Create ifsbench virtual environment ${VENV_NAME}" )

execute_process( COMMAND ${Python3_EXECUTABLE} -m venv --copies ${VENV_PATH} )

# Store the current Python executable for later use.
set( Python3_EXECUTABLE_CACHE ${Python3_EXECUTABLE} )

# Change the context of the search to only find the venv.
set( Python3_FIND_VIRTUALENV ONLY )

# Unset Python3_EXECUTABLE because it is also an input variable
#  (see documentation, Artifacts Specification section).
unset( Python3_EXECUTABLE )

# To allow cmake to discover the newly created venv if Python3_ROOT_DIR
# was passed as an argument at build-time
set( Python3_ROOT_DIR "${VENV_PATH}" )

# Find newly created python venv,
find_package( Python3 COMPONENTS Interpreter REQUIRED )

set( IFSBENCH_PYTHON ${Python3_EXECUTABLE} )

# Reset Python3_EXECUTABLE to the system installed one.
set( Python3_EXECUTABLE ${Python3_EXECUTABLE_CACHE} )

# Make sure the Python installation has (sufficiently recent) pip.
execute_process( COMMAND ${IFSBENCH_PYTHON} -m ensurepip -U OUTPUT_QUIET )

if( Python3_VERSION VERSION_EQUAL 3.8 )
   execute_process( COMMAND ${IFSBENCH_PYTHON} -m pip --disable-pip-version-check
                    install --upgrade pip OUTPUT_QUIET ERROR_QUIET )
endif()


# If an editable install was requested, set the corresponding flag.
if( ENABLE_ECLAND_IFSBENCH_EDITABLE )
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
  @ONLY
)

## CI tests
list(APPEND TEST_FILES
    v1_EU_default
    v1_EU_centered
    v1_EU_forward
    v1_EU_multiprocess
    v1_EU_multithread
    v1_EU_multiprocess_thread
)

foreach( TEST_NAME ${TEST_FILES} )
    ecbuild_add_test(
        TARGET ecland_ifsbench_${TEST_NAME}
        TYPE SCRIPT
        COMMAND ${LAUNCH_COMMAND}
        ARGS from_yaml ${CMAKE_CURRENT_BINARY_DIR}/test.yaml ${TEST_NAME}
            --run-dir=${CMAKE_CURRENT_BINARY_DIR}/ecland_ifsbench_${TEST_NAME}
    )
endforeach()


## Test exceptions

# Disable Forward test variant - currently not supported
set_property(TEST ecland_ifsbench_v1_EU_forward PROPERTY DISABLED TRUE)
