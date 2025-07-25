# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

list( APPEND tests
    insitu_US-Ha1_1991-1996
    2D_EU-001_20220101-20220102
)
foreach( test ${tests} )

    unset(TEST_ARGS)

    # set test-dir
    list(APPEND TEST_ARGS "--test-dir=${CMAKE_CURRENT_SOURCE_DIR}/${test}")

    # set launch parameters
    list(APPEND TEST_ARGS "--launch=ecland-launch -np 1 -nt 8")

    # set relative tolerance for tests
    if(${test} MATCHES "insitu_US-Ha1_1991-1996")
       if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
          list(APPEND TEST_ARGS "--tolerance=5.e-8 5.e-8 5.e-8" )
       elseif(CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC")
          list(APPEND TEST_ARGS "--tolerance=1.e-6 1.e-6 1.e-6" )
       else()
          list(APPEND TEST_ARGS "--tolerance=1.e-8 1.e-8 1.e-8" )
       endif()
    else()
       list(APPEND TEST_ARGS "--tolerance=1.e-8 1.e-8 1.e-8" )
    endif()

    ecbuild_add_test(
        TARGET ecland_test_${test}
        TYPE SCRIPT
        COMMAND ${PROJECT_SOURCE_DIR}/share/ecland/scripts/ecland_run_test.sh
        ARGS ${TEST_ARGS}
        ENVIRONMENT
            ECLAND_MASTER=${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}-master
            WORK_DIR=${CMAKE_CURRENT_BINARY_DIR}/${test}/work
            OUTPUT_DIR=${CMAKE_CURRENT_BINARY_DIR}/${test}/output
            INPUT_DIR=${CMAKE_CURRENT_BINARY_DIR}/${test}/input
    )
endforeach()
