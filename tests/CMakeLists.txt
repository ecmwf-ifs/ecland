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

    foreach( prec sp dp )
       if( HAVE_${prec} )
          unset(TEST_ARGS)

          # set precision
          list(APPEND TEST_ARGS "--prec=${prec}")

          # set test-dir
          list(APPEND TEST_ARGS "--test-dir=${CMAKE_CURRENT_SOURCE_DIR}/${test}")

          # set launch parameters
          list(APPEND TEST_ARGS "--launch=ecland-launch -np 1 -nt 8")

          # set precision dependent relative tolerances for tests
          if(${prec} MATCHES "dp")
             if(${test} MATCHES "insitu_US-Ha1_1991-1996")
                if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
                   list(APPEND TEST_ARGS "--tolerance=5.e-5 5.e-5 5.e-5" )
                elseif(CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC|IntelLLVM")
                   list(APPEND TEST_ARGS "--tolerance=5.e-6 5.e-6 5.e-6" )
                else()
                   list(APPEND TEST_ARGS "--tolerance=1.e-8 1.e-8 1.e-8" )
                endif()
             else()
                list(APPEND TEST_ARGS "--tolerance=1.e-8 1.e-8 1.e-8" )
             endif()
          else()
             if(${test} MATCHES "insitu_US-Ha1_1991-1996")
                if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU|IntelLLVM|PGI|NVHPC")
                   list(APPEND TEST_ARGS "--tolerance=2.e-4 2.e-4 2.e-4" )
                else()
                   list(APPEND TEST_ARGS "--tolerance=5.e-5 5.e-5 5.e-5" )
                endif()
             else()
                list(APPEND TEST_ARGS "--tolerance=1.e-6 1.e-6 1.e-6" )
             endif()
          endif()

          ecbuild_add_test(
              TARGET ecland_test_${test}_${prec}
              TYPE SCRIPT
              COMMAND ${CMAKE_BINARY_DIR}/share/ecland/scripts/ecland_run_test.sh
              ARGS ${TEST_ARGS}
              ENVIRONMENT
                  ECLAND_MASTER=${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}-master-${prec}
                  ECLAND_CI=1
                  WORK_DIR=${CMAKE_CURRENT_BINARY_DIR}/${test}_${prec}/work
                  OUTPUT_DIR=${CMAKE_CURRENT_BINARY_DIR}/${test}_${prec}/output
                  INPUT_DIR=${CMAKE_CURRENT_BINARY_DIR}/${test}_${prec}/input
          )
       endif()
    endforeach()
endforeach()

if( HAVE_IFSBENCH_TESTS )
    add_subdirectory( ifsbench )
endif()
