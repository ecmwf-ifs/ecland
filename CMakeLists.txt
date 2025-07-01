# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

cmake_minimum_required( VERSION 3.24 FATAL_ERROR )

include(FetchContent)

find_package( ecbuild 3.11 QUIET HINTS ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild )
if( NOT ecbuild_FOUND )
  include(cmake/ecland_fetchcontent_ecbuild.cmake)
endif()

if( NOT ECLAND_PROJECT_NAME )
  set( ECLAND_PROJECT_NAME ecland )
endif()

project( ${ECLAND_PROJECT_NAME} LANGUAGES Fortran C CXX )

include( ecland_macros )
ecbuild_enable_fortran( REQUIRED NO_MODULE_DIRECTORY )

### Dependencies
########################################################################
ecland_fetchcontent_dependencies()

ecbuild_find_package( NetCDF COMPONENTS Fortran REQUIRED )

#### eccodes
ecbuild_find_package(eccodes REQUIRED)
if( NOT TARGET eccodes_f90 )
    ecbuild_critical("eccodes found without Fortran at ${eccodes_DIR}")
endif()

#### fiat
ecbuild_find_package(fiat REQUIRED)
set( HAVE_MPI ${fiat_HAVE_MPI} )

### Options
########################################################################

ecbuild_add_option( FEATURE OMP
                    DESCRIPTION "support for OpenMP shared memory parallelism"
                    REQUIRED_PACKAGES "OpenMP COMPONENTS Fortran" )

ecbuild_add_option( FEATURE SINGLE_PRECISION
                    DEFAULT OFF
                    DESCRIPTION "Support for Single Precision" )

### Set third party libraries

if( HAVE_SINGLE_PRECISION )
  add_library( parkind ALIAS parkind_sp )
else()
  add_library( parkind ALIAS parkind_dp )
endif()

unset( OpenMP_Fortran_LIBRARIES )
if( HAVE_OMP )
  set( OpenMP_Fortran_LIBRARIES OpenMP::OpenMP_Fortran )
else()
  ecbuild_critical( "Currently OpenMP is non-optional, but only for offline file cpg1s.F90" )
endif()

### Sources
########################################################################

include(ecland_compile_flags)

add_subdirectory(src)
add_subdirectory(share)

### Tests

add_subdirectory(tests)

### Documentation

#add_subdirectory(doc)

### Export

ecbuild_install_project( NAME ${PROJECT_NAME} )

ecbuild_print_summary()

