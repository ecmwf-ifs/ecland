# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.
# ----------------------------------------------------------------------------------------
# util (Tools independent from surf library)

ecbuild_add_library(TARGET ${PROJECT_NAME}_offline_util_objs
  SOURCES offline/util/lib_dates.F90
          offline/util/get_points_mod.F90
  PRIVATE_LIBS NetCDF::NetCDF_Fortran
  TYPE OBJECT)

ecbuild_target_fortran_module_directory(
    TARGET ${PROJECT_NAME}_offline_util_objs
    MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/module/offline_util
)

foreach(program IN ITEMS

    create_grid_info
    convNetcdf2Grib
    create_init_clim
    adjust_forc
    caldtdz
    conv_forcing
    find_points)

  ecbuild_add_executable(TARGET ${PROJECT_NAME}-${program}
    SOURCES offline/util/${program}.F90
    OBJECTS ${PROJECT_NAME}_offline_util_objs
    LIBS NetCDF::NetCDF_Fortran eccodes_f90)
  ecbuild_target_fortran_module_directory(
      TARGET ${PROJECT_NAME}-${program}
      MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/module/offline_util
  )
  
endforeach()

