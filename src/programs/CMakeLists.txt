# (C) Copyright 2023- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

string( TOUPPER ${PROJECT_NAME} PNAME )
foreach( lang Fortran C CXX )
    set( ${lang}_flags "${CMAKE_${lang}_FLAGS} ${CMAKE_${lang}_FLAGS_${CMAKE_BUILD_TYPE_CAPS}} ${${PNAME}_${lang}_FLAGS} ${${PNAME}_${lang}_FLAGS_${CMAKE_BUILD_TYPE_CAPS}}" )
    string(REGEX REPLACE "[ ]+" " " ${lang}_flags ${${lang}_flags})
    string(STRIP "${${lang}_flags}" ${lang}_flags)
endforeach()
set( PROJECT_VERSION     ${${PROJECT_NAME}_VERSION} )
set( PROJECT_VERSION_STR ${${PROJECT_NAME}_VERSION_STR} )
set( PROJECT_GIT_SHA1    ${${PROJECT_NAME}_GIT_SHA1} )
if( HAVE_SINGLE_PRECISION )
  set( PROJECT_PRECISION single )
else()
  set( PROJECT_PRECISION double )
endif()
configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/ecland.in ${CMAKE_BINARY_DIR}/bin/${ECLAND_PROJECT_NAME} @ONLY )

install( FILES
  ${CMAKE_BINARY_DIR}/bin/${ECLAND_PROJECT_NAME}
  DESTINATION
  ${INSTALL_BIN_DIR}
  PERMISSIONS
  OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)
