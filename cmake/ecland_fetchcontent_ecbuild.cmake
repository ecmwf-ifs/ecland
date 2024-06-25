include(FetchContent)
FetchContent_Populate(
    ecbuild
    URL            https://github.com/ecmwf/ecbuild/archive/refs/tags/3.8.0.tar.gz
    SOURCE_DIR     ${CMAKE_BINARY_DIR}/ecbuild
    BINARY_DIR     ${CMAKE_BINARY_DIR}/_deps/ecbuild-build
    SUBBUILD_DIR   ${CMAKE_BINARY_DIR}/_deps/ecbuild-subbuild
  )
find_package( ecbuild 3.8 REQUIRED HINTS ${CMAKE_BINARY_DIR} )
