macro(ecland_fetchcontent_dependencies)

if(PROJECT_IS_TOP_LEVEL)

ecbuild_add_option( FEATURE FETCHCONTENT_DEPENDENCIES
                    DESCRIPTION "Use FETCHCONTENT"
)

if (HAVE_FETCHCONTENT_DEPENDENCIES)

#### eccodes
FetchContent_Declare(
  eccodes
  URL https://github.com/ecmwf/eccodes/archive/refs/tags/2.31.0.tar.gz
  FIND_PACKAGE_ARGS
)
set( ECCODES_ENABLE_MEMFS ON )
set( ECCODES_ENABLE_TESTS OFF )


#### fiat
FetchContent_Declare(
  fiat
  URL https://github.com/ecmwf-ifs/fiat/archive/refs/tags/1.5.1.tar.gz
  FIND_PACKAGE_ARGS
)
set( FIAT_ENABLE_TESTS OFF )

FetchContent_MakeAvailable(eccodes fiat) # Internally calls find_package() first

endif()

endif()
endmacro()
