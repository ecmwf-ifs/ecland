macro(ecland_fetchcontent_dependencies)

if(PROJECT_IS_TOP_LEVEL)

ecbuild_add_option( FEATURE FETCHCONTENT_DEPENDENCIES
                    DESCRIPTION "Use FETCHCONTENT"
)

if (HAVE_FETCHCONTENT_DEPENDENCIES)

#### eccodes
FetchContent_Declare(
  eccodes
  URL https://github.com/ecmwf/eccodes/archive/refs/tags/2.46.0.tar.gz
  FIND_PACKAGE_ARGS
)
set( ECCODES_ENABLE_MEMFS ON )
set( ECCODES_ENABLE_TESTS OFF )
set( ECCODES_ENABLE_JPG OFF )
set( ECCODES_ENABLE_PNG OFF )
set( ECCODES_ENABLE_NETCDF OFF )
set( ECCODES_ENABLE_JPG_LIBJASPER OFF )
set( ECCODES_ENABLE_JPG_LIBOPENJPEG OFF )
set( ECCODES_ENABLE_PRODUCT_GRIB OFF )

#### fiat
FetchContent_Declare(
  fiat
  URL https://github.com/ecmwf-ifs/fiat/archive/refs/tags/1.6.1.tar.gz
  FIND_PACKAGE_ARGS
)
set( FIAT_ENABLE_TESTS OFF )
set( FIAT_ENABLE_DR_HOOK_NVTX OFF )

#### field api
FetchContent_Declare(
    field_api 
    URL            https://github.com/ecmwf-ifs/field_api/archive/refs/tags/v0.3.7.tar.gz 
    FIND_PACKAGE_ARGS
)
set( FIELD_API_ENABLE_TESTS OFF )

FetchContent_MakeAvailable(eccodes fiat field_api) # Internally calls find_package() first

endif()

endif()
endmacro()
