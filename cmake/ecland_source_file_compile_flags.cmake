# Add here extra compile flags for specific files
# This file gets included in the directory scope where targets are created

if (CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC")

    if( NOT CMAKE_BUILD_TYPE MATCHES "Debug" )
        set_source_files_properties(
                external/surfexcdriverstl.F90
                module/surfexcdriverstl_ctl_mod.F90
            PROPERTIES COMPILE_OPTIONS "-g;-O1"
        )
    endif()

endif()

# See ifs-source/cmake/compile_flags.cmake for more flags that may be needed!
