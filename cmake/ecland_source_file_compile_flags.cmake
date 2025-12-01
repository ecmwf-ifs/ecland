# Add here extra compile flags for specific files
# This file gets included in the directory scope where targets are created

if (CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC")

    set_source_files_properties(
            external/surfexcdriverstl.F90
            module/surfexcdriverstl_ctl_mod.F90
            module/surfexcdrivers_ctl_mod.F90
            module/surfexcdriversad_ctl_mod.F90
            module/vexcss_mod.F90
            module/vsurfs_mod.F90
            module/vupdz0s_mod.F90
            offline/driver/wrtdcdf.F90
            offline/driver/wrtpcdf.F90
        PROPERTIES OVERRIDE_COMPILE_FLAGS_BIT "-Mbyteswapio -Kieee -mp -g -O1"
    )

    set_source_files_properties(
            module/flakeene_mod.F90
            module/srfcotwo_mod.F90
        PROPERTIES OVERRIDE_COMPILE_FLAGS_BIT "-Mbyteswapio -Kieee -mp -g -O1 -Mstack_arrays"
    )

    set_source_files_properties(
            # Only to reduce compilation memory consumption
            offline/driver/cpg1s.F90
        PROPERTIES OVERRIDE_COMPILE_FLAGS_BIT "-Mbyteswapio -Kieee -mp -O1"
    )

    set_source_files_properties(
            external/surfexcdriver.F90
        PROPERTIES OVERRIDE_COMPILE_FLAGS_BIT "-Mbyteswapio -Kieee -mp -g -O2 -Mvect=idiom -Mflushz -Mno-signed-zeros"
    )

    set_source_files_properties(
            module/vupdz0_mod.F90
            module/sppcfl_mod.F90
        PROPERTIES OVERRIDE_COMPILE_FLAGS_BIT "-Mbyteswapio -Kieee -mp -g -O1 -Mvect=idiom -Mflushz -Mno-signed-zeros -Mstack_arrays"
    )

    set_source_files_properties(
            module/srfwdif_mod.F90
        PROPERTIES OVERRIDE_COMPILE_FLAGS_BIT "-Mbyteswapio -Kieee -mp -g -O1 -Mflushz -Mno-signed-zeros -Mstack_arrays"
    )

    set_source_files_properties(
            module/vsurf_mod.F90
            module/vexcs_mod.F90
            module/voskin_mod.F90
            module/srft_mod.F90
            module/surfexcdriver_ctl_mod.F90
        PROPERTIES OVERRIDE_COMPILE_FLAGS_BIT "-Mbyteswapio -Kieee -mp -g -O1 -tp=host -Wlopt,'-passes=default<O2>' -Wllc,-O1"
    )

    if( CMAKE_BUILD_TYPE MATCHES "Debug" )
        # TODO: we need this because compilation with nvhpc hangs interminably when
        # debugging symbls are requested for cpg1s.F90, so we disable it for debug builds.
        # This may be fixed with the introduction of the memory blocking, so this should
        # be revisited then.
        set_source_files_properties(
                offline/driver/cpg1s.F90
            PROPERTIES COMPILE_OPTIONS "-Mnoopenmp"
        )
    endif()

endif()

# See ifs-source/cmake/compile_flags.cmake for more flags that may be needed!
