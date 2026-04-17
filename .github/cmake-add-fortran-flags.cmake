if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    add_compile_options(
        $<$<COMPILE_LANGUAGE:Fortran>:-check>
        $<$<COMPILE_LANGUAGE:Fortran>:noarg_temp_created>
    )
endif()
