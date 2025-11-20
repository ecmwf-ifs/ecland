# (C) Copyright 2022- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


#ecbuild_add_fortran_flags( "-ffree-line-length-none" NAME line_length )

if(HAVE_SINGLE_PRECISION AND CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  ecbuild_add_fortran_flags( "-fno-range-check" NAME no_range_check )
      #   sppcflstl_mod.F90:444:17:
      #
      #   444 |         Z4EXP5 = EXP(200.0_JPRD)
      #       |                 1
      # Error: Arithmetic overflow converting REAL(8) to REAL(4) at (1). This check can be disabled with the option '-fno-range-check'
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
  set(autopromote_flags   "-sreal64")
  set(checkbounds_flags   "-Rb")
  set(fpe_flags           "-Ktrap=fp")
  set(initsnan_flags      "-ei")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(linelength_flags    "-ffree-line-length-none")
  set(autopromote_flags   "-fdefault-real-8 -fdefault-double-8")
  set(checkbounds_flags   "-fcheck=bounds")
  set(fpe_flags           "-ffpe-trap=invalid,zero,overflow")
  set(initsnan_flags      "-finit-real=snan")

  # Needed to guarantee matching test results with Debug build
  set(fpmodel_flags       "-ffp-contract=off")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(autopromote_flags   "-real-size 64")
  set(checkbounds_flags   "-check bounds")
  set(initsnan_flags      "-init=snan")
  set(fpe_flags           "-fpe0")
  ecbuild_add_fortran_flags( "-diag-disable=7713" NAME unused_statement_function_remark )

  # Needed to guarantee matching test results with Debug build
  set(fpmodel_flags       "-fp-model=precise")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC")
  set(autopromote_flags   "-r8")
  set(checkbounds_flags   "-Mbounds")
  set(fpe_flags           "-Ktrap=fp")
  set(initsnan_flags      "-Minit-real=snan")

  # Needed to guarantee matching test results with Debug build
  set(fpmodel_flags       "-Kieee")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Flang")
  set(autopromote_flags   "-fdefault-real-8")
  set(fpe_flags           "-ffp-exception-behavior=strict")

endif()

# if( NOT HAVE_SINGLE_PRECISION )
#   ecbuild_add_fortran_flags( "${autopromote_flags}"   NAME autopromote )
# endif()
if(linelength_flags)
  ecbuild_add_fortran_flags( "${linelength_flags}" NAME linelength )
endif()

if( CMAKE_BUILD_TYPE MATCHES "Debug" )
  foreach( debug_flag    fpe initsnan checkbounds )
    if( ${debug_flag}_flags )
      ecbuild_add_fortran_flags( "${${debug_flag}_flags}" NAME ${debug_flag} )
    endif()
  endforeach()
  if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    ecbuild_add_fortran_flags( "-check noarg_temp_created" NAME arg_temp_created )
  endif()
endif()

if(DEFINED fpmodel_flags)
  ecbuild_add_fortran_flags( "${fpmodel_flags}" NAME fpmodel )
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  if( NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 10 )
    ecbuild_add_fortran_flags( "-fallow-argument-mismatch" NAME argument_mismatch )
  endif()
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "Flang")
  # Linker complains of unknown arguments:
  #    warning: argument unused during compilation: '-fdefault-real-8' [-Wunused-command-line-argument]
  foreach( LINKER_FLAGS CMAKE_EXE_LINKER_FLAGS CMAKE_SHARED_LINKER_FLAGS CMAKE_STATIC_LINKER_FLAGS )
    set( ${LINKER_FLAGS} "${${LINKER_FLAGS}} -Wno-unused-command-line-argument")
  endforeach()
endif()
