# (C) Copyright 2022- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


# Capture ecbuild flags set by a toolchain
set( ${PNAME}_Fortran_FLAGS "${ECBUILD_Fortran_FLAGS} " )
set( ${PNAME}_Fortran_FLAGS_BIT "${ECBUILD_Fortran_FLAGS_BIT} " )
set( ${PNAME}_Fortran_FLAGS_DEBUG "${ECBUILD_Fortran_FLAGS_DEBUG} " )

if(CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
  set(checkbounds_flags   "-Rb")
  set(fpe_flags           "-Ktrap=fp")
  set(initsnan_flags      "-ei")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(linelength_flags    "-ffree-line-length-none")
  set(checkbounds_flags   "-fcheck=bounds")
  set(fpe_flags           "-ffpe-trap=invalid,zero,overflow")
  set(initsnan_flags      "-finit-real=snan")
  set(vectorization_flags "-march=native")

  ecbuild_add_fortran_flags( "-fno-range-check" NAME no_range_check )

  # Needed to guarantee matching test results with Debug build
  set(fpmodel_flags       "-ffp-contract=off")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(checkbounds_flags   "-check bounds")
  set(initsnan_flags      "-init=snan")
  set(fpe_flags           "-fpe0")
  set(vectorization_flags "-march=core-avx2 -no-fma")
  if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "IntelLLVM")
    set(transcendentals_flags "-fast-transcendentals -ftz")
  endif()
  set(optimization_flags  "-O2")

  # Needed to guarantee matching test results with Debug build
  set(fpmodel_flags       "-fp-model precise -fp-speculation=safe")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "PGI|NVHPC")
  set(endian_flags        "-Mbyteswapio")
  set(checkbounds_flags   "-Mbounds")
  set(fpe_flags           "-Ktrap=fp")
  set(initsnan_flags      "-Minit-real=snan")
  set(optimization_flags  "-g -O3 -fast")

  # Remove fp trapping flags from optimised builds in case they are present in the toolchain
  string(REPLACE "-Ktrap=fp" "" ${PNAME}_Fortran_FLAGS ${${PNAME}_Fortran_FLAGS})

  # Needed to guarantee matching test results with Debug build
  set(fpmodel_flags       "-Kieee")

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Flang")
  set(fpe_flags           "-ffp-exception-behavior=strict")

endif()

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

ecbuild_add_fortran_flags( "-g -O0"   NAME base_debug BUILD DEBUG)
if(DEFINED fpmodel_flags)
  ecbuild_add_fortran_flags( "${fpmodel_flags}" NAME fpmodel )
endif()
if(DEFINED endian_flags)
  ecbuild_add_fortran_flags( "${endian_flags}" NAME endian )
endif()
if(DEFINED transcendentals_flags)
  ecbuild_add_fortran_flags( "${transcendentals_flags}"   NAME transcendentals BUILD BIT)
endif()

# Only add optimisation flags if they're not already present in ${PNAME}_Fortran_FLAGS
if( DEFINED optimization_flags )
  string(FIND "${${PNAME}_Fortran_FLAGS} ${${PNAME}_Fortran_FLAGS_BIT}" ${optimization_flags} _pos)
  if( _pos EQUAL -1)
    # optimization flags must be per-sourcefile overrideable, so are set via ${PNAME}_Fortran_FLAGS
    set( ${PNAME}_Fortran_FLAGS_BIT "${${PNAME}_Fortran_FLAGS_BIT} ${optimization_flags}" )
  endif()
endif()

if(DEFINED vectorization_flags)
  # vectorization flags must be per-sourcefile overrideable, so are set via ${PNAME}_Fortran_FLAGS
  set( ${PNAME}_Fortran_FLAGS_BIT "${${PNAME}_Fortran_FLAGS_BIT} ${vectorization_flags}" )
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
