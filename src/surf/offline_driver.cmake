# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.
# ----------------------------------------------------------------------------------------
# master

ecbuild_generate_fortran_interfaces(
    TARGET ${PROJECT_NAME}_offline_driver_intfb
    DIRECTORIES offline/driver
    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}
    DESTINATION intfb
    INCLUDE_DIRS {PROJECT_NAME}_offline_driver_intfb_includes
    PARALLEL ${FCM_PARALLEL}
)

list(APPEND offline_driver_src
    callpar1s.F90
    cnt01s.F90
    cnt21s.F90
    cnt31s.F90
    cnt41s.F90
    cntend.F90
    cpg1s.F90
    dattim.F90
    dtforc.F90
#    ibm.F90 # Already included in surf
    incdat.F90
    minmax.F90
    netcdf_utils.F90
#    parkind1.F90 # Already included in fiat
    ptrgp1s.F90
    ptrgpd1s.F90
    rdclim.F90
    rdcoor.F90
    rdfvar.F90
    rdnml_params.F90
    rdres.F90
    rdssdp.F90
    rdsupr.F90
    stepo1s.F90
    su0phy1s.F90
    su0yom1s.F90
    su1s.F90
    sucdfres.F90
    sucdh1s.F90
    sucst.F90
    suct01s.F90
    sudcdf.F90
    sudim1s.F90
    sudyn1s.F90
    sufcdf.F90
    sugc1s.F90
    sugdi1s.F90
    sugp1s.F90
    sugpd1s.F90
    suinif1s.F90
    sulun1s.F90
    sulwn.F90
    suoptsurf.F90
    supcdf.F90
    suphec.F90
    surdi.F90
    surdi1s.F90
    surip.F90
    suswn.F90
    suvdf.F90
    suvdfs.F90
    updcal.F90
    upddiag.F90
    updtim1s.F90
    vdfdifc.F90
    vdfdifh1s.F90
    vdfdifm1s.F90
    vdfincr.F90
    vdfmain1s.F90
    wrtclim.F90
    wrtd1s.F90
    wrtd2cdf.F90
    wrtdcdf.F90
    wrtp1s.F90
    wrtpcdf.F90
    wrtres.F90
    yoelw.F90
    yoeoptsurf.F90
    yoephy.F90
    yoerad.F90
    yoerdi.F90
    yoerdi1s.F90
    yoerip.F90
    yoesoil1s.F90
    yoesw.F90
    yoethf.F90
    yoevdf.F90
    yoevdfs.F90
    yomcc1s.F90
    yomcdh1s.F90
    yomcst.F90
    yomct01s.F90
    yomdim1s.F90
    yomdphy.F90
    yomdyn1s.F90
    yomforc1s.F90
    yomgc1s.F90
    yomgdi1s.F90
    yomgf1s.F90
    yomgp1s0.F90
    yomgp1s1.F90
    yomgp1sa.F90
    yomgpd1s.F90
    yomjfh.F90
    yomlog1s.F90
    yomlun1s.F90
    yomrip.F90
)
list(TRANSFORM offline_driver_src PREPEND offline/driver/)

ecbuild_add_executable(TARGET ${PROJECT_NAME}-master
  SOURCES offline/master1s.F90
          ${offline_driver_src}
  INCLUDES
    offline/function
    offline/namelist
    interface
  LIBS 
    ${PROJECT_NAME}_offline_driver_intfb ${PROJECT_NAME}_surf ${PROJECT_NAME}_cmflood
    fiat parkind
    ${OpenMP_Fortran_LIBRARIES}
    NetCDF::NetCDF_Fortran
)
ecbuild_target_fortran_module_directory(
    TARGET ${PROJECT_NAME}-master
    MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/module/offline_driver
)
