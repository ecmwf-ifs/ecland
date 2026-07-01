if( HAVE_LOKI )
  set( LOKI_FRONTEND "fp" CACHE STRING "Frontend parser for Loki source transformations" )
  set( LOKI_CONFIG_FILE ${CMAKE_CURRENT_SOURCE_DIR}/ecland_loki.config )

  if( NOT LOKI_MODE MATCHES "^(idem|scc|scc-stack)$" )
    ecbuild_critical( "Only LOKI_MODE=idem, scc, or scc-stack is currently configured for ecland" )
  endif()

  loki_transform_target(
    TARGET ${PROJECT_NAME}_surf ${PROJECT_NAME}-master
    MODE ${LOKI_MODE}
    FRONTEND ${LOKI_FRONTEND}
    CONFIG ${LOKI_CONFIG_FILE}
    PLAN ${CMAKE_CURRENT_BINARY_DIR}/loki_plan_ecland_surf.cmake
    CPP
    INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/function
    SOURCES
      ${CMAKE_CURRENT_SOURCE_DIR}/offline
      ${CMAKE_CURRENT_SOURCE_DIR}/module
      ${CMAKE_CURRENT_SOURCE_DIR}/external
    HEADERS
      ${CMAKE_CURRENT_SOURCE_DIR}//offline/driver/ecland_surface_type_mod.F90
      ${CMAKE_CURRENT_SOURCE_DIR}//offline/driver/ecland_atmo_type_mod.F90
      ${CMAKE_CURRENT_SOURCE_DIR}//offline/driver/ecland_aux_type_mod.F90
      ${CMAKE_CURRENT_SOURCE_DIR}//offline/driver/ecland_aux_diag_type_mod.F90
      ${CMAKE_CURRENT_SOURCE_DIR}//offline/driver/ecland_flux_type_mod.F90
      ${CMAKE_CURRENT_SOURCE_DIR}//offline/driver/ecland_ddh_type_mod.F90
      ${CMAKE_CURRENT_SOURCE_DIR}//offline/driver/ecland_climate_type_mod.F90
      ${CMAKE_CURRENT_SOURCE_DIR}//offline/driver/ecland_internal_type_mod.F90
      ${CMAKE_CURRENT_SOURCE_DIR}/module/yos_cst.F90
      ${CMAKE_CURRENT_SOURCE_DIR}/module/yos_thf.F90
      ${CMAKE_CURRENT_SOURCE_DIR}/module/yos_surf.F90
      ${CMAKE_CURRENT_SOURCE_DIR}/module/yos_soil.F90
      ${CMAKE_CURRENT_SOURCE_DIR}/module/yomsurf_ssdp_mod.F90
  )
endif()
