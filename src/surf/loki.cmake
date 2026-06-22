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
  )
endif()
