if( HAVE_LOKI )
  set( LOKI_FRONTEND "fp" CACHE STRING "Frontend parser for Loki source transformations" )

  if( NOT LOKI_MODE STREQUAL "idem" )
    ecbuild_critical( "Only LOKI_MODE=idem is currently configured for ecland" )
  endif()

  foreach( prec sp dp )
    if( HAVE_${prec} )

      # Pre-process ecland_loki.config file, since the 'lib' attribute needs to
      # match the target(s) and therefore the precision 'prec' is necessary to be correct.
      set( LOKI_CONFIG_FILE ${CMAKE_CURRENT_BINARY_DIR}/ecland_loki.config.${prec} )
      set( LOKI_PLAN_FILE ${CMAKE_CURRENT_BINARY_DIR}/loki_plan_ecland_surf.${prec}.cmake )
      configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/ecland_loki.config.in ${LOKI_CONFIG_FILE} @ONLY )

      loki_transform_target(
        TARGET ${PROJECT_NAME}_surf_${prec} ${PROJECT_NAME}-master-${prec}
        MODE ${LOKI_MODE}
        FRONTEND ${LOKI_FRONTEND}
        CONFIG ${LOKI_CONFIG_FILE}
        PLAN ${LOKI_PLAN_FILE}
        CPP
        INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/function
        SOURCES
          ${CMAKE_CURRENT_SOURCE_DIR}/offline
          ${CMAKE_CURRENT_SOURCE_DIR}/module
          ${CMAKE_CURRENT_SOURCE_DIR}/external
      )

    endif()
  endforeach()
endif()
