# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.# ----------------------------------------------------------------------------------------
# cmflood

list(APPEND cmflood_src
    cmf_calc_diag_mod.F90
    cmf_ctrl_damout_mod.F90
    cmf_calc_fldstg_mod.F90
    cmf_calc_outflw_mod.F90
    cmf_calc_pthout_mod.F90
    cmf_calc_stonxt_mod.F90
    cmf_ctrl_boundary_mod.F90
    cmf_ctrl_forcing_mod.F90
    cmf_ctrl_levee_mod.F90
    cmf_ctrl_maps_mod.F90
    cmf_ctrl_nmlist_mod.F90
    cmf_ctrl_output_mod.F90
    cmf_ctrl_physics_mod.F90
    cmf_ctrl_restart_mod.F90
    cmf_ctrl_time_mod.F90
    cmf_ctrl_vars_mod.F90
    cmf_drv_advance_mod.F90
    cmf_drv_control_mod.F90
    cmf_opt_outflw_mod.F90
    cmf_utils_mod.F90
    yos_cmf_diag.F90
    yos_cmf_input.F90
    yos_cmf_map.F90
    yos_cmf_prog.F90
    yos_cmf_time.F90
)
list(TRANSFORM cmflood_src PREPEND cmflood/)

ecbuild_add_library( TARGET ${PROJECT_NAME}_cmflood
    SOURCES ${cmflood_src}
    PRIVATE_LIBS fiat parkind
                 NetCDF::NetCDF_Fortran ${OpenMP_Fortran_LIBRARIES}
    PRIVATE_DEFINITIONS UseCDF_CMF
)

ecbuild_target_fortran_module_directory(
    TARGET ${PROJECT_NAME}_cmflood
    MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module/${PROJECT_NAME}
    INSTALL_MODULE_DIRECTORY module/${PROJECT_NAME}
)
