# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.
# ----------------------------------------------------------------------------------------
# surf

list( APPEND module_src
    abort_surf_mod.F90
    bvoc_emis_mod.F90
    canalb_mod.F90
    ccetr_mod.F90
    cotwo_mod.F90
    cotworestress_mod.F90
    cptave_mod.F90
    farquhar_mod.F90
    flake_driver_mod.F90
    flakeene_mod.F90
    flakerad_mod.F90
    kpp_abk80_mod.F90
    kpp_bldepth_mod.F90
    kpp_blmix_mod.F90
    kpp_cpsw_mod.F90
    kpp_interior_mix_mod.F90
    kpp_kppmix_mod.F90
    kpp_ocnint_mod.F90
    kpp_swfrac_mod.F90
    kpp_tridcof_mod.F90
    kpp_tridmat_mod.F90
    kpp_tridrhs_mod.F90
    kpp_wscale_mod.F90
    nitro_decline_mod.F90
    oc_mlm_mod.F90
    ocean_ml_driver_mod.F90
    ocean_ml_driver_v2_mod.F90
    source_e_mod.F90
    sppcfl_mod.F90
    sppcfls_mod.F90
    sppcflsad_mod.F90
    sppcflstl_mod.F90
    sppgust_mod.F90
    srfbvoc_mod.F90
    srfcotwo_mod.F90
    srfene_mod.F90
    srfi_mod.F90
    srfil_mod.F90
    srfils_mod.F90
    srfilsad_mod.F90
    srfilstl_mod.F90
    srfis_mod.F90
    srfisad_mod.F90
    srfistl_mod.F90
    srfrcg_mod.F90
    srfrcgs_mod.F90
    srfrcgsad_mod.F90
    srfrcgstl_mod.F90
    srfrootfr_mod.F90
    srfsn_asn_mod.F90
    srfsn_driver_mod.F90
    srfsn_lwexp_mod.F90
    srfsn_lwimp_mod.F90
    srfsn_lwimpmls_mod.F90
    srfsn_lwimps_mod.F90
    srfsn_lwimpsad_mod.F90
    srfsn_lwimpstl_mod.F90
    srfsn_mod.F90
    srfsn_regrid_mod.F90
    srfsn_rsn_mod.F90
    srfsn_ssrabs_mod.F90
    srfsn_ssrabss_mod.F90
    srfsn_ssrabssad_mod.F90
    srfsn_ssrabsstl_mod.F90
    srfsn_vgrid_mod.F90
    srfsn_webal_mod.F90
    srfsn_webals_mod.F90
    srfsn_webalsad_mod.F90
    srfsn_webalstl_mod.F90
    srft_mod.F90
    srfts_mod.F90
    srftsad_mod.F90
    srftstl_mod.F90
    srfvegevol_mod.F90
    srfwdif_mod.F90
    srfwdifs_mod.F90
    srfwdifsad_mod.F90
    srfwdifstl_mod.F90
    srfwexc_mod.F90
    srfwexc_vg_mod.F90
    srfwinc_mod.F90
    srfwl_mod.F90
    srfwls_mod.F90
    srfwlsad_mod.F90
    srfwlstl_mod.F90
    srfwng_mod.F90
    susdp_deriv_ctl_mod.F90
    sussurf_params.F90
    sucotwo_mod.F90
    sufarquhar_mod.F90
    sugridmlm_mod.F90
    surfbc_ctl_mod.F90
    surfexcdriver_ctl_mod.F90
    surfexcdrivers_ctl_mod.F90
    surfexcdriversad_ctl_mod.F90
    surfexcdriverstl_ctl_mod.F90
    surfpp_ctl_mod.F90
    surfpps_ctl_mod.F90
    surfppsad_ctl_mod.F90
    surfppstl_ctl_mod.F90
    surfrad_ctl_mod.F90
    surfseb_ctl_mod.F90
    surfsebs_ctl_mod.F90
    surfsebsad_ctl_mod.F90
    surfsebstl_ctl_mod.F90
    surftstp_ctl_mod.F90
    surftstps_ctl_mod.F90
    surftstpsad_ctl_mod.F90
    surftstpstl_ctl_mod.F90
    surfws_ctl_mod.F90
    surfws_fgprof.F90
    surfws_init_ml_mod.F90
    surfws_init_mloff_mod.F90
    surfws_init_sl_mod.F90
    surfws_massadj_mod.F90
    surfws_tsnadj_mod.F90
    surwn_mod.F90
    susbvoc_mod.F90
    suscst_mod.F90
    susdp_dflt_ctl_mod.F90
    susflake_mod.F90
    susocean_ml_mod.F90
    susrad_mod.F90
    sussoil_mod.F90
    susthf_mod.F90
    susurb_mod.F90
    susurf_ctl_mod.F90
    susveg_mod.F90
    suvexc_mod.F90
    suvexcs_mod.F90
    tridag_mod.F90
    vevap_mod.F90
    vevaps_mod.F90
    vevapsad_mod.F90
    vevapstl_mod.F90
    vexcs_mod.F90
    vexcss_mod.F90
    vexcssad_mod.F90
    vexcsstl_mod.F90
    vlamsk_mod.F90
    voskin_mod.F90
    vsflx_mod.F90
    vsurf_mod.F90
    vsurfs_mod.F90
    vsurfsad_mod.F90
    vsurfstl_mod.F90
    vupdz0_mod.F90
    vupdz0s_mod.F90
    vupdz0sad_mod.F90
    vupdz0stl_mod.F90
    yomsurf_ssdp_mod.F90
    yos_agf.F90
    yos_ags.F90
    yos_bvoc.F90
    yos_cst.F90
    yos_dim.F90
    yos_exc.F90
    yos_excs.F90
    yos_flake.F90
    yos_lw.F90
    yos_mlm.F90
    yos_nampars1.F90
    yos_ocean_ml.F90
    yos_rad.F90
    yos_rdi.F90
    yos_soil.F90
    yos_surf.F90
    yos_sw.F90
    yos_thf.F90
    yos_urb.F90
    yos_veg.F90
    ecphys_state_type_mod.F90
    ecphys_flux_type_mod.F90
    ecphys_surface_type_mod.F90
    ecphys_aux_diag_type_mod.F90
    ecphys_aux_type_mod.F90
    yomphyder.F90
)
list(TRANSFORM module_src PREPEND module/)

list(APPEND external_src
    surf_inq.F90
    surfbc.F90
    surfdeallo.F90
    surfexcdriver.F90
    surfexcdrivers.F90
    surfexcdriversad.F90
    surfexcdriverstl.F90
    surfpp.F90
    surfpps.F90
    surfppsad.F90
    surfppstl.F90
    surfrad.F90
    surfseb.F90
    surfsebs.F90
    surfsebsad.F90
    surfsebstl.F90
    surftstp.F90
    surftstps.F90
    surftstpsad.F90
    surftstpstl.F90
    surfws.F90
    susdp_deriv.F90
    susdp_dflt.F90
    susurf.F90
)
list(TRANSFORM external_src PREPEND external/)

ecbuild_add_library( TARGET ${PROJECT_NAME}_surf 
    SOURCES ${module_src}
            offline/driver/ibm.F90 # dummies for IBM vmass library
            ${external_src}
    PUBLIC_LIBS fiat parkind
    PRIVATE_LIBS ${OpenMP_Fortran_LIBRARIES}
    PRIVATE_INCLUDES function
)

ecbuild_target_fortran_module_directory(
    TARGET ${PROJECT_NAME}_surf
    MODULE_DIRECTORY         ${CMAKE_BINARY_DIR}/module/${PROJECT_NAME}
    INSTALL_MODULE_DIRECTORY module/${PROJECT_NAME}
)
