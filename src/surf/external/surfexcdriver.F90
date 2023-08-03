SUBROUTINE SURFEXCDRIVER    (YDSURF, CDCONF &
 & , KIDIA, KFDIA, KLON, KLEVS, KTILES, KVTYPES, KDIAG, KSTEP &
 & , KLEVSN, KLEVI,LDLAND, KDHVTLS, KDHFTLS, KDHVTSS, KDHFTSS &
 & , KDHVTTS, KDHFTTS, KDHVTIS, KDHFTIS, K_VMASS &
 & , KDHVCO2S,KDHFCO2S,KDHVBVOCS,KDHVVEGS,KDHFVEGS &
 & , PTSTEP,PTSTEPF &
 & , PPPFD_TOA &
! input data, non-tiled
 & , KTVL, KCO2TYP, KTVH, PCVL, PCVH, PCUR &
 & , PLAIL, PLAIH &
 & , PLAILP, PLAIHP, PAVGPAR, PISOP_EP &
 & , PFWET, PLAT &
 & , PSNM , PRSN &
 & , PMU0 , PCARDI &
 & , PUMLEV, PVMLEV, PTMLEV, PQMLEV, PCMLEV, PAPHMS, PGEOMLEV, PCPTGZLEV &
 & , PSST, PTSKM1M, PCHAR, PCHARHQ, PSSRFL, PSLRFL, PEMIS, PTICE, PTSN &
 & , PHLICE,PTLICE,PTLWML & 
 & , PTHKICE,PSNTICE &
 & , PWLMX, PUCURR, PVCURR, PI10FGCV &
 & , PSSDP2, PSSDP3 &
! input data, soil
 & , PTSAM1M, PWSAM1M, KSOTY &
! input data, tiled
 & , PFRTI, PALBTI &
! updated data, tiled
 & , PUSTRTI, PVSTRTI, PAHFSTI, PEVAPTI, PTSKTI &
 & , PANDAYVT,PANFMVT &
! updated data, non-tiled
 & , PZ0M, PZ0H &
! output data, tiled
 & , PSSRFLTI, PQSTI, PDQSTI, PCPTSTI, PCFHTI, PCFQTI, PCSATTI, PCAIRTI &
 & , PCPTSTIU, PCSATTIU, PCAIRTIU,PRAQTI,PTSRF,PLAMSK &
 & , PZ0MTIW, PZ0HTIW, PZ0QTIW, PZDLTI, PQSAPPTI, PCPTSPPTI &
! output data, non-tiled
 & , PKHLEV, PKCLEV, PCFMLEV, PKMFL, PKHFL, PKQFL, PEVAPSNW &
 & , PZ0MW, PZ0HW, PZ0QW, PBLENDPP, PCPTSPP, PQSAPP, PBUOMPP, PZDLPP &
! output data, non-tiled CO2
 & , PAN,PAG,PRD,PRSOIL_STR,PRECO,PCO2FLUX,PCH4FLUX&
! output data: Biogenic VOC (BVOC) emissions
 & , PBVOCFLUX &
! output canopy and bare soild resistance 
 & , PWETB, PWETL, PWETLU, PWETH, PWETHS &
! output data, diagnostics
 & , PDHTLS, PDHTSS, PDHTTS, PDHTIS &
 & , PDHVEGS, PEXDIAG, PDHCO2S,PDHBVOCS &
 & , PRPLRG &
 & , LSICOUP, LDSICE, LBLEND &
 & )

USE PARKIND1, ONLY : JPIM, JPRB
USE ISO_C_BINDING

!ifndef INTERFACE

USE YOMHOOK, ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_SURF, ONLY : TSURF, GET_SURF

USE ABORT_SURF_MOD
USE SURFEXCDRIVER_CTL_MOD

!endif INTERFACE

! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!------------------------------------------------------------------------

!  PURPOSE:
!    Routine SURFEXCDRIVER controls the ensemble of routines that prepare
!    the surface exchange coefficients and associated surface quantities
!    needed for the solution of the vertical diffusion equations. 

!  SURFEXCDRIVER is called by VDFMAIN

!  METHOD:
!    This routine is only a shell needed by the surface library
!    externalisation.

!  AUTHOR:
!    P. Viterbo       ECMWF May 2005   

!  REVISION HISTORY:
!    E. Dutra/G. Balsamo May 2008 Add lake tile
!    S. Boussetta/G.Balsamo May 2009 Add lai
!    S. Boussetta/G.Balsamo May 2010 Add CTESSEL
!    N. Semane  04-10-2012  Include small planet PRPLRG
!    Linus Magnusson     10-09-28 Sea-ice

!  INTERFACE: 

!    Characters (In):
!      CDCONF   :    IFS Configuration

!    Integers (In):
!      KIDIA    :    Begin point in arrays
!      KFDIA    :    End point in arrays
!      KLON     :    Length of arrays
!      KLEVS    :    Number of soil layers
!      KTILES   :    Number of tiles
!      KVTYPES  :    Number of biomes for carbon
!      KDIAG    :    Number of diagnostic parameters
!      KSTEP    :    Time step index
!      KLEVSN   :    Number of snow layers (diagnostics) 
!      KLEVI    :    Number of sea ice layers (diagnostics)
!      KDHVTLS  :    Number of variables for individual tiles
!      KDHFTLS  :    Number of fluxes for individual tiles
!      KDHVTSS  :    Number of variables for snow energy budget
!      KDHFTSS  :    Number of fluxes for snow energy budget
!      KDHVTTS  :    Number of variables for soil energy budget
!      KDHFTTS  :    Number of fluxes for soil energy budget
!      KDHVTIS  :    Number of variables for sea ice energy budget
!      KDHFTIS  :    Number of fluxes for sea ice energy budget
!      K_VMASS  :    Controls the use of vector functions in the IBM scientific
!                     library. Set K_VMASS=0 to use standard functions
!      KTVL     :    Dominant low vegetation type
!      KCO2TYP :    Type of photosynthetic pathway for low vegetation (c3/c4)
!      KTVH     :    Dominant high vegetation type
!      KSOTY    :    SOIL TYPE                                        (1-7)
!      LBLEND   :    Option to make blending heigh function of z0m (Logical)

!    *KDHVCO2S*     Number of variables for CO2
!    *KDHFCO2S*     Number of fluxes for CO2
!    *KDHVBVOCS*    Number of variables for BVOC
!    *KDHVVEGS*     Number of variables for vegetation
!    *KDHFVEGS*     Number of fluxes for vegetation


!    Reals (In):
!      PTSTEP   :    Timestep
!      PCVL     :    Low vegetation fraction
!      PCVH     :    High vegetation fraction
!      PCUR     :    Urban cover                                      (0-1)
!      PLAIL    :    Low vegetation LAI
!      PLAIH    :    High vegetation LAI
!      PLAILP   :    Low vegetation LAI previous time step
!      PLAIHP   :    High vegetation LAI previous time step
!      PAVGPAR  :    Average PAR
!      PISOP_EP :    Isoprene Emission Potential 

!     PSNM      :       SNOW MASS (per unit area)                      kg/m**2
!     PRSN      :      SNOW DENSITY                                   kg/m**3

!      PMU0          : COS SOLAR angle
!      PCARDI        : CONCENTRATION ATMOSPHERIC CO2
!      PRPLRG    :   ! GRAVITY SMALL PLANET FACTOR 

!    Reals with tile index (In): 
!      PFRTI    :    TILE FRACTIONS                                   (0-1)
!            1 : WATER                  5 : SNOW ON LOW-VEG+BARE-SOIL
!            2 : ICE                    6 : DRY SNOW-FREE HIGH-VEG
!            3 : WET SKIN               7 : SNOW UNDER HIGH-VEG
!            4 : DRY SNOW-FREE LOW-VEG  8 : BARE SOIL
!      PALBTI   :    Tile albedo                                      (0-1)

!    Reals independent of tiles (In):
!      PUMLEV   :    X-VELOCITY COMPONENT, lowest atmospheric level   m/s
!      PVMLEV   :    Y-VELOCITY COMPONENT, lowest atmospheric level   m/s
!      PTMLEV   :    TEMPERATURE,   lowest atmospheric level          K
!      PQMLEV   :    SPECIFIC HUMIDITY                                kg/kg
!      PCMLEV   :    ATMOSPHERIC CO2                                  kg/kg
!      PAPHMS   :    Surface pressure                                 Pa
!      PGEOMLEV :    Geopotential, lowest atmospehric level           m2/s2
!      PCPTGZLEV:    Geopotential, lowest atmospehric level           J/kg
!      PSST     :    (OPEN) SEA SURFACE TEMPERATURE                   K
!      PTSKM1M  :    SKIN TEMPERATURE                                 K
!      PCHAR    :    CHARNOCK PARAMETER                               -
!      PCHARHQ  :    CHARNOCK PARAMETER FOR HEAT AND MOISTURE         -
!      PSSRFL   :    NET SHORTWAVE RADIATION FLUX AT SURFACE          W/m2
!      PSLRFL   :    NET LONGWAVE RADIATION FLUX AT SURFACE           W/m2
!      PEMIS    :    MODEL SURFACE LONGWAVE EMISSIVITY
!      PTSAM1M  :    SURFACE TEMPERATURE                              K
!      PWSAM1M  :    SOIL MOISTURE ALL LAYERS                         m**3/m**3
!      PTICE    :    Ice temperature, top slab                        K
!      PTSN     :    Snow temperature                                 K
!      PHLICE   :    Lake ice thickness                               m
!      PTLICE   :    Lake ice temperature                             K
!      PTLWML   :    Lake mean water temperature                      K
!      PTHKICE  :    Sea-ice thickness                                m
!      PSNTICE  :    Snow thickness on sea-ice                        m
!      PWLMX    :    Maximum interception layer capacity              kg/m**2
!      PUCURR   :    u component of ocean surface current             m/s
!      PVCURR   :    v component of ocean surface current             m/s
!      PI10FGCV :    gust velocity from deep convection               m/s
!    Logicals independent of tiles (In):
!     LDLAND    :    LAND SEA MASK INDICATOR                       -

!    Reals with tile index (In/Out):
!      PUSTRTI  :    SURFACE U-STRESS                                 N/m2 
!      PVSTRTI  :    SURFACE V-STRESS                                 N/m2
!      PAHFSTI  :    SURFACE SENSIBLE HEAT FLUX                       W/m2
!      PEVAPTI  :    SURFACE MOISTURE FLUX                            KG/m2/s
!      PTSKTI   :    SKIN TEMPERATURE                                 K

!    	UPDATED PARAMETERS FOR VEGETATION TYPES (REAL):
!      PANDAYVT :    DAILY NET CO2 ASSIMILATION OVER CANOPY           kg_CO2/m**2
!      PANFMVT  :    MAXIMUM LEAF ASSIMILATION                        kg_CO2/kg_Air m/s

!    Reals independent of tiles (In/Out):
!      PZ0M     :    AERODYNAMIC ROUGHNESS LENGTH                     m
!      PZ0H     :    ROUGHNESS LENGTH FOR HEAT                        m

!    Reals with tile index (Out):
!      PSSRFLTI :    Tiled NET SHORTWAVE RADIATION FLUX AT SURFACE    W/m2
!      PQSTI    :    Tiled SATURATION Q AT SURFACE                    kg/kg
!      PDQSTI   :    Tiled DERIVATIVE OF SATURATION Q-CURVE           kg/kg/K
!      PCPTSTI  :    Tiled DRY STATIC ENERGY AT SURFACE               J/kg
!      PCFHTI   :    Tiled EXCHANGE COEFFICIENT AT THE SURFACE        ????
!      PCFQTI   :    Tiled EXCHANGE COEFFICIENT AT THE SURFACE        ????
!      PCSATTI  :    MULTIPLICATION FACTOR FOR QS AT SURFACE          -
!                      FOR SURFACE FLUX COMPUTATION
!      PCAIRTI  :    MULTIPLICATION FACTOR FOR Q AT  LOWEST MODEL     - 
!                      LEVEL FOR SURFACE FLUX COMPUTATION
!      PCPTSTIU :    AS PCPTSTI FOR UNSTRESSED LOW VEGETATION         J/kg
!      PCSATTIU :    AS PCSATTI FOR UNSTRESSED LOW VEGETATION         -
!      PCAIRTIU :    AS PCAIRTI FOR UNSTRESSED LOW VEGETATION         - 
!      PRAQTI   :    Aerodynamic resistance                           - 
!      PTSRF    :    Tiled surface temperature for each tile 
!                    Boundary condition in surfseb                    K
!      PLAMSK   :    Tiled skin layer conductivity                    W m-2 K-1

!    Reals independent of tiles (Out):
!      PKHLEV   :    SURFACE LAYER: CH*U                              m/s
!      PKCLEV   :    SURFACE LAYER: Cc*U  tracer transfer             m/s
!      PCFMLEV  :    PROP. TO EXCH. COEFF. FOR MOMENTUM               ????
!                     (C-STAR IN DOC.) (SURFACE LAYER ONLY)
!      PKMFL    :    Kinematic momentum flux                          ????
!      PKHFL    :    Kinematic heat flux                              ????
!      PKQFL    :    Kinematic moisture flux                          ????
!      PEVAPSNW :    Evaporation from snow under forest               kgm-2s-1
!      PZ0MW    :    Roughness length for momentum, WMO station       m
!      PZ0HW    :    Roughness length for heat, WMO station           m
!      PZ0QW    :    Roughness length for moisture, WMO station       m
!      PBLENDPP :    Blending weight for 10 m wind postprocessing     m
!      PCPTSPP  :    Cp*Ts for post-processing of weather parameters  J/kg
!      PQSAPP   :    Apparent surface humidity for post-processing    kg/kg
!                     of weather parameters
!      PBUOMPP  :    Buoyancy flux, for post-processing of gustiness  ???? 
!      PZDLPP   :    z/L for post-processing of weather parameters    -
!      PDHTLS   :    Diagnostic array for tiles (see module yomcdh)
!                      (Wm-2 for energy fluxes, kg/(m2s) for water fluxes)
!      PDHTSS   :    Diagnostic array for snow T (see module yomcdh)
!                      (Wm-2 for fluxes)
!      PDHTTS   :    Diagnostic array for soil T (see module yomcdh)
!                      (Wm-2 for fluxes)
!      PDHTIS   :    Diagnostic array for ice T (see module yomcdh)
!                      (Wm-2 for fluxes)

!     *PDHVEGS*      Diagnostic array for vegetation (see module yomcdh) 
!     *PEXDIAG*      Diagnostic array for optional pp of canopy resistances
!     *PDHCO2S*      Diagnostic array for CO2 (see module yomcdh)
!     *PDHBVOCS*     Diagnostic array for BVOC (see module yomcdh)
!       PWETB        Surface resistance bare soild 
!       PWETL        Canopy resistance of low vegetation  
!       PWETLU       Canopy resistance of low vegetation , unstressed 
!       PWETH        Canopy resistance of high vegetation  
!       PWETHS       Canopy resistance of high vegetation snow cover  

!     EXTERNALS.
!     ----------

!     ** SURFEXCDRIVER_CTL CALLS SUCCESSIVELY:
!         *VUPDZ0*
!         *VSURF*
!         *CO2* 
!         *VEXCS*
!         *VEVAP*
!         *VSFLX*

!  DOCUMENTATION:
!    See Physics Volume of IFS documentation

!------------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments

TYPE(C_PTR),                  INTENT(IN)  :: YDSURF

CHARACTER(LEN=1)  ,INTENT(IN)    :: CDCONF 

INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVS
INTEGER(KIND=JPIM),INTENT(IN)    :: KTILES
INTEGER(KIND=JPIM),INTENT(IN)    :: KVTYPES
INTEGER(KIND=JPIM),INTENT(IN)    :: KDIAG
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTEP
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVSN 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVI 
INTEGER(KIND=JPIM),INTENT(IN)    :: KDHVTLS 
INTEGER(KIND=JPIM),INTENT(IN)    :: KDHFTLS 
INTEGER(KIND=JPIM),INTENT(IN)    :: KDHVTSS 
INTEGER(KIND=JPIM),INTENT(IN)    :: KDHFTSS 
INTEGER(KIND=JPIM),INTENT(IN)    :: KDHVTTS 
INTEGER(KIND=JPIM),INTENT(IN)    :: KDHFTTS 
INTEGER(KIND=JPIM),INTENT(IN)    :: KDHVTIS 
INTEGER(KIND=JPIM),INTENT(IN)    :: KDHFTIS 
INTEGER(KIND=JPIM),INTENT(IN)    :: K_VMASS
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSTEP
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSTEPF
REAL(KIND=JPRB)   ,INTENT(IN)    :: PPPFD_TOA
LOGICAL,INTENT(IN)               :: LDLAND(KLON)
INTEGER(KIND=JPIM),INTENT(IN)    :: KDHVCO2S
INTEGER(KIND=JPIM),INTENT(IN)    :: KDHFCO2S
INTEGER(KIND=JPIM),INTENT(IN)    :: KDHVBVOCS
INTEGER(KIND=JPIM),INTENT(IN)    :: KDHVVEGS
INTEGER(KIND=JPIM),INTENT(IN)    :: KDHFVEGS
INTEGER(KIND=JPIM),INTENT(IN)    :: KTVL(KLON) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KCO2TYP(KLON) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTVH(KLON) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KSOTY(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVL(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVH(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCUR(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAIL(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAIH(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAILP(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAIHP(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAVGPAR(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PISOP_EP(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFWET(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAT(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PI10FGCV(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSDP2(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSDP3(:,:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSNM(KLON,KLEVSN)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRSN(KLON,KLEVSN)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PMU0(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCARDI
REAL(KIND=JPRB)   ,INTENT(IN)    :: PUMLEV(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVMLEV(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTMLEV(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQMLEV(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCMLEV(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHMS(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEOMLEV(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCPTGZLEV(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSST(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSKM1M(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PHLICE(KLON)   
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTLICE(KLON)   
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTLWML(KLON)  
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTHKICE(KLON)  
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSNTICE(KLON)  
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCHAR(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCHARHQ(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSRFL(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSLRFL(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PEMIS(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTICE(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSN(KLON,KLEVSN) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWLMX(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PUCURR(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCURR(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSAM1M(KLON,KLEVS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWSAM1M(KLON,KLEVS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFRTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PALBTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PUSTRTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PVSTRTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PAHFSTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PEVAPTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSKTI(KLON,KTILES)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PANDAYVT(KLON,KVTYPES)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PANFMVT(KLON,KVTYPES) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0M(KLON) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0H(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSSRFLTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQSTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDQSTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCPTSTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFHTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFQTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCSATTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCAIRTI(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCPTSTIU(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCSATTIU(KLON,KTILES) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCAIRTIU(KLON,KTILES)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PRAQTI(KLON,KTILES)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PTSRF(KLON,KTILES)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLAMSK(KLON,KTILES)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKHLEV(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKCLEV(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFMLEV(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKMFL(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKHFL(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKQFL(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PEVAPSNW(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0MW(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0HW(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0QW(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBLENDPP(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCPTSPP(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQSAPP(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBUOMPP(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZDLPP(KLON)
! Tile depend
REAL(KIND=JPRB)   ,INTENT(OUT)    :: PZ0MTIW(KLON,KTILES)
REAL(KIND=JPRB)   ,INTENT(OUT)    :: PZ0HTIW(KLON,KTILES)
REAL(KIND=JPRB)   ,INTENT(OUT)    :: PZ0QTIW(KLON,KTILES)
REAL(KIND=JPRB)   ,INTENT(OUT)    :: PZDLTI(KLON,KTILES)
REAL(KIND=JPRB)   ,INTENT(OUT)    :: PQSAPPTI(KLON,KTILES)
REAL(KIND=JPRB)   ,INTENT(OUT)    :: PCPTSPPTI(KLON,KTILES)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PAN(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PAG(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PRD(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PRSOIL_STR(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PRECO(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCO2FLUX(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCH4FLUX(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDHVEGS(KLON,2,KDHVVEGS+KDHFVEGS) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDHCO2S(KLON,2,KDHVCO2S+KDHFCO2S) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDHBVOCS(KLON,2,KDHVBVOCS)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBVOCFLUX(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PEXDIAG(KLON,KDIAG)

REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDHTLS(KLON,KTILES,KDHVTLS+KDHFTLS) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDHTSS(KLON,KLEVSN,KDHVTSS+KDHFTSS) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDHTTS(KLON,KLEVS,KDHVTTS+KDHFTTS) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDHTIS(KLON,KLEVI,KDHVTIS+KDHFTIS)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRPLRG

LOGICAL           ,INTENT(IN)    :: LSICOUP
LOGICAL           ,INTENT(IN)    :: LDSICE(KLON)
LOGICAL           ,INTENT(IN)    :: LBLEND
!canopy / bare soild resistances 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWETB(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWETL(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWETLU(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWETH(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWETHS(KLON)
!ifndef INTERFACE

TYPE(TSURF), POINTER :: YSURF
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('SURFEXCDRIVER',0,ZHOOK_HANDLE)

YSURF => GET_SURF(YDSURF)

!the diagnostics are not needed in SURFEXCDRIVER_CTL
!    *KDHVCO2S*     Number of variables for CO2
!    *KDHFCO2S*     Number of fluxes for CO2
!    *KDHVVEGS*     Number of variables for vegetation
!    *KDHFVEGS*     Number of fluxes for vegetation

CALL SURFEXCDRIVER_CTL(CDCONF &
 & , KIDIA, KFDIA, KLON, KLEVS, KTILES, KVTYPES, KDIAG, KSTEP &
 & , KLEVSN, KLEVI, LDLAND, KDHVTLS, KDHFTLS, KDHVTSS, KDHFTSS &
 & , KDHVTTS, KDHFTTS, KDHVTIS, KDHFTIS, K_VMASS &
 & , KDHVCO2S,KDHFCO2S,KDHVBVOCS,KDHVVEGS,KDHFVEGS &
 & , PTSTEP, PTSTEPF &
 & , PPPFD_TOA &
 & , KTVL, KCO2TYP, KTVH, PCVL, PCVH, PCUR & 
 & , PLAIL, PLAIH &
 & , PLAILP, PLAIHP, PAVGPAR, PISOP_EP &
 & , PFWET, PLAT &
 & , PSNM , PRSN &
 & , PMU0 , PCARDI &
 & , PUMLEV, PVMLEV, PTMLEV, PQMLEV, PCMLEV, PAPHMS, PGEOMLEV, PCPTGZLEV &
 & , PSST, PTSKM1M, PCHAR, PCHARHQ, PSSRFL, PSLRFL, PEMIS, PTICE, PTSN &
 & , PHLICE,PTLICE,PTLWML &   
 & , PTHKICE,PSNTICE &
 & , PWLMX, PUCURR, PVCURR, PI10FGCV  &
 & , PSSDP2, PSSDP3 &
 & , PTSAM1M, PWSAM1M, KSOTY &
 & , PFRTI, PALBTI &
 & , PUSTRTI, PVSTRTI, PAHFSTI, PEVAPTI, PTSKTI &
 & , PANDAYVT,PANFMVT &
 & , PZ0M, PZ0H &
 & , PSSRFLTI, PQSTI, PDQSTI, PCPTSTI, PCFHTI, PCFQTI, PCSATTI, PCAIRTI &
 & , PCPTSTIU, PCSATTIU, PCAIRTIU, PRAQTI, PTSRF, PLAMSK &
 & , PZ0MTIW, PZ0HTIW, PZ0QTIW, PZDLTI, PQSAPPTI, PCPTSPPTI &
 & , PKHLEV, PKCLEV, PCFMLEV, PKMFL, PKHFL, PKQFL, PEVAPSNW &
 & , PZ0MW, PZ0HW, PZ0QW, PBLENDPP, PCPTSPP, PQSAPP, PBUOMPP, PZDLPP &
 & , PAN,PAG,PRD,PRSOIL_STR,PRECO,PCO2FLUX,PCH4FLUX,PBVOCFLUX &
 & , PWETB, PWETL, PWETLU, PWETH, PWETHS &
 & , PDHTLS, PDHTSS, PDHTTS, PDHTIS &
 & , PDHVEGS, PEXDIAG, PDHCO2S, PDHBVOCS &
 & , PRPLRG &
 & , LSICOUP, LDSICE, LBLEND &
 & , YSURF%YCST, YSURF%YEXC, YSURF%YVEG, YSURF%YBVOC &
 & , YSURF%YAGS, YSURF%YAGF, YSURF%YSOIL, YSURF%YFLAKE, YSURF%YURB & 
 & )
IF (LHOOK) CALL DR_HOOK('SURFEXCDRIVER',1,ZHOOK_HANDLE)

!endif INTERFACE

!------------------------------------------------------------------------

END SUBROUTINE SURFEXCDRIVER
