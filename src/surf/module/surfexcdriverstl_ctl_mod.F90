MODULE SURFEXCDRIVERSTL_CTL_MOD
CONTAINS
SUBROUTINE SURFEXCDRIVERSTL_CTL( &
 &   KIDIA, KFDIA, KLON, KLEVS, KTILES, KSTEP &
 & , PTSTEP, PRVDIFTS &
 & , LDNOPERT, LDKPERTS, LDSURF2, LDREGSF &
! input data, non-tiled
 & , KTVL, KTVH, PCVL, PCVH &
 & , PLAIL, PLAIH &
 & , PUMLEV5, PVMLEV5 , PTMLEV5, PQMLEV5, PAPHMS5, PGEOMLEV5, PCPTGZLEV5 &
 & , PSST   , PTSKM1M5, PCHAR  , PSSRFL5, PTICE5 , PTSNOW5  &
 & , PWLMX5 &
! input data, soil - trajectory
 & , PTSAM1M5, PWSAM1M5, KSOTY &
! input data, tiled - trajectory
 & , PFRTI, PALBTI5 &
!
 & , YDCST   , YDEXC   , YDVEG , YDSOIL , YDFLAKE & 
! updated data, tiled - trajectory
 & , PUSTRTI5, PVSTRTI5, PAHFSTI5, PEVAPTI5, PTSKTI5 &
! updated data, non-tiled - trajectory
 & , PZ0M5, PZ0H5 &
! output data, tiled - trajectory
 & , PSSRFLTI5, PQSTI5 , PDQSTI5 , PCPTSTI5 &
 & , PCFHTI5  , PCFQTI5, PCSATTI5, PCAIRTI5 &
! output data, non-tiled - trajectory
 & , PCFMLEV5 &
 & , PKMFL5  , PKHFL5  , PKQFL5 &
 & , PEVAPSNW5 &
 & , PZ0MW5  , PZ0HW5    , PZ0QW5, PCPTSPP5, PQSAPP5, PBUOMPP5 &
! input data, non-tiled
 & , PUMLEV  , PVMLEV    , PTMLEV, PQMLEV  , PAPHMS , PGEOMLEV, PCPTGZLEV &
 & , PTSKM1M , PSSRFL    , PTICE , PTSNOW  &
! input data, soil
 & , PTSAM1M , PWSAM1M &
! input data, tiled
 & , PALBTI &
! updated data, tiled
 & , PUSTRTI , PVSTRTI , PAHFSTI, PEVAPTI, PTSKTI &
! updated data, non-tiled
 & , PZ0M, PZ0H &
! output data, tiled
 & , PSSRFLTI, PQSTI   , PDQSTI, PCPTSTI , PCFHTI, PCFQTI, PCSATTI, PCAIRTI &
! output data, non-tiled
 & , PCFMLEV , PEVAPSNW &
 & , PZ0MW   , PZ0HW   , PZ0QW , PCPTSPP , PQSAPP, PBUOMPP &
 & )

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_THF  , ONLY : R2ES, R3LES, R3IES, R4LES, R4IES, R5LES, R5IES
USE YOS_CST  , ONLY : TCST
USE YOS_EXC  , ONLY : TEXC
USE YOS_VEG  , ONLY : TVEG
USE YOS_SOIL , ONLY : TSOIL
USE YOS_FLAKE, ONLY : TFLAKE
USE VUPDZ0S_MOD
USE VSURFS_MOD
USE VEXCSS_MOD
USE VEVAPS_MOD
USE VUPDZ0STL_MOD
USE VSURFSTL_MOD
USE VEXCSSTL_MOD
USE VEVAPSTL_MOD

! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!------------------------------------------------------------------------

!  PURPOSE:
!    Routine SURFEXCDRIVERSTL controls the ensemble of routines that prepare
!    the surface exchange coefficients and associated surface quantities
!    needed for the solution of the vertical diffusion equations. 
!    (Tangent linear)

!  SURFEXCDRIVERSTL is called by VDFMAINSTL

!  METHOD:
!    This routine is only a shell needed by the surface library
!    externalisation.

!  AUTHOR:
!    M. Janiskova       ECMWF June 2005  

!  REVISION HISTORY:
!    M. Janiskova     10/03/2006 call for simplified routines (suffix s)
!                                and for TL routines when lenopert=.false.
!    G. Balsamo       10/07/2006 Add soil type
!    M. Janiskova     21/05/2007 clean-up of roughness length initialization
!    M. Janiskova     02/10/2007 computation of kinematic fluxes
!    S. Boussetta/G.Balsamo May 2009 Add lai
!    M. Janiskova     July 2011-> 2013 Modified computation of snow evaporation
!                                Perturbation of top layer surface fields
!    M. Janiskova     Jan 2015   use previous time step fluxes for heat&momentum
!    P. Lopez         June 2015  Added regularization of wet skin tile
!                                perturbation in low wind situations.

!  INTERFACE: 

!    Integers (In):
!      KIDIA    :    Begin point in arrays
!      KFDIA    :    End point in arrays
!      KLON     :    Length of arrays
!      KLEVS    :    Number of soil layers
!      KTILES   :    Number of tiles
!      KSTEP    :    Time step index
!      KTVL     :    Dominant low vegetation type
!      KTVH     :    Dominant high vegetation type
!      KSOTY    :    SOIL TYPE                                        (1-7)

!    Reals (In):
!      PTSTEP   :    Timestep
!      PRVDIFTS :    Semi-implicit factor for vertical diffusion discretization
!      PCVL     :    LOW VEGETATION COVER                          -  
!      PCVH     :    HIGH VEGETATION COVER                         -  
!      PLAIL    :    LAI of low vegetation
!      PLAIH    :    LAI of High vegetation

!  Logical:
!      LDNOPERT :    TRUE when no perturbations is required for surface arrays
!      LDKPERTS :    TRUE when pertubations of surf. exchange coefficients used
!      LDSURF2  :    TRUE when simplified surface scheme called
!      LDREGSF  :    TRUE when regularization used

!*      Reals with tile index (In): 
!  Trajectory  Perturbation  Description                               Unit
!  PFRTI       ---           TILE FRACTIONS                            (0-1)
!                            1: WATER         5: SNOW ON LOW-VEG+BARE-SOIL
!                            2: ICE           6: DRY SNOW-FREE HIGH-VEG
!                            3: WET SKIN      7: SNOW UNDER HIGH-VEG
!                            4: DRY SNOW-FREE 8: BARE SOIL
!                               LOW-VEG
!  PALBTI5     PALBTI        Tile albedo                               (0-1)

!*      Reals independent of tiles (In):
!  Trajectory  Perturbation  Description                               Unit
!  PUMLEV5     PUMLEV        X-VELOCITY COMPONENT, lowest              m/s
!                            atmospheric level
!  PVMLEV5     PVMLEV        Y-VELOCITY COMPONENT, lowest              m/s
!                            atmospheric level
!  PTMLEV5     PTMLEV        TEMPERATURE, lowest atmospheric level     K
!  PQMLEV5     PQMLEV        SPECIFIC HUMIDITY                         kg/kg
!  PAPHMS5     PAPHMS        Surface pressure                          Pa
!  PGEOMLEV5   PGEOMLEV      Geopotential, lowest atmospehric level    m2/s2
!  PCPTGZLEV5  PCPTGZLEV     Geopotential, lowest atmospehric level    J/kg
!  PSST        ---           (OPEN) SEA SURFACE TEMPERATURE            K
!  PTSKM1M5    PTSKM1M       SKIN TEMPERATURE                          K
!  PCHAR       ---           "EQUIVALENT" CHARNOCK PARAMETER           -
!  PSSRFL5     PSSRFL        NET SHORTWAVE RADIATION FLUX AT SURFACE   W/m2
!  PTSAM1M5    PTSAM1M       SURFACE TEMPERATURE                       K
!  PWSAM1M5    PWSAM1M       SOIL MOISTURE ALL LAYERS                 m**3/m**3
!  PTICE5      PTICE         Ice temperature, top slab                 K
!  PTSNOW5     PTSNOW        Snow temperature                          K
!  PWLMX5      ---           Maximum interception layer capacity       kg/m**2

!*      Reals with tile index (In/Out):
!  Trajectory  Perturbation  Description                               Unit
!  PUSTRTI5    PUSTRTI       SURFACE U-STRESS                          N/m2
!  PVSTRTI5    PVSTRTI       SURFACE V-STRESS                          N/m2
!  PAHFSTI5    PAHFSTI       SURFACE SENSIBLE HEAT FLUX                W/m2
!  PEVAPTI5    PEVAPTI       SURFACE MOISTURE FLUX                     KG/m2/s
!  PTSKTI5     PTSKTI        SKIN TEMPERATURE                          K

!*      Reals independent of tiles (In/Out):
!  Trajectory  Perturbation  Description                               Unit
!  PZ0M5       PZ0M          AERODYNAMIC ROUGHNESS LENGTH              m
!  PZ0H5       PZ0H          ROUGHNESS LENGTH FOR HEAT                 m

!*      Reals with tile index (Out):
!  Trajectory  Perturbation  Description                               Unit
!  PSSRFLTI5   PSSRFLTI      Tiled NET SHORTWAVE RADIATION FLUX        W/m2
!                            AT SURFACE
!  PQSTI5      PQSTI         Tiled SATURATION Q AT SURFACE             kg/kg
!  PDQSTI5     PDQSTI        Tiled DERIVATIVE OF SATURATION Q-CURVE    kg/kg/K
!  PCPTSTI5    PCPTSTI       Tiled DRY STATIC ENERGY AT SURFACE        J/kg
!  PCFHTI5     PCFHTI        Tiled EXCHANGE COEFFICIENT AT THE SURFACE ????
!  PCFQTI5     PCFQTI        Tiled EXCHANGE COEFFICIENT AT THE SURFACE ????
!  PCSATTI5    PCSATTI       MULTIPLICATION FACTOR FOR QS AT SURFACE   -
!                            FOR SURFACE FLUX COMPUTATION
!  PCAIRTI5    PCAIRTI       MULTIPLICATION FACTOR FOR Q AT LOWEST     - 
!                            MODEL LEVEL FOR SURFACE FLUX COMPUTATION

!*      Reals independent of tiles (Out):
!  Trajectory  Perturbation  Description                               Unit
!  PCFMLEV5    PCFMLEV       PROP. TO EXCH. COEFF. FOR MOMENTUM        ????
!                             (C-STAR IN DOC.) (SURFACE LAYER ONLY)
!  PKMFL5      ---           Kinematic momentum flux                   ????
!  PKHFL5      ---           Kinematic heat flux                       ????
!  PKQFL5      ---           Kinematic moisture flux                   ????
!  PEVAPSNW5   PEVAPSNW      Evaporation from snow under forest        kgm-2s-1
!  PZ0MW5      PZ0MW         Roughness length for momentum,WMO station m
!  PZ0HW5      PZ0HW         Roughness length for heat, WMO station    m
!  PZ0QW5      PZ0QW         Roughness length for moisture,WMO station m
!  PCPTSPP5    PCPTSPP       Cp*Ts for post-processing of weather      J/kg
!                            parameters
!  PQSAPP5     PQSAPP        Apparent surface humidity                 kg/kg
!                            post-processing of weather parameters
!  PBUOMPP5    PBUOMPP       Buoyancy flux, for post-processing        ???? 
!                            of gustiness

!     EXTERNALS.
!     ----------

!     ** SURFEXCDRIVERSTL_CTL CALLS SUCCESSIVELY:
!         *VUPDZ0*
!         *VSURF*
!         *VEXCS*
!         *VEVAP*

!  DOCUMENTATION:
!    See Physics Volume of IFS documentation

!------------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVS
INTEGER(KIND=JPIM),INTENT(IN)    :: KTILES
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTEP
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSTEP
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRVDIFTS
LOGICAL           ,INTENT(IN)    :: LDNOPERT
LOGICAL           ,INTENT(IN)    :: LDKPERTS
LOGICAL           ,INTENT(IN)    :: LDSURF2
LOGICAL           ,INTENT(IN)    :: LDREGSF

INTEGER(KIND=JPIM),INTENT(IN)    :: KTVL(:) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTVH(:) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KSOTY(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVL(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVH(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAIL(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAIH(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PUMLEV5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVMLEV5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTMLEV5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQMLEV5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHMS5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEOMLEV5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCPTGZLEV5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSST(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSKM1M5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCHAR(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSRFL5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTICE5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSNOW5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWLMX5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSAM1M5(:,:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWSAM1M5(:,:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFRTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PALBTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PUSTRTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PVSTRTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PAHFSTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PEVAPTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSKTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0M5(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0H5(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSSRFLTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQSTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDQSTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCPTSTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFHTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFQTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCSATTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCAIRTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFMLEV5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKMFL5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKHFL5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKQFL5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PEVAPSNW5(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0MW5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0HW5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0QW5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCPTSPP5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQSAPP5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBUOMPP5(:)

REAL(KIND=JPRB)   ,INTENT(IN)    :: PUMLEV(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVMLEV(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTMLEV(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQMLEV(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHMS(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEOMLEV(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCPTGZLEV(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSKM1M(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSRFL(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTICE(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSNOW(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSAM1M(:,:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWSAM1M(:,:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PALBTI(:,:) 
TYPE(TCST)        ,INTENT(IN)    :: YDCST
TYPE(TEXC)        ,INTENT(IN)    :: YDEXC
TYPE(TVEG)        ,INTENT(IN)    :: YDVEG
TYPE(TSOIL)       ,INTENT(IN)    :: YDSOIL
TYPE(TFLAKE)      ,INTENT(IN)    :: YDFLAKE
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PUSTRTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PVSTRTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PAHFSTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PEVAPTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSKTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0M(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0H(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSSRFLTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQSTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDQSTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCPTSTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFHTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFQTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCSATTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCAIRTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCFMLEV(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PEVAPSNW(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0MW(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0HW(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0QW(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCPTSPP(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQSAPP(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBUOMPP(:)

! Local variables

INTEGER(KIND=JPIM) :: IFRMAX(KLON)

REAL(KIND=JPRB) :: ZZ0MTI5(KLON,KTILES) , ZZ0HTI5(KLON,KTILES) ,&
                 & ZZ0QTI5(KLON,KTILES) , ZBUOMTI5(KLON,KTILES),&
                 & ZZDLTI5(KLON,KTILES) , ZRAQTI5(KLON,KTILES) ,&
                 & ZQSATI5(KLON,KTILES) , ZCFMTI5(KLON,KTILES) ,&
                 & ZKMFLTI5(KLON,KTILES), ZKHFLTI5(KLON,KTILES),&
                 & ZKQFLTI5(KLON,KTILES), ZZQSATI5(KLON,KTILES)

REAL(KIND=JPRB) :: ZFRMAX5(KLON) , ZALB5(KLON)  , ZSSRFL15(KLON) , &
                 & ZSRFD5(KLON)  , ZWETL5(KLON) , ZWETH5(KLON)   , &
                 & ZWETHS5(KLON) , ZWETB5(KLON) , &
                 & ZTSA5(KLON)   , ZCSNW5(KLON)

REAL(KIND=JPRB) :: ZZ0MTI(KLON,KTILES) , ZZ0HTI(KLON,KTILES) ,&
                 & ZZ0QTI(KLON,KTILES) , ZBUOMTI(KLON,KTILES),&
                 & ZZDLTI(KLON,KTILES) , ZRAQTI(KLON,KTILES) ,&
                 & ZQSATI(KLON,KTILES) , ZCFMTI(KLON,KTILES) ,&
                 & ZZQSATI(KLON,KTILES)

REAL(KIND=JPRB) :: ZFRMAX(KLON)   , ZALB(KLON)     , ZSSRFL1(KLON)  , &
                 & ZSRFD(KLON)    , ZWETL(KLON)    , ZWETH(KLON)    , &
                 & ZWETHS(KLON)   , ZWETB(KLON)    , &
                 & ZTSA(KLON)     , ZCSNW(KLON)

INTEGER(KIND=JPIM) :: JL, JTILE, KTILE

REAL(KIND=JPRB) :: ZQSSN5, ZCOR5, ZCDRO5
REAL(KIND=JPRB) :: ZQSSN, ZCOR, ZCONS1, ZZ0MWMO, ZZ0HWMO
REAL(KIND=JPRB) :: ZDIV15, ZDIV25, Z3S5, Z4S5, Z3S, Z4S
REAL(KIND=JPRB) :: ZCONS2
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
LOGICAL         :: LLAND, LLSICE, LLHISSR(KLON)
#include "fcsttre.h"

!*         1.     Set up of general quantities
!                 ----------------------------

IF (LHOOK) CALL DR_HOOK('SURFEXCDRIVERSTL_CTL_MOD:SURFEXCDRIVERSTL_CTL',0,ZHOOK_HANDLE)
ASSOCIATE(RCPD=>YDCST%RCPD, RD=>YDCST%RD, RETV=>YDCST%RETV, RG=>YDCST%RG, &
 & RSIGMA=>YDCST%RSIGMA, RTT=>YDCST%RTT, &
 & REPDU2=>YDEXC%REPDU2, RKAP=>YDEXC%RKAP, RZ0ICE=>YDEXC%RZ0ICE, &
 & RVTRSR=>YDVEG%RVTRSR, RVZ0M=>YDVEG%RVZ0M)

ZCONS1=1./(RG*PTSTEP)

!*         1.1  ESTIMATE SURF.FL. FOR STEP 0
!*              (ASSUME NEUTRAL STRATIFICATION)

IF ( KSTEP == 0) THEN
  DO JTILE=2,KTILES
    DO JL=KIDIA,KFDIA
      PTSKTI  (JL,JTILE) = PTSKM1M(JL)
      PTSKTI5 (JL,JTILE) = PTSKM1M5(JL)
    ENDDO
  ENDDO
  DO JL=KIDIA,KFDIA
    PTSKTI (JL,1) = 0.0_JPRB
    PTSKTI5(JL,1) = PSST(JL)
  ENDDO
ENDIF


!*         1.2  UPDATE Z0

IF (LDNOPERT) THEN
  CALL VUPDZ0S(KIDIA,KFDIA,KLON,KTILES,KSTEP,&
   & KTVL, KTVH, PCVL, PCVH,&
   & PUMLEV5 , PVMLEV5 ,&
   & PTMLEV5 , PQMLEV5 , PAPHMS5 , PGEOMLEV5,&
   & PUSTRTI5, PVSTRTI5, PAHFSTI5, PEVAPTI5 ,&
   & PTSKTI5 , PCHAR   , PFRTI   ,&
   & YDCST   , YDEXC   ,YDVEG    ,YDFLAKE   , &
   & ZZ0MTI5 , ZZ0HTI5 , ZZ0QTI5 , ZBUOMTI5 , ZZDLTI5, ZRAQTI5 )

! perturbations are put to zero

  DO JTILE=1,KTILES
    ZZ0MTI(KIDIA:KFDIA,JTILE)=0.0_JPRB
    ZZ0HTI(KIDIA:KFDIA,JTILE)=0.0_JPRB
    ZZ0QTI(KIDIA:KFDIA,JTILE)=0.0_JPRB
    ZBUOMTI(KIDIA:KFDIA,JTILE)=0.0_JPRB
    ZZDLTI(KIDIA:KFDIA,JTILE)=0.0_JPRB
    ZRAQTI(KIDIA:KFDIA,JTILE)=0.0_JPRB
  ENDDO
ELSE
  CALL VUPDZ0STL(KIDIA,KFDIA,KLON,KTILES,KSTEP,&
   & KTVL, KTVH, PCVL, PCVH,&
   & PUMLEV5 , PVMLEV5 ,&
   & PTMLEV5 , PQMLEV5 , PAPHMS5 , PGEOMLEV5,&
   & PUSTRTI5, PVSTRTI5, PAHFSTI5, PEVAPTI5 ,&
   & PTSKTI5 , PCHAR   , PFRTI   ,&
   & YDCST   , YDEXC   ,YDVEG    ,YDFLAKE   , &
   & ZZ0MTI5 , ZZ0HTI5 , ZZ0QTI5 , ZBUOMTI5 , ZZDLTI5 , ZRAQTI5 ,&
   & PUMLEV  , PVMLEV  ,&
   & PTMLEV  , PQMLEV  , PAPHMS  , PGEOMLEV ,&
   & PUSTRTI , PVSTRTI , PAHFSTI , PEVAPTI  ,&
   & PTSKTI  , &
   & ZZ0MTI  , ZZ0HTI  , ZZ0QTI  , ZBUOMTI  , ZZDLTI  , ZRAQTI )
ENDIF


!*         1.3  FIND DOMINANT SURFACE TYPE parameters for postprocessing

ZFRMAX (KIDIA:KFDIA) = 0.0_JPRB
ZFRMAX5(KIDIA:KFDIA) = PFRTI(KIDIA:KFDIA,1)
IFRMAX(KIDIA:KFDIA) = 1
DO JTILE=2,KTILES
  DO JL=KIDIA,KFDIA
    IF (PFRTI(JL,JTILE)  >  ZFRMAX5(JL)) THEN
      ZFRMAX(JL) = 0.0_JPRB
      ZFRMAX5(JL)= PFRTI(JL,JTILE)
      IFRMAX(JL) = JTILE
    ENDIF
  ENDDO
ENDDO

DO JL=KIDIA,KFDIA
  JTILE=IFRMAX(JL)
  PZ0M (JL) = ZZ0MTI(JL,JTILE)
  PZ0M5(JL) = ZZ0MTI5(JL,JTILE)
  PZ0H (JL) = ZZ0HTI(JL,JTILE)
  PZ0H5(JL) = ZZ0HTI5(JL,JTILE)
ENDDO


!     ------------------------------------------------------------------

!*         2.     SURFACE BOUNDARY CONDITIONS FOR T AND Q
!                 ---------------------------------------

!    2.1 Albedo

ZALB(KIDIA:KFDIA)=PFRTI(KIDIA:KFDIA,1)*PALBTI(KIDIA:KFDIA,1)&
 & +PFRTI(KIDIA:KFDIA,2)*PALBTI(KIDIA:KFDIA,2)&
 & +PFRTI(KIDIA:KFDIA,3)*PALBTI(KIDIA:KFDIA,3)&
 & +PFRTI(KIDIA:KFDIA,4)*PALBTI(KIDIA:KFDIA,4)&
 & +PFRTI(KIDIA:KFDIA,5)*PALBTI(KIDIA:KFDIA,5)&
 & +PFRTI(KIDIA:KFDIA,6)*PALBTI(KIDIA:KFDIA,6)&
 & +PFRTI(KIDIA:KFDIA,7)*PALBTI(KIDIA:KFDIA,7)&
 & +PFRTI(KIDIA:KFDIA,8)*PALBTI(KIDIA:KFDIA,8)
ZALB5(KIDIA:KFDIA)=PFRTI(KIDIA:KFDIA,1)*PALBTI5(KIDIA:KFDIA,1)&
 & +PFRTI(KIDIA:KFDIA,2)*PALBTI5(KIDIA:KFDIA,2)&
 & +PFRTI(KIDIA:KFDIA,3)*PALBTI5(KIDIA:KFDIA,3)&
 & +PFRTI(KIDIA:KFDIA,4)*PALBTI5(KIDIA:KFDIA,4)&
 & +PFRTI(KIDIA:KFDIA,5)*PALBTI5(KIDIA:KFDIA,5)&
 & +PFRTI(KIDIA:KFDIA,6)*PALBTI5(KIDIA:KFDIA,6)&
 & +PFRTI(KIDIA:KFDIA,7)*PALBTI5(KIDIA:KFDIA,7)&
 & +PFRTI(KIDIA:KFDIA,8)*PALBTI5(KIDIA:KFDIA,8)

ZSSRFL1 (KIDIA:KFDIA) = 0.0_JPRB
ZSSRFL15(KIDIA:KFDIA) = 0.0_JPRB

LLHISSR(KIDIA:KFDIA)=.FALSE.
DO JTILE=1,KTILES
  DO JL=KIDIA,KFDIA
! Disaggregate solar flux but limit to 700 W/m2 (due to inconsistency
!  with albedo)
    PSSRFLTI(JL,JTILE) = ((1.0_JPRB-PALBTI5(JL,JTILE))&
     & /(1.0_JPRB-ZALB5(JL)))*PSSRFL(JL)&
     & -(PSSRFL5(JL)/(1.0_JPRB-ZALB5(JL)))&
     & *PALBTI(JL,JTILE)&
     & +(PSSRFL5(JL)*(1.0_JPRB-PALBTI5(JL,JTILE))&
     & /(1.0_JPRB-ZALB5(JL))**2)*ZALB(JL)
    PSSRFLTI5(JL,JTILE) = ((1.0_JPRB-PALBTI5(JL,JTILE))&
     & /(1.0_JPRB-ZALB5(JL)))*PSSRFL5(JL)
    IF (PSSRFLTI5(JL,JTILE) > 700._JPRB) THEN
      LLHISSR(JL)=.TRUE.
      PSSRFLTI (JL,JTILE) = 0._JPRB
      PSSRFLTI5(JL,JTILE) = 700._JPRB
    ENDIF

! Compute averaged net solar flux after limiting to 700 W/m2
    ZSSRFL1(JL)  = ZSSRFL1(JL)&
     & + PFRTI(JL,JTILE)*PSSRFLTI(JL,JTILE)
    ZSSRFL15(JL) = ZSSRFL15(JL)&
     & + PFRTI(JL,JTILE)*PSSRFLTI5(JL,JTILE)
  ENDDO
ENDDO

DO JTILE=1,KTILES
  DO JL=KIDIA,KFDIA
    IF (LLHISSR(JL)) THEN
      PSSRFLTI(JL,JTILE)  = (PSSRFLTI(JL,JTILE)*PSSRFL5(JL) &
       & + PSSRFLTI5(JL,JTILE)*PSSRFL(JL))/ZSSRFL15(JL) &
       & - PSSRFLTI5(JL,JTILE)*PSSRFL5(JL)*ZSSRFL1(JL)/ZSSRFL15(JL)**2
      PSSRFLTI5(JL,JTILE) = PSSRFLTI5(JL,JTILE)*PSSRFL5(JL)/ZSSRFL15(JL)
    ENDIF
    ZSRFD(JL)  = PSSRFLTI(JL,JTILE)/(1.0_JPRB-PALBTI5(JL,JTILE)) &
     & + PALBTI(JL,JTILE)*PSSRFLTI5(JL,JTILE) &
     & / (1.0_JPRB-PALBTI5(JL,JTILE))**2
    ZSRFD5(JL) = PSSRFLTI5(JL,JTILE)/(1.0_JPRB-PALBTI5(JL,JTILE))
  ENDDO
  
  IF (LDNOPERT) THEN
    CALL VSURFS(KIDIA,KFDIA,KLON,KLEVS,JTILE,&
     & KTVL,KTVH,&
     & PLAIL,PLAIH,&
     & PTMLEV5  ,PQMLEV5 ,PAPHMS5 ,&
     & PTSKTI5(:,JTILE)  ,PWSAM1M5,PTSAM1M5, KSOTY, &
     & ZSRFD5, ZRAQTI5(:,JTILE) , &
     & YDCST, YDVEG, YDSOIL,&
     & ZQSATI5(:,JTILE),PQSTI5(:,JTILE)  ,PDQSTI5(:,JTILE) ,&
     & ZWETB5, PCPTSTI5(:,JTILE), ZWETL5 , ZWETH5, ZWETHS5 )  

! perturbations are put to zero

    ZQSATI (KIDIA:KFDIA,JTILE) = 0.0_JPRB
    PQSTI  (KIDIA:KFDIA,JTILE) = 0.0_JPRB
    PDQSTI (KIDIA:KFDIA,JTILE) = 0.0_JPRB
    PCPTSTI(KIDIA:KFDIA,JTILE) = 0.0_JPRB
    ZWETB  (KIDIA:KFDIA) = 0.0_JPRB
    ZWETL  (KIDIA:KFDIA) = 0.0_JPRB
    ZWETH  (KIDIA:KFDIA) = 0.0_JPRB
    ZWETHS (KIDIA:KFDIA) = 0.0_JPRB
  ELSE
    CALL VSURFSTL(KIDIA,KFDIA,KLON,KLEVS,JTILE,&
     & KTVL,KTVH,&
     & PLAIL, PLAIH, &
     & PTMLEV5  ,PQMLEV5 ,PAPHMS5 ,&
     & PTSKTI5(:,JTILE)  ,PWSAM1M5,PTSAM1M5, KSOTY,&
     & ZSRFD5   ,ZRAQTI5(:,JTILE)  ,&
     & YDCST    ,YDVEG   ,YDSOIL   ,&
     & ZQSATI5(:,JTILE)  ,PQSTI5(:,JTILE)  ,PDQSTI5(:,JTILE) ,&
     & ZWETB5, PCPTSTI5(:,JTILE)  , ZWETL5 , ZWETH5, ZWETHS5,&
     & PTMLEV   ,PQMLEV  ,PAPHMS  ,&
     & PTSKTI(:,JTILE)   ,PWSAM1M ,PTSAM1M ,&
     & ZSRFD ,  ZRAQTI(:,JTILE)   , ZQSATI(:,JTILE) ,&
     & PQSTI(:,JTILE)    ,PDQSTI(:,JTILE)  ,&
     & ZWETB , PCPTSTI(:,JTILE)   , ZWETL  , ZWETH , ZWETHS )
  ENDIF
ENDDO


!*         3.     EXCHANGE COEFFICIENTS
!                 ---------------------

!*         3.1  SURFACE EXCHANGE COEFFICIENTS

IF (LDNOPERT) THEN
  DO JTILE=1,KTILES

    CALL VEXCSS(KIDIA,KFDIA,KLON,PTSTEP,PRVDIFTS,&
     & PUMLEV5,PVMLEV5,PTMLEV5,PQMLEV5,PAPHMS5,PGEOMLEV5,PCPTGZLEV5,&
     & PCPTSTI5(:,JTILE),ZQSATI5(:,JTILE) ,&
     & ZZ0MTI5(:,JTILE) ,ZZ0HTI5(:,JTILE) ,&
     & ZZ0QTI5(:,JTILE) ,ZBUOMTI5(:,JTILE),&
     & YDCST,YDEXC,&
     & ZCFMTI5(:,JTILE) ,PCFHTI5(:,JTILE) ,&
     & PCFQTI5(:,JTILE) )

! perturbations are put to zero
    ZCFMTI(KIDIA:KFDIA,JTILE)=0.0_JPRB
    PCFHTI(KIDIA:KFDIA,JTILE)=0.0_JPRB
    PCFQTI(KIDIA:KFDIA,JTILE)=0.0_JPRB
  ENDDO
ELSE
  DO JTILE=1,KTILES
    CALL VEXCSSTL(KIDIA,KFDIA,KLON,PTSTEP,PRVDIFTS,&
     & LDKPERTS, &
     & PUMLEV5,PVMLEV5,PTMLEV5,PQMLEV5,PAPHMS5,PGEOMLEV5,PCPTGZLEV5,&
     & PCPTSTI5(:,JTILE),ZQSATI5(:,JTILE) ,&
     & ZZ0MTI5(:,JTILE) ,ZZ0HTI5(:,JTILE) ,&
     & ZZ0QTI5(:,JTILE) ,ZBUOMTI5(:,JTILE),&
     & YDCST            ,YDEXC            ,&
     & ZCFMTI5(:,JTILE) ,PCFHTI5(:,JTILE) ,&
     & PCFQTI5(:,JTILE) ,&
     & PUMLEV ,PVMLEV ,PTMLEV ,PQMLEV ,PAPHMS ,PGEOMLEV ,PCPTGZLEV ,&
     & PCPTSTI(:,JTILE) ,ZQSATI(:,JTILE)  ,&
     & ZZ0MTI(:,JTILE)  ,ZZ0HTI(:,JTILE)  ,&
     & ZZ0QTI(:,JTILE)  ,ZBUOMTI(:,JTILE) ,&
     & ZCFMTI(:,JTILE)  ,PCFHTI(:,JTILE)  ,&
     & PCFQTI(:,JTILE) )
  ENDDO

  IF (LDREGSF) THEN
    ! Regularization of wet skin tile perturbation in low wind situations. 
    DO JL=KIDIA,KFDIA
      IF (SQRT(PUMLEV5(JL)**2 + PVMLEV5(JL)**2) < 1.5_JPRB) PCFQTI(JL,3)=0.0_JPRB
    ENDDO
  ENDIF

ENDIF


!*         3.2  EQUIVALENT EVAPOTRANSPIRATION EFFICIENCY COEFFICIENT

IF (LDNOPERT) THEN
  DO JTILE=1,KTILES
    IF     (JTILE == 1) THEN
      ZTSA5(KIDIA:KFDIA)=PSST(KIDIA:KFDIA)
    ELSEIF (JTILE == 2) THEN
      ZTSA5(KIDIA:KFDIA)=PTICE5(KIDIA:KFDIA)
    ELSEIF (JTILE == 5 .OR. JTILE == 7) THEN
      ZTSA5(KIDIA:KFDIA)=PTSNOW5(KIDIA:KFDIA)
    ELSE
      ZTSA5(KIDIA:KFDIA)=PTSAM1M5(KIDIA:KFDIA,1)
    ENDIF
    CALL VEVAPS(KIDIA,KFDIA,KLON,PTSTEP,PRVDIFTS,JTILE,&
     & PWLMX5 ,PTMLEV5 ,PQMLEV5 ,PAPHMS5 ,PTSKTI5(:,JTILE) ,ZTSA5,&
     & PQSTI5(:,JTILE) ,PCFQTI5(:,JTILE) ,ZWETB5,ZWETL5,ZWETH5,ZWETHS5,&
     & YDCST           ,YDVEG            ,&
     & PCPTSTI5(:,JTILE),PCSATTI5(:,JTILE),PCAIRTI5(:,JTILE),&
     & ZCSNW5)  

! perturbations are put to zero
    PCPTSTI(KIDIA:KFDIA,JTILE) = 0.0_JPRB
    PCSATTI(KIDIA:KFDIA,JTILE) = 0.0_JPRB
    PCAIRTI(KIDIA:KFDIA,JTILE) = 0.0_JPRB
    ZCSNW(KIDIA:KFDIA) = 0.0_JPRB
  ENDDO
ELSE
  DO JTILE=1,KTILES
    IF     (JTILE == 1) THEN
      ZTSA5(KIDIA:KFDIA)=PSST(KIDIA:KFDIA)
      ZTSA (KIDIA:KFDIA)=0.0_JPRB
    ELSEIF (JTILE == 2) THEN
      ZTSA5(KIDIA:KFDIA)=PTICE5(KIDIA:KFDIA)
      ZTSA (KIDIA:KFDIA)=PTICE (KIDIA:KFDIA)
    ELSEIF (JTILE == 5 .OR. JTILE == 7) THEN
      ZTSA5(KIDIA:KFDIA)=PTSNOW5(KIDIA:KFDIA)
      ZTSA (KIDIA:KFDIA)=PTSNOW (KIDIA:KFDIA)
    ELSE
      ZTSA5(KIDIA:KFDIA)=PTSAM1M5(KIDIA:KFDIA,1)
      ZTSA (KIDIA:KFDIA)=PTSAM1M (KIDIA:KFDIA,1)
    ENDIF
    CALL VEVAPSTL(KIDIA,KFDIA,KLON,PTSTEP,PRVDIFTS,JTILE,&
     & PWLMX5 ,PTMLEV5 ,PQMLEV5 ,PAPHMS5 ,PTSKTI5(:,JTILE) ,ZTSA5,&
     & PQSTI5(:,JTILE) ,PCFQTI5(:,JTILE) ,ZWETB5,ZWETL5,ZWETH5,ZWETHS5,&
     & YDCST           ,YDVEG,&
     & PCPTSTI5(:,JTILE),PCSATTI5(:,JTILE),PCAIRTI5(:,JTILE),&
     & ZCSNW5,&
     & PTMLEV  ,PQMLEV ,PAPHMS  ,PTSKTI(:,JTILE)  ,ZTSA ,&
     & PQSTI(:,JTILE)  ,PCFQTI(:,JTILE)  ,ZWETB ,ZWETL ,ZWETH ,ZWETHS ,&
     & PCPTSTI (:,JTILE),PCSATTI (:,JTILE),PCAIRTI (:,JTILE),&
     & ZCSNW )
  ENDDO
ENDIF

!          COMPUTE SNOW EVAPORATION FROM BELOW TREES i.e. TILE 7

IF (LDSURF2) THEN

! Note the use of qsat(Tsnow), rather than tile 7 skin. Skin T7 is a
! canopy temperature, definitely not what is desirable. Skin T5 can go
! up (and down ..) freely, not really what we want. The use of
! qsat (Tsnow) is tantamount to neglecting the skin effect there.

  DO JL=KIDIA,KFDIA
    IF (PFRTI(JL,7) > 0.0_JPRB) THEN
      IF (PTSNOW5(JL) > RTT) THEN
        ZDIV15 = 1.0_JPRB/(PTSNOW5(JL)-R4LES)
        Z3S  = R3LES*(RTT-R4LES)*ZDIV15*ZDIV15*PTSNOW (JL)
        Z3S5 = R3LES*(PTSNOW5(JL)-RTT)*ZDIV15
      ELSE
        ZDIV15 = 1.0_JPRB/(PTSNOW5(JL)-R4IES)
        Z3S  = R3IES*(RTT-R4IES)*ZDIV15*ZDIV15*PTSNOW (JL)
        Z3S5 = R3IES*(PTSNOW5(JL)-RTT)*ZDIV15
      ENDIF
      Z4S5 = EXP(Z3S5)
      Z4S  = Z4S5*Z3S
      ZDIV25 = 1.0_JPRB/PAPHMS5(JL)
      ZQSSN  = R2ES*(Z4S *PAPHMS5(JL)-Z4S5*PAPHMS (JL))*ZDIV25*ZDIV25
      ZQSSN5 = (R2ES*Z4S5)*ZDIV25
!    ZQSSN  = -(FOEEW(PTSNOW5(JL))/PAPHMS5(JL)**2)*PAPHMS(JL)
!    ZQSSN5 = FOEEW(PTSNOW5(JL))/PAPHMS5(JL)
      ZCOR  = RETV * ZQSSN/(1.0_JPRB-RETV  *ZQSSN5)**2
      ZCOR5 = 1.0_JPRB/(1.0_JPRB-RETV  *ZQSSN5)
      ZQSSN = ZQSSN5*ZCOR + ZCOR5*ZQSSN
      ZQSSN5= ZQSSN5*ZCOR5
      PEVAPSNW (JL) = &
       &   ZCONS1*ZCSNW5(JL)*(PQMLEV5(JL)-ZQSSN5)*PCFQTI(JL,7)&
       & + ZCONS1*PCFQTI5(JL,7)*(PQMLEV5(JL)-ZQSSN5)*ZCSNW(JL)&
       & + ZCONS1*ZCSNW5(JL)*PCFQTI5(JL,7)*PQMLEV(JL)&
       & - ZCONS1*ZCSNW5(JL)*PCFQTI5(JL,7)*ZQSSN
      PEVAPSNW5(JL) = &
       & ZCONS1*PCFQTI5(JL,7)*ZCSNW5(JL)*(PQMLEV5(JL)-ZQSSN5)
    ELSE
      PEVAPSNW (JL) = 0.0_JPRB
      PEVAPSNW5(JL) = 0.0_JPRB
    ENDIF
  ENDDO
ELSE

  DO JL=KIDIA,KFDIA
    ZQSSN  = -(FOEEW(PTSNOW5(JL))/PAPHMS5(JL)**2)*PAPHMS(JL)
    ZQSSN5 = FOEEW(PTSNOW5(JL))/PAPHMS5(JL)
    ZCOR  = RETV * ZQSSN/(1.0_JPRB-RETV  *ZQSSN5)**2
    ZCOR5 = 1.0_JPRB/(1.0_JPRB-RETV  *ZQSSN5)
    ZQSSN = ZQSSN5*ZCOR + ZCOR5*ZQSSN
    ZQSSN5= ZQSSN5*ZCOR5
    PEVAPSNW (JL) = &
     &   ZCONS1*ZCSNW5(JL)*(PQMLEV5(JL)-ZQSSN5)*PCFQTI(JL,7)&
     & + ZCONS1*PCFQTI5(JL,7)*(PQMLEV5(JL)-ZQSSN5)*ZCSNW(JL)&
     & + ZCONS1*ZCSNW5(JL)*PCFQTI5(JL,7)*PQMLEV(JL)&
     & - ZCONS1*ZCSNW5(JL)*PCFQTI5(JL,7)*ZQSSN
    PEVAPSNW5(JL) = &
     & ZCONS1*PCFQTI5(JL,7)*ZCSNW5(JL)*(PQMLEV5(JL)-ZQSSN5)
  ENDDO
ENDIF

!*         3.3  COMPUTE SURFACE FLUXES FOR TILES
!               (replaces vsflx routine without currents)

ZCONS2 = 1.0_JPRB/(RG*PTSTEP*PRVDIFTS)
DO JL=KIDIA,KFDIA
  DO JTILE=1,KTILES
    ZCDRO5 = ( RD*PTMLEV5(JL)*(1.0_JPRB+RETV*PQMLEV5(JL)) )/PAPHMS5(JL)
    ZKMFLTI5(JL,JTILE) = ZCDRO5 * ZCONS2 * ZCFMTI5(JL,JTILE) &
        & * SQRT(PUMLEV5(JL)**2+PVMLEV5(JL)**2)

!   use previous times tep fluxes for heat and moisture
    ZKHFLTI5(JL,JTILE) = ZCDRO5 * PAHFSTI5(JL,JTILE) / RCPD
    ZKQFLTI5(JL,JTILE) = ZCDRO5 * PEVAPTI5(JL,JTILE) 
  ENDDO
ENDDO

!          3.4    COMPUTE SURFACE FLUXES, WEIGHTED AVERAGE OVER TILES
!                 ------- ------- ------- -------- ------- ---- -----

PKMFL5(KIDIA:KFDIA) = 0.0_JPRB
PKHFL5(KIDIA:KFDIA) = 0.0_JPRB
PKQFL5(KIDIA:KFDIA) = 0.0_JPRB
PCFMLEV (KIDIA:KFDIA) = 0.0_JPRB
PCFMLEV5(KIDIA:KFDIA) = 0.0_JPRB
DO JTILE=1,KTILES
  DO JL=KIDIA,KFDIA
    PKMFL5(JL) = PKMFL5(JL)+PFRTI(JL,JTILE)*ZKMFLTI5(JL,JTILE)
    PKHFL5(JL) = PKHFL5(JL)+PFRTI(JL,JTILE)*ZKHFLTI5(JL,JTILE)
    PKQFL5(JL) = PKQFL5(JL)+PFRTI(JL,JTILE)*ZKQFLTI5(JL,JTILE)
    PCFMLEV (JL) = PCFMLEV(JL)+PFRTI(JL,JTILE)*ZCFMTI(JL,JTILE)
    PCFMLEV5(JL) = PCFMLEV5(JL)+PFRTI(JL,JTILE)*ZCFMTI5(JL,JTILE)
  ENDDO
ENDDO

!*         4.  Preparation for "POST-PROCESSING" of surface weather parameters

!          POST-PROCESSING WITH LOCAL INSTEAD OF EFFECTIVE
!          SURFACE ROUGHNESS LENGTH. THE LOCAL ONES ARE FOR
!          WMO-TYPE WIND STATIONS I.E. OPEN TERRAIN WITH GRASS

ZZ0MWMO=0.03_JPRB
ZZ0HWMO=0.003_JPRB
DO JL=KIDIA,KFDIA
  JTILE=IFRMAX(JL)
  IF (JTILE  >  2.AND. ZZ0MTI5(JL,JTILE)  >  ZZ0MWMO) THEN
    PZ0MW (JL) = 0.0_JPRB
    PZ0MW5(JL) = ZZ0MWMO 
    PZ0HW (JL) = 0.0_JPRB
    PZ0HW5(JL) = ZZ0HWMO
    PZ0QW (JL) = 0.0_JPRB
    PZ0QW5(JL) = ZZ0HWMO
  ELSE
    PZ0MW (JL) = ZZ0MTI (JL,JTILE)
    PZ0MW5(JL) = ZZ0MTI5(JL,JTILE)
    PZ0HW (JL) = ZZ0HTI (JL,JTILE)
    PZ0HW5(JL) = ZZ0HTI5(JL,JTILE)
    PZ0QW (JL) = ZZ0QTI (JL,JTILE)
    PZ0QW5(JL) = ZZ0QTI5(JL,JTILE)
  ENDIF
ENDDO

DO JTILE=1,KTILES
  DO JL=KIDIA,KFDIA
    ZZQSATI (JL,JTILE) = PQMLEV (JL)*(1.0_JPRB-PCAIRTI5(JL,JTILE)) &
     & - PQMLEV5(JL)*PCAIRTI (JL,JTILE) &
     & + PCSATTI5(JL,JTILE)*PQSTI (JL,JTILE) &
     & + PCSATTI (JL,JTILE)*PQSTI5(JL,JTILE)
    ZZQSATI5(JL,JTILE)=PQMLEV5(JL)*(1.0_JPRB-PCAIRTI5(JL,JTILE)) &
     & + PCSATTI5(JL,JTILE)*PQSTI5(JL,JTILE)
    IF (ZZQSATI5(JL,JTILE) < 1.0E-12_JPRB) THEN
      ZZQSATI (JL,JTILE) = 0.0_JPRB
      ZZQSATI5(JL,JTILE) = 1.0E-12_JPRB
    ENDIF
  ENDDO
ENDDO

DO JL=KIDIA,KFDIA
  JTILE=IFRMAX(JL)
  PCPTSPP (JL) = PCPTSTI (JL,JTILE)
  PCPTSPP5(JL) = PCPTSTI5(JL,JTILE)
  PQSAPP (JL) = ZZQSATI (JL,JTILE)
  PQSAPP5(JL) = ZZQSATI5(JL,JTILE)
  PBUOMPP (JL) = ZBUOMTI (JL,JTILE)
  PBUOMPP5(JL) = ZBUOMTI5(JL,JTILE)
ENDDO

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SURFEXCDRIVERSTL_CTL_MOD:SURFEXCDRIVERSTL_CTL',1,ZHOOK_HANDLE)

END SUBROUTINE SURFEXCDRIVERSTL_CTL
END MODULE SURFEXCDRIVERSTL_CTL_MOD
