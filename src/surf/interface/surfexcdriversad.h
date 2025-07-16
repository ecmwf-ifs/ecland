INTERFACE
SUBROUTINE SURFEXCDRIVERSAD  ( YDSURF, &
 &   KIDIA , KFDIA, KLON, KLEVS,KLEVSN, KTILES, KSTEP &
 & , PTSTEP, PRVDIFTS &
 & , LDNOPERT, LDKPERTS, LDSURF2, LDREGSF &
! input data, non-tiled - trajectory
 & , KTVL   , KTVH    , PCVL   , PCVH, PCUR &
 & , PLAIL  , PLAIH   &
 & , PSNM5 , PRSN5 &
 & , PUMLEV5, PVMLEV5 , PTMLEV5, PQMLEV5, PAPHMS5, PGEOMLEV5, PCPTGZLEV5 &
 & , PSST   , PTSKM1M5, PCHAR  , PSSRFL5, PTICE5 , PTSNOW5  &
 & , PWLMX5 &
 & , PUCURR5, PVCURR5 &
 & , PSSDP2 , PSSDP3 &
! input data, soil - trajectory
 & , PTSAM1M5, PWSAM1M5, KSOTY &
! input data, tiled - trajectory
 & , PFRTI, PALBTI5 &
! updated data, tiled - trajectory
 & , PUSTRTI5, PVSTRTI5, PAHFSTI5, PEVAPTI5, PTSKTI5 &
! updated data, non-tiled - trajectory
 & , PZ0M5, PZ0H5 &
! output data, tiled - trajectory
 & , PSSRFLTI5, PQSTI5 , PDQSTI5 , PCPTSTI5 &
 & , PCFHTI5  , PCFQTI5, PCSATTI5, PCAIRTI5 &
 & , PZ0MTIW5, PZ0HTIW5, PZ0QTIW5, PBUOMTI5, PQSAPPTI5, PCPTSPPTI5 &
! output data, non-tiled - trajectory
 & , PCFMLEV5, PEVAPSNW5 &
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
 & , PZ0MTIW, PZ0HTIW, PZ0QTIW, PBUOMTI, PQSAPPTI, PCPTSPPTI &
! output data, non-tiled
 & , PCFMLEV , PEVAPSNW &
 & , PZ0MW   , PZ0HW    , PZ0QW, PCPTSPP , PQSAPP, PBUOMPP &
 & )

USE PARKIND1, ONLY : JPIM, JPRB
USE, INTRINSIC :: ISO_C_BINDING

! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!------------------------------------------------------------------------

!  PURPOSE:
!    Routine SURFEXCDRIVERSAD controls the ensemble of routines that prepare
!    the surface exchange coefficients and associated surface quantities
!    needed for the solution of the vertical diffusion equations. 
!    (Adjoint)

!  SURFEXCDRIVERSAD is called by VDFMAINSAD

!  METHOD:
!    This routine is only a shell needed by the surface library
!    externalisation.

!  AUTHOR:
!    M. Janiskova       ECMWF July 2005   

!  REVISION HISTORY:
!    S. Boussetta/G.Balsamo May 2009 Add lai
!    M. Janiskova           Apr 2012 Perturbation of top layer surface fields
!    P. Lopez               June 2015 Added regularization of wet skin tile perturbation
!    P. Lopez               July 2025 Added ocean currents (trajectory only)

!  INTERFACE: 

!    Integers (In):
!      KIDIA    :    Begin point in arrays
!      KFDIA    :    End point in arrays
!      KLON     :    Length of arrays
!      KLEVS    :    Number of soil layers
!      KLEVSN   :    Number of snow layers
!      KTILES   :    Number of tiles
!      KSTEP    :    Time step index
!      KTVL     :    Dominant low vegetation type
!      KTVH     :    Dominant high vegetation type
!      KSOTY    :    SOIL TYPE                                       (1-7)

!  Reals (In):
!      PTSTEP   :    Timestep
!      PRVDIFTS :    Semi-implicit factor for vertical diffusion discretization
!      PCVL     :    Low vegetation fraction
!      PCVH     :    High vegetation fraction
!      PCUR     :    Urban fraction
!      PLAIL     :    Low vegetation LAI
!      PLAIH     :    High vegetation LAI


!  Logical:
!      LDNOPERT :    TRUE when no perturbations is required for surface arrays
!      LDKPERTS :    TRUE when pertubations of exchange coefficients are used
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
!  PWSAM1M5    PWSAM1M       SOIL MOISTURE ALL LAYERS                  m**3/m**3
!  PTICE5      PTICE         Ice temperature, top slab                 K
!  PTSNOW5     PTSNOW        Snow temperature                          K
!  PWLMX5      ---           Maximum interception layer capacity       kg/m**2
!  PUCURR5     ---           Ocean current U-component                 m/s
!  PVCURR5     ---           Ocean current V-component                 m/s
!  PSNM5       ---           SNOW MASS (per unit area)                 kg/m**2
!  PRSN5       ---           SNOW DENSITY                              kg/m**3

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

!     ** SURFEXCDRIVERS_CTL CALLS SUCCESSIVELY:
!         *VUPDZ0*
!         *VSURF*
!         *VEXCS*
!         *VEVAP*

!  DOCUMENTATION:
!    See Physics Volume of IFS documentation

!------------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments

TYPE(C_PTR)       ,INTENT(IN)    :: YDSURF
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVS
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVSN
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
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVL(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVH(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCUR(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAIL(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAIH(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSNM5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRSN5(:,:)
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
REAL(KIND=JPRB)   ,INTENT(IN)    :: PUCURR5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVCURR5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSDP2(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSDP3(:,:,:)
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
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PEVAPSNW5(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0MW5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0HW5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0QW5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCPTSPP5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQSAPP5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBUOMPP5(:)
! Tile dependent pp
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0MTIW5(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0HTIW5(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0QTIW5(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQSAPPTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCPTSPPTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBUOMTI5(:,:)

REAL(KIND=JPRB)   ,INTENT(INOUT) :: PUMLEV(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PVMLEV(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTMLEV(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQMLEV(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PAPHMS(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGEOMLEV(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PCPTGZLEV(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSKM1M(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSSRFL(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTICE(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSNOW(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSAM1M(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PWSAM1M(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PALBTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PUSTRTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PVSTRTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PAHFSTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PEVAPTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSKTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0M(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0H(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSSRFLTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQSTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PDQSTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PCPTSTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PCFHTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PCFQTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PCSATTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PCAIRTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PCFMLEV(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PEVAPSNW(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0MW(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0HW(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0QW(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PCPTSPP(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQSAPP(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PBUOMPP(:)
! Tile dependent pp
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0MTIW(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0HTIW(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0QTIW(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQSAPPTI(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCPTSPPTI(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBUOMTI(:,:)


!------------------------------------------------------------------------

END SUBROUTINE SURFEXCDRIVERSAD
END INTERFACE
