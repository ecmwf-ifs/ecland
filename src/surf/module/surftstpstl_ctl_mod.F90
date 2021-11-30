MODULE SURFTSTPSTL_CTL_MOD
CONTAINS
SUBROUTINE SURFTSTPSTL_CTL(KIDIA , KFDIA , KLON  , KLEVS , KLEVSN,&
 & KTILES, KSOTY,&
 & PTSPHY , PFRTI,&
!-trajectory
 & LDLAND  , LDSICE  , LDSI     , LDNH   , LDREGSF,&
 & PAHFSTI5, PEVAPTI5, PSSRFLTI5,&
 & PSNM1M5 , PTSNM1M5, PRSNM1M  ,&
 & PTSAM1M5, PTIAM1M5,&
 & PWLM1M5 , PWSAM1M5,&
 & PHLICEM1M5, PAPRS5,&
 & PRSFC5  , PRSFL5  ,&
 & PSLRFL5 , PSSFC5  , PSSFL5   ,&
 & PCVL    , PCVH    , PWLMX5   , PEVAPSNW5, &
 & YDCST   , YDVEG   , YDSOIL   , YDFLAKE, YDURB,&
!-TENDENCIES OUTPUT
 & PTSNE15 , PTSAE15 , PTIAE15  ,&
!-perturbations
 & PAHFSTI , PEVAPTI , PSSRFLTI ,&
 & PSNM1M  , PTSNM1M ,&
 & PTSAM1M , PTIAM1M ,&
 & PWLM1M  , PWSAM1M ,&
 & PAPRS   ,          &
 & PRSFC   , PRSFL   ,&
 & PSLRFL  , PSSFC   , PSSFL    ,&
 & PEVAPSNW,&
!-TENDENCIES OUTPUT
 & PTSNE1  , PTSAE1  , PTIAE1)

!-END OF CALL SURFTSTPSTL

USE PARKIND1  ,ONLY : JPIM, JPRB
USE YOMHOOK   ,ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_CST   ,ONLY : TCST
USE YOS_VEG   ,ONLY : TVEG
USE YOS_SOIL  ,ONLY : TSOIL
USE YOS_FLAKE ,ONLY : TFLAKE
USE YOS_URB   ,ONLY : TURB

USE SRFSN_LWIMPSTL_MOD
USE SRFSN_SSRABSSTL_MOD
USE SRFSN_WEBALSTL_MOD
USE SRFTSTL_MOD
USE SRFISTL_MOD
USE SRFWLSTL_MOD
USE SRFRCGSTL_MOD

#ifdef DOC
! (C) Copyright 2012- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SURFTSTPSTL* - UPDATES LAND VALUES OF TEMPERATURE AND SNOW.
!                     (Tangent linear)

!     PURPOSE.
!     --------
!          This routine updates the sea ice values of temperature
!     and the land values of soil temperature and snow temperature. 
!     For temperature, this is done via a forward time
!     step damped with some implicit linear considerations: as if all
!     fluxes that explicitely depend on the variable had only a linear
!     variation around the t-1 value. 

!**   INTERFACE.
!     ----------
!          *SURFTSTPSTL* IS CALLED FROM *CALLPARTL*.
!          THE ROUTINE TAKES ITS INPUT FROM THE LONG-TERM STORAGE:
!     TSA,WL,SN AT T-1,TSK,SURFACE FLUXES COMPUTED IN OTHER PARTS OF
!     THE PHYSICS, AND W AND LAND-SEA MASK. IT RETURNS AS AN OUTPUT
!     TENDENCIES TO THE SAME VARIABLES (TSA,WL,SN).


!     PARAMETER   DESCRIPTION                                         UNITS
!     ---------   -----------                                         -----
!     INPUT PARAMETERS (INTEGER):
!     *KIDIA*      START POINT
!     *KFDIA*      END POINT
!     *KLEV*       NUMBER OF LEVELS
!     *KLON*       NUMBER OF GRID POINTS PER PACKET
!     *KLEVS*      NUMBER OF SOIL LAYERS
!     *KTILES*     NUMBER OF TILES (I.E. SUBGRID AREAS WITH DIFFERENT
!                 SURFACE BOUNDARY CONDITION)
!     *KSOTY*      SOIL TYPE                                   (1-7)

!     INPUT PARAMETERS (LOGICAL):
!     *LDLAND*     LAND/SEA MASK (TRUE/FALSE)
!     *LDSICE*     SEA ICE MASK (.T. OVER SEA ICE)
!     *LDSI*       TRUE IF THERMALLY RESPONSIVE SEA-ICE
!     *LDNH*       TRUE FOR NORTHERN HEMISPHERE LATITUDE ROW
!     *LDREGSF*    TRUE WHEN REGULARIZATION USED

!     INPUT PARAMETERS (REAL):
!     *PTSPHY*     TIME STEP FOR THE PHYSICS                      S
!     *PFRTI*      TILE FRACTIONS                              (0-1)
!            1 : WATER                  5 : SNOW ON LOW-VEG+BARE-SOIL
!            2 : ICE                    6 : DRY SNOW-FREE HIGH-VEG
!            3 : WET SKIN               7 : SNOW UNDER HIGH-VEG
!            4 : DRY SNOW-FREE LOW-VEG  8 : BARE SOIL

!     INPUT PARAMETERS (REAL):
!  Trajectory  Perturbation  Description                                Unit
!  PAHFSTI5    PAHFSTI       SURFACE SENSIBLE HEAT FLUX, FOR EACH TILE  W/m2
!  PEVAPTI5    PEVAPTI       SURFACE MOISTURE FLUX, FOR EACH TILE       kg/m2/s
!  PSSRFLTI5   PSSRFLTI      NET SHORTWAVE RADIATION FLUX AT SURFACE
!                            FOR EACH TILE                              W/m2
  
!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!  Trajectory  Perturbation  Description                                Unit
!  PSNM1M5     PSNM1M        SNOW MASS (per unit area)                  kg/m2
!  PTSNM1M5    PTSNM1M       SNOW TEMPERATURE                           K
!  PRSNM1M     ---           SNOW DENSITY                               kg/m3
!  PTSAM1M5    PTSAM1M       SOIL TEMPERATURE                           K
!  PTIAM1M5    PTIAM1M       SEA-ICE TEMPERATURE                        K
!              (NB: REPRESENTS THE FRACTION OF ICE ONLY, NOT THE GRID-BOX)
!  PWLM1M5     PWLM1M        SKIN RESERVOIR WATER CONTENT               kg/m2
!  PWSAM1M5    PWSAM1M       SOIL MOISTURE                              m3/m3
!  PHLICEM1M5  ---           LAKE ICE THICKNESS                         m
!  PRSFC5      PRSFC         CONVECTIVE RAIN FLUX AT THE SURFACE        kg/m2/s
!  PRSFL5      PRSFL         LARGE SCALE RAIN FLUX AT THE SURFACE       kg/m2/s
!  PSLRFL5     PSLRFL        NET LONGWAVE  RADIATION AT THE SURFACE     W/m2
!  PSSFC5      PSSFC         CONVECTIVE  SNOW FLUX AT THE SURFACE       kg/m2/s
!  PSSFL5      PSSFL         LARGE SCALE SNOW FLUX AT THE SURFACE       kg/m2/s
!  PCVL        ---           LOW VEGETATION COVER  (CORRECTED)          (0-1)
!  PCVH        ---           HIGH VEGETATION COVER (CORRECTED)          (0-1)
!  PWLMX5      ---           MAXIMUM SKIN RESERVOIR CAPACITY            kg/m2
!  PEVAPSNW5   PEVAPSNW      EVAPORATION FROM SNOW UNDER FOREST         kg/m2/s

!     OUTPUT PARAMETERS (TENDENCIES):
!  Trajectory  Perturbation  Description                                Unit
!  PTSNE15     PTSNE1        SNOW TEMPERATURE                           K/s
!  PTSAE15     PTSAE1        SOIL TEMPERATURE TENDENCY                  K/s
!  PTIAE15     PTIAE1        SEA-ICE TEMPERATURE TENDENCY               K/s


!     METHOD.
!     -------
!          STRAIGHTFORWARD ONCE THE DEFINITION OF THE CONSTANTS IS
!     UNDERSTOOD. FOR THIS REFER TO DOCUMENTATION. FOR THE TIME FILTER
!     SEE CORRESPONDING PART OF THE DOCUMENTATION OF THE ADIABATIC CODE.

!     EXTERNALS.
!     ----------
!          *SRFWLSTL*        COMPUTES THE SKIN RESERVOIR CHANGES.
!          *SRFSN_LWIMPSTL*  REVISED SNOW SCHEME W. DIAG. LIQ. WATER.
!          *SRFRCGSTL*       COMPUTE SOIL HEAT CAPACITY.
!          *SRFTSTL*         COMPUTES THE TEMPERATURE CHANGES BEFORE THE SNOW
!                            MELTING.
!          *SRFISTL*         COMPUTES TEMPERATURE EVOLUTION OF SEA ICE.

!     REFERENCE.
!     ----------
!          SEE SOIL PROCESSES' PART OF THE MODEL'S DOCUMENTATION FOR
!     DETAILS ABOUT THE MATHEMATICS OF THIS ROUTINE.

!     Original   
!     --------
!     M. Janiskova              E.C.M.W.F.     31-01-2012  

!     Modifications
!     -------------
!    J. McNorton           24/08/2022  urban tile

!     ------------------------------------------------------------------
#endif

IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVS
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVSN
INTEGER(KIND=JPIM),INTENT(IN)    :: KTILES
INTEGER(KIND=JPIM),INTENT(IN)    :: KSOTY(:)
LOGICAL           ,INTENT(IN)    :: LDLAND(:)
LOGICAL           ,INTENT(IN)    :: LDSICE(:)
LOGICAL           ,INTENT(IN)    :: LDSI
LOGICAL           ,INTENT(IN)    :: LDNH(:)
LOGICAL           ,INTENT(IN)    :: LDREGSF
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSPHY
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFRTI(:,:)

REAL(KIND=JPRB)   ,INTENT(IN)    :: PAHFSTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PEVAPTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSRFLTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSNM1M5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSNM1M5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRSNM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSAM1M5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTIAM1M5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWLM1M5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWSAM1M5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRSFC5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRSFL5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSLRFL5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSFC5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSFL5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVL(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVH(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWLMX5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PEVAPSNW5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PHLICEM1M5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRS5(:)
TYPE(TCST)        ,INTENT(IN)    :: YDCST
TYPE(TVEG)        ,INTENT(IN)    :: YDVEG
TYPE(TSOIL)       ,INTENT(IN)    :: YDSOIL
TYPE(TFLAKE)      ,INTENT(IN)    :: YDFLAKE
TYPE(TURB)        ,INTENT(IN)    :: YDURB

REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSNE15(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSAE15(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTIAE15(:,:)

REAL(KIND=JPRB)   ,INTENT(IN)    :: PAHFSTI(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PEVAPTI(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSRFLTI(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSNM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSNM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSAM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTIAM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWLM1M(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWSAM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRS(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRSFC(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRSFL(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSLRFL(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSFC(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSFL(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PEVAPSNW(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSNE1(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSAE1(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTIAE1(:,:)


!*         0.2    DECLARATION OF LOCAL VARIABLES.
!

REAL(KIND=JPRB) :: ZRSFC5(KLON),ZRSFL5(KLON),ZSSFC5(KLON),ZSSFL5(KLON)
REAL(KIND=JPRB) :: ZTSFC5(KLON),ZTSFL5(KLON)
REAL(KIND=JPRB) :: ZTSN5(KLON,KLEVSN) ,ZGSN5(KLON)
REAL(KIND=JPRB) :: ZCTSA5(KLON,KLEVS)
REAL(KIND=JPRB) :: ZTSA5(KLON,KLEVS),ZTIA5(KLON,KLEVS)

REAL(KIND=JPRB) :: ZRSFC(KLON),ZRSFL(KLON),ZSSFC(KLON),ZSSFL(KLON)
REAL(KIND=JPRB) :: ZTSFC(KLON),ZTSFL(KLON)
REAL(KIND=JPRB) :: ZTSN(KLON,KLEVSN) ,ZGSN(KLON)
REAL(KIND=JPRB) :: ZCTSA(KLON,KLEVS)
REAL(KIND=JPRB) :: ZTSA(KLON,KLEVS),ZTIA(KLON,KLEVS)

REAL(KIND=JPRB) :: ZFRSN
LOGICAL         :: LLNOSNOW(KLON)
REAL(KIND=JPRB) :: ZWSNM1M5(KLON,KLEVSN),  ZWSNM1M(KLON,KLEVSN) 
REAL(KIND=JPRB) :: ZSNOTRS5(KLON,KLEVSN+1),ZSNOTRS(KLON,KLEVSN+1) 

REAL(KIND=JPRB) :: ZTSPHY

INTEGER(KIND=JPIM) :: JK, JL
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

!*         1.1   SET UP SOME CONSTANTS, INITIALISE ARRAYS.
!                --- -- ---- -----------------------------
IF (LHOOK) CALL DR_HOOK('SURFTSTPTL_CTL_MOD:SURFTSTPTL_CTL',0,ZHOOK_HANDLE)

ZTSPHY=1.0_JPRB/PTSPHY

DO JL=KIDIA,KFDIA
! protect against negative precip
  IF (PRSFC5(JL) < 0.0_JPRB) THEN
    ZRSFC (JL) = 0.0_JPRB
    ZRSFC5(JL) = 0.0_JPRB
  ELSE
    ZRSFC (JL) = PRSFC (JL)
    ZRSFC5(JL) = PRSFC5(JL)
  ENDIF
  IF (PRSFL5(JL) < 0.0_JPRB) THEN
    ZRSFL (JL) = 0.0_JPRB
    ZRSFL5(JL) = 0.0_JPRB
  ELSE
    ZRSFL (JL) = PRSFL (JL)
    ZRSFL5(JL) = PRSFL5(JL)
  ENDIF
  IF (PSSFC5(JL) < 0.0_JPRB) THEN
    ZSSFC (JL) = 0.0_JPRB
    ZSSFC5(JL) = 0.0_JPRB
  ELSE
    ZSSFC (JL) = PSSFC (JL)
    ZSSFC5(JL) = PSSFC5(JL)
  ENDIF
  IF (PSSFL5(JL) < 0.0_JPRB) THEN
    ZSSFL (JL) = 0.0_JPRB
    ZSSFL5(JL) = 0.0_JPRB
  ELSE
    ZSSFL (JL) = PSSFL (JL)
    ZSSFL5(JL) = PSSFL5(JL)
  ENDIF
ENDDO

!     ------------------------------------------------------------------
!*         0.1     INTERCEPTION RESERVOIR AND ASSOCIATED RUNOFF CHANGES.

CALL SRFWLSTL(KIDIA ,KFDIA ,KLON,&
 & PTSPHY ,PWLM1M5  ,PCVL  ,PCVH  ,PWLMX5,&
 & PFRTI  ,PEVAPTI5 ,ZRSFC5,ZRSFL5,&
 & LDLAND ,&
 & YDSOIL ,YDVEG    ,&
 & ZTSFC5 ,ZTSFL5   ,&
 & PWLM1M ,&
 & PEVAPTI,ZRSFC    ,ZRSFL ,&
 & ZTSFC  ,ZTSFL )

!     ------------------------------------------------------------------

!*         2.     SNOW CHANGES.

IF (YDSOIL%LESNML) THEN
!--
! ! Initialise some snowML related working arrays
  DO JL=KIDIA,KFDIA
    ZFRSN=MAX(PFRTI(JL,5)+PFRTI(JL,7),YDSOIL%RFRTINY)
    IF (ZFRSN < YDSOIL%RFRSMALL) THEN
      LLNOSNOW(JL)=.TRUE.
    ELSE
      LLNOSNOW(JL)=.FALSE.
    ENDIF
  ENDDO
! Input Liquid water is not yet considered in tl/ad ml
  ZWSNM1M5(KIDIA:KFDIA, 1:KLEVSN)    = 0._JPRB
  ZWSNM1M(KIDIA:KFDIA, 1:KLEVSN)     = 0._JPRB
  ZSNOTRS5(KIDIA:KFDIA, 1:KLEVSN+1)  = 0._JPRB
  ZSNOTRS(KIDIA:KFDIA, 1:KLEVSN+1)   = 0._JPRB
  CALL SRFSN_SSRABSSTL(KIDIA,KFDIA,KLON,KLEVSN,&
                     & LLNOSNOW, PFRTI,&
                     & PSSRFLTI5, PSNM1M5, PRSNM1M,&
                     &  YDSOIL,YDCST,&
                     & ZSNOTRS5, &
!------- perturbations
                     & PSSRFLTI, PSNM1M, &
                     & ZSNOTRS           &
                     & )

  CALL SRFSN_WEBALSTL(KIDIA,KFDIA,KLON,KLEVSN,   &
    & PTSPHY,PFRTI,LDLAND, LLNOSNOW,             &
    & PSNM1M5,ZWSNM1M5,PRSNM1M,PTSNM1M5,         &
    & PSLRFL5,PSSRFLTI5,PAHFSTI5 ,PEVAPTI5,      &
    & ZSSFC5, ZSSFL5, PEVAPSNW5, ZTSFC5  ,ZTSFL5,&
    & PTSAM1M5(KIDIA:KFDIA,1), ZSNOTRS5,         &
    & PAPRS5, PWSAM1M5(KIDIA:KFDIA,1), KSOTY,    &
    & YDSOIL,YDCST,                              &
    & ZTSN5, ZGSN5,                              &
!--- perturbations
    & PSNM1M,ZWSNM1M,PTSNM1M,                &
    & PSLRFL,PSSRFLTI,PAHFSTI ,PEVAPTI,      &
    & ZSSFC, ZSSFL, PEVAPSNW, ZTSFC  ,ZTSFL, &
    & PTSAM1M(KIDIA:KFDIA,1), ZSNOTRS,       &
    & PAPRS, PWSAM1M(KIDIA:KFDIA,1),         &
    & ZTSN, ZGSN )

ELSE
!* LIQUID WATER DIAGNOSTIC  (LESN09)
  CALL SRFSN_LWIMPSTL(KIDIA  ,KFDIA  ,KLON ,LDLAND,&
   & PTSPHY  ,LDREGSF  ,&
   & PSNM1M5(KIDIA:KFDIA,1) ,PTSNM1M5(KIDIA:KFDIA,1) ,PRSNM1M(KIDIA:KFDIA,1)  ,&
   &  PTSAM1M5 , PTIAM1M5, PHLICEM1M5,&
   & PSLRFL5 ,PSSRFLTI5,PFRTI    ,PAHFSTI5 ,PEVAPTI5,&
   & ZSSFC5  ,ZSSFL5   ,PEVAPSNW5,&
   & ZTSFC5  ,ZTSFL5   ,&
   & YDCST   ,YDVEG    ,YDSOIL   ,YDFLAKE  ,YDURB ,&
   & ZTSN5(KIDIA:KFDIA,1)   ,ZGSN5    ,&
   & PSNM1M(KIDIA:KFDIA,1)  ,PTSNM1M(KIDIA:KFDIA,1)  ,PTSAM1M ,PTIAM1M,&
   & PSLRFL  ,PSSRFLTI ,PAHFSTI ,PEVAPTI,&
   & ZSSFC   ,ZSSFL    ,PEVAPSNW,&
   & ZTSFC   ,ZTSFL    ,&
   & ZTSN(KIDIA:KFDIA,1)    ,ZGSN )
ENDIF
!     ------------------------------------------------------------------
!*         3.     SOIL HEAT BUDGET.
!                 ---- ---- -------

!*         3.1     COMPUTE SOIL HEAT CAPACITY.

CALL SRFRCGSTL(KIDIA ,KFDIA  ,KLON ,KLEVS ,&
 & LDLAND   ,LDSICE  ,LDREGSF,&
 & PTSAM1M5 ,KSOTY   ,PCVL   ,PCVH ,&
 & YDCST    ,YDSOIL  ,&
 & ZCTSA5   ,&
 & PTSAM1M  ,&
 & ZCTSA)


!*         3.2     TEMPERATURE CHANGES.

! soil
CALL SRFTSTL(KIDIA    ,KFDIA    ,KLON   ,KLEVS  , &
 & PTSPHY  ,PTSAM1M5  ,PWSAM1M5 ,KSOTY  , &
 & PFRTI   ,PAHFSTI5  ,PEVAPTI5 ,&
 & PSLRFL5 ,PSSRFLTI5 ,ZGSN5    ,&
 & ZCTSA5  ,ZTSA5     ,LDLAND   ,&
 & YDCST   ,YDSOIL    ,YDFLAKE  ,YDURB,&  
 & PTSAM1M ,PWSAM1M   ,&
 & PAHFSTI ,PEVAPTI   ,&
 & PSLRFL  ,PSSRFLTI  ,ZGSN     , &
 & ZCTSA   ,ZTSA   &
 & )

! sea-ice
IF (LDSI) THEN
  CALL SRFISTL(KIDIA    ,KFDIA    , KLON    , KLEVS  , &
   & PTSPHY  ,PFRTI     ,PTIAM1M5  ,PAHFSTI5 , PEVAPTI5, ZGSN5  , &
   & PSLRFL5 ,PSSRFLTI5 ,ZTIA5    , LDSICE  , LDNH   ,&
   & YDCST   , YDSOIL   ,&
   & PTIAM1M ,PAHFSTI   ,PEVAPTI  , ZGSN, &
   & PSLRFL  ,PSSRFLTI  ,ZTIA     &
   & )
ELSE
  DO JK=1,KLEVS
    DO JL=KIDIA,KFDIA
      ZTIA (JL,JK) = PTIAM1M (JL,JK)
      ZTIA5(JL,JK) = PTIAM1M5(JL,JK)
    ENDDO
  ENDDO
ENDIF

!     ------------------------------------------------------------------
!*         5.     COMPUTE TENDENCIES (TEND. FOR TSK COMP. IN VDIFF)
!                 ------- ---------- ------------------------------
!*         5.1   TENDENCIES

DO JK=1,KLEVSN
  DO JL=KIDIA,KFDIA
    PTSNE1 (JL,JK) = PTSNE1 (JL,JK)+(ZTSN (JL,JK)-PTSNM1M (JL,JK))*ZTSPHY
    PTSNE15(JL,JK) = PTSNE15(JL,JK)+(ZTSN5(JL,JK)-PTSNM1M5(JL,JK))*ZTSPHY
  ENDDO
ENDDO

DO JK=1,KLEVS
  DO JL=KIDIA,KFDIA
    PTIAE1 (JL,JK) = PTIAE1 (JL,JK)+(ZTIA (JL,JK)-PTIAM1M (JL,JK))*ZTSPHY
    PTIAE15(JL,JK) = PTIAE15(JL,JK)+(ZTIA5(JL,JK)-PTIAM1M5(JL,JK))*ZTSPHY
  ENDDO
ENDDO
! PTSA represents the grid-box average T; the sea-ice fraction
!    has a T representative of the fraction
DO JK=1,KLEVS
  DO JL=KIDIA,KFDIA
    IF (LDSICE(JL)) THEN
      PTSAE1 (JL,JK) = PTSAE1 (JL,JK) &
       & +((PFRTI(JL,2)+PFRTI(JL,5))*(ZTIA (JL,JK)-PTIAM1M (JL,JK)) &
       & +(1.0_JPRB-PFRTI(JL,1)-PFRTI(JL,2)-PFRTI(JL,5)) &
       & *(ZTSA (JL,JK)-PTSAM1M (JL,JK)))*ZTSPHY
      PTSAE15(JL,JK) = PTSAE15(JL,JK) &
       & +((PFRTI(JL,2)+PFRTI(JL,5))*(ZTIA5(JL,JK)-PTIAM1M5(JL,JK)) &
       & +(1.0_JPRB-PFRTI(JL,1)-PFRTI(JL,2)-PFRTI(JL,5)) &
       & *(ZTSA5(JL,JK)-PTSAM1M5(JL,JK)))*ZTSPHY
    ELSE
      PTSAE1 (JL,JK) = PTSAE1 (JL,JK) &
       & +(PFRTI(JL,2)*(ZTIA (JL,JK)-PTIAM1M (JL,JK)) &
       & +(1.0_JPRB-PFRTI(JL,1)-PFRTI(JL,2)) &
       & *(ZTSA (JL,JK)-PTSAM1M (JL,JK)))*ZTSPHY
      PTSAE15(JL,JK) = PTSAE15(JL,JK) &
       & +(PFRTI(JL,2)*(ZTIA5(JL,JK)-PTIAM1M5(JL,JK)) &
       & +(1.0_JPRB-PFRTI(JL,1)-PFRTI(JL,2)) &
       & *(ZTSA5(JL,JK)-PTSAM1M5(JL,JK)))*ZTSPHY
     ENDIF
  ENDDO
ENDDO

IF (LHOOK) CALL DR_HOOK('SURFTSTPSTL_CTL_MOD:SURFTSTPTL_CTL',1,ZHOOK_HANDLE)

END SUBROUTINE SURFTSTPSTL_CTL
END MODULE SURFTSTPSTL_CTL_MOD
