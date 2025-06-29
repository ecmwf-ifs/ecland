MODULE SURFTSTPSAD_CTL_MOD
CONTAINS
SUBROUTINE SURFTSTPSAD_CTL(KIDIA , KFDIA , KLON  , KLEVS , KLEVSN,& 
 & KTILES, KSOTY,&
 & PTSPHY , PFRTI,&
!-trajectory
 & LDLAND  , LDSICE  , LDSI     , LDNH   , LDREGSF,&
 & PAHFSTI5, PEVAPTI5, PSSRFLTI5,&
 & PSNM1M5 , PTSNM1M5, PRSNM1M  ,&
 & PTSAM1M5, PTIAM1M5,&
 & PWLM1M5 , PWSAM1M5,&
 & PHLICEM1M5,PAPRS5, &
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

USE SRFSN_SSRABSS_MOD
USE SRFSN_SSRABSSAD_MOD
USE SRFSN_WEBALS_MOD
USE SRFSN_WEBALSAD_MOD
USE SRFSN_LWIMPS_MOD
USE SRFSN_LWIMPSAD_MOD
USE SRFTS_MOD
USE SRFTSAD_MOD
USE SRFIS_MOD
USE SRFISAD_MOD
USE SRFWLS_MOD
USE SRFWLSAD_MOD
USE SRFRCGS_MOD
USE SRFRCGSAD_MOD

#ifdef DOC
! (C) Copyright 2012- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SURFTSTPSAD* - UPDATES LAND VALUES OF TEMPERATURE AND SNOW.
!                     (Adjoint)

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
!          *SURFTSTPSAD* IS CALLED FROM *CALLPARAD*.
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
!     *SRFWLS*       *SRFWLSAD*        COMPUTE THE SKIN RESERVOIR CHANGES.
!     *SRFSN_LWIMP*  *SRFSN_LWIMPSAD*  REVISED SNOW SCHEME W. DIAG. LIQ. WATER.
!     *SRFRCGS*      *SRFRCGSAD*       COMPUTE SOIL HEAT CAPACITY.
!     *SRFTS*        *SRFTSAD*         COMPUTE TEMPERATURE CHANGES BEFORE SNOW
!                                      MELTING.
!     *SRFIS*        *SRFISAD*         COMPUTES TEMPERATURE EVOLUTION OF SEA ICE

!     REFERENCE.
!     ----------
!          SEE SOIL PROCESSES' PART OF THE MODEL'S DOCUMENTATION FOR
!     DETAILS ABOUT THE MATHEMATICS OF THIS ROUTINE.

!     Original   
!     --------
!     M. Janiskova              E.C.M.W.F.     03-04-2012  

!     Modifications
!     -------------
!     J. McNorton           24/08/2022  urban tile

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

REAL(KIND=JPRB)   ,INTENT(INOUT) :: PAHFSTI(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PEVAPTI(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSSRFLTI(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSNM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSNM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSAM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTIAM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PWLM1M(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PWSAM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PAPRS(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PRSFC(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PRSFL(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSLRFL(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSSFC(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSSFL(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PEVAPSNW(:)
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
REAL(KIND=JPRB) :: ZSNOTRS5(KLON,KLEVSN+1), ZSNOTRS(KLON,KLEVSN+1)
REAL(KIND=JPRB) :: ZTSNSAVE5(KLON,KLEVSN)
REAL(KIND=JPRB) :: ZWSNM1M5(KLON,KLEVSN),ZWSNM1M(KLON,KLEVSN)

REAL(KIND=JPRB) :: ZTSPHY

INTEGER(KIND=JPIM) :: JK, JL
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

!*         1.1   SET UP SOME CONSTANTS, INITIALISE ARRAYS.
!                --- -- ---- -----------------------------
IF (LHOOK) CALL DR_HOOK('SURFTSTPAD_CTL_MOD:SURFTSTPAD_CTL',0,ZHOOK_HANDLE)

ZTSPHY=1.0_JPRB/PTSPHY

DO JL=KIDIA,KFDIA
! protect against negative precip
  IF (PRSFC5(JL) < 0.0_JPRB) THEN
    ZRSFC5(JL) = 0.0_JPRB
  ELSE
    ZRSFC5(JL) = PRSFC5(JL)
  ENDIF
  IF (PRSFL5(JL) < 0.0_JPRB) THEN
    ZRSFL5(JL) = 0.0_JPRB
  ELSE
    ZRSFL5(JL) = PRSFL5(JL)
  ENDIF
  IF (PSSFC5(JL) < 0.0_JPRB) THEN
    ZSSFC5(JL) = 0.0_JPRB
  ELSE
    ZSSFC5(JL) = PSSFC5(JL)
  ENDIF
  IF (PSSFL5(JL) < 0.0_JPRB) THEN
    ZSSFL5(JL) = 0.0_JPRB
  ELSE
    ZSSFL5(JL) = PSSFL5(JL)
  ENDIF
ENDDO

!     ------------------------------------------------------------------
!*         1.     INTERCEPTION RESERVOIR AND ASSOCIATED RUNOFF CHANGES.

CALL SRFWLS(KIDIA,KFDIA,KLON,&
 & PTSPHY ,PWLM1M5  ,PCVL   ,PCVH   ,PWLMX5,&
 & PFRTI  ,PEVAPTI5 ,ZRSFC5 ,ZRSFL5 ,&
 & LDLAND ,&
 & YDSOIL ,YDVEG,&
 & ZTSFC5 ,ZTSFL5)

!     ------------------------------------------------------------------

!*         2.     SNOW CHANGES.
IF (YDSOIL%LESNML) THEN
  ! Initialise some snowML related working arrays
  DO JL=KIDIA,KFDIA
    ZFRSN=MAX(PFRTI(JL,5)+PFRTI(JL,7),YDSOIL%RFRTINY)
    IF (ZFRSN < YDSOIL%RFRSMALL) THEN
      LLNOSNOW(JL)=.TRUE.
    ELSE
      LLNOSNOW(JL)=.FALSE.
    ENDIF
  ENDDO
  ZWSNM1M5(KIDIA:KFDIA, 1:KLEVSN)     = 0._JPRB
  ZSNOTRS5(KIDIA:KFDIA, 1:KLEVSN+1)   = 0._JPRB
  ! 2.1 Compute absorption of solar radiation into the snowpack
  CALL SRFSN_SSRABSS(KIDIA,KFDIA,KLON,KLEVSN,&
                  &  LLNOSNOW,PFRTI,&
                  &  PSSRFLTI5, PSNM1M5,PRSNM1M,&
                  &  YDSOIL,YDCST,&
                  &  ZSNOTRS5)
! 2.2 Compute energy balance of the snowpack
  CALL SRFSN_WEBALS(KIDIA,KFDIA,KLON,KLEVSN,&
   & PTSPHY,PFRTI,LDLAND, LLNOSNOW,  &
   & PSNM1M5,ZWSNM1M5,PRSNM1M,PTSNM1M5,&
   & PSLRFL5,PSSRFLTI5,PAHFSTI5 ,PEVAPTI5,&
   & ZSSFC5, ZSSFL5, PEVAPSNW5, ZTSFC5  ,ZTSFL5   ,&
   & PTSAM1M5(KIDIA:KFDIA,1), ZSNOTRS5,         &
   & PAPRS5, PWSAM1M5(KIDIA:KFDIA,1), KSOTY,&
   & YDSOIL,YDCST,&
   & ZTSN5, ZGSN5)
 
ELSE

!* LIQUID WATER DIAGNOSTIC  (LESN09)
!* working array in srfsn_lwimps are 1-d
  CALL SRFSN_LWIMPS(KIDIA  ,KFDIA  ,KLON   ,PTSPHY, &
   & PSNM1M5(KIDIA:KFDIA,1) ,PTSNM1M5(KIDIA:KFDIA,1)  ,PRSNM1M(KIDIA:KFDIA,1)   ,&
   & PTSAM1M5 ,PHLICEM1M5,&
   & PSLRFL5 ,PSSRFLTI5 ,PFRTI     ,PAHFSTI5 ,PEVAPTI5,  &
   & ZSSFC5  ,ZSSFL5    ,PEVAPSNW5 ,&
   & ZTSFC5  ,ZTSFL5    ,&
   & YDCST   ,YDVEG     ,YDSOIL    ,YDFLAKE  ,YDURB,&
   & ZTSN5(KIDIA:KFDIA,1)   ,ZGSN5 )
ENDIF
!     ------------------------------------------------------------------
!*         3.     SOIL HEAT BUDGET.
!                 ---- ---- -------

!*         3.1     COMPUTE SOIL HEAT CAPACITY.

CALL SRFRCGS(KIDIA  , KFDIA  , KLON, KLEVS ,&
 & LDLAND  , LDSICE ,&
 & PTSAM1M5, KSOTY  , PCVL   , PCVH   ,&
 & YDCST   , YDSOIL ,&
 & ZCTSA5)

!*         3.2     TEMPERATURE CHANGES.

! soil
CALL SRFTS(KIDIA , KFDIA , KLON , KLEVS , &
 & PTSPHY  ,PTSAM1M5  ,PWSAM1M5 , KSOTY , &
 & PFRTI   ,PAHFSTI5  ,PEVAPTI5 ,&
 & PSLRFL5 ,PSSRFLTI5 ,ZGSN5 ,&
 & ZCTSA5  ,ZTSA5     ,LDLAND,&
 & YDCST   ,YDSOIL    ,YDFLAKE, YDURB)  

! sea-ice
IF (LDSI) THEN
  CALL SRFIS(KIDIA , KFDIA , KLON , KLEVS  , &
   & PTSPHY  ,PTIAM1M5  ,PAHFSTI5 ,PEVAPTI5, &
   & PSLRFL5 ,PSSRFLTI5 ,ZTIA5    ,LDSICE  , LDNH  ,&
   & YDCST   ,YDSOIL)
ELSE
  DO JK=1,KLEVS
    DO JL=KIDIA,KFDIA
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
    PTSNE15(JL,JK) = PTSNE15(JL,JK)+(ZTSN5(JL,JK)-PTSNM1M5(JL,JK))*ZTSPHY
  ENDDO
ENDDO

DO JK=1,KLEVS
  DO JL=KIDIA,KFDIA
    PTIAE15(JL,JK) = PTIAE15(JL,JK)+(ZTIA5(JL,JK)-PTIAM1M5(JL,JK))*ZTSPHY
  ENDDO
ENDDO
! PTSA represents the grid-box average T; the sea-ice fraction
!    has a T representative of the fraction
DO JK=1,KLEVS
  DO JL=KIDIA,KFDIA
    PTSAE15(JL,JK) = PTSAE15(JL,JK) &
     & +(PFRTI(JL,2)*(ZTIA5(JL,JK)-PTIAM1M5(JL,JK)) &
     & +(1.0_JPRB-PFRTI(JL,1)-PFRTI(JL,2)) &
     & *(ZTSA5(JL,JK)-PTSAM1M5(JL,JK)))*ZTSPHY
  ENDDO
ENDDO

!          0.  ADJOINT CALCULATIONS
!              --------------------

!* Set local variables to zero

ZRSFC(:) = 0.0_JPRB
ZRSFL(:) = 0.0_JPRB
ZSSFC(:) = 0.0_JPRB
ZSSFL(:) = 0.0_JPRB
ZTSFC(:) = 0.0_JPRB
ZTSFL(:) = 0.0_JPRB
ZTSN (:,:) = 0.0_JPRB
ZGSN (:)   = 0.0_JPRB
ZCTSA(:,:) = 0.0_JPRB
ZTSA (:,:) = 0.0_JPRB
ZTIA (:,:) = 0.0_JPRB

!     ------------------------------------------------------------------
!*         0.5.     COMPUTE TENDENCIES (TEND. FOR TSK COMP. IN VDIFF)
!                   ------- ---------- ------------------------------
!*         0.5.1   TENDENCIES

DO JK=KLEVS,1,-1
  DO JL=KIDIA,KFDIA
    ZTIA(JL,JK) = ZTIA(JL,JK)+ZTSPHY*PFRTI(JL,2)*PTSAE1(JL,JK) 
    PTIAM1M(JL,JK) = PTIAM1M(JL,JK)-ZTSPHY*PFRTI(JL,2)*PTSAE1(JL,JK)
    ZTSA(JL,JK) = ZTSA(JL,JK) &
     & +ZTSPHY*(1.0_JPRB-PFRTI(JL,1)-PFRTI(JL,2))*PTSAE1(JL,JK)
    PTSAM1M (JL,JK) = PTSAM1M (JL,JK) &
     & -ZTSPHY*(1.0_JPRB-PFRTI(JL,1)-PFRTI(JL,2))*PTSAE1(JL,JK)
  ENDDO
ENDDO

DO JK=KLEVS,1,-1
  DO JL=KIDIA,KFDIA
    ZTIA(JL,JK) = ZTIA(JL,JK)+ZTSPHY*PTIAE1(JL,JK)
    PTIAM1M(JL,JK) = PTIAM1M(JL,JK)-ZTSPHY*PTIAE1(JL,JK)
  ENDDO
ENDDO

DO JK=KLEVSN,1,-1
  DO JL=KIDIA,KFDIA
    ZTSN(JL,JK)    = ZTSN(JL,JK)+ZTSPHY*PTSNE1(JL,JK)
    PTSNM1M(JL,JK) = PTSNM1M(JL,JK)-ZTSPHY*PTSNE1(JL,JK)
  ENDDO
ENDDO

!     ------------------------------------------------------------------
!*         0.3.     SOIL HEAT BUDGET.
!                   ---- ---- -------

!*         0.3.2     TEMPERATURE CHANGES.

! sea-ice
IF (LDSI) THEN
  CALL SRFISAD(KIDIA    ,KFDIA    , KLON    , KLEVS  , &
   & PTSPHY  ,PTIAM1M5  ,PAHFSTI5 , PEVAPTI5, &
   & PSLRFL5 ,PSSRFLTI5 ,ZTIA5    , LDSICE  , LDNH   ,&
   & YDCST   , YDSOIL   ,&
   & PTIAM1M ,PAHFSTI   ,PEVAPTI  , &
   & PSLRFL  ,PSSRFLTI  ,ZTIA     &
   & )
ELSE
  DO JK=KLEVS,1,-1
    DO JL=KIDIA,KFDIA
      PTIAM1M(JL,JK) = PTIAM1M(JL,JK)+ZTIA(JL,JK)
      ZTIA(JL,JK) = 0.0_JPRB
    ENDDO
  ENDDO
ENDIF

! soil
CALL SRFTSAD(KIDIA    ,KFDIA    ,KLON   ,KLEVS  , &
 & PTSPHY  ,PTSAM1M5  ,PWSAM1M5 ,KSOTY  , &
 & PFRTI   ,PAHFSTI5  ,PEVAPTI5 ,&
 & PSLRFL5 ,PSSRFLTI5 ,ZGSN5    ,&
 & ZCTSA5  ,ZTSA5     ,LDLAND   ,&
 & YDCST   , YDSOIL   ,YDFLAKE  , YDURB ,&  
 & PTSAM1M ,PWSAM1M   ,&
 & PAHFSTI ,PEVAPTI   ,&
 & PSLRFL  ,PSSRFLTI  ,ZGSN     , &
 & ZCTSA   ,ZTSA   &
 & )

!*         0.3.1     COMPUTE SOIL HEAT CAPACITY.

CALL SRFRCGSAD(KIDIA ,KFDIA  ,KLON ,KLEVS ,&
 & LDLAND   ,LDSICE  ,LDREGSF,&
 & PTSAM1M5 ,KSOTY   ,PCVL   ,PCVH ,&
 & YDCST    , YDSOIL , &
 & ZCTSA5   ,&
 & PTSAM1M  ,&
 & ZCTSA)

!     ------------------------------------------------------------------

!*         0.2.     SNOW CHANGES.

!* LIQUID WATER DIAGNOSTIC  (LESN09)

IF (YDSOIL%LESNML) THEN
  ! Initialise some snowML related working arrays
  ZWSNM1M(KIDIA:KFDIA,1:KLEVSN)  = 0._JPRB
  ZSNOTRS(KIDIA:KFDIA,1:KLEVSN+1)  = 0._JPRB

! 2.2 Compute energy balance of the snowpack
  CALL SRFSN_WEBALSAD(KIDIA,KFDIA,KLON,KLEVSN,   &
   & PTSPHY, PFRTI,LDLAND, LLNOSNOW,             &
   & PSNM1M5,ZWSNM1M5,PRSNM1M,PTSNM1M5,         &
   & PSLRFL5,PSSRFLTI5,PAHFSTI5 ,PEVAPTI5,      &
   & ZSSFC5, ZSSFL5, PEVAPSNW5, ZTSFC5  ,ZTSFL5,&
   & PTSAM1M5(KIDIA:KFDIA,1), ZSNOTRS5,         &
   & PAPRS5, PWSAM1M5(KIDIA:KFDIA,1), KSOTY,    &
   & YDSOIL,YDCST,                              &
   & ZTSN5, ZGSN5,                              &
!--- perturbations
   & PSNM1M,ZWSNM1M,PTSNM1M,                &
   & PSLRFL,PSSRFLTI,PAHFSTI, PEVAPTI,      &
   & ZSSFC, ZSSFL, PEVAPSNW, ZTSFC  ,ZTSFL, &
   & PTSAM1M(KIDIA:KFDIA,1), ZSNOTRS,       &
   & PAPRS, PWSAM1M(KIDIA:KFDIA,1),         &
   & ZTSN, ZGSN )

  ZSNOTRS5(KIDIA:KFDIA,1:KLEVSN+1)=0._JPRB
! 2.1 Compute absorption of solar radiation into the snowpack
  CALL SRFSN_SSRABSSAD(KIDIA,KFDIA,KLON,KLEVSN,&
                 &  LLNOSNOW,PFRTI,&
                 &  PSSRFLTI5, PSNM1M5, PRSNM1M,&
                 & YDSOIL,YDCST,&
                 & ZSNOTRS5, &
!------- perturbations
                 & PSSRFLTI, PSNM1M, &
                 & ZSNOTRS  ) 

ELSE ! Single-layer
!* LIQUID WATER DIAGNOSTIC  (LESN09)
  CALL SRFSN_LWIMPSAD(KIDIA  ,KFDIA  ,KLON ,&
   & PTSPHY  ,LDREGSF  ,&
   & PSNM1M5(KIDIA:KFDIA,1) ,PTSNM1M5(KIDIA:KFDIA,1) ,PRSNM1M(KIDIA:KFDIA,1), &
   & PTSAM1M5 ,PHLICEM1M5,&
   & PSLRFL5 ,PSSRFLTI5,PFRTI    ,PAHFSTI5 ,PEVAPTI5,&
   & ZSSFC5  ,ZSSFL5   ,PEVAPSNW5,&
   & ZTSFC5  ,ZTSFL5   ,&
   & YDCST   ,YDVEG    ,YDSOIL   ,YDFLAKE  ,YDURB,&
   & ZTSN5(KIDIA:KFDIA,1)   ,ZGSN5    ,&
   & PSNM1M(KIDIA:KFDIA,1)  ,PTSNM1M(KIDIA:KFDIA,1)  ,PTSAM1M ,&
   & PSLRFL  ,PSSRFLTI ,PAHFSTI ,PEVAPTI,&
   & ZSSFC   ,ZSSFL    ,PEVAPSNW,&
   & ZTSFC   ,ZTSFL    ,&
   & ZTSN(KIDIA:KFDIA,1)    ,ZGSN )

ENDIF

!     ------------------------------------------------------------------
!*         0.1     INTERCEPTION RESERVOIR AND ASSOCIATED RUNOFF CHANGES.

CALL SRFWLSAD(KIDIA ,KFDIA ,KLON,&
 & PTSPHY ,PWLM1M5  ,PCVL  ,PCVH  ,PWLMX5,&
 & PFRTI  ,PEVAPTI5 ,ZRSFC5,ZRSFL5,&
 & LDLAND ,&
 & YDSOIL ,YDVEG    ,&
 & ZTSFC5 ,ZTSFL5   ,&
 & PWLM1M ,&
 & PEVAPTI,ZRSFC    ,ZRSFL ,&
 & ZTSFC  ,ZTSFL )

DO JL=KIDIA,KFDIA
! protect against negative precip
  IF (PSSFL5(JL) < 0.0_JPRB) THEN
    ZSSFL(JL) = 0.0_JPRB
  ELSE
    PSSFL(JL) = PSSFL(JL)+ZSSFL(JL)
    ZSSFL(JL) = 0.0_JPRB
  ENDIF
  IF (PSSFC5(JL) < 0.0_JPRB) THEN
    ZSSFC(JL) = 0.0_JPRB
  ELSE
    PSSFC(JL) = PSSFC(JL)+ZSSFC(JL)
    ZSSFC(JL) = 0.0_JPRB
  ENDIF
  IF (PRSFL5(JL) < 0.0_JPRB) THEN
    ZRSFL(JL) = 0.0_JPRB
  ELSE
    PRSFL(JL) = PRSFL(JL)+ZRSFL(JL)
    ZRSFL(JL) = 0.0_JPRB
  ENDIF
  IF (PRSFC5(JL) < 0.0_JPRB) THEN
    ZRSFC(JL) = 0.0_JPRB  
  ELSE
    PRSFC(JL) = PRSFC(JL)+ZRSFC(JL)
    ZRSFC(JL) = 0.0_JPRB 
  ENDIF
ENDDO

IF (LHOOK) CALL DR_HOOK('SURFTSTPSAD_CTL_MOD:SURFTSTPAD_CTL',1,ZHOOK_HANDLE)

END SUBROUTINE SURFTSTPSAD_CTL
END MODULE SURFTSTPSAD_CTL_MOD
