INTERFACE
SUBROUTINE SURFTSTPSAD(YDSURF , KIDIA , KFDIA , KLON  , KLEVS ,KLEVSN,&
 & KTILES, KSOTY,&
 & PSSDP2, PSSDP3,&
 & PTSPHY , PFRTI,&
!-trajectory
 & LDLAND  , LDSICE  , LDSI     , LDNH   , LDREGSF,&
 & PAHFSTI5, PEVAPTI5, PSSRFLTI5,&
 & PSNM1M5 , PTSNM1M5, PRSNM1M  ,&
 & PTSAM1M5, PTIAM1M5,&
 & PWLM1M5 , PWSAM1M5,&
 & PHLICEM1M5,PAPRS5 ,&
 & PRSFC5  ,PRSFL5   ,&
 & PSLRFL5 ,PSSFC5   ,PSSFL5   ,&
 & PCVL    ,PCVH     , PCUR     ,PWLMX5   ,PEVAPSNW5, &
 & LNEMOICETHK, PTHKICE5, &
!-TENDENCIES OUTPUT
 & PTSNE15 , PTSAE15 ,PTIAE15  ,&
!-perturbations
 & PAHFSTI , PEVAPTI ,PSSRFLTI ,&
 & PSNM1M  , PTSNM1M ,&
 & PTSAM1M , PTIAM1M ,&
 & PWLM1M  , PWSAM1M ,&
 & PAPRS   ,          &
 & PRSFC   , PRSFL   ,&
 & PSLRFL  , PSSFC   , PSSFL    ,&
 & PEVAPSNW,&
!-TENDENCIES OUTPUT
 & PTSNE1  , PTSAE1  , PTIAE1)

USE PARKIND1, ONLY : JPIM, JPRB
USE, INTRINSIC :: ISO_C_BINDING

! (C) Copyright 2012- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     ------------------------------------------------------------------

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
!  PCUR        ---           URBAN COVER (PASSIVE)                      (0-1)
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

!     ------------------------------------------------------------------

!     Original   
!     --------
!     M. Janiskova              E.C.M.W.F.     04-05-2012  

!     Modifications
!     -------------

!     ------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments

TYPE(C_PTR)       ,INTENT(IN)    :: YDSURF
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSDP2(:,:)                                                          
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSDP3(:,:,:)   
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
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCUR(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWLMX5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PEVAPSNW5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PHLICEM1M5(:)
LOGICAL           ,INTENT(IN)    :: LNEMOICETHK
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTHKICE5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRS5(:)
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

!     ------------------------------------------------------------------

END SUBROUTINE SURFTSTPSAD
END INTERFACE
