INTERFACE
SUBROUTINE SURFTSTPS    (YDSURF , KIDIA , KFDIA , KLON  , KLEVS , KLEVSN,& 
 & KTILES, KSOTY,&
 & PTSPHY , PSDOR, PFRTI,&
 & PAHFSTI, PEVAPTI, PSSRFLTI,&
 & LDLAND,  LDSICE, LDSI, LDNH,&
 & PSNM1M  ,PTSNM1M,PRSNM1M,&
 & PTSAM1M, PTIAM1M,&
 & PWLM1M  ,PWSAM1M,&
 & PHLICEM1M,PAPRS, &
 & PRSFC   ,PRSFL,&
 & PSLRFL  ,PSSFC  ,PSSFL,&
 & PCVL    ,PCVH   ,PCUR   ,PWLMX   ,PEVAPSNW,&
!-TENDENCIES OUTPUT
 & PTSNE1 , PTSAE1 , PTIAE1)

USE PARKIND1, ONLY : JPIM, JPRB
USE, INTRINSIC :: ISO_C_BINDING
! (C) Copyright 2011- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     ------------------------------------------------------------------

!**** *SURFTSTPS* - UPDATES LAND VALUES OF TEMPERATURE AND SNOW.

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
!          *SURFTSTPS* IS CALLED FROM *CALLPAR*.
!          THE ROUTINE TAKES ITS INPUT FROM THE LONG-TERM STORAGE:
!     TSA,WL,SN AT T-1,TSK,SURFACE FLUXES COMPUTED IN OTHER PARTS OF
!     THE PHYSICS, AND W AND LAND-SEA MASK. IT RETURNS AS AN OUTPUT
!     TENDENCIES TO THE SAME VARIABLES (TSA,WL,SN).

!     PARAMETER   DESCRIPTION                                    UNITS
!     ---------   -----------                                    -----
!     INPUT PARAMETERS (INTEGER):
!    *KIDIA*      START POINT
!    *KFDIA*      END POINT
!    *KLEV*       NUMBER OF LEVELS
!    *KLON*       NUMBER OF GRID POINTS PER PACKET
!    *KLEVS*      NUMBER OF SOIL LAYERS
!    *KTILES*     NUMBER OF TILES (I.E. SUBGRID AREAS WITH DIFFERENT
!                 SURFACE BOUNDARY CONDITION)
!    *KSOTY*      SOIL TYPE                                   (1-7)

!     INPUT PARAMETERS (REAL):
!    *PTSPHY*     TIME STEP FOR THE PHYSICS                      S
!    *PFRTI*      TILE FRACTIONS                              (0-1)
!            1 : WATER                  5 : SNOW ON LOW-VEG+BARE-SOIL
!            2 : ICE                    6 : DRY SNOW-FREE HIGH-VEG
!            3 : WET SKIN               7 : SNOW UNDER HIGH-VEG
!            4 : DRY SNOW-FREE LOW-VEG  8 : BARE SOIL
!    *PAHFSTI*      SURFACE SENSIBLE HEAT FLUX, FOR EACH TILE  W/M2
!    *PEVAPTI*      SURFACE MOISTURE FLUX, FOR EACH TILE      KG/M2/S
!    *PSSRFLTI*     NET SHORTWAVE RADIATION FLUX AT SURFACE, FOR
!                       EACH TILE                                 W/M2

!     INPUT PARAMETERS (LOGICAL):
!    *LDLAND*     LAND/SEA MASK (TRUE/FALSE)
!    *LDSICE*     SEA ICE MASK (.T. OVER SEA ICE)
!    *LDSI*       TRUE IF THERMALLY RESPONSIVE SEA-ICE
!    *LDNH*       TRUE FOR NORTHERN HEMISPHERE LATITUDE ROW

!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!    *PSNM1M*     SNOW MASS (per unit area)                      KG/M**2
!    *PRSNM1M*    SNOW DENSITY                                   KG/M3
!    *PTSNM1M*    SNOW TEMPERATURE                               K
!    *PTSAM1M*    SOIL TEMPERATURE                               K
!    *PTIAM1M*    SEA-ICE TEMPERATURE                            K
!          (NB: REPRESENTS THE FRACTION OF ICE ONLY, NOT THE GRID-BOX)
!    *PWLM1M*     SKIN RESERVOIR WATER CONTENT                   kg/m**2
!    *PWSAM1M*    SOIL MOISTURE                                m**3/m**3
!    *PHLICEM1M*  LAKE ICE THICKNESS                             m
!    *PRSFC*      CONVECTIVE RAIN FLUX AT THE SURFACE          KG/M**2/S
!    *PRSFL*      LARGE SCALE RAIN FLUX AT THE SURFACE         KG/M**2/S
!    *PSLRFL*     NET LONGWAVE  RADIATION AT THE SURFACE         W/M**2
!    *PSSFC*      CONVECTIVE  SNOW FLUX AT THE SURFACE         KG/M**2/S
!    *PSSFL*      LARGE SCALE SNOW FLUX AT THE SURFACE         KG/M**2/S
!    *PCVL*       LOW VEGETATION COVER  (CORRECTED)              (0-1)
!    *PCVH*       HIGH VEGETATION COVER (CORRECTED)              (0-1)
!    *PCUR*       URBAN COVER (PASSIVE)                          (0-1)
!    *PWLMX*      MAXIMUM SKIN RESERVOIR CAPACITY                kg/m**2
!    *PEVAPSNW*   EVAPORATION FROM SNOW UNDER FOREST           KG/M**2/S


!     OUTPUT PARAMETERS (TENDENCIES):
!    *PTSNE1*     SNOW TEMPERATURE                              K/S
!    *PTSAE1*     SOIL TEMPERATURE TENDENCY                     K/S
!    *PTIAE1*     SEA-ICE TEMPERATURE TENDENCY                  K/S

!     METHOD.
!     -------
!          STRAIGHTFORWARD ONCE THE DEFINITION OF THE CONSTANTS IS
!     UNDERSTOOD. FOR THIS REFER TO DOCUMENTATION. FOR THE TIME FILTER
!     SEE CORRESPONDING PART OF THE DOCUMENTATION OF THE ADIABATIC CODE.

!     EXTERNALS.
!     ----------
!          *SRFWLS*        COMPUTES THE SKIN RESERVOIR CHANGES.
!          *SRFSN_LWIMPS*  REVISED SNOW SCHEME W. DIAG. LIQ. WATER.
!          *SRFRCGS*       COMPUTE SOIL HEAT CAPACITY.
!          *SRFTS*         COMPUTES THE TEMPERATURE CHANGES BEFORE THE SNOW
!                            MELTING.
!          *SRFIS*         COMPUTES TEMPERATURE EVOLUTION OF SEA ICE.

!     REFERENCE.
!     ----------
!          SEE SOIL PROCESSES' PART OF THE MODEL'S DOCUMENTATION FOR
!     DETAILS ABOUT THE MATHEMATICS OF THIS ROUTINE.

!     ------------------------------------------------------------------

!     Original   
!     --------
!          Simplified version based on SURFTSTP:
!     M. Janiskova              E.C.M.W.F.     27-07-2011  

!     Modifications
!     -------------

!     ------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments

TYPE(C_PTR)       ,INTENT(IN)    :: YDSURF
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVS
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVSN
INTEGER(KIND=JPIM),INTENT(IN)    :: KTILES
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAHFSTI(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PEVAPTI(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSRFLTI(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSPHY
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSDOR(:) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KSOTY(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFRTI(:,:)
LOGICAL           ,INTENT(IN)    :: LDLAND(:)
LOGICAL           ,INTENT(IN)    :: LDSICE(:)
LOGICAL           ,INTENT(IN)    :: LDSI
LOGICAL           ,INTENT(IN)    :: LDNH(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSNM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSNM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRSNM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSAM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTIAM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWLM1M(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWSAM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRSFC(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRSFL(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSLRFL(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSFC(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSFL(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVL(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVH(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCUR(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWLMX(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PEVAPSNW(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PHLICEM1M(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRS(:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSNE1(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSAE1(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTIAE1(:,:)

!     ------------------------------------------------------------------

END SUBROUTINE SURFTSTPS
END INTERFACE