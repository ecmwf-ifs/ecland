SUBROUTINE SURFWS    (YDSURF,KIDIA,KFDIA,KLON, KLEVS, KLEVSN, KTILES, PSDOR, &
                    & PLSM,    PFRTI, PMU0,                          &
                    & PTSAM1M, PTSKIN,PALBSN,                         &
                    & PTSNM1M, PSNM1M,                                &
                    & PRSNM1M, PWSNM1M                                )

! (C) Copyright 2017- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     ------------------------------------------------------------------
!**   *SURFWS* - CREATES WARM START CONDITIONS FOR SURFACE VARIABLES

!     PURPOSE
!     -------
!     REPLACE COLD STARTED VARIABLES (for instance multi-layer var
!     initialised with single-layer values) WITH IDEALISED PROFILES.

!     INTERFACE
!     ---------
!     *SURFWS* IS CALLED BY *CALLPAR* 

!     INPUT PARAMETERS (INTEGER):
!     *KIDIA*        START POINT
!     *KFDIA*        END POINT
!     *KLON*         NUMBER OF GRID POINTS PER PACKET
!     *KLEVS*       NUMBER OF SOIL LAYERS
!     *KLEVSN*       NUMBER OF SNOW LAYERS

!     INPUT PARAMETERS (REAL):
!     *PLSM*         LAND-SEA MASK                                  (0-1)
!     *PSNM1M*       SNOW MASS (per unit area)                      kg/m**2
!     *PRSNM1M*      SNOW DENSITY                                   kg/m**3

!     OUTPUT PARAMETERS (REAL):
!     *PTSN*         SNOW TEMPERATURE WARM STARTED
!     *PSNS*         SEA-ICE INDICATOR
!     *PRSN*         LAKE INDICATOR
!     *PWSN*         NORTHERN HEMISPHERE INDICATOR

!     METHOD
!     ------
!     IT IS NOT ROCKET SCIENCE, BUT CHECK DOCUMENTATION

!     Modifications
!     G. Arduini        1 Sept 2017       Created
!     ------------------------------------------------------------------

USE PARKIND1, ONLY : JPIM, JPRB
USE, INTRINSIC :: ISO_C_BINDING

!ifndef INTERFACE

USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_SURF, ONLY : TSURF, GET_SURF

USE ABORT_SURF_MOD
USE SURFWS_CTL_MOD

!endif INTERFACE

IMPLICIT NONE

! Declaration of arguments

TYPE(C_PTR)       ,INTENT(IN)    :: YDSURF
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVS
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVSN
INTEGER(KIND=JPIM),INTENT(IN)    :: KTILES
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSM(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PMU0(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSDOR(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFRTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSKIN(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PALBSN(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSAM1M(:,:)

REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSNM1M(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSNM1M(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PRSNM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PWSNM1M(:,:) 

!ifndef INTERFACE

TYPE(TSURF), POINTER :: YSURF
REAL(KIND=JPRB) :: ZTHRESH, ZEPS, ZSUM(KLON), ZSUM2(KLON)
INTEGER(KIND=JPIM) :: IK, JK
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE



IF (LHOOK) CALL DR_HOOK('SURFWS',0,ZHOOK_HANDLE)

YSURF => GET_SURF(YDSURF)

IF(UBOUND(PLSM,1) < KLON) THEN
  CALL ABORT_SURF('SURFWS: PLSM TOO SHORT!')
ENDIF

IF(UBOUND(PFRTI,1) < KLON) THEN
  CALL ABORT_SURF('SURFWS: PFRTI TOO SHORT!')
ENDIF

IF(UBOUND(PFRTI,2) < KTILES) THEN
  CALL ABORT_SURF('SURFWS: SECOND DIMENSION OF PFRTI TOO SHORT!')
ENDIF

IF(UBOUND(PMU0,1) < KLON) THEN
  CALL ABORT_SURF('SURFWS: PFRSO TOO SHORT!')
ENDIF
IF(UBOUND(PALBSN,1) < KLON) THEN
  CALL ABORT_SURF('SURFWS: PALBSN TOO SHORT!')
ENDIF
IF(UBOUND(PTSKIN,1) < KLON) THEN
  CALL ABORT_SURF('SURFWS: PTSKIN TOO SHORT!')
ENDIF
IF(UBOUND(PTSKIN,1) < KLON) THEN
  CALL ABORT_SURF('SURFWS: PTSKIN TOO SHORT!')
ENDIF

IF(UBOUND(PTSAM1M,1) < KLON) THEN
  CALL ABORT_SURF('SURFWS: PTSAM1M TOO SHORT!')
ENDIF

IF(UBOUND(PTSAM1M,2) < KLEVS) THEN
  CALL ABORT_SURF('SURFWS:PTSAM1M  TOO SHORT LEVEL!')
ENDIF

IF(UBOUND(PTSNM1M,1) < KLON) THEN
  CALL ABORT_SURF('SURFWS: PTSNM1M TOO SHORT!')
ENDIF

IF(UBOUND(PTSNM1M,2) < KLEVSN) THEN
  CALL ABORT_SURF('SURFWS: PTSNM1M TOO SHORT LEVEL!')
ENDIF

IF(UBOUND(PSNM1M,1) < KLON) THEN
  CALL ABORT_SURF('SURFWS: PSNM1M TOO SHORT!')
ENDIF

IF(UBOUND(PSNM1M,2) < KLEVSN) THEN
  CALL ABORT_SURF('SURFWS: PSNM1M TOO SHORT LEVEL!')
ENDIF

IF(UBOUND(PRSNM1M,1) < KLON) THEN
  CALL ABORT_SURF('SURFWS: PRSNM1M TOO SHORT!')
ENDIF

IF(UBOUND(PRSNM1M,2) < KLEVSN) THEN
  CALL ABORT_SURF('SURFWS: PRSNM1M TOO SHORT LEVEL!')
ENDIF

IF(UBOUND(PWSNM1M,1) < KLON) THEN
  CALL ABORT_SURF('SURFWS: PWSNM1M TOO SHORT!')
ENDIF

IF(UBOUND(PWSNM1M,2) < KLEVSN) THEN
  CALL ABORT_SURF('SURFWS: PWSNM1M TOO SHORT LEVEL!')
ENDIF


CALL SURFWS_CTL   (KIDIA, KFDIA, KLON, KLEVSN, PSDOR, &
 & PLSM, PFRTI, PMU0,                         &
 & PTSAM1M, PTSKIN, PALBSN,                    &
 & PTSNM1M, PSNM1M, PRSNM1M, PWSNM1M,          &  
 & YSURF%YCST, YSURF%YSOIL                     )  


IF (LHOOK) CALL DR_HOOK('SURFWS',1,ZHOOK_HANDLE)

!endif INTERFACE

!     ------------------------------------------------------------------

END SUBROUTINE SURFWS
