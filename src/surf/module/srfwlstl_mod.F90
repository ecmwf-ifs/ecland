MODULE SRFWLSTL_MOD
CONTAINS
SUBROUTINE SRFWLSTL(KIDIA ,KFDIA ,KLON   ,&
 & PTMST  ,PWLM1M5 ,PCVL  ,PCVH  ,PWLMX5 ,&
 & PFRTI  ,PEVAPTI5,PRSFC5,PRSFL5,&
 & LDLAND ,&
 & YDSOIL ,YDVEG   ,&
 & PTSFC5 ,PTSFL5  ,&
 & PWLM1M ,&
 & PEVAPTI,PRSFC   ,PRSFL ,&
 & PTSFC  ,PTSFL )

USE PARKIND1  , ONLY : JPIM, JPRB
USE YOMHOOK   , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_THF   , ONLY : RHOH2O
USE YOS_SOIL  , ONLY : TSOIL
USE YOS_VEG   , ONLY : TVEG

#ifdef DOC

! (C) Copyright 2012- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SRFWLSTL* - COMPUTES CHANGES IN THE SKIN RESERVOIR.
!                  (Tangent linear)

!     PURPOSE.
!     --------
!          THIS ROUTINE COMPUTES THE CHANGES IN THE SKIN RESERVOIR AND
!     THE RUN-OFF BEFORE THE SNOW MELTS.

!**   INTERFACE.
!     ----------
!          *SRFWLSTL* IS CALLED FROM *SURFTSTPSTL*.

!     PARAMETER   DESCRIPTION                                         UNITS
!     ---------   -----------                                         -----

!     INPUT PARAMETERS (INTEGER):
!     *KIDIA*      START POINT
!     *KFDIA*      END POINT
!     *KLON*       NUMBER OF GRID POINTS PER PACKET

!     INPUT PARAMETERS (REAL):
!     *PTMST*      TIME STEP                                           S

!     INPUT PARAMETERS (LOGICAL):
!     *LDLAND*     LAND/SEA MASK (TRUE/FALSE)

!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!     *PFRTI*      TILE FRACTIONS                                      (0-1)
!            1 : WATER                  5 : SNOW ON LOW-VEG+BARE-SOIL
!            2 : ICE                    6 : DRY SNOW-FREE HIGH-VEG
!            3 : WET SKIN               7 : SNOW UNDER HIGH-VEG
!            4 : DRY SNOW-FREE LOW-VEG  8 : BARE SOIL

!  Trajectory  Perturbation  Description                                Unit
!  PWLM1M5     PWLM1M        SKIN RESERVOIR WATER CONTENT               kg/m2
!  PCVL        ---           LOW VEGETATION COVER  (CORRECTED)          (0-1)
!  PCVH        ---           HIGH VEGETATION COVER (CORRECTED)          (0-1)
!  PWLMX5      ---           MAXIMUM SKIN RESERVOIR CAPACITY            kg/m2
!  PEVAPTI5    PEVAPTI       SURFACE MOISTURE FLUX, FOR EACH TILE       kg/m2/s
!  PRSFC5      PRSFC         CONVECTIVE RAIN FLUX AT THE SURFAC         kg/m2/s
!  PRSFL5      PRSFL         LARGE SCALE RAIN FLUX AT THE SURFAC        kg/m2/s

!     OUTPUT PARAMETERS AT T+1 (UNFILTERED,REAL):
!  Trajectory  Perturbation  Description                                Unit
!  PTSFC5      PTSFC         CONVECTIVE THROUGHFALL AT THE SURFACE      kg/m2/s
!  PTSFL5      PTSFL         LARGE SCALE THROUGHFALL AT THE SURFACE     kg/m2/s
!                            (NB: THROUGHFALL=RAINFALL-INTERCEPTION)


!     METHOD.
!     -------
!          STRAIGHTFORWARD ONCE THE DEFINITION OF THE CONSTANTS IS
!     UNDERSTOOD. FOR THIS REFER TO DOCUMENTATION.

!     EXTERNALS.
!     ----------
!          NONE.

!     REFERENCE.
!     ----------
!          SEE SOIL PROCESSES' PART OF THE MODEL'S DOCUMENTATION FOR
!     DETAILS ABOUT THE MATHEMATICS OF THIS ROUTINE.

!     Original   
!     --------
!     M. Janiskova              E.C.M.W.F.     03-02-2012

!     Modifications
!     -------------

!     ------------------------------------------------------------------
#endif

IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM), INTENT(IN)   :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KLON

LOGICAL,            INTENT(IN)   :: LDLAND(:)

TYPE(TSOIL),        INTENT(IN)   :: YDSOIL
TYPE(TVEG),         INTENT(IN)   :: YDVEG

REAL(KIND=JPRB),    INTENT(IN)   :: PTMST
REAL(KIND=JPRB),    INTENT(IN)   :: PWLM1M5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PCVL(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PCVH(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PWLMX5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PFRTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PRSFC5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PRSFL5(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PTSFC5(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PTSFL5(:)

REAL(KIND=JPRB),    INTENT(IN)   :: PWLM1M(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PRSFC(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PRSFL(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PTSFC(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PTSFL(:)

! Declaration of local variables

INTEGER(KIND=JPIM) :: JL

REAL(KIND=JPRB) :: ZPWL5(KLON)
REAL(KIND=JPRB) :: ZPWL(KLON)

REAL(KIND=JPRB) :: ZWL5, ZQHFLW5, ZTPRCP5, ZIPRCP5, ZMPRCP5
REAL(KIND=JPRB) :: ZZX5, ZZY5

REAL(KIND=JPRB) :: ZEPPRCP,&
 & ZEPTINY, ZIPRCP, ZMPRCP, ZPSFR, ZQHFLW, ZTMST, &
 & ZTPRCP, ZVINTER, ZWL


REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SRFWLSTL_MOD:SRFWLSTL',0,ZHOOK_HANDLE)
ASSOCIATE(RPSFR=>YDSOIL%RPSFR, &
 & RVINTER=>YDVEG%RVINTER)

!*         1.    SET UP SOME CONSTANTS.
!                --- -- ---- ----------
!*               PHYSICAL CONSTANTS.

ZVINTER=RVINTER
ZPSFR=1.0_JPRB/RPSFR

!*    SECURITY PARAMETERS
ZEPTINY=10._JPRB*TINY(RHOH2O)
ZEPPRCP=ZEPTINY

!*    COMPUTATIONAL CONSTANTS.
ZTMST=1.0_JPRB/PTMST

!     ------------------------------------------------------------------

!     ------------------------------------------------------------------
!*          3.  UPWARDS EVAPORATION.
!               ------- ------------
DO JL=KIDIA,KFDIA
  IF (LDLAND(JL)) THEN

!           INITIALISE PWL (TO MAKE THE CODE SIMPLER).
    ZPWL (JL) = PWLM1M (JL)
    ZPWL5(JL) = PWLM1M5(JL)

!           EVAPORATION OF THE SKIN RESERVOIR (EL < 0).
    IF (PEVAPTI5(JL,3) < 0.0_JPRB) THEN
      ZWL  = PWLM1M(JL)
      ZWL5 = PWLM1M5(JL)
      ZQHFLW  = PTMST*PFRTI(JL,3)*PEVAPTI (JL,3)
      ZQHFLW5 = PTMST*PFRTI(JL,3)*PEVAPTI5(JL,3)
      ZPWL (JL) = ZWL +ZQHFLW
      ZPWL5(JL) = ZWL5+ZQHFLW5
      IF (ZPWL5(JL) < 0.0_JPRB) THEN
        ZPWL (JL) = 0.0_JPRB
        ZPWL5(JL) = 0.0_JPRB
      ENDIF
    ENDIF
  ELSE
!    Sea points
    ZPWL (JL) = 0.0_JPRB
    ZPWL5(JL) = 0.0_JPRB
  ENDIF
ENDDO

!           6.  INTERCEPTION OF PRECIPITATION BY THE VEGETATION.
!               ------------ -- ------------- -- --- -----------
DO JL=KIDIA,KFDIA
  IF (LDLAND(JL)) THEN
!          LARGE SCALE PRECIPITATION.
    IF (PRSFL5(JL) > ZEPPRCP) THEN
      ZTPRCP  = PRSFL (JL)      
      ZTPRCP5 = PRSFL5(JL)
      ZIPRCP  = ZTPRCP *(PCVL(JL)+PCVH(JL))*ZVINTER
      ZIPRCP5 = ZTPRCP5*(PCVL(JL)+PCVH(JL))*ZVINTER
      ZZX5 = PWLMX5(JL)-ZEPTINY-ZPWL5(JL)
      ZZY5 = PTMST*ZIPRCP5
      IF (ZZX5 < ZZY5) THEN
        ZMPRCP  = -ZPWL(JL)
        ZMPRCP5 = ZZX5
      ELSE
        ZMPRCP  = PTMST*ZIPRCP
        ZMPRCP5 = ZZY5
      ENDIF
      PTSFL (JL) = PRSFL (JL)-ZMPRCP *ZTMST
      PTSFL5(JL) = PRSFL5(JL)-ZMPRCP5*ZTMST
    ELSE
      PTSFL (JL) = 0.0_JPRB
      PTSFL5(JL) = 0.0_JPRB
    ENDIF

!          CONVECTIVE PRECIPITATION.
    IF (PRSFC5(JL) > ZEPPRCP) THEN
      ZTPRCP  = PRSFC (JL)*RPSFR
      ZTPRCP5 = PRSFC5(JL)*RPSFR
      ZIPRCP  = ZTPRCP *(PCVL(JL)+PCVH(JL))*ZVINTER
      ZIPRCP5 = ZTPRCP5*(PCVL(JL)+PCVH(JL))*ZVINTER
      ZZX5 = PWLMX5(JL)-ZEPTINY-ZPWL5(JL)
      ZZY5 = PTMST*ZIPRCP5
      IF (ZZX5 < ZZY5) THEN
        ZMPRCP  = -ZPWL(JL)*ZPSFR
        ZMPRCP5 = ZZX5*ZPSFR
      ELSE
        ZMPRCP  = PTMST*ZIPRCP*ZPSFR
        ZMPRCP5 = ZZY5*ZPSFR
      ENDIF
      PTSFC (JL) = PRSFC (JL)-ZMPRCP *ZTMST
      PTSFC5(JL) = PRSFC5(JL)-ZMPRCP5*ZTMST
    ELSE
      PTSFC (JL) = 0.0_JPRB
      PTSFC5(JL) = 0.0_JPRB
    ENDIF

!          SEA POINTS.
  ELSE
    PTSFC (JL) = PRSFC (JL)
    PTSFC5(JL) = PRSFC5(JL)
    PTSFL (JL) = PRSFL (JL)
    PTSFL5(JL) = PRSFL5(JL)
  ENDIF
ENDDO

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SRFWLSTL_MOD:SRFWLSTL',1,ZHOOK_HANDLE)

END SUBROUTINE SRFWLSTL
END MODULE SRFWLSTL_MOD




