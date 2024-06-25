MODULE SRFRCGSAD_MOD
CONTAINS
SUBROUTINE SRFRCGSAD(KIDIA   , KFDIA  , KLON , KLEVS ,&
 & LDLAND  , LDSICE , LDREGSF,&
 & PTSAM1M5, KSOTY  , PCVL   , PCVH   ,&
 & YDCST   , YDSOIL , &
 & PCTSA5  , PTSAM1M, PCTSA  )  
 
USE PARKIND1  , ONLY : JPIM, JPRB
USE YOMHOOK   , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_THF   , ONLY : RHOH2O
USE YOS_CST   , ONLY : TCST
USE YOS_SOIL  , ONLY : TSOIL

#ifdef DOC
! (C) Copyright 2012- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SRFRCGSAD* - COMPUTES SOIL VOLUMETRIC HEAT CAPACITY.
!                   (Adjoint)

!     PURPOSE.
!     --------
!          THIS ROUTINE COMPUTES THE APARENT VOLUMETRIC HEAT CAPACITY
!          IN THE SOIL, TAKING INTO ACCOUNT SNOW. APPARENT STANDS FOR
!          THE FACT THAT THE EFFECTS OF FREEZING AND MELTING OF WATER
!          IN THE SOIL ARE TAKEN INTO ACCOUNT.

!**   INTERFACE.
!     ----------
!          *SRFRCGSAD* IS CALLED FROM *SURFTSTPSAD*.

!     PARAMETER   DESCRIPTION                                          UNITS
!     ---------   -----------                                          -----
!     INPUT PARAMETERS (INTEGER):
!     *KIDIA*      START POINT
!     *KFDIA*      END POINT
!     *KLON*       NUMBER OF GRID POINTS PER PACKET
!     *KLEVS*      NUMBER OF SOIL LAYERS

!     INPUT PARAMETERS (LOGICAL):
!     *LDLAND*     LAND/SEA MASK (TRUE/FALSE)
!     *LDSICE*     SEA ICE MASK (.T. OVER SEA ICE)
!     *LDREGSF*    TRUE WHEN REGULARIZATION USED

!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!  Trajectory  Perturbation  Description                                Unit
!  PTSAM1M5    PTSAM1M       SOIL TEMPERATURE                           K
!  PCVL        ---           LOW VEGETATION COVER  (CORRECTED)          (0-1)
!  PCVH        ---           HIGH VEGETATION COVER (CORRECTED)          (0-1)

!     OUTPUT PARAMETERS:
!  Trajectory  Perturbation  Description                                Unit
!  PCTSA5      PCTSA         VOLUMETRIC HEAT CAPACITY                   J/K/m3

!     METHOD.
!     -------
!          STRAIGHTFORWARD ONCE THE DEFINITION OF THE CONSTANTS IS
!     UNDERSTOOD. FOR THIS REFER TO DOCUMENTATION.

!     EXTERNALS.
!     ----------
!          NONE.

!     REFERENCE.
!     ----------

!     Original   
!     --------
!     M. Janiskova              E.C.M.W.F.     30-03-2012  

!     Modifications
!     -------------

!     ------------------------------------------------------------------
#endif

IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM), INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)    :: KLON
INTEGER(KIND=JPIM), INTENT(IN)    :: KLEVS
INTEGER(KIND=JPIM), INTENT(IN)    :: KSOTY(:)

LOGICAL,            INTENT(IN)    :: LDLAND(:)
LOGICAL,            INTENT(IN)    :: LDSICE(:)
LOGICAL,            INTENT(IN)    :: LDREGSF

REAL(KIND=JPRB),    INTENT(IN)    :: PTSAM1M5(:,:)
REAL(KIND=JPRB),    INTENT(IN)    :: PCVL(:)
REAL(KIND=JPRB),    INTENT(IN)    :: PCVH(:)

TYPE(TCST),         INTENT(IN)    :: YDCST
TYPE(TSOIL),        INTENT(IN)    :: YDSOIL

REAL(KIND=JPRB),    INTENT(OUT)   :: PCTSA5(:,:)

REAL(KIND=JPRB),    INTENT(INOUT) :: PTSAM1M(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PCTSA(:,:)

INTEGER(KIND=JPIM) :: JK, JL, JS

REAL(KIND=JPRB) :: ZDFDT5(KLON,KLEVS)

REAL(KIND=JPRB) :: ZWA(KLON)

REAL(KIND=JPRB) :: ZD1, ZD2, ZD3, ZD4, ZGICE, ZRCSICE, ZSNOWI
REAL(KIND=JPRB) :: ZRCSOIL, ZWCAP
REAL(KIND=JPRB) :: ZDFDT(KLON,KLEVS)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SRFRCGSAD_MOD:SRFRCGSAD',0,ZHOOK_HANDLE)
ASSOCIATE(RLMLT=>YDCST%RLMLT, &
 & LEVGEN=>YDSOIL%LEVGEN, RDAT=>YDSOIL%RDAT, RGH2O=>YDSOIL%RGH2O, &
 & RRCSICE=>YDSOIL%RRCSICE, RRCSOIL=>YDSOIL%RRCSOIL, RRCSOILM=>YDSOIL%RRCSOILM, &
 & RTF1=>YDSOIL%RTF1, RTF2=>YDSOIL%RTF2, RTF3=>YDSOIL%RTF3, RTF4=>YDSOIL%RTF4, &
 & RWCAP=>YDSOIL%RWCAP, RWCAPM=>YDSOIL%RWCAPM)

!*         1.    SET UP SOME CONSTANTS.
!                --- -- ---- ----------
!*               PHYSICAL CONSTANTS.
!                -------- ----------

ZD1=RDAT(1)
ZD2=RDAT(2)
ZD3=RDAT(3)
ZD4=RDAT(4)
ZGICE=0.5_JPRB*RGH2O
ZSNOWI=1.0_JPRB/ZD1
ZRCSICE=RRCSICE

!     ------------------------------------------------------------------
!*         2. CONTRIBUTION TO APPARENT HEAT CAPACITY.
!             ---------------------------------------
!          CONTRIBUTION TO APPARENT HEAT CAPACITY, TAKING INTO ACCOUNT
!          FREEZING/MELTING OF SOIL WATER.

DO JK=1,KLEVS
  DO JL=KIDIA,KFDIA
    IF (LDLAND(JL)) THEN

!     NOTE: ZDFDT STANDS FOR D/DT OF THE FUNCTION F(T) IN THE
!           ROUTINE SRFENE

      IF(PTSAM1M5(JL,JK) < RTF1.AND.PTSAM1M5(JL,JK) > RTF2) THEN
        ZDFDT5(JL,JK) = -0.5_JPRB*RTF4*COS(RTF4*(PTSAM1M5(JL,JK)-RTF3))
      ELSE
        ZDFDT5(JL,JK) = 0.0_JPRB
      ENDIF
    ENDIF
  ENDDO
ENDDO

!     ------------------------------------------------------------------
!*         3. COMPUTE HEAT CAPACITIES.
!             ------------------------

DO JL=KIDIA,KFDIA

  IF (LDLAND(JL)) THEN

!          SOIL THERMAL COEFFICIENTS MODIFIED WHEN SNOW COVERS
!          THE GROUND AND IS PARTIALLY MASKED BY THE VEGETATION.
    IF(LEVGEN)THEN
      JS=KSOTY(JL)
      ZWCAP=RWCAPM(JS)
      ZRCSOIL=RRCSOILM(JS)
    ELSE
      ZWCAP=RWCAP
      ZRCSOIL=RRCSOIL
    ENDIF
    ZWA(JL)=(PCVL(JL)+PCVH(JL))*ZWCAP
    PCTSA5(JL,1) = ZRCSOIL-RLMLT*RHOH2O*ZWA(JL)*ZDFDT5(JL,1)
    PCTSA5(JL,2) = ZRCSOIL-RLMLT*RHOH2O*ZWA(JL)*ZDFDT5(JL,2)
    PCTSA5(JL,3) = ZRCSOIL-RLMLT*RHOH2O*ZWA(JL)*ZDFDT5(JL,3)
    PCTSA5(JL,4) = ZRCSOIL-RLMLT*RHOH2O*ZWA(JL)*ZDFDT5(JL,4)

!          SEA ICE POINTS

  ELSEIF (LDSICE(JL)) THEN
    PCTSA5(JL,1) = ZRCSICE
    PCTSA5(JL,2) = 0.0_JPRB
    PCTSA5(JL,3) = 0.0_JPRB
    PCTSA5(JL,4) = 0.0_JPRB

!          SEA POINTS

  ELSE
    PCTSA5(JL,1) = 0.0_JPRB
    PCTSA5(JL,2) = 0.0_JPRB
    PCTSA5(JL,3) = 0.0_JPRB
    PCTSA5(JL,4) = 0.0_JPRB
  ENDIF
ENDDO

!          0.  ADJOINT CALCULATIONS
!              --------------------

!* Set local variables to zero

ZDFDT(:,:) = 0.0_JPRB

!*         0.3. COMPUTE HEAT CAPACITIES.
!               ------------------------

DO JL=KIDIA,KFDIA

  IF (LDLAND(JL)) THEN
    ZDFDT(JL,4) = ZDFDT(JL,4)-RLMLT*RHOH2O*ZWA(JL)*PCTSA(JL,4)
    ZDFDT(JL,3) = ZDFDT(JL,3)-RLMLT*RHOH2O*ZWA(JL)*PCTSA(JL,3)
    ZDFDT(JL,2) = ZDFDT(JL,2)-RLMLT*RHOH2O*ZWA(JL)*PCTSA(JL,2)
    ZDFDT(JL,1) = ZDFDT(JL,1)-RLMLT*RHOH2O*ZWA(JL)*PCTSA(JL,1)
  ENDIF
  PCTSA(JL,4) = 0.0_JPRB
  PCTSA(JL,3) = 0.0_JPRB
  PCTSA(JL,2) = 0.0_JPRB
  PCTSA(JL,1) = 0.0_JPRB
ENDDO

!*         0.2. CONTRIBUTION TO APPARENT HEAT CAPACITY.
!               ---------------------------------------
!          CONTRIBUTION TO APPARENT HEAT CAPACITY, TAKING INTO ACCOUNT
!          FREEZING/MELTING OF SOIL WATER.

DO JK=KLEVS,1,-1
  DO JL=KIDIA,KFDIA
    IF (LDLAND(JL)) THEN

      IF (PTSAM1M5(JL,JK) < RTF1.AND.PTSAM1M5(JL,JK) > RTF2) THEN
        IF (LDREGSF) THEN
          ZDFDT (JL,JK) = ZDFDT (JL,JK)/100.0_JPRB
        ENDIF
        PTSAM1M(JL,JK) = PTSAM1M(JL,JK)+0.5_JPRB*RTF4*RTF4 &
         & *SIN(RTF4*(PTSAM1M5(JL,JK)-RTF3))*ZDFDT(JL,JK)
      ENDIF
      ZDFDT(JL,JK) = 0.0_JPRB
    ENDIF
  ENDDO
ENDDO

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SRFRCGSAD_MOD:SRFRCGSAD',1,ZHOOK_HANDLE)

END SUBROUTINE SRFRCGSAD
END MODULE SRFRCGSAD_MOD
