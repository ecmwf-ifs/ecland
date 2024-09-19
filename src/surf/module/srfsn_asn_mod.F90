MODULE SRFSN_ASN_MOD
CONTAINS
SUBROUTINE SRFSN_ASN(KIDIA,KFDIA,KLON,PTMST,LLNOSNOW,PASNM1M,&
  & PCIL,LDNH,&
  & PMSN,PTSNM1M,PSNOWF,YDSOIL,YDCST,PASN)


USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK

USE YOS_SOIL , ONLY : TSOIL 
USE YOS_CST  , ONLY : TCST
! (C) Copyright 2015- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SRFSN_RSN* - Snow albedo
!     PURPOSE.
!     --------
!          THIS ROUTINE CONTROLS THE ALBEDO EVOLUTION

!**   INTERFACE.
!     ----------
!          *SRFSN_ASN* IS CALLED FROM *SRFSN_DRIVER*.

!     PARAMETER   DESCRIPTION                                    UNITS
!     ---------   -----------                                    -----

!     INPUT PARAMETERS (INTEGER):
!    *KIDIA*      START POINT
!    *KFDIA*      END POINT
!    *KLON*       NUMBER OF GRID POINTS PER PACKET


!     INPUT PARAMETERS (REAL):
!     *PTMST*      TIME STEP                                      S

!     INPUT PARAMETERS (LOGICAL):
!    *LLNOSNOW*   NO-SNOW/SNOW MASK (TRUE IF NO-SNOW)
!    *LDNH*       TRUE FOR NORTHERN HEMISPHERE

!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!    *PASNM1M*    SNOW ALBEDO                                      0-1
!    *PTSNM1M*    TEMPERATURE OF SNOW LAYER                        K
!    *PSNOWF*     TOTAL SNOW FLUX AT THE SURFACE                 KG/M**2/S
!    *PMSN*       DIFFERENCE BETWEEN LATENT HEAT OF MELTING      J/m**2
!                 AND LATENT HEAT OF FREEZING. PROXY FOR MELTING COND 

!     OUTPUT PARAMETERS AT T+1 (UNFILTERED,REAL):
!    *PASN*       SNOW ALBEDO                                      0-1
!     

!     METHOD.
!     -------
!          

!     EXTERNALS.
!     ----------
!          NONE.

!     REFERENCE.
!     ----------
!          

!     Modifications:
!     Original   E. Dutra      ECMWF     04/12/2015
!                G. Arduini    ECMWF     01/09/2021

!     ------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments 
INTEGER(KIND=JPIM), INTENT(IN)   :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KLON
REAL(KIND=JPRB)   , INTENT(IN)   :: PTMST
LOGICAL           , INTENT(IN)   :: LLNOSNOW(:) 
REAL(KIND=JPRB)   , INTENT(IN)   :: PCIL(:)
LOGICAL,            INTENT(IN)   :: LDNH(:)

REAL(KIND=JPRB)   , INTENT(IN)   :: PASNM1M(:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PMSN(:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PTSNM1M(:,:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PSNOWF(:)
TYPE(TSOIL)       , INTENT(IN)   :: YDSOIL
TYPE(TCST)        , INTENT(IN)   :: YDCST


REAL(KIND=JPRB)   , INTENT(OUT)  :: PASN(:)

! Local variables 
INTEGER(KIND=JPIM) :: JL
REAL(KIND=JPRB)    :: ZALBMIN, ZALBMAX, ZRTAU_A,ZTSNTHR
REAL(KIND=JPRB)    :: ZALBRESET(KLON)
REAL(KIND=JPRB)    :: ZRTAUF_GL
REAL(KIND=JPRB)    :: ZASN_L, ZASN_I
REAL(KIND=JPRB)    :: ZEPSILON
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!    -----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SRFSN_ASN_MOD:SRFSN_ASN',0,ZHOOK_HANDLE)

!    -----------------------------------------------------------------

! Glacier variables
ZALBMIN=0.65_JPRB
!*ZALBMAX=0.82_JPRB ! RALFMINPSN albedo glacier
!* Value tested over PROMICE Sites giving good results:
ZALBMAX=0.815_JPRB
ZRTAU_A=0.004     ! RTAU/2
ZRTAUF_GL=0.11_JPRB
ZTSNTHR=5._JPRB
ZEPSILON=100._JPRB*EPSILON(ZEPSILON)

!*ZALBRESET=10._JPRB ! mm to reset to alfa_max
! Test use different albedo reset over Antarctica or NH Glaciers:
DO JL=KIDIA,KFDIA
  IF (LDNH(JL))THEN
     ZALBRESET(JL)=10._JPRB ! mm to reset to alfa_max in NH
  ELSE
     ZALBRESET(JL)=5._JPRB ! mm to reset to alfa_max in SH (~Antarctica)
  ENDIF
ENDDO


DO JL=KIDIA,KFDIA
   IF (LLNOSNOW(JL)) THEN
     PASN(JL)= YDSOIL%RALFMAXSN
   ELSE
    ! Land part of the grid-cell
      IF (PMSN(JL) > 0.0_JPRB  ) THEN
      ! MELTING CONDITIONS
        ZASN_L=(PASNM1M(JL)-YDSOIL%RALFMINSN)*&
         & EXP(-YDSOIL%RTAUF*PTMST/YDCST%RDAY)+&
         & YDSOIL%RALFMINSN
      ELSE
      ! NORMAL CONDITIONS
        ZASN_L=MAX(YDSOIL%RALFMINSN,PASNM1M(JL)-YDSOIL%RTAUA*PTMST/YDCST%RDAY)
      ENDIF
      ! UPDATE ALBEDO DUE TO SNOWFALL EVENTS
      ZASN_L=ZASN_L+ MIN(MAX(PSNOWF(JL)*PTMST , 0._JPRB)/(10._JPRB) , 1._JPRB) *&
       & (YDSOIL%RALFMAXSN-ZASN_L)
      ZASN_L=MIN(YDSOIL%RALFMAXSN,MAX(ZASN_L,YDSOIL%RALFMINSN))
    
    !ELSE
    ! Ice part of the grid-cell
      IF (PTSNM1M(JL,1) > YDCST%RTT-ZTSNTHR) THEN
         ! MELTING CONDITIONS, modified minimum albedo and use different RTAUF.
         ! ZRTAUF_GL gives a rate of change of albedo in between RTAUF and the linear 
         ! relationship over a continuous change over 10-days.
         ZASN_I=(PASNM1M(JL)-ZALBMIN)*EXP(-ZRTAUF_GL*PTMST/YDCST%RDAY)+ZALBMIN
         ! MELTING CONDITIONS, modified minimum albedo
         !*ZASN_I=(PASNM1M(JL)-ZALBMIN)*EXP(-YDSOIL%RTAUF*PTMST/YDCST%RDAY)+ZALBMIN
         !* Use linear relationship also in melting conditions.
         !*ZASN_I=MAX(ZALBMIN,PASNM1M(JL)-YDSOIL%RTAUA*PTMST/YDCST%RDAY)
      ELSE
         ZASN_I=PASNM1M(JL)
      !*   ! NORMAL CONDITIONS, modified minimum albedo and tau_a
      !*   PASN(JL)=MAX(ZALBMIN,PASNM1M(JL)-ZRTAU_A*PTMST/YDCST%RDAY)
      ENDIF
      ! UPDATE ALBEDO DUE TO SNOWFALL EVENTS, modified threhshold to 5 instead of 10 kg m-2
      ZASN_I=ZASN_I+ MIN(MAX(PSNOWF(JL)*PTMST , 0._JPRB)/ZALBRESET(JL) , 1._JPRB) *&
              & (ZALBMAX-ZASN_I)
      ZASN_I=MIN(ZALBMAX,MAX(ZASN_I,ZALBMIN))

    IF (PCIL(JL)<1._JPRB-ZEPSILON)THEN
      PASN(JL)=(1._JPRB - PCIL(JL))*ZASN_L+PCIL(JL)*ZASN_I
    ELSE
      PASN(JL)=ZASN_I
    ENDIF
    ! UPDATE ALBEDO DUE TO SNOWFALL EVENTS
    PASN(JL)=PASN(JL)+ MIN(MAX(PSNOWF(JL)*PTMST , 0._JPRB)/(10._JPRB) , 1._JPRB) *&
     & (YDSOIL%RALFMAXSN-PASN(JL))
    PASN(JL)=MIN(YDSOIL%RALFMAXSN,MAX(PASN(JL),YDSOIL%RALFMINSN))

   ENDIF
ENDDO



!    -----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SRFSN_ASN_MOD:SRFSN_ASN',1,ZHOOK_HANDLE)

END SUBROUTINE SRFSN_ASN
END MODULE SRFSN_ASN_MOD
