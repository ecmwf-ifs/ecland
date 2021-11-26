MODULE SRFISTL_MOD
CONTAINS
SUBROUTINE SRFISTL(KIDIA , KFDIA , KLON  , KLEVS ,&
 & PTMST   , PTIAM1M5  , PAHFSTI5, PEVAPTI5,&
 & PSLRFL5 , PSSRFLTI5 , PTIA5   , LDICE   , LDNH,&
 & YDCST   , YDSOIL    ,&
 & PTIAM1M , PAHFSTI   , PEVAPTI ,&
 & PSLRFL  , PSSRFLTI  , PTIA  &
 & )

USE PARKIND1  , ONLY : JPIM, JPRB
USE YOMHOOK   , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_CST   , ONLY : TCST
USE YOS_SOIL  , ONLY : TSOIL

USE SRFWDIFSTL_MOD

#ifdef DOC
! (C) Copyright 2011- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SRFISTL* - Computes temperature changes in soil.
!                 (Tangent linear)

!     PURPOSE.
!     --------
!**   Computes temperature evolution of sea ice
!**   INTERFACE.
!     ----------
!          *SRFISTL* IS CALLED FROM *SURFTSTPSTL*.

!     PARAMETER   DESCRIPTION                                           UNITS
!     ---------   -----------                                           -----
!     INPUT PARAMETERS (INTEGER):
!     *KIDIA*      START POINT
!     *KFDIA*      END POINT
!     *KLON*       NUMBER OF GRID POINTS PER PACKET
!     *KLEVS*      NUMBER OF SOIL LAYERS
!     *KTILES*     NUMBER OF SURFACE TILES

!     INPUT PARAMETERS (LOGICAL):
!     *LDICE*      ICE MASK (TRUE for sea ice)
!     *LDNH*       TRUE FOR NORTHERN HEMISPHERE


!     INPUT PARAMETERS (REAL):
!     *PTMST*      TIME STEP                                            S

!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!  Trajectory  Perturbation  Description                                Unit
!  PTIAM1M5    PTIAM1M       SEA ICE TEMPERATURE                        K
!  PSLRFL5     PSLRFL        NET LONGWAVE  RADIATION AT THE SURFACE     W/m2
!  PAHFSTI5    PAHFSTI       TILE SURFACE SENSIBLE HEAT FLUX            W/m2
!  PEVAPTI5    PEVAPTI       TILE SURFACE MOISTURE FLUX                 kg/m2/s
!  PSSRFLTI5   PSSRFLTI      TILE NET SHORTWAVE RADIATION FLUX          W/m2
!                            AT SURFACE

!     UPDATED PARAMETERS AT T+1 (UNFILTERED,REAL):
!  Trajectory  Perturbation  Description                                Unit
!  PTIA5       PTIA          SOIL TEMPERATURE                           K

!     METHOD.
!     -------
!          Parameters are set and the tridiagonal solver is called.

!     EXTERNALS.
!     ----------
!     *SRFWDIFSTL*

!     REFERENCE.
!     ----------
!          See documentation.

!     Original
!     --------
!       M. Janiskova              E.C.M.W.F.     08-02-2012  

!     Modifications
!     -------------

!     ------------------------------------------------------------------
#endif

IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM), INTENT(IN)   :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KLON
INTEGER(KIND=JPIM), INTENT(IN)   :: KLEVS

LOGICAL,            INTENT(IN)   :: LDICE(:)
LOGICAL,            INTENT(IN)   :: LDNH(:)

REAL(KIND=JPRB),    INTENT(IN)   :: PTMST
REAL(KIND=JPRB),    INTENT(IN)   :: PTIAM1M5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PAHFSTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSLRFL5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSRFLTI5(:,:)

TYPE(TCST),         INTENT(IN)   :: YDCST
TYPE(TSOIL),        INTENT(IN)   :: YDSOIL

REAL(KIND=JPRB),    INTENT(OUT)  :: PTIA5(:,:)

REAL(KIND=JPRB),    INTENT(IN)   :: PTIAM1M(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PAHFSTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSLRFL(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSRFLTI(:,:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PTIA(:,:)


!      LOCAL VARIABLES

REAL(KIND=JPRB) :: ZSURFL5(KLON)
REAL(KIND=JPRB) :: ZLST5(KLON,KLEVS), ZCDZ5(KLON,KLEVS), ZRHS5(KLON,KLEVS)
REAL(KIND=JPRB) :: ZTIA5(KLON,KLEVS)
REAL(KIND=JPRB) :: ZTHFL5

REAL(KIND=JPRB) :: ZSURFL(KLON)
REAL(KIND=JPRB) :: ZRHS(KLON,KLEVS), ZCDZ(KLON,KLEVS)
REAL(KIND=JPRB) :: ZLST(KLON,KLEVS), ZTIA(KLON,KLEVS)
REAL(KIND=JPRB) :: ZDAI(KLON,KLEVS)
REAL(KIND=JPRB) :: ZCONS1, ZCONS2, ZTHFL

LOGICAL ::LLALLAYS, LLDOICE
INTEGER(KIND=JPIM) :: JK, JL

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

! -------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SRFISTL_MOD:SRFISTL',0,ZHOOK_HANDLE)
ASSOCIATE(RLSTT=>YDCST%RLSTT, &
 & RCONDSICE=>YDSOIL%RCONDSICE, RDAI=>YDSOIL%RDAI, RDANSICE=>YDSOIL%RDANSICE, &
 & RDARSICE=>YDSOIL%RDARSICE, RRCSICE=>YDSOIL%RRCSICE, RSIMP=>YDSOIL%RSIMP, &
 & RTFREEZSICE=>YDSOIL%RTFREEZSICE, RTMELTSICE=>YDSOIL%RTMELTSICE)

!*    0. INITIALIZATION
!     ------------------

LLDOICE = .FALSE.
DO JL=KIDIA,KFDIA
  IF (LDICE(JL)) THEN
    LLDOICE = .TRUE.  ! if any point is sea ice
  ENDIF

  ZSURFL (JL) = 0.0_JPRB
  ZSURFL5(JL) = 0.0_JPRB
ENDDO

DO JK=1,KLEVS
  DO JL=KIDIA,KFDIA
    ZLST (JL,JK) = 0.0_JPRB
    ZLST5(JL,JK) = 0.0_JPRB
    ZCDZ (JL,JK) = 0.0_JPRB
    ZCDZ5(JL,JK) = 0.0_JPRB
    ZRHS (JL,JK) = 0.0_JPRB
    ZRHS5(JL,JK) = 0.0_JPRB

    ZTIA (JL,JK) = 0.0_JPRB
    ZTIA5(JL,JK) = 0.0_JPRB
    PTIA (JL,JK) = 0.0_JPRB
    PTIA5(JL,JK) = RTFREEZSICE
  ENDDO
ENDDO

!* Computation done for only top or all soil layers

LLALLAYS = .TRUE.    ! done for all layers
!LLALLAYS = .FALSE.   ! done for top layer only

IF (LLDOICE) THEN

!*         1. SET UP SOME CONSTANTS.
!             --- -- ---- ----------

!*    PHYSICAL CONSTANTS.
!     -------- ----------

  DO JK=1,KLEVS-1
    DO JL=KIDIA,KFDIA
      ZDAI(JL,JK)=RDAI(JK)
    ENDDO
  ENDDO

  DO JL=KIDIA,KFDIA
    IF (LDNH(JL)) THEN
      ZDAI(JL,KLEVS)=RDARSICE-(RDAI(1)+RDAI(2)+RDAI(3))
    ELSE
      ZDAI(JL,KLEVS)=RDANSICE-(RDAI(1)+RDAI(2)+RDAI(3))
    ENDIF
  ENDDO

!*    COMPUTATIONAL CONSTANTS.
!     ------------- ----------

  ZCONS1=PTMST*RSIMP*2.0_JPRB
  ZCONS2=1.0_JPRB-1.0_JPRB/RSIMP


!*         2. Compute net heat flux at the surface.
!             -------------------------------------

  DO JL=KIDIA,KFDIA
    IF (LDICE(JL)) THEN
      ZTHFL  = PAHFSTI (JL,2)+RLSTT*PEVAPTI (JL,2)
      ZTHFL5 = PAHFSTI5(JL,2)+RLSTT*PEVAPTI5(JL,2)
      ZSURFL (JL) = PSSRFLTI (JL,2)+PSLRFL (JL)+ZTHFL
      ZSURFL5(JL) = PSSRFLTI5(JL,2)+PSLRFL5(JL)+ZTHFL5
    ENDIF
  ENDDO

!     Layer 1

  JK=1
  DO JL=KIDIA,KFDIA
    IF (LDICE(JL)) THEN
      ZLST (JL,JK) = 0.0_JPRB
      ZLST5(JL,JK) = ZCONS1*RCONDSICE/(ZDAI(JL,JK)+ZDAI(JL,JK+1))
      ZCDZ (JL,JK) = 0.0_JPRB
      ZCDZ5(JL,JK) = RRCSICE*ZDAI(JL,JK)
      ZRHS (JL,JK) = PTMST*(ZCDZ5(JL,JK)*ZSURFL(JL)-ZSURFL5(JL)*ZCDZ(JL,JK)) &
       & /ZCDZ5(JL,JK)**2
      ZRHS5(JL,JK) = PTMST*ZSURFL5(JL)/ZCDZ5(JL,JK)
    ENDIF
  ENDDO

  IF (LLALLAYS) THEN

!     Layers 2 to KLEVS-1
    DO JK=2,KLEVS-1
      DO JL=KIDIA,KFDIA
        IF (LDICE(JL)) THEN
          ZLST (JL,JK) = 0.0_JPRB 
          ZLST5(JL,JK) = ZCONS1*RCONDSICE/(ZDAI(JL,JK)+ZDAI(JL,JK+1))
          ZCDZ (JL,JK) = 0.0_JPRB 
          ZCDZ5(JL,JK) = RRCSICE*ZDAI(JL,JK)
        ENDIF
      ENDDO
    ENDDO

!     Layers KLEVS
    JK=KLEVS
    DO JL=KIDIA,KFDIA
      IF (LDICE(JL)) THEN
        ZLST (JL,JK) = 0.0_JPRB
        ZLST5(JL,JK) = ZCONS1*RCONDSICE/(2.*ZDAI(JL,JK))
        ZCDZ (JL,JK) = 0.0_JPRB
        ZCDZ5(JL,JK) = RRCSICE*ZDAI(JL,JK)
        ZRHS (JL,JK) = (RTFREEZSICE/RSIMP) &
         & *(ZCDZ5(JL,JK)*ZLST(JL,JK)-ZLST5(JL,JK)*ZCDZ(JL,JK)) &
         & /ZCDZ5(JL,JK)**2
        ZRHS5(JL,JK) = (RTFREEZSICE/RSIMP)*ZLST5(JL,JK)/ZCDZ5(JL,JK)
      ENDIF
     ENDDO
  ENDIF

!*         4. Call tridiagonal solver
!             -----------------------


  CALL SRFWDIFSTL(KIDIA,KFDIA,KLON,KLEVS,PTIAM1M5,ZLST5,ZRHS5,ZCDZ5,ZTIA5,&
   & LDICE,LLALLAYS,YDSOIL,&
   & PTIAM1M,ZLST,ZRHS,ZCDZ,ZTIA)

!*         6. New temperatures
!             ----------------

  DO JK=1,KLEVS
    DO JL=KIDIA,KFDIA
      IF (LDICE(JL)) THEN
        PTIA (JL,JK) = PTIAM1M (JL,JK)*ZCONS2+ZTIA (JL,JK)
        PTIA5(JL,JK) = PTIAM1M5(JL,JK)*ZCONS2+ZTIA5(JL,JK)
      ELSE
        PTIA (JL,JK) = 0.0_JPRB
        PTIA5(JL,JK) = RTFREEZSICE
      ENDIF
      IF (PTIA5(JL,JK) > RTMELTSICE) THEN
        PTIA (JL,JK) = 0.0_JPRB
        PTIA5(JL,JK) = RTMELTSICE
      ENDIF
    ENDDO
  ENDDO

ENDIF

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SRFISTL_MOD:SRFISTL',1,ZHOOK_HANDLE)

END SUBROUTINE SRFISTL
END MODULE SRFISTL_MOD



