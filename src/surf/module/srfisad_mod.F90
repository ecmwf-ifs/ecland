MODULE SRFISAD_MOD
CONTAINS
SUBROUTINE SRFISAD(KIDIA , KFDIA , KLON  , KLEVS ,&
 & PTMST   , PFRTI     , PTIAM1M5  , PAHFSTI5, PEVAPTI5,PGSN5, &
 & PSLRFL5 , PSSRFLTI5 , PTIA5   , LDICE   , LDNH,&
 & LNEMOLIMTHK, PTHKICE5, &
 & YDCST   , YDSOIL    ,&
 & PTIAM1M , PAHFSTI   , PEVAPTI , PGSN, &
 & PSLRFL  , PSSRFLTI  , PTIA  &
 & )

USE PARKIND1  , ONLY : JPIM, JPRB
USE YOMHOOK   , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_CST   , ONLY : TCST
USE YOS_SOIL  , ONLY : TSOIL

USE SRFWDIFS_MOD
USE SRFWDIFSAD_MOD

#ifdef DOC
! (C) Copyright 2011- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SRFISAD* - Computes temperature changes in soil.
!                 (Adjoint)

!     PURPOSE.
!     --------
!**   Computes temperature evolution of sea ice
!**   INTERFACE.
!     ----------
!          *SRFISAD* IS CALLED FROM *SURFTSTPSAD*.

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
!     *SRFWDIFSAD*

!     REFERENCE.
!     ----------
!          See documentation.

!     Original
!     --------
!       M. Janiskova              E.C.M.W.F.     02-04-2012  

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

LOGICAL,            INTENT(IN)    :: LDICE(:)
LOGICAL,            INTENT(IN)    :: LDNH(:)

REAL(KIND=JPRB),    INTENT(IN)    :: PTMST
REAL(KIND=JPRB),    INTENT(IN)   :: PFRTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)    :: PTIAM1M5(:,:)
REAL(KIND=JPRB),    INTENT(IN)    :: PAHFSTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)    :: PEVAPTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)    :: PSLRFL5(:)
REAL(KIND=JPRB),    INTENT(IN)    :: PSSRFLTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PGSN5(:) ! snow over seaice
LOGICAL,            INTENT(IN)   :: LNEMOLIMTHK
REAL(KIND=JPRB),    INTENT(IN)   :: PTHKICE5(:)

TYPE(TCST),         INTENT(IN)    :: YDCST
TYPE(TSOIL),        INTENT(IN)    :: YDSOIL

REAL(KIND=JPRB),    INTENT(OUT)   :: PTIA5(:,:)

REAL(KIND=JPRB),    INTENT(INOUT) :: PTIAM1M(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PAHFSTI(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PEVAPTI(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PSLRFL(:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PSSRFLTI(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PGSN(:) 
REAL(KIND=JPRB),    INTENT(INOUT) :: PTIA(:,:)


!      LOCAL VARIABLES

REAL(KIND=JPRB) :: ZSURFL5(KLON)
REAL(KIND=JPRB) :: ZLST5(KLON,KLEVS), ZCDZ5(KLON,KLEVS), ZRHS5(KLON,KLEVS)
REAL(KIND=JPRB) :: ZTIA5(KLON,KLEVS)
REAL(KIND=JPRB) :: ZPTIA5(KLON,KLEVS)
REAL(KIND=JPRB) :: ZTHFL5
REAL(KIND=JPRB) :: ZSSRFL5, ZSLRFL5
REAL(KIND=JPRB) :: ZSSRFL, ZSLRFL

REAL(KIND=JPRB) :: ZSURFL(KLON)
REAL(KIND=JPRB) :: ZRHS(KLON,KLEVS), ZCDZ(KLON,KLEVS)
REAL(KIND=JPRB) :: ZLST(KLON,KLEVS), ZTIA(KLON,KLEVS)
REAL(KIND=JPRB) :: ZDAI(KLON,KLEVS)
REAL(KIND=JPRB) :: ZCONS1, ZCONS2, ZTHFL
REAL(KIND=JPRB) :: ZTHICK,ZTHICK2
REAL(KIND=JPRB) :: ZTHICKICE_ENERGY
REAL(KIND=JPRB) :: ZEPSICE

LOGICAL ::LLALLAYS, LLDOICE
INTEGER(KIND=JPIM) :: JK, JL

REAL(KIND=JPRB) :: ZEPSILON
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

! -------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SRFISAD_MOD:SRFISAD',0,ZHOOK_HANDLE)
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

  ZSURFL5(JL) = 0.0_JPRB
ENDDO

DO JK=1,KLEVS
  DO JL=KIDIA,KFDIA
    ZLST5(JL,JK) = 0.0_JPRB
    ZCDZ5(JL,JK) = 0.0_JPRB
    ZRHS5(JL,JK) = 0.0_JPRB

    ZTIA5(JL,JK) = 0.0_JPRB
    PTIA5(JL,JK) = RTFREEZSICE
  ENDDO
ENDDO

!* Computation done for only top or all soil layers

LLALLAYS = .TRUE.    ! done for all layers
!LLALLAYS = .FALSE.   ! done for top layer only
ZEPSILON=EPSILON(ZEPSILON)

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

  ZEPSICE=0.01_JPRB
  DO JL=KIDIA,KFDIA
    !Limit ice thickness to 0.5m
    ZTHICKICE_ENERGY=MAX(0.5_JPRB, PTHKICE5(JL))
    IF (LNEMOLIMTHK) THEN
      ZTHICK=RDAI(1)+RDAI(2)+RDAI(3)
      IF ( (ZTHICKICE_ENERGY >= (ZTHICK+ZEPSICE)) .AND. &
          &(ZTHICKICE_ENERGY-ZTHICK >= RDAI(3)) ) THEN
          
         ZDAI(JL,KLEVS)=ZTHICKICE_ENERGY-ZTHICK
      ELSEIF ( (ZTHICKICE_ENERGY >= (ZTHICK+ZEPSICE)) .AND. &
          &(ZTHICKICE_ENERGY-ZTHICK<RDAI(3)) ) THEN
         ZTHICK2 = (RDAI(3)+ZTHICKICE_ENERGY-ZTHICK)/2._JPRB
         ZDAI(JL,KLEVS-1:KLEVS)=ZTHICK2
      ELSEIF ( (ZTHICKICE_ENERGY < (ZTHICK+ZEPSICE)) .AND. &
               (ZTHICKICE_ENERGY > KLEVS*RDAI(1)) )THEN
         ZTHICK2 = (ZTHICKICE_ENERGY-RDAI(1))/(KLEVS-1._JPRB)
         ZDAI(JL,2:KLEVS)=ZTHICK2
      ELSE IF (ZTHICKICE_ENERGY <= KLEVS*RDAI(1))THEN
         ZDAI(JL,1:KLEVS)   = RDAI(1)
      ELSE 
         ZDAI(JL,1:KLEVS)   = RDAI(1)
      ENDIF

    ELSE
      IF (LDNH(JL)) THEN
        ZDAI(JL,KLEVS)=RDARSICE-(RDAI(1)+RDAI(2)+RDAI(3))
      ELSE
        ZDAI(JL,KLEVS)=RDANSICE-(RDAI(1)+RDAI(2)+RDAI(3))
      ENDIF
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
      ZTHFL5 = PAHFSTI5(JL,2)+RLSTT*PEVAPTI5(JL,2)
      ZSURFL5(JL) = PSSRFLTI5(JL,2)+PSLRFL5(JL)+ZTHFL5
      IF (YDSOIL%LESNICE) THEN
        ZSSRFL5=PSSRFLTI5(JL,2)*PFRTI(JL,2)
        ZSLRFL5=PSLRFL5(JL)*PFRTI(JL,2)
        ZTHFL5=PAHFSTI5(JL,2)*PFRTI(JL,2)+RLSTT*PEVAPTI5(JL,2)*PFRTI(JL,2)
        ZSURFL5(JL)=(PGSN5(JL)+ZSSRFL5+ZSLRFL5+ZTHFL5)/MAX(ZEPSILON,(PFRTI(JL,2)+PFRTI(JL,5)))
      ENDIF
    ENDIF
  ENDDO

!     Layer 1

  JK=1
  DO JL=KIDIA,KFDIA
    IF (LDICE(JL)) THEN
      ZLST5(JL,JK) = ZCONS1*RCONDSICE/(ZDAI(JL,JK)+ZDAI(JL,JK+1))
      ZCDZ5(JL,JK) = RRCSICE*ZDAI(JL,JK)
      ZRHS5(JL,JK) = PTMST*ZSURFL5(JL)/ZCDZ5(JL,JK)
    ENDIF
  ENDDO

  IF (LLALLAYS) THEN

!     Layers 2 to KLEVS-1
    DO JK=2,KLEVS-1
      DO JL=KIDIA,KFDIA
        IF (LDICE(JL)) THEN
          ZLST5(JL,JK) = ZCONS1*RCONDSICE/(ZDAI(JL,JK)+ZDAI(JL,JK+1))
          ZCDZ5(JL,JK) = RRCSICE*ZDAI(JL,JK)
        ENDIF
      ENDDO
    ENDDO

!     Layers KLEVS
    JK=KLEVS
    DO JL=KIDIA,KFDIA
      IF (LDICE(JL)) THEN
        ZLST5(JL,JK) = ZCONS1*RCONDSICE/(2.*ZDAI(JL,JK))
        ZCDZ5(JL,JK) = RRCSICE*ZDAI(JL,JK)
        ZRHS5(JL,JK) = (RTFREEZSICE/RSIMP)*ZLST5(JL,JK)/ZCDZ5(JL,JK)
      ENDIF
     ENDDO
  ENDIF
ENDIF


!*         4. Call tridiagonal solver
!             -----------------------
  CALL SRFWDIFS(KIDIA,KFDIA,KLON,KLEVS,PTIAM1M5,ZLST5,ZRHS5,ZCDZ5,ZTIA5,&
   & LDICE,LLALLAYS,YDSOIL)

 DO JK=1,KLEVS
    DO JL=KIDIA,KFDIA
      IF (LDICE(JL)) THEN
        PTIA5(JL,JK) = PTIAM1M5(JL,JK)*ZCONS2+ZTIA5(JL,JK)
      ELSE
        PTIA5(JL,JK) = RTFREEZSICE
      ENDIF
      ZPTIA5(JL,JK) = PTIA5(JL,JK)

      IF (PTIA5(JL,JK) > RTMELTSICE) THEN
        PTIA5(JL,JK) = RTMELTSICE
      ENDIF
    ENDDO
  ENDDO

!          0.  ADJOINT CALCULATIONS
!              --------------------

!* Set local variables to zero

ZSURFL(:) = 0.0_JPRB
ZRHS(:,:) = 0.0_JPRB
ZCDZ(:,:) = 0.0_JPRB
ZLST(:,:) = 0.0_JPRB
ZTIA(:,:) = 0.0_JPRB
ZDAI(:,:) = 0.0_JPRB

!*         0.6. New temperatures
!               ----------------

IF (LLDOICE) THEN
  DO JK=KLEVS,1,-1
    DO JL=KIDIA,KFDIA
      IF (ZPTIA5(JL,JK) > RTMELTSICE) THEN
        PTIA(JL,JK) = 0.0_JPRB
      ENDIF

      IF (LDICE(JL)) THEN
        PTIAM1M(JL,JK) = PTIAM1M(JL,JK)+ZCONS2*PTIA(JL,JK)
        ZTIA(JL,JK) = ZTIA(JL,JK)+PTIA(JL,JK)
      ENDIF
      PTIA(JL,JK) = 0.0_JPRB
    ENDDO
  ENDDO

!*         0.4. Call tridiagonal solver
!               -----------------------


  CALL SRFWDIFSAD(KIDIA,KFDIA,KLON,KLEVS,PTIAM1M5,ZLST5,ZRHS5,ZCDZ5,ZTIA5,&
   & LDICE,LLALLAYS,YDSOIL,&
   & PTIAM1M,ZLST,ZRHS,ZCDZ,ZTIA)

!*         0.2. Compute net heat flux at the surface.
!               -------------------------------------

  IF (LLALLAYS) THEN

!     Layers KLEVS
    JK=KLEVS
    DO JL=KIDIA,KFDIA
      IF (LDICE(JL)) THEN
        ZLST(JL,JK) = ZLST(JL,JK)+(RTFREEZSICE/RSIMP)*ZRHS(JL,JK)/ZCDZ5(JL,JK)
        ZCDZ(JL,JK) = ZCDZ(JL,JK)-(RTFREEZSICE/RSIMP)*ZLST5(JL,JK)*ZRHS(JL,JK) &
         & /ZCDZ5(JL,JK)**2
        ZRHS(JL,JK) = 0.0_JPRB
        ZCDZ(JL,JK) = 0.0_JPRB
        ZLST(JL,JK) = 0.0_JPRB
      ENDIF
    ENDDO

!     Layers 2 to KLEVS-1
    DO JK=KLEVS-1,2,-1
      DO JL=KIDIA,KFDIA
        IF (LDICE(JL)) THEN
          ZCDZ(JL,JK) = 0.0_JPRB
          ZLST(JL,JK) = 0.0_JPRB 
        ENDIF
      ENDDO
    ENDDO
  ENDIF

!     Layer 1

  JK=1
  DO JL=KIDIA,KFDIA
    IF (LDICE(JL)) THEN
      ZSURFL(JL) = ZSURFL(JL)+PTMST*ZRHS(JL,JK)/ZCDZ5(JL,JK)
      ZCDZ(JL,JK) = ZCDZ(JL,JK)-PTMST*ZSURFL5(JL)*ZRHS(JL,JK)/ZCDZ5(JL,JK)**2
      ZRHS(JL,JK) = 0.0_JPRB 
      ZCDZ(JL,JK) = 0.0_JPRB
      ZLST(JL,JK) = 0.0_JPRB
     ENDIF
   ENDDO

  DO JL=KIDIA,KFDIA
    IF (LDICE(JL)) THEN
      IF(.NOT. YDSOIL%LESNICE) THEN
        ZTHFL = 0.0_JPRB

        PSSRFLTI(JL,2) = PSSRFLTI(JL,2)+ZSURFL(JL)
        PSLRFL(JL) = PSLRFL(JL)+ZSURFL(JL)
        ZTHFL = ZTHFL+ZSURFL(JL)
        ZSURFL(JL) = 0.0_JPRB
        PAHFSTI(JL,2) = PAHFSTI(JL,2)+ZTHFL
        PEVAPTI(JL,2) = PEVAPTI(JL,2)+RLSTT*ZTHFL
      ELSE
        ZTHFL = 0.0_JPRB
        ZSSRFL= 0._JPRB
        ZSLRFL= 0._JPRB
        PGSN(JL)=PGSN(JL)+ZSURFL(JL)/MAX(ZEPSILON,(PFRTI(JL,2)+PFRTI(JL,5)))
        ZSSRFL=ZSSRFL+ZSURFL(JL)/MAX(ZEPSILON,(PFRTI(JL,2)+PFRTI(JL,5)))
        ZSLRFL=ZSLRFL+ZSURFL(JL)/MAX(ZEPSILON,(PFRTI(JL,2)+PFRTI(JL,5)))
        ZTHFL=ZTHFL+ZSURFL(JL)/MAX(ZEPSILON,(PFRTI(JL,2)+PFRTI(JL,5)))
        PAHFSTI(JL,2)=PAHFSTI(JL,2)+ZTHFL*PFRTI(JL,2)
        PEVAPTI(JL,2)=PEVAPTI(JL,2)+ZTHFL*PFRTI(JL,2)*RLSTT
        PSLRFL(JL)=PSLRFL(JL)+ZSLRFL*PFRTI(JL,2)
        PSSRFLTI(JL,2)=PSSRFLTI(JL,2)+ZSLRFL*PFRTI(JL,2)
      ENDIF
    ENDIF
  ENDDO

ENDIF

DO JK=1,KLEVS
  DO JL=KIDIA,KFDIA
    PTIA(JL,JK) = 0.0_JPRB
    ZTIA(JL,JK) = 0.0_JPRB

    ZRHS(JL,JK) = 0.0_JPRB
    ZCDZ(JL,JK) = 0.0_JPRB
    ZLST(JL,JK) = 0.0_JPRB
  ENDDO
ENDDO

DO JL=KIDIA,KFDIA
  ZSURFL(JL) = 0.0_JPRB
ENDDO

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SRFISAD_MOD:SRFISAD',1,ZHOOK_HANDLE)

END SUBROUTINE SRFISAD
END MODULE SRFISAD_MOD



