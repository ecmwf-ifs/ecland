MODULE SRFILSAD_MOD
CONTAINS
SUBROUTINE SRFILSAD(KIDIA  , KFDIA  , KLON , KLEVI  ,LDLAND, PCIL,&
 & PTMST  ,PTIAM1M5 ,PFRTI , PAHFSTI5, PEVAPTI5,PGSNICE5,&
 & PSLRFL5 ,PSSRFLTI5, PTSOIL5, LDICE  , LDNH   ,&
 & PTIA5  ,PGICE5, &
 & YDCST  ,YDSOIL, &
 & PTIAM1M, PAHFSTI , PEVAPTI , PGSNICE, &
 & PSLRFL , PSSRFLTI, PTSOIL, &
 & PTIA  ,PGICE)

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_CST  , ONLY : TCST
USE YOS_SOIL , ONLY : TSOIL

USE SRFWDIFS_MOD
USE SRFWDIFSAD_MOD

! (C) Copyright 2025- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SRFILSAD* - Computes temperature changes in land ice (adjoint)

!     PURPOSE.
!     --------
!**   Computes temperature evolution of land ice  (adjoin)
!**   INTERFACE.
!     ----------
!          *SRFILSAD* IS CALLED FROM *SURF*.
!     PARAMETER   DESCRIPTION                                    UNITS
!     ---------   -----------                                    -----
!     INPUT PARAMETERS (INTEGER):
!    *KIDIA*      START POINT
!    *KFDIA*      END POINT
!    *KLON*       NUMBER OF GRID POINTS PER PACKET
!    *KTILES*     NUMBER OF SURFACE TILES
!    *KLEVI*      Number of ice layers

!     INPUT PARAMETERS (REAL):
!    *PTMST*      TIME STEP                                      S

!     INPUT PARAMETERS (LOGICAL):
!    *LDICE*      ICE MASK (TRUE for land ice)
!    *LDNH*       TRUE FOR NORTHERN HEMISPHERE

!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!    *PFRTI*      TILE FRACTIONS                              (0-1)
!            1 : WATER                  5 : SNOW ON LOW-VEG+BARE-SOIL
!            2 : ICE                    6 : DRY SNOW-FREE HIGH-VEG
!            3 : WET SKIN               7 : SNOW UNDER HIGH-VEG
!            4 : DRY SNOW-FREE LOW-VEG  8 : BARE SOIL
!            9 : LAKE                  10 : URBAN
!    Traject   Perturbation    Descript                         Units
!    *PTIAM1M5*  *PTIAM1M*    ICE TEMPERATURE                            K
!    *PSLRFL5*   *PSLRFL*     NET LONGWAVE  RADIATION AT THE SURFACE        W/M**2
                             
!    *PAHFSTI5*  *PAHFSTI*    TILE SURFACE SENSIBLE HEAT FLUX                 W/M2
!    *PEVAPTI5*  *PEVAPTI*    TILE SURFACE MOISTURE FLUX                     KG/M2/S
!    *PSSRFLTI5* *PSSRFLTI*   TILE NET SHORTWAVE RADIATION FLUX AT SURFACE    W/M2
!    *PGSN5*     *PGSN*       SNOW basal heat flux between snow and ice       W/M2
!    UPDATED PARAMETERS AT T+1 (UNFILTERED,REAL):
!    Traject   Perturbation    Descript                         Units
!    *PTIA5*    *PTIA*     ICE TEMPERATURE                               K
!    *PGICE5*   *PGICE*    BASAL HEAT FLUX FROM LAND ICE  to soil          K

!     METHOD.
!     -------
!          Parameters are set and the tridiagonal solver is called.

!     EXTERNALS.
!     ----------
!     *SRFWDIF*

!     REFERENCE.
!     ----------
!          See documentation.
!     G. Arduini                2024 Adjoint of srfils

!     ------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM), INTENT(IN)   :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KLON
INTEGER(KIND=JPIM), INTENT(IN)   :: KLEVI
LOGICAL, INTENT(IN)   :: LDLAND(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PCIL(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTMST
REAL(KIND=JPRB),    INTENT(IN)   :: PFRTI(:,:)
! Trajectory
REAL(KIND=JPRB),    INTENT(IN)   :: PTIAM1M5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PAHFSTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSLRFL5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PGSNICE5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSRFLTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSOIL5(:)
! Output, trajectory
REAL(KIND=JPRB),    INTENT(OUT)  :: PTIA5(:,:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PGICE5(:)

! Pertubation
REAL(KIND=JPRB),    INTENT(INOUT) :: PTIAM1M(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PAHFSTI(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PEVAPTI(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PSLRFL(:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PGSNICE(:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PSSRFLTI(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PTSOIL(:)
! Output, Perturbation
REAL(KIND=JPRB),    INTENT(INOUT)  :: PTIA(:,:)
REAL(KIND=JPRB),    INTENT(INOUT)  :: PGICE(:)

LOGICAL,            INTENT(IN)   :: LDICE(:)
LOGICAL,            INTENT(IN)   :: LDNH(:)
TYPE(TCST),         INTENT(IN)   :: YDCST
TYPE(TSOIL),        INTENT(IN)   :: YDSOIL

!      LOCAL VARIABLES

REAL(KIND=JPRB) :: ZSURFL5(KLON),     ZSURFL(KLON)
REAL(KIND=JPRB) :: ZRHS5(KLON,KLEVI), ZRHS(KLON,KLEVI) 
REAL(KIND=JPRB) :: ZCDZ5(KLON,KLEVI), ZCDZ(KLON,KLEVI)
REAL(KIND=JPRB) :: ZLST5(KLON,KLEVI), ZLST(KLON,KLEVI)
REAL(KIND=JPRB) :: ZTIA5(KLON,KLEVI), ZTIA(KLON,KLEVI)  
REAL(KIND=JPRB) :: ZDAI(KLON,KLEVI)
REAL(KIND=JPRB) :: ZPTIA5(KLON,KLEVI)
REAL(KIND=JPRB) :: ZDARLICE
REAL(KIND=JPRB) :: ZCSN_I

REAL(KIND=JPRB) :: ZEPSILON
LOGICAL :: LLDOICE(KLON)
LOGICAL ::LLALLAYS

INTEGER(KIND=JPIM) :: JK, JL

REAL(KIND=JPRB) :: ZCONS1, ZCONS2,ZTMST 
REAL(KIND=JPRB) :: ZSLRFL,  ZSSRFL,  ZTHFL
REAL(KIND=JPRB) :: ZSLRFL5, ZSSRFL5, ZTHFL5
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!*         1. SET UP SOME CONSTANTS.
!             --- -- ---- ----------

IF (LHOOK) CALL DR_HOOK('SRFILSAD_MOD:SRFILSAD',0,ZHOOK_HANDLE)
ASSOCIATE(RLSTT=>YDCST%RLSTT, RTT=>YDCST%RTT, RLMLT=>YDCST%RLMLT, &
 & RCONDSICE=>YDSOIL%RCONDSICE, RDAI=>YDSOIL%RDAI, RDANSICE=>YDSOIL%RDANSICE, &
 & RDARSICE=>YDSOIL%RDARSICE, RRCSICE=>YDSOIL%RRCSICE, RSIMP=>YDSOIL%RSIMP, &
 & RTFREEZSICE=>YDSOIL%RTFREEZSICE, RTMELTSICE=>YDSOIL%RTMELTSICE, &
 & RHOCI=>YDSOIL%RHOCI,RHOICE=>YDSOIL%RHOICE)

!*    COMPUTATIONAL CONSTANTS.
!     ------------- ----------

ZTMST=1.0_JPRB/PTMST
ZCONS1=PTMST*RSIMP*2.0_JPRB
ZCONS2=1.0_JPRB-1.0_JPRB/RSIMP
ZEPSILON=EPSILON(ZEPSILON)


!    Set up the ice layer thicknesses.
ZDARLICE=10.86_JPRB ! equivalent to 10000/920, previous tests: 30._JPRB
DO JK=1,KLEVI-1
  DO JL=KIDIA,KFDIA
    ZDAI(JL,JK)=RDAI(JK)
  ENDDO
ENDDO

DO JL=KIDIA,KFDIA
  ZDAI(JL,KLEVI)=ZDARLICE-(RDAI(1)+RDAI(2)+RDAI(3))
ENDDO

LLALLAYS = .TRUE.    ! done for all layers


!*         2. Compute net heat flux at the surface.
!             -------------------------------------

DO JL=KIDIA,KFDIA
  LLDOICE(JL)=LDICE(JL) .and. LDLAND(JL)

  IF (LLDOICE(JL)) THEN
    ! This need to be weighted properly:
      ZSSRFL5=PFRTI(JL,2)*PSSRFLTI5(JL,2)
      ZSLRFL5=PFRTI(JL,2)*PSLRFL5(JL)
      ZTHFL5=PFRTI(JL,2)*PAHFSTI5(JL,2)+RLSTT*PFRTI(JL,2)*PEVAPTI5(JL,2)
    ! PGSNICE(JL) only applies to the fraction of snow over the ice fraction.
      IF (PCIL(JL) > ZEPSILON) THEN
        ZSURFL5(JL)=(PGSNICE5(JL)+ZSSRFL5+ZSLRFL5+ZTHFL5)/PCIL(JL)
      ELSE
        ZSURFL5(JL)=(PGSNICE5(JL)+ZSSRFL5+ZSLRFL5+ZTHFL5)/ZEPSILON
      ENDIF
  ELSE
    ZSURFL5(JL)=0.0_JPRB
  ENDIF
ENDDO

!*         3. Set arrays
!             ----------

PGICE5(KIDIA:KFDIA)=0.0_JPRB
!     Layer 1

JK=1
DO JL=KIDIA,KFDIA
  IF (LLDOICE(JL)) THEN
    ZLST5(JL,JK)=ZCONS1*RCONDSICE/(ZDAI(JL,JK)+ZDAI(JL,JK+1))
    ZCDZ5(JL,JK)=RRCSICE*ZDAI(JL,JK)
    ZRHS5(JL,JK)=PTMST*ZSURFL5(JL)/ZCDZ5(JL,JK)
  ENDIF
ENDDO

!     Layers 2 to KLEVI-1

DO JK=2,KLEVI-1
  DO JL=KIDIA,KFDIA
    IF (LLDOICE(JL)) THEN
      ZLST5(JL,JK)=ZCONS1*RCONDSICE/(ZDAI(JL,JK)+ZDAI(JL,JK+1))
      ZCDZ5(JL,JK)=RRCSICE*ZDAI(JL,JK)
      ZRHS5(JL,JK)=0.0_JPRB
    ENDIF
  ENDDO
ENDDO

!     Layers KLEVI

JK=KLEVI
DO JL=KIDIA,KFDIA
  IF (LLDOICE(JL)) THEN
    ! We use soil temperature as bottom boundary condition
      ! This we can do better and compute average conductivity with
      ! half soil layer like in srfsn_
      ZLST5(JL,JK)=ZCONS1*RCONDSICE/(2._JPRB*ZDAI(JL,JK))
      ZCDZ5(JL,JK)=RRCSICE*ZDAI(JL,JK)
      ZRHS5(JL,JK)=(PTSOIL5(JL)/RSIMP)*ZLST5(JL,JK)/ZCDZ5(JL,JK)
  ENDIF
ENDDO

!*         4. Call tridiagonal solver
!             -----------------------

CALL SRFWDIFS(KIDIA,KFDIA,KLON,KLEVI,PTIAM1M5,ZLST5,ZRHS5,ZCDZ5,&
 &            ZTIA5,LLDOICE,LLALLAYS,YDSOIL)

!*         5. New temperatures
!             ----------------

DO JK=KLEVI,1,-1
  DO JL=KIDIA,KFDIA
    IF (LLDOICE(JL)) THEN
      PTIA5(JL,JK)=PTIAM1M5(JL,JK)*ZCONS2+ZTIA5(JL,JK)
      ZPTIA5(JL,JK)=PTIA5(JL,JK)
      IF (PTIA5(JL,JK)>RTT) THEN
        PTIA5(JL,JK)=RTT
      ENDIF
    ELSEIF (LDLAND(JL)) THEN
      PTIA5(JL,JK)=RTT
    ELSE
      PTIA5(JL,JK)=RTFREEZSICE ! keep the same value as srfi for safety.
                              ! it should be anyhow overwrite afterwards.
    ENDIF
    ! 6.1 Compute amount of ice temperature flux to the soil underneath.
    !     this is scaled by gridbox fraction as for the snowpack and passed to srft.
    IF (JK==KLEVI)THEN
      PGICE5(JL)=PCIL(JL)*RCONDSICE*(PTIA5(JL,KLEVI)-PTSOIL5(JL))
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


!*         0.5. New temperatures
!               ----------------
DO JK=KLEVI,1,-1
  DO JL=KIDIA,KFDIA
    IF (JK==KLEVI)THEN
      PTIA(JL,KLEVI) = PTIA(JL,KLEVI) + PGICE(JL)*PCIL(JL)*RCONDSICE
      PTSOIL(JL)     = PTSOIL(JL) - PGICE(JL)*PCIL(JL)*RCONDSICE
    ENDIF
    IF (LLDOICE(JL)) THEN
        IF (ZPTIA5(JL,JK)>RTT) THEN
            PTIA(JL,JK)=0.0_JPRB
        ENDIF
        ZTIA(JL,JK)=ZTIA(JL,JK)+PTIA(JL,JK)
        PTIAM1M(JL,JK)=PTIAM1M(JL,JK)+PTIA(JL,JK)*ZCONS2
    ELSEIF (LDLAND(JL)) THEN
        PTIA(JL,JK)=0.0_JPRB
    ELSE
        PTIA(JL,JK)=0.0_JPRB
    ENDIF
  ENDDO
ENDDO

!*         0.4. Call tridiagonal solver
!               -----------------------


  CALL SRFWDIFSAD(KIDIA,KFDIA,KLON,KLEVI,PTIAM1M5,ZLST5,ZRHS5,ZCDZ5,ZTIA5,&
   & LLDOICE,LLALLAYS,YDSOIL,&
   & PTIAM1M,ZLST,ZRHS,ZCDZ,ZTIA)

!*         0.3. Compute net heat flux at the surface.
!               -------------------------------------

  IF (LLALLAYS) THEN
!     Layers KLEVI
    JK=KLEVI
    DO JL=KIDIA,KFDIA
      IF (LLDOICE(JL)) THEN
        ZLST(JL,JK) = ZLST(JL,JK)+(PTSOIL(JL)/RSIMP)*ZRHS(JL,JK)/ZCDZ5(JL,JK)
        ZCDZ(JL,JK) = ZCDZ(JL,JK)-(PTSOIL(JL)/RSIMP)*ZLST5(JL,JK)*ZRHS(JL,JK) &
         & /ZCDZ5(JL,JK)**2
        ZRHS(JL,JK) = 0.0_JPRB
        ZCDZ(JL,JK) = 0.0_JPRB
        ZLST(JL,JK) = 0.0_JPRB
      ENDIF
    ENDDO

!     Layers 2 to KLEVI-1
    DO JK=KLEVI-1,2,-1
      DO JL=KIDIA,KFDIA
        IF (LLDOICE(JL)) THEN
          ZCDZ(JL,JK) = 0.0_JPRB
          ZLST(JL,JK) = 0.0_JPRB 
        ENDIF
      ENDDO
    ENDDO
  ENDIF

!     Layer 1

JK=1
DO JL=KIDIA,KFDIA
  IF (LLDOICE(JL)) THEN
    ZSURFL(JL) = ZSURFL(JL)+PTMST*ZRHS(JL,JK)/ZCDZ5(JL,JK)
    ZCDZ(JL,JK) = ZCDZ(JL,JK)-PTMST*ZSURFL5(JL)*ZRHS(JL,JK)/ZCDZ5(JL,JK)**2
    ZRHS(JL,JK) = 0.0_JPRB 
    ZCDZ(JL,JK) = 0.0_JPRB
    ZLST(JL,JK) = 0.0_JPRB
  ENDIF
ENDDO

PGICE(KIDIA:KFDIA)=0.0_JPRB


!*         0.2 Compute net heat flux at the surface.
!              ----------
DO JL=KIDIA,KFDIA
  IF (LLDOICE(JL)) THEN
     ZSSRFL=0._JPRB
     ZSLRFL=0._JPRB
     ZTHFL=0._JPRB

     PGSNICE(JL)=PGSNICE(JL)+ZSURFL(JL)/PCIL(JL)
     ZSSRFL=ZSSRFL+ZSURFL(JL)/PCIL(JL)
     ZSLRFL=ZSLRFL+ZSURFL(JL)/PCIL(JL)
     ZTHFL=ZTHFL+ZSURFL(JL)/PCIL(JL)

     PAHFSTI(JL,2)=PAHFSTI(JL,2)+ZTHFL*PFRTI(JL,2)
     PEVAPTI(JL,2)=PEVAPTI(JL,2)+ZTHFL*PFRTI(JL,2)*RLSTT
     PSLRFL(JL)=PSLRFL(JL)+ZSLRFL*PFRTI(JL,2)
     PSSRFLTI(JL,2)=PSSRFLTI(JL,2)+ZSSRFL*PFRTI(JL,2)
     
     ZSURFL(JL)=0.0_JPRB
  ELSE
     ZSURFL(JL)=0.0_JPRB
  ENDIF

ENDDO

DO JK=1,KLEVI
  DO JL=KIDIA,KFDIA
  ! order of call in surftstpsad is important:
  ! srfilsad is after srfisad, so all ptia values can be set to zero.
    !IF (LLDOICE(JL)) THEN
      PTIA(JL,JK) = 0.0_JPRB
      ZTIA(JL,JK) = 0.0_JPRB
    !ENDIF

    ZRHS(JL,JK) = 0.0_JPRB
    ZCDZ(JL,JK) = 0.0_JPRB
    ZLST(JL,JK) = 0.0_JPRB
  ENDDO
ENDDO


END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SRFILSAD_MOD:SRFILSAD',1,ZHOOK_HANDLE)

END SUBROUTINE SRFILSAD
END MODULE SRFILSAD_MOD

