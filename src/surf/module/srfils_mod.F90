MODULE SRFILS_MOD
CONTAINS
SUBROUTINE SRFILS(KIDIA  , KFDIA  , KLON , KLEVI  ,LDLAND, PCIL,&
 & PTMST  ,PTIAM1M ,PFRTI , PAHFSTI, PEVAPTI,PGSNICE,&
 & PSLRFL ,PSSRFLTI, PTSOIL, LDICE  , LDNH   ,&
 & YDCST  ,YDSOIL  ,&
 & PTIA   ,PGICE)

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_CST  , ONLY : TCST
USE YOS_SOIL , ONLY : TSOIL

USE SRFWDIFS_MOD

! (C) Copyright 2025- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SRFILS* - Computes temperature changes in land ice (simplified)

!     PURPOSE.
!     --------
!**   Computes temperature evolution of land ice  
!**   INTERFACE.
!     ----------
!          *SRFILS* IS CALLED FROM *SURF*.

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
!    *PTIAM1M*    ICE TEMPERATURE                            K
!    *PSLRFL*     NET LONGWAVE  RADIATION AT THE SURFACE        W/M**2
!    *PFRTI*      TILE FRACTIONS                              (0-1)
!            1 : WATER                  5 : SNOW ON LOW-VEG+BARE-SOIL
!            2 : ICE                    6 : DRY SNOW-FREE HIGH-VEG
!            3 : WET SKIN               7 : SNOW UNDER HIGH-VEG
!            4 : DRY SNOW-FREE LOW-VEG  8 : BARE SOIL
!            9 : LAKE                  10 : URBAN
!    *PAHFSTI*    TILE SURFACE SENSIBLE HEAT FLUX                 W/M2
!    *PEVAPTI*    TILE SURFACE MOISTURE FLUX                     KG/M2/S
!    *PSSRFLTI*   TILE NET SHORTWAVE RADIATION FLUX AT SURFACE    W/M2
!    *PGSN* .     SNOW basal heat flux between snow and ice       W/M2
!     UPDATED PARAMETERS AT T+1 (UNFILTERED,REAL):
!    *PTIA*       ICE TEMPERATURE                               K
!    *PGICE*      BASAL HEAT FLUX FROM LAND ICE  to soil          K

!     METHOD.
!     -------
!          Parameters are set and the tridiagonal solver is called.

!     EXTERNALS.
!     ----------
!     *SRFWDIF*

!     REFERENCE.
!     ----------
!          See documentation.
!     G. Arduini                2024 Adapted from SRFIS

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
REAL(KIND=JPRB),    INTENT(IN)   :: PTIAM1M(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PFRTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PAHFSTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSLRFL(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PGSNICE(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSRFLTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSOIL(:)
LOGICAL,            INTENT(IN)   :: LDICE(:)
LOGICAL,            INTENT(IN)   :: LDNH(:)
TYPE(TCST),         INTENT(IN)   :: YDCST
TYPE(TSOIL),        INTENT(IN)   :: YDSOIL
REAL(KIND=JPRB),    INTENT(OUT)  :: PTIA(:,:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PGICE(:)

!      LOCAL VARIABLES

REAL(KIND=JPRB) :: ZSURFL(KLON)
REAL(KIND=JPRB) :: ZRHS(KLON,KLEVI), ZCDZ(KLON,KLEVI),&
 & ZLST(KLON,KLEVI),&
 & ZTIA(KLON,KLEVI)  
REAL(KIND=JPRB) :: ZDAI(KLON,KLEVI)
REAL(KIND=JPRB) :: ZDARLICE
REAL(KIND=JPRB) :: ZCSN_I

REAL(KIND=JPRB) :: ZEPSILON
LOGICAL :: LLDOICE(KLON)
LOGICAL ::LLALLAYS

INTEGER(KIND=JPIM) :: JK, JL

REAL(KIND=JPRB) :: ZCONS1, ZCONS2, ZSLRFL, ZSSRFL, ZTHFL, ZTMST
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!*         1. SET UP SOME CONSTANTS.
!             --- -- ---- ----------

!*    PHYSICAL CONSTANTS.
!     -------- ----------


IF (LHOOK) CALL DR_HOOK('SRFILS_MOD:SRFILS',0,ZHOOK_HANDLE)
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
  LLDOICE(JL)=LDICE(JL) 

  IF (LLDOICE(JL)) THEN
    ! This need to be weighted properly:
      ZSSRFL=PFRTI(JL,2)*PSSRFLTI(JL,2)
      ZSLRFL=PFRTI(JL,2)*PSLRFL(JL)
      ZTHFL=PFRTI(JL,2)*PAHFSTI(JL,2)+RLSTT*PFRTI(JL,2)*PEVAPTI(JL,2)
    ! PGSNICE(JL) only applies to the fraction of snow over the ice fraction.
      IF (PCIL(JL) > ZEPSILON) THEN
        ZSURFL(JL)=(PGSNICE(JL)+ZSSRFL+ZSLRFL+ZTHFL)/PCIL(JL)
      ELSE
        ZSURFL(JL)=(PGSNICE(JL)+ZSSRFL+ZSLRFL+ZTHFL)/ZEPSILON
      ENDIF
  ELSE
    ZSURFL(JL)=0.0_JPRB
  ENDIF
ENDDO

!*         3. Set arrays
!             ----------

PGICE(KIDIA:KFDIA)=0.0_JPRB
!     Layer 1

JK=1
DO JL=KIDIA,KFDIA
  IF (LLDOICE(JL)) THEN
    ZLST(JL,JK)=ZCONS1*RCONDSICE/(ZDAI(JL,JK)+ZDAI(JL,JK+1))
    ZCDZ(JL,JK)=RRCSICE*ZDAI(JL,JK)
    ZRHS(JL,JK)=PTMST*ZSURFL(JL)/ZCDZ(JL,JK)
  ENDIF
ENDDO

!     Layers 2 to KLEVI-1

DO JK=2,KLEVI-1
  DO JL=KIDIA,KFDIA
    IF (LLDOICE(JL)) THEN
      ZLST(JL,JK)=ZCONS1*RCONDSICE/(ZDAI(JL,JK)+ZDAI(JL,JK+1))
      ZCDZ(JL,JK)=RRCSICE*ZDAI(JL,JK)
      ZRHS(JL,JK)=0.0_JPRB
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
      ZLST(JL,JK)=ZCONS1*RCONDSICE/(2._JPRB*ZDAI(JL,JK))
      ZCDZ(JL,JK)=RRCSICE*ZDAI(JL,JK)
      ZRHS(JL,JK)=(PTSOIL(JL)/RSIMP)*ZLST(JL,JK)/ZCDZ(JL,JK)
  ENDIF
ENDDO

!*         4. Call tridiagonal solver
!             -----------------------

CALL SRFWDIFS(KIDIA,KFDIA,KLON,KLEVI,PTIAM1M,ZLST,ZRHS,ZCDZ,&
 &            ZTIA,LLDOICE,LLALLAYS,YDSOIL)

!*         5. New temperatures
!             ----------------

DO JK=1,KLEVI
  DO JL=KIDIA,KFDIA
    IF (LLDOICE(JL)) THEN
      PTIA(JL,JK)=PTIAM1M(JL,JK)*ZCONS2+ZTIA(JL,JK)
      IF (PTIA(JL,JK) > RTT) THEN
        PTIA(JL,JK)=RTT
      ENDIF
    ELSEIF (LDLAND(JL)) THEN
      PTIA(JL,JK)=RTT
    ELSE
      PTIA(JL,JK)=RTFREEZSICE ! keep the same value as srfi for safety.
                              ! it should be anyhow overwrite afterwards.
    ENDIF
    ! 6.1 Compute amount of ice temperature flux to the soil underneath.
    !     this is scaled by gridbox fraction as for the snowpack and passed to srft.
    IF (JK==KLEVI)THEN
      PGICE(JL)=PCIL(JL)*RCONDSICE*(PTIA(JL,KLEVI)-PTSOIL(JL))
    ENDIF
  ENDDO
ENDDO
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SRFILS_MOD:SRFILS',1,ZHOOK_HANDLE)

END SUBROUTINE SRFILS
END MODULE SRFILS_MOD
