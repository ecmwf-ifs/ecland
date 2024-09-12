MODULE SRFI_MOD
CONTAINS
SUBROUTINE SRFI(KIDIA  , KFDIA  , KLON , KLEVS  , KLEVI  ,LDLAND, PCIL, &
 & PTMST  ,PFRTI ,PTIAM1M , PAHFSTI, PEVAPTI, PGSN,&
 & PSLRFL ,PSSRFLTI, LDICE  , LDNH   ,&
 & LNEMOICETHK, PTHKICE, &
 & YDCST  ,YDSOIL  ,&
 & PTIA   ,PDHTIS)  
 
USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_CST  , ONLY : TCST
USE YOS_SOIL , ONLY : TSOIL

USE SRFWDIF_MOD

! (C) Copyright 1999- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SRFI* - Computes temperature changes in soil.

!     PURPOSE.
!     --------
!**   Computes temperature evolution of sea ice  
!**   INTERFACE.
!     ----------
!          *SRFI* IS CALLED FROM *SURF*.

!     PARAMETER   DESCRIPTION                                    UNITS
!     ---------   -----------                                    -----
!     INPUT PARAMETERS (INTEGER):
!    *KIDIA*      START POINT
!    *KFDIA*      END POINT
!    *KLON*       NUMBER OF GRID POINTS PER PACKET
!    *KLEVS*      NUMBER OF SOIL LAYERS
!    *KTILES*     NUMBER OF SURFACE TILES
!    *KLEVI*      Number of sea ice layers (diagnostics)
!    *KDHVTIS*    Number of variables for sea ice energy budget
!    *KDHFTIS*    Number of fluxes for sea ice energy budget

!     INPUT PARAMETERS (REAL):
!    *PTMST*      TIME STEP                                      S

!     INPUT PARAMETERS (LOGICAL):
!    *LDICE*      ICE MASK (TRUE for sea ice)
!    *LDNH*       TRUE FOR NORTHERN HEMISPHERE

!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!    *PTIAM1M*    SEA ICE TEMPERATURE                            K
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
!     UPDATED PARAMETERS AT T+1 (UNFILTERED,REAL):
!    *PTIA*       SOIL TEMPERATURE                               K

!     OUTPUT PARAMETERS (DIAGNOSTIC):
!    *PDHTIS*     Diagnostic array for ice T (see module yomcdh)

!     METHOD.
!     -------
!          Parameters are set and the tridiagonal solver is called.

!     EXTERNALS.
!     ----------
!     *SRFWDIF*

!     REFERENCE.
!     ----------
!          See documentation.
!     P.VITERBO/A.BELJAARS      E.C.M.W.F.     15/03/1999
!     Modified P. Viterbo       17-05-2000  Surface DDH for TILES
!     Modified J.F. Estrade *ECMWF* 03-10-01 move in surf vob
!              G. Arduini       Jan 2024     Variable ice thickness
!              G. Arduini       Sept 2024 Land-ice fraction 
!     ------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM), INTENT(IN)   :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KLON
INTEGER(KIND=JPIM), INTENT(IN)   :: KLEVS
INTEGER(KIND=JPIM), INTENT(IN)   :: KLEVI
LOGICAL, INTENT(IN)   :: LDLAND(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PCIL(:)

REAL(KIND=JPRB),    INTENT(IN)   :: PTMST
REAL(KIND=JPRB),    INTENT(IN)   :: PTIAM1M(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PFRTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PAHFSTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSLRFL(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSRFLTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PGSN(:) ! introduced PGSN for snow over seaice
LOGICAL,            INTENT(IN)   :: LDICE(:)
LOGICAL,            INTENT(IN)   :: LDNH(:)
LOGICAL,            INTENT(IN)   :: LNEMOICETHK
REAL(KIND=JPRB),    INTENT(IN)   :: PTHKICE(:)
TYPE(TCST),         INTENT(IN)   :: YDCST
TYPE(TSOIL),        INTENT(IN)   :: YDSOIL
REAL(KIND=JPRB),    INTENT(OUT)  :: PTIA(:,:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PDHTIS(:,:,:)

!      LOCAL VARIABLES

REAL(KIND=JPRB) :: ZSURFL(KLON)
REAL(KIND=JPRB) :: ZRHS(KLON,KLEVS), ZCDZ(KLON,KLEVS),&
 & ZLST(KLON,KLEVS),&
 & ZTIA(KLON,KLEVS)  
REAL(KIND=JPRB) :: ZDAI(KLON,KLEVS)
REAL(KIND=JPRB) :: ZDARLICE
REAL(KIND=JPRB) :: ZCSN_I

REAL(KIND=JPRB) :: ZEPSILON
LOGICAL :: LLDOICE(KLON)
REAL(KIND=JPRB) :: ZRES
REAL(KIND=JPRB) :: ZTHICK,ZTHICK2
REAL(KIND=JPRB) :: ZEPSICE
INTEGER(KIND=JPIM) :: ZKLEVI

INTEGER(KIND=JPIM) :: JK, JL

REAL(KIND=JPRB) :: ZCONS1, ZCONS2, ZSLRFL, ZSSRFL, ZTHFL, ZTMST
REAL(KIND=JPRB) :: ZTHICKICE_ENERGY
REAL(KIND=JPRB) :: ZTMP0
REAL(KIND=JPRB) :: ZEPSILON
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!*         1. SET UP SOME CONSTANTS.
!             --- -- ---- ----------

!*    PHYSICAL CONSTANTS.
!     -------- ----------

IF (LHOOK) CALL DR_HOOK('SRFI_MOD:SRFI',0,ZHOOK_HANDLE)
ASSOCIATE(RLSTT=>YDCST%RLSTT, RTT=>YDCST%RTT, RLMLT=>YDCST%RLMLT, &
 & RCONDSICE=>YDSOIL%RCONDSICE, RDAI=>YDSOIL%RDAI, RDANSICE=>YDSOIL%RDANSICE, &
 & RDARSICE=>YDSOIL%RDARSICE, RRCSICE=>YDSOIL%RRCSICE, RSIMP=>YDSOIL%RSIMP, &
 & RTFREEZSICE=>YDSOIL%RTFREEZSICE, RTMELTSICE=>YDSOIL%RTMELTSICE, &
 & RHOCI=>YDSOIL%RHOCI,RHOICE=>YDSOIL%RHOICE)
DO JK=1,KLEVS-1
  DO JL=KIDIA,KFDIA
    ZDAI(JL,JK)=RDAI(JK)
  ENDDO
ENDDO


! Ice thickness coupling:
! We always compute temperature evo on 4 levels. 
! Layer thickness is updated based on total ice layer depth and a minimum
! of 0.07m thickness per layer is mantained.
ZKLEVI=KLEVS
ZEPSICE=0.01_JPRB

DO JL=KIDIA,KFDIA
  !Limit ice thickness to 0.5m
  ZTHICKICE_ENERGY=MAX(0.28_JPRB, MIN(1.5_JPRB, PTHKICE(JL)))
  IF (LNEMOICETHK) THEN
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

ZTMST=1.0_JPRB/PTMST
ZCONS1=PTMST*RSIMP*2.0_JPRB
ZCONS2=1.0_JPRB-1.0_JPRB/RSIMP
ZEPSILON=EPSILON(ZEPSILON)

!*         2. Compute net heat flux at the surface.
!             -------------------------------------

DO JL=KIDIA,KFDIA
  LLDOICE(JL)=LDICE(JL)
  IF (LLDOICE(JL)) THEN
    ZSSRFL=PSSRFLTI(JL,2)
    ZSLRFL=PSLRFL(JL)
    ZTHFL=PAHFSTI(JL,2)+RLSTT*PEVAPTI(JL,2)
    ZSURFL(JL)=ZSSRFL+ZSLRFL+ZTHFL
    IF (YDSOIL%LESNICE) THEN
      ZSSRFL=PSSRFLTI(JL,2)*PFRTI(JL,2)
      ZSLRFL=PSLRFL(JL)*PFRTI(JL,2)
      ZTHFL=PAHFSTI(JL,2)*PFRTI(JL,2)+RLSTT*PEVAPTI(JL,2)*PFRTI(JL,2)
      ! PGSN(JL) is already smeared out over the entire grid square.
      ZSURFL(JL)=(PGSN(JL)+ZSSRFL+ZSLRFL+ZTHFL)/MAX(ZEPSILON,(PFRTI(JL,2)+PFRTI(JL,5)))
    ENDIF
  ELSE
    ZSURFL(JL)=0.0_JPRB
  ENDIF
ENDDO

!*         3. Set arrays
!             ----------

!     Layer 1

JK=1
DO JL=KIDIA,KFDIA
  IF (LLDOICE(JL)) THEN
    ZLST(JL,JK)=ZCONS1*RCONDSICE/(ZDAI(JL,JK)+ZDAI(JL,JK+1))
    ZCDZ(JL,JK)=RRCSICE*ZDAI(JL,JK)
    ZRHS(JL,JK)=PTMST*ZSURFL(JL)/ZCDZ(JL,JK)
  ENDIF
ENDDO

!     Layers 2 to KLEVS-1

DO JK=2,KLEVS-1
  DO JL=KIDIA,KFDIA
    IF (LLDOICE(JL)) THEN
      ZLST(JL,JK)=ZCONS1*RCONDSICE/(ZDAI(JL,JK)+ZDAI(JL,JK+1))
      ZCDZ(JL,JK)=RRCSICE*ZDAI(JL,JK)
      ZRHS(JL,JK)=0.0_JPRB
    ENDIF
  ENDDO
ENDDO

!     Layers KLEVS

JK=KLEVS
DO JL=KIDIA,KFDIA
  IF (LLDOICE(JL)) THEN
    ZLST(JL,JK)=ZCONS1*RCONDSICE/(2.*ZDAI(JL,JK))
    ZCDZ(JL,JK)=RRCSICE*ZDAI(JL,JK)
    ZRHS(JL,JK)=(RTFREEZSICE/RSIMP)*ZLST(JL,JK)/ZCDZ(JL,JK)
  ENDIF
ENDDO

!*         4. Call tridiagonal solver
!             -----------------------

CALL SRFWDIF(KIDIA,KFDIA,KLON,KLEVS,PTIAM1M,ZLST,ZRHS,ZCDZ,YDSOIL,ZTIA,LLDOICE)

!*          5. DDH diagnostics
!              ---------------

IF (SIZE(PDHTIS) > 0) THEN
  DO JL=KIDIA,KFDIA
    IF (LLDOICE(JL)) THEN
! Sensible heat flux
      PDHTIS(JL,1,9)=PFRTI(JL,2)*PAHFSTI(JL,2)
! Latent heat flux
      PDHTIS(JL,1,10)=RLSTT*PFRTI(JL,2)*PEVAPTI(JL,2)
    ELSE
      PDHTIS(JL,1,9)=0.0_JPRB
      PDHTIS(JL,1,10)=0.0_JPRB
      PDHTIS(JL,1,13)=0.0_JPRB
    ENDIF
  ENDDO
  
  DO JK=2,KLEVI
    DO JL=KIDIA,KFDIA
      PDHTIS(JL,JK,9)=0.0_JPRB
      PDHTIS(JL,JK,10)=0.0_JPRB
    ENDDO
  ENDDO
! Flux ice-soil
  DO JK=1,KLEVI
    DO JL=KIDIA,KFDIA
      PDHTIS(JL,JK,13)=0.0_JPRB
    ENDDO
  ENDDO

  DO JK=1,KLEVI
    DO JL=KIDIA,KFDIA
      IF (LLDOICE(JL)) THEN
! Heat capacity per unit surface
        PDHTIS(JL,JK,1)=RRCSICE*ZDAI(JL,JK)
! Soil temperature
        PDHTIS(JL,JK,2)=PTIAM1M(JL,JK)
! Layer energy per unit surface
        PDHTIS(JL,JK,3)=RRCSICE*ZDAI(JL,JK)*PTIAM1M(JL,JK)
! Layer depth
        PDHTIS(JL,JK,4)=ZDAI(JL,JK)
      ELSE
        PDHTIS(JL,JK,1:4)=0.0_JPRB
      ENDIF
    ENDDO
  ENDDO
! Ice water phase changes
  DO JK=1,KLEVI
    DO JL=KIDIA,KFDIA
      PDHTIS(JL,JK,12)=0.0_JPRB
    ENDDO
  ENDDO

  DO JK=1,KLEVI-1
    DO JL=KIDIA,KFDIA
      IF (LLDOICE(JL)) THEN
! Ground heat flux
        PDHTIS(JL,JK,11)=RSIMP*(ZTIA(JL,JK)-ZTIA(JL,JK+1))*ZLST(JL,JK)*ZTMST
      ELSE
        PDHTIS(JL,JK,11)=0.0_JPRB
      ENDIF
    ENDDO
  ENDDO
  DO JL=KIDIA,KFDIA
    IF (LLDOICE(JL)) THEN
      PDHTIS(JL,KLEVS,11)=RSIMP*(ZTIA(JL,KLEVS)-RTFREEZSICE)*ZLST(JL,JK)*ZTMST
    ELSE
      PDHTIS(JL,KLEVS,11)=0.0_JPRB
    ENDIF
  ENDDO
ENDIF

!*         6. New temperatures
!             ----------------

DO JK=1,KLEVS
  DO JL=KIDIA,KFDIA
    IF (LLDOICE(JL)) THEN
      PTIA(JL,JK)=PTIAM1M(JL,JK)*ZCONS2+ZTIA(JL,JK)
      PTIA(JL,JK)=MIN(PTIA(JL,JK),RTMELTSICE)
    ELSEIF (.not. LDLAND(JL)) THEN
      PTIA(JL,JK)=RTFREEZSICE
    ENDIF
  ENDDO
ENDDO
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SRFI_MOD:SRFI',1,ZHOOK_HANDLE)

END SUBROUTINE SRFI
END MODULE SRFI_MOD
