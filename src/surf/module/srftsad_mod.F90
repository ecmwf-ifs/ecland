MODULE SRFTSAD_MOD
CONTAINS
SUBROUTINE SRFTSAD(KIDIA  , KFDIA  , KLON   , KLEVS ,&
 & PTMST  , PTSAM1M5 , PWSAM1M5, KSOTY, &
 & PFRTI  , PAHFSTI5 , PEVAPTI5,&
 & PSLRFL5, PSSRFLTI5, PGSN5   ,&
 & PCTSA5 , PTSA5    , LDLAND  ,&
 & YDCST  , YDSOIL   , YDFLAKE ,YDURB,&  
 & PTSAM1M, PWSAM1M  ,&
 & PAHFSTI, PEVAPTI  ,&
 & PSLRFL , PSSRFLTI , PGSN    ,&
 & PCTSA  , PTSA &
 & )
  
USE PARKIND1  , ONLY : JPIM, JPRB
USE YOMHOOK   , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_CST   , ONLY : TCST
USE YOS_SOIL  , ONLY : TSOIL
USE YOS_FLAKE , ONLY : TFLAKE
USE YOS_URB   , ONLY : TURB

USE SRFWDIFSAD_MOD

#ifdef DOC

! (C) Copyright 2012- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SRFTSAD* - Computes temperature changes in soil.
!                 (Adjoint)

!     PURPOSE.
!     --------
!**   Computes temperature changes in soil due to 
!**   surface heat flux and diffusion.  

!**   INTERFACE.
!     ----------
!          *SRFTSAD* IS CALLED FROM *SURFTSTPSAD*.

!     PARAMETER   DESCRIPTION                                         UNITS
!     ---------   -----------                                         -----

!     INPUT PARAMETERS (INTEGER):
!     *KIDIA*      START POINT
!     *KFDIA*      END POINT
!     *KLON*       NUMBER OF GRID POINTS PER PACKET
!     *KLEVS*      NUMBER OF SOIL LAYERS
!     *KSOTY*      SOIL TYPE                                           (1-7)

!     INPUT PARAMETERS (LOGICAL):
!     *LDLAND*     LAND/SEA MASK (TRUE/FALSE)

!     INPUT PARAMETERS (REAL):
!     *PTMST*      TIME STEP                                           s
!     *PFRTI*      TILE FRACTIONS                                      (0-1)
!            1 : WATER                  5 : SNOW ON LOW-VEG+BARE-SOIL
!            2 : ICE                    6 : DRY SNOW-FREE HIGH-VEG
!            3 : WET SKIN               7 : SNOW UNDER HIGH-VEG
!            4 : DRY SNOW-FREE LOW-VEG  8 : BARE SOIL

!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!  Trajectory  Perturbation  Description                                Unit
!  PTSAM1M5    PTSAM1M       SOIL TEMPERATURE                           K
!  PWSAM1M5    PWSAM1M       SOIL MOISTURE                              m3/m3
!  PSLRFL5     PSLRFL        NET LONGWAVE  RADIATION AT THE SURFACE     W/m2
!  PGSN5       PGSN          GROUND HEAT FLUX FROM SNOW DECK TO SOIL    W/m2
!  PCTSA5      PCTSA         VOLUMETRIC HEAT CAPACITY                   J/K/m3
!  PAHFSTI5    PAHFSTI       TILE SURFACE SENSIBLE HEAT FLUX            W/m2
!  PEVAPTI5    PEVAPTI       TILE SURFACE MOISTURE FLUX                 kg/m2/s
!  PSSRFLTI5   PSSRFLTI      TILE NET SHORTWAVE RADIATION FLUX          W/m2
!                            AT SURFACE

!     UPDATED PARAMETERS AT T+1 (UNFILTERED,REAL):
!  Trajectory  Perturbation  Description                                Unit
!  PTSA5       PTSA          SOIL TEMPERATURE                           K

!     METHOD.
!     -------

!          Parameters are set and the tridiagonal solver is called.

!     EXTERNALS.
!     ----------
!     *SRFWDIFS*
!     *SRFWDIFSAD*

!     REFERENCE.
!     ----------
!          See documentation.

!     Original   
!     --------
!     M. Janiskova              E.C.M.W.F.     02-04-2012 

!     Modifications
!     -------------
!     J. McNorton           24/08/2022  urban tile

!     ------------------------------------------------------------------
#endif

IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM), INTENT(IN)   :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KLON
INTEGER(KIND=JPIM), INTENT(IN)   :: KLEVS
INTEGER(KIND=JPIM), INTENT(IN)   :: KSOTY(:)

LOGICAL,            INTENT(IN)   :: LDLAND(:)

TYPE(TCST),         INTENT(IN)   :: YDCST
TYPE(TSOIL),        INTENT(IN)   :: YDSOIL
TYPE(TFLAKE),       INTENT(IN)   :: YDFLAKE
TYPE(TURB),         INTENT(IN)   :: YDURB

REAL(KIND=JPRB),    INTENT(IN)   :: PTMST
REAL(KIND=JPRB),    INTENT(IN)   :: PFRTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSAM1M5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PWSAM1M5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PAHFSTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSLRFL5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSRFLTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PGSN5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PCTSA5(:,:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PTSA5(:,:)

REAL(KIND=JPRB),    INTENT(INOUT) :: PTSAM1M(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PWSAM1M(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PAHFSTI(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PEVAPTI(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PSLRFL(:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PSSRFLTI(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PGSN(:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PCTSA(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PTSA(:,:)

!      LOCAL VARIABLES

REAL(KIND=JPRB) :: ZSURFL5(KLON)
REAL(KIND=JPRB) :: ZDIF5(KLON,KLEVS),ZLST5(KLON,KLEVS)
REAL(KIND=JPRB) :: ZCDZ5(KLON,KLEVS),ZRHS5(KLON,KLEVS),ZTSA5(KLON,KLEVS)
REAL(KIND=JPRB) :: ZLWT5(KLON,KLEVS),ZLIC5(KLON,KLEVS)
REAL(KIND=JPRB) :: ZLAMBDASAT5(KLON,KLEVS),ZCOND5(KLON,KLEVS)
REAL(KIND=JPRB) :: ZKERSTEN5(KLON,KLEVS)
REAL(KIND=JPRB) :: ZSSRFL5,ZSLRFL5,ZTHFL5,ZWU5

REAL(KIND=JPRB) :: ZSURFL(KLON)
REAL(KIND=JPRB) :: ZRHS(KLON,KLEVS), ZCDZ(KLON,KLEVS),&
 & ZLST(KLON,KLEVS), ZDIF(KLON,KLEVS),&
 & ZTSA(KLON,KLEVS)  
REAL(KIND=JPRB) :: ZINVWSAT(KLON)
REAL(KIND=JPRB) :: ZCONS1, ZCONS2, ZSLRFL, ZSSRFL, ZTHFL,&
 & ZFF, ZWU, ZLIC, ZLWT, ZLAMBDASAT, ZKERSTEN, ZCOND, ZLN10

INTEGER(KIND=JPIM) :: JK, JL, JS, IKLEVS
LOGICAL ::LLALLAYS

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

! -------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SRFTSAD_MOD:SRFTSAD',0,ZHOOK_HANDLE)
ASSOCIATE(RLVTT=>YDCST%RLVTT, &
 & LEFLAKE=>YDFLAKE%LEFLAKE,LEURBAN=>YDURB%LEURBAN, &
 & LEVGEN=>YDSOIL%LEVGEN, RDAT=>YDSOIL%RDAT, RFRSMALL=>YDSOIL%RFRSMALL, &
 & RKERST1=>YDSOIL%RKERST1, RKERST2=>YDSOIL%RKERST2, RKERST3=>YDSOIL%RKERST3, &
 & RLAMBDADRY=>YDSOIL%RLAMBDADRY, RLAMBDADRYM=>YDSOIL%RLAMBDADRYM, &
 & RLAMBDAICE=>YDSOIL%RLAMBDAICE, RLAMBDAWAT=>YDSOIL%RLAMBDAWAT, &
 & RLAMSAT1=>YDSOIL%RLAMSAT1, RLAMSAT1M=>YDSOIL%RLAMSAT1M, RSIMP=>YDSOIL%RSIMP, &
 & RWSAT=>YDSOIL%RWSAT, RWSATM=>YDSOIL%RWSATM)

!*    0. INITIALIZATION
!     ------------------

ZLN10 = LOG(10.0_JPRB)

DO JL=KIDIA,KFDIA
  ZSURFL5(JL) = 0.0_JPRB
ENDDO

DO JK=1,KLEVS
  DO JL=KIDIA,KFDIA
    ZDIF5(JL,JK) = 0.0_JPRB
    ZLST5(JL,JK) = 0.0_JPRB
    ZCDZ5(JL,JK) = 0.0_JPRB
    ZRHS5(JL,JK) = 0.0_JPRB
  ENDDO
ENDDO

!* Computation done for only top or all soil layers

LLALLAYS = .TRUE.    ! done for all layers
!LLALLAYS = .FALSE.   ! done for top layer only

IF (LLALLAYS) THEN
  IKLEVS = KLEVS
ELSE
  IKLEVS = 1
ENDIF

!*         1. SET UP SOME CONSTANTS.
!             --- -- ---- ----------
!*    COMPUTATIONAL CONSTANTS.
!     ------------- ----------

ZCONS1=PTMST*RSIMP
ZCONS2=1.0_JPRB-1.0_JPRB/RSIMP

!*         2. Compute net heat flux at the surface.
!             -------------------------------------

DO JL=KIDIA,KFDIA
  IF (LDLAND(JL)) THEN

!         In principle this should be fractional averaging,  
!         but since the land sea mask is used fractions 3,4,5,6
!         7 and 8 add up to 1 for land. PGSN(JL) is already 
!         smeared out over the entire grid square. In future 
!         (when fractional land is used), ZSSRFL+ZSLRFL+ZTHFL 
!         should be divided by the sum of fractions 3,4,6 and 8. 

    ZSSRFL5 = PFRTI(JL,3)*PSSRFLTI5(JL,3)&
     & +PFRTI(JL,4)*PSSRFLTI5(JL,4)&
     & +PFRTI(JL,6)*PSSRFLTI5(JL,6)&
     & +PFRTI(JL,8)*PSSRFLTI5(JL,8)  
    ZSLRFL5 = (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8))*PSLRFL5(JL)

    ZTHFL5 = PFRTI(JL,3)*(PAHFSTI5(JL,3)+RLVTT*PEVAPTI5(JL,3))&
     & +PFRTI(JL,4)*(PAHFSTI5(JL,4)+RLVTT*PEVAPTI5(JL,4))&
     & +PFRTI(JL,6)*(PAHFSTI5(JL,6)+RLVTT*PEVAPTI5(JL,6))&
     & +PFRTI(JL,8)*(PAHFSTI5(JL,8)+RLVTT*PEVAPTI5(JL,8))

    IF ( LEURBAN ) THEN
     ZSSRFL5 = ZSSRFL5+PFRTI(JL,10)*PSSRFLTI5(JL,10)
     ZSLRFL5 = ZSLRFL5+PFRTI(JL,10)*PSLRFL5(JL)
     ZTHFL5  = ZTHFL5+PFRTI(JL,10)*(PAHFSTI5(JL,10)+RLVTT*PEVAPTI5(JL,10))
    ENDIF

    ZSURFL5(JL) = ZSSRFL5+ZSLRFL5+ZTHFL5+PGSN5(JL)

    IF ( LEFLAKE ) THEN
      IF ( PFRTI(JL,9) > RFRSMALL ) THEN
        ZSURFL5(JL) = PGSN5(JL)
        IF ( LEURBAN ) THEN
          IF ( (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8)+PFRTI(JL,10)) > RFRSMALL ) THEN
            ZSURFL5(JL) = PGSN5(JL)+(ZSSRFL5+ZSLRFL5+ZTHFL5) &
            & / (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8)+PFRTI(JL,10))
          ENDIF
        ELSE
         IF ( (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8)) > RFRSMALL ) THEN
           ZSURFL5(JL) = PGSN5(JL)+(ZSSRFL5+ZSLRFL5+ZTHFL5) & 
            & / (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8))
         ENDIF
        ENDIF
      ENDIF
    ENDIF
    
  ELSE
    ZSURFL5(JL) = 0.0_JPRB
  ENDIF
ENDDO

!*         3. Compute exchange coeff. layer by layer
!             --------------------------------------

DO JK=1,IKLEVS
  DO JL=KIDIA,KFDIA
    IF (LDLAND(JL)) THEN
      ZFF = 0.0_JPRB
      ZWU5 = PWSAM1M5(JL,JK)*(1.0_JPRB-ZFF)
      ZLWT5(JL,JK) = RLAMBDAWAT**ZWU5

      IF(LEVGEN)THEN
        JS = KSOTY(JL)
        ZINVWSAT(JL) = 1.0_JPRB/RWSATM(JS)
        ZLIC5(JL,JK) = RLAMBDAICE**(RWSATM(JS)-ZWU5)
        ZLAMBDASAT5(JL,JK) = RLAMSAT1M(JS)*ZLIC5(JL,JK)*ZLWT5(JL,JK)
        IF (PWSAM1M5(JL,JK)*ZINVWSAT(JL) > RKERST1) THEN
          ZCOND5(JL,JK) = PWSAM1M5(JL,JK)*ZINVWSAT(JL)
        ELSE
          ZCOND5(JL,JK) = RKERST1
        ENDIF
        ZKERSTEN5(JL,JK) = RKERST2*LOG10(ZCOND5(JL,JK))+RKERST3
        ZDIF5(JL,JK) = RLAMBDADRYM(JS) &
         & +ZKERSTEN5(JL,JK)*(ZLAMBDASAT5(JL,JK)-RLAMBDADRYM(JS))
      ELSE
        ZINVWSAT(JL) = 1.0_JPRB/RWSAT
        ZLIC5(JL,JK) = RLAMBDAICE**(RWSAT-ZWU5)
        ZLAMBDASAT5(JL,JK) = RLAMSAT1*ZLIC5(JL,JK)*ZLWT5(JL,JK)
        IF (PWSAM1M5(JL,JK)*ZINVWSAT(JL) > RKERST1) THEN
          ZCOND5(JL,JK) = PWSAM1M5(JL,JK)*ZINVWSAT(JL)
        ELSE
          ZCOND5(JL,JK) = RKERST1
        ENDIF
        ZKERSTEN5(JL,JK) = RKERST2*LOG10(ZCOND5(JL,JK))+RKERST3
        ZDIF5(JL,JK) = RLAMBDADRY &
         & +ZKERSTEN5(JL,JK)*(ZLAMBDASAT5(JL,JK)-RLAMBDADRY)
      ENDIF
    ENDIF
  ENDDO
ENDDO

!*         4. Set arrays
!             ----------
!     Layer 1
JK=1
DO JL=KIDIA,KFDIA
  IF (LDLAND(JL)) THEN
    ZLST5(JL,JK) = ZCONS1*(ZDIF5(JL,JK)+ZDIF5(JL,JK+1))/(RDAT(JK)+RDAT(JK+1))
    ZCDZ5(JL,JK) = PCTSA5(JL,JK)*RDAT(JK)
    ZRHS5(JL,JK) = PTMST*ZSURFL5(JL)/ZCDZ5(JL,JK)
  ENDIF
ENDDO

IF (LLALLAYS) THEN

!     Layers 2 to KLEVS-1
  DO JK=2,KLEVS-1
    DO JL=KIDIA,KFDIA
      IF (LDLAND(JL)) THEN
        ZLST5(JL,JK) = ZCONS1*(ZDIF5(JL,JK)+ZDIF5(JL,JK+1)) &
         & /(RDAT(JK)+RDAT(JK+1))
        ZCDZ5(JL,JK) = PCTSA5(JL,JK)*RDAT(JK)
      ENDIF
    ENDDO
  ENDDO

!     Layers KLEVS
  JK=KLEVS
  DO JL=KIDIA,KFDIA
    IF (LDLAND(JL)) THEN
      ZLST5(JL,JK) = 0.0_JPRB
      ZCDZ5(JL,JK) = PCTSA5(JL,JK)*RDAT(JK)
    ENDIF
  ENDDO
ENDIF

!*         5. Call tridiagonal solver
!             -----------------------
!CALL SRFWDIFS(KIDIA,KFDIA,KLON,KLEVS,PTSAM1M5,ZLST5,ZRHS5,ZCDZ5,ZTSA5, &
! & LDLAND,LLALLAYS)
! Not necessary since required ZTSA5 will be computed in SRFWDIFSAD as it
! is not needed before. This will save a computational time.


!          0.  ADJOINT CALCULATIONS
!              --------------------

!* Set local variables to zero

ZSURFL(:) = 0.0_JPRB
ZRHS(:,:) = 0.0_JPRB
ZCDZ(:,:) = 0.0_JPRB
ZLST(:,:) = 0.0_JPRB
ZDIF(:,:) = 0.0_JPRB
ZTSA(:,:) = 0.0_JPRB

!*         0.7. New temperatures
!               ----------------
DO JK=KLEVS,1,-1
  DO JL=KIDIA,KFDIA
    IF (LDLAND(JL)) THEN
      PTSAM1M(JL,JK) = PTSAM1M(JL,JK)+ZCONS2*PTSA(JL,JK)
      ZTSA(JL,JK) = ZTSA(JL,JK)+PTSA(JL,JK)
    ELSE
      PTSAM1M(JL,JK) = PTSAM1M(JL,JK)+PTSA(JL,JK)
    ENDIF
    PTSA(JL,JK) = 0.0_JPRB
  ENDDO
ENDDO

!*         0.5. Call tridiagonal solver
!               -----------------------
CALL SRFWDIFSAD(KIDIA,KFDIA,KLON,KLEVS,PTSAM1M5,ZLST5,ZRHS5,ZCDZ5,ZTSA5, &
 & LDLAND,LLALLAYS,YDSOIL, &
 & PTSAM1M,ZLST,ZRHS,ZCDZ,ZTSA)

!*         7. New temperatures
!             ----------------
! Finishing computation of trajectory requiring ZTSA5 computed by SRFWDIFSAD
DO JK=1,KLEVS
  DO JL=KIDIA,KFDIA
    IF (LDLAND(JL)) THEN
      PTSA5(JL,JK) = PTSAM1M5(JL,JK)*ZCONS2+ZTSA5(JL,JK)
    ELSE
      PTSA5(JL,JK) = PTSAM1M5(JL,JK)
    ENDIF
  ENDDO
ENDDO

!*         0.4. Set arrays
!               ----------

IF (LLALLAYS) THEN

!     Layers KLEVS
  JK=KLEVS
  DO JL=KIDIA,KFDIA
    IF (LDLAND(JL)) THEN
      PCTSA(JL,JK) = PCTSA(JL,JK)+RDAT(JK)*ZCDZ(JL,JK)
      ZCDZ(JL,JK) = 0.0_JPRB
      ZLST(JL,JK) = 0.0_JPRB
    ENDIF
  ENDDO

!     Layers 2 to KLEVS-1
  DO JK=KLEVS-1,2,-1
    DO JL=KIDIA,KFDIA
      IF (LDLAND(JL)) THEN
        PCTSA(JL,JK) = PCTSA(JL,JK)+RDAT(JK)*ZCDZ(JL,JK)
        ZCDZ(JL,JK) = 0.0_JPRB
        ZDIF(JL,JK) = ZDIF(JL,JK)+ZCONS1*ZLST(JL,JK)/(RDAT(JK)+RDAT(JK+1))
        ZDIF(JL,JK+1) = ZDIF(JL,JK+1)+ZCONS1*ZLST(JL,JK)/(RDAT(JK)+RDAT(JK+1))
        ZLST(JL,JK) = 0.0_JPRB
      ENDIF
    ENDDO
  ENDDO
ENDIF

!     Layer 1
JK=1
DO JL=KIDIA,KFDIA
  IF (LDLAND(JL)) THEN
    ZSURFL(JL) = ZSURFL(JL)+PTMST*ZRHS(JL,JK)/ZCDZ5(JL,JK)
    ZCDZ(JL,JK) = ZCDZ(JL,JK)-PTMST*ZSURFL5(JL)*ZRHS(JL,JK)/(ZCDZ5(JL,JK)**2)
    ZRHS(JL,JK) = 0.0_JPRB
    PCTSA(JL,JK) = PCTSA(JL,JK)+RDAT(JK)*ZCDZ(JL,JK)
    ZCDZ(JL,JK) = 0.0_JPRB
    ZDIF(JL,JK) = ZDIF(JL,JK)+ZCONS1*ZLST(JL,JK)/(RDAT(JK)+RDAT(JK+1))
    ZDIF(JL,JK+1) = ZDIF(JL,JK+1)+ZCONS1*ZLST(JL,JK)/(RDAT(JK)+RDAT(JK+1))
    ZLST(JL,JK) = 0.0_JPRB
  ENDIF
ENDDO

!*         0.3. Compute exchange coeff. layer by layer
!               --------------------------------------

DO JK=IKLEVS,1,-1
  DO JL=KIDIA,KFDIA
    IF (LDLAND(JL)) THEN
      ZFF = 0.0_JPRB

      ZWU = 0.0_JPRB
      ZLWT = 0.0_JPRB
      ZLIC = 0.0_JPRB
      ZLAMBDASAT = 0.0_JPRB
      ZCOND = 0.0_JPRB
      ZKERSTEN = 0.0_JPRB

      IF (LEVGEN) THEN
        JS = KSOTY(JL)

        ZKERSTEN = ZKERSTEN+(ZLAMBDASAT5(JL,JK)-RLAMBDADRYM(JS))*ZDIF(JL,JK)
        ZLAMBDASAT = ZLAMBDASAT+ZKERSTEN5(JL,JK)*ZDIF(JL,JK)
        ZDIF(JL,JK) = 0.0_JPRB
        ZCOND = ZCOND+RKERST2*ZKERSTEN/(ZLN10*ZCOND5(JL,JK))
        IF (PWSAM1M5(JL,JK)*ZINVWSAT(JL) > RKERST1) THEN
          PWSAM1M(JL,JK) = PWSAM1M(JL,JK)+ZINVWSAT(JL)*ZCOND
        ENDIF
        ZLWT = ZLWT+RLAMSAT1M(JS)*ZLIC5(JL,JK)*ZLAMBDASAT
        ZLIC = ZLIC+RLAMSAT1M(JS)*ZLWT5(JL,JK)*ZLAMBDASAT
        ZWU = ZWU-ZLIC5(JL,JK)*LOG(RLAMBDAICE)*ZLIC
      ELSE
        ZKERSTEN = ZKERSTEN+(ZLAMBDASAT5(JL,JK)-RLAMBDADRY)*ZDIF(JL,JK)
        ZLAMBDASAT = ZLAMBDASAT+ZKERSTEN5(JL,JK)*ZDIF(JL,JK)
        ZDIF(JL,JK) = 0.0_JPRB
        ZCOND = ZCOND+RKERST2*ZKERSTEN/(ZLN10*ZCOND5(JL,JK))
        IF (PWSAM1M5(JL,JK)*ZINVWSAT(JL) > RKERST1) THEN
          PWSAM1M(JL,JK) = PWSAM1M(JL,JK)+ZINVWSAT(JL)*ZCOND
        ENDIF
        ZLWT = ZLWT+RLAMSAT1*ZLIC5(JL,JK)*ZLAMBDASAT
        ZLIC = ZLIC+RLAMSAT1*ZLWT5(JL,JK)*ZLAMBDASAT
        ZWU = ZWU-ZLIC5(JL,JK)*LOG(RLAMBDAICE)*ZLIC
      ENDIF
      ZWU = ZWU+ZLWT5(JL,JK)*LOG(RLAMBDAWAT)*ZLWT
      PWSAM1M(JL,JK) = PWSAM1M(JL,JK)+ZWU*(1.0_JPRB-ZFF)
    ENDIF
  ENDDO
ENDDO

!*         0.2. Compute net heat flux at the surface.
!               -------------------------------------

DO JL=KIDIA,KFDIA
  IF (LDLAND(JL)) THEN
    ZSSRFL = 0.0_JPRB
    ZSLRFL = 0.0_JPRB
    ZTHFL  = 0.0_JPRB

    IF ( LEFLAKE ) THEN
      IF ( PFRTI(JL,9) > RFRSMALL ) THEN
        IF ( LEURBAN ) THEN
           IF ( (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8)+PFRTI(JL,10)) > RFRSMALL ) THEN
             PGSN(JL) = PGSN(JL)+ZSURFL(JL)
             ZSSRFL = ZSSRFL+ZSURFL(JL) &
              & /(PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8)+PFRTI(JL,10))
             ZSLRFL = ZSLRFL+ZSURFL(JL) &
              & /(PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8)+PFRTI(JL,10))
             ZTHFL = ZTHFL+ZSURFL(JL) &
              & /(PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8)+PFRTI(JL,10))
             ZSURFL(JL) = 0.0_JPRB
           ENDIF
        ELSE
          IF ( (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8)) > RFRSMALL ) THEN
            PGSN(JL) = PGSN(JL)+ZSURFL(JL)
            ZSSRFL = ZSSRFL+ZSURFL(JL) &
             & /(PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8))
            ZSLRFL = ZSLRFL+ZSURFL(JL) &
             & /(PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8))
            ZTHFL = ZTHFL+ZSURFL(JL) &
             & /(PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8))
            ZSURFL(JL) = 0.0_JPRB
          ENDIF
        ENDIF
        PGSN(JL) = PGSN(JL)+ZSURFL(JL)
        ZSURFL(JL) = 0.0_JPRB
      ENDIF
    ENDIF

    ZSSRFL = ZSSRFL+ZSURFL(JL)
    ZSLRFL = ZSLRFL+ZSURFL(JL)
    ZTHFL = ZTHFL+ZSURFL(JL)
    PGSN(JL) = PGSN(JL)+ZSURFL(JL)
    ZSURFL(JL) = 0.0_JPRB

    PAHFSTI(JL,3) = PAHFSTI(JL,3)+PFRTI(JL,3)*ZTHFL
    PEVAPTI(JL,3) = PEVAPTI(JL,3)+PFRTI(JL,3)*RLVTT*ZTHFL
    PAHFSTI(JL,4) = PAHFSTI(JL,4)+PFRTI(JL,4)*ZTHFL
    PEVAPTI(JL,4) = PEVAPTI(JL,4)+PFRTI(JL,4)*RLVTT*ZTHFL
    PAHFSTI(JL,6) = PAHFSTI(JL,6)+PFRTI(JL,6)*ZTHFL
    PEVAPTI(JL,6) = PEVAPTI(JL,6)+PFRTI(JL,6)*RLVTT*ZTHFL
    PAHFSTI(JL,8) = PAHFSTI(JL,8)+PFRTI(JL,8)*ZTHFL
    PEVAPTI(JL,8) = PEVAPTI(JL,8)+PFRTI(JL,8)*RLVTT*ZTHFL

    PSLRFL(JL) = PSLRFL(JL) &
     & +(PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8))*ZSLRFL
    PSSRFLTI(JL,3) = PSSRFLTI(JL,3)+PFRTI(JL,3)*ZSSRFL
    PSSRFLTI(JL,4) = PSSRFLTI(JL,4)+PFRTI(JL,4)*ZSSRFL
    PSSRFLTI(JL,6) = PSSRFLTI(JL,6)+PFRTI(JL,6)*ZSSRFL
    PSSRFLTI(JL,8) = PSSRFLTI(JL,8)+PFRTI(JL,8)*ZSSRFL

    IF ( LEURBAN ) THEN
      PAHFSTI(JL,10) = PAHFSTI(JL,10)+PFRTI(JL,10)*ZTHFL
      PEVAPTI(JL,10) = PEVAPTI(JL,10)+PFRTI(JL,10)*RLVTT*ZTHFL
      PSLRFL(JL) = PSLRFL(JL)+PFRTI(JL,10)*ZSLRFL
      PSSRFLTI(JL,10) = PSSRFLTI(JL,10)+PFRTI(JL,10)*ZSSRFL
    ENDIF

  ELSE
    ZSURFL (JL) = 0.0_JPRB
  ENDIF
ENDDO
      
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SRFTSAD_MOD:SRFTSAD',1,ZHOOK_HANDLE)

END SUBROUTINE SRFTSAD
END MODULE SRFTSAD_MOD
