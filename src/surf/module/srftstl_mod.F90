MODULE SRFTSTL_MOD
CONTAINS
SUBROUTINE SRFTSTL(KIDIA  , KFDIA  , KLON   , KLEVS ,&
 & PTMST  , PTSAM1M5 , PWSAM1M5, KSOTY, &
 & PFRTI  , PAHFSTI5 , PEVAPTI5,&
 & PSLRFL5, PSSRFLTI5, PGSN5   ,&
 & PCTSA5 , PTSA5    , LDLAND  ,&
 & PSSDP3,&
 & YDCST  , YDSOIL   , YDFLAKE , YDURB,&  
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

USE SRFWDIFSTL_MOD
USE YOMSURF_SSDP_MOD

#ifdef DOC
! (C) Copyright 2012- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SRFTSTL* - Computes temperature changes in soil.
!                 (Tangent linear)

!     PURPOSE.
!     --------
!**   Computes temperature changes in soil due to 
!**   surface heat flux and diffusion.  

!**   INTERFACE.
!     ----------
!          *SRFTSTL* IS CALLED FROM *SURFTSTPSTL*.

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
!     *SRFWDIFSTL*

!     REFERENCE.
!     ----------
!          See documentation.

!     Original   
!     --------
!     M. Janiskova              E.C.M.W.F.     07-02-2012 

!     Modifications
!     -------------
!     J. McNorton           24/08/2022  urban tile
!     I. Ayan-Miguez (BSC)  Sep 2023    Added PSSDP3 object for spatially distributed parameters 
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

REAL(KIND=JPRB),    INTENT(IN)   :: PSSDP3(:,:,:)

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

REAL(KIND=JPRB),    INTENT(IN)   :: PTSAM1M(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PWSAM1M(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PAHFSTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSLRFL(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSRFLTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PGSN(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PCTSA(:,:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PTSA(:,:)

!      LOCAL VARIABLES

REAL(KIND=JPRB) :: ZSURFL5(KLON)
REAL(KIND=JPRB) :: ZDIF5(KLON,KLEVS),ZLST5(KLON,KLEVS)
REAL(KIND=JPRB) :: ZCDZ5(KLON,KLEVS),ZRHS5(KLON,KLEVS),ZTSA5(KLON,KLEVS)
REAL(KIND=JPRB) :: ZSSRFL5,ZSLRFL5,ZTHFL5,ZWU5,ZLWT5,ZLIC5,ZLAMBDASAT5
REAL(KIND=JPRB) :: ZKERSTEN5
REAL(KIND=JPRB) :: ZCOND5

REAL(KIND=JPRB) :: ZSURFL(KLON)
REAL(KIND=JPRB) :: ZRHS(KLON,KLEVS), ZCDZ(KLON,KLEVS),&
 & ZLST(KLON,KLEVS), ZDIF(KLON,KLEVS),&
 & ZTSA(KLON,KLEVS)  
REAL(KIND=JPRB) :: ZCONS1, ZCONS2, ZSLRFL, ZSSRFL, ZTHFL,&
 & ZFF, ZWU, ZLIC, ZLWT, ZLAMBDASAT, ZKERSTEN, ZINVWSAT, ZCOND, ZLN10

INTEGER(KIND=JPIM) :: JK, JL, JS, IKLEVS
LOGICAL ::LLALLAYS

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

! -------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SRFTSTL_MOD:SRFTSTL',0,ZHOOK_HANDLE)
ASSOCIATE(RLVTT=>YDCST%RLVTT, &
 & LEFLAKE=>YDFLAKE%LEFLAKE, LEURBAN=>YDURB%LEURBAN, &
 & LEVGEN=>YDSOIL%LEVGEN, RDAT=>YDSOIL%RDAT, RFRSMALL=>YDSOIL%RFRSMALL, &
 & RKERST1=>YDSOIL%RKERST1, RKERST2=>YDSOIL%RKERST2, RKERST3=>YDSOIL%RKERST3, &
 & RLAMBDADRY=>YDSOIL%RLAMBDADRY, RLAMBDADRYM3D=>PSSDP3(:,:,SSDP3D_ID%NRLAMBDADRYM3D), &
 & RLAMBDAICE=>YDSOIL%RLAMBDAICE, RLAMBDAWAT=>YDSOIL%RLAMBDAWAT, &
 & RLAMSAT1=>YDSOIL%RLAMSAT1, RLAMSAT1M3D=>PSSDP3(:,:,SSDP3D_ID%NRLAMSAT1M3D), RSIMP=>YDSOIL%RSIMP, &
 & RWSAT=>YDSOIL%RWSAT, RWSATM3D=>PSSDP3(:,:,SSDP3D_ID%NRWSATM3D))

!*    0. INITIALIZATION
!     ------------------

ZLN10 = LOG(10.0_JPRB)

DO JL=KIDIA,KFDIA
  ZSURFL (JL) = 0.0_JPRB
  ZSURFL5(JL) = 0.0_JPRB
ENDDO

DO JK=1,KLEVS
  DO JL=KIDIA,KFDIA
    ZDIF (JL,JK) = 0.0_JPRB
    ZDIF5(JL,JK) = 0.0_JPRB
    ZLST (JL,JK) = 0.0_JPRB
    ZLST5(JL,JK) = 0.0_JPRB
    ZCDZ (JL,JK) = 0.0_JPRB
    ZCDZ5(JL,JK) = 0.0_JPRB
    ZRHS (JL,JK) = 0.0_JPRB
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

    ZSSRFL = PFRTI(JL,3)*PSSRFLTI(JL,3)&
     & +PFRTI(JL,4)*PSSRFLTI(JL,4)&
     & +PFRTI(JL,6)*PSSRFLTI(JL,6)&
     & +PFRTI(JL,8)*PSSRFLTI(JL,8) 
    ZSSRFL5 = PFRTI(JL,3)*PSSRFLTI5(JL,3)&
     & +PFRTI(JL,4)*PSSRFLTI5(JL,4)&
     & +PFRTI(JL,6)*PSSRFLTI5(JL,6)&
     & +PFRTI(JL,8)*PSSRFLTI5(JL,8)  
    ZSLRFL  = (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8))*PSLRFL (JL)
    ZSLRFL5 = (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8))*PSLRFL5(JL)

    ZTHFL=PFRTI(JL,3)*(PAHFSTI(JL,3)+RLVTT*PEVAPTI(JL,3))&
     & +PFRTI(JL,4)*(PAHFSTI(JL,4)+RLVTT*PEVAPTI(JL,4))&
     & +PFRTI(JL,6)*(PAHFSTI(JL,6)+RLVTT*PEVAPTI(JL,6))&
     & +PFRTI(JL,8)*(PAHFSTI(JL,8)+RLVTT*PEVAPTI(JL,8))  
    ZTHFL5 = PFRTI(JL,3)*(PAHFSTI5(JL,3)+RLVTT*PEVAPTI5(JL,3))&
     & +PFRTI(JL,4)*(PAHFSTI5(JL,4)+RLVTT*PEVAPTI5(JL,4))&
     & +PFRTI(JL,6)*(PAHFSTI5(JL,6)+RLVTT*PEVAPTI5(JL,6))&
     & +PFRTI(JL,8)*(PAHFSTI5(JL,8)+RLVTT*PEVAPTI5(JL,8))

    IF ( LEURBAN ) THEN 
     ZSSRFL  = ZSSRFL+PFRTI(JL,10)*PSSRFLTI(JL,10)
     ZSSRFL5 = ZSSRFL5+PFRTI(JL,10)*PSSRFLTI5(JL,10)
     ZSLRFL  = ZSLRFL+PFRTI(JL,10)*PSLRFL (JL)
     ZSLRFL5 = ZSLRFL5+PFRTI(JL,10)*PSLRFL5(JL)

     ZTHFL   = ZTHFL+PFRTI(JL,10)*(PAHFSTI(JL,10)+RLVTT*PEVAPTI(JL,10))
     ZTHFL5  = ZTHFL5+PFRTI(JL,10)*(PAHFSTI5(JL,10)+RLVTT*PEVAPTI5(JL,10))
    ENDIF

    ZSURFL (JL) = ZSSRFL +ZSLRFL +ZTHFL +PGSN (JL)     
    ZSURFL5(JL) = ZSSRFL5+ZSLRFL5+ZTHFL5+PGSN5(JL)

    IF ( LEFLAKE ) THEN
      IF ( PFRTI(JL,9) > RFRSMALL ) THEN
        ZSURFL (JL) = PGSN (JL)
        ZSURFL5(JL) = PGSN5(JL)
        IF ( LEURBAN ) THEN
          IF ( (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8)+PFRTI(JL,10)) > RFRSMALL ) THEN
            ZSURFL (JL) = PGSN (JL)+(ZSSRFL+ZSLRFL+ZTHFL) &
             & / (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8)+PFRTI(JL,10))
            ZSURFL5(JL) = PGSN5(JL)+(ZSSRFL5+ZSLRFL5+ZTHFL5) &
             & / (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8)+PFRTI(JL,10))
          ENDIF
        ELSE
         IF ( (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8)) > RFRSMALL ) THEN
           ZSURFL (JL) = PGSN (JL)+(ZSSRFL+ZSLRFL+ZTHFL) & 
            & / (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8))
           ZSURFL5(JL) = PGSN5(JL)+(ZSSRFL5+ZSLRFL5+ZTHFL5) & 
            & / (PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8))
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    
  ELSE
    ZSURFL (JL) = 0.0_JPRB
    ZSURFL5(JL) = 0.0_JPRB
  ENDIF
ENDDO

!*         3. Compute exchange coeff. layer by layer
!             --------------------------------------

DO JK=1,IKLEVS
  DO JL=KIDIA,KFDIA
    IF (LDLAND(JL)) THEN
      ZFF = 0.0_JPRB
      ZWU  = PWSAM1M (JL,JK)*(1.0_JPRB-ZFF)
      ZWU5 = PWSAM1M5(JL,JK)*(1.0_JPRB-ZFF)
      ZLWT5 = RLAMBDAWAT**ZWU5
      ZLWT  = ZLWT5*LOG(RLAMBDAWAT)*ZWU

      IF(LEVGEN)THEN
        JS = KSOTY(JL)
        ZINVWSAT = 1.0_JPRB/RWSATM3D(JL,JK)
        ZLIC5 = RLAMBDAICE**(RWSATM3D(JL,JK)-ZWU5)
        ZLIC  = -ZLIC5*LOG(RLAMBDAICE)*ZWU
        ZLAMBDASAT  = RLAMSAT1M3D(JL,JK)*(ZLIC5*ZLWT+ZLWT5*ZLIC)
        ZLAMBDASAT5 = RLAMSAT1M3D(JL,JK)*ZLIC5*ZLWT5
        IF (PWSAM1M5(JL,JK)*ZINVWSAT > RKERST1) THEN
          ZCOND  = PWSAM1M (JL,JK)*ZINVWSAT
          ZCOND5 = PWSAM1M5(JL,JK)*ZINVWSAT
        ELSE
          ZCOND  = 0.0_JPRB
          ZCOND5 = RKERST1
        ENDIF
        ZKERSTEN  = RKERST2*ZCOND/(ZLN10*ZCOND5)
        ZKERSTEN5 = RKERST2*LOG10(ZCOND5)+RKERST3
        ZDIF (JL,JK) = (ZLAMBDASAT5-RLAMBDADRYM3D(JL,JK))*ZKERSTEN &
         & +ZKERSTEN5*ZLAMBDASAT
        ZDIF5(JL,JK) = RLAMBDADRYM3D(JL,JK)+ZKERSTEN5*(ZLAMBDASAT5-RLAMBDADRYM3D(JL,JK))
      ELSE
        ZINVWSAT = 1.0_JPRB/RWSAT
        ZLIC5 = RLAMBDAICE**(RWSAT-ZWU5)
        ZLIC  = -ZLIC5*LOG(RLAMBDAICE)*ZWU
        ZLAMBDASAT  = RLAMSAT1*(ZLIC5*ZLWT+ZLWT5*ZLIC)
        ZLAMBDASAT5 = RLAMSAT1*ZLIC5*ZLWT5
        IF (PWSAM1M5(JL,JK)*ZINVWSAT > RKERST1) THEN
          ZCOND  = PWSAM1M (JL,JK)*ZINVWSAT
          ZCOND5 = PWSAM1M5(JL,JK)*ZINVWSAT
        ELSE
          ZCOND  = 0.0_JPRB
          ZCOND5 = RKERST1
        ENDIF
        ZKERSTEN  = RKERST2*ZCOND/(ZLN10*ZCOND5)
        ZKERSTEN5 = RKERST2*LOG10(ZCOND5)+RKERST3
        ZDIF (JL,JK) = (ZLAMBDASAT5-RLAMBDADRY)*ZKERSTEN &
         & +ZKERSTEN5*ZLAMBDASAT
        ZDIF5(JL,JK) = RLAMBDADRY+ZKERSTEN5*(ZLAMBDASAT5-RLAMBDADRY)
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
    ZLST (JL,JK) = ZCONS1*(ZDIF (JL,JK)+ZDIF (JL,JK+1))/(RDAT(JK)+RDAT(JK+1))
    ZLST5(JL,JK) = ZCONS1*(ZDIF5(JL,JK)+ZDIF5(JL,JK+1))/(RDAT(JK)+RDAT(JK+1))
    ZCDZ (JL,JK) = PCTSA (JL,JK)*RDAT(JK)
    ZCDZ5(JL,JK) = PCTSA5(JL,JK)*RDAT(JK)
    ZRHS (JL,JK) = PTMST*(ZSURFL(JL)/ZCDZ5(JL,JK) &
     & -ZSURFL5(JL)*ZCDZ(JL,JK)/(ZCDZ5(JL,JK)**2))
    ZRHS5(JL,JK) = PTMST*ZSURFL5(JL)/ZCDZ5(JL,JK)
  ENDIF
ENDDO

IF (LLALLAYS) THEN

!     Layers 2 to KLEVS-1
  DO JK=2,KLEVS-1
    DO JL=KIDIA,KFDIA
      IF (LDLAND(JL)) THEN
        ZLST (JL,JK) = ZCONS1*(ZDIF (JL,JK)+ZDIF (JL,JK+1)) &
         & /(RDAT(JK)+RDAT(JK+1))
        ZLST5(JL,JK) = ZCONS1*(ZDIF5(JL,JK)+ZDIF5(JL,JK+1)) &
         & /(RDAT(JK)+RDAT(JK+1))
        ZCDZ (JL,JK) = PCTSA (JL,JK)*RDAT(JK)
        ZCDZ5(JL,JK) = PCTSA5(JL,JK)*RDAT(JK)
      ENDIF
    ENDDO
  ENDDO

!     Layers KLEVS
  JK=KLEVS
  DO JL=KIDIA,KFDIA
    IF (LDLAND(JL)) THEN
      ZLST (JL,JK) = 0.0_JPRB
      ZLST5(JL,JK) = 0.0_JPRB
      ZCDZ (JL,JK) = PCTSA (JL,JK)*RDAT(JK)
      ZCDZ5(JL,JK) = PCTSA5(JL,JK)*RDAT(JK)
    ENDIF
  ENDDO
ENDIF

!*         5. Call tridiagonal solver
!             -----------------------
CALL SRFWDIFSTL(KIDIA,KFDIA,KLON,KLEVS,PTSAM1M5,ZLST5,ZRHS5,ZCDZ5,ZTSA5, &
 & LDLAND,LLALLAYS,YDSOIL,&
 & PTSAM1M,ZLST,ZRHS,ZCDZ,ZTSA)


!*         7. New temperatures
!             ----------------
DO JK=1,KLEVS
  DO JL=KIDIA,KFDIA
    IF (LDLAND(JL)) THEN
      PTSA (JL,JK) = PTSAM1M (JL,JK)*ZCONS2+ZTSA (JL,JK)
      PTSA5(JL,JK) = PTSAM1M5(JL,JK)*ZCONS2+ZTSA5(JL,JK)
    ELSE
      PTSA (JL,JK) = PTSAM1M(JL,JK)
      PTSA5(JL,JK) = PTSAM1M5(JL,JK)
    ENDIF
  ENDDO
ENDDO

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SRFTSTL_MOD:SRFTSTL',1,ZHOOK_HANDLE)

END SUBROUTINE SRFTSTL
END MODULE SRFTSTL_MOD
