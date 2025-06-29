MODULE SRFSN_LWIMPSTL_MOD
CONTAINS
SUBROUTINE SRFSN_LWIMPSTL(KIDIA ,KFDIA    ,KLON   ,&
 & PTMST    ,LDREGSF  ,&
 & PSSNM1M5 ,PTSNM1M5 ,PRSNM1M  ,PTSAM1M5 ,PHLICEM1M5,&
 & PSLRFL5  ,PSSRFLTI5,PFRTI    ,PAHFSTI5 ,PEVAPTI5 ,&
 & PSSFC5   ,PSSFL5   ,PEVAPSNW5,&
 & PTSFC5   ,PTSFL5   ,&
 & YDCST    ,YDVEG    ,YDSOIL   ,YDFLAKE  , YDURB, &
 & PTSN5    ,PGSN5    ,&                             
 & PSSNM1M  ,PTSNM1M  ,PTSAM1M  ,&
 & PSLRFL   ,PSSRFLTI ,PAHFSTI  ,PEVAPTI,&
 & PSSFC    ,PSSFL    ,PEVAPSNW ,&
 & PTSFC    ,PTSFL    ,&
 & PTSN    , PGSN )

USE PARKIND1  , ONLY : JPIM, JPRB
USE YOMHOOK   , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_CST   , ONLY : TCST
USE YOS_VEG   , ONLY : TVEG
USE YOS_SOIL  , ONLY : TSOIL
USE YOS_FLAKE , ONLY : TFLAKE
USE YOS_URB   , ONLY : TURB

#ifdef DOC
! (C) Copyright 2011- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!**** *SRFSNS_LWIMPTL* - CONTAINS SNOW PARAMETRIZATION
!                        (Tangent linear)
!
!     PURPOSE.
!     --------
!          COMPUTES CHANGES IN SNOW TEMPERATURE

!**   INTERFACE.
!     ----------
!          *SRFSN_SLWIMPTL* IS CALLED FROM *SURFTSTPSTL*.

!     PARAMETER   DESCRIPTION                                          UNITS
!     ---------   -----------                                          -----

!     INPUT PARAMETERS (INTEGER):
!     *KIDIA*      START POINT
!     *KFDIA*      END POINT
!     *KLON*       NUMBER OF GRID POINTS PER PACKET

!     INPUT PARAMETERS (REAL):
!     *PTMST*      TIME STEP                                            S
!     *PFRTI*      TILE FRACTIONS                                       -
 
!     INPUT PARAMETERS (LOGICAL):
!     *LDREGSF*    TRUE WHEN REGULARIZATION USED

!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!  Trajectory  Perturbation  Description                                Unit
!  PSSNM1M5    PSSNM1M       SNOW MASS (per unit area)                  kg/m2/s
!  PTSNM1M5    PTSNM1M       SNOW TEMPERATURE                           K
!  PRSNM1M     ---           SNOW DENSITY                               kg/m3 
!  PTSAM1M5    PTSAM1M       SOIL TEMPERATURE                           K
!  PHLICEM1M5  ---           LAKE ICE THICKNESS                         m
!  PSLRFL5     PSLRFL        NET LONGWAVE  RADIATION AT THE SURFACE     W/m2
!  PSSRFLTI5   PSSRFLTI      NET SHORTWAVE RADIATION AT THE SURFACE,    W/m2
!                            FOR EACH TILE  
!  PAHFSTI5    PAHFSTI       TILE SENSIBLE HEAT FLUX                    W/m2
!  PEVAPTI5    PEVAPTI       TILE EVAPORATION                           kg/m2/s
!  PSSFC5      PSSFC         CONVECTIVE  SNOW FLUX AT THE SURFACE       kg/m2/s
!  PSSFL5      PSSFL         LARGE SCALE SNOW FLUX AT THE SURFACE       kg/m2/s
!  PEVAPSNW5   PEVAPSNW      EVAPORATION FROM SNOW UNDER FOREST         kg/m2/s
!  PTSFC5      PTSFC         Convective Throughfall at the surface      kg/m2/s
!  PTSFL5      PTSFL         Large Scale Throughfall at the surface     kg/m2/s

!     PARAMETERS AT T+1 :
!  Trajectory  Perturbation  Description                                Unit
!  PTSN5       PTSN          SNOW TEMPERATURE                           K

!    FLUXES FROM SNOW SCHEME:
!  Trajectory  Perturbation  Description                                Unit
!  PGSN5       PGSN          GROUND HEAT FLUX FROM SNOW DECK TO SOIL    W/m2 (#)


! (#) THOSE TWO QUANTITIES REPRESENT THE WHOLE GRID-BOX. IN RELATION
!       TO THE DOCUMENTATION, THEY ARE PGSN=Fr_s*G_s

!     METHOD.
!     -------
!     Based on the original snow (as in ERA-40) with the following updates:
!     - Liquid water as a diagnostics
!     - Interception of rainfall

!     EXTERNALS.
!     ----------

!     REFERENCE.
!     ----------
!          SEE SOIL PROCESSES' PART OF THE MODEL'S DOCUMENTATION FOR
!     DETAILS ABOUT THE MATHEMATICS OF THIS ROUTINE.

!     Original:
!     ---------
!     M. Janiskova              E.C.M.W.F.     06-02-2012  

!     Modifications
!     -------------

!     ------------------------------------------------------------------
#endif

IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM), INTENT(IN)   :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KLON

LOGICAL,            INTENT(IN)   :: LDREGSF

REAL(KIND=JPRB),    INTENT(IN)   :: PTMST
REAL(KIND=JPRB),    INTENT(IN)   :: PSSNM1M5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSNM1M5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PRSNM1M(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSAM1M5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PHLICEM1M5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSLRFL5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSRFLTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PFRTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PAHFSTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSFC5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSFL5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPSNW5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSFC5(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSFL5(:)
TYPE(TCST),         INTENT(IN)   :: YDCST
TYPE(TVEG),         INTENT(IN)   :: YDVEG
TYPE(TSOIL),        INTENT(IN)   :: YDSOIL
TYPE(TFLAKE),       INTENT(IN)   :: YDFLAKE
TYPE(TURB),         INTENT(IN)   :: YDURB

REAL(KIND=JPRB),    INTENT(OUT)  :: PTSN5(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PGSN5(:)

REAL(KIND=JPRB),    INTENT(IN)   :: PSSNM1M(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSNM1M(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSAM1M(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSLRFL(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSRFLTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PAHFSTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSFC(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSFL(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPSNW(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSFC(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSFL(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PTSN(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PGSN(:)


!      LOCAL VARIABLES

LOGICAL            :: LLNOSNOW(KLON)

REAL(KIND=JPRB),DIMENSION(KLON) :: ZSSFC5,ZSSFL5
REAL(KIND=JPRB),DIMENSION(KLON) :: ZTSFC5,ZTSFL5,ZLWC5,ZDSNR5,ZDSNRP5
REAL(KIND=JPRB) :: ZTSTAR5,ZHFLUXP5,ZHFLUX5,ZSSTAR5,ZFUNC5,ZDFUNC5
REAL(KIND=JPRB) :: ZCONSB5,ZCONS25,ZSNRES5,ZRS5,ZWFLUXRF5
REAL(KIND=JPRB) :: ZTSTARI5,ZDTH5,ZDTHI5,ZDTM5,ZGSN5,ZMSN5,ZSSTAR15
REAL(KIND=JPRB) :: ZCOND15,ZCOND25,ZCOND35,ZCOND45,ZCOND55,ZCOND65
REAL(KIND=JPRB) :: ZHFLUXRF15,ZDSN5,ZCONSA5

REAL(KIND=JPRB),DIMENSION(KLON) :: ZLICE,ZFRSN,ZIFRSN
REAL(KIND=JPRB),DIMENSION(KLON) :: ZSSFC,ZSSFL,ZLWC,ZDSNR,ZDSNRP,ZTSFC,ZTSFL
REAL(KIND=JPRB) :: ZFUNC,ZDFUNC,ZDSN,ZDTH,ZDTHI,ZDTM,ZEXPF,&
 &        ZGSN,ZHFLUXP,ZHFLUX,ZLINA,ZMSN,ZRS,ZSNRES,ZSOILRES,ZSSTAR,ZSSTAR1,&
 &        ZT0,ZTSTAR,ZTSTARI,ZHOICE,ZTMST,ZCONSA,ZCONSAMAX,ZCONSB,ZCONS2,&
 &        ZCONSLWC,ZWFLUXRF,ZHFLUXRF1,ZITEMPAMP,&
 &        ZGRIDFRAC
REAL(KIND=JPRB) :: ZCOND1,ZCOND2,ZCOND3,ZCOND4,ZCOND5,ZCOND6
REAL(KIND=JPRB) :: ZRDSNMAX, ZRDSNRESMAX

INTEGER(KIND=JPIM) :: JL

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SRFSN_LWIMPSTL_MOD:SRFSN_LWIMPSTL',0,ZHOOK_HANDLE)
ASSOCIATE(RDAY=>YDCST%RDAY, RLMLT=>YDCST%RLMLT, RLSTT=>YDCST%RLSTT, &
 & RLVTT=>YDCST%RLVTT, RPI=>YDCST%RPI, RTT=>YDCST%RTT, &
 & LEFLAKE=>YDFLAKE%LEFLAKE, LEURBAN=>YDURB%LEURBAN, RH_ICE_MIN_FLK=>YDFLAKE%RH_ICE_MIN_FLK, &
 & RALAMSN=>YDSOIL%RALAMSN, RDSNMAX=>YDSOIL%RDSNMAX, RFRSMALL=>YDSOIL%RFRSMALL, &
 & RFRTINY=>YDSOIL%RFRTINY, RHOCI=>YDSOIL%RHOCI, RHOICE=>YDSOIL%RHOICE, &
 & RLAMICE=>YDSOIL%RLAMICE, RLWCSWEA=>YDSOIL%RLWCSWEA, &
 & RLWCSWEB=>YDSOIL%RLWCSWEB, RLWCSWEC=>YDSOIL%RLWCSWEC, RTAUA=>YDSOIL%RTAUA, &
 & RTAUF=>YDSOIL%RTAUF, RTEMPAMP=>YDSOIL%RTEMPAMP, &
 & RVLAMSK=>YDVEG%RVLAMSK)

!*         1.    SET UP SOME CONSTANTS.
!                --- -- ---- ----------
!*               PHYSICAL CONSTANTS.
!                -------- ----------
! RESISTANCE FOR HEAT CONDUCTION IN HALF TOP SOIL LAYER
!  (ESTIMATE FROM SKIN LAYER CONDUCTIVITY FOR BARE SOIL; W/M2K)


ZHOICE=1.0_JPRB/RHOICE
ZSOILRES=1.0_JPRB/RVLAMSK(8)
ZT0=RTT
ZTMST=1.0_JPRB/PTMST
ZEXPF=EXP(-RTAUF*PTMST/RDAY)
ZLINA=RTAUA*PTMST/RDAY
ZITEMPAMP=1.0_JPRB/RTEMPAMP

ZRDSNMAX=1.0_JPRB
ZRDSNRESMAX=1._JPRB

!*   FIND LAKE POINTS WITH ICE COVER
DO JL=KIDIA,KFDIA
  IF ( PHLICEM1M5(JL) > RH_ICE_MIN_FLK  .AND. LEFLAKE ) THEN
    ZLICE(JL)=1._JPRB
  ELSE
    ZLICE(JL)=0._JPRB
  ENDIF
ENDDO

!     ------------------------------------------------------------------
!*         2. NEW SNOW T AND MASS INCLUDING GROUND HEAT FLUX AND MELTING.
!             -----------------------------------------------------------

DO JL=KIDIA,KFDIA
  ZFRSN(JL)=MAX(PFRTI(JL,5)+PFRTI(JL,7),RFRTINY)
  ZIFRSN(JL)=1.0_JPRB/ZFRSN(JL)
  IF (ZFRSN(JL) < RFRSMALL) THEN
    LLNOSNOW(JL)=.TRUE.
  ELSE
    LLNOSNOW(JL)=.FALSE.
  ENDIF
  ZGRIDFRAC=(PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,5)&
   & +PFRTI(JL,6)+PFRTI(JL,7)+PFRTI(JL,8))

  IF ( LEURBAN ) THEN
   ZGRIDFRAC=(PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,5)&
   & +PFRTI(JL,6)+PFRTI(JL,7)+PFRTI(JL,8)+PFRTI(JL,10))
  ENDIF

  IF ( LEFLAKE ) THEN
    IF ( PFRTI(JL,9) .EQ. 1._JPRB ) THEN
      ZGRIDFRAC=0._JPRB ! FULL COVER OF LAKE --NO SNOW THERE !
    ELSE
      ZGRIDFRAC=ZGRIDFRAC+PFRTI(JL,9)
    ENDIF
  ENDIF

  ZSSFC (JL) = ZGRIDFRAC*PSSFC (JL)
  ZSSFC5(JL) = ZGRIDFRAC*PSSFC5(JL)
  ZSSFL (JL) = ZGRIDFRAC*PSSFL (JL)
  ZSSFL5(JL) = ZGRIDFRAC*PSSFL5(JL)
  ZTSFC (JL) = ZGRIDFRAC*PTSFC (JL)
  ZTSFC5(JL) = ZGRIDFRAC*PTSFC5(JL)
  ZTSFL(JL)  = ZGRIDFRAC*PTSFL(JL)
  ZTSFL5(JL) = ZGRIDFRAC*PTSFL5(JL)
ENDDO

DO JL=KIDIA,KFDIA
  IF (LLNOSNOW(JL)) THEN
    ZSSTAR  = PSSNM1M (JL)+PTMST*(ZSSFC (JL)+ZSSFL (JL))
    ZSSTAR5 = PSSNM1M5(JL)+PTMST*(ZSSFC5(JL)+ZSSFL5(JL))
    PTSN (JL) = 0.0_JPRB
    PTSN5(JL) = ZT0
    PGSN (JL) = 0.0_JPRB
    PGSN5(JL) = 0.0_JPRB
    ZLWC (JL) = 0.0_JPRB
    ZLWC5(JL) = 0.0_JPRB
  ELSE

!           NET HEAT FLUX AT SNOW SURFACE; THIS IS MULTIPLIED
!           BY FRACTION BECAUSE EQUATIONS APPLY TO TOTAL SNOW MASS
!           IN THE GRID SQUARE

    ZHFLUXP  = PFRTI(JL,5)*(PAHFSTI (JL,5)+RLSTT*PEVAPTI (JL,5))&
     & + PFRTI(JL,7)*(PAHFSTI (JL,7)+RLVTT*(PEVAPTI (JL,7)-PEVAPSNW (JL))&
     & + RLSTT*PEVAPSNW (JL))&
     & + PFRTI(JL,5)*PSSRFLTI (JL,5)&
     & + PFRTI(JL,7)*PSSRFLTI (JL,7)&
     & + (PFRTI(JL,5)+PFRTI(JL,7))*PSLRFL (JL)
    ZHFLUXP5 = PFRTI(JL,5)*(PAHFSTI5(JL,5)+RLSTT*PEVAPTI5(JL,5))&
     & + PFRTI(JL,7)*(PAHFSTI5(JL,7)+RLVTT*(PEVAPTI5(JL,7)-PEVAPSNW5(JL))&
     & + RLSTT*PEVAPSNW5(JL))&
     & + PFRTI(JL,5)*PSSRFLTI5(JL,5)&
     & + PFRTI(JL,7)*PSSRFLTI5(JL,7)&
     & + (PFRTI(JL,5)+PFRTI(JL,7))*PSLRFL5(JL)
    ZHFLUX  = ZIFRSN(JL)*ZHFLUXP
    ZHFLUX5 = ZIFRSN(JL)*ZHFLUXP5

! PRELIMINARY SNOW MASS
    ZSSTAR  = PSSNM1M (JL)+PTMST&
     & *(ZSSFC (JL)+ZSSFL (JL)+PFRTI(JL,5)*PEVAPTI (JL,5)&
     & +PFRTI(JL,7)*PEVAPSNW (JL))
    ZSSTAR5 = PSSNM1M5(JL)+PTMST*&
     & (ZSSFC5(JL)+ZSSFL5(JL)+PFRTI(JL,5)*PEVAPTI5(JL,5)&
     & +PFRTI(JL,7)*PEVAPSNW5(JL))
    ! CORRECT FOR NEGATIVE VALUES OF SNOW MASS AFTER P-E
    IF (ZSSTAR5 < 0.0_JPRB) THEN
!      ZSSTAR=MAX(ZSSTAR,0.0_JPRB)
      ZSSTAR  = 0.0_JPRB     
      ZSSTAR5 = 0.0_JPRB
    ENDIF

! SNOW LIQUID WATER - IMPLICIT
    ! SNOW DEPTH (REAL)
    ZDSNR (JL) = PSSNM1M (JL)/(PRSNM1M(JL)*ZFRSN(JL))
    ZDSNR5(JL) = PSSNM1M5(JL)/(PRSNM1M(JL)*ZFRSN(JL))

    ! SNOW LIQUID WATER CAPACITY
    ZCONSLWC=MAX(0._JPRB,(RLWCSWEA-PRSNM1M(JL))/RLWCSWEA)
    ZLWC (JL) = PSSNM1M (JL)*(RLWCSWEB+(RLWCSWEC-RLWCSWEB)*ZCONSLWC)
    ZLWC5(JL) = PSSNM1M5(JL)*(RLWCSWEB+(RLWCSWEC-RLWCSWEB)*ZCONSLWC)
    ! LWC IS RESCALLED CONSIDERING EFFECTIVE DEPTH (ZRDSNMAX)
    ! ZLWC(JL)=ZLWC(JL)*MIN(ZDSNR(JL),ZRDSNMAX)/ZDSNR(JL)
    IF (ZDSNR5(JL) < ZRDSNMAX) THEN
      ZDSNRP (JL) = ZDSNR (JL)
      ZDSNRP5(JL) = ZDSNR5(JL)
    ELSE
      ZDSNRP (JL) = 0.0_JPRB
      ZDSNRP5(JL) = ZRDSNMAX
    ENDIF
    ZLWC (JL) = (ZLWC5(JL)*ZDSNRP(JL)+ZDSNRP5(JL)*ZLWC(JL))/ZDSNR5(JL) &
     & - ZLWC5(JL)*ZDSNRP5(JL)*ZDSNR(JL)/(ZDSNR5(JL)**2)
    ZLWC5(JL) = ZLWC5(JL)*ZDSNRP5(JL)/ZDSNR5(JL)  

    ! ANALYTICAL FUNCTIONS - SNOW LIQUID WATER
    IF ( PTSNM1M5(JL) < (ZT0-0.5_JPRB*RTEMPAMP) ) THEN
      ZFUNC  = 0._JPRB
      ZFUNC5 = 0._JPRB
      ZDFUNC  = 0._JPRB
      ZDFUNC5 = 0._JPRB
    ELSEIF(PTSNM1M5(JL) < ZT0) THEN
      ZFUNC  = RPI*ZITEMPAMP*COS(RPI*(PTSNM1M5(JL)-ZT0)*ZITEMPAMP)*PTSNM1M(JL)
      ZFUNC5 = 1._JPRB+SIN(RPI*(PTSNM1M5(JL)-ZT0)*ZITEMPAMP)
      ZDFUNC  = -RPI*RPI*ZITEMPAMP*ZITEMPAMP &
       & *SIN(RPI*(PTSNM1M5(JL)-ZT0)*ZITEMPAMP)*PTSNM1M(JL)
      IF (LDREGSF) THEN
        ZDFUNC  = ZDFUNC/100.0_JPRB
      ENDIF
      ZDFUNC5 = COS(RPI*(PTSNM1M5(JL)-ZT0)*ZITEMPAMP)*RPI*ZITEMPAMP
    ELSE
      ZFUNC  = 0._JPRB
      ZFUNC5 = 0._JPRB
      ZDFUNC  = 0.0_JPRB
      ZDFUNC5 = RPI*ZITEMPAMP
    ENDIF

! TERMS FOR IMPLICIT TEMPERATURE EQUATION
    ZCONSB  = RLMLT*(ZLWC5(JL)*ZDFUNC+ZDFUNC5*ZLWC(JL))
    ZCONSB5 = RLMLT*ZLWC5(JL)*ZDFUNC5
    ZCONSAMAX=(PRSNM1M(JL)*RHOCI*ZRDSNMAX)*ZHOICE
    ZCOND1  = (PSSNM1M (JL)*RHOCI*ZHOICE)*ZIFRSN(JL)
    ZCOND15 = (PSSNM1M5(JL)*RHOCI*ZHOICE)*ZIFRSN(JL)
    IF (ZCOND15 < ZCONSAMAX) THEN
      ZCONSA  = ZCOND1
      ZCONSA5 = ZCOND15
    ELSE
      ZCONSA  = 0.0_JPRB
      ZCONSA5 = ZCONSAMAX
    ENDIF
    ZCONS2  = -PTMST*(ZCONSA+ZCONSB)/(ZCONSA5+ZCONSB5)**2
    ZCONS25 = PTMST/(ZCONSA5+ZCONSB5)

! REAL SNOW DEPTH
    ZCOND2  = PSSNM1M (JL)/(PRSNM1M(JL)*ZFRSN(JL))
    ZCOND25 = PSSNM1M5(JL)/(PRSNM1M(JL)*ZFRSN(JL))
    IF (ZCOND25 < ZRDSNRESMAX) THEN
      ZDSN  = ZCOND2
      ZDSN5 = ZCOND25
    ELSE
      ZDSN  = 0.0_JPRB
    ! ZDSN5 = ZCOND25 ! ZRDSNRESMAX
      ZDSN5 = ZRDSNRESMAX
    ENDIF
    ! RESISTANCE FOR HEAT FLUX BETWEEN SNOW AND SOIL LAYER
    ZSNRES  = 0.5_JPRB*ZDSN /(RLAMICE*(PRSNM1M(JL)*ZHOICE)**RALAMSN)
    ZSNRES5 = 0.5_JPRB*ZDSN5/(RLAMICE*(PRSNM1M(JL)*ZHOICE)**RALAMSN)
    ZRS5    = 1.0_JPRB/(ZSNRES5+ZSOILRES)
    ZRS     = -ZSNRES*ZRS5*ZRS5

!  ACCOUNT FOR THROUGHFALL INTERCEPTION
    ! mass advected
    ZWFLUXRF  = (PFRTI(JL,5)+PFRTI(JL,7))*(ZTSFC (JL)+ZTSFL (JL))
    ZWFLUXRF5 = (PFRTI(JL,5)+PFRTI(JL,7))*(ZTSFC5(JL)+ZTSFL5(JL))
    ! phase changes
    ZCOND3  = ZWFLUXRF *PTMST
    ZCOND35 = ZWFLUXRF5*PTMST
    ZCOND4  = (1.0_JPRB-ZFUNC5)*ZLWC(JL)-ZLWC5(JL)*ZFUNC
    ZCOND45 = ZLWC5(JL)*(1.0_JPRB-ZFUNC5)
    IF (ZCOND35 < ZCOND45) THEN
      ZWFLUXRF  = ZCOND3
      ZWFLUXRF5 = ZCOND35
    ELSE
      ZWFLUXRF  = ZCOND4
      ZWFLUXRF5 = ZCOND45
    ENDIF
    ZCOND5  = ZTMST*(ZT0-PTSNM1M5(JL))*ZCONSA-ZTMST*ZCONSA5*PTSNM1M(JL) &
     & + ZT0*ZRS
    ZCOND55 = ZCONSA5*(ZT0-PTSNM1M5(JL))*ZTMST+ZT0*ZRS5
    ZCOND6  = RLMLT*ZWFLUXRF *ZTMST
    ZCOND65 = RLMLT*ZWFLUXRF5*ZTMST
    IF (ZCOND55 < ZCOND65) THEN
      ZHFLUXRF1  = ZCOND5
      ZHFLUXRF15 = ZCOND55
    ELSE
      ZHFLUXRF1  = ZCOND6
      ZHFLUXRF15 = ZCOND65
    ENDIF
    IF (ZHFLUXRF15 < 0.0_JPRB) THEN
      ZHFLUXRF1  = 0.0_JPRB
      ZHFLUXRF15 = 0.0_JPRB
    ENDIF
    !mass intercepted - recalculate from ZHFLUXRF1
    ZWFLUXRF  = ZHFLUXRF1 /RLMLT
    ZWFLUXRF5 = ZHFLUXRF15/RLMLT
    ZSSTAR  = ZSSTAR +ZWFLUXRF *PTMST
    ZSSTAR5 = ZSSTAR5+ZWFLUXRF5*PTMST
! SOLVE IMPLICIT TEMPERATURE EQUATION
    ZTSTARI5 = 1.0_JPRB/(1.0_JPRB+ZCONS25*ZRS5)
    ZTSTARI  = -(ZCONS25*ZRS+ZRS5*ZCONS2)*ZTSTARI5*ZTSTARI5
    ZTSTAR  = (PTSNM1M5(JL)+ZCONS25*(ZHFLUX5+ZHFLUXRF15+PTSAM1M5(JL,1)*ZRS5)) &
     & *ZTSTARI &
     & +ZTSTARI5*PTSNM1M(JL) &
     & +ZTSTARI5*(ZHFLUX5+ZHFLUXRF15+PTSAM1M5(JL,1)*ZRS5)*ZCONS2 &
     & +ZTSTARI5*ZCONS25*(ZHFLUX+ZHFLUXRF1+PTSAM1M5(JL,1)*ZRS &
     & +ZRS5*PTSAM1M(JL,1))
    ZTSTAR5 = (PTSNM1M5(JL)+ZCONS25*(ZHFLUX5+ZHFLUXRF15+PTSAM1M5(JL,1)*ZRS5)) &
     & *ZTSTARI5
    IF (ZTSTAR5 <= ZT0) THEN
! NO MELTING OF SNOW
      PTSN (JL) = ZTSTAR
      PTSN5(JL) = ZTSTAR5
      PGSN (JL) = (ZTSTAR5-PTSAM1M5(JL,1))*ZRS+ZRS5*(ZTSTAR-PTSAM1M(JL,1))
      PGSN5(JL) = (ZTSTAR5-PTSAM1M5(JL,1))*ZRS5
    ELSE
! MELTING OF SNOW:
! HEATING PART OF THE TIME STEP
      ZDTHI5 = 1.0_JPRB/(ZHFLUX5+ZHFLUXRF15-(ZT0-PTSAM1M5(JL,1))*ZRS5)
      ZDTHI  = -(ZHFLUX+ZHFLUXRF1-(ZT0-PTSAM1M5(JL,1))*ZRS+ZRS5*PTSAM1M(JL,1)) &
       & *ZDTHI5*ZDTHI5
      ZDTH  = ((ZT0-PTSNM1M5(JL))*(ZCONSA5+ZCONSB5))*ZDTHI &
       & +ZDTHI5*(ZT0-PTSNM1M5(JL))*(ZCONSA+ZCONSB) &
       & -ZDTHI5*(ZCONSA5+ZCONSB5)*PTSNM1M(JL)
      ZDTH5 = ((ZT0-PTSNM1M5(JL))*(ZCONSA5+ZCONSB5))*ZDTHI5
! MELTING PART OF THE TIME STEP
      ZDTM  = -ZDTH
      ZDTM5 = PTMST-ZDTH5
      ZGSN  = (ZT0-PTSAM1M5(JL,1))*ZRS-ZRS5*PTSAM1M(JL,1)
      ZGSN5 = (ZT0-PTSAM1M5(JL,1))*ZRS5
      ZMSN  = (ZHFLUX +ZHFLUXRF1 -ZGSN )/RLMLT
      ZMSN5 = (ZHFLUX5+ZHFLUXRF15-ZGSN5)/RLMLT
      ZSSTAR1 = ZSSTAR-ZFRSN(JL)*(ZDTM5*ZMSN-ZMSN5*ZDTM)
      ZSSTAR15 = ZSSTAR5-ZDTM5*ZMSN5*ZFRSN(JL)
      IF (ZSSTAR15 >= 0.0_JPRB) THEN
! NOT ALL THE SNOW HAS BEEN MELTED
        PTSN (JL) = 0.0_JPRB
        PTSN5(JL) = ZT0
        PGSN (JL) = (ZT0-PTSAM1M5(JL,1))*ZRS-ZRS5*PTSAM1M(JL,1)
        PGSN5(JL) = (ZT0-PTSAM1M5(JL,1))*ZRS5
      ELSE
! ALL THE SNOW HAS BEEN MELTED; COMPUTE TIME IT TOOK TO MELT
        PTSN (JL) = 0.0_JPRB
        PTSN5(JL) = ZT0
        PGSN (JL) = ZTMST*( ((ZT0-PTSAM1M5(JL,1))*ZRS5)*(ZDTM+ZDTH) &
         & +(ZT0-PTSAM1M5(JL,1))*(ZDTM5+ZDTH5)*ZRS &
         & -ZRS5*(ZDTM5+ZDTH5)*PTSAM1M(JL,1) &
         & +(ZHFLUX5+ZHFLUXRF15)*(-ZDTM-ZDTH) &
         & +(PTMST-ZDTM5-ZDTH5)*(ZHFLUX+ZHFLUXRF1) )
        PGSN5(JL) = ( ((ZT0-PTSAM1M5(JL,1))*ZRS5)*(ZDTM5+ZDTH5)&
         & +(ZHFLUX5+ZHFLUXRF15)*(PTMST-ZDTM5-ZDTH5) )*ZTMST
      ENDIF
    ENDIF

  ENDIF
ENDDO

!*         5. NORMALIZE QUANTITIES TO THE GRID-SQUARE.
!             ----------------------------------------

DO JL=KIDIA,KFDIA
  PGSN (JL) = ZFRSN(JL)*PGSN (JL)
  PGSN5(JL) = ZFRSN(JL)*PGSN5(JL)
ENDDO

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SRFSN_LWINPSTL_MOD:SRFSN_LWIMPSTL',1,ZHOOK_HANDLE)

END SUBROUTINE SRFSN_LWIMPSTL
END MODULE SRFSN_LWIMPSTL_MOD
