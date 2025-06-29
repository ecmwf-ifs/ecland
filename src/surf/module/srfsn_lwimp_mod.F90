MODULE SRFSN_LWIMP_MOD
CONTAINS
SUBROUTINE SRFSN_LWIMP(KIDIA  ,KFDIA  ,KLON   ,KTILES   ,PTMST,LDLAND,  &
 & PSSNM1M ,PTSNM1M ,PASNM1M ,PRSNM1M ,PTSAM1M,PHLICEM1M,     &
 & PSLRFLTI,PSSRFLTI,PFRTI   ,PAHFSTI ,PEVAPTI,               &
 & PSSFC   ,PSSFL   ,PEVAPSNW,                                &
 & PTSFC   ,PTSFL   ,PUSRF   ,PVSRF   ,PTSRF,                 & 
 & YDCST   ,YDVEG   ,YDSOIL  ,YDFLAKE ,YDURB  ,YDEXC,         &
 & PSSN    ,PTSN    ,PASN    ,PRSN    ,PGSN   ,PMSN,          &
 & PEMSSN  ,PTSFCIN ,PTSFLIN ,                                &
 & PDHTSS , PDHSSS)

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_THF  , ONLY : RHOH2O
USE YOS_CST  , ONLY : TCST
USE YOS_VEG  , ONLY : TVEG
USE YOS_SOIL , ONLY : TSOIL
USE YOS_FLAKE, ONLY : TFLAKE
USE YOS_URB  , ONLY : TURB
USE YOS_EXC  , ONLY : TEXC

! (C) Copyright 1999- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!**** *SRFSN_LWIMP* - CONTAINS SNOW PARAMETRIZATION 
!
!     PURPOSE.
!     --------
!          COMPUTES CHANGES IN SNOW TEMPERATURE, DEPTH, DENSITY AND 
!          ALBEDO

!**   INTERFACE.
!     ----------
!          *SRFSN_LWIMP* IS CALLED FROM *SURFTSTP*.

!     PARAMETER   DESCRIPTION                                    UNITS
!     ---------   -----------                                    -----

!     INPUT PARAMETERS (INTEGER):
!    *KIDIA*      START POINT
!    *KFDIA*      END POINT
!    *KLON*       NUMBER OF GRID POINTS PER PACKET
!    *KTILES*     NUMBER OF SURFACE TILES

!     INPUT PARAMETERS (REAL):
!    *PTMST*      TIME STEP                                      S

!    *LDLAND*     LAND/SEA MASK (TRUE/FALSE)

!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!    *PSSNM1M*    SNOW MASS (per unit area)                    kg/m**2/s
!    *PTSNM1M*    SNOW TEMPERATURE                               K
!    *PASNM1M*    SNOW ALBEDO                                    -
!    *PRSNM1M*    SNOW DENSITY                                 KG/M3
!    *PTSAM1M*    SOIL TEMPERATURE                               K
!    *PHLICEM1M*  LAKE ICE THICKNESS                             m 
!    *PSSRFLTI*   NET SHORTWAVE RADIATION AT THE SURFACE,
!                  FOR EACH TILE                                 W/M**2
!    *PSLRFLTI*   NET LONGWAVE  RADIATION AT THE SURFACE  W/M**2
!                  For earch tile   
!    *PFRTI*      TILE FRACTIONS                                 -
!    *PAHFSTI*    TILE SENSIBLE HEAT FLUX                      W/M**2
!    *PEVAPTI*    TILE EVAPORATION                             KG/M**2/S
!    *PSSFC*      CONVECTIVE  SNOW FLUX AT THE SURFACE         KG/M**2/S
!    *PSSFL*      LARGE SCALE SNOW FLUX AT THE SURFACE         KG/M**2/S
!    *PEVAPSNW*   EVAPORATION FROM SNOW UNDER FOREST           KG/M2/S
!    *PTSFC*      Convective Throughfall at the surface        KG/M**2/S
!    *PTSFL*      Large Scale Throughfall at the surface       KG/M**2/S
!    *PUSRF*      X-COMPONENT OF WIND (LAST LEVEL)             M/S
!    *PVSRF*      Y-COMPONENT OF WIND.(LAST LEVEL)             M/S
!    *PTSRF*      TEMPERATURE (LAST LEVEL)                     K

!     PARAMETERS AT T+1 :
!    *PSSN*       SNOW MASS (per unit area)                    kg/m**2
!    *PTSN*       SNOW TEMPERATURE                               K
!    *PASN*       SNOW ALBEDO                                    -
!    *PRSN*       SNOW DENSITY                               KG/M**3

!    FLUXES FROM SNOW SCHEME:
!    *PGSN*       GROUND HEAT FLUX FROM SNOW DECK TO SOIL     W/M**2   (#)
!    *PMSN*       FLUX OF MELT WATER FROM SNOW TO SOIL       KG/M**2/S (#)
!    *PEMSSN*     EVAPORATIVE MISMATCH RESULTING FROM
!                  CLIPPING THE SNOW TO ZERO (AFTER P-E)     KG/M**2/S
!    *PTSFCIN*   Intercepted convective throughfall          KG/M**2/S
!    *PTSFLIN*   Intercepted large scale throughfall          KG/M**2/S

!     OUTPUT PARAMETERS (DIAGNOSTIC):
!    *PDHTSS*     Diagnostic array for snow T (see module yomcdh)
!    *PDHSSS*     Diagnostic array for snow mass (see module yomcdh)

! (#) THOSE TWO QUANTITIES REPRESENT THE WHOLE GRID-BOX. IN RELATION
!       TO THE DOCUMENTATION, THEY ARE PGSN=Fr_s*G_s, PMSN=Fr_s*M_s

!     METHOD.
!     -------
!     Based on the original snow (as in ERA-40) with the following updates: 
!     - Liquid water as a diagnostics
!     - Interception of rainfall 
!     - New snow density (Anderson 1976)
!     - New snow albedo evolution 
!        See Dutra et al 2009 JH for details 

!     EXTERNALS.
!     ----------

!     REFERENCE.
!     ----------
!          SEE SOIL PROCESSES' PART OF THE MODEL'S DOCUMENTATION FOR
!     DETAILS ABOUT THE MATHEMATICS OF THIS ROUTINE.

!     ORIGINAL :
!     P.VITERBO/A.BELJAARS      E.C.M.W.F.     20/02/1999
!     MODIFIED BY
!     P. Viterbo    Surface DDH for TILES      17/05/2000
!     J.F. Estrade *ECMWF* 03-10-01 move in surf vob
!     P. Viterbo     24-05-2004     Change surface units
!     P. Viterbo     24-05-2004     Change surface units
!     E. Dutra       03-2008        Add snow liquid water content as a new diagnostic 
!                                   New snow density parametrization 
!     E. Dutra       10-2009        Cleanning 
!     E. Dutra       10/10/2014    net longwave tiled 
!     F. Vana        17-Dec-2015    Support for single precision
!     J. McNorton    24/08/2022     urban tile
!     ------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM), INTENT(IN)   :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KLON
INTEGER(KIND=JPIM), INTENT(IN)   :: KTILES
LOGICAL           ,INTENT(IN)    :: LDLAND(:)

REAL(KIND=JPRB),    INTENT(IN)   :: PTMST
REAL(KIND=JPRB),    INTENT(IN)   :: PSSNM1M(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSNM1M(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PASNM1M(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PRSNM1M(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSAM1M(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PHLICEM1M(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSLRFLTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSRFLTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PFRTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PAHFSTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSFC(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSFL(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPSNW(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSFC(:) 
REAL(KIND=JPRB),    INTENT(IN)   :: PTSFL(:) 
REAL(KIND=JPRB),    INTENT(IN)   :: PUSRF(:) 
REAL(KIND=JPRB),    INTENT(IN)   :: PVSRF(:) 
REAL(KIND=JPRB),    INTENT(IN)   :: PTSRF(:) 
TYPE(TCST),         INTENT(IN)   :: YDCST
TYPE(TVEG),         INTENT(IN)   :: YDVEG
TYPE(TSOIL),        INTENT(IN)   :: YDSOIL
TYPE(TFLAKE),       INTENT(IN)   :: YDFLAKE
TYPE(TURB),         INTENT(IN)   :: YDURB
TYPE(TEXC),         INTENT(IN)   :: YDEXC
REAL(KIND=JPRB),    INTENT(OUT)  :: PSSN(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PTSN(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PASN(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PRSN(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PGSN(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PMSN(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PEMSSN(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PTSFCIN(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PTSFLIN(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PDHTSS(:,:,:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PDHSSS(:,:,:)

!      LOCAL VARIABLES

LOGICAL            :: LLNOSNOW(KLON)  

REAL(KIND=JPRB),DIMENSION(KLON)   :: ZFRSN,ZSSFC,ZSSFL,ZLWC,ZLW,ZPMSNINT,ZDSNR,ZTSFC,ZTSFL,&
                                     ZHFLUXPP,ZLICE
REAL(KIND=JPRB) :: ZFUNC,ZDFUNC,ZDSNRSTAR,ZRHOMINSN,ZDSN,ZDTH,ZDTM,&
                   ZEXPF,ZGSN,ZHFLUX,ZLINA,ZMSN,ZRS,ZRSTAR,ZSNRES,ZSOILRES,ZSSTAR,ZSSTAR1,& 
                   ZT0,ZTSTAR,ZHOICE,ZTMST,ZCONSA,ZCONSAMAX,ZCONSB,ZCONS2,ZCONSLWC,& 
                   ZRSNDT,ZRSNDTOVER,ZRSNDTDEST,ZRSNDTMELT,ZWFLUXRF,ZHFLUXRF1,ZPTSF,&
                   GRIDFRAC,ZFRSNGP,ZFRLDGP,ZRSNDTDESTC

REAL(KIND=JPRB) :: ZEPSILON
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

INTEGER(KIND=JPIM) :: JL

!     ------------------------------------------------------------------
!*         1.    SET UP SOME CONSTANTS.
!                --- -- ---- ----------
!*               PHYSICAL CONSTANTS.
!                -------- ----------
! RESISTANCE FOR HEAT CONDUCTION IN HALF TOP SOIL LAYER
!  (ESTIMATE FROM SKIN LAYER CONDUCTIVITY FOR BARE SOIL; W/M2K)
IF (LHOOK) CALL DR_HOOK('SRFSN_LWIMP_MOD:SRFSN_LWIMP',0,ZHOOK_HANDLE)
ASSOCIATE(RSNPER=>YDSOIL%RSNPER, RSNDTDESTROI=>YDSOIL%RSNDTDESTROI, &
 & RLAMICE=>YDSOIL%RLAMICE, RHOCI=>YDSOIL%RHOCI, RSNDTDESTA=>YDSOIL%RSNDTDESTA, &
 & RTEMPAMP=>YDSOIL%RTEMPAMP, RDSNMAX=>YDSOIL%RDSNMAX, &
 & RFRSMALL=>YDSOIL%RFRSMALL, RALAMSN=>YDSOIL%RALAMSN, &
 & RSNDTOVERC=>YDSOIL%RSNDTOVERC, RSNDTOVERB=>YDSOIL%RSNDTOVERB, &
 & RHOMAXSN=>YDSOIL%RHOMAXSN, RSNDTDESTC=>YDSOIL%RSNDTDESTC, &
 & RHOMINSNA=>YDSOIL%RHOMINSNA, RHOMINSNC=>YDSOIL%RHOMINSNC, &
 & RHOMINSNB=>YDSOIL%RHOMINSNB, RHOMINSND=>YDSOIL%RHOMINSND, &
 & RHOICE=>YDSOIL%RHOICE, RALFMAXSN=>YDSOIL%RALFMAXSN, RFRTINY=>YDSOIL%RFRTINY, &
 & RALFMINPSN=>YDSOIL%RALFMINPSN, RHOMINSN=>YDSOIL%RHOMINSN, &
 & RLWCSWEA=>YDSOIL%RLWCSWEA, RLWCSWEC=>YDSOIL%RLWCSWEC, &
 & RLWCSWEB=>YDSOIL%RLWCSWEB, RALFMINSN=>YDSOIL%RALFMINSN, RTAUF=>YDSOIL%RTAUF, &
 & RTAUA=>YDSOIL%RTAUA, RHOMAXSN_NEW=>YDSOIL%RHOMAXSN_NEW, &
 & RSNDTDESTB=>YDSOIL%RSNDTDESTB, RSFRESH=>YDSOIL%RSFRESH, &
 & RSNDTOVERA=>YDSOIL%RSNDTOVERA, &
 & LEFLAKE=>YDFLAKE%LEFLAKE, LEURBAN=>YDURB%LEURBAN, RH_ICE_MIN_FLK=>YDFLAKE%RH_ICE_MIN_FLK, &
 & RG=>YDCST%RG, RDAY=>YDCST%RDAY, RLVTT=>YDCST%RLVTT, RTT=>YDCST%RTT, &
 & RPI=>YDCST%RPI, RLSTT=>YDCST%RLSTT, RLMLT=>YDCST%RLMLT, &
 & RVLAMSK=>YDVEG%RVLAMSK, LELWTL=>YDEXC%LELWTL)

! Security constant
ZEPSILON=100._JPRB*TINY(1._JPRB)

ZHOICE=1.0_JPRB/RHOICE
ZSOILRES=1.0_JPRB/RVLAMSK(8)
ZT0=RTT
ZTMST=1.0_JPRB/PTMST
ZEXPF=EXP(-RTAUF*PTMST/RDAY)
ZLINA=RTAUA*PTMST/RDAY

!*   FIND LAKE POINTS WITH ICE COVER
DO JL=KIDIA,KFDIA
  IF ( PHLICEM1M(JL) > RH_ICE_MIN_FLK  .AND. LEFLAKE ) THEN
    ZLICE(JL)=1._JPRB
  ELSE 
    ZLICE(JL)=0._JPRB
  ENDIF
ENDDO

!     ------------------------------------------------------------------
!*         2. NEW SNOW T AND MASS INCLUDING GROUND HEAT FLUX AND MELTING.
!             -----------------------------------------------------------

DO JL=KIDIA,KFDIA
  ZFRSNGP=PFRTI(JL,5)+PFRTI(JL,7) ! snow fraction of the grid-box 
  ZFRLDGP=ZFRSNGP+PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8) ! land fraction of the grib-box 
  IF ( LEURBAN ) THEN
   ZFRLDGP=ZFRSNGP+PFRTI(JL,3)+PFRTI(JL,4)+PFRTI(JL,6)+PFRTI(JL,8)+PFRTI(JL,10)
  ENDIF
  IF (LDLAND(JL)) THEN
    GRIDFRAC=1._JPRB
    ! snow cover fraction is normalized by the  land fraction 
!   ZFRSN(JL)=MAX(ZFRSNGP/ZFRLDGP,RFRTINY)
    ZFRSN(JL)=MAX(ZFRSNGP,RFRTINY)  ! to be replaced by the line above in 42r cycles
  ELSE
    GRIDFRAC=0._JPRB
    ZFRSN(JL)=MAX(ZFRSNGP,RFRTINY)  ! it should be zero!
  ENDIF
  
  IF (ZFRSN(JL) < RFRSMALL) THEN
    LLNOSNOW(JL)=.TRUE.
  ELSE
    LLNOSNOW(JL)=.FALSE.
  ENDIF
    
  ZSSFC(JL)=GRIDFRAC*PSSFC(JL)
  ZSSFL(JL)=GRIDFRAC*PSSFL(JL)
  ZTSFC(JL)=GRIDFRAC*PTSFC(JL)
  ZTSFL(JL)=GRIDFRAC*PTSFL(JL)

ENDDO

DO JL=KIDIA,KFDIA
  PEMSSN(JL)=0.0_JPRB
  PTSFCIN(JL)=0.0_JPRB
  PTSFLIN(JL)=0.0_JPRB
ENDDO

DO JL=KIDIA,KFDIA
  IF (LLNOSNOW(JL)) THEN
    IF ( YDSOIL%LEWBSOILFIX ) THEN
      ZSSTAR=PSSNM1M(JL)+(PTMST)*(ZSSFC(JL)+ZSSFL(JL)+PFRTI(JL,5)*PEVAPTI(JL,5)&
             & +PFRTI(JL,7)*PEVAPSNW(JL))
    ELSE
      ZSSTAR=PSSNM1M(JL)+(PTMST)*(ZSSFC(JL)+ZSSFL(JL))
    ENDIF
    IF ( LDLAND(JL) ) THEN
      PSSN(JL)=ZSSTAR
    ELSE
      PSSN(JL)=0.0_JPRB
    ENDIF
    PTSN(JL)=ZT0
    PGSN(JL)=0.0_JPRB
    PMSN(JL)=0.0_JPRB
    IF (SIZE(PDHTSS) > 0) THEN
      PDHTSS(JL,1,1)=0.0_JPRB
      PDHTSS(JL,1,2)=PTSNM1M(JL)
      PDHTSS(JL,1,3)=0.0_JPRB
      PDHTSS(JL,1,4)=0.0_JPRB
      PDHTSS(JL,1,11)=0.0_JPRB
      PDHTSS(JL,1,12)=0.0_JPRB
      PDHTSS(JL,1,15)=0.0_JPRB
      PDHSSS(JL,1,4)=0._JPRB

!      PDHSSS(JL,1,6)=0._JPRB
!      PDHSSS(JL,1,7)=0._JPRB
!      PDHTSS(JL,1,16)=0._JPRB
    ENDIF

    ZLWC(JL)=0.0_JPRB
    ZLW(JL)=0.0_JPRB
    ZHFLUXPP(JL)=0._JPRB ! ONLY FOR DDH
  ELSE


!           NET HEAT FLUX AT SNOW SURFACE; THIS IS MULTIPLIED 
!           BY FRACTION BECAUSE EQUATIONS APPLY TO TOTAL SNOW MASS 
!           IN THE GRID SQUARE

    ZHFLUX=( PFRTI(JL,5)*(PAHFSTI(JL,5)+RLSTT*PEVAPTI(JL,5))&
     & +PFRTI(JL,7)*(PAHFSTI(JL,7)+RLVTT*(PEVAPTI(JL,7)-PEVAPSNW(JL))&
     & +RLSTT*PEVAPSNW(JL))&
     & +PFRTI(JL,5)*PSSRFLTI(JL,5)&
     & +PFRTI(JL,7)*PSSRFLTI(JL,7)&
     & +PFRTI(JL,5)*PSLRFLTI(JL,5)&
     & +PFRTI(JL,7)*PSLRFLTI(JL,7)&
     & )/ZFRSN(JL)
    ZHFLUXPP(JL)=ZHFLUX ! ONLY FOR DDH

! PRELIMINARY SNOW MASS
    ZSSTAR=PSSNM1M(JL)+(PTMST)*&
     & (ZSSFC(JL)+ZSSFL(JL)+PFRTI(JL,5)*PEVAPTI(JL,5)&
     & +PFRTI(JL,7)*PEVAPSNW(JL))  
    ! CORRECT FOR NEGATIVE VALUES OF SNOW MASS AFTER P-E
    IF (ZSSTAR < 0.0_JPRB) THEN
      PEMSSN(JL)=ZTMST*ZSSTAR
      ZSSTAR=MAX(ZSSTAR,0.0_JPRB)
    ENDIF


! SNOW LIQUID WATER - IMPLICIT 
    ! SNOW DEPTH (REAL)
    ZDSNR(JL)=PSSNM1M(JL)/(PRSNM1M(JL)*ZFRSN(JL))
    ZDSNRSTAR=ZSSTAR/(PRSNM1M(JL)*ZFRSN(JL))
    ! SNOW LIQUID WATER CAPACITY
    ZCONSLWC=MAX(0._JPRB,(RLWCSWEA-PRSNM1M(JL))/RLWCSWEA)
    ZLWC(JL)=PSSNM1M(JL)*(RLWCSWEB+(RLWCSWEC-RLWCSWEB)*ZCONSLWC)
    ! LWC IS RESCALLED CONSIDERING EFFECTIVE DEPTH (RDSNMAX)
    ZLWC(JL)=ZLWC(JL)*MIN(ZDSNR(JL),RDSNMAX)/ZDSNR(JL)
    ! ANALYTICAL FUNCTIONS - SNOW LIQUID WATER
    IF ( PTSNM1M(JL) < ZT0-0.5_JPRB*RTEMPAMP ) THEN
      ZFUNC=0._JPRB
      ZDFUNC=0._JPRB
    ELSEIF(PTSNM1M(JL) < ZT0) THEN
      ZFUNC=(1._JPRB+SIN(RPI*(PTSNM1M(JL)-ZT0)/(RTEMPAMP)))
      ZDFUNC=cos(RPI*(PTSNM1M(JL)-ZT0)/(RTEMPAMP))*RPI/RTEMPAMP
    ELSE
      ZFUNC=0._JPRB
      ZDFUNC=RPI/RTEMPAMP
    ENDIF
    ZLW(JL)=ZFUNC*ZLWC(JL)

! TERMS FOR IMPLICIT TEMPERATURE EQUATION 
    ZCONSB=RLMLT*ZLWC(JL)*ZDFUNC
    ZCONSAMAX=(PRSNM1M(JL)*RHOCI*RDSNMAX)*ZHOICE
    ZCONSA=MIN(((PSSNM1M(JL))*RHOCI*ZHOICE)/ZFRSN(JL),ZCONSAMAX)
    ZCONS2=PTMST/(ZCONSA+ZCONSB) 

! REAL SNOW DEPTH 
    ZDSN=MIN(PSSNM1M(JL)/(PRSNM1M(JL)*ZFRSN(JL)),RDSNMAX)
    ! RESISTANCE FOR HEAT FLUX BETWEEN SNOW AND SOIL LAYER 
    ZSNRES=0.5_JPRB*ZDSN/(RLAMICE*(PRSNM1M(JL)*ZHOICE)**RALAMSN)
    ZRS=1.0_JPRB/(ZSNRES+ZSOILRES)

!  ACCOUNT FOR THROUGHFALL INTERCEPTION 
    ! mass advected
    ZWFLUXRF=(PFRTI(JL,5)+PFRTI(JL,7))*(ZTSFC(JL)+ZTSFL(JL))
    ! phase changes
    ZWFLUXRF=MIN(ZWFLUXRF*PTMST,ZLWC(JL)*(1.0_JPRB-ZFUNC))
    ZHFLUXRF1=MIN(ZCONSA*(ZT0-PTSNM1M(JL))*ZTMST,&
                  RLMLT*ZWFLUXRF*ZTMST)
    ZHFLUXRF1=MAX(ZHFLUXRF1,0.0_JPRB)
    !mass intercepted - recalculate from ZHFLUXRF1
    ZWFLUXRF=ZHFLUXRF1/RLMLT
    ZSSTAR=ZSSTAR+ZWFLUXRF*PTMST
    !CHANGE THE THROUGHFALL AT THE SURFACE - remove ZWFLUXRF
    IF ( ZWFLUXRF .GT. 0.0_JPRB ) THEN
      ZPTSF=PTSFC(JL)+PTSFL(JL)
      PTSFCIN(JL)=MAX(0._JPRB,ZWFLUXRF*PTSFC(JL)/ZPTSF)
      PTSFLIN(JL)=MAX(0._JPRB,ZWFLUXRF*PTSFL(JL)/ZPTSF)
    ENDIF
  
! SOLVE IMPLICIT TEMPERATURE EQUATION
    ZTSTAR=(PTSNM1M(JL)+ZCONS2*(ZHFLUX+ZHFLUXRF1+PTSAM1M(JL,1)*ZRS))/(1.0_JPRB+ZCONS2*ZRS)

    IF (ZTSTAR <= ZT0) THEN
! NO MELTING OF SNOW
      PTSN(JL)=ZTSTAR
      PSSN(JL)=ZSSTAR
      PGSN(JL)=(ZTSTAR-PTSAM1M(JL,1))*ZRS
      PMSN(JL)=0.0_JPRB
    ELSE
! MELTING OF SNOW: 
! HEATING PART OF THE TIME STEP
      ZDTH=((ZT0-PTSNM1M(JL))*(ZCONSA+ZCONSB))/SIGN(MAX(ZEPSILON, &
       & ABS(ZHFLUX+ZHFLUXRF1-(ZT0-PTSAM1M(JL,1))*ZRS)), &
       & ZHFLUX+ZHFLUXRF1-(ZT0-PTSAM1M(JL,1))*ZRS )
! MELTING PART OF THE TIME STEP
      ZDTM=PTMST-ZDTH
      ZGSN=(ZT0-PTSAM1M(JL,1))*ZRS
      ZMSN=(ZHFLUX+ZHFLUXRF1-ZGSN)/RLMLT
      ZSSTAR1=ZSSTAR-(ZDTM)*ZMSN*ZFRSN(JL)
      IF (ZSSTAR1 >= 0.0_JPRB) THEN
! NOT ALL THE SNOW HAS BEEN MELTED
        PTSN(JL)=ZT0
        PSSN(JL)=ZSSTAR1
        PGSN(JL)=(ZT0-PTSAM1M(JL,1))*ZRS
        PMSN(JL)=ZMSN*ZDTM*ZTMST
      ELSE
! ALL THE SNOW HAS BEEN MELTED; COMPUTE TIME IT TOOK TO MELT
        ZDTM=ZSSTAR/(ZMSN*ZFRSN(JL))
        PTSN(JL)=ZT0
        PSSN(JL)=0.0_JPRB
        PGSN(JL)=( ((ZT0-PTSAM1M(JL,1))*ZRS)*(ZDTM+ZDTH)&
         & +(ZHFLUX+ZHFLUXRF1)*(PTMST-ZDTM-ZDTH) )*ZTMST    
        PMSN(JL)=ZMSN*ZDTM*ZTMST
      ENDIF
    ENDIF

! INTERNAL MELTING/REFREEZING 
    ZPMSNINT(JL)=ZCONSB*ZFRSN(JL)*(PTSN(JL)-PTSNM1M(JL))/PTMST

! DDH diagnostics.
    IF (SIZE(PDHTSS) > 0 .AND. SIZE(PDHSSS) > 0) THEN
! Snow heat capacity per unit surface
      PDHTSS(JL,1,1)=(ZCONSA+ZCONSB)*ZFRSN(JL)
! Snow temperature
      PDHTSS(JL,1,2)=PTSNM1M(JL)
! Snow energy per unit surface
      PDHTSS(JL,1,3)=(ZCONSA+ZCONSB)*ZFRSN(JL)*PTSNM1M(JL)
! Snow thermally active depth
      PDHTSS(JL,1,4)=ZDSN*ZFRSN(JL)
! Snow sensible heat flux
      PDHTSS(JL,1,11)=PFRTI(JL,5)*PAHFSTI(JL,5)+PFRTI(JL,7)*PAHFSTI(JL,7)
! Snow latent heat flux
      PDHTSS(JL,1,12)=PFRTI(JL,5)*RLSTT*PEVAPTI(JL,5)+&
        & PFRTI(JL,7)*(RLVTT*(PEVAPTI(JL,7)-PEVAPSNW(JL))+&
        & RLSTT*PEVAPSNW(JL))
! Snow heat content change
      PDHTSS(JL,1,15)=PDHTSS(JL,1,1)*(PTSN(JL)-PTSNM1M(JL))/PTMST 
! Snow evaporation
      PDHSSS(JL,1,4)=PFRTI(JL,5)*PEVAPTI(JL,5)+&
        & PFRTI(JL,7)*PEVAPSNW(JL)  
    
! Extra DDH 
  ! Mass advected (intercepted) by THROUGHFALL
!      PDHSSS(JL,1,6)=ZWFLUXRF  ! FLUX UNITS -kg.m-2.s-1
  ! diagnostic liquid water content 
!      PDHSSS(JL,1,7)=ZLW(JL)  ! FLUX UNITS -kg.m-2.s-1
  ! PHASE CHANGES INDUCED BY FREEZING of THROUGHFALL 
! PDHTSS(JL,1,16)=ZFRSN(JL)*ZHFLUXRF1
    ENDIF
    IF ( .NOT. LELWTL ) THEN
      PTSN(JL) = MAX(180.0_JPRB,PTSN(JL))  ! Limit snow temperature to a reasonable value (minimum observed temperature 
    ENDIF
  ENDIF
ENDDO

!*         3. NEW SNOW ALBEDO AND DENSITY.
!             ----------------------------
ZRSNDTDESTC=460._JPRB ! Original value up to CY43
DO JL=KIDIA,KFDIA
  IF (LLNOSNOW(JL)) THEN
    PASN(JL)=RALFMAXSN
    PRSN(JL)=RHOMINSN
  ELSE

!*        3.1 NEW SNOW DENSITY. 
!             -----------------

  ! NEW SNOW DENSITY (INCLUDE FRESH SNOW)
    ZRHOMINSN=RHOMINSNA + RHOMINSNB*(PTSRF(JL)-ZT0) + &
      & RHOMINSNC*(PUSRF(JL)**2+PVSRF(JL)**2)**0.25_JPRB
    ZRHOMINSN=MAX(ZRHOMINSN,RHOMINSND)
    ! Original formulation : weighted on snow mass 
    !ZRSTAR=(PSSNM1M(JL)*PRSNM1M(JL)&
    !  & +PTMST*(ZSSFC(JL)+ZSSFL(JL))*ZRHOMINSN)&
    !  & /(PSSNM1M(JL)+PTMST*(ZSSFC(JL)+ZSSFL(JL)))
    
    ! New formulation, weighted on snow depth
    ZRSTAR=( PSSNM1M(JL) + PTMST*(ZSSFC(JL)+ZSSFL(JL)) ) / &
           ( PTMST*(ZSSFC(JL)+ZSSFL(JL))/ZRHOMINSN + PSSNM1M(JL)/PRSNM1M(JL) )

  ! COMPACTATION DUE TO OVERBURNING 
    ZRSNDTOVER=0.5*PSSNM1M(JL)*RG/(RSNDTOVERA*&
      & EXP(RSNDTOVERB*(ZT0-PTSN(JL))+RSNDTOVERC*PRSNM1M(JL)))
  ! COMPACTATION DUE TO DESTRUCTIVE METAMORPHISM
    ZRSNDTDEST=RSNDTDESTA*EXP(-RSNDTDESTB*(ZT0-PTSN(JL))-&
      & ZRSNDTDESTC*MAX(0.0_JPRB,PRSNM1M(JL)-RSNDTDESTROI))
  ! COMPACTATION DUE TO MELTING 
    IF (ZPMSNINT(JL) > 0.0_JPRB .AND. PSSNM1M(JL) >= 15.0_JPRB) THEN 
      ZRSNDTMELT=ZTMST*(ZPMSNINT(JL)*PTMST/RLMLT)/(PSSNM1M(JL)-ZLW(JL))
    ELSE
      ZRSNDTMELT=0.0_JPRB
    ENDIF
  ! TOTAL COMPACTATION 
    ZRSNDT=ZRSNDTOVER+ZRSNDTDEST+ZRSNDTMELT
    ! NEW DENSITY 
    PRSN(JL)=ZRSTAR+ZRSTAR*ZRSNDT*PTMST
    PRSN(JL)=MIN(450._JPRB,PRSN(JL))
    PRSN(JL)=MAX(RHOMINSN,PRSN(JL))
!*        3.2 NEW SNOW ALBEDO.
!             -----------------

    IF (PMSN(JL) > 0.0_JPRB .OR. ZPMSNINT(JL) > 0.0_JPRB .OR. PTSNM1M(JL) > ZT0-2._JPRB ) THEN
    ! MELTING CONDITIONS
      PASN(JL)=(PASNM1M(JL)-RALFMINSN)*ZEXPF+RALFMINSN
    ELSE
    ! NORMAL CONDITIONS
      PASN(JL)=MAX(RALFMINSN,PASNM1M(JL)-ZLINA)
    ENDIF
    ! UPDATE ALBEDO DUE TO SNOWFALL EVENTS
    PASN(JL)=PASN(JL)+ MIN(MAX((ZSSFC(JL)+ZSSFL(JL))*PTMST , 0._JPRB)/(10._JPRB) , 1._JPRB) *(RALFMAXSN-PASN(JL))
    PASN(JL)=MIN(RALFMAXSN,MAX(PASN(JL),RALFMINSN))
    
  ENDIF
ENDDO

!*         5. NORMALIZE QUANTITIES TO THE GRID-SQUARE.
!             ----------------------------------------

DO JL=KIDIA,KFDIA
  PGSN(JL)=ZFRSN(JL)*PGSN(JL)
  PMSN(JL)=ZFRSN(JL)*PMSN(JL)

!*         6. DDH diagnostics.
!             --- ------------
  IF (SIZE(PDHTSS) > 0 .AND. SIZE(PDHSSS) > 0) THEN
! Snow density
    PDHTSS(JL,1,5)=PRSNM1M(JL)
! Snow basal flux
    PDHTSS(JL,1,13)=PGSN(JL)
! Snow phase change energy
    PDHTSS(JL,1,14)=-RLMLT*PMSN(JL)

! Snow mass
    PDHSSS(JL,1,1)=PSSNM1M(JL)
! Large-scale snowfall
    PDHSSS(JL,1,2)=ZSSFL(JL)
! Convective snowfall
    PDHSSS(JL,1,3)=ZSSFC(JL)
! Snow melt
    PDHSSS(JL,1,5)=-PMSN(JL)
  ENDIF

ENDDO

!CALL DDH_BUDGET 

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SRFSN_LWINP_MOD:SRFSN_LWIMP',1,ZHOOK_HANDLE)


CONTAINS 

SUBROUTINE DDH_BUDGET
IMPLICIT NONE
REAL(KIND=JPRB) :: ZZTMP,ZZTMP1,ZZTMP2
LOGICAL FIRSTCALL
INTEGER TSTEP
INTEGER JL,JK

DATA FIRSTCALL/.TRUE./
SAVE FIRSTCALL,TSTEP

JL=1
JK=1
IF ( FIRSTCALL ) THEN	
  FIRSTCALL=.FALSE.
  TSTEP=-1
ENDIF  

TSTEP=TSTEP+1

  ZZTMP=( PDHSSS(JL,1,2)+PDHSSS(JL,1,3)+PDHSSS(JL,1,6)+PDHSSS(JL,1,4)+PDHSSS(JL,1,5) -PEMSSN(JL) )*PTMST
  ZZTMP1=ZZTMP-(PSSN(JL)-PSSNM1M(JL))
  ZZTMP2=ZHFLUXPP(JL)
  ZZTMP=ZFRSN(JL)*ZZTMP2 + PDHTSS(JL,1,14) - PDHTSS(JL,1,13) +PDHTSS(JL,1,16)
  ZZTMP1=ZZTMP-PDHTSS(JL,1,15)

END SUBROUTINE DDH_BUDGET 

END SUBROUTINE SRFSN_LWIMP

END MODULE SRFSN_LWIMP_MOD
