MODULE SURFSEBSTL_CTL_MOD
CONTAINS
SUBROUTINE SURFSEBSTL_CTL(KIDIA,KFDIA,KLON,KTILES,KTVL,KTVH,&
           &PTMST,PSSKM1M5,PTSKM1M5,PQSKM1M5,PDQSDT5,PRHOCHU5,PRHOCQU5,&
           &PALPHAL5,PALPHAS5,PSSRFL5,PFRTI5,PTSRF5,PSNS5,PRSN5,&
           &PSLRFL5,PTSKRAD5,PEMIS5,PASL5,PBSL5,PAQL5,PBQL5,&
           &PJS5,PJQ5,PSSK5,PTSK5,PSSH5,PSLH5,PSTR5,PG05,&
           &PSL5,PQL5, &
           &PSSKM1M,PTSKM1M,PQSKM1M,PDQSDT,PRHOCHU,PRHOCQU,&
           &PALPHAL,PALPHAS,PSSRFL,PTSRF,&
           &PSLRFL,PTSKRAD,PASL,PBSL,PAQL,PBQL,&
           &YDCST,YDEXC,YDVEG,YDURB,YDSOIL,&
!out
           &PJS,PJQ,PSSK,PTSK,PSSH,PSLH,PSTR,PG0,&
           &PSL,PQL)

USE PARKIND1  , ONLY : JPIM, JPRB
USE YOMHOOK   , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_THF   , ONLY : RVTMP2
USE YOS_CST   , ONLY : TCST
USE YOS_EXC   , ONLY : TEXC
USE YOS_VEG   , ONLY : TVEG
USE YOS_URB   , ONLY : TURB
USE YOS_SOIL  , ONLY : TSOIL
! (C) Copyright 2003- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!------------------------------------------------------------------------

!  PURPOSE:
!    VDFSEBTL computes surface energy balance and skin temperature 
!             for each tile. 
!             (Tangent linear)

!  VDFSEBTL is called by VDFDIFHTL

!  METHOD:
!    A linear relation between lowest model level dry static 
!    energy and moisture and their fluxes is specified as input. 
!    The surface energy balance equation is used to eliminate 
!    the skin temperature as in the derivation of the 
!    Penmann-Monteith equation. 

!    The routine can also be used in stand alone simulations by
!    putting PASL and PAQL to zero and by specifying for PBSL and PBQL 
!    the forcing with dry static energy and specific humidity. 

!  AUTHOR of TL routine:
!    M. Janiskova       ECMWF September 2003   

!  REVISION HISTORY:
!    P. Viterbo         ECMWF March 2004      move to surf vob
!    G. Balsamo/A. Beljaars 09-08-2013 snow scheme stability fix
!    I. Sandu    24-02-2014  Lambda skin values by vegetation type instead of tile
!    J. McNorton           24/08/2022  urban tile

!  INTERFACE: 

!    Integers (In):
!      KIDIA   :    Begin point in arrays
!      KFDIA   :    End point in arrays
!      KLON    :    Length of arrays
!      KTILES  :    Number of tiles
!      KTVL    :    Dominant low vegetation type
!      KTVH    :    Dominant high vegetation type

!    INPUT PARAMETERS 

!    Reals with tile index (In): 
!      PTMST    :    Time Step
!      PSSKM1M5 :    Dry static energy of skin at T-1           (Trajectory)
!      PTSKM1M5 :    Skin temperature at T-1                    (Trajectory)
!      PQSKM1M5 :    Saturation specific humidity at PTSKM1M    (Trajectory)
!      PDQSDT5  :    dqsat/dT at PTSKM1M                        (Trajectory)
!      PRHOCHU5 :    Rho*Ch*|U|                                 (Trajectory)
!      PRHOCQU5 :    Rho*Cq*|U|                                 (Trajectory)
!      PALPHAL5 :    multiplier of ql in moisture flux eq.      (Trajectory)
!      PALPHAS5 :    multiplier of qs in moisture flux eq.      (Trajectory)
!      PSSRFL5  :    Net short wave radiation at the surface    (Trajectory)
!      PFRTI5   :    Fraction of surface area of each tile      (Trajectory)
!      PTSRF5   :    Surface temp. below skin (e.g. Soil or SST)(Trajectory) 
!      PSNS5    :    Snow mass per unit area                    (Trajectory)
!      PSNS5    :    Snow density                               (Trajectory)

!    Reals independent of tiles (In):
!      PSLRFL5  :    Net long wave radiation at the surface     (Trajectory) 
!      PTSKRAD5 :    Mean skin temp. at radiation time level    (Trajectory)
!      PEMIS5   :    Surface emissivity                         (Trajectory)
!      PASL5    :    Asl in Sl=Asl*Js+Bsl                       (Trajectory)
!      PBSL5    :    Bsl in Sl=Asl*Js+Bsl                       (Trajectory)
!      PAQL5    :    Aql in Ql=Aql*Jq+Bql                       (Trajectory)
!      PBQL5    :    Bql in Ql=Aql*Jq+Bql                       (Trajectory)


!    Reals with tile index (In): 
!      PSSKM1M :    Dry static energy of skin at T-1           (J/kg)
!      PTSKM1M :    Skin temperature at T-1                    (K)
!      PQSKM1M :    Saturation specific humidity at PTSKM1M    (kg/kg)
!      PDQSDT  :    dqsat/dT at PTSKM1M                        (kg/kg K)
!      PRHOCHU :    Rho*Ch*|U|                                 (kg/m2s)
!      PRHOCQU :    Rho*Cq*|U|                                 (kg/m2s)
!      PALPHAL :    multiplier of ql in moisture flux eq.      (-)
!      PALPHAS :    multiplier of qs in moisture flux eq.      (-)
!      PSSRFL  :    Net short wave radiation at the surface    (W/m2)
!      PTSRF   :    Surface temp. below skin (e.g. Soil or SST)  (K) 

!    Reals independent of tiles (In):
!      PSLRFL  :    Net long wave radiation at the surface     (W/m2) 
!      PTSKRAD :    Mean skin temp. at radiation time level    (K)
!      PASL    :    Asl in Sl=Asl*Js+Bsl                       (m2s/kg)
!      PBSL    :    Bsl in Sl=Asl*Js+Bsl                       (J/kg)
!      PAQL    :    Aql in Ql=Aql*Jq+Bql                       (m2s/kg)
!      PBQL    :    Bql in Ql=Aql*Jq+Bql                       (kg/kg)


!     OUTPUT PARAMETERS 

!    Reals with tile index (Out):
!      PJS5     :    Flux of dry static energy                  (Trajectory)
!      PQS5     :    Moisture flux                              (Trajectory)
!      PSSK5    :    New dry static energy of skin              (Trajectory)
!      PTSK5    :    New skin temperature                       (Trajectory)
!      PSSH5    :    Surface sensible heat flux                 (Trajectory)
!      PSLH5    :    Surface latent heat flux                   (Trajectory)
!      PSTR5    :    Surface net thermal radiation              (Trajectory)
!      PG05     :    Surface ground heat flux (solar radiation  (Trajectory)
!                   leakage is not included in this term)

!    Reals independent of tiles (Out):
!      PSL5     :    New lowest model level dry static energy   (Trajectory)
!      PQL5     :    New lowest model level specific humidity   (Trajectory)

!    Reals with tile index (Out):
!      PJS     :    Flux of dry static energy                  (W/m2)
!      PQS     :    Moisture flux                              (kg/m2s)
!      PSSK    :    New dry static energy of skin              (J/kg)
!      PTSK    :    New skin temperature                       (K)
!      PSSH    :    Surface sensible heat flux                 (W/m2)
!      PSLH    :    Surface latent heat flux                   (W/m2)
!      PSTR    :    Surface net thermal radiation              (W/m2)
!      PG0     :    Surface ground heat flux (solar radiation  (W/m2)
!                   leakage is not included in this term)

!    Reals independent of tiles (Out):
!      PSL     :    New lowest model level dry static energy   (J/kg)
!      PQL     :    New lowest model level specific humidity   (kg/kg)


!  DOCUMENTATION:
!    See Physics Volume of IFS documentation
!    This routine uses the method suggested by Polcher and Best
!    (the basic idea is to start with a linear relation between 
!     the lowest model level varibles and their fluxes, which is 
!     obtained after the downward elimination of the vertical 
!     diffusion tridiagonal matrix). 

!------------------------------------------------------------------------



IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM), INTENT(IN)  :: KLON
INTEGER(KIND=JPIM), INTENT(IN)  :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)  :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)  :: KTILES
INTEGER(KIND=JPIM), INTENT(IN)  :: KTVL(KLON) 
INTEGER(KIND=JPIM), INTENT(IN)  :: KTVH(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PTMST

REAL(KIND=JPRB),    INTENT(IN)  :: PSSKM1M5(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PTSKM1M5(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PQSKM1M5(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PDQSDT5(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PRHOCHU5(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PRHOCQU5(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PALPHAL5(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PALPHAS5(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PSSRFL5(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PFRTI5(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PTSRF5(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PSNS5(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PRSN5(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PSLRFL5(:)
REAL(KIND=JPRB),    INTENT(IN)  :: PTSKRAD5(:)
REAL(KIND=JPRB),    INTENT(IN)  :: PEMIS5(:)
REAL(KIND=JPRB),    INTENT(IN)  :: PASL5(:)
REAL(KIND=JPRB),    INTENT(IN)  :: PBSL5(:)
REAL(KIND=JPRB),    INTENT(IN)  :: PAQL5(:)
REAL(KIND=JPRB),    INTENT(IN)  :: PBQL5(:)
TYPE(TCST),         INTENT(IN)  :: YDCST
TYPE(TEXC),         INTENT(IN)  :: YDEXC
TYPE(TVEG),         INTENT(IN)  :: YDVEG
TYPE(TURB),         INTENT(IN)  :: YDURB
TYPE(TSOIL),        INTENT(IN)  :: YDSOIL
REAL(KIND=JPRB),    INTENT(OUT) :: PJS5(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PJQ5(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PSSK5(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PTSK5(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PSSH5(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PSLH5(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PSTR5(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PG05(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PSL5(:)
REAL(KIND=JPRB),    INTENT(OUT) :: PQL5(:)

REAL(KIND=JPRB),    INTENT(IN)  :: PSSKM1M(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PTSKM1M(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PQSKM1M(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PDQSDT(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PRHOCHU(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PRHOCQU(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PALPHAL(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PALPHAS(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PSSRFL(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PTSRF(:,:)
REAL(KIND=JPRB),    INTENT(IN)  :: PSLRFL(:)
REAL(KIND=JPRB),    INTENT(IN)  :: PTSKRAD(:)
REAL(KIND=JPRB),    INTENT(IN)  :: PASL(:)
REAL(KIND=JPRB),    INTENT(IN)  :: PBSL(:)
REAL(KIND=JPRB),    INTENT(IN)  :: PAQL(:)
REAL(KIND=JPRB),    INTENT(IN)  :: PBQL(:)
REAL(KIND=JPRB),    INTENT(OUT) :: PJS(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PJQ(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PSSK(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PTSK(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PSSH(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PSLH(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PSTR(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PG0(:,:)
REAL(KIND=JPRB),    INTENT(OUT) :: PSL(:)
REAL(KIND=JPRB),    INTENT(OUT) :: PQL(:)


!        Local variables

REAL(KIND=JPRB) ::   ZEJS15(KLON),ZEJS25(KLON),ZEJS45(KLON),&
          & ZEJQ15(KLON),ZEJQ25(KLON),ZEJQ45(KLON),&
          & ZDLWDT5(KLON)
REAL(KIND=JPRB) ::   ZCJS15(KLON,KTILES),ZCJS35(KLON,KTILES),&
          & ZCJQ25(KLON,KTILES),ZCJQ35(KLON,KTILES),&
          & ZCJQ45(KLON,KTILES),ZDSS15(KLON,KTILES),&
          & ZDSS25(KLON,KTILES),ZDSS45(KLON,KTILES),&
          & ZDJS15(KLON,KTILES),&
          & ZDJS25(KLON,KTILES),ZDJS45(KLON,KTILES),&
          & ZDJQ15(KLON,KTILES),ZDJQ25(KLON,KTILES),&
          & ZDJQ45(KLON,KTILES),ZCPTM15(KLON,KTILES),&
          & ZICPTM15(KLON,KTILES)

REAL(KIND=JPRB) ::   ZEJS1(KLON),ZEJS2(KLON),ZEJS4(KLON),&
          & ZEJQ1(KLON),ZEJQ2(KLON),ZEJQ4(KLON),&
          & ZDLWDT(KLON), ZSTABEXSN(KLON)
REAL(KIND=JPRB) ::   ZCJS1(KLON,KTILES),ZCJS3(KLON,KTILES),&
          & ZCJQ2(KLON,KTILES),ZCJQ3(KLON,KTILES),&
          & ZCJQ4(KLON,KTILES),ZDSS1(KLON,KTILES),&
          & ZDSS2(KLON,KTILES),ZDSS4(KLON,KTILES),&
          & ZDJS1(KLON,KTILES),&
          & ZDJS2(KLON,KTILES),ZDJS4(KLON,KTILES),&
          & ZDJQ1(KLON,KTILES),ZDJQ2(KLON,KTILES),&
          & ZDJQ4(KLON,KTILES),ZCPTM1(KLON,KTILES),&
          & ZICPTM1(KLON,KTILES),ZLAMSK(KLON,KTILES)

INTEGER(KIND=JPIM) :: JL,JT
REAL(KIND=JPRB) :: ZDELTA,ZLARGE,ZLARGESN,ZRTTMEPS,ZLAM,ZFRSR,ZLARGEWT
REAL(KIND=JPRB) :: ZCOEF15,ZIZZ5
REAL(KIND=JPRB) :: ZZ5,ZZ15,ZZ25,ZZ35,ZY15,ZY25,ZY35,ZJS5,ZJQ5
REAL(KIND=JPRB) :: ZCOEF1,ZIZZ 
REAL(KIND=JPRB) :: ZZ,ZZ1,ZZ2,ZZ3,ZY1,ZY2,ZY3,ZJS,ZJQ
REAL(KIND=JPRB) :: ZSNOW,ZSNOWHVEG,ZSNOW_GLACIER
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


! ------------------------------------------------------------------------



!      1. Initialize constants

IF (LHOOK) CALL DR_HOOK('SURFSEBSTL_CTL_MOD:SURFSEBSTL_CTL',0,ZHOOK_HANDLE)
ASSOCIATE(RCPD=>YDCST%RCPD, RLSTT=>YDCST%RLSTT, RLVTT=>YDCST%RLVTT, &
 & RSIGMA=>YDCST%RSIGMA, RTT=>YDCST%RTT, &
 & LELWDD=>YDEXC%LELWDD, &
 & RHOCI=>YDSOIL%RHOCI, RHOICE=>YDSOIL%RHOICE, RQSNCR=>YDSOIL%RQSNCR, &
 & RVLAMSK=>YDVEG%RVLAMSK, RVLAMSKS=>YDVEG%RVLAMSKS, RVTRSR=>YDVEG%RVTRSR,RURBTC=>YDURB%RURBTC)
ZDELTA=RVTMP2              ! moisture coeff. in cp  
ZLARGE=1.E10_JPRB          ! large number to impose Tsk=SST
ZLARGESN=50._JPRB          ! large number to constrain Tsk variations in case
                           !   of melting snow 
ZLARGEWT=20._JPRB          ! 1/lamdaSK(w)+1/lamdaSK(tvh)                           
ZRTTMEPS=RTT-0.2_JPRB      ! slightly below zero to start snow melt
ZSNOW=7._JPRB
ZSNOW_GLACIER=8._JPRB
ZSNOWHVEG=20._JPRB


!      2. Prepare tile independent arrays

IF (LELWDD) THEN
  DO JL=KIDIA,KFDIA
    ZDLWDT (JL)=-12._JPRB*PEMIS5(JL)*RSIGMA*PTSKRAD(JL)*PTSKRAD5(JL)**2
    ZDLWDT5(JL)= -4._JPRB*PEMIS5(JL)*RSIGMA*PTSKRAD5(JL)**3
  ENDDO
ELSE
  DO JL=KIDIA,KFDIA
    ZDLWDT (JL) = 4._JPRB*(PTSKRAD5(JL)*PSLRFL(JL)-PSLRFL5(JL)*PTSKRAD(JL)) &
     &          / PTSKRAD5(JL)**2
    ZDLWDT5(JL) = 4._JPRB*(PSLRFL5(JL)/PTSKRAD5(JL))
  ENDDO
ENDIF

!      2.b Compute a stability factor for the explicit snow scheme and
!         forest-snow mix to prevent numerical instability for "thin-rough" snow 
!         when running with long time-step (e.g. 1-hour)
 
DO JL=KIDIA,KFDIA
  ZSTABEXSN(JL)=PTMST/(MAX(RQSNCR,(PSNS5(JL,1)/PRSN5(JL,1)))*RHOCI*PRSN5(JL,1)/RHOICE)
ENDDO 

!      3. Compute coefficients for dry static energy flux Js and 
!         moisture flux Jq, expressed in Sl,Ql and Ssk 

DO JT=1,KTILES
  DO JL=KIDIA,KFDIA
!or    ZCPTM15(JL,JT) = PSSKM1M5(JL,JT)/PTSKM1M5(JL,JT)
    ZICPTM1 (JL,JT) = (PSSKM1M5(JL,JT)*PTSKM1M(JL,JT) &
     &              - PTSKM1M5(JL,JT)*PSSKM1M(JL,JT))/PSSKM1M5(JL,JT)**2
    ZICPTM15(JL,JT) = PTSKM1M5(JL,JT)/PSSKM1M5(JL,JT)

    ZCJS1 (JL,JT) = PRHOCHU (JL,JT)
    ZCJS15(JL,JT) = PRHOCHU5(JL,JT)
    ZCJS3 (JL,JT) = -PRHOCHU(JL,JT)
    ZCJS35(JL,JT) = -PRHOCHU5(JL,JT)
    ZCJQ2 (JL,JT) = PRHOCQU5(JL,JT)*PALPHAL(JL,JT) &
     &            + PALPHAL5(JL,JT)*PRHOCQU(JL,JT)
    ZCJQ25(JL,JT) = PRHOCQU5(JL,JT)*PALPHAL5(JL,JT)
    ZCJQ3 (JL,JT) = (-PRHOCQU(JL,JT)*PALPHAS5(JL,JT)*PDQSDT5(JL,JT) &
     &            - PRHOCQU5(JL,JT)*PALPHAS(JL,JT)*PDQSDT5(JL,JT) &
     &            - PRHOCQU5(JL,JT)*PALPHAS5(JL,JT)*PDQSDT(JL,JT)) &
     &            * ZICPTM15(JL,JT) &
     &            - PRHOCQU5(JL,JT)*PALPHAS5(JL,JT)*PDQSDT5(JL,JT) &
     &            * ZICPTM1(JL,JT)
    ZCJQ35(JL,JT) = -PRHOCQU5(JL,JT)*PALPHAS5(JL,JT)*PDQSDT5(JL,JT) &
     &            * ZICPTM15(JL,JT)
    ZCJQ4 (JL,JT) = -(PQSKM1M5(JL,JT)-PDQSDT5(JL,JT)*PTSKM1M5(JL,JT)) &
     &            * (PRHOCQU5(JL,JT)*PALPHAS(JL,JT) &
     &            + PALPHAS5(JL,JT)*PRHOCQU(JL,JT)) &
     &            - PRHOCQU5(JL,JT)*PALPHAS5(JL,JT) &
     &            * (PQSKM1M(JL,JT)-PDQSDT5(JL,JT)*PTSKM1M(JL,JT) &
     &            - PTSKM1M5(JL,JT)*PDQSDT(JL,JT))
    ZCJQ45(JL,JT) = -PRHOCQU5(JL,JT)*PALPHAS5(JL,JT)*&
     &             (PQSKM1M5(JL,JT)-PDQSDT5(JL,JT)*PTSKM1M5(JL,JT))
  ENDDO
ENDDO


!      4. Compute coefficients for dry static energy flux Js and 
!         moisture flux Jq, expressed in Sl and Ql (Ssk has been 
!         eliminated using surface energy balance. 

DO JT=1,KTILES
!
  IF (JT == 2 .OR. JT == 5) THEN
    ZLAM=RLSTT
  ELSE
    ZLAM=RLVTT
  ENDIF
  ZFRSR=1._JPRB

  SELECT CASE(JT)
    CASE(1)
      DO JL=KIDIA,KFDIA
        ZLAMSK(JL,JT)=ZLARGE
      ENDDO
    CASE(2)
      DO JL=KIDIA,KFDIA
        IF (PTSKM1M5(JL,JT) > PTSRF5(JL,JT)) THEN
          ZLAMSK(JL,JT)=RVLAMSKS(12)
        ELSE
          ZLAMSK(JL,JT)=RVLAMSK(12)
        ENDIF
      ENDDO
    CASE(3)
      DO JL=KIDIA,KFDIA
        !IF (PTSKM1M5(JL,JT) > PTSRF5(JL,JT)) THEN
        !  ZLAMSK(JL,JT)=RVLAMSKS(KTVL(JL))
        !ELSE
        !  ZLAMSK(JL,JT)=RVLAMSK(KTVL(JL))
        !ENDIF
        ZLAMSK(JL,JT)=ZLARGEWT        
      ENDDO
      ZFRSR=1.0_JPRB-RVTRSR(1)
    CASE(4)
      DO JL=KIDIA,KFDIA
        IF (PTSKM1M5(JL,JT) > PTSRF5(JL,JT)) THEN
          ZLAMSK(JL,JT)=RVLAMSKS(KTVL(JL))
        ELSE
          ZLAMSK(JL,JT)=RVLAMSK(KTVL(JL))
        ENDIF
      ENDDO
      ZFRSR=1.0_JPRB-RVTRSR(1)
    CASE(5)
      DO JL=KIDIA,KFDIA
        IF (PTSKM1M5(JL,JT) >= PTSRF5(JL,JT)  &
         &  .AND. PTSKM1M5(JL,JT) > ZRTTMEPS) THEN
          ZLAMSK(JL,JT)=ZLARGESN
        ELSEIF (SUM(PSNS5(JL,:),DIM=1)>9000.0_JPRB) THEN
          ZLAMSK(JL,JT)=ZSNOW_GLACIER
        ELSE
          ZLAMSK(JL,JT)=ZSNOW
        ENDIF
      ENDDO
    CASE(6)
      DO JL=KIDIA,KFDIA
        IF (PTSKM1M5(JL,JT) > PTSRF5(JL,JT)) THEN
          ZLAMSK(JL,JT)=RVLAMSKS(KTVH(JL))
        ELSE
          ZLAMSK(JL,JT)=RVLAMSK(KTVH(JL))
        ENDIF
      ENDDO
      ZFRSR=1.0_JPRB-RVTRSR(3)
    CASE(7)
      DO JL=KIDIA,KFDIA
        IF (PTSKM1M5(JL,JT) > PTSRF5(JL,JT)) THEN
          ZLAMSK(JL,JT)=RVLAMSKS(KTVH(JL))/(1+ZSTABEXSN(JL)*RVLAMSKS(KTVH(JL)))
        ELSE
          ZLAMSK(JL,JT)=ZSNOWHVEG/(1+ZSTABEXSN(JL)*ZSNOWHVEG)
        ENDIF
      ENDDO
      ZFRSR=1.0_JPRB-RVTRSR(3)
    CASE(8)
      DO JL=KIDIA,KFDIA
        IF (PTSKM1M5(JL,JT) > PTSRF5(JL,JT)) THEN
          ZLAMSK(JL,JT)=RVLAMSKS(8)
        ELSE
          ZLAMSK(JL,JT)=RVLAMSK(8)
        ENDIF
      ENDDO
    CASE(9)
      DO JL=KIDIA,KFDIA
        ZLAMSK(JL,JT)=ZLARGE
      ENDDO
   CASE(10)
    DO JL=KIDIA,KFDIA
      ZLAMSK(JL,JT)=RURBTC
    ENDDO
  END SELECT


  DO JL=KIDIA,KFDIA
    ZCOEF1  = -RCPD*ZDELTA*PTSKM1M(JL,JT)
    ZCOEF15 = ZLAM-RCPD*PTSKM1M5(JL,JT)*ZDELTA
    ZZ  = (ZDLWDT5(JL)-ZLAMSK(JL,JT))*ZICPTM1(JL,JT) &
     &  + ZDLWDT(JL)*ZICPTM15(JL,JT)+ZCJS3(JL,JT) &
     &  + ZCOEF15*ZCJQ3(JL,JT)+ZCOEF1*ZCJQ35(JL,JT)
    ZZ5 = (ZDLWDT5(JL)-ZLAMSK(JL,JT))*ZICPTM15(JL,JT)+ZCJS35(JL,JT)&
     &  + ZCOEF15*ZCJQ35(JL,JT)

    ZIZZ5 = 1._JPRB/ZZ5
    ZIZZ  = -ZZ*ZIZZ5**2

    ZDSS1 (JL,JT) = -ZCJS15(JL,JT)*ZIZZ-ZCJS1(JL,JT)*ZIZZ5
    ZDSS15(JL,JT) = -ZCJS15(JL,JT)*ZIZZ5
    ZDSS2 (JL,JT) = (-ZCOEF1*ZCJQ25(JL,JT)-ZCOEF15*ZCJQ2(JL,JT))*ZIZZ5 &
     &            - ZCOEF15*ZCJQ25(JL,JT)*ZIZZ
    ZDSS25(JL,JT) = -ZCOEF15*ZCJQ25(JL,JT)*ZIZZ5
    ZDSS4 (JL,JT) = (-ZFRSR*PSSRFL(JL,JT)-PSLRFL(JL) &
     &            + ZDLWDT(JL)*PTSKRAD5(JL)+ZDLWDT5(JL)*PTSKRAD(JL) &
     &            - ZCOEF1*ZCJQ45(JL,JT)-ZCOEF15*ZCJQ4(JL,JT) &
     &            - ZLAMSK(JL,JT)*PTSRF(JL,JT))*ZIZZ5 &
     &            + (-PSSRFL5(JL,JT)*ZFRSR-PSLRFL5(JL) &
     &            + ZDLWDT5(JL)*PTSKRAD5(JL)&
     &            - ZCOEF15*ZCJQ45(JL,JT)-ZLAMSK(JL,JT)*PTSRF5(JL,JT))*ZIZZ
    ZDSS45(JL,JT) = (-PSSRFL5(JL,JT)*ZFRSR-PSLRFL5(JL) &
     &            + ZDLWDT5(JL)*PTSKRAD5(JL)&
     &            - ZCOEF15*ZCJQ45(JL,JT)-ZLAMSK(JL,JT)*PTSRF5(JL,JT))*ZIZZ5
    ZDJS1 (JL,JT) = ZCJS1(JL,JT)+ZCJS35(JL,JT)*ZDSS1(JL,JT) &
     &            + ZDSS15(JL,JT)*ZCJS3(JL,JT)
    ZDJS15(JL,JT) = ZCJS15(JL,JT)+ZCJS35(JL,JT)*ZDSS15(JL,JT)
    ZDJS2 (JL,JT) = ZCJS35(JL,JT)*ZDSS2(JL,JT)+ZDSS25(JL,JT)*ZCJS3(JL,JT)
    ZDJS25(JL,JT) = ZCJS35(JL,JT)*ZDSS25(JL,JT)
    ZDJS4 (JL,JT) = ZCJS35(JL,JT)*ZDSS4(JL,JT)+ZDSS45(JL,JT)*ZCJS3(JL,JT)
    ZDJS45(JL,JT) = ZCJS35(JL,JT)*ZDSS45(JL,JT)
    ZDJQ1 (JL,JT) = ZCJQ35(JL,JT)*ZDSS1(JL,JT)+ZDSS15(JL,JT)*ZCJQ3(JL,JT)
    ZDJQ15(JL,JT) = ZCJQ35(JL,JT)*ZDSS15(JL,JT)
    ZDJQ2 (JL,JT) = ZCJQ2(JL,JT)+ZCJQ35(JL,JT)*ZDSS2(JL,JT) &
     &            + ZDSS25(JL,JT)*ZCJQ3(JL,JT)
    ZDJQ25(JL,JT) = ZCJQ25(JL,JT)+ZCJQ35(JL,JT)*ZDSS25(JL,JT)
    ZDJQ4 (JL,JT) = ZCJQ35(JL,JT)*ZDSS4(JL,JT) &
     &            + ZDSS45(JL,JT)*ZCJQ3(JL,JT)+ZCJQ4(JL,JT)
    ZDJQ45(JL,JT) = ZCJQ35(JL,JT)*ZDSS45(JL,JT)+ZCJQ45(JL,JT)
  ENDDO
ENDDO


!      5.  Average coefficients over tiles

DO JL=KIDIA,KFDIA
  ZEJS1 (JL) = 0.0_JPRB
  ZEJS15(JL) = 0.0_JPRB
  ZEJS2 (JL) = 0.0_JPRB
  ZEJS25(JL) = 0.0_JPRB
  ZEJS4 (JL) = 0.0_JPRB
  ZEJS45(JL) = 0.0_JPRB
  ZEJQ1 (JL) = 0.0_JPRB
  ZEJQ15(JL) = 0.0_JPRB
  ZEJQ2 (JL) = 0.0_JPRB
  ZEJQ25(JL) = 0.0_JPRB
  ZEJQ4 (JL) = 0.0_JPRB
  ZEJQ45(JL) = 0.0_JPRB
ENDDO

DO JT=1,KTILES
  DO JL=KIDIA,KFDIA
    ZEJS1 (JL) = ZEJS1(JL)+PFRTI5(JL,JT)*ZDJS1(JL,JT)
    ZEJS15(JL) = ZEJS15(JL)+PFRTI5(JL,JT)*ZDJS15(JL,JT)
    ZEJS2 (JL) = ZEJS2(JL)+PFRTI5(JL,JT)*ZDJS2(JL,JT)
    ZEJS25(JL) = ZEJS25(JL)+PFRTI5(JL,JT)*ZDJS25(JL,JT)
    ZEJS4 (JL) = ZEJS4(JL)+PFRTI5(JL,JT)*ZDJS4(JL,JT)
    ZEJS45(JL) = ZEJS45(JL)+PFRTI5(JL,JT)*ZDJS45(JL,JT)
    ZEJQ1 (JL) = ZEJQ1(JL)+PFRTI5(JL,JT)*ZDJQ1(JL,JT)
    ZEJQ15(JL) = ZEJQ15(JL)+PFRTI5(JL,JT)*ZDJQ15(JL,JT)
    ZEJQ2 (JL) = ZEJQ2(JL)+PFRTI5(JL,JT)*ZDJQ2(JL,JT)
    ZEJQ25(JL) = ZEJQ25(JL)+PFRTI5(JL,JT)*ZDJQ25(JL,JT)
    ZEJQ4 (JL) = ZEJQ4(JL)+PFRTI5(JL,JT)*ZDJQ4(JL,JT)
    ZEJQ45(JL) = ZEJQ45(JL)+PFRTI5(JL,JT)*ZDJQ45(JL,JT)
  ENDDO
ENDDO


!      5.  Eliminate mean fluxes to find Sl and Ql

DO JL=KIDIA,KFDIA
  ZZ1  = -PASL5(JL)*ZEJS1(JL)-ZEJS15(JL)*PASL(JL)
  ZZ15 = 1._JPRB-PASL5(JL)*ZEJS15(JL)
  ZZ2  = PAQL5(JL)*ZEJS2(JL)+ZEJS25(JL)*PAQL(JL)
  ZZ25 = PAQL5(JL)*ZEJS25(JL)
  ZZ3  = PBSL5(JL)*ZEJS1(JL)+ZEJS15(JL)*PBSL(JL) &
   &   + PBQL5(JL)*ZEJS2(JL)+ZEJS25(JL)*PBQL(JL)+ZEJS4(JL)
  ZZ35 = PBSL5(JL)*ZEJS15(JL)+PBQL5(JL)*ZEJS25(JL)+ZEJS45(JL)
  ZY1  = -PAQL5(JL)*ZEJQ2(JL)-ZEJQ25(JL)*PAQL(JL)
  ZY15 = 1._JPRB-PAQL5(JL)*ZEJQ25(JL)
  ZY2  = PASL5(JL)*ZEJQ1(JL)+ZEJQ15(JL)*PASL(JL)
  ZY25 = PASL5(JL)*ZEJQ15(JL)
  ZY3  = PBSL5(JL)*ZEJQ1(JL)+ZEJQ15(JL)*PBSL(JL) &
   &   + PBQL5(JL)*ZEJQ2(JL)+ZEJQ25(JL)*PBQL(JL)+ZEJQ4(JL)
  ZY35 = PBSL5(JL)*ZEJQ15(JL)+PBQL5(JL)*ZEJQ25(JL)+ZEJQ45(JL)

  ZZ  = ZZ15*ZY1+ZY15*ZZ1-ZZ25*ZY2-ZY25*ZZ2
  ZZ5 = ZZ15*ZY15-ZZ25*ZY25

  ZIZZ5 = 1._JPRB/ZZ5
  ZIZZ  = -ZZ*ZIZZ5**2

  ZJQ  = (ZZ3*ZY25+ZZ35*ZY2+ZZ1*ZY35+ZZ15*ZY3)*ZIZZ5 &
   &   + (ZZ35*ZY25+ZZ15*ZY35)*ZIZZ
  ZJQ5 = (ZZ35*ZY25+ZZ15*ZY35)*ZIZZ5
  ZJS  = (ZZ2*ZY35+ZZ25*ZY3+ZZ3*ZY15+ZZ35*ZY1)*ZIZZ5 &
   &   + (ZZ25*ZY35+ZZ35*ZY15)*ZIZZ
  ZJS5 = (ZZ25*ZY35+ZZ35*ZY15)*ZIZZ5

  PSL (JL) = PASL5(JL)*ZJS+ZJS5*PASL(JL)+PBSL(JL)
  PSL5(JL) = PASL5(JL)*ZJS5+PBSL5(JL)
  PQL (JL) = PAQL5(JL)*ZJQ+ZJQ5*PAQL(JL)+PBQL(JL)
  PQL5(JL) = PAQL5(JL)*ZJQ5+PBQL5(JL)

ENDDO


!      6.  Compute tile dependent fluxes and skin values

DO JT=1,KTILES
  IF (JT == 2 .OR. JT == 5) THEN
    ZLAM=RLSTT
  ELSE
    ZLAM=RLVTT
  ENDIF
  DO JL=KIDIA,KFDIA
    PSSK (JL,JT) = ZDSS15(JL,JT)*PSL(JL)+PSL5(JL)*ZDSS1(JL,JT) &
     &           + ZDSS25(JL,JT)*PQL(JL)+PQL5(JL)*ZDSS2(JL,JT) &
     &           + ZDSS4(JL,JT)
    PSSK5(JL,JT) = ZDSS15(JL,JT)*PSL5(JL)+ZDSS25(JL,JT)*PQL5(JL) &
     &           + ZDSS45(JL,JT)
    PJS (JL,JT) = ZDJS15(JL,JT)*PSL(JL)+PSL5(JL)*ZDJS1(JL,JT) &
     &          + ZDJS25(JL,JT)*PQL(JL)+PQL5(JL)*ZDJS2(JL,JT) &
     &          + ZDJS4(JL,JT)
    PJS5(JL,JT) = ZDJS15(JL,JT)*PSL5(JL)+ZDJS25(JL,JT)*PQL5(JL) &
     &          + ZDJS45(JL,JT)
    PJQ (JL,JT) = ZDJQ15(JL,JT)*PSL(JL)+PSL5(JL)*ZDJQ1(JL,JT) &
     &          + ZDJQ25(JL,JT)*PQL(JL)+PQL5(JL)*ZDJQ2(JL,JT) &
     &          + ZDJQ4(JL,JT)
    PJQ5(JL,JT) = ZDJQ15(JL,JT)*PSL5(JL)+ZDJQ25(JL,JT)*PQL5(JL) &
     &          + ZDJQ45(JL,JT)
    PTSK(JL,JT)  = PSSK(JL,JT)*ZICPTM15(JL,JT) &
     &           + PSSK5(JL,JT)*ZICPTM1(JL,JT) 
    PTSK5(JL,JT) = PSSK5(JL,JT)*ZICPTM15(JL,JT)

!         Surface heat fluxes

    PSSH (JL,JT) = PJS(JL,JT)-RCPD*ZDELTA*(PTSKM1M5(JL,JT)*PJQ(JL,JT) &
     &           + PJQ5(JL,JT)*PTSKM1M(JL,JT))
    PSSH5(JL,JT) = PJS5(JL,JT)-RCPD*PTSKM1M5(JL,JT)*ZDELTA*PJQ5(JL,JT)
    PSLH (JL,JT) = ZLAM*PJQ(JL,JT)
    PSLH5(JL,JT) = ZLAM*PJQ5(JL,JT)
    PSTR (JL,JT) = PSLRFL(JL)+ZDLWDT5(JL)*(PTSK(JL,JT)-PTSKRAD(JL)) &
     &           + (PTSK5(JL,JT)-PTSKRAD5(JL))*ZDLWDT(JL)
    PSTR5(JL,JT) = PSLRFL5(JL)+ZDLWDT5(JL)*(PTSK5(JL,JT)-PTSKRAD5(JL))
    PG0 (JL,JT) = ZLAMSK(JL,JT)*(PTSK(JL,JT)-PTSRF(JL,JT))  
    PG05(JL,JT) = ZLAMSK(JL,JT)*(PTSK5(JL,JT)-PTSRF5(JL,JT))
  ENDDO
ENDDO
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SURFSEBSTL_CTL_MOD:SURFSEBSTL_CTL',1,ZHOOK_HANDLE)


!      7.  Wrap up

END SUBROUTINE SURFSEBSTL_CTL
END MODULE SURFSEBSTL_CTL_MOD
