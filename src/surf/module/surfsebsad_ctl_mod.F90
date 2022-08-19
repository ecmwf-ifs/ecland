MODULE SURFSEBSAD_CTL_MOD
CONTAINS
SUBROUTINE SURFSEBSAD_CTL(KIDIA,KFDIA,KLON,KTILES,KTVL,KTVH,&
           &PTMST,PSSKM1M5,PTSKM1M5,PQSKM1M5,PDQSDT5,PRHOCHU5,PRHOCQU5,&
           &PALPHAL5,PALPHAS5,PSSRFL5,PFRTI5,PTSRF5,PSNS5,PRSN5,&
           &PSLRFL5,PTSKRAD5,PEMIS5,PASL5,PBSL5,PAQL5,PBQL5,&
           &PJS5,PJQ5,PSSK5,PTSK5,PSSH5,PSLH5,PSTR5,PG05,&
           &PSL5,PQL5, &
           &PSSKM1M,PTSKM1M,PQSKM1M,PDQSDT,PRHOCHU,PRHOCQU,&
           &PALPHAL,PALPHAS,PSSRFL,PTSRF,&
           &PSLRFL,PTSKRAD,PASL,PBSL,PAQL,PBQL,&
           &YDCST,YDEXC,YDVEG,YDSOIL,&
!out
           &PJS,PJQ,PSSK,PTSK,PSSH,PSLH,PSTR,PG0,&
           &PSL,PQL)

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_THF  , ONLY : RVTMP2
USE YOS_CST  , ONLY : TCST
USE YOS_EXC  , ONLY : TEXC
USE YOS_VEG  , ONLY : TVEG
USE YOS_SOIL , ONLY : TSOIL

! (C) Copyright 2003- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!------------------------------------------------------------------------

!  PURPOSE:
!    VDFSEBAD computes surface energy balance and skin temperature 
!             for each tile. 
!             (Adjoint)

!  VDFSEBAD is called by VDFDIFHAD

!  METHOD:
!    A linear relation between lowest model level dry static 
!    energy and moisture and their fluxes is specified as input. 
!    The surface energy balance equation is used to eliminate 
!    the skin temperature as in the derivation of the 
!    Penmann-Monteith equation. 

!    The routine can also be used in stand alone simulations by
!    putting PASL and PAQL to zero and by specifying for PBSL and PBQL 
!    the forcing with dry static energy and specific humidity. 

!  AUTHOR of AD routine:
!    M. Janiskova       ECMWF October 2003   

!  REVISION HISTORY:
!    P. Viterbo         ECMWF March 2004      move to surf vob


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



!
IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM), INTENT(IN)  :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)  :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)  :: KLON
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

REAL(KIND=JPRB),    INTENT(INOUT) :: PSSKM1M(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PTSKM1M(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PQSKM1M(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PDQSDT(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PRHOCHU(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PRHOCQU(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PALPHAL(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PALPHAS(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PSSRFL(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PTSRF(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PSLRFL(:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PTSKRAD(:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PASL(:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PBSL(:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PAQL(:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PBQL(:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PJS(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PJQ(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PSSK(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PTSK(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PSSH(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PSLH(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PSTR(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PG0(:,:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PSL(:)
REAL(KIND=JPRB),    INTENT(INOUT) :: PQL(:)


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
          & ZDJQ45(KLON,KTILES),&
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
          & ZDJQ4(KLON,KTILES),&
          & ZICPTM1(KLON,KTILES),ZLAMSK(KLON,KTILES)

REAL(KIND=JPRB) :: ZCOEF15(KLON,KTILES),ZZ5(KLON,KTILES),ZIZZ5(KLON,KTILES)
REAL(KIND=JPRB) :: ZZ15(KLON),ZZ25(KLON),ZZ35(KLON),ZY15(KLON),ZY25(KLON)
REAL(KIND=JPRB) :: ZY35(KLON),ZZB5(KLON),ZIZZB5(KLON),ZJQ5(KLON),ZJS5(KLON)

INTEGER(KIND=JPIM) :: JL,JT
REAL(KIND=JPRB) :: ZDELTA,ZLARGE,ZLARGESN,ZRTTMEPS,ZLAM,ZFRSR,ZLARGEWT
REAL(KIND=JPRB) :: ZCOEF1,ZIZZ 
REAL(KIND=JPRB) :: ZZ,ZZ1,ZZ2,ZZ3,ZY1,ZY2,ZY3,ZJS,ZJQ
REAL(KIND=JPRB) :: ZSNOW,ZSNOWHVEG,ZSNOW_GLACIER
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


! ------------------------------------------------------------------------



!      1. Initialize constants

IF (LHOOK) CALL DR_HOOK('SURFSEBSAD_CTL_MOD:SURFSEBSAD_CTL',0,ZHOOK_HANDLE)
ASSOCIATE(RCPD=>YDCST%RCPD, RLSTT=>YDCST%RLSTT, RLVTT=>YDCST%RLVTT, &
 & RSIGMA=>YDCST%RSIGMA, RTT=>YDCST%RTT, &
 & LELWDD=>YDEXC%LELWDD, &
 & RHOCI=>YDSOIL%RHOCI, RHOICE=>YDSOIL%RHOICE, RQSNCR=>YDSOIL%RQSNCR, &
 & RVLAMSK=>YDVEG%RVLAMSK, RVLAMSKS=>YDVEG%RVLAMSKS, RVTRSR=>YDVEG%RVTRSR)
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
    ZDLWDT5(JL)= -4._JPRB*PEMIS5(JL)*RSIGMA*PTSKRAD5(JL)**3
  ENDDO
ELSE
  DO JL=KIDIA,KFDIA
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
    ZICPTM15(JL,JT) = PTSKM1M5(JL,JT)/PSSKM1M5(JL,JT)
    ZCJS15(JL,JT) = PRHOCHU5(JL,JT)
    ZCJS35(JL,JT) = -PRHOCHU5(JL,JT)
    ZCJQ25(JL,JT) = PRHOCQU5(JL,JT)*PALPHAL5(JL,JT)
    ZCJQ35(JL,JT) = -PRHOCQU5(JL,JT)*PALPHAS5(JL,JT)*PDQSDT5(JL,JT) &
     &            * ZICPTM15(JL,JT)
    ZCJQ45(JL,JT) = -PRHOCQU5(JL,JT)*PALPHAS5(JL,JT)*&
     &             (PQSKM1M5(JL,JT)-PDQSDT5(JL,JT)*PTSKM1M5(JL,JT))
  ENDDO
ENDDO


!      4. Compute coefficients for dry static energy flux Js and 
!         moisture flux Jq, expressed in Sl and Ql (Ssk has been 
!         eliminated using surface energy balance. 

DO JT=1,KTILES

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
     CASE(8,10)
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
  END SELECT


  DO JL=KIDIA,KFDIA
    ZCOEF15(JL,JT) = ZLAM-RCPD*PTSKM1M5(JL,JT)*ZDELTA
    ZZ5(JL,JT) = (ZDLWDT5(JL)-ZLAMSK(JL,JT))*ZICPTM15(JL,JT) &
     &         + ZCJS35(JL,JT)+ ZCOEF15(JL,JT)*ZCJQ35(JL,JT)
    ZIZZ5(JL,JT) = 1._JPRB/ZZ5(JL,JT)
    ZDSS15(JL,JT) = -ZCJS15(JL,JT)*ZIZZ5(JL,JT)
    ZDSS25(JL,JT) = -ZCOEF15(JL,JT)*ZCJQ25(JL,JT)*ZIZZ5(JL,JT)
    ZDSS45(JL,JT) = (-PSSRFL5(JL,JT)*ZFRSR-PSLRFL5(JL) &
     &            + ZDLWDT5(JL)*PTSKRAD5(JL) &
     &            - ZCOEF15(JL,JT)*ZCJQ45(JL,JT) &
     &            - ZLAMSK(JL,JT)*PTSRF5(JL,JT))*ZIZZ5(JL,JT)
    ZDJS15(JL,JT) = ZCJS15(JL,JT)+ZCJS35(JL,JT)*ZDSS15(JL,JT)
    ZDJS25(JL,JT) = ZCJS35(JL,JT)*ZDSS25(JL,JT)
    ZDJS45(JL,JT) = ZCJS35(JL,JT)*ZDSS45(JL,JT)
    ZDJQ15(JL,JT) = ZCJQ35(JL,JT)*ZDSS15(JL,JT)
    ZDJQ25(JL,JT) = ZCJQ25(JL,JT)+ZCJQ35(JL,JT)*ZDSS25(JL,JT)
    ZDJQ45(JL,JT) = ZCJQ35(JL,JT)*ZDSS45(JL,JT)+ZCJQ45(JL,JT)
  ENDDO
ENDDO


!      5.  Average coefficients over tiles

DO JL=KIDIA,KFDIA
  ZEJS15(JL) = 0.0_JPRB
  ZEJS25(JL) = 0.0_JPRB
  ZEJS45(JL) = 0.0_JPRB
  ZEJQ15(JL) = 0.0_JPRB
  ZEJQ25(JL) = 0.0_JPRB
  ZEJQ45(JL) = 0.0_JPRB
ENDDO

DO JT=1,KTILES
  DO JL=KIDIA,KFDIA
    ZEJS15(JL) = ZEJS15(JL)+PFRTI5(JL,JT)*ZDJS15(JL,JT)
    ZEJS25(JL) = ZEJS25(JL)+PFRTI5(JL,JT)*ZDJS25(JL,JT)
    ZEJS45(JL) = ZEJS45(JL)+PFRTI5(JL,JT)*ZDJS45(JL,JT)
    ZEJQ15(JL) = ZEJQ15(JL)+PFRTI5(JL,JT)*ZDJQ15(JL,JT)
    ZEJQ25(JL) = ZEJQ25(JL)+PFRTI5(JL,JT)*ZDJQ25(JL,JT)
    ZEJQ45(JL) = ZEJQ45(JL)+PFRTI5(JL,JT)*ZDJQ45(JL,JT)
  ENDDO
ENDDO


!      5.  Eliminate mean fluxes to find Sl and Ql

DO JL=KIDIA,KFDIA
  ZZ15(JL) = 1._JPRB-PASL5(JL)*ZEJS15(JL)
  ZZ25(JL) = PAQL5(JL)*ZEJS25(JL)
  ZZ35(JL) = PBSL5(JL)*ZEJS15(JL)+PBQL5(JL)*ZEJS25(JL)+ZEJS45(JL)
  ZY15(JL) = 1._JPRB-PAQL5(JL)*ZEJQ25(JL)
  ZY25(JL) = PASL5(JL)*ZEJQ15(JL)
  ZY35(JL) = PBSL5(JL)*ZEJQ15(JL)+PBQL5(JL)*ZEJQ25(JL)+ZEJQ45(JL)

  ZZB5(JL) = ZZ15(JL)*ZY15(JL)-ZZ25(JL)*ZY25(JL)
  ZIZZB5(JL) = 1._JPRB/ZZB5(JL)

  ZJQ5(JL) = (ZZ35(JL)*ZY25(JL)+ZZ15(JL)*ZY35(JL))*ZIZZB5(JL)
  ZJS5(JL) = (ZZ25(JL)*ZY35(JL)+ZZ35(JL)*ZY15(JL))*ZIZZB5(JL)

  PSL5(JL) = PASL5(JL)*ZJS5(JL)+PBSL5(JL)
  PQL5(JL) = PAQL5(JL)*ZJQ5(JL)+PBQL5(JL)
!
ENDDO


!      6.  Compute tile dependent fluxes and skin values

DO JT=1,KTILES
  IF (JT == 2 .OR. JT == 5) THEN
    ZLAM=RLSTT
  ELSE
    ZLAM=RLVTT
  ENDIF
  DO JL=KIDIA,KFDIA
    PSSK5(JL,JT) = ZDSS15(JL,JT)*PSL5(JL)+ZDSS25(JL,JT)*PQL5(JL) &
     &           + ZDSS45(JL,JT)
    PJS5(JL,JT) = ZDJS15(JL,JT)*PSL5(JL)+ZDJS25(JL,JT)*PQL5(JL) &
     &          + ZDJS45(JL,JT)
    PJQ5(JL,JT) = ZDJQ15(JL,JT)*PSL5(JL)+ZDJQ25(JL,JT)*PQL5(JL) &
     &          + ZDJQ45(JL,JT)
    PTSK5(JL,JT) = PSSK5(JL,JT)*ZICPTM15(JL,JT)

!         Surface heat fluxes

    PSSH5(JL,JT) = PJS5(JL,JT)-RCPD*PTSKM1M5(JL,JT)*ZDELTA*PJQ5(JL,JT)
    PSLH5(JL,JT) = ZLAM*PJQ5(JL,JT)
    PSTR5(JL,JT) = PSLRFL5(JL)+ZDLWDT5(JL)*(PTSK5(JL,JT)-PTSKRAD5(JL))
    PG05(JL,JT) = ZLAMSK(JL,JT)*(PTSK5(JL,JT)-PTSRF5(JL,JT))
  ENDDO
ENDDO

!     ------------------------------------------------------------------

!                 ----- ADJOINT COMPUTATION ------

!     ------------------------------------------------------------------

!*                        INITIALIZATIONS
!                         ---------------

!     arrays with the dimensions (KLON,KTILES)

ZICPTM1(:,:) = 0.0_JPRB
ZCJS1 (:,:) = 0.0_JPRB
ZCJS3 (:,:) = 0.0_JPRB
ZCJQ2 (:,:) = 0.0_JPRB
ZCJQ3 (:,:) = 0.0_JPRB
ZCJQ4 (:,:) = 0.0_JPRB
ZDSS1 (:,:) = 0.0_JPRB
ZDSS2 (:,:) = 0.0_JPRB
ZDSS4 (:,:) = 0.0_JPRB
ZDJS1 (:,:) = 0.0_JPRB
ZDJS2 (:,:) = 0.0_JPRB
ZDJS4 (:,:) = 0.0_JPRB
ZDJQ1 (:,:) = 0.0_JPRB
ZDJQ2 (:,:) = 0.0_JPRB
ZDJQ4 (:,:) = 0.0_JPRB

!     arrays with the dimensions (KLON)

ZDLWDT(:) = 0.0_JPRB
ZEJS1 (:) = 0.0_JPRB
ZEJS2 (:) = 0.0_JPRB
ZEJS4 (:) = 0.0_JPRB
ZEJQ1 (:) = 0.0_JPRB
ZEJQ2 (:) = 0.0_JPRB
ZEJQ4 (:) = 0.0_JPRB

!     ------------------------------------------------------------

!     ------------------------------------------------------------------



!      6.  Compute tile dependent fluxes and skin values

DO JT=KTILES,1,-1
  IF (JT == 2 .OR. JT == 5) THEN
    ZLAM=RLSTT
  ELSE
    ZLAM=RLVTT
  ENDIF
  DO JL=KIDIA,KFDIA

!         Surface heat fluxes

    PTSK(JL,JT) = PTSK(JL,JT)+ZLAMSK(JL,JT)*PG0 (JL,JT)
    PTSRF(JL,JT) = PTSRF(JL,JT)-ZLAMSK(JL,JT)*PG0 (JL,JT)
    PG0 (JL,JT) = 0.0_JPRB

    PSLRFL(JL) = PSLRFL(JL)+PSTR (JL,JT)
    PTSK(JL,JT) = PTSK(JL,JT)+ZDLWDT5(JL)*PSTR (JL,JT)
    PTSKRAD(JL) = PTSKRAD(JL)-ZDLWDT5(JL)*PSTR (JL,JT)
    ZDLWDT(JL) = ZDLWDT(JL)+(PTSK5(JL,JT)-PTSKRAD5(JL))*PSTR (JL,JT)
    PSTR (JL,JT) = 0.0_JPRB

    PJQ(JL,JT) = PJQ(JL,JT)+ZLAM*PSLH (JL,JT)
    PSLH (JL,JT) = 0.0_JPRB

    PJS(JL,JT) = PJS(JL,JT)+PSSH (JL,JT)
    PJQ(JL,JT) = PJQ(JL,JT)-RCPD*ZDELTA*PTSKM1M5(JL,JT)*PSSH (JL,JT)
    PTSKM1M(JL,JT) = PTSKM1M(JL,JT)-RCPD*ZDELTA*PJQ5(JL,JT)*PSSH (JL,JT)
    PSSH (JL,JT) = 0.0_JPRB

    PSSK(JL,JT) = PSSK(JL,JT)+ZICPTM15(JL,JT)*PTSK(JL,JT)
    ZICPTM1(JL,JT) = ZICPTM1(JL,JT)+PSSK5(JL,JT)*PTSK(JL,JT)
    PTSK(JL,JT) = 0.0_JPRB

    PSL(JL) = PSL(JL)+ZDJQ15(JL,JT)*PJQ(JL,JT)
    ZDJQ1(JL,JT) = ZDJQ1(JL,JT)+PSL5(JL)*PJQ(JL,JT)
    PQL(JL) = PQL(JL)+ZDJQ25(JL,JT)*PJQ(JL,JT)
    ZDJQ2(JL,JT) = ZDJQ2(JL,JT)+PQL5(JL)*PJQ(JL,JT)
    ZDJQ4(JL,JT) = ZDJQ4(JL,JT)+PJQ(JL,JT)
    PJQ(JL,JT) = 0.0_JPRB

    PSL(JL) = PSL(JL)+ZDJS15(JL,JT)*PJS(JL,JT)
    ZDJS1(JL,JT) = ZDJS1(JL,JT)+PSL5(JL)*PJS(JL,JT)
    PQL(JL) = PQL(JL)+ZDJS25(JL,JT)*PJS(JL,JT)
    ZDJS2(JL,JT) = ZDJS2(JL,JT)+PQL5(JL)*PJS(JL,JT)
    ZDJS4(JL,JT) = ZDJS4(JL,JT)+PJS(JL,JT)
    PJS(JL,JT) = 0.0_JPRB

    PSL(JL) = PSL(JL)+ZDSS15(JL,JT)*PSSK(JL,JT)
    ZDSS1(JL,JT) = ZDSS1(JL,JT)+PSL5(JL)*PSSK(JL,JT)
    PQL(JL) = PQL(JL)+ZDSS25(JL,JT)*PSSK(JL,JT)
    ZDSS2(JL,JT) = ZDSS2(JL,JT)+PQL5(JL)*PSSK(JL,JT)
    ZDSS4(JL,JT) = ZDSS4(JL,JT)+PSSK(JL,JT)
    PSSK(JL,JT) = 0.0_JPRB
  ENDDO
ENDDO



!      5.  Eliminate mean fluxes to find Sl and Ql

DO JL=KIDIA,KFDIA
  ZZ1 = 0.0_JPRB
  ZZ2 = 0.0_JPRB
  ZZ3 = 0.0_JPRB
  ZY1 = 0.0_JPRB
  ZY2 = 0.0_JPRB
  ZY3 = 0.0_JPRB
  ZZ  = 0.0_JPRB
  ZIZZ= 0.0_JPRB
  ZJQ = 0.0_JPRB
  ZJS = 0.0_JPRB

  ZJQ = ZJQ+PAQL5(JL)*PQL(JL)
  PAQL(JL) = PAQL(JL)+ZJQ5(JL)*PQL(JL)
  PBQL(JL) = PBQL(JL)+PQL(JL)
  PQL(JL) = 0.0_JPRB
  ZJS = ZJS+PASL5(JL)*PSL(JL)
  PASL(JL) = PASL(JL)+ZJS5(JL)*PSL(JL)
  PBSL(JL) = PBSL(JL)+PSL(JL)
  PSL(JL) = 0.0_JPRB

  ZZ2 = ZZ2+ZIZZB5(JL)*ZY35(JL)*ZJS
  ZY3 = ZY3+ZIZZB5(JL)*ZZ25(JL)*ZJS
  ZZ3 = ZZ3+ZIZZB5(JL)*ZY15(JL)*ZJS
  ZY1 = ZY1+ZIZZB5(JL)*ZZ35(JL)*ZJS
  ZIZZ= ZIZZ+(ZZ25(JL)*ZY35(JL)+ZZ35(JL)*ZY15(JL))*ZJS

  ZZ3 = ZZ3+ZIZZB5(JL)*ZY25(JL)*ZJQ
  ZY2 = ZY2+ZIZZB5(JL)*ZZ35(JL)*ZJQ
  ZZ1 = ZZ1+ZIZZB5(JL)*ZY35(JL)*ZJQ
  ZY3 = ZY3+ZIZZB5(JL)*ZZ15(JL)*ZJQ
  ZIZZ= ZIZZ+(ZZ35(JL)*ZY25(JL)+ZZ15(JL)*ZY35(JL))*ZJQ

  ZZ = ZZ-ZIZZ*ZIZZB5(JL)**2
  ZY1 = ZY1+ZZ15(JL)*ZZ
  ZZ1 = ZZ1+ZY15(JL)*ZZ
  ZY2 = ZY2-ZZ25(JL)*ZZ
  ZZ2 = ZZ2-ZY25(JL)*ZZ

  ZEJQ1(JL) = ZEJQ1(JL)+PBSL5(JL)*ZY3
  PBSL(JL) = PBSL(JL)+ZEJQ15(JL)*ZY3
  ZEJQ2(JL) = ZEJQ2(JL)+PBQL5(JL)*ZY3
  PBQL(JL) = PBQL(JL)+ZEJQ25(JL)*ZY3
  ZEJQ4(JL) = ZEJQ4(JL)+ZY3
  ZEJQ1(JL) = ZEJQ1(JL)+PASL5(JL)*ZY2
  PASL(JL) = PASL(JL)+ZEJQ15(JL)*ZY2
  ZEJQ2(JL) = ZEJQ2(JL)-PAQL5(JL)*ZY1
  PAQL(JL) = PAQL(JL)-ZEJQ25(JL)*ZY1

  ZEJS1(JL) = ZEJS1(JL)+PBSL5(JL)*ZZ3
  PBSL(JL) = PBSL(JL)+ZEJS15(JL)*ZZ3
  ZEJS2(JL) = ZEJS2(JL)+PBQL5(JL)*ZZ3
  PBQL(JL) = PBQL(JL)+ZEJS25(JL)*ZZ3
  ZEJS4(JL) = ZEJS4(JL)+ZZ3
  ZEJS2(JL) = ZEJS2(JL)+PAQL5(JL)*ZZ2
  PAQL(JL) = PAQL(JL)+ZEJS25(JL)*ZZ2
  ZEJS1(JL) = ZEJS1(JL)-PASL5(JL)*ZZ1
  PASL(JL) = PASL(JL)-ZEJS15(JL)*ZZ1
ENDDO


!      5.  Average coefficients over tiles

DO JT=KTILES,1,-1
  DO JL=KIDIA,KFDIA
    ZDJQ4(JL,JT) = ZDJQ4(JL,JT)+PFRTI5(JL,JT)*ZEJQ4(JL)
    ZDJQ2(JL,JT) = ZDJQ2(JL,JT)+PFRTI5(JL,JT)*ZEJQ2 (JL)
    ZDJQ1(JL,JT) = ZDJQ1(JL,JT)+PFRTI5(JL,JT)*ZEJQ1 (JL)
    ZDJS4(JL,JT) = ZDJS4(JL,JT)+PFRTI5(JL,JT)*ZEJS4 (JL)
    ZDJS2(JL,JT) = ZDJS2(JL,JT)+PFRTI5(JL,JT)*ZEJS2 (JL)
    ZDJS1(JL,JT) = ZDJS1(JL,JT)+PFRTI5(JL,JT)*ZEJS1 (JL)
  ENDDO
ENDDO

DO JL=KIDIA,KFDIA
  ZEJQ4 (JL) = 0.0_JPRB
  ZEJQ2 (JL) = 0.0_JPRB
  ZEJQ1 (JL) = 0.0_JPRB
  ZEJS4 (JL) = 0.0_JPRB
  ZEJS2 (JL) = 0.0_JPRB
  ZEJS1 (JL) = 0.0_JPRB
ENDDO


!      4. Compute coefficients for dry static energy flux Js and 
!         moisture flux Jq, expressed in Sl and Ql (Ssk has been 
!         eliminated using surface energy balance. 

DO JT=KTILES,1,-1

  IF (JT == 2 .OR. JT == 5) THEN
    ZLAM=RLSTT
  ELSE
    ZLAM=RLVTT
  ENDIF
  ZFRSR=1._JPRB

  SELECT CASE(JT)
    CASE(3)
      ZFRSR=1.0_JPRB-RVTRSR(1)
    CASE(4)
      ZFRSR=1.0_JPRB-RVTRSR(1)
    CASE(6)
      ZFRSR=1.0_JPRB-RVTRSR(3)
    CASE(7)
      ZFRSR=1.0_JPRB-RVTRSR(3)
  END SELECT

  DO JL=KIDIA,KFDIA
    ZCOEF1 = 0.0_JPRB
    ZZ     = 0.0_JPRB
    ZIZZ   = 0.0_JPRB

    ZDSS4(JL,JT) = ZDSS4(JL,JT)+ZCJQ35(JL,JT)*ZDJQ4(JL,JT)
    ZCJQ3(JL,JT) = ZCJQ3(JL,JT)+ZDSS45(JL,JT)*ZDJQ4(JL,JT)
    ZCJQ4(JL,JT) = ZCJQ4(JL,JT)+ZDJQ4(JL,JT)
    ZDJQ4(JL,JT) = 0.0_JPRB

    ZCJQ2(JL,JT) = ZCJQ2(JL,JT)+ZDJQ2(JL,JT)
    ZDSS2(JL,JT) = ZDSS2(JL,JT)+ZCJQ35(JL,JT)*ZDJQ2(JL,JT)
    ZCJQ3(JL,JT) = ZCJQ3(JL,JT)+ZDSS25(JL,JT)*ZDJQ2(JL,JT)
    ZDJQ2(JL,JT) = 0.0_JPRB

    ZDSS1(JL,JT) = ZDSS1(JL,JT)+ZCJQ35(JL,JT)*ZDJQ1(JL,JT)
    ZCJQ3(JL,JT) = ZCJQ3(JL,JT)+ZDSS15(JL,JT)*ZDJQ1(JL,JT)
    ZDJQ1(JL,JT) = 0.0_JPRB
    ZDSS4(JL,JT) = ZDSS4(JL,JT)+ZCJS35(JL,JT)*ZDJS4(JL,JT)
    ZCJS3(JL,JT) = ZCJS3(JL,JT)+ZDSS45(JL,JT)*ZDJS4(JL,JT)
    ZDJS4(JL,JT) = 0.0_JPRB
    ZDSS2(JL,JT) = ZDSS2(JL,JT)+ZCJS35(JL,JT)*ZDJS2(JL,JT)
    ZCJS3(JL,JT) = ZCJS3(JL,JT)+ZDSS25(JL,JT)*ZDJS2(JL,JT)
    ZDJS2(JL,JT) = 0.0_JPRB
    ZCJS1(JL,JT) = ZCJS1(JL,JT)+ZDJS1(JL,JT)
    ZDSS1(JL,JT) = ZDSS1(JL,JT)+ZCJS35(JL,JT)*ZDJS1(JL,JT)
    ZCJS3(JL,JT) = ZCJS3(JL,JT)+ZDSS15(JL,JT)*ZDJS1(JL,JT)
    ZDJS1(JL,JT) = 0.0_JPRB

    PSSRFL(JL,JT) = PSSRFL(JL,JT)-ZFRSR*ZIZZ5(JL,JT)*ZDSS4(JL,JT)
    PSLRFL(JL) = PSLRFL(JL)-ZIZZ5(JL,JT)*ZDSS4(JL,JT)
    ZDLWDT(JL) = ZDLWDT(JL)+PTSKRAD5(JL)*ZIZZ5(JL,JT)*ZDSS4(JL,JT)
    PTSKRAD(JL) = PTSKRAD(JL)+ZDLWDT5(JL)*ZIZZ5(JL,JT)*ZDSS4(JL,JT)
    ZCOEF1 = ZCOEF1-ZCJQ45(JL,JT)*ZIZZ5(JL,JT)*ZDSS4(JL,JT)
    ZCJQ4(JL,JT) = ZCJQ4(JL,JT)-ZCOEF15(JL,JT)*ZIZZ5(JL,JT)*ZDSS4(JL,JT)
    PTSRF(JL,JT) = PTSRF(JL,JT)-ZLAMSK(JL,JT)*ZIZZ5(JL,JT)*ZDSS4(JL,JT)
    ZIZZ = ZIZZ+(-PSSRFL5(JL,JT)*ZFRSR-PSLRFL5(JL) &
     &   + ZDLWDT5(JL)*PTSKRAD5(JL)&
     &   - ZCOEF15(JL,JT)*ZCJQ45(JL,JT)-ZLAMSK(JL,JT)*PTSRF5(JL,JT)) &
     &   * ZDSS4(JL,JT)
    ZDSS4(JL,JT) = 0.0_JPRB
  
    ZCOEF1 = ZCOEF1-ZCJQ25(JL,JT)*ZIZZ5(JL,JT)*ZDSS2(JL,JT)
    ZCJQ2(JL,JT) = ZCJQ2(JL,JT)-ZCOEF15(JL,JT)*ZIZZ5(JL,JT)*ZDSS2(JL,JT)
    ZIZZ = ZIZZ-ZCOEF15(JL,JT)*ZCJQ25(JL,JT)*ZDSS2(JL,JT)
    ZDSS2(JL,JT) = 0.0_JPRB
    ZIZZ = ZIZZ-ZCJS15(JL,JT)*ZDSS1(JL,JT)
    ZCJS1(JL,JT) = ZCJS1(JL,JT)-ZIZZ5(JL,JT)*ZDSS1(JL,JT)
    ZDSS1(JL,JT) = 0.0_JPRB

    ZZ = ZZ-ZIZZ*ZIZZ5(JL,JT)**2
    ZICPTM1(JL,JT) = ZICPTM1(JL,JT)+(ZDLWDT5(JL)-ZLAMSK(JL,JT))*ZZ
    ZDLWDT(JL) = ZDLWDT(JL)+ZICPTM15(JL,JT)*ZZ
    ZCJS3(JL,JT) = ZCJS3(JL,JT)+ZZ
    ZCJQ3(JL,JT) = ZCJQ3(JL,JT)+ZCOEF15(JL,JT)*ZZ
    ZCOEF1 = ZCOEF1+ZCJQ35(JL,JT)*ZZ
    PTSKM1M(JL,JT) = PTSKM1M(JL,JT)-RCPD*ZDELTA*ZCOEF1
  ENDDO
ENDDO


!      3. Compute coefficients for dry static energy flux Js and 
!         moisture flux Jq, expressed in Sl,Ql and Ssk 

DO JT=KTILES,1,-1
  DO JL=KIDIA,KFDIA
    PALPHAS(JL,JT) = PALPHAS(JL,JT)-PRHOCQU5(JL,JT) &
     &    * (PQSKM1M5(JL,JT)-PDQSDT5(JL,JT)*PTSKM1M5(JL,JT))*ZCJQ4(JL,JT)
    PRHOCQU(JL,JT) = PRHOCQU(JL,JT)-PALPHAS5(JL,JT) &
     &    * (PQSKM1M5(JL,JT)-PDQSDT5(JL,JT)*PTSKM1M5(JL,JT))*ZCJQ4(JL,JT)
    PQSKM1M(JL,JT) = PQSKM1M(JL,JT)-PRHOCQU5(JL,JT)*PALPHAS5(JL,JT) &
     &    * ZCJQ4(JL,JT)
    PTSKM1M(JL,JT) = PTSKM1M(JL,JT)+PRHOCQU5(JL,JT)*PALPHAS5(JL,JT) &
     &    * PDQSDT5(JL,JT)*ZCJQ4(JL,JT)
    PDQSDT(JL,JT) = PDQSDT(JL,JT)+PRHOCQU5(JL,JT)*PALPHAS5(JL,JT) &
     &    * PTSKM1M5(JL,JT)*ZCJQ4(JL,JT)
    ZCJQ4(JL,JT) = 0.0_JPRB

    PRHOCQU(JL,JT) = PRHOCQU(JL,JT) &
     &    - PALPHAS5(JL,JT)*PDQSDT5(JL,JT)*ZICPTM15(JL,JT)*ZCJQ3(JL,JT)
    PALPHAS(JL,JT) = PALPHAS(JL,JT) &
     &    - PRHOCQU5(JL,JT)*PDQSDT5(JL,JT)*ZICPTM15(JL,JT)*ZCJQ3(JL,JT)
    PDQSDT(JL,JT) = PDQSDT(JL,JT) &
     &    - PRHOCQU5(JL,JT)*PALPHAS5(JL,JT)*ZICPTM15(JL,JT)*ZCJQ3(JL,JT)
    ZICPTM1(JL,JT) = ZICPTM1(JL,JT) &
     &    - PRHOCQU5(JL,JT)*PALPHAS5(JL,JT)*PDQSDT5(JL,JT)*ZCJQ3(JL,JT)
    ZCJQ3(JL,JT) = 0.0_JPRB

    PALPHAL(JL,JT) = PALPHAL(JL,JT)+PRHOCQU5(JL,JT)*ZCJQ2(JL,JT)
    PRHOCQU(JL,JT) = PRHOCQU(JL,JT)+PALPHAL5(JL,JT)*ZCJQ2(JL,JT)
    ZCJQ2(JL,JT) = 0.0_JPRB
    PRHOCHU(JL,JT) = PRHOCHU(JL,JT)-ZCJS3(JL,JT)
    ZCJS3(JL,JT) = 0.0_JPRB
    PRHOCHU(JL,JT) = PRHOCHU(JL,JT)+ZCJS1(JL,JT)
    ZCJS1(JL,JT) = 0.0_JPRB

    PTSKM1M(JL,JT) = PTSKM1M(JL,JT)+ZICPTM1(JL,JT)/PSSKM1M5(JL,JT)
    PSSKM1M(JL,JT) = PSSKM1M(JL,JT)-PTSKM1M5(JL,JT)*ZICPTM1(JL,JT) &
     &             / PSSKM1M5(JL,JT)**2
  ENDDO
ENDDO


!      2. Prepare tile independent arrays

IF (LELWDD) THEN
  DO JL=KIDIA,KFDIA
    PTSKRAD(JL) = PTSKRAD(JL)-12._JPRB*PEMIS5(JL)*RSIGMA*ZDLWDT(JL) &
     &          * PTSKRAD5(JL)**2
    ZDLWDT(JL) = 0.0_JPRB
  ENDDO
ELSE
  DO JL=KIDIA,KFDIA
    PSLRFL(JL) = PSLRFL(JL)+4._JPRB*ZDLWDT(JL)/PTSKRAD5(JL)
    PTSKRAD(JL) = PTSKRAD(JL)-4._JPRB*PSLRFL5(JL)*ZDLWDT(JL) &
     &          / PTSKRAD5(JL)**2
    ZDLWDT(JL) = 0.0_JPRB
  ENDDO
ENDIF
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SURFSEBSAD_CTL_MOD:SURFSEBSAD_CTL',1,ZHOOK_HANDLE)

!      7.  Wrap up

END SUBROUTINE SURFSEBSAD_CTL
END MODULE SURFSEBSAD_CTL_MOD
