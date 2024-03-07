MODULE VLAMSK_MOD
CONTAINS
SUBROUTINE VLAMSK(KIDIA,KFDIA,KLON,KTILES,LDSICE,KTVL,KTVH,&
 & PTSTEP,PTSKM1M,PTSRF,&
 & PSNM,PRSN,PSNTICE,&
 & PWSAM1M,KSOTY,PSSDP2,PSSDP3,&
 & YDCST,YDVEG,YDSOIL,YDURB,LSICOUP,&
 & PLAMSK)

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_CST   , ONLY : TCST
USE YOS_VEG   , ONLY : TVEG
USE YOS_SOIL  , ONLY : TSOIL
USE YOS_URB   , ONLY : TURB
USE YOMSURF_SSDP_MOD
! (C) Copyright 2016- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     ------------------------------------------------------------------

!**   *VLAMSK* - COMPUTE Skin layer conductivity 

!     PURPOSE
!     -------

!     COMPUTE SKIN LAYER CONDUCTIVITY

!     INTERFACE
!     ---------

!     *VLAMSK* IS CALLED BY *SURFEXCDRIVER*

!     INPUT PARAMETERS (INTEGER):

!     *KIDIA*        START POINT
!     *KFDIA*        END POINT
!     *KLON*         NUMBER OF GRID POINTS PER PACKET
!     *KTILES*       NUMBER OF TILES 
!     KTVL    :    Dominant low vegetation type 
!     KTVH    :    Dominant high vegetation type  

!     Real (in) 
!     PTSTEP     : Time step    (s)

!     Reals with tile index (in) 
!     PTSKM1M :    Skin temperature at T-1                    (K)
!     PTSRF   :    Surface temperature at T-1 unde each tile  (K) 

!     Reals (in)
!     PSNTICE :    Snow temperature on top of the sea-ice     (K) 
!     PWSAM1M : Soil moisture 

!     Integers(in)
!     KSOTY  : Soil type 

!     Real with tile index (out) 
!     PLAMSK :        Tiled Skin layer conductivity 

!     METHOD
!     ------

!     SEE DOCUMENTATION

!     E. Dutra , ECMWF, 04/04/2016 
!     I. Ayan-Miguez (BSC), July 2023 Refactorization of calibrated surface spatially distributed parameters

!     MODIFED
!     M. Kelbling and S. Thober (UFZ) 11/6/2020 use of parameter values defined in namelist
!     J. McNorton           24/08/2022  urban tile
!     I. Ayan-Miguez (BSC), July 2023 Refactorization of calibrated surface spatially distributed parameters

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTILES 
LOGICAL           ,INTENT(IN)    :: LDSICE(:)
INTEGER(KIND=JPIM),INTENT(IN)    :: KTVH(:) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTVL(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSTEP
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSNM(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRSN(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSKM1M(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSRF(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSNTICE(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWSAM1M(:,:)
INTEGER(KIND=JPIM),INTENT(IN)    :: KSOTY(:)

TYPE(TCST)        ,INTENT(IN)    :: YDCST
TYPE(TVEG)        ,INTENT(IN)    :: YDVEG
TYPE(TSOIL)       ,INTENT(IN)    :: YDSOIL
TYPE(TURB)        ,INTENT(IN)    :: YDURB

LOGICAL           ,INTENT(IN)    :: LSICOUP
 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLAMSK(:,:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSDP2(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSDP3(:,:,:)

!*    LOCAL STORAGE
!     ----- -------

REAL(KIND=JPRB)  :: ZSTABEXSN,ZSNOW_GLACIER
REAL(KIND=JPRB)  :: ZSNOWSK,ZTMP1,ZTMP2,ZTMP3,ZTMP4, ZFF

INTEGER(KIND=JPIM) :: JL,JT
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE


#include "fcsurf.h"

!     ------------------------------------------------------------------

!*       1.     INITIALIZE CONSTANTS
!               ---------- ----------


IF (LHOOK) CALL DR_HOOK('VLAMSK_MOD:VLAMSK',0,ZHOOK_HANDLE)
ASSOCIATE(RTT=>YDCST%RTT,RVLAMSKL2D=>PSSDP2(:,SSDP2D_ID%NRVLAMSKL2D), &
     & RVLAMSKH2D=>PSSDP2(:,SSDP2D_ID%NRVLAMSKH2D), RVLAMSKSL2D=>PSSDP2(:,SSDP2D_ID%NRVLAMSKSL2D), &
     & RVLAMSKSH2D=>PSSDP2(:,SSDP2D_ID%NRVLAMSKSH2D), &
     & RVLAMSK_DESERT=>YDVEG%RVLAMSK_DESERT, RVLAMSKS_DESERT=>YDVEG%RVLAMSKS_DESERT, &
     & RVLAMSK_SNOW=>YDVEG%RVLAMSK_SNOW, RVLAMSKS_SNOW=>YDVEG%RVLAMSKS_SNOW, &
     & RQSNCR=>YDSOIL%RQSNCR,RHOCI=>YDSOIL%RHOCI, RHOICE=>YDSOIL%RHOICE, RURBTC=>YDURB%RURBTC, &
     & RTF1=>YDSOIL%RTF1, RTF2=>YDSOIL%RTF2, RTF3=>YDSOIL%RTF3, RTF4=>YDSOIL%RTF4, &
     & RLAMBDADRYM3D=>PSSDP3(:,:,SSDP3D_ID%NRLAMBDADRYM3D), RWSATM3D=>PSSDP3(:,:,SSDP3D_ID%NRWSATM3D), &
     & RLAMSAT1M3D=>PSSDP3(:,:,SSDP3D_ID%NRLAMSAT1M3D), &
     & RLAMBDAMIN=>YDSOIL%RLAMBDAMIN, RLAMBDAMAX=>YDSOIL%RLAMBDAMAX, RSMINZ=>YDSOIL%RSMINZ, &
     & RSNLARGE=>YDSOIL%RSNLARGE, RSNLARGESN=>YDSOIL%RSNLARGESN, RSNLARGEWT=>YDSOIL%RSNLARGEWT, &
     & RSNRTTEMP=>YDSOIL%RSNRTTEMP, RSNSNOW=>YDSOIL%RSNSNOW, RSNSNOWHVEG=>YDSOIL%RSNSNOWHVEG)

! ZLARGE=1.E10_JPRB          ! large number to impose Tsk=SST
! ZLARGESN=50._JPRB          ! large number to constrain Tsk variations in case
!                             !   of melting snow
! ZLARGEWT=20._JPRB          ! 1/lamdaSK(w)+1/lamdaSK(tvh)
! ZRTTMEPS=RTT-0.2_JPRB      ! slightly below zero to start snow melt
! ZSNOW=7._JPRB
ZSNOW_GLACIER=8._JPRB
!ZSNOWHVEG=20._JPRB

!     ------------------------------------------------------------------

!          2.    Set-up default values from look-up tables 
!                ------- ---------- ---------- --- -----------

DO JT=1,KTILES
  SELECT CASE(JT)
  
  CASE(1,9)
    PLAMSK(KIDIA:KFDIA,JT)=RSNLARGE
  
  CASE(2)
    IF (LSICOUP) THEN
      PLAMSK(KIDIA:KFDIA,JT)=RSNLARGE
    ELSE
      DO JL=KIDIA,KFDIA
        IF (PTSKM1M(JL,JT) > PTSRF(JL,JT)) THEN
          PLAMSK(JL,JT)=RVLAMSKS_SNOW
        ELSE
          PLAMSK(JL,JT)=RVLAMSK_SNOW
        ENDIF
      ENDDO
   ENDIF
   
   CASE(3)
    PLAMSK(KIDIA:KFDIA,JT)=RSNLARGEWT
    
   CASE(4)
    DO JL=KIDIA,KFDIA
      IF(PTSKM1M(JL,JT) > PTSRF(JL,JT) ) THEN
        PLAMSK(JL,JT) = RVLAMSKSL2D(JL)
      ELSE
        PLAMSK(JL,JT) = RVLAMSKL2D(JL)
      ENDIF
    ENDDO
  
  CASE(5) 
    WHERE(PTSKM1M(KIDIA:KFDIA,JT) >= PTSRF(KIDIA:KFDIA,JT) .AND. PTSKM1M(KIDIA:KFDIA,JT) > RSNRTTEMP )
      PLAMSK(KIDIA:KFDIA,JT) = RSNLARGESN
!   ELSEWHERE(LDSICE(KIDIA:KFDIA))
!     PLAMSK(KIDIA:KFDIA,JT) = ZSNOW_SICE
    ELSEWHERE(SUM(PSNM(KIDIA:KFDIA,:),DIM=2)>9000._JPRB)
      PLAMSK(KIDIA:KFDIA,JT) = ZSNOW_GLACIER
    ELSEWHERE
      PLAMSK(KIDIA:KFDIA,JT) = RSNSNOW
    ENDWHERE
    
  CASE(6)
    DO JL=KIDIA,KFDIA
      IF(PTSKM1M(JL,JT) > PTSRF(JL,JT) ) THEN
        PLAMSK(JL,JT) = RVLAMSKSH2D(JL)
      ELSE
        PLAMSK(JL,JT) = RVLAMSKH2D(JL)
      ENDIF
    ENDDO
  
  CASE(7)
    DO JL=KIDIA,KFDIA
      IF(PTSKM1M(JL,JT) > PTSRF(JL,JT) ) THEN
        PLAMSK(JL,JT) = RVLAMSKSH2D(JL)
      ELSE
      ! ARDU: comment this out   
      !!IF (YDSOIL%LESNML ) THEN
      !!  ! When the multi-layer is active we avoid ZSNOWHVEG
      !!  ! Avoid instabilities ... To be re-evaluated ... 
      !!  PLAMSK(JL,JT) = RVLAMSK(KTVH(JL))
      !!ELSE  
        PLAMSK(JL,JT) = RSNSNOWHVEG !
      !!ENDIF
      ENDIF
    ENDDO
  
  CASE(8)
    DO JL=KIDIA,KFDIA
      IF(PTSKM1M(JL,JT) > PTSRF(JL,JT) ) THEN
        PLAMSK(JL,JT) = RVLAMSKS_DESERT
      ELSE
        PLAMSK(JL,JT) = RVLAMSK_DESERT
      ENDIF
    ENDDO

  CASE(10)
    DO JL=KIDIA,KFDIA
      IF(PTSKM1M(JL,JT) > PTSRF(JL,JT) ) THEN
        PLAMSK(JL,JT) = RURBTC
      ELSE
        PLAMSK(JL,JT) = RURBTC
      ENDIF
    ENDDO


  END SELECT
END DO

!* STABILITY FACTOR FOR EXPLICIT SNOW SCHEME AND FOREST-SNOW MIX
!      3. Compute a stability factor for the explicit snow scheme and
!         forest-snow mix to prevent numerical instability for "thin-rough" snow 
!         when running with long time-step (e.g. 1-hour)
JT=7
DO JL=KIDIA,KFDIA
  ZSTABEXSN=PTSTEP/(MAX(RQSNCR,(PSNM(JL,1)/PRSN(JL,1)))*RHOCI*PRSN(JL,1)/RHOICE)
  PLAMSK(JL,JT) = PLAMSK(JL,JT)/(1._JPRB+ZSTABEXSN*PLAMSK(JL,JT))
ENDDO 

!!========================================================
!! New formulations
IF (YDSOIL%LESKTI5) THEN
  JT=5
  DO JL=KIDIA,KFDIA
    ZTMP1 = 1._JPRB/MAX(RSMINZ,(PSNM(JL,1)/PRSN(JL,1))) ! 1/DZ
    ZTMP2=RHOCI*PRSN(JL,1)/RHOICE  ! rhoC
    ZSNOWSK=FSNTCOND(PRSN(JL,1))
    PLAMSK(JL,JT)=2._JPRB*ZSNOWSK*ZTMP1
    
!     stability  original
!     ZSTABEXSN=PTSTEP*ZTMP1/ZTMP2
!     print*,'1',PSNM(JL,1)/PRSN(JL,1),PLAMSK(JL,JT),ZSTABEXSN
! stability new 
    
!     ZTMP3=ZTMP1*SQRT(ZSNOWSK*PTSTEP/ZTMP2)  ! x in f(x)
!     ZTMP4=ZTMP3/(1._JPRB+ZTMP3**1.3_JPRB)**(0.7692307692307_JPRB) ! f(x) 1/1.3 == 0.769
!     ZSTABEXSN = ZTMP4 / SQRT(ZSNOWSK*ZTMP2/PTSTEP)
! 
!     PLAMSK(JL,JT) = PLAMSK(JL,JT)/(1._JPRB+ZSTABEXSN*PLAMSK(JL,JT))
!      print*,'2',PLAMSK(JL,JT),ZSTABEXSN
  ENDDO
ENDIF
  
IF (YDSOIL%LESKTI8) THEN
  JT=8
  DO JL=KIDIA,KFDIA
    ZTMP1 = 1._JPRB/YDSOIL%RDAW(1) ! 1/DZ

! added fix to be consistent with Peters-Lidard et al. 1998 
      IF(PTSRF(JL,JT) < RTF1.AND.PTSRF(JL,JT) > RTF2) THEN
        ZFF=0.5_JPRB*(1.0_JPRB-SIN(RTF4*(PTSRF(JL,JT)-RTF3)))
      ELSEIF (PTSRF(JL,JT) <= RTF2) THEN
        ZFF=1.0_JPRB
      ELSE
        ZFF=0.0_JPRB
      ENDIF
    !ZFF = 0._JPRB
    ! MAX(1,KSOTY(JL)) to avoid floating exceptions
    ZSNOWSK=MAX(RLAMBDAMIN,MIN(RLAMBDAMAX,FSOILTCOND(PWSAM1M(JL,1),RLAMBDADRYM3D(JL,1),RWSATM3D(JL,1),RLAMSAT1M3D(JL,1),ZFF)))
    PLAMSK(JL,JT)=2._JPRB*ZSNOWSK*ZTMP1
  ENDDO

ENDIF 



END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('VLAMSK_MOD:VLAMSK',1,ZHOOK_HANDLE)
END SUBROUTINE VLAMSK
END MODULE VLAMSK_MOD
