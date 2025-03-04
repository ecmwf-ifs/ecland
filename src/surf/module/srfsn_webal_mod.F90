MODULE SRFSN_WEBAL_MOD
CONTAINS
SUBROUTINE SRFSN_WEBAL(KIDIA,KFDIA,KLON,KLEVSN,LDLAND,&
 & PTMST,LLNOSNOW,PFRSN,&
 & PSSNM1M,PWSNM1M,PRSNM1M,PTSNM1M,&
 & PTSURF,PHFLUX,ZSNOTRS,PSNOWF,PRAINF,PEVAPSN,PSURFCOND,&
 & PAPRS,&
 & YDSOIL,YDCST,&
 & PSSN,PWSN,PTSN,&
 & PGSN,PRUSN,PMELTSN,PFREZSN,&
 & PDHTSS,PDHSSS)

USE PARKIND1 , ONLY : JPIM, JPRB, JPRD
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_SOIL , ONLY : TSOIL 
USE YOS_CST  , ONLY : TCST
USE EC_LUN   , ONLY : NULERR
USE SRFSN_TRISOLVER_MOD, ONLY: TRISOLVER

USE ABORT_SURF_MOD

! (C) Copyright 2015- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SRFSN_WEBAL* - Snow water & energy balance 
!     PURPOSE.
!     --------
!     THIS ROUTINE COMPUTES ENERGY AND WATER BALANCE IN THE SNOWPACK

!**   INTERFACE.
!     ----------
!          *SRFSN_WEBAL* IS CALLED FROM *SRFSN_DRIVER*.

!     PARAMETER   DESCRIPTION                                    UNITS
!     ---------   -----------                                    -----

!     INPUT PARAMETERS (INTEGER):
!    *KIDIA*      START POINT
!    *KFDIA*      END POINT
!    *KLON*       NUMBER OF GRID POINTS PER PACKET
!    *KLEVSN*     VERTICAL SNOW LAYERS

!     INPUT PARAMETERS (REAL):
!    *PTMST*      TIME STEP                                      S

!     INPUT PARAMETERS (LOGICAL):
!    *LDLAND*     LAND/SEA MASK (TRUE/FALSE)
!    *LLNOSNOW*   NO-SNOW/SNOW MASK (TRUE IF NO-SNOW)

!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!    *PWLM1M*     SKIN RESERVOIR WATER CONTENT                 kg/m**2
!    *PFRSN*      total snow fraction tile 5 + 7
!    *PSSNM1M*    TOTAL SNOW MASS IN EACH LAYER (per unit area) kg/m**2
!    *PWSNM1M*    LIQUID WATER CONTENT IN SNOW                 kg/m**2
!    *PRSNM1M*    SNOW DENSITY in each layer                   kg/m**3
!    *PTSNM1M*    TEMPERATURE OF SNOW LAYER                    K
!    *PTSURF*     TEMPERATURE OF TOP SOIL LAYER                K
!    *PHFLUX*     CONDUCTIVE HEAT FLUX INTO THE SNOWPACK       W/m**2
!    *PSNOWF*     TOTAL SNOW FLUX AT THE SURFACE         KG/M**2/S
!    *PRAINF*     TOTAL RAIN FLUX AT THE SURFACE         KG/M**2/S
!    *PEVAPSN*    EVAPORATION FROM SNOW UNDER FOREST           KG/M2/S
!    *PSURFCOND*  THERMAL CONDUCTIVITY OF TOP SOIL LAYER
!    *ZSNOTRS*    SOLAR RADIATION FLUX INTO THE SNOWPACK      W/m**2
!    *PAPRS*      ATMOSPHERIC PRESSURE ON BOTTOM HALF LEVEL   Pa
!    

!     OUTPUT PARAMETERS AT T+1 (UNFILTERED,REAL):
!    *PSSN*        SNOW MASS each layer (per unit area)        kg/m**2
!    *PWSN*        LIQUID WATER CONTENT IN SNOW                 kg/m**2
!    *PTSN*        TEMPERATURE OF SNOW LAYER                    K

!    FLUXES FROM SNOW SCHEME:
!    *PGSN*       GROUND HEAT FLUX FROM SNOW DECK TO SOIL     W/M**2   (#)
!    *PRUSN*      FLUX OF MELT WATER FROM SNOW TO SOIL       KG/M**2/S (#)
!    *PMELTSN*    LATENT HEAT OF MELTED WATER                 J/m**2
!    *PFREZSN*    LATENT HEAT OF REFREEZE                     J/m**2



!     OUTPUT PARAMETERS (DIAGNOSTIC):
!    *PDHIIS*     Diagnostic array for interception layer (see module yomcdh)

!     METHOD.
!     -------
!          

!     EXTERNALS.
!     ----------
!          NONE.

!     REFERENCE.
!     ----------
!          

!     Modifications:
!     Original   E. Dutra      ECMWF     04/12/2015
!                G. Arduini              01/09/2021

!     ------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments 
INTEGER(KIND=JPIM), INTENT(IN)   :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KLON
INTEGER(KIND=JPIM), INTENT(IN)   :: KLEVSN
REAL(KIND=JPRB)   , INTENT(IN)   :: PTMST
LOGICAL           , INTENT(IN)   :: LDLAND(:)
LOGICAL           , INTENT(IN)   :: LLNOSNOW(:) 

REAL(KIND=JPRB)   , INTENT(IN)   :: PFRSN(:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PSSNM1M(:,:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PWSNM1M(:,:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PRSNM1M(:,:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PTSNM1M(:,:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PTSURF(:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PHFLUX(:)
REAL(KIND=JPRB)   , INTENT(IN)   :: ZSNOTRS(:,:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PSNOWF(:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PRAINF(:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PEVAPSN(:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PSURFCOND(:)
REAL(KIND=JPRB)   , INTENT(IN)   :: PAPRS(:)

TYPE(TSOIL)       , INTENT(IN)   :: YDSOIL
TYPE(TCST)        , INTENT(IN)   :: YDCST

REAL(KIND=JPRB)   , INTENT(OUT)  :: PSSN(:,:)
REAL(KIND=JPRB)   , INTENT(OUT)  :: PWSN(:,:)
REAL(KIND=JPRB)   , INTENT(OUT)  :: PTSN(:,:)
REAL(KIND=JPRB)   , INTENT(OUT)  :: PGSN(:)
REAL(KIND=JPRB)   , INTENT(OUT)  :: PRUSN(:)
REAL(KIND=JPRB)   , INTENT(OUT)  :: PMELTSN(:,:)
REAL(KIND=JPRB)   , INTENT(OUT)  :: PFREZSN(:,:)

REAL(KIND=JPRB)   , INTENT(OUT)  :: PDHTSS(:,:,:)
REAL(KIND=JPRB)   , INTENT(OUT)  :: PDHSSS(:,:,:)

! Local variables 
REAL(KIND=JPRB) :: ZDSN(KLON,KLEVSN)    ! actual snow depth
REAL(KIND=JPRB) :: ZSNHC(KLON,KLEVSN)   ! snow heat capacity 
REAL(KIND=JPRB) :: ZICE(KLON,KLEVSN)    ! snow ice content (PSSN-PWSN)
REAL(KIND=JPRB) :: ZSNCOND(KLON,KLEVSN) ! Snow thermal conductivity 
REAL(KIND=JPRB) :: ZSNCONDH(KLON,KLEVSN+1) ! THERMAL CONDUCTIVITY IN HALF LEVEL TERM 
REAL(KIND=JPRB) :: ZTA(KLON,KLEVSN),ZTB(KLON,KLEVSN),ZTC(KLON,KLEVSN),ZTR(KLON,KLEVSN)  ! TERMS TO TRI-DIAG
REAL(KIND=JPRB) :: ZTSTAR(KLON,KLEVSN)  ! New snow temperature 
REAL(KIND=JPRB) :: ZISTAR(KLON,KLEVSN)  ! New ice content 
REAL(KIND=JPRB) :: ZWSTAR(KLON,KLEVSN)  ! New liquid water content 
REAL(KIND=JPRB) :: ZLIQF(KLON,0:KLEVSN) ! LIQUID WATER FLUX 

REAL(KIND=JPRB) :: ZGSN,ZWCAP,ZQ,ZGSNRES

REAL(KIND=JPRB) :: ZSNVCOND
REAL(KIND=JPRB) :: ZTMP0,ZTMP1
REAL(KIND=JPRB)  :: ZTMST,ZIHCAP,ZSOILDEPTH1,ZEPSILON
REAL(KIND=JPRB) :: ZSOILRES, ZHOICE, ZSNRES, ZWHCAP
REAL(KIND=JPRB) :: ZDTM, ZDTH
INTEGER(KIND=JPIM) :: JL,JK,KLACT
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!! INCLUDE FUNCTIONS

#include "fcsurf.h"

!    -----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SRFSN_WEBAL_MOD:SRFSN_WEBAL',0,ZHOOK_HANDLE)

!    -----------------------------------------------------------------
ASSOCIATE(RHOCI=>YDSOIL%RHOCI,RHOICE=>YDSOIL%RHOICE,&
 & RTT=>YDCST%RTT,RLMLT=>YDCST%RLMLT,&
 & RALAMSN=>YDSOIL%RALAMSN, RLAMICE=>YDSOIL%RLAMICE,&
 & RDSNMAX=>YDSOIL%RDSNMAX, SNHCONDAV=>YDSOIL%SNHCONDAV, &
 & SNHCONDBV=>YDSOIL%SNHCONDBV, SNHCONDCV=>YDSOIL%SNHCONDCV, &
 & SNHCONDPOV=>YDSOIL%SNHCONDPOV )

! RLMLT (latent heat of fusion J Kg -1)
ZTMST = 1.0_JPRB/PTMST 
ZIHCAP = RHOCI/RHOICE  ! Ice heat capacity (J K-1 Kg-1)
ZSOILDEPTH1=YDSOIL%RDAW(1) ! 1sth Soil layer depth
ZEPSILON=10._JPRB*EPSILON(ZEPSILON)
ZWHCAP=4180._JPRB ! J K-1 Kg-1

DO JL=KIDIA,KFDIA
  IF (.not. YDSOIL%LESNICE) THEN
    ZSOILDEPTH1=YDSOIL%RDAW(1) ! 1sth Soil layer depth
  else
    IF (LDLAND(JL)) THEN
      ZSOILDEPTH1=YDSOIL%RDAW(1) ! 1sth Soil layer depth
    ELSE
      ZSOILDEPTH1=YDSOIL%RDAI(1) ! 1sth Soil layer depth
    ENDIF
  ENDIF
  IF (LLNOSNOW(JL)) THEN 
    PSSN(JL,1) = MAX(0._JPRB, PSSNM1M(JL,1) + PTMST*(PSNOWF(JL)+PEVAPSN(JL)))  ! update snow mass with snowfall
    PSSN(JL,2:KLEVSN) = 0._JPRB 
    PTSN(JL,:) = RTT
    PWSN(JL,:) = 0.0_JPRB
    PGSN(JL)   = 0.0_JPRB
    PRUSN(JL)  = 0.0_JPRB
    PMELTSN(JL,:) = 0.0_JPRB
    PFREZSN(JL,:) = 0.0_JPRB 
    
!! DDH DIAGNOSTICS 
    IF (SIZE(PDHTSS) > 0 .AND. SIZE(PDHSSS) > 0) THEN
      ! Snow heat capacity per unit surface
      !PDHTSS(JL,1:KLEVSN,1)=SUM(ZSNHC(JL,1:KLEVSN)*PTMST)
      PDHTSS(JL,1:KLEVSN,1)=0._JPRB
      ! Snow temperature
      PDHTSS(JL,1:KLEVSN,2)=RTT
      ! Snow energy per unit surface
      PDHTSS(JL,1:KLEVSN,3)=0._JPRB
      ! Snow thermally active depth 
      PDHTSS(JL,1:KLEVSN,4)=0._JPRB
      ! Snow basal heat flux
      PDHTSS(JL,1:KLEVSN,13)=0._JPRB
      ! phase changes
      PDHTSS(JL,1:KLEVSN,14)=0._JPRB
      ! Snow heat content change
      PDHTSS(JL,1:KLEVSN,15)=0._JPRB
      
      ! SNOW MASS
      PDHSSS(JL,1:KLEVSN,1)=0._JPRB
      ! snow evap
      PDHSSS(JL,1:KLEVSN,4)=0._JPRB
      ! phase changes 
      PDHSSS(JL,1:KLEVSN,5)=0._JPRB
    ENDIF
  ELSE

!! Preparation
    KLACT=0
    DO JK=1,KLEVSN
      IF (PSSNM1M(JL,JK) > ZEPSILON ) KLACT=JK
    ENDDO
    DO JK=1,KLEVSN
      ZICE(JL,JK)    = PSSNM1M(JL,JK) - PWSNM1M(JL,JK) ! Ice (Kg m-2)
      ! Limit thermally active snow depth to 1 meter of snow (for glaciers in particular)
      ZDSN(JL,JK)    = MIN(RDSNMAX, (PSSNM1M(JL,JK) / PRSNM1M(JL,JK))) ! read snow depth (m)
      !*ZSNHC(JL,JK)   = (ZIHCAP*PRSNM1M(JL,JK) * MIN(RDSNMAX, (ZICE(JL,JK)/PRSNM1M(JL,JK))) + ZWHCAP*PWSNM1M(JL,JK) ) * ZTMST
      ZSNHC(JL,JK)   = (ZIHCAP*PRSNM1M(JL,JK) * MIN(RDSNMAX, ZDSN(JL,JK))) * ZTMST
      ! heat conductivity from water vapor transport into the snowpack
      ZSNVCOND=(SNHCONDPOV/PAPRS(JL))*MAX(0._JPRB,(SNHCONDAV-SNHCONDBV/(PTSNM1M(JL,JK)-SNHCONDCV)))
      ! snow heat conductivity 
      ZSNCOND(JL,JK) = FSNTCOND(PRSNM1M(JL,JK))+ZSNVCOND  ! add the thermal cond from water vapor transfer
    ENDDO
    ! special case for 1 layer only
    IF (KLACT == 1 ) ZSNHC(JL,1) = MAX(ZEPSILON, ZSNHC(JL,1))
     
    ZSNCONDH(JL,1)=PFRSN(JL)*ZSNCOND(JL,1) / MAX(ZEPSILON,(0.5_JPRB*ZDSN(JL,1))*(0.5_JPRB*ZDSN(JL,1)))   ! ACTUALY NOT USED ! (W m-2 K-1)
    DO JK=2,KLACT
      ZSNCONDH(JL,JK)=PFRSN(JL)*2._JPRB*(ZDSN(JL,JK-1)*ZSNCOND(JL,JK-1)+ZDSN(JL,JK)*ZSNCOND(JL,JK))/&
                  & MAX(ZEPSILON, (ZDSN(JL,JK-1)+ZDSN(JL,JK))*(ZDSN(JL,JK-1)+ZDSN(JL,JK)))
    ENDDO
    ! Divided by two to decrease heat flux between snow and soil (accounting for litter,organic etc). 
    ! This needs to be in double-precision to avoid some problematic computation between small numbers 
     ZSNCONDH(JL,KLACT+1)=REAL(PFRSN(JL),KIND=JPRD)*&
                      &(REAL(ZDSN(JL,KLACT),KIND=JPRD)*REAL(ZSNCOND(JL,KLACT),KIND=JPRD)+ZSOILDEPTH1*PSURFCOND(JL))/&
                      &MAX(ZEPSILON, REAL(ZDSN(JL,KLACT)+ZSOILDEPTH1,KIND=JPRD)*REAL(ZDSN(JL,KLACT)+ZSOILDEPTH1,KIND=JPRD))
    ! ZSNCONDH(JL,KLACT+1)=PFRSN(JL)*1._JPRB*(ZDSN(JL,KLACT)*ZSNCOND(JL,KLACT)+ZSOILDEPTH1*PSURFCOND(JL))/&
    !              &MAX(ZEPSILON, (ZDSN(JL,KLACT)+ZSOILDEPTH1)*(ZDSN(JL,KLACT)+ZSOILDEPTH1))

    IF (KLACT > 1 ) THEN 
      JK=1
      ZTA(JL,JK)=0._JPRB
      ZTB(JL,JK)=ZSNHC(JL,JK)+ZSNCONDH(JL,JK+1)
      ZTC(JL,JK)=-ZSNCONDH(JL,JK+1)
      ZTR(JL,JK)=(PHFLUX(JL)-ZSNOTRS(JL,JK))+ZSNHC(JL,JK)*PTSNM1M(JL,JK)
! Add radiative flux to second to bottom layer:
      JK=KLACT
      ZTA(JL,JK)=-ZSNCONDH(JL,JK)
      ZTB(JL,JK)=ZSNHC(JL,JK)+ZSNCONDH(JL,JK)+ZSNCONDH(JL,JK+1)
      ZTC(JL,JK)=0._JPRB
      ZTR(JL,JK)=ZSNHC(JL,JK)*PTSNM1M(JL,JK)+ZSNCONDH(JL,JK+1)*PTSURF(JL)+ZSNOTRS(JL,JK)
      DO JK=2,KLACT-1
        ZTA(JL,JK)=-ZSNCONDH(JL,JK)
        ZTB(JL,JK)=ZSNHC(JL,JK)+ZSNCONDH(JL,JK)+ZSNCONDH(JL,JK+1)
        ZTC(JL,JK)=-ZSNCONDH(JL,JK+1)
        ZTR(JL,JK)=ZSNHC(JL,JK)*PTSNM1M(JL,JK) + ZSNOTRS(JL,JK)
      ENDDO
    ENDIF
    
    ! SOLVE
    ZTSTAR(JL,:) = PTSNM1M(JL,:)
    IF (KLACT == 1 ) THEN
      ! SINGLE LAYER
      ZTB(JL,1)=ZSNHC(JL,1)+ZSNCONDH(JL,2)
      ZTR(JL,1)=(PHFLUX(JL)-ZSNOTRS(JL,1))+ZSNHC(JL,1)*PTSNM1M(JL,1)+ZSNCONDH(JL,2)*PTSURF(JL)
      ZTSTAR(JL,1)=ZTR(JL,1)/ZTB(JL,1)
      ZGSN=ZSNCONDH(JL,2)*(ZTSTAR(JL,1)-PTSURF(JL))
    ELSE
      ! MULTI LAYER
      CALL TRISOLVER(KLACT,KLEVSN,ZTA(JL,:),ZTB(JL,:),ZTC(JL,:),ZTR(JL,:),ZTSTAR(JL,:))
      ZGSN=ZSNCONDH(JL,KLACT+1)*(ZTSTAR(JL,KLACT)-PTSURF(JL))
    ENDIF 
    ! * SPECIAL CASE WHEN THERE IS MELTING ON THE 1ST LAYER
    IF ( ZTSTAR(JL,1) > RTT ) THEN 
      IF (KLACT == 1 ) THEN
        ! SINGLE LAYER
        ZGSN=ZSNCONDH(JL,2)*(RTT-PTSURF(JL))
        ZTSTAR(JL,1)=(PHFLUX(JL)-ZSNOTRS(JL,1)+ZSNHC(JL,1)*PTSNM1M(JL,1)-ZGSN)/MAX(ZEPSILON,ZSNHC(JL,1))
      ELSE
        ! MULTI LAYER
        ! SOLVE SYSTEM SETTING T1 == RTT 
        ZTB(JL,1) = 1._JPRB
        ZTC(JL,1) = 0._JPRB
        ZTR(JL,1) = RTT
        ZTSTAR(JL,:) = PTSNM1M(JL,:)  
        CALL TRISOLVER(KLACT,KLEVSN,ZTA(JL,:),ZTB(JL,:),ZTC(JL,:),ZTR(JL,:),ZTSTAR(JL,:))
        ZGSN=ZSNCONDH(JL,KLACT+1)*(ZTSTAR(JL,KLACT)-PTSURF(JL))
        ! EXPLICIT 1ST LAYER TEMP CALCULATION
        ZTSTAR(JL,1)=(PHFLUX(JL)-ZSNOTRS(JL,1)+ZSNHC(JL,1)*PTSNM1M(JL,1)-ZSNCONDH(JL,2)*(ZTSTAR(JL,1)-ZTSTAR(JL,2)))/MAX(ZEPSILON,ZSNHC(JL,1))
      ENDIF
    ENDIF
    

!     !! UPDATE SOLID/LIQUID CONTENTS OF FIRST LAYER BEFORE FURTHER CALCULATIONS
    ZISTAR(JL,:) = ZICE(JL,:)
    ZWSTAR(JL,:) = PWSNM1M(JL,:)
    ZLIQF(JL,:)  = 0._JPRB
      !! INCLUDE SNOWFALL AND RAINFALL INTERCEPTION 
    ZISTAR(JL,1) = ZICE(JL,1)+PTMST*(PSNOWF(JL)+PEVAPSN(JL))
    ZLIQF(JL,0) = PTMST*PRAINF(JL)
    
    !! PHASE CHANGES AND LIQUID WATER BALANCE STARTING IN LAYER 1
    PMELTSN(JL,:)=0._JPRB
    PFREZSN(JL,:)= 0._JPRB
    ZGSNRES=0._JPRB
    DO JK=1,KLACT
      ! UPDATE LIQUID WATER FROM FLUX ABOVER 
      ZWSTAR(JL,JK) = PWSNM1M(JL,JK) + ZLIQF(JL,JK-1)
      
      ! PHASE CHANGES 
      ZTMP0 = ZSNHC(JL,JK)*(ZTSTAR(JL,JK)-RTT)
      PMELTSN(JL,JK) = MAX(0._JPRB, MIN( ZTMP0 , RLMLT*ZTMST*ZISTAR(JL,JK) ) )
      PFREZSN(JL,JK) = MIN(0._JPRB, MAX( ZTMP0 , -RLMLT*ZTMST*ZWSTAR(JL,JK) ) )
      ZQ = PMELTSN(JL,JK) + PFREZSN(JL,JK)
      
      ! FINAL TEMP. UPDATE
      ZTSTAR(JL,JK) = ZTSTAR(JL,JK) - (ZQ+ZGSNRES)/MAX(ZEPSILON,ZSNHC(JL,JK))
      IF ( ZTSTAR(JL,JK) > RTT ) THEN
        ZGSNRES=ZSNHC(JL,JK)*(RTT-ZTSTAR(JL,JK))
        ZTSTAR(JL,JK)=RTT
      ELSE
        ZGSNRES=0._JPRB
      ENDIF
      
      ! UPDATE SOLID / LIQUID MASS
      ZWSTAR(JL,JK) = ZWSTAR(JL,JK) + ZQ/RLMLT*PTMST 
      ZISTAR(JL,JK) = ZISTAR(JL,JK) - ZQ/RLMLT*PTMST 
      
      ! LIQUD WATER BUDGET
      ZWCAP = FLWC(ZISTAR(JL,JK),PRSNM1M(JL,JK))
      ZLIQF(JL,JK) = MAX( 0._JPRB, (ZWSTAR(JL,JK)-ZWCAP))
      ZWSTAR(JL,JK) = ZWSTAR(JL,JK)-ZLIQF(JL,JK)
    ENDDO
    DO JK=KLACT+1,KLEVSN
      ZTSTAR(JL,JK) = ZTSTAR(JL,KLACT)
    ENDDO  
      
    !! FINAL VALUES
    DO JK=1,KLEVSN
      PTSN(JL,JK) = MIN( RTT, ZTSTAR(JL,JK) )
      PSSN(JL,JK) = MAX( 0._JPRB, ZWSTAR(JL,JK) + ZISTAR(JL,JK) )
      PWSN(JL,JK) = MIN( PSSN(JL,JK), MAX(0._JPRB, ZWSTAR(JL,JK)) )
    ENDDO

    PRUSN(JL) = ZLIQF(JL,KLACT)*ZTMST
    PGSN(JL) = ZGSN - ZGSNRES + ZSNOTRS(JL,KLACT+1)
    

    
    IF (ANY(PTSN(JL,:)>RTT+ZEPSILON)) THEN
      write(*,*) 'Tsn above zero C'
      !*CALL ABORT_SURF('ALL SNOW MELTED')
    ENDIF
    IF (ANY(PTSN(JL,:)<100._JPRB)) THEN
      write(NULERR,*) 'Very cold snow temperature, webal'
      write(NULERR,*) 'Tsn-1',PTSNM1M(JL,:)
      write(NULERR,*) 'Tsn',PTSN(JL,:)
      write(NULERR,*) 'SWE-1',PSSNM1M(JL,:)
      write(NULERR,*) 'SWE',PSSN(JL,:)
      write(NULERR,*) 'Snow frac,heat,pg0',PFRSN(JL),PHFLUX(JL),PGSN(JL)

      WHERE (PTSN(JL,:)<100._JPRB)
          PTSN(JL,:)=100.0_JPRB
      ENDWHERE
      !* CALL ABORT_SURF('Very snow cold temperature')
    ENDIF 
    
    
    !! DDH DIAGNOSTICS 
    IF (SIZE(PDHTSS) > 0 .AND. SIZE(PDHSSS) > 0) THEN
      DO JK=1,KLEVSN
      ! Snow heat capacity per unit surface
        PDHTSS(JL,JK,1)=ZSNHC(JL,JK)*PTMST
      ! Snow temperature
        PDHTSS(JL,JK,2)=PTSNM1M(JL,JK)
      ! Snow energy per unit surface
        PDHTSS(JL,JK,3)=ZSNHC(JL,JK)*PTMST*PTSNM1M(JL,JK)
      ! Snow thermally active depth 
        PDHTSS(JL,JK,4)=PSSNM1M(JL,JK) / PRSNM1M(JL,JK)
      ! Snow basal heat flux
        PDHTSS(JL,1,13)=PGSN(JL)
      ! phase changes
        PDHTSS(JL,JK,14)=PMELTSN(JL,JK)+PFREZSN(JL,JK)
      ! Snow heat content change
        PDHTSS(JL,JK,15)=ZSNHC(JL,JK)*PTMST*(PTSN(JL,JK)-PTSNM1M(JL,JK))
      
      ! SNOW MASS
        PDHSSS(JL,JK,1)=PSSNM1M(JL,JK)
      ! snow evap
        PDHSSS(JL,JK,4)=PEVAPSN(JL)
      ! phase changes (snow melt/refreezing)
        PDHSSS(JL,JK,5)=PDHTSS(JL,JK,14)/RLMLT
!       ! snow liquid water content 
!       PDHSSS(JL,JK,6)=PWSNM1M(JL,JK)
!       ! snow runoff 
!       PDHSSS(JL,JK,8)=PRUSN(JL)
      ENDDO
    ENDIF 

  ENDIF 
ENDDO
                           
  
END ASSOCIATE
!    -----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SRFSN_WEBAL_MOD:SRFSN_WEBAL',1,ZHOOK_HANDLE)
END SUBROUTINE SRFSN_WEBAL

END MODULE SRFSN_WEBAL_MOD
