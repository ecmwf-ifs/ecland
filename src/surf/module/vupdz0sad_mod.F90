MODULE VUPDZ0SAD_MOD

USE PARKIND1  , ONLY : JPIM, JPRB

PRIVATE PZ0WN
PUBLIC  VUPDZ0SAD

CONTAINS
SUBROUTINE VUPDZ0SAD (KIDIA, KFDIA, KLON, KTILES, KSTEP, &
 & KTVL, KTVH, PCVL  , PCVH, PCUR, & 
 & PUMLEV5 , PVMLEV5 ,&
 & PTMLEV5 , PQMLEV5 , PAPHMS5 , PGEOMLEV5, &
 & PDSN5, &
 & PUSTRTI5, PVSTRTI5, PAHFSTI5, PEVAPTI5 , &
 & PTSKTI5 , PCHAR   , PFRTI   , &
 & YDCST   , YDEXC   , YDVEG   , YDFLAKE  , YDURB   ,&
 & PZ0MTI5 , PZ0HTI5 , PZ0QTI5 , PBUOMTI5 , PZDLTI5 , PRAQTI5, &
 & PUMLEV  , PVMLEV  , &
 & PTMLEV  , PQMLEV  , PAPHMS  , PGEOMLEV , &
 & PUSTRTI , PVSTRTI , PAHFSTI , PEVAPTI  , &
 & PTSKTI  , &
 & PZ0MTI  , PZ0HTI  , PZ0QTI  , PBUOMTI  , PZDLTI  , PRAQTI )  

USE YOMHOOK   , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_EXCS  , ONLY : RCHBCD, DRITBL, RCHBBCD, RCHBB, RCHB23A, RITBL, &
 & RCHBA, RCHBHDL, RCDHALF, RCHETB, RCHBD, RCHETA, RCDHPI2, JPRITBL
USE YOS_CST   , ONLY : TCST
USE YOS_EXC   , ONLY : TEXC
USE YOS_VEG   , ONLY : TVEG
USE YOS_FLAKE , ONLY : TFLAKE
USE YOS_URB   , ONLY : TURB

! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     ------------------------------------------------------------------

!**   *VUPDZ0SAD*- COMPUTES Z0M,Z0H,Z0Q OVER SEA; SETS Z0H,Z0Q OVER LAND
!                   (Adjoint)

!     P. Viterbo   ECMWF    22/06/2005     Externalise surf

!     based on

!     J.F. MAHFOUF          E.C.M.W.F.    02/10/95

!     adapted from

!     A.C.M. BELJAARS       E.C.M.W.F.    26/03/90.

!  MODIFIED:
!    M. Janiskova  ECMWF   24/03/2006  modified for tiling of land surface
!                                      (as used in non-linear VUPDZ0 routine)
!    M. Janiskova          22/05/2007  clean-up of z0 initialization
!                                      (as done in vupdz0)
!    G. Balsamo            16/10/2008  lake tile
!    M. Janiskova          21/01/2015  stability param.consistent with NL
!    J. Bidlot             15/12/2018  to use PZ0WN to initialise Z0M over the oceans for step 0
!    J. Bidlot             15/02/2021 Sea state effect in Z0H and Z0Q over the oceans.
!    J. McNorton           24/08/2022  urban tile

!     PURPOSE
!     -------

!     DERIVE Z0M,Z0H AND Z0Q FROM SURFACE FLUXES OVER SEA, SET Z0H AND
!     Z0Q OVER LAND AND DERIVE THE BUOYANCY FLUX.
!     (THE T-1 VALUES ARE UPDATED WITH FLUXES FROM THE PREVIOUS TIME
!      STEP)

!     INTERFACE
!     ---------

!     *VUPDZ0SAD* IS CALLED BY *SURFEXCDRIVERSAD*

!     INPUT PARAMETERS (INTEGER):

!     *KIDIA*        START OF LOOPS
!     *KFDIA*        END OF LOOPS
!     *KLON*         NUMBER OF POINTS IN PACKET
!     *KTILES*       NUMBER OF TILES
!     *KSTEP*        TIME STEP INDEX
!     *KTVL*         LOW VEGETATION TYPE
!     *KTVH*         HIGH VEGETATION TYPE

!     INPUT PARAMETERS (REAL):

!  Trajectory  Perturbation  Description                               Unit
!  PCVL        ---           LOW VEGETATION COVER (CLIMATOLOGICAL)
!  PCVH        ---           HIGH VEGETATION COVER (CLIMATOLOGICAL)
!  PCUR        ---           URBAN COVER
!  PUMLEV5     PUMLEV        WIND X-COMPONENT AT T-1                   m/s
!  PVMLEV5     PVMLEV        WIND Y-COMPONENT AT T-1                   m/s
!  PTMLEV5     PTMLEV        TEMPERATURE AT T-1                        K
!  PQMLEV5     PQMLEV        SPECIFIC HUMUDITY AT T-1                  kg/kg
!  PAPHMS5     PAPHMS        PRESSURE AT T-1                           Pa
!  PGEOMLEV5   PGEOMLEV      GEOPOTENTIAL T-1                          m2/s2
!  PUSTRTI5    PUSTRTI       X-STRESS                                  N/m2
!  PVSTRTI5    PVSTRTI       Y-STRESS                                  N/m2
!  PAHFSTI5    PAHFSTI       SENSIBLE HEAT FLUX                        W/m2
!  PEVAPTI5    PEVAPTI       MOISTURE FLUX                             kg/m2/s
!  PTSKTI5     PTSKTI        SURFACE TEMPERATURE                       K
!  PCHAR       ---           "EQUIVALENT" CHARNOCK PARAMETER           -
!  PFRTI       ---           TILE FRACTION                             -
!  PDSN5       ---          Total snow depth (m) 

!     OUTPUT PARAMETERS (REAL):

!  Trajectory  Perturbation  Description                               Unit
!  PZ0MTI5     PZ0MTI        NEW AERODYNAMIC ROUGHNESS LENGTH          m
!  PZ0HTI5     PZ0HTI        NEW ROUGHNESS LENGTH FOR HEAT             m
!  PZ0QTI5     PZ0QTI        NEW ROUGHNESS LENGTH FOR MOISTURE         m
!  PBUOMTI5    PBUOMTI       BUOYANCY FLUX                             ????
!  PZDLTI5     PZDLTI        Z/L AT LOWEST MODEL LEVEL 
!  PRAQTI5     PRAQTI        PRELIMINARY AERODYNAMIC RESISTANCE        
!                            FOR MOISTURE

!     METHOD
!     ------

!     SEE DOCUMENTATION

!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTILES
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTEP
INTEGER(KIND=JPIM),INTENT(IN)    :: KTVL(KLON)
INTEGER(KIND=JPIM),INTENT(IN)    :: KTVH(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVL(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVH(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCUR(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PUMLEV5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVMLEV5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTMLEV5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQMLEV5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHMS5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEOMLEV5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDSN5(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PUSTRTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PVSTRTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PAHFSTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PEVAPTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSKTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCHAR(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFRTI(:,:)
TYPE(TCST)        ,INTENT(IN)    :: YDCST
TYPE(TEXC)        ,INTENT(IN)    :: YDEXC
TYPE(TVEG)        ,INTENT(IN)    :: YDVEG
TYPE(TFLAKE)      ,INTENT(IN)    :: YDFLAKE
TYPE(TURB)        ,INTENT(IN)    :: YDURB
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0MTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0HTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZ0QTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBUOMTI5(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PZDLTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PRAQTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PUMLEV(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PVMLEV(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTMLEV(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQMLEV(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PAPHMS(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PGEOMLEV(:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PUSTRTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PVSTRTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PAHFSTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PEVAPTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSKTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0MTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0HTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZ0QTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PBUOMTI(:,:) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PZDLTI(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PRAQTI(:,:)

INTEGER(KIND=JPIM) :: JL, JTILE

REAL(KIND=JPRB) :: ZUST(KLON,KTILES), ZUST2(KLON,KTILES)
REAL(KIND=JPRB) :: ZRHO(KLON), ZDU2(KLON), ZDUA(KLON)
REAL(KIND=JPRB) :: ZUST5(KLON,KTILES), ZUST25(KLON,KTILES)
REAL(KIND=JPRB) :: ZRHO5(KLON), ZDU25(KLON), ZDUA5(KLON)

REAL(KIND=JPRB) :: ZAS5(KLON,KTILES), Z1S5(KLON,KTILES)
REAL(KIND=JPRB) :: Z2S5(KLON,KTILES), Z4S5(KLON,KTILES)
REAL(KIND=JPRB) :: Z5S5(KLON,KTILES), Z6S5(KLON,KTILES)
REAL(KIND=JPRB) :: Z7S5(KLON,KTILES), Z8S5(KLON,KTILES)
REAL(KIND=JPRB) :: ZEXP15(KLON,KTILES), ZEXP25(KLON,KTILES)
REAL(KIND=JPRB) :: ZLOG15(KLON,KTILES), ZDIV25(KLON,KTILES)
REAL(KIND=JPRB) :: ZDIV55(KLON,KTILES), ZDIV75(KLON,KTILES)
REAL(KIND=JPRB) :: ZDIV85(KLON,KTILES), ZDIV95(KLON,KTILES)
REAL(KIND=JPRB) :: ZZCDN5(KLON,KTILES), ZROWQ5(KLON,KTILES)
REAL(KIND=JPRB) :: ZROWT5(KLON,KTILES), ZWST25(KLON,KTILES)
REAL(KIND=JPRB) :: ZUST45(KLON,KTILES)
REAL(KIND=JPRB) :: ZNLEV5(KLON,KTILES), ZXLNQ5(KLON,KTILES)
REAL(KIND=JPRB) :: ZPRH15(KLON,KTILES), ZPRQ05(KLON,KTILES)
REAL(KIND=JPRB) :: ZPRQ5(KLON,KTILES), ZZ0MTI5(KLON,KTILES)
REAL(KIND=JPRB) :: Z0S5(KLON), Z3S5(KLON)
REAL(KIND=JPRB) :: ZDIV15(KLON), ZDIV45(KLON), ZDIV65(KLON)

REAL(KIND=JPRB) :: Z1DZ0Q5

REAL(KIND=JPRB) :: ZAS, Z1S, ZCON2, ZIPBL, ZROWQ, ZROWT, ZUST4, ZWST2
REAL(KIND=JPRB) :: ZZCDN
REAL(KIND=JPRB) :: ZNLEV, ZXLNQ, Z1DZ0Q, ZPRH1, ZPRQ0, ZPRQ
REAL(KIND=JPRB) :: Z0S, Z2S, Z3S, Z4S, Z5S, Z6S, Z7S, Z8S
REAL(KIND=JPRB) :: ZEXP1, ZEXP2, ZLOG1
REAL(KIND=JPRB) :: ZRCPDI, ZRGI
REAL(KIND=JPRB) :: ZCDFC
REAL(KIND=JPRB) :: Z0H, Z0Q
REAL(KIND=JPRB) :: ZURBF, ZPCVL, ZPCVH, ZPCVB
! snow
REAL(KIND=JPRB) :: ZSNWGHT5(KLON), ZMLOW5, ZHLOW5, ZTMP5

LOGICAL :: LLINIT

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!             INCLUDE ROUGNESS LENGTH FUNCTIONS
!             ------- -------- ------ ---------
#include "fcz0.h"

!     ------------------------------------------------------------------

!*       1.     INITIALIZE CONSTANTS
!               ---------- ----------

IF (LHOOK) CALL DR_HOOK('VUPDZ0SAD_MOD:VUPDZ0SAD',0,ZHOOK_HANDLE)
ASSOCIATE(RCPD=>YDCST%RCPD, RD=>YDCST%RD, RETV=>YDCST%RETV, RG=>YDCST%RG, &
 & REPDU2=>YDEXC%REPDU2, REPUST=>YDEXC%REPUST, RKAP=>YDEXC%RKAP, &
 & RNUH=>YDEXC%RNUH, RNUM=>YDEXC%RNUM, RNUQ=>YDEXC%RNUQ, RPARZI=>YDEXC%RPARZI, &
 & RZ0ICE=>YDEXC%RZ0ICE, &
 & LEFLAKE=>YDFLAKE%LEFLAKE, LEURBAN=>YDURB%LEURBAN,&
 & RVZ0H=>YDVEG%RVZ0H, RVZ0M=>YDVEG%RVZ0M, &
 & RURBZTM=>YDURB%RURBZTM,RURBZTH=>YDURB%RURBZTH)

IF (KTILES.LT.8) THEN
  STOP "Wrong number of tiles in VDFUPDZ0SAD"
ENDIF

ZCON2 = 2.0_JPRB/3._JPRB
ZRCPDI = 1.0_JPRB/RCPD
ZRGI = 1.0_JPRB/RG

!     PBL HEIGHT FOR W* - EFFECT

ZIPBL = RPARZI

LLINIT= ( KSTEP == 0)

!--------------------------------------------------------------------------

!                -------- TRAJECTORY COMPUTATIONS --------

!--------------------------------------------------------------------------

!*         2.      STABILITY PARAMETERS AND UPDATING OF SEA SURFACE
!                  ROUGHNESS LENGTHS

DO JL = KIDIA, KFDIA
  ZDIV15(JL) = 1.0_JPRB/(RD*PTMLEV5(JL)*(1.0_JPRB+RETV*PQMLEV5(JL)))
  ZRHO5(JL) = PAPHMS5(JL)*ZDIV15(JL)
  Z0S5(JL) = PUMLEV5(JL)**2+PVMLEV5(JL)**2
  IF (REPDU2 >= Z0S5(JL)) THEN
    ZDU25(JL) = REPDU2
  ELSE
    ZDU25(JL) = Z0S5(JL)
  ENDIF
    IF (PDSN5(JL)>0.0_JPRB)THEN
    ZTMP5=PDSN5(JL)/0.25_JPRB
  IF (ZTMP5 < 1.0_JPRB)THEN
      ZSNWGHT5(JL)=ZTMP5
    ELSE
      ZSNWGHT5(JL)=1.0_JPRB
    ENDIF
  ELSE
    ZSNWGHT5(JL)=0_JPRB
  ENDIF
ENDDO

!*         3.   ESTIMATE SURF.FL. FOR STEP 0
!*              (ASSUME NEUTRAL STRATIFICATION)

IF (LLINIT) THEN
  DO JL=KIDIA,KFDIA
    ZDUA5(JL) = SQRT(ZDU25(JL))

!       - Preliminatry value for water
    ZZ0MTI5(JL,1) = PZ0WN(ZDUA5(JL), PGEOMLEV5(JL), PCHAR(JL), RG, RNUM, RKAP)
!       - Sea ice
    ZZ0MTI5(JL,2) = PZ0ICE(RZ0ICE,PFRTI(JL,2))
!       - Wet skin
    ZZ0MTI5(JL,3) = PCVL(JL)*RVZ0M(KTVL(JL))+PCVH(JL)*RVZ0M(KTVH(JL))&
     & +(1.-PCVL(JL)-PCVH(JL))*RVZ0M(0)
!       - Low Vegetation
    ZZ0MTI5(JL,4) = RVZ0M(KTVL(JL))
!       - Exposed snow
    ZMLOW5=PCVL(JL)/(1.0_JPRB-PCVH(JL))*RVZ0M(KTVL(JL))+(1.0_JPRB-PCVL(JL)-PCVH(JL))/(1.0_JPRB-PCVH(JL))*RVZ0M(0)
    ZZ0MTI5(JL,5)=ZSNWGHT5(JL)*RVZ0M(12)+(1.0_JPRB-ZSNWGHT5(JL))*ZMLOW5
!       - High vegetation
    ZZ0MTI5(JL,6) = RVZ0M(KTVH(JL))
!       - Sheltered snow
    ZZ0MTI5(JL,7) = RVZ0M(KTVH(JL))
!       - Bare soil
    ZZ0MTI5(JL,8) = RVZ0M(0)
    IF (LEFLAKE) THEN
!       - Preliminatry value for lakes
      ZZ0MTI5(JL,9) = 1.E-4_JPRB
    ENDIF
    IF (LEURBAN) THEN
!       - Urban
      ZURBF = 0.0_JPRB
      ZPCVL = 0.0_JPRB
      ZPCVH = 0.0_JPRB
      ZPCVB = 0.0_JPRB
      ZURBF=PCUR(JL)
      ZPCVL=MAX(PCVL(JL)*(1.0_JPRB-ZURBF),0.0_JPRB)
      ZPCVH=MAX(PCVH(JL)*(1.0_JPRB-ZURBF),0.0_JPRB)
      ZPCVB=MAX(1.0_JPRB-ZPCVL-ZPCVH-PCUR(JL),0.0_JPRB)
      IF (ZURBF.GT.0.0_JPRB) THEN
       ZZ0MTI5(JL,3)=ZPCVL*RVZ0M(KTVL(JL))+ZPCVH*RVZ0M(KTVH(JL))&
         & +ZPCVB*RVZ0M(0)+ZURBF*RURBZTM
       ZZ0MTI5(JL,5)=(ZPCVL/(1.0_JPRB-ZPCVH))*RVZ0M(12)&
         & +(ZPCVB/(1.0_JPRB-ZPCVH))*RVZ0M(12)&
         & +(ZURBF/(1.0_JPRB-ZPCVH))*RURBZTM
      ENDIF
      ZZ0MTI5(JL,10)=RURBZTM
    ENDIF


  ENDDO

  DO JTILE=1,KTILES
    DO JL=KIDIA,KFDIA
      ZAS5(JL,JTILE) = 1.0_JPRB+PGEOMLEV5(JL)/(RG*ZZ0MTI5(JL,JTILE))
      ZLOG15(JL,JTILE) = LOG(ZAS5(JL,JTILE))
      ZZCDN5(JL,JTILE) = (RKAP/ZLOG15(JL,JTILE))**2
      PUSTRTI5(JL,JTILE)=ZRHO5(JL)*PUMLEV5(JL)*ZDUA5(JL)*ZZCDN5(JL,JTILE)
      PVSTRTI5(JL,JTILE)=ZRHO5(JL)*PVMLEV5(JL)*ZDUA5(JL)*ZZCDN5(JL,JTILE)
      PAHFSTI5(JL,JTILE)=0.0_JPRB
      PEVAPTI5(JL,JTILE)=0.0_JPRB
    ENDDO
  ENDDO
ENDIF

!*         4.      STABILITY PARAMETERS AND FREE CONVECTION
!                  VELOCITY SCALE

!                  ZCDFC is a rough estimate of the drag coefficient used  
!                  to convert w*-gustiness at the lowest model level into u*. 
!                  The value is choosen from Fig. 1 on page 37 of the ECMWF 
!                  seminar proceedings on "Atmopshere-surface interaction", 
!                  ie charqacteristic for 1 m/s in unstable situations. 
ZCDFC=2.E-3_JPRB

DO JTILE=1,KTILES
  DO JL=KIDIA,KFDIA
    ZROWQ5(JL,JTILE) = PEVAPTI5(JL,JTILE)
    ZROWT5(JL,JTILE) = PAHFSTI5(JL,JTILE)*ZRCPDI
    ZDIV25(JL,JTILE) = 1.0_JPRB/(PTSKTI5(JL,JTILE)*ZRHO5(JL))
    PBUOMTI5(JL,JTILE) = -RG*(RETV*ZROWQ5(JL,JTILE)*PTSKTI5(JL,JTILE) &
     & +ZROWT5(JL,JTILE))*ZDIV25(JL,JTILE)
    ZUST45(JL,JTILE) = PUSTRTI5(JL,JTILE)**2+PVSTRTI5(JL,JTILE)**2
    Z1S5(JL,JTILE) = SQRT(ZUST45(JL,JTILE))
    ZDIV45(JL) = 1.0_JPRB/ZRHO5(JL)
    ZUST25(JL,JTILE) = Z1S5(JL,JTILE)*ZDIV45(JL)

!     APPLY W* CORRECTION

    IF (PBUOMTI5(JL,JTILE) > 0.0_JPRB) THEN
      ZWST25(JL,JTILE) = (ZIPBL*PBUOMTI5(JL,JTILE))**ZCON2
      ZUST25(JL,JTILE) = ZUST25(JL,JTILE)+ZCDFC*ZWST25(JL,JTILE)
    ENDIF

    Z2S5(JL,JTILE) = SQRT(ZUST25(JL,JTILE))
    IF (Z2S5(JL,JTILE) >= REPUST) THEN
      ZUST5(JL,JTILE) = Z2S5(JL,JTILE)
    ELSE
      ZUST5(JL,JTILE) = REPUST
    ENDIF
    Z3S5(JL) = -RKAP*PGEOMLEV5(JL)
    ZDIV55(JL,JTILE) = 1.0_JPRB/ZUST5(JL,JTILE)
    PZDLTI5(JL,JTILE) = ZRGI*(Z3S5(JL)*PBUOMTI5(JL,JTILE)) &
     & *(ZDIV55(JL,JTILE)**3)
  ENDDO
ENDDO

!*         5.    SETTING OF ROUGHNESS LENGTHS
!                ----------------------------

JTILE=1
!   - Ocean open water
DO JL=KIDIA,KFDIA
  ZDIV65(JL) = 1.0_JPRB/ZUST5(JL,JTILE)
  PZ0MTI5(JL,JTILE) = RNUM*ZDIV65(JL) + (ZRGI*PCHAR(JL))*ZUST25(JL,JTILE)
  Z0H               = RNUH*ZDIV65(JL)
  PZ0HTI5(JL,JTILE) = SQRT(Z0H*PZ0MTI5(JL,JTILE))
  Z0Q               = RNUQ*ZDIV65(JL)
  PZ0QTI5(JL,JTILE) = SQRT(Z0Q*PZ0MTI5(JL,JTILE))
ENDDO

JTILE = 2
!   - Sea ice
DO JL=KIDIA,KFDIA
  PZ0MTI5(JL,JTILE) = PZ0ICE(RZ0ICE,PFRTI(JL,JTILE))
  PZ0HTI5(JL,JTILE) = RZ0ICE
  PZ0QTI5(JL,JTILE) = RZ0ICE
ENDDO

JTILE = 3
!   - Wet skin
DO JL=KIDIA,KFDIA
  PZ0MTI5(JL,JTILE) = PCVL(JL)*RVZ0M(KTVL(JL))+PCVH(JL)*RVZ0M(KTVH(JL)) &
   & +(1.-PCVL(JL)-PCVH(JL))*RVZ0M(0)
  PZ0HTI5(JL,JTILE) = PCVL(JL)*RVZ0H(KTVL(JL))+PCVH(JL)*RVZ0H(KTVH(JL)) &
   & +(1.-PCVL(JL)-PCVH(JL))*RVZ0H(0)
  PZ0QTI5(JL,JTILE) = PZ0HTI5(JL,JTILE)
ENDDO

JTILE = 4
!   - Low Vegetation
DO JL=KIDIA,KFDIA
  PZ0MTI5(JL,JTILE) = RVZ0M(KTVL(JL))
  PZ0HTI5(JL,JTILE) = RVZ0H(KTVL(JL))
  PZ0QTI5(JL,JTILE) = PZ0HTI5(JL,JTILE)
ENDDO

JTILE = 5
!   - Exposed snow
DO JL=KIDIA,KFDIA
  ZMLOW5=PCVL(JL)/(1.0_JPRB-PCVH(JL))*RVZ0M(KTVL(JL))+(1.0_JPRB-PCVL(JL)-PCVH(JL))/(1.0_JPRB-PCVH(JL))*RVZ0M(0)
  ZHLOW5=PCVL(JL)/(1.0_JPRB-PCVH(JL))*RVZ0H(KTVL(JL))+(1.0_JPRB-PCVL(JL)-PCVH(JL))/(1.0_JPRB-PCVH(JL))*RVZ0H(0)
  PZ0MTI5(JL,JTILE)=ZSNWGHT5(JL)*RVZ0M(12)+(1.0_JPRB-ZSNWGHT5(JL))*ZMLOW5
  PZ0HTI5(JL,JTILE)=ZSNWGHT5(JL)*RVZ0H(12)+(1.0_JPRB-ZSNWGHT5(JL))*ZHLOW5
  PZ0QTI5(JL,JTILE)=PZ0HTI5(JL,JTILE)
ENDDO

JTILE = 6
!   - High vegetation
DO JL=KIDIA,KFDIA
  PZ0MTI5(JL,JTILE) = RVZ0M(KTVH(JL))
  PZ0HTI5(JL,JTILE) = RVZ0H(KTVH(JL))
  PZ0QTI5(JL,JTILE) = PZ0HTI5(JL,JTILE)
ENDDO

JTILE = 7
!   - Sheltered snow
DO JL=KIDIA,KFDIA
  PZ0MTI5(JL,JTILE) = RVZ0M(KTVH(JL))
  PZ0HTI5(JL,JTILE) = RVZ0H(KTVH(JL))
  PZ0QTI5(JL,JTILE) = PZ0HTI5(JL,JTILE)
ENDDO

JTILE = 8
!   - Bare soil
DO JL=KIDIA,KFDIA
  PZ0MTI5(JL,JTILE) = RVZ0M(0)
  PZ0HTI5(JL,JTILE) = RVZ0H(0)
  PZ0QTI5(JL,JTILE) = PZ0HTI5(JL,JTILE)
ENDDO

IF (LEFLAKE) THEN
  JTILE = 9
!   - Lake
  DO JL=KIDIA,KFDIA
    PZ0MTI5(JL,JTILE) = 1.E-4_JPRB
    PZ0HTI5(JL,JTILE) = 1.E-4_JPRB
    PZ0QTI5(JL,JTILE) = 1.E-4_JPRB
  ENDDO
ENDIF

IF (LEURBAN) THEN
  JTILE=10
!   - Urban
  DO JL=KIDIA,KFDIA
     ZURBF = 0.0_JPRB
     ZPCVL = 0.0_JPRB
     ZPCVH = 0.0_JPRB
     ZPCVB = 0.0_JPRB
     ZURBF=PCUR(JL)
     ZPCVL=MAX(PCVL(JL)*(1.0_JPRB-ZURBF),0.0_JPRB)
     ZPCVH=MAX(PCVH(JL)*(1.0_JPRB-ZURBF),0.0_JPRB)
     ZPCVB=MAX(1.0_JPRB-ZPCVL-ZPCVH-PCUR(JL),0.0_JPRB)
     IF (ZURBF.GT.0.0_JPRB) THEN
      PZ0MTI5(JL,3)=PZ0MTI5(JL,3)*(1.0_JPRB-ZURBF)+(RURBZTM*ZURBF)
      PZ0HTI5(JL,3)=PZ0HTI5(JL,3)*(1.0_JPRB-ZURBF)+(RURBZTH*ZURBF)
      PZ0QTI5(JL,3)=PZ0QTI5(JL,3)*(1.0_JPRB-ZURBF)+(RURBZTH*ZURBF)
 
      PZ0MTI5(JL,5)=PZ0MTI5(JL,5)*((ZPCVL+ZPCVB)/(1.0_JPRB-ZPCVH))+RURBZTM*(ZURBF/(1.0_JPRB-ZPCVH))
      PZ0HTI5(JL,5)=PZ0HTI5(JL,5)*((ZPCVL+ZPCVB)/(1.0_JPRB-ZPCVH))+RURBZTH*(ZURBF/(1.0_JPRB-ZPCVH))
      PZ0QTI5(JL,5)=PZ0HTI5(JL,5)
     ENDIF
   PZ0MTI5(JL,JTILE)=RURBZTM
   PZ0HTI5(JL,JTILE)=RURBZTH
   PZ0QTI5(JL,JTILE)=RURBZTH
  ENDDO
ENDIF

!*        7.   COMPUTE PRELIMINARY AERODYNAMIC RESISTANCE FOR COMPUTATION
!*             OF APPARENT SURFACE HUMIDITY IN VDFSURF

DO JTILE=1,KTILES
  DO JL=KIDIA,KFDIA
    ZNLEV5(JL,JTILE) = ZRGI*PGEOMLEV5(JL)+PZ0MTI5(JL,JTILE)
    ZDIV75(JL,JTILE) = 1.0_JPRB/PZ0QTI5(JL,JTILE)
    Z1DZ0Q5 = ZNLEV5(JL,JTILE)*ZDIV75(JL,JTILE)
    ZDIV85(JL,JTILE) = 1.0_JPRB/Z1DZ0Q5
    ZXLNQ5(JL,JTILE) = LOG(Z1DZ0Q5)
    Z4S5(JL,JTILE) = PZDLTI5(JL,JTILE)*ZDIV85(JL,JTILE)

    IF (PZDLTI5(JL,JTILE)  >  0.0_JPRB) THEN
      ZEXP15(JL,JTILE) = EXP  (-RCHBD*PZDLTI5(JL,JTILE))
      Z5S5(JL,JTILE) = SQRT(1.0_JPRB+RCHB23A*PZDLTI5(JL,JTILE))
      ZPRH15(JL,JTILE) = -RCHBB*(PZDLTI5(JL,JTILE) &
       & - RCHBCD)*ZEXP15(JL,JTILE) &
       & - (1.0_JPRB+RCHB23A*PZDLTI5(JL,JTILE))*Z5S5(JL,JTILE) &
       & - RCHBBCD+1.0_JPRB
      ZEXP25(JL,JTILE) = EXP  (-RCHBD*Z4S5(JL,JTILE))
      Z6S5(JL,JTILE) = SQRT(1.0_JPRB+RCHB23A*Z4S5(JL,JTILE))
      ZPRQ05(JL,JTILE) =  -RCHBB*(Z4S5(JL,JTILE) &
       & - RCHBCD)*ZEXP25(JL,JTILE) &
       & - (1.0_JPRB+RCHB23A*Z4S5(JL,JTILE))*Z6S5(JL,JTILE) &
       & - RCHBBCD+1.0_JPRB
    ELSE
      Z7S5(JL,JTILE) = SQRT(1.0_JPRB-RCDHALF*PZDLTI5(JL,JTILE))
      ZPRH15(JL,JTILE) = 2.0_JPRB*LOG((1.0_JPRB+Z7S5(JL,JTILE))*0.5_JPRB)
      Z8S5(JL,JTILE) = SQRT(1.0_JPRB-RCDHALF*Z4S5(JL,JTILE))
      ZPRQ05(JL,JTILE) = 2.0_JPRB*LOG((1.0_JPRB+Z8S5(JL,JTILE))*0.5_JPRB)
    ENDIF
    ZPRQ5(JL,JTILE) = ZXLNQ5(JL,JTILE)-ZPRH15(JL,JTILE)+ZPRQ05(JL,JTILE)
    ZDIV95(JL,JTILE) = 1.0_JPRB/(ZUST5(JL,JTILE)*RKAP)
    PRAQTI5(JL,JTILE) = (ZXLNQ5(JL,JTILE)-ZPRH15(JL,JTILE) &
     & + ZPRQ05(JL,JTILE))*ZDIV95(JL,JTILE)
  ENDDO
ENDDO

!     ------------------------------------------------------------------

!                 ----- ADJOINT COMPUTATION ------

!     ------------------------------------------------------------------
 
!*                             INITIALISATIONS
!                              ---------------

ZUST (:,:) = 0.0_JPRB
ZUST2(:,:) = 0.0_JPRB
ZRHO(:) = 0.0_JPRB
ZDU2(:) = 0.0_JPRB
ZDUA(:) = 0.0_JPRB

!*        7.   COMPUTE PRELIMINARY AERODYNAMIC RESISTANCE FOR COMPUTATION
!*             OF APPARENT SURFACE HUMIDITY IN VDFSURF

DO JTILE=1,KTILES
  DO JL=KIDIA,KFDIA
    Z4S = 0.0_JPRB
    Z5S = 0.0_JPRB
    Z6S = 0.0_JPRB
    Z7S = 0.0_JPRB
    Z8S = 0.0_JPRB
    ZEXP1 = 0.0_JPRB
    ZEXP2 = 0.0_JPRB
    ZNLEV = 0.0_JPRB
    Z1DZ0Q = 0.0_JPRB
    ZXLNQ = 0.0_JPRB
    ZPRH1 = 0.0_JPRB
    ZPRQ0 = 0.0_JPRB
    ZPRQ = 0.0_JPRB

    ZXLNQ = ZXLNQ+ZDIV95(JL,JTILE)*PRAQTI(JL,JTILE)
    ZPRH1 = ZPRH1-ZDIV95(JL,JTILE)*PRAQTI(JL,JTILE)
    ZPRQ0 = ZPRQ0+ZDIV95(JL,JTILE)*PRAQTI(JL,JTILE)
    ZUST(JL,JTILE)  = ZUST(JL,JTILE)-RKAP*ZDIV95(JL,JTILE)*ZDIV95(JL,JTILE) &
     & *(ZXLNQ5(JL,JTILE)-ZPRH15(JL,JTILE) &
     & +ZPRQ05(JL,JTILE))*PRAQTI(JL,JTILE)
    PRAQTI(JL,JTILE) = 0.0_JPRB
    ZXLNQ = ZXLNQ+ZPRQ
    ZPRH1 = ZPRH1-ZPRQ
    ZPRQ0 = ZPRQ0+ZPRQ
    IF (PZDLTI5(JL,JTILE)  >  0.0_JPRB) THEN
      ZEXP2 = ZEXP2-RCHBB*(Z4S5(JL,JTILE)-RCHBCD)*ZPRQ0
      Z4S = Z4S-(RCHBB*ZEXP25(JL,JTILE)+RCHB23A*Z6S5(JL,JTILE))*ZPRQ0
      Z6S = Z6S-(1.0_JPRB+RCHB23A*Z4S5(JL,JTILE))*ZPRQ0

      Z4S = Z4S+0.5_JPRB*RCHB23A*Z6S/Z6S5(JL,JTILE)
      Z4S = Z4S-RCHBD*ZEXP25(JL,JTILE)*ZEXP2

      ZEXP1 = ZEXP1-RCHBB*(PZDLTI5(JL,JTILE)-RCHBCD)*ZPRH1
      PZDLTI(JL,JTILE) = PZDLTI(JL,JTILE)-(RCHBB*ZEXP15(JL,JTILE) &
       & + RCHB23A*Z5S5(JL,JTILE))*ZPRH1
      Z5S = Z5S-(1.0_JPRB+RCHB23A*PZDLTI5(JL,JTILE))*ZPRH1
      PZDLTI(JL,JTILE) = PZDLTI(JL,JTILE)+0.5_JPRB*RCHB23A*Z5S/Z5S5(JL,JTILE)
      PZDLTI(JL,JTILE) = PZDLTI(JL,JTILE)-RCHBD*ZEXP15(JL,JTILE)*ZEXP1
    ELSE
      Z8S = Z8S+2.0_JPRB*ZPRQ0/(1.0_JPRB+Z8S5(JL,JTILE))
      Z4S = Z4S-0.5_JPRB*RCDHALF*Z8S/Z8S5(JL,JTILE)

      Z7S = Z7S+2.0_JPRB*ZPRH1/(1.0_JPRB+Z7S5(JL,JTILE))
      PZDLTI(JL,JTILE) = PZDLTI(JL,JTILE) &
       & -0.5_JPRB*RCDHALF*Z7S/Z7S5(JL,JTILE)
    ENDIF
    PZDLTI(JL,JTILE) = PZDLTI(JL,JTILE)+ZDIV85(JL,JTILE)*Z4S
    Z1DZ0Q = Z1DZ0Q-ZDIV85(JL,JTILE)*ZDIV85(JL,JTILE)*PZDLTI5(JL,JTILE)*Z4S
    Z1DZ0Q = Z1DZ0Q+ZDIV85(JL,JTILE)*ZXLNQ

    ZNLEV = ZNLEV+ZDIV75(JL,JTILE)*Z1DZ0Q
    PZ0QTI(JL,JTILE) = PZ0QTI(JL,JTILE) &
     & - ZNLEV5(JL,JTILE)*ZDIV75(JL,JTILE)*ZDIV75(JL,JTILE)*Z1DZ0Q

    PGEOMLEV(JL) = PGEOMLEV(JL)+ZRGI*ZNLEV
    PZ0MTI(JL,JTILE) = PZ0MTI(JL,JTILE)+ZNLEV
  ENDDO
ENDDO

!*         5.    SETTING OF ROUGHNESS LENGTHS
!                ----------------------------

IF (LEURBAN) THEN
  JTILE = 10
!   - Urban
  DO JL=KIDIA,KFDIA
    PZ0QTI(JL,JTILE) = 0.0_JPRB
    PZ0HTI(JL,JTILE) = 0.0_JPRB
    PZ0MTI(JL,JTILE) = 0.0_JPRB
  ENDDO
ENDIF

IF (LEFLAKE) THEN
  JTILE = 9
!   - Lake
  DO JL=KIDIA,KFDIA  
    PZ0QTI(JL,JTILE) = 0.0_JPRB
    PZ0HTI(JL,JTILE) = 0.0_JPRB
    PZ0MTI(JL,JTILE) = 0.0_JPRB
  ENDDO
ENDIF

JTILE = 8
!   - Bare soil
DO JL=KIDIA,KFDIA
  PZ0HTI(JL,JTILE) = PZ0HTI(JL,JTILE)+PZ0QTI(JL,JTILE)
  PZ0QTI(JL,JTILE) = 0.0_JPRB
  PZ0HTI(JL,JTILE) = 0.0_JPRB
  PZ0MTI(JL,JTILE) = 0.0_JPRB
ENDDO

JTILE = 7
!   - Sheltered snow
DO JL=KIDIA,KFDIA
  PZ0HTI(JL,JTILE) = PZ0HTI(JL,JTILE)+PZ0QTI(JL,JTILE)
  PZ0QTI(JL,JTILE) = 0.0_JPRB
  PZ0HTI(JL,JTILE) = 0.0_JPRB
  PZ0MTI(JL,JTILE) = 0.0_JPRB
ENDDO

JTILE = 6
!   - High vegetation
DO JL=KIDIA,KFDIA
  PZ0HTI(JL,JTILE) = PZ0HTI(JL,JTILE)+PZ0QTI(JL,JTILE)
  PZ0QTI(JL,JTILE) = 0.0_JPRB
  PZ0HTI(JL,JTILE) = 0.0_JPRB
  PZ0MTI(JL,JTILE) = 0.0_JPRB
ENDDO

JTILE = 5
!   - Exposed snow
DO JL=KIDIA,KFDIA
  PZ0HTI(JL,JTILE) = PZ0HTI(JL,JTILE)+PZ0QTI(JL,JTILE)
  PZ0QTI(JL,JTILE) = 0.0_JPRB
  PZ0HTI(JL,JTILE) = 0.0_JPRB
  PZ0MTI(JL,JTILE) = 0.0_JPRB
ENDDO

JTILE = 4
!   - Low Vegetation
DO JL=KIDIA,KFDIA
  PZ0HTI(JL,JTILE) = PZ0HTI(JL,JTILE)+PZ0QTI(JL,JTILE)
  PZ0QTI(JL,JTILE) = 0.0_JPRB
  PZ0HTI(JL,JTILE) = 0.0_JPRB
  PZ0MTI(JL,JTILE) = 0.0_JPRB
ENDDO

JTILE = 3
!   - Wet skin
DO JL=KIDIA,KFDIA
  PZ0HTI(JL,JTILE) = PZ0HTI(JL,JTILE)+PZ0QTI(JL,JTILE)
  PZ0QTI(JL,JTILE) = 0.0_JPRB
  PZ0HTI(JL,JTILE) = 0.0_JPRB
  PZ0MTI(JL,JTILE) = 0.0_JPRB
ENDDO

JTILE = 2
!   - Sea ice
DO JL=KIDIA,KFDIA
  PZ0QTI(JL,JTILE) = 0.0_JPRB
  PZ0HTI(JL,JTILE) = 0.0_JPRB
  PZ0MTI(JL,JTILE) = 0.0_JPRB
ENDDO

JTILE=1
!   - Ocean open water
DO JL=KIDIA,KFDIA
  Z0Q = RNUQ*ZDIV65(JL)
  ZUST(JL,JTILE) = ZUST(JL,JTILE) - 0.5_JPRB * Z0Q * ZDIV65(JL) * PZ0MTI5(JL,JTILE) &
               & * PZ0QTI(JL,JTILE) / PZ0QTI5(JL,JTILE)
  PZ0MTI(JL,JTILE) = PZ0MTI(JL,JTILE) + 0.5_JPRB * Z0Q * PZ0QTI(JL,JTILE) / PZ0QTI5(JL,JTILE)
  PZ0QTI(JL,JTILE) = 0.0_JPRB
  
  Z0H = RNUH*ZDIV65(JL)
  ZUST(JL,JTILE) = ZUST(JL,JTILE) - 0.5_JPRB * Z0H * ZDIV65(JL) * PZ0MTI5(JL,JTILE) &
               & * PZ0HTI(JL,JTILE) / PZ0HTI5(JL,JTILE)
  PZ0MTI(JL,JTILE) = PZ0MTI(JL,JTILE) + 0.5_JPRB * Z0H * PZ0HTI(JL,JTILE) / PZ0HTI5(JL,JTILE)
  PZ0HTI(JL,JTILE) = 0.0_JPRB

  ZUST(JL,JTILE) = ZUST(JL,JTILE) &
   & - RNUM*ZDIV65(JL)*ZDIV65(JL)*PZ0MTI(JL,JTILE)
  ZUST2(JL,JTILE) = ZUST2(JL,JTILE)+(ZRGI*PCHAR(JL))*PZ0MTI(JL,JTILE)
  PZ0MTI(JL,JTILE) = 0.0_JPRB
ENDDO

!*         4.      STABILITY PARAMETERS AND FREE CONVECTION
!                  VELOCITY SCALE

DO JTILE=1,KTILES
  DO JL=KIDIA,KFDIA
    ZROWQ = 0.0_JPRB
    ZROWT = 0.0_JPRB
    ZUST4 = 0.0_JPRB
    ZWST2 = 0.0_JPRB
    Z1S = 0.0_JPRB
    Z3S = 0.0_JPRB

    Z3S = Z3S+ZRGI*PBUOMTI5(JL,JTILE)*(ZDIV55(JL,JTILE)**3)*PZDLTI(JL,JTILE)
    PBUOMTI(JL,JTILE) = PBUOMTI(JL,JTILE) &
     & + ZRGI*Z3S5(JL)*(ZDIV55(JL,JTILE)**3)*PZDLTI(JL,JTILE)
    ZUST(JL,JTILE) = ZUST(JL,JTILE) &
     & -3._JPRB*ZRGI*(Z3S5(JL)*PBUOMTI5(JL,JTILE)) &
     & * (ZDIV55(JL,JTILE)**4)*PZDLTI(JL,JTILE)
    PZDLTI(JL,JTILE) = 0.0_JPRB
    PGEOMLEV(JL) = PGEOMLEV(JL)-RKAP*Z3S
    IF (Z2S5(JL,JTILE) >= REPUST) THEN
      ZUST2(JL,JTILE) = ZUST2(JL,JTILE) &
       & + 0.5_JPRB*ZUST(JL,JTILE)/Z2S5(JL,JTILE)
    ENDIF
    ZUST(JL,JTILE) = 0.0_JPRB

!     APPLY W* CORRECTION

    IF (PBUOMTI5(JL,JTILE) > 0.0_JPRB) THEN
      ZWST2 = ZWST2+ZCDFC*ZUST2 (JL,JTILE)
      PBUOMTI(JL,JTILE) = PBUOMTI(JL,JTILE) &
       & + ZCON2*ZIPBL*(ZIPBL*PBUOMTI5(JL,JTILE))**(ZCON2-1.0_JPRB)*ZWST2
    ENDIF

    Z1S = Z1S+ZRHO5(JL)*ZDIV45(JL)*ZDIV45(JL)*ZUST2(JL,JTILE)
    ZRHO(JL) = ZRHO(JL)-Z1S5(JL,JTILE)*ZDIV45(JL)*ZDIV45(JL)*ZUST2(JL,JTILE)
    ZUST2(JL,JTILE) = 0.0_JPRB
    ZUST4 = ZUST4+0.5_JPRB*Z1S/Z1S5(JL,JTILE)
    PUSTRTI(JL,JTILE) = PUSTRTI(JL,JTILE)+2*PUSTRTI5(JL,JTILE)*ZUST4
    PVSTRTI(JL,JTILE) = PVSTRTI(JL,JTILE)+2*PVSTRTI5(JL,JTILE)*ZUST4

    ZROWQ = ZROWQ-RG*ZDIV25(JL,JTILE)*RETV*PTSKTI5(JL,JTILE) &
     & * PBUOMTI(JL,JTILE)
    ZROWT = ZROWT-RG*ZDIV25(JL,JTILE)*PBUOMTI(JL,JTILE)
    ZRHO(JL) = ZRHO(JL) &
     & + RG*ZDIV25(JL,JTILE)*ZDIV25(JL,JTILE)*PTSKTI5(JL,JTILE) &
     & * (RETV*ZROWQ5(JL,JTILE)*PTSKTI5(JL,JTILE) &
     & + ZROWT5(JL,JTILE))*PBUOMTI(JL,JTILE)
    PTSKTI(JL,JTILE) = PTSKTI(JL,JTILE)+RG*ZDIV25(JL,JTILE) &
     & * ZDIV25(JL,JTILE)*ZROWT5(JL,JTILE)*ZRHO5(JL)*PBUOMTI(JL,JTILE)
    PBUOMTI(JL,JTILE) = 0.0_JPRB
    PAHFSTI(JL,JTILE) = PAHFSTI(JL,JTILE)+ZRCPDI*ZROWT
    PEVAPTI(JL,JTILE) = PEVAPTI(JL,JTILE)+ZROWQ
  ENDDO
ENDDO

!*         3.   ESTIMATE SURF.FL. FOR STEP 0
!*              (ASSUME NEUTRAL STRATIFICATION)

IF (LLINIT) THEN
  DO JTILE=1,KTILES
    DO JL=KIDIA,KFDIA
      ZAS = 0.0_JPRB
      ZLOG1 = 0.0_JPRB
      ZZCDN = 0.0_JPRB

      PEVAPTI(JL,JTILE) = 0.0_JPRB
      PAHFSTI(JL,JTILE) = 0.0_JPRB
      ZRHO(JL) = ZRHO(JL) &
       & + PVSTRTI(JL,JTILE)*PVMLEV5(JL)*ZDUA5(JL)*ZZCDN5(JL,JTILE)
      PVMLEV(JL) = PVMLEV(JL) &
       & + PVSTRTI(JL,JTILE)*ZRHO5(JL)*ZDUA5(JL)*ZZCDN5(JL,JTILE)
      ZDUA(JL) = ZDUA(JL) &
       & + PVSTRTI(JL,JTILE)*ZRHO5(JL)*PVMLEV5(JL)*ZZCDN5(JL,JTILE)
      ZZCDN = ZZCDN + PVSTRTI(JL,JTILE)*ZRHO5(JL)*PVMLEV5(JL)*ZDUA5(JL)
      PVSTRTI (JL,JTILE) = 0.0_JPRB
      ZRHO(JL) = ZRHO(JL) &
       & + PUSTRTI(JL,JTILE)*PUMLEV5(JL)*ZDUA5(JL)*ZZCDN5(JL,JTILE)
      PUMLEV(JL) = PUMLEV(JL) &
       & + PUSTRTI(JL,JTILE)*ZRHO5(JL)*ZDUA5(JL)*ZZCDN5(JL,JTILE)
      ZDUA(JL) = ZDUA(JL) &
       & + PUSTRTI(JL,JTILE)*ZRHO5(JL)*PUMLEV5(JL)*ZZCDN5(JL,JTILE)
      ZZCDN = ZZCDN + PUSTRTI(JL,JTILE)*ZRHO5(JL)*PUMLEV5(JL)*ZDUA5(JL)
      PUSTRTI (JL,JTILE) = 0.0_JPRB
      ZLOG1 = ZLOG1-2.0_JPRB*RKAP**2*ZZCDN/ZLOG15(JL,JTILE)**3
      ZAS = ZAS+ZLOG1/ZAS5(JL,JTILE)
      PGEOMLEV(JL) = PGEOMLEV(JL) &
       & + ZAS*ZZ0MTI5(JL,JTILE)/(RG*ZZ0MTI5(JL,JTILE)**2)
      PZ0MTI(JL,JTILE) = PZ0MTI(JL,JTILE) &
       & - ZAS*PGEOMLEV5(JL)/(RG*ZZ0MTI5(JL,JTILe)**2)
    ENDDO
  ENDDO

  DO JL=KIDIA,KFDIA
    IF (LEURBAN) THEN
!       - Urban
      PZ0MTI(JL,10) = 0.0_JPRB
    ENDIF
    IF (LEFLAKE) THEN
!       - Preliminatry value for lake
      PZ0MTI(JL,9) = 0.0_JPRB
    ENDIF
!       - Bare soil
    PZ0MTI(JL,8) = 0.0_JPRB
!       - Sheltered snow
    PZ0MTI(JL,7) = 0.0_JPRB
!       - High vegetation
    PZ0MTI(JL,6) = 0.0_JPRB
!       - Exposed snow
    PZ0MTI(JL,5) = 0.0_JPRB
!       - Low Vegetation
    PZ0MTI(JL,4) = 0.0_JPRB
!       - Wet skin
    PZ0MTI(JL,3) = 0.0_JPRB
!       - Sea ice
    PZ0MTI(JL,2) = 0.0_JPRB
!       - Preliminatry value for water
    PZ0MTI(JL,1) = 0.0_JPRB

    ZDU2(JL) = ZDU2(JL)+ZDUA(JL)/(2.0_JPRB*SQRT(ZDU25(JL)))
    ZDUA(JL) = 0.0_JPRB
  ENDDO
ENDIF

!*         2.      STABILITY PARAMETERS AND UPDATING OF SEA SURFACE
!                  ROUGHNESS LENGTHS

DO JL = KIDIA, KFDIA
  Z0S = 0.0_JPRB

  IF (REPDU2 >= Z0S5(JL)) THEN
    ZDU2(JL) = 0.0_JPRB
  ELSE
    Z0S = Z0S+ZDU2(JL)
    ZDU2(JL) = 0.0_JPRB
  ENDIF
  PUMLEV(JL) = PUMLEV(JL)+2.0_JPRB*PUMLEV5(JL)*Z0S
  PVMLEV(JL) = PVMLEV(JL)+2.0_JPRB*PVMLEV5(JL)*Z0S

  PAPHMS(JL) = PAPHMS(JL)+ZDIV15(JL)*ZRHO(JL)
  PTMLEV(JL) = PTMLEV(JL)-RD*PAPHMS5(JL)*ZDIV15(JL)*ZDIV15(JL) &
   & * (1.0_JPRB+RETV*PQMLEV5(JL))*ZRHO(JL)
  PQMLEV(JL) = PQMLEV(JL)-RD*PAPHMS5(JL)*ZDIV15(JL)*ZDIV15(JL) &
   & *RETV*PTMLEV5(JL)*ZRHO(JL)
  ZRHO(JL) = 0.0_JPRB
ENDDO
 
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('VUPDZ0SAD_MOD:VUPDZ0SAD',1,ZHOOK_HANDLE)
END SUBROUTINE VUPDZ0SAD

! Function to compute Z0M for neutral wind conditions
#include "fcz0wn.h"

END MODULE VUPDZ0SAD_MOD
