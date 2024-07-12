MODULE SURFPPSTL_CTL_MOD
CONTAINS
SUBROUTINE SURFPPSTL_CTL( KIDIA,KFDIA,KLON,KTILES,LDPPCFLS &
 & , PTSTEP &
! input - trajectory
 & , PFRTI, PTSKTIP15, PAHFLTI5, PG0TI5   , PSTRTULEV5, PSTRTVLEV5, PTSKM1M5 &
 & , PUMLEV5, PVMLEV5, PQMLEV5 , PGEOMLEV5, PCPTSPP5  , PCPTGZLEV5 &
 & , PAPHMS5, PZ0MW5 , PZ0HW5  , PZ0QW5   , PQSAPP5   , PBUOM5 &
 & , YDCST  , YDEXC &
! input, tile dependent
 & , PZ0MTIW5, PZ0HTIW5, PZ0QTIW5, PQSAPPTI5, PCPTSPPTI5, PBUOMTI5 &
! updated - trajectory
 & , PAHFSTI5, PEVAPTI5, PTSKE15 &
! output - trajectory
 & , PDIFTSLEV5, PDIFTQLEV5, PUSTRTI5, PVSTRTI5, PTSKTI5 &
 & , PU10M5    , PV10M5    , PT2M5   , PD2M5   , PQ2M5 &
 & , P10NU5    , P10NV5    &
! input
 & , PTSKTIP1, PAHFLTI, PG0TI , PSTRTULEV, PSTRTVLEV, PTSKM1M &
 & , PUMLEV  , PVMLEV , PQMLEV, PGEOMLEV , PCPTSPP  , PCPTGZLEV &
 & , PAPHMS  , PZ0MW  , PZ0HW , PZ0QW    , PQSAPP   , PBUOM &
! input, tile dependent
 & , PZ0MTIW, PZ0HTIW, PZ0QTIW, PQSAPPTI, PCPTSPPTI, PBUOMTI &
! updated
 & , PAHFSTI , PEVAPTI, PTSKE1 &
! output
 & , PDIFTSLEV, PDIFTQLEV, PUSTRTI, PVSTRTI, PTSKTI &
 & , PU10M    , PV10M    , PT2M   , PD2M   , PQ2M &
 & , P10NU    , P10NV    &
 & )

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_CST  , ONLY : TCST
USE YOS_EXC  , ONLY : TEXC

USE SPPCFLSTL_MOD

! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!------------------------------------------------------------------------

!  PURPOSE:
!    Routine SURFPPSTL controls the computation of quantities at the end of
!    vertical diffusion, includting routines to post-process weather elements
!    and gustiness.
!    (Tangent linear)

!  SURFPPSTL is called by VDFMAINSTL

!  METHOD:
!    This routine is a shell needed by the surface library  externalisation.

!  AUTHOR:
!    M. Janiskova       ECMWF June 2005

!  REVISION HISTORY:


!  INTERFACE: 

!    Integers (In):
!      KIDIA    :    Begin point in arrays
!      KFDIA    :    End point in arrays
!      KLON     :    Length of arrays
!      KTILES   :    Number of files


!    Reals (In):
!      PTSTEP    :  Timestep                                          s

!*      Reals (In):
!  Trajectory  Perturbation  Description                               Unit
!  PFRTI       ---           TILE FRACTIONS                            (0-1)
!                            1: WATER         5: SNOW ON LOW-VEG+BARE-SOIL
!                            2: ICE           6: DRY SNOW-FREE HIGH-VEG
!                            3: WET SKIN      7: SNOW UNDER HIGH-VEG
!                            4: DRY SNOW-FREE 8: BARE SOIL
!                               LOW-VEG
!  PTSKTIP15   PTSKTIP1      Tile skin temperature, t+1                K
!  PAHFLTI5    PAHFLTI       Surface latent heat flux                  Wm-2
!  PG0TI5      PG0TI         Surface ground heat flux                  W/m2
!  PSTRTULEV5  PSTRTULEV     TURBULENT FLUX OF U-MOMEMTUM             kg/(m*s2)
!  PSTRTVLEV5  PSTRTVLEV     TURBULENT FLUX OF V-MOMEMTUM             kg/(m*s2)
!  PTSKM1M5    PTSKM1M       Skin temperature, t                       K
!  PUMLEV5     PUMLEV        X-VELOCITY COMPONENT,                     m/s
!                            lowest atmospheric level
!  PVMLEV5     PVMLEV        Y-VELOCITY COMPONENT,                     m/s
!                            lowest atmospheric level
!  PQMLEV5     PQMLEV        SPECIFIC HUMIDITY                         kg/kg
!  PGEOMLEV5   PGEOMLEV      Geopotential, lowest atmospehric level    m2/s2
!  PCPTSPP5    PCPTSPP       Cp*Ts for post-processing of weather      J/kg
!                            parameters
!  PCPTGZLEV5  PCPTGZLEV     Geopotential, lowest atmospehric level    J/kg
!  PAPHMS5     PAPHMS        Surface pressure                           Pa
!  PZ0MW5      PZ0MW         Roughness length for momentum,WMO station  m
!  PZ0HW5      PZ0HW         Roughness length for heat, WMO station     m
!  PZ0QW5      PZ0QW         Roughness length for moisture,WMO station  m
!  PQSAPP5     PQSAPP        Apparent surface humidity                 kg/kg
!  PBUOM5      PBUOM         Buoyancy flux, for post-processing of     ????
!                            gustiness

!*      Reals (Updated):
!  Trajectory  Perturbation  Description                               Unit
!  PAHFSTI5    PAHFSTI       SURFACE SENSIBLE HEAT FLUX                W/m2
!  PEVAPTI5    PEVAPTI       SURFACE MOISTURE FLUX                     kg/m2/s
!  PTSKE15     PTSKE1        SKIN TEMPERATURE TENDENCY                 K/s

!*      Reals (Out):
!  Trajectory  Perturbation  Description                               Unit
!  PDIFTSLEV5  PDIFTSLEV     TURBULENT FLUX OF HEAT                    J/(m2*s)
!  PDIFTQLEV5  PDIFTQLEV     TURBULENT FLUX OF SPECIFIC HUMIDITY      kg/(m2*s)
!  PUSTRTI5    PUSTRTI       SURFACE U-STRESS                          N/m2 
!  PVSTRTI5    PVSTRTI       SURFACE V-STRESS                          N/m2
!  PTSKTI5     PTSKTI        SKIN TEMPERATURE                          K
!  PU10M5      PU10M         U-COMPONENT WIND AT 10 M                  m/s
!  PV10M5      PV10M         V-COMPONENT WIND AT 10 M                  m/s
!  P10NU5      P10NU         U-COMPONENT NEUTRAL WIND AT 10 M          m/s
!  P10NV5      P10NV         V-COMPONENT NEUTRAL WIND AT 10 M          m/s
!  PT2M5       PT2M          TEMPERATURE AT 2M                         K
!  PD2M5       PD2M          DEW POINT TEMPERATURE AT 2M               K
!  PQ2M5       PQ2M          SPECIFIC HUMIDITY AT 2M                   kg/kg

!     EXTERNALS.
!     ----------

!     ** SURFPPSTL_CTL CALLS SUCCESSIVELY:
!         *SPPCFLTL*

!  DOCUMENTATION:
!    See Physics Volume of IFS documentation

!------------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KTILES
LOGICAL           ,INTENT(IN)    :: LDPPCFLS
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSTEP
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFRTI(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSKTIP15(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAHFLTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PG0TI5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTRTULEV5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTRTVLEV5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSKM1M5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PUMLEV5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVMLEV5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQMLEV5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEOMLEV5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCPTSPP5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCPTGZLEV5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHMS5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PZ0MW5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PZ0HW5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PZ0QW5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQSAPP5(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBUOM5(:)
TYPE(TCST)        ,INTENT(IN)    :: YDCST
TYPE(TEXC)        ,INTENT(IN)    :: YDEXC
! Tile dependent pp
REAL(KIND=JPRB)   ,INTENT(IN)    :: PZ0MTIW5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PZ0HTIW5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PZ0QTIW5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQSAPPTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCPTSPPTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBUOMTI5(:,:)

REAL(KIND=JPRB)   ,INTENT(INOUT) :: PAHFSTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PEVAPTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSKE15(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDIFTSLEV5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDIFTQLEV5(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PUSTRTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PVSTRTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PTSKTI5(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PU10M5(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PV10M5(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P10NU5(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P10NV5(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PT2M5(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PD2M5(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQ2M5(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSKTIP1(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAHFLTI(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PG0TI(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTRTULEV(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSTRTVLEV(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSKM1M(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PUMLEV(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVMLEV(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQMLEV(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEOMLEV(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCPTSPP(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCPTGZLEV(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHMS(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PZ0MW(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PZ0HW(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PZ0QW(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQSAPP(:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBUOM(:)
! Tile dependent pp
REAL(KIND=JPRB)   ,INTENT(IN)    :: PZ0MTIW(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PZ0HTIW(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PZ0QTIW(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQSAPPTI(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCPTSPPTI(:,:)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PBUOMTI(:,:)

REAL(KIND=JPRB)   ,INTENT(INOUT) :: PAHFSTI(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PEVAPTI(:,:)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSKE1(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDIFTSLEV(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDIFTQLEV(:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PUSTRTI(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PVSTRTI(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PTSKTI(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PU10M(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PV10M(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P10NU(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: P10NV(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PT2M(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PD2M(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQ2M(:) 

! Local variables
LOGICAL :: LT2MTILE
LOGICAL :: LWIND

INTEGER(KIND=JPIM) :: JTILE, JL

REAL(KIND=JPRB) :: ZTSK5(KLON), ZAHFSM5(KLON), ZEVAPM5(KLON)
REAL(KIND=JPRB) :: ZTSK(KLON) , ZAHFSM(KLON) , ZEVAPM(KLON)
REAL(KIND=JPRB) :: ZZ0MW(KLON),ZZ0MW5(KLON)
REAL(KIND=JPRB) :: ZT2M5(KLON,KTILES), ZD2M5(KLON,KTILES), ZQ2M5(KLON,KTILES)
REAL(KIND=JPRB) :: ZT2M(KLON,KTILES), ZD2M(KLON,KTILES), ZQ2M(KLON,KTILES)
REAL(KIND=JPRB) :: ZT2M_DL5(KLON), ZQ2M_DL5(KLON), ZD2M_DL5(KLON)
REAL(KIND=JPRB) :: ZT2M_DL(KLON), ZQ2M_DL(KLON), ZD2M_DL(KLON)
REAL(KIND=JPRB) :: ZU10M_DUMMY5(KLON), ZV10M_DUMMY5(KLON), Z10NU_DUMMY5(KLON),&
                 & Z10NV_DUMMY5(KLON)
REAL(KIND=JPRB) :: ZU10M_DUMMY(KLON), ZV10M_DUMMY(KLON), Z10NU_DUMMY(KLON),&
                 & Z10NV_DUMMY(KLON)


REAL(KIND=JPRB) :: ZRTMST
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('SURFPPSTL_CTL_MOD:SURFPPSTL_CTL',0,ZHOOK_HANDLE)
ASSOCIATE(RLSTT=>YDCST%RLSTT)

ZRTMST      = 1.0_JPRB/PTSTEP    ! optimization

!*         1.     SURFACE FLUXES - TILES
!                 ----------------------

!*         1.1  SURFACE FLUXES OF HEAT AND MOISTURE FOR THE 
!*              DIFFERENT TILES AND THE MEAN OVER TILES

ZTSK  (KIDIA:KFDIA) = 0.0_JPRB
ZTSK5 (KIDIA:KFDIA) = 0.0_JPRB
ZAHFSM (KIDIA:KFDIA) = 0.0_JPRB
ZAHFSM5(KIDIA:KFDIA) = 0.0_JPRB
ZEVAPM (KIDIA:KFDIA) = 0.0_JPRB
ZEVAPM5(KIDIA:KFDIA) = 0.0_JPRB
DO JTILE=1,KTILES
  DO JL=KIDIA,KFDIA
    ZTSK (JL) = ZTSK (JL)+PFRTI(JL,JTILE)*PTSKTIP1(JL,JTILE)
    ZTSK5(JL) = ZTSK5(JL)+PFRTI(JL,JTILE)*PTSKTIP15(JL,JTILE)
    ZAHFSM (JL) = ZAHFSM (JL)+PFRTI(JL,JTILE)*PAHFSTI(JL,JTILE)
    ZAHFSM5(JL) = ZAHFSM5(JL)+PFRTI(JL,JTILE)*PAHFSTI5(JL,JTILE)
    ZEVAPM (JL) = ZEVAPM (JL)+PFRTI(JL,JTILE)*PEVAPTI(JL,JTILE)
    ZEVAPM5(JL) = ZEVAPM5(JL)+PFRTI(JL,JTILE)*PEVAPTI5(JL,JTILE)
  ENDDO
ENDDO

PDIFTSLEV (KIDIA:KFDIA) = 0.0_JPRB
PDIFTSLEV5(KIDIA:KFDIA) = 0.0_JPRB
PDIFTQLEV (KIDIA:KFDIA) = 0.0_JPRB
PDIFTQLEV5(KIDIA:KFDIA) = 0.0_JPRB
DO JTILE=1,KTILES
  DO JL=KIDIA,KFDIA
    PDIFTSLEV (JL) = PDIFTSLEV (JL)+PFRTI(JL,JTILE)*PAHFSTI(JL,JTILE)
    PDIFTSLEV5(JL) = PDIFTSLEV5(JL)+PFRTI(JL,JTILE)*PAHFSTI5(JL,JTILE)
    PDIFTQLEV (JL) = PDIFTQLEV (JL)+PFRTI(JL,JTILE)*PEVAPTI(JL,JTILE)
    PDIFTQLEV5(JL) = PDIFTQLEV5(JL)+PFRTI(JL,JTILE)*PEVAPTI5(JL,JTILE)

    PUSTRTI (JL,JTILE) = PSTRTULEV (JL)
    PUSTRTI5(JL,JTILE) = PSTRTULEV5(JL)
    PVSTRTI (JL,JTILE) = PSTRTVLEV (JL)
    PVSTRTI5(JL,JTILE) = PSTRTVLEV5(JL)
    IF (PFRTI(JL,JTILE) == 0._JPRB) THEN
      PAHFSTI (JL,JTILE) = ZAHFSM (JL)
      PAHFSTI5(JL,JTILE) = ZAHFSM5(JL)
      PEVAPTI (JL,JTILE) = ZEVAPM (JL)
      PEVAPTI5(JL,JTILE) = ZEVAPM5(JL)
      PTSKTI (JL,JTILE) = ZTSK (JL)
      PTSKTI5(JL,JTILE) = ZTSK5(JL)
    ELSE
      PTSKTI (JL,JTILE) = PTSKTIP1 (JL,JTILE)
      PTSKTI5(JL,JTILE) = PTSKTIP15(JL,JTILE)
    ENDIF
  ENDDO
ENDDO

!*         1.2  Skin temperature tendencies

DO JL=KIDIA,KFDIA
  PTSKE1 (JL) = PTSKE1 (JL)+(ZTSK (JL)-PTSKM1M (JL))*ZRTMST
  PTSKE15(JL) = PTSKE15(JL)+(ZTSK5(JL)-PTSKM1M5(JL))*ZRTMST
ENDDO

!      2. Post-processing of weather parameters
!         -------------------------------------

IF (LDPPCFLS) THEN
  LT2MTILE=.TRUE. 
  IF (LT2MTILE)THEN
    ! Initialise just for safety
    ZT2M(KIDIA:KFDIA,1:KTILES)=0._JPRB
    ZT2M5(KIDIA:KFDIA,1:KTILES)=0._JPRB
    ZD2M(KIDIA:KFDIA,1:KTILES)=0._JPRB
    ZD2M5(KIDIA:KFDIA,1:KTILES)=0._JPRB
    ZQ2M(KIDIA:KFDIA,1:KTILES)=0._JPRB
    ZQ2M5(KIDIA:KFDIA,1:KTILES)=0._JPRB
    ! Dummy fields for second call to sppcfl for wind calculation
    ZT2M_DL(KIDIA:KFDIA)=0._JPRB
    ZT2M_DL5(KIDIA:KFDIA)=0._JPRB
    ZD2M_DL(KIDIA:KFDIA)=0._JPRB
    ZD2M_DL5(KIDIA:KFDIA)=0._JPRB
    ZQ2M_DL(KIDIA:KFDIA)=0._JPRB
    ZQ2M_DL5(KIDIA:KFDIA)=0._JPRB
    ! Dummy fields for first call to sppcfl for winds
    ZU10M_DUMMY(KIDIA:KFDIA)=0._JPRB
    ZU10M_DUMMY5(KIDIA:KFDIA)=0._JPRB
    ZV10M_DUMMY(KIDIA:KFDIA)=0._JPRB
    ZV10M_DUMMY5(KIDIA:KFDIA)=0._JPRB
    Z10NU_DUMMY(KIDIA:KFDIA)=0._JPRB
    Z10NU_DUMMY5(KIDIA:KFDIA)=0._JPRB
    Z10NV_DUMMY(KIDIA:KFDIA)=0._JPRB
    Z10NV_DUMMY5(KIDIA:KFDIA)=0._JPRB

    LWIND=.FALSE.
    DO JTILE=1,KTILES
      CALL SPPCFLSTL(KIDIA,KFDIA,KLON &
      & , PUMLEV5 , PVMLEV5, PQMLEV5, PAPHMS5, PGEOMLEV5, PCPTGZLEV5  &
      & , PCPTSPPTI5(KIDIA:KFDIA,JTILE), PQSAPPTI5(KIDIA:KFDIA,JTILE) &
      & , PZ0MTIW5(KIDIA:KFDIA,JTILE)  , PZ0HTIW5(KIDIA:KFDIA,JTILE)  , PZ0QTIW5(KIDIA:KFDIA,JTILE)   &
      & , PBUOMTI5(KIDIA:KFDIA,JTILE) &
      & , YDCST   , YDEXC &
      & , ZU10M_DUMMY5  ,  ZV10M_DUMMY5, Z10NU_DUMMY5,  Z10NV_DUMMY5  &
      & , ZT2M5(KIDIA:KFDIA,JTILE)  , ZD2M5(KIDIA:KFDIA,JTILE)  , ZQ2M5(KIDIA:KFDIA,JTILE) &
      & , PUMLEV  , PVMLEV , PQMLEV , PAPHMS , PGEOMLEV , PCPTGZLEV &
      & , PCPTSPPTI(KIDIA:KFDIA,JTILE), PQSAPPTI(KIDIA:KFDIA,JTILE) &
      & , PZ0MTIW(KIDIA:KFDIA,JTILE) , PZ0HTIW(KIDIA:KFDIA,JTILE)  , PZ0QTIW(KIDIA:KFDIA,JTILE)   &
      & , PBUOMTI(KIDIA:KFDIA,JTILE) &
      & , ZU10M_DUMMY, ZV10M_DUMMY, Z10NU_DUMMY, Z10NV_DUMMY &
      & , ZT2M(KIDIA:KFDIA,JTILE), ZD2M(KIDIA:KFDIA,JTILE), ZQ2M(KIDIA:KFDIA,JTILE))
    ENDDO
    ! Compute postprocessed wind over dominant low
    LWIND=.TRUE.
    DO JL=KIDIA,KFDIA
    IF (PZ0MW5(JL)>=PZ0HW5(JL))THEN
       ZZ0MW(JL)=PZ0MW(JL)
       ZZ0MW5(JL)=PZ0MW5(JL)
    ELSE
       ZZ0MW(JL)=PZ0HW(JL)
       ZZ0MW5(JL)=PZ0HW5(JL)
    ENDIF
    ENDDO
    CALL SPPCFLSTL(KIDIA,KFDIA,KLON &
      & , PUMLEV5 , PVMLEV5, PQMLEV5, PAPHMS5, PGEOMLEV5, PCPTGZLEV5 &
      & , PCPTSPP5, PQSAPP5, ZZ0MW5 , PZ0HW5 , PZ0QW5   , PBUOM5 &
      & , YDCST   , YDEXC &
      & , PU10M5  , PV10M5 , P10NU5 , P10NV5 ,ZT2M_DL5  , ZD2M_DL5  , ZQ2M_DL5 &
      & , PUMLEV  , PVMLEV , PQMLEV , PAPHMS , PGEOMLEV , PCPTGZLEV &
      & , PCPTSPP , PQSAPP , ZZ0MW  , PZ0HW  , PZ0QW    , PBUOM &
      & , PU10M   , PV10M  , P10NU  , P10NV  ,ZT2M_DL   , ZD2M_DL   , ZQ2M_DL )

    PT2M(KIDIA:KFDIA)=0._JPRB
    PT2M5(KIDIA:KFDIA)=0._JPRB
    PD2M(KIDIA:KFDIA)=0._JPRB
    PD2M5(KIDIA:KFDIA)=0._JPRB
    PQ2M(KIDIA:KFDIA)=0._JPRB
    PQ2M5(KIDIA:KFDIA)=0._JPRB
    DO JTILE=1,KTILES
      DO JL=KIDIA,KFDIA
        PT2M(JL)=PT2M(JL)+&
                         &PFRTI(JL,JTILE)*ZT2M(JL,JTILE)
        PT2M5(JL)=PT2M5(JL)+&
                         &PFRTI(JL,JTILE)*ZT2M5(JL,JTILE)
        PD2M(JL)=PD2M(JL)+&
                         &PFRTI(JL,JTILE)*ZD2M(JL,JTILE)
        PD2M5(JL)=PD2M5(JL)+&
                         &PFRTI(JL,JTILE)*ZD2M5(JL,JTILE)
        PQ2M(JL)=PQ2M(JL)+&
                         &PFRTI(JL,JTILE)*ZQ2M(JL,JTILE)
        PQ2M5(JL)=PQ2M5(JL)+&
                         &PFRTI(JL,JTILE)*ZQ2M5(JL,JTILE)
      ENDDO
    ENDDO

  ELSE ! Business as usual
    DO JL=KIDIA,KFDIA
    IF (PZ0MW5(JL)>=PZ0HW5(JL))THEN
       ZZ0MW(JL)=PZ0MW(JL)
       ZZ0MW5(JL)=PZ0MW5(JL)
    ELSE
       ZZ0MW(JL)=PZ0HW(JL)
       ZZ0MW5(JL)=PZ0HW5(JL)
    ENDIF
    ENDDO

    CALL SPPCFLSTL(KIDIA,KFDIA,KLON &
     & , PUMLEV5 , PVMLEV5, PQMLEV5, PAPHMS5, PGEOMLEV5, PCPTGZLEV5 &
     & , PCPTSPP5, PQSAPP5, ZZ0MW5 , PZ0HW5 , PZ0QW5   , PBUOM5 &
     & , YDCST   , YDEXC &
     & , PU10M5  , PV10M5 , P10NU5 , P10NV5 ,PT2M5  , PD2M5  , PQ2M5 &
     & , PUMLEV  , PVMLEV , PQMLEV , PAPHMS , PGEOMLEV , PCPTGZLEV &
     & , PCPTSPP , PQSAPP , ZZ0MW  , PZ0HW  , PZ0QW    , PBUOM &
     & , PU10M   , PV10M  , P10NU  , P10NV  ,PT2M   , PD2M   , PQ2M )
  ENDIF
ENDIF

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SURFPPSTL_CTL_MOD:SURFPPSTL_CTL',1,ZHOOK_HANDLE)

END SUBROUTINE SURFPPSTL_CTL
END MODULE SURFPPSTL_CTL_MOD
