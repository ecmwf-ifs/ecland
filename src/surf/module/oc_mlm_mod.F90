MODULE OC_MLM_MOD

CONTAINS

SUBROUTINE OC_MLM(KIDIA,KFDIA,KLON,KLEVO,DELT,NSTEP,RT,Q_0,FC,&
 &                PHI_W,HS,Z0,PPHIOC,WSTAR,&
 &                PUSTOKES ,PVSTOKES ,&
 &                XKS,U_S,V_S,T_S,&
 &                POTKE,U,V,T,YDMLM,YDCST)

! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!
!***  DETERMINES EVOLUTION OF U, V AND T USING A MIXED LAYER MODEL 
!     FOR THE UPPER OCEAN
!
!
!     AUTHOR: PETER A.E.M. JANSSEN, SEPTEMBER 2009
!     ------
!
!
!     PURPOSE.
!     --------
!
!         THIS IS A SINGLE COLUMN MODEL FOR THE UPPER OCEAN. IT SOLVES
!         THE AGEOSTROPHIC MOMENTUM AND HEAT EQUATIONS WHERE THE TURBULENT
!         EXCHANGE COEFFICIENTS FOLLOW FROM THE TURBULENT KINETIC ENERGY
!         (TKE) EQUATION.             
!         THE TKE EQUATION IS SOLVED FORWARD IN TIME WITH AN IMPLICIT SCHEME.
!         THE SOURCES INVOLVE TURBULENT DISSIPATION, SHEAR
!         PRODUCTION, BUOYANCY, LANGMUIR TURBULENT PRODUCTION AND 
!         WAVE BREAKING.   
!
!**   INTERFACE.
!     ----------
!
!         CALL OC_MLM(KIDIA,KFDIA,KLON,KLEVO,DELT,NSTEP,RT,Q_0,FC,PHI_W,
!    V                HS,Z0,PPHIOC,WSTAR,
!    V                PUSTOKES,PVSTOKES,XKS,DML,U_S,V_S,
!    V                T_S,POTKE,U,V,T)
!         
!         INPUT:
!         -----
!
!         *KIDIA*    INTEGER       START INDEX
!         *KFDIA*    INTEGER       LAST INDEX
!         *KLON*     INTEGER       NUMBER OF GRID POINTS PER PACKET
!         *KLEVO*    INTEGER       NUMBER OF LAYERS
!         *DELT*     REAL          TIMESTEP
!         *NSTEP*    INTEGER       NUMBER OF STEPS IN TIME INTERVAL DELTAU
!         *RT*       REAL          RADIATION
!         *Q_0*      REAL          TOTAL HEAT FLUX
!         *FC*       REAL          CORIOLIS PARAMETER
!         *U10*      REAL          WINDSPEED
!         *PHI_W*    REAL          WIND DIRECTION
!         *HS*       REAL          SIGNIFICANT WAVE HEIGHT
!         *Z0*       REAL          GRADIENT SCALE WAVE-INDUCED STRESS
!         *PPHIOC*   REAL          ENERGY FLUX INTO THE OCEAN
!         *WSTAR*    REAL          FRICTION VELOCITY IN WATER
!         *PUSTOKES* REAL          SURFACE STOKES VELOCITY X-DIRECTION
!         *PVSTOKES* REAL          SURFACE STOKES VELOCITY Y-DIRECTION
!         *XKS*      REAL          INVERSE OF DECAY LENGTH SCALE STOKES DRIFT
!         *U_S*      REAL          U COMPONENT VELOCITY AT DEPTH DML
!         *V_S*      REAL          V COMPONENT VELOCITY AT DEPTH DML
!         *T_S*      REAL          TEMPERATURE AT DEPTH DML !!! in degree C !!!
!         *POTKE*    REAL          TURBULENT KINETIC ENERGY
!         *U*        REAL          U COMPONENT VELOCITY PROFILE
!         *V*        REAL          V COMPONENT VELOCITY PROFILE
!         *T*        REAL          TEMPERATURE PROFILE  !!! in degree C
!         
!         OUTPUT:
!         ------
!
!         *POTKE*    REAL          updated TURBULENT KINETIC ENERGY
!         *T*        REAL          SURFACE TEMPERATURE
!         *U*        REAL          U-COMPONENT SURFACE CURRENT
!         *V*        REAL          V-COMPONENT SURFACE CURRENT
!
!     METHOD.
!     -------
!
!
!         SOLVE SIMULTANEOUSLY THE TKE EQUATION WHICH IS OF THE FORM
!
!            DE/DT = D/DZ(D_E DE/DZ) + ALPHA*Q -Q^3/BL(Z)+ S_{WAVES}
!
!         TOGETHER WITH THE HEAT EQUATION AND THE TWO-DIMENSIONAL MOMENTUM
!
!         EQUATION, USING A SEMI-IMPLICIT SCHEME
!
!
!     EXTERNALS.
!     ----------
!
!         NO EXTERNALS.
!
!     REFERENCE.
!     ----------
!      
!         MIXED LAYER MODELLING, WAVE BREAKING AND THE GENERATION OF 
!         LANGMUIR CIRCULATION  BY P.A.E.M. JANSSEN, 7 SEPTEMBER 2009.
!
!
!     HEALTH WARNING
!     --------------
!
!         CODE IS WRITTEN ASSUMING DEPTH Z IS POSITIVE (Z => -Z)		
!
!----------------------------------------------------------------------
  
USE PARKIND1, ONLY : JPIM, JPRB
USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_MLM,  ONLY : TMLM
USE YOS_CST,  ONLY : TCST

USE SOURCE_E_MOD
USE TRIDAG_MOD

IMPLICIT NONE

INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA, KFDIA, KLON, KLEVO, NSTEP

REAL(KIND=JPRB), INTENT(IN) :: DELT

REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: RT
REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: Q_0 
REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: FC 
REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: PHI_W
REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: HS
REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: Z0
REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: PPHIOC 
REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: WSTAR
REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: PUSTOKES 
REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: PVSTOKES 
REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: XKS
REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: U_S
REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: V_S
REAL(KIND=JPRB), DIMENSION(KLON), INTENT(IN) :: T_S

REAL(KIND=JPRB), DIMENSION(KLON,KLEVO+1), INTENT(INOUT) :: POTKE ! TURBULENT KINETIC ENERGY
REAL(KIND=JPRB), DIMENSION(KLON,KLEVO+1), INTENT(INOUT) :: U
REAL(KIND=JPRB), DIMENSION(KLON,KLEVO+1), INTENT(INOUT) :: V
REAL(KIND=JPRB), DIMENSION(KLON,KLEVO+1), INTENT(INOUT) :: T

TYPE(TMLM), INTENT(IN) :: YDMLM
TYPE(TCST), INTENT(IN) :: YDCST


INTEGER(KIND=JPIM) :: JL, I, ITIME, IMIN, J

REAL(KIND=JPRB), PARAMETER :: EPS = 0.0001_JPRB
REAL(KIND=JPRB), PARAMETER :: ONE_FOURTH = 0.25_JPRB
REAL(KIND=JPRB), PARAMETER :: ONE_HALF = 0.5_JPRB
REAL(KIND=JPRB), PARAMETER :: ONE = 1._JPRB
REAL(KIND=JPRB), PARAMETER :: TWO = 2._JPRB

REAL(KIND=JPRB) :: ALP_IMP, ALP_E, ZIMP
REAL(KIND=JPRB) :: ZFAC0, ZFAC1, ZFAC2, ZFAC3, ZFAC4, ZFAC5
REAL(KIND=JPRB) :: DUDZ, DVDZ, PHI, SIG1, SIG2
REAL(KIND=JPRB) :: XINCR_D, XINCR_H, FCD, ABS_XINCR_H
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

REAL(KIND=JPRB), DIMENSION(KLON) :: E_MIN 
REAL(KIND=JPRB), DIMENSION(KLON) :: ZUS0
REAL(KIND=JPRB), DIMENSION(KLON) :: S
REAL(KIND=JPRB), DIMENSION(KLON) :: DTDZ
REAL(KIND=JPRB), DIMENSION(KLON) :: FST 
REAL(KIND=JPRB), DIMENSION(KLON) :: SDS

REAL(KIND=JPRB), DIMENSION(KLON,KLEVO+1) :: AU, BU, CU, FU, FV
REAL(KIND=JPRB), DIMENSION(KLON,KLEVO+1) :: AT, BT, CT, FT, AE, BE, CE, FE
REAL(KIND=JPRB), DIMENSION(KLON,KLEVO+1) :: DM, DH, DE, STOT, DSDE
REAL(KIND=JPRB), DIMENSION(KLON,KLEVO+1) :: USTOKES, VSTOKES  ! Stokes profile
REAL(KIND=JPRB), DIMENSION(KLON,KLEVO+1) :: Q_TURB

LOGICAL :: LLINCR
LOGICAL :: LLINITIME
    
!--------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('OC_MLM_MOD:OC_MLM',0,ZHOOK_HANDLE)

ASSOCIATE(B=>YDMLM%B, CP=>YDMLM%CP, DELZ=>YDMLM%DELZ, DML=>YDMLM%DML, &
 & F=>YDMLM%F, R=>YDMLM%R, R1=>YDMLM%R1, R2=>YDMLM%R2, &
 & R3=>YDMLM%R3, RHO=>YDMLM%RHO, SM=>YDMLM%SM, SQ=>YDMLM%SQ, &
 & XKAPPA=>YDMLM%XKAPPA, Z=>YDMLM%Z, ZR1=>YDMLM%ZR1, &
 & ZR2=>YDMLM%ZR2, ZR3=>YDMLM%ZR3, ZS=>YDMLM%ZS)
!
!***  1. SETTINGS IMPLICIT SCHEME, MODEL CONSTANTS AND INITIALISATION.
!     --------------------------------------------------------------- 
!

ALP_IMP = ONE
ALP_E   = ONE
ZIMP    = ONE

DO JL=KIDIA,KFDIA
  E_MIN(JL) = EPS*ONE_HALF*WSTAR(JL)**2
ENDDO

!    SURFACE STOKES DRIFT MAGNITUDE 
DO JL=KIDIA,KFDIA
   ZUS0(JL) = SQRT(MAX(PUSTOKES(JL)**2+PVSTOKES(JL)**2,1.E-8_JPRB))
ENDDO

!
!*    1.1 INITIALIZE CONSTANT ARRAYS AND FIELDS.
!     -----------------------------------------
!


!!!!!! this is not ideal,
!! for now, because we have initialied the fields to 0., we assume that it it is all 0, then, it will need to initialised 
   LLINITIME = .TRUE.
   OUTER : DO I=1,KLEVO+1
     DO JL=KIDIA,KFDIA
       IF ( POTKE(JL,I) > 0._JPRB ) THEN
         LLINITIME = .FALSE.
         EXIT OUTER 
       ENDIF 
     ENDDO
   ENDDO OUTER

IF (LLINITIME) THEN
   ZFAC0   = (B/SM)**(ONE_FOURTH)
   DO I=1,KLEVO+1
     DO JL=KIDIA,KFDIA
       T(JL,I) = T_S(JL)
       U(JL,I) = WSTAR(JL)/XKAPPA*LOG((Z(KLEVO+1)+Z0(JL))/(Z(I)+Z0(JL)))
       V(JL,I) = 0.0_JPRB 

       Q_TURB(JL,I) = ZFAC0*WSTAR(JL)
       POTKE(JL,I)  = ONE_HALF*Q_TURB(JL,I)**2
     ENDDO
   ENDDO
ENDIF


DO I=1,KLEVO+1
  DO JL=KIDIA,KFDIA
    Q_TURB(JL,I) = SQRT(TWO*POTKE(JL,I))
  ENDDO
ENDDO
  
!
!*    1.2 APPLY BOUNDARY CONDITIONS: SET T,U AND V TO FOUNDATION VALUES.
!     -----------------------------------------------------------------
!
DO JL=KIDIA,KFDIA
  T(JL,KLEVO+1) = T_S(JL)
  U(JL,KLEVO+1) = U_S(JL)
  V(JL,KLEVO+1) = V_S(JL)
ENDDO
!
!***  2. DETERMINE TEMPERATURE AND VELOCITY PROFILE.
!     ---------------------------------------------
!

!
!*    2.1 DETERMINE EXCHANGE COEFFICIENTS AND SOURCES FOR TKE EQUATION.
!     ----------------------------------------------------------------
!
DO ITIME=1,NSTEP

  DO I=1,KLEVO+1

    ZFAC1 = ONE/DELZ(I)
    IMIN  = MAX(1,I-1)

    DO JL=KIDIA,KFDIA

      DUDZ     = (U(JL,I)-U(JL,IMIN))*ZFAC1
      DVDZ     = (V(JL,I)-V(JL,IMIN))*ZFAC1
      DTDZ(JL) = (T(JL,I)-T(JL,IMIN))*ZFAC1

      S(JL)   = SQRT(DUDZ**2+DVDZ**2)
      PHI     = PHI_W(JL)
      SDS(JL) = ZUS0(JL)*(DUDZ*SIN(PHI)+DVDZ*COS(PHI))

    ENDDO

    CALL SOURCE_E(KIDIA,KFDIA,KLON,Z(I),T(:,I),&
 &                PPHIOC,Z0,WSTAR,XKS,S,SDS,DTDZ,&
 &                Q_TURB(:,I),&
 &                FST,STOT(:,I),DSDE(:,I),DM(:,I),&
 &                DH(:,I),DE(:,I),&
 &                YDMLM,YDCST)

    DO JL=KIDIA,KFDIA
      USTOKES(JL,I) = PUSTOKES(JL)*FST(JL)
      VSTOKES(JL,I) = PVSTOKES(JL)*FST(JL)
    ENDDO

  ENDDO
!
!*    2.2 DETERMINE THE A,B AND C'S FOR T,U,V, AND E.
!     ----------------------------------------------
!
  DO I=1,KLEVO

    ZFAC1 = DELT/DELZ(I)
    ZFAC2 = DELT/DELZ(I+1)
    ZFAC3 = DELT/DELZ(I)**2
    ZFAC4 = DELT/(DELZ(I)*DELZ(I+1))
    DO JL=KIDIA,KFDIA

      IMIN = MAX(1,I-1)
!
!*    2.2.1  INCREMENT POTKE(I).
!     -------------------------
!
      SIG1 = DE(JL,I)*ZFAC3
      SIG2 = DE(JL,I+1)*ZFAC4 

      CE(JL,I) = -ALP_E*SIG2
      BE(JL,I) = MAX(ONE,ONE+ALP_E*(SIG1+SIG2)-ZIMP*DELT*DSDE(JL,I))
      AE(JL,I) = -ALP_E*SIG1

      XINCR_D = SIG2*(POTKE(JL,I+1)-POTKE(JL,I))-SIG1*(POTKE(JL,I)-POTKE(JL,IMIN))
      XINCR_H = DELT*STOT(JL,I)
      FE(JL,I) = XINCR_D+XINCR_H
!
!*    2.2.2  INCREMENT T(I).
!     ---------------------
!
      SIG2 = DH(JL,I+1)*ZFAC4
      SIG1 = DH(JL,I)*ZFAC3

      CT(JL,I) = -ALP_IMP*SIG2
      BT(JL,I) = ONE+ALP_IMP*(SIG1+SIG2)
      AT(JL,I) = -ALP_IMP*SIG1

      XINCR_D = SIG2*(T(JL,I+1)-T(JL,I))-SIG1*(T(JL,I)-T(JL,IMIN))
      XINCR_H = -ZFAC1*(Q_0(JL)*(F(I+1)-F(I))+RT(JL)*(R(I+1)-R(I)))/(RHO*CP)
      FT(JL,I) = XINCR_D+XINCR_H
!
!*    2.2.3  INCREMENT U(I).
!     ---------------------
!
      FCD = DELT*FC(JL)
      SIG2 = DM(JL,I+1)*ZFAC4
      SIG1 = DM(JL,I)*ZFAC3

      CU(JL,I) = -ALP_IMP*SIG2
      BU(JL,I) = ONE+ALP_IMP*(SIG1+SIG2)
      AU(JL,I) = -ALP_IMP*SIG1

      XINCR_D = SIG2*(U(JL,I+1)-U(JL,I))-&
     &          SIG1*(U(JL,I)-U(JL,IMIN))+FCD*(V(JL,I)&
     &          +VSTOKES(JL,I))
      ABS_XINCR_H = -ZFAC1*WSTAR(JL)**2*(F(I+1)-F(I))
      XINCR_H = ABS_XINCR_H*SIN(PHI_W(JL))
      FU(JL,I) = XINCR_D+XINCR_H
!
!*    2.2.4  INCREMENT V(I).
!     ---------------------
!
      XINCR_D = SIG2*(V(JL,I+1)-V(JL,I))-&
     &          SIG1*(V(JL,I)-V(JL,IMIN))-FCD*(U(JL,I)&
     &          +USTOKES(JL,I))
      XINCR_H = ABS_XINCR_H*COS(PHI_W(JL))
      FV(JL,I) = XINCR_D+XINCR_H

    ENDDO

  ENDDO
!
!*    2.3 INTEGRATE T,U,V, AND POTKE FOR ONE TIME STEP WITH IMPLICIT SCHEME.
!     ----------------------------------------------------------------------
!
  LLINCR = .FALSE.
  CALL TRIDAG(AU,BU,CU,FU,U,KLEVO,KIDIA,KFDIA,KLON,LLINCR)
  CALL TRIDAG(AU,BU,CU,FV,V,KLEVO,KIDIA,KFDIA,KLON,LLINCR)
  CALL TRIDAG(AT,BT,CT,FT,T,KLEVO,KIDIA,KFDIA,KLON,LLINCR)
  CALL TRIDAG(AE,BE,CE,FE,POTKE,KLEVO,KIDIA,KFDIA,KLON,LLINCR)

!
!*   2.3.1 MAKE SURE TKE IS POSITIVE, DETERMINE TURBULENT VELOCITY Q_TURB
!          AND APPLY BOUNDARY CONDITION DE/DZ(Z=DML)=0
!    --------------------------------------------------------------------    
!
  DO I=1,KLEVO
    DO JL=KIDIA,KFDIA
      POTKE(JL,I) = MAX(E_MIN(JL),POTKE(JL,I))
      Q_TURB(JL,I) = SQRT(TWO*POTKE(JL,I))
    ENDDO
  ENDDO
  
  DO JL=KIDIA,KFDIA
    POTKE(JL,KLEVO+1) = POTKE(JL,KLEVO)
    Q_TURB(JL,KLEVO+1) = Q_TURB(JL,KLEVO)
  ENDDO
!
!*    END OF TIME LOOP
! 
ENDDO
 
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('OC_MLM_MOD:OC_MLM',1,ZHOOK_HANDLE)

END SUBROUTINE OC_MLM

END MODULE OC_MLM_MOD
