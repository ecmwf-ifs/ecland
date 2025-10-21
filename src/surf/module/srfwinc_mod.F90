MODULE SRFWINC_MOD
CONTAINS
SUBROUTINE SRFWINC(KIDIA,KFDIA,KLEVS,KCWS,&
 & PTMST,PWSAM1M,PWSADIF,PSAWGFL,PCFW,&
 & YDSOIL,&
 & PWSA,PWFSD,LDLAND,PDHWLS)  

USE PARKIND1  , ONLY : JPIM, JPRB
USE YOMHOOK   , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_THF   , ONLY : RHOH2O
USE YOS_SOIL  , ONLY : TSOIL

! (C) Copyright 1993- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SRFWINC* -  COMPUTES THE T+1 VALUES OF SOIL MOISTURE

!     PURPOSE.
!     --------
!          COMPUTES THE T+1 VALUES OF SOIL MOISTURE BASED ON THE VALUES
!     OBTAINED BY SOLVING THE SYSTEM OF EQUATIONS. IT SHOULD BE
!     PRECEDED BY A CALL TO *SRFWEXC* AND *SRFWDIF*.

!**   INTERFACE.
!     ----------
!          *SRFWINC* IS CALLED FROM *SURFTSTP*

!     PARAMETER   DESCRIPTION                                    UNITS
!     ---------   -----------                                    -----

!     INPUT PARAMETERS (INTEGER):
!    *KIDIA*      START POINT
!    *KFDIA*      END POINT
!    *KLON*       NUMBER OF GRID POINTS PER PACKET
!    *KLEVS*      NUMBER OF SURFACE LAYERS
!    *KCWS        Number of layers to merge at the end for the soil water profile (for > 4layers)
!    *KDHVWLS*    Number of variables for soil water budget
!    *KDHFWLS*    Number of fluxes for soil water budget

!     INPUT PARAMETERS (REAL):
!    *PTMST*      TIME STEP                                      S

!     INPUT PARAMETERS (LOGICAL):
!    *LDLAND*     LAND/SEA MASK (TRUE/FALSE)

!     INPUT PARAMETERS AT T-1 (REAL):
!    *PWSAM1M*    MULTI-LAYER SOIL MOISTURE                  M**3/M**3
!    *PWSADIF*    (W *)                     DIVIDED BY ALFA  M**3/M**3
!    *PCFW*       MODIFIED DIFFUSIVITIES                         M

!     OUTPUT PARAMETERS (REAL):
!    *PWSA*       UNFILTERED VALUE AT T+1                    M**3/M**3
!    *PSAWGFL*    GRAVITY PART OF WATER FLUX (AT INPUT)
!                 DRAINAGE + WATER DIFFUSIVITY (OUTPUT)       KG/M**2/S
!                   (positive downwards, at layer bottom)
!    *PWFSD*      WATER FLUX BETWEEN LAYER 1 AND 2            KG/M**2/S

!     OUTPUT PARAMETERS (DIAGNOSTIC):
!    *PDHWLS*     Diagnostic array for soil water (see module yomcdh)

!     METHOD.
!     -------
!     TRIVIAL.

!     EXTERNALS.
!     ----------
!          NONE.

!     REFERENCE.
!     ----------
!          SEE SOIL PROCESSES' PART OF THE MODEL'S DOCUMENTATION FOR
!     DETAILS ABOUT THE MATHEMATICS OF THIS ROUTINE.

!      Original :
!     P.VITERBO      E.C.M.W.F.      9/02/93
!     P.VITERBO      E.C.M.W.F.      17-05-2000
!        (Surface DDH for TILES)
!     J.F. Estrade *ECMWF* 03-10-01 move in surf vob
!     P. Viterbo    24-05-2004      Change surface units
!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM), INTENT(IN)   :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KLEVS
INTEGER(KIND=JPIM), INTENT(IN)   :: KCWS 

LOGICAL,   INTENT(IN)   :: LDLAND(:)

REAL(KIND=JPRB),    INTENT(IN)   :: PTMST
REAL(KIND=JPRB),    INTENT(IN)   :: PWSAM1M(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PWSADIF(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PCFW(:,:)
TYPE(TSOIL),        INTENT(IN)   :: YDSOIL

REAL(KIND=JPRB),    INTENT(INOUT):: PDHWLS(:,:,:)

REAL(KIND=JPRB),    INTENT(OUT)  :: PSAWGFL(:,:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PWSA(:,:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PWFSD(:)

INTEGER(KIND=JPIM) :: JK, JL
INTEGER(KIND=JPIM) :: KLEVS_WB,ILEVM1_WB

REAL(KIND=JPRB) :: ZWSA_tot(KIDIA:KFDIA)
REAL(KIND=JPRB) :: ZDI(KLEVS)

REAL(KIND=JPRB) :: ZCONS, ZTPFAC2, ZTPFAC3,ZHOH2O
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!     ------------------------------------------------------------------

!*         1.    SET UP SOME CONSTANTS.
!                --- -- ---- ----------

!     NUMERICAL CONSTANTS

IF (LHOOK) CALL DR_HOOK('SRFWINC_MOD:SRFWINC',0,ZHOOK_HANDLE)
ASSOCIATE(RDAW=>YDSOIL%RDAW, RSIMP=>YDSOIL%RSIMP)
ZTPFAC2=1.0_JPRB/RSIMP
ZTPFAC3=ZTPFAC2-1.0_JPRB
ZHOH2O=1.0_JPRB/RHOH2O
ZCONS=RHOH2O/PTMST

IF (KCWS .gt. 0_JPIM) THEN
  KLEVS_WB=KLEVS-KCWS
  ILEVM1_WB=KLEVS_WB-1
ELSE
  KLEVS_WB=KLEVS
  ILEVM1_WB=KLEVS-1
ENDIF

DO JK=1,KLEVS
  ZDI(JK)= RDAW(JK)
ENDDO

!*         2.    COMPUTE T+1 UNFILTERED VALUES
!                ------- --- ---------- ------

DO JK=1,KLEVS_WB
  DO JL=KIDIA,KFDIA
    IF (LDLAND(JL)) THEN
      PWSA(JL,JK)=PWSADIF(JL,JK)-ZTPFAC3*PWSAM1M(JL,JK)

!     DIAGNOSTICS

      IF (JK < KLEVS_WB) THEN
        PSAWGFL(JL,JK)=PSAWGFL(JL,JK)+ZCONS*PCFW(JL,JK)*&
         & (PWSADIF(JL,JK)-PWSADIF(JL,JK+1))  
      ENDIF
    ELSE
      PWSA(JL,JK)=PWSAM1M(JL,JK)
      PSAWGFL(JL,JK)=0.0_JPRB
    ENDIF
  ENDDO
ENDDO

 IF (KCWS .gt. 0_JPIM) THEN
  DO JK=KLEVS_WB+1,KLEVS
    DO JL=KIDIA,KFDIA
      PSAWGFL(JL,JK)=0.0_JPRB
    ENDDO
  ENDDO
 ENDIF


! Total water layer KLEVS_WB
IF (KCWS .gt. 0_JPIM) THEN
DO JK=KLEVS_WB+1,KLEVS
  DO JL=KIDIA,KFDIA
    ZWSA_tot(JL) = PWSA(JL,KLEVS_WB)*ZDI(KLEVS_WB)*RHOH2O
    PWSA(JL,JK) = ZWSA_tot(JL)/(ZDI(JK)*RHOH2O)
  ! IF (JK==KLEVS) PWSA(JL,JK) = 0.80_JPRB
  ENDDO
ENDDO
ENDIF

!     ------------------------------------------------------------------
!*         3.  CONTRIBUTION TO DIAGNOSTICS.
!              ------------ -- ------------

DO JL=KIDIA,KFDIA
  PWFSD(JL)=PSAWGFL(JL,1)
ENDDO

!*         3a. DDH DIAGNOSTICS
!              ---------------
IF (SIZE(PDHWLS) > 0) THEN
! Ground water flux (positive downwards)
  DO JK=1,KLEVS
    DO JL=KIDIA,KFDIA
      PDHWLS(JL,JK,8)=PSAWGFL(JL,JK)
    ENDDO
  ENDDO

! Bottom water flux accounted as runoff (deep drainage); 
!    (negative values mean water lost by the layer)
  DO JK=1,KLEVS
    DO JL=KIDIA,KFDIA
      PDHWLS(JL,KLEVS,6)=PDHWLS(JL,KLEVS,6)-PSAWGFL(JL,KLEVS)
    ENDDO
  ENDDO
ENDIF

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SRFWINC_MOD:SRFWINC',1,ZHOOK_HANDLE)

END SUBROUTINE SRFWINC
END MODULE SRFWINC_MOD
