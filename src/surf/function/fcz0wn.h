! (C) Copyright 2018- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     ------------------------------------------------------------------

!     *FCZ0WN** CONTAINS STATEMENT FUNCTIONS DESCRIBING ROUGNESS LENGTH
!               FOR MOMENTUM UNDER NEUTRAL WIND CONDITIONS.

!     JEAN BIDLOT    E.C.M.W.F.      10/12/2018.
!     I. AYAN-MIGUEZ    BSC     Oct 2023: Added refactorization of Global parameters

!     ------------------------------------------------------------------

FUNCTION PZ0WN(PWIND, PGEO, PCHAR, PG, PNUM, PKAP, NITER, ACD, &
               & BCD, XEPS, USTMIN, PCHARMAX, Z0FG)

IMPLICIT NONE

REAL(KIND=JPRB) :: PZ0WN
! *PZ0WN*  AERODYNAMIC ROUGHNESS LENGTH FOR NEUTRAL WIND PROFILE WITH
!          WIND SPEED PWIND AT HEIGHT Z=PGEO/PG FOR A CHARNOCK PARAMETER
!          PCHAR AND A VISCOUS COEFFIENT PNUM AND A VON KARMAN CONSTANCE

!          FIND Z0 WHICH SATISTIES THE NEUTRAL WIND PROFILE
!          PWIND = (UST/PKAP)*LOG(1.+Z/Z0)
!          WHERE  Z0=PNUM/UST+PCHAR*UST**2/PG, Z=PGEO/PG
!          A NEWTON METHOD IS USED TO IETRATIVELY FIND UST AND HENCE Z0:
!          GIVEN F=PWIND-(UST/PKAP)*LOG(1.+Z/Z0)
!          UST(n+1) = UST(n) - F/(dF/dUST)

REAL(KIND=JPRB) :: PWIND, PGEO, PCHAR, PG, PNUM, PKAP
! *PWIND* WIND SPEED AT LOWEST LEVEL
! *PGEO*  GEOPOTENTIAL AT LOWEST MODEL LEVEL
! *PCHAR* CHARNOCK PARAMETER VALUE
! *PG*    GRAVITY
! *PNUM*  EFFECTIVE KINEMATIC VISCOUS COEFFIENT OF AIR
! *PKAP*  VON KARMAN CONSTANCE

INTEGER(KIND=JPIM) :: NITER

! CD=ACD+BCD*U10
REAL(KIND=JPRB) :: ACD
REAL(KIND=JPRB) :: BCD

REAL(KIND=JPRB) :: XEPS
REAL(KIND=JPRB) :: USTMIN
REAL(KIND=JPRB) :: PCHARMAX
REAL(KIND=JPRB) :: Z0FG

  !*    LOCAL STORAGE
  !     -------------
INTEGER(KIND=JPIM) :: ITER
REAL(KIND=JPRB) :: ZLEV, XZLEV, PCHAROG, XKPWIND, XOLOGZ0
REAL(KIND=JPRB) :: UST, USTOLD, Z0, Z0CH, Z0VIS, F, DELF

! ------------------------------------------------------------------

ZLEV=PGEO/PG

XKPWIND=PKAP*PWIND
! protect the scheme by preventing PCHAR to be larger than PCHARMAX
PCHAROG = MIN(PCHAR,PCHARMAX)/PG

! simple first guess
UST = PWIND*SQRT(ACD+BCD*PWIND)

! iterate
DO ITER=1,NITER
  USTOLD = MAX(UST,USTMIN)
  Z0CH   = PCHAROG*UST**2
  Z0VIS  = PNUM/UST
  Z0     = Z0CH+Z0VIS
  XZLEV  = ZLEV/(ZLEV+Z0)
  XOLOGZ0= 1.0_JPRB/LOG(1.0_JPRB+ZLEV/Z0)
  F      = UST-XKPWIND*XOLOGZ0
  DELF   = 1.0_JPRB-XKPWIND*XOLOGZ0**2*XZLEV*(2.0_JPRB*Z0CH-Z0VIS)/(UST*Z0)
  IF(DELF /= 0.0_JPRB) UST=UST-F/DELF
  IF(ABS(UST-USTOLD)<=UST*XEPS .AND. ABS(F)<=XEPS) EXIT
ENDDO

IF(ITER > NITER) THEN
! failed to iterate
  PZ0WN = Z0FG
ELSE
  UST=MAX(UST,USTMIN)
  PZ0WN=PNUM/UST+PCHAROG*UST**2
ENDIF
 
END FUNCTION PZ0WN
