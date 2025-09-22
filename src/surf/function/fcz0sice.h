
! (C) Copyright 2025- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     ------------------------------------------------------------------

!     *PZ0SICE** CONTAINS STATEMENT FUNCTIONS DESCRIBING ROUGNESS LENGTH
!                FOR HEAT OR MOISTURE OVER SEA ICE 

!     JEAN BIDLOT    E.C.M.W.F.      12/02/2021.

!     This is based on Andreas (1987) work in which the roughness length scale of
!     these scalars over sea ice is proportional to the roughness length scale of
!     momentum (Z0M) via a scaling factor which is function of the Reynolds roughness number:
!     Z0M * USTAR / nu_air, where USTAR is the friction velocity anf nu_air the kinematic
!     air density 

!     ------------------------------------------------------------------

FUNCTION PZ0SICE(PZ0MICE, PUST, JHQ)

IMPLICIT NONE

REAL(KIND=JPRB) :: PZ0SICE

REAL(KIND=JPRB) :: PZ0MICE ! ROUGHNESS LENGTH FOR MOMEMTUM OVER SEA ICE 
REAL(KIND=JPRB) :: PUST    ! FRICTION VELOCITY

INTEGER(KIND=JPIM) :: JHQ  ! ( = 1 Heat,  = 2 Moisture)


!*    LOCAL STORAGE
!     -------------

REAL(KIND=JPRD) :: PNU = 1.35E-5_JPRD     ! AIR VISCOSITY AT 0 DEGREE C

! Andreas (1987) discontinous 3 parameter functional relation was replaced with a 4 parameter
! continous approximation
! HEAT/ MOISTURE :
REAL(KIND=JPRD), PARAMETER, DIMENSION(2) :: PAZ0ICE = (/-0.0061586_JPRD, -0.00544869_JPRD/)
REAL(KIND=JPRD), PARAMETER, DIMENSION(2) :: PBZ0ICE = (/-0.12756_JPRD, -0.12027_JPRD/)
REAL(KIND=JPRD), PARAMETER, DIMENSION(2) :: PCZ0ICE = (/-0.66267_JPRD, -0.68407_JPRD/)
REAL(KIND=JPRD), PARAMETER, DIMENSION(2) :: PDZ0ICE = (/0.25344_JPRD, 0.4826_JPRD/) 

! Limit of validity of Andreas formulation
REAL(KIND=JPRD), PARAMETER :: PRSTARMIN=0.01_JPRD   ! REYNOLDS ROUGHNESS NUMBER MINUMUM
REAL(KIND=JPRD), PARAMETER :: PRSTARMAX=1000._JPRD  ! REYNOLDS ROUGHNESS NUMBER MAXIMUM

REAL(KIND=JPRD) :: ZLNRSTAR  ! LOG OF REYNOLDS ROUGHNESS NUMBER FOR SEA ICE
REAL(KIND=JPRD) :: ZLNRSTAR2 ! ZLNRSTAR**2
REAL(KIND=JPRD) :: ZLNRSTAR3 ! ZLNRSTAR**3

! ------------------------------------------------------------------

ZLNRSTAR = LOG(MIN(MAX(REAL(PZ0MICE*PUST,KIND=JPRD)/PNU, PRSTARMIN), PRSTARMAX))
ZLNRSTAR2 = ZLNRSTAR*ZLNRSTAR
ZLNRSTAR3 = ZLNRSTAR2*ZLNRSTAR

PZ0SICE = REAL(PZ0MICE, KIND=JPRD) * EXP( PAZ0ICE(JHQ)*ZLNRSTAR3 + PBZ0ICE(JHQ)*ZLNRSTAR2 + PCZ0ICE(JHQ)*ZLNRSTAR + PDZ0ICE(JHQ) )

END FUNCTION PZ0SICE 
