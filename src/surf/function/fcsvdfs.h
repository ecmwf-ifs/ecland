! (C) Copyright 1990- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     ------------------------------------------------------------------

!     *FCVDFS** CONTAINS STATEMENT FUNCTIONS DESCRIBING STAB. FUNCT.

!     A.C.M. BELJAARS    E.C.M.W.F.      26/03/90.

!     ------------------------------------------------------------------

!          *THE STABILITY FUNCTIONS ARE THE SO-CALLED *PHI* AND
!     *PSI*-FUNCTIONS. THE *PSI*-FUNCTIONS GIVE THE STABILITY
!     CORRECTIONS IN THE LOGARITHMIC PROFILES FOR
!     WIND, DRY STATIC ENERGY AND SPECIFIC HUMIDITY. THE FUNCTIONS
!     DEPEND ON THE RATIO OF HEIGHT AND *OBUKHOV LENGTH (*ETA*).
!          FOR THE UNSTABLE BOUNDARY LAYER, THE *DYER AND *HICKS
!     FORMULATIONS ARE USED (CF. *DYER, 1974; *HOGSTROM, 1988). IN
!     STABLE SITUATIONS, THE EMPIRICAL FORMS, PROPOSED BY *HOLTSLAG
!     AND *DEBRUIN ARE USED WITH A MODIFICATION TO SATISFY A CRITICAL
!     FLUX-*RICHARDSON NUMBER FOR LARGE *ETA*.
!          THE *PHI* AND *PSI* FUNCTIONS ARE INTERRELATED. THE *PSI*
!     FUNCTIONS CAN BE DERIVED FROM THE *PHI* FUNCTIONS BY INTEGRATION
!     OF (1.-PHI)/ETA OR *PHI* FROM *PSI* BY COMPUTING
!     (1.-ETA*DPSI/DETA) (SEE ALSO *HAUGEN, 1973; WORKSHOP ON
!     MICROMETEOROLOGY, P. 77).


!     ------------------------------------------------------------------

!        *PHI AND *PSI FUNCTIONS FOR UNSTABLE SITUATIONS ACCORDING
!        TO *DYER AND *HICKS
!        3/2/03 Modification, J.Hague  X**1.5 -> X*SQRT(X)

REAL(KIND=JPRB) :: PHIHU,PHIMU
REAL(KIND=JPRB) :: PETA
PHIHU(PETA)= 1.0_JPRB/     SQRT(1.0_JPRB-RCDHALF*PETA)
PHIMU(PETA)= 1.0_JPRB/SQRT(SQRT(1.0_JPRB-RCDHALF*PETA))

REAL(KIND=JPRD) :: PSIHU,PSIMU
REAL(KIND=JPRD) :: DPETA

PSIHU(DPETA)= 2.0_JPRD*LOG((1.0_JPRD+     SQRT(1.0_JPRD-RCDHALF*DPETA))*0.5_JPRD )
PSIMU(DPETA)=    LOG((1.0_JPRD+SQRT(SQRT(1.0_JPRD-RCDHALF*DPETA)))**2 &
                  &*(1.0_JPRD+     SQRT(1.0_JPRD-RCDHALF*DPETA) ) *0.125_JPRD )&
                  &-2.0_JPRD*ATAN(SQRT(SQRT(1.0_JPRD-RCDHALF*DPETA)))&
                  &+ RCDHPI2

!        *PHI AND *PSI FUNCTIONS FOR UNSTABLE SITUATIONS ACCORDING
!        TO HOGSTROM FOR MOMENTUM AND DERIVED FROM THE ELLISON AND
!        TURNER RELATION FOR THE RATIO OF PHIM AMD PHIH.

REAL(KIND=JPRB) :: PHIMS,PHIHS
PHIMS(PETA)=1.0_JPRB+RCHETA*PETA
PHIHS(PETA)=(1.0_JPRB+RCHETB*PETA)**2

!     PSI FUNCTIONS NOT COMPATIBLE WITH PHI FUNCTIONS IN THE STABLE CASE

REAL(KIND=JPRD) :: PSIHS,PSIMS
!PSIHS(DPETA)= -RCHBB*(DPETA-RCHBCD)*EXP  (-RCHBD*DPETA)&
!               &-(_ONE_+RCHB23A*DPETA)**1.5_JPRD - RCHBBCD + _ONE_
PSIHS(DPETA)= -RCHBB*(DPETA-RCHBCD)*EXP  (-RCHBD*DPETA)&
              &-(1.0_JPRD+RCHB23A*DPETA)*SQRT(1.0_JPRD+RCHB23A*DPETA)-RCHBBCD+1.0_JPRD
PSIMS(DPETA)= -RCHBB*(DPETA-RCHBCD)*EXP  (-RCHBD*DPETA)&
               &-RCHBA*DPETA - RCHBBCD

