INTERFACE
SUBROUTINE SURFWS    (YDSURF,KIDIA,KFDIA,KLON,KLEVS,KLEVSN, KTILES,&
                    & PSDOR,LDSICE, &
                    & PLSM, PCIL, PFRTI, PMU0,                          &
                    & PTSAM1M,PTSKIN, PALBSN,                      &  
                    & PTSNM1M ,PSNM1M ,                            &
                    & PRSNM1M, PWSNM1M                              )

! (C) Copyright 2017- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     ------------------------------------------------------------------
!**   *SURFWS* - CREATES WARM START CONDITIONS FOR SURFACE VARIABLES

!     PURPOSE
!     -------
!     REPLACE COLD STARTED VARIABLES (for instance multi-layer var
!     initialised with single-layer values) WITH PHYSICAL PROFILES.

!     INTERFACE
!     ---------
!     *SURFWS* IS CALLED BY *CALLPAR* 

!     INPUT PARAMETERS (INTEGER):
!     *KIDIA*        START POINT
!     *KFDIA*        END POINT
!     *KLON*         NUMBER OF GRID POINTS PER PACKET
!     *KLEVSN*       NUMBER OF SNOW LAYERS

!     INPUT PARAMETERS (REAL):
!     *PLSM*         LAND-SEA MASK                                  (0-1)
!     *PCIL*         LAND-ICE FRACTION                              (0-1)
!     *PSNM1M*       SNOW MASS (per unit area)                      kg/m**2
!     *PRSNM1M*      SNOW DENSITY                                   kg/m**3

!     OUTPUT PARAMETERS (REAL):
!     *PTSN*         SNOW TEMPERATURE WARM STARTED
!     *PSNS*         SEA-ICE INDICATOR
!     *PRSN*         LAKE INDICATOR
!     *PWSN*         NORTHERN HEMISPHERE INDICATOR

!     METHOD
!     ------
!     IT IS NOT ROCKET SCIENCE, BUT CHECK DOCUMENTATION

!     Modifications
!     G. Arduini        1 Sept 2017       Created
!     ------------------------------------------------------------------


USE PARKIND1, ONLY : JPIM, JPRB
USE, INTRINSIC :: ISO_C_BINDING


IMPLICIT NONE

! Declaration of arguments

TYPE(C_PTR)       ,INTENT(IN)    :: YDSURF
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVS
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVSN
INTEGER(KIND=JPIM),INTENT(IN)    :: KTILES
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSM(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCIL(KLON)
LOGICAL           ,INTENT(IN)    :: LDSICE(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PMU0(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSDOR(KLON)

REAL(KIND=JPRB)   ,INTENT(IN)    :: PFRTI(KLON,KTILES)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSKIN(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PALBSN(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSAM1M(KLON,KLEVSN)

REAL(KIND=JPRB)   ,INTENT(INOUT) :: PTSNM1M(KLON,KLEVSN) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSNM1M(KLON,KLEVSN) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PRSNM1M(KLON,KLEVSN)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PWSNM1M(KLON,KLEVSN) 

!     ------------------------------------------------------------------

END SUBROUTINE SURFWS
END INTERFACE
