MODULE YOS_EXC
 
USE PARKIND1  ,ONLY : JPIM     ,JPRB

! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------
!*    ** *YOS_EXC* CONTAINS CONSTANTS NEEDED BY *V....*
!     ------------------------------------------------------------------

TYPE :: TEXC
LOGICAL :: LELWDD           ! TRUE when longwave downward derivative is used for skin temperature
LOGICAL :: LELWTL           ! TRUE when longwave net radiation is tiled to the surface
LOGICAL :: LEOCWA           ! TRUE if WARM OCEAN LAYER PARAMETRIZATION active
LOGICAL :: LEOCCO           ! TRUE if COOL OCEAN SKIN PARAMETRIZATION active
LOGICAL :: LWCOU            ! TRUE if coupled to wave model
LOGICAL :: LWCOU2W          ! TRUE if coupled to wave model, there are feedbacks onto the atmosphere (momentum, warm layer scheme or ocean TKE scheme)
LOGICAL :: LWCOUHMF         ! TRUE if coupled to wave model, there is a direct feedback onto the ocean heat and moisture flux
LOGICAL :: LEOCSA           ! TRUE if SALINTY EFFECT ON SATURATION AT OCEAN SURFACE active
LOGICAL :: LEOCLA           ! TRUE if LANGMUIR CIRCULATION EFFIECT active
LOGICAL :: LSCMEC           ! TRUE if ECMWF Single Column Model
LOGICAL :: LROUGH           ! TRUE if externally specified roughness lengths
REAL(KIND=JPRB) :: REXTZ0M  ! roughness length for momentum (if LROUGH)
REAL(KIND=JPRB) :: REXTZ0H  ! roughness length for heat and moisture (if LROUGH)
REAL(KIND=JPRB) :: RKAP     ! VONKARMAN CONSTANT
REAL(KIND=JPRB) :: REPDU2   ! MINIMUM VELOCITY DIFFERENCE IN RI-NUMBER
REAL(KIND=JPRB) :: RPARZI   ! ANSATZ FOR PBL-H IN W* COMPUTATION
REAL(KIND=JPRB) :: RZ0ICE   ! ROUGHNESS OVER SEA ICE
REAL(KIND=JPRB) :: REPUST   ! MINIMUM FRICTION VELOCITY (SECURITY PARAMETER)
REAL(KIND=JPRB) :: RNU      ! SMOOTH SURFACE CONSTANT KINEMATIC AIR DENSITY
REAL(KIND=JPRB) :: RNUM     ! SMOOTH SURFACE CONSTANT IN Z0M=RNUM/u* (RNUM a faction of RNU)
REAL(KIND=JPRB) :: RNUH     ! SMOOTH SURFACE CONSTANT IN Z0H=RNUH/u* (RNUH a faction of RNU)
REAL(KIND=JPRB) :: RNUQ     ! SMOOTH SURFACE CONSTANT IN Z0Q=RNUQ/u* (RNUQ a faction of RNU)
REAL(KIND=JPRB) :: RCHAR    ! CHARNOCK CONSTANT

REAL(KIND=JPRB) :: RSSRFLTIMAX ! MAX SOLAR FLUX W/m2 (introduced due to inconsitency with albedo)

REAL(KIND=JPRB) :: RBLENDWMO ! WMO POST-PROCESSING
REAL(KIND=JPRB) :: RZ0MWMO   ! WMO POST-PROCESSING

REAL(KIND=JPRB) :: RETACONV  ! SAFETY PROVISION FOR DIVERGING ITERATIONS
REAL(KIND=JPRB) :: RCDFC     ! RCDFC is a rough estimate of the drag coefficient used
                             ! to convert w*-gustiness at the lowest model level into u*. 
                             ! The value is choosen from Fig. 1 on page 37 of the ECMWF 
                             ! seminar proceedings on "Atmopshere-surface interaction", 
                             ! ie charqacteristic for 1 m/s in unstable situations. 
REAL(KIND=JPRB) :: RBLENDSNVEG  ! snowheight up to which a blending between veg. roughness and snow roughness is calc.

REAL(KIND=JPRB) :: RUGNA     ! coefficient in ZUG formulation, see sppgust_mod
REAL(KIND=JPRB) :: RUGNB     ! coefficient in ZUG formulation, see sppgust_mod
REAL(KIND=JPRB) :: RUGN      ! coefficient in ZUG formulation, see sppgust_mod

REAL(KIND=JPRB) :: RPARZIINI ! RPARZI before divided by PRPLRG
REAL(KIND=JPRB) :: RZ0ICEINI ! RZ0ICE before divided by PRPLRG
REAL(KIND=JPRB) :: RNUINI    ! RNU before divided by PRPLRG
REAL(KIND=JPRB) :: RNUMINI   ! RNUM before mult. by RNU
REAL(KIND=JPRB) :: RNUHINI   ! RNUH before mult. by RNU
REAL(KIND=JPRB) :: RNUQINI   ! RNUQ before mult. by RNU

! PZ0ICE function
REAL(KIND=JPRB) :: RAZ0ICE    ! coefficient in PZ0ICE function
REAL(KIND=JPRB) :: RBZ0ICE    ! coefficient in PZ0ICE function
REAL(KIND=JPRB) :: RCZ0ICE    ! coefficient in PZ0ICE function
REAL(KIND=JPRB) :: RDZ0ICE    ! coefficient in PZ0ICE function


! PZ0WN function
INTEGER(KIND=JPIM) :: RITZ0WN ! coeffiecient in PZ0WN function
REAL(KIND=JPRB) :: RACDZ0WN   ! coeffiecient in PZ0WN function
REAL(KIND=JPRB) :: RBCDZ0WN   ! coeffiecient in PZ0WN function
REAL(KIND=JPRB) :: RXEPSZ0WN  ! coeffiecient in PZ0WN function
REAL(KIND=JPRB) :: RUSTMINZ0WN    ! coeffiecient in PZ0WN function
REAL(KIND=JPRB) :: RPCHARMAXZ0WN  ! coeffiecient in PZ0WN function
REAL(KIND=JPRB) :: RZ0FGZ0WN  ! coeffiecient in PZ0WN function

REAL(KIND=JPRB) :: RSALIN     ! salinty effect on saturation at ocean surface
REAL(KIND=JPRB) :: RBLENDZ0

END TYPE TEXC

                            !  (simplified physics only)

!*     *YOS_EXC* CONTAINS CONSTANTS NEEDED BY *V....*
!     FOR THE COMPUTATION OF SURFACE DIFFUSION EXCHANGE COEFFICIENTS

!     A.C.M. BELJAARS      E.C.M.W.F.    14/12/89
!     OBUKHOV-L UPDATE     ACMB          26/03/90.
!     surf externalisation P. Viterbo    09/06/2005

END MODULE YOS_EXC
