MODULE YOS_VEG
 
USE PARKIND1  ,ONLY : JPIM     ,JPRB

! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

IMPLICIT NONE
!prepifs tp
SAVE

TYPE :: TVEG
INTEGER(KIND=JPIM) :: NVTYPES                ! Number of vegetation (surface cover) types
INTEGER(KIND=JPIM) :: NVTILES                ! Number of vegetation (surface cover) tiles

REAL(KIND=JPRB),ALLOCATABLE :: RVCOV(:)      ! VEGETATION COVER FOR EACH TYPE
REAL(KIND=JPRB),ALLOCATABLE :: RVLAI(:)      ! LEAF AREA INDEX
REAL(KIND=JPRB),ALLOCATABLE :: RVROOTSA(:,:) ! PERCENTAGE OF ROOTS IN EACH SOIL LAYER
REAL(KIND=JPRB),ALLOCATABLE :: RVLAMSK(:)    ! Unstable SKIN LAYER CONDUCT. FOR EACH TILE
REAL(KIND=JPRB),ALLOCATABLE :: RVLAMSKS(:)   ! Stable SKIN LAYER CONDUCT. FOR EACH TILE
REAL(KIND=JPRB),ALLOCATABLE :: RVTRSR(:)     ! TRANSMISSION OF NET SOLAR RAD. 
                                             ! THROUGH VEG.
REAL(KIND=JPRB),ALLOCATABLE :: RVZ0M(:)      ! ROUGHNESS LENGTH FOR MOMENTUM
REAL(KIND=JPRB),ALLOCATABLE :: RVZ0H(:)      ! ROUGHNESS LENGTH FOR HEAT 
REAL(KIND=JPRB) :: REPEVAP                   ! MINIMUM ATMOSPHERIC DEMAND
REAL(KIND=JPRB) :: RVINTER                   ! EFFICIENCY OF INTERCEPTION OF PRECIPITATION
REAL(KIND=JPRB) :: RCEPSW                    ! MINIMUM RELATIVE HUMIDITY
REAL(KIND=JPRB),ALLOCATABLE :: RVRSMIN(:)    ! MIN STOMATAL RESISTANCE FOR EACH VEG. TYPE (S/M)
REAL(KIND=JPRB),ALLOCATABLE :: RVHSTR(:)     ! HUMIDITY STRESS FUNCTION PARAMETER (M/S kgkg-1)
REAL(KIND=JPRB) :: RLHAERO                   ! Unstable Aerodyn. lambda between high canopy and surface
REAL(KIND=JPRB) :: RLHAEROS                  ! Stable Aerodyn. lambda between high canopy and surface
REAL(KIND=JPRB) :: RCVC                      ! MINIMUM STOMATAL RESIsTANCE
                                             !  (Simplified physics only)
REAL(KIND=JPRB) :: RVLT                      ! Leaf Area index
                                             !  (Simplified physics only)
REAL(KIND=JPRB) :: RVRAD                     ! FRACTION OF THE NET SW RADIATION
                                             !  CONTRIBUTING TO PAR
                                             !  (Simplified physics only)
REAL(KIND=JPRB) :: REPSR                     ! MINIMUM VALUE FOR SW RADIATION IN
                                             !  THE CANOPY RESISTANCE COMPUTATION
                                             !  (Simplified physics only)
REAL(KIND=JPRB) :: RLAIINT                   ! Interactive LAI coefficient (1=interactive ; 0=climatology)
LOGICAL         :: LELAIV                    ! VARIABLE LAI
LOGICAL         :: LECTESSEL                 ! True when using CTESSEL scheme for CO2 surface fluxes
LOGICAL         :: LEAGS                     ! True when using CTESSEL scheme for H20 surface fluxes
LOGICAL         :: LEFARQUHAR                ! True when using Farquhar photosynthesis model
LOGICAL         :: LEAIRCO2COUP              ! True when using variable atmospheric CO2 in photosynthesis
LOGICAL         :: LFACO2BIOFLUX             ! True when rescaling CO2 biogenic fluxes based on opt.flux clim budget



! parameters Q10 function - McGuire et al. 1992
REAL(KIND=JPRB) :: RVQ10A                    ! 2.5665_JPRB  CTESSEL
REAL(KIND=JPRB) :: RVQ10B                    ! 0.05308_JPRB CTESSEL
REAL(KIND=JPRB) :: RVQ10C                    ! 0.00238_JPRB CTESSEL
REAL(KIND=JPRB) :: RVQ10D                    ! 0.00004_JPRB CTESSEL
REAL(KIND=JPRB) :: RVQ10MIN                  ! min value for Q10 when temperature is very high (>56deg): 0.0001

! New global parameters
REAL(KIND=JPRB) :: RVLAMSK_DESERT ! Unstable SKIN LAYER CONDUCT. DESERT
REAL(KIND=JPRB) :: RVLAMSK_SNOW   ! Unstable SKIN LAYER CONDUCT. SNOW
REAL(KIND=JPRB) :: RVLAMSKS_DESERT! Stable SKIN LAYER CONDUCT. DESERT
REAL(KIND=JPRB) :: RVLAMSKS_SNOW  ! Stable SKIN LAYER CONDUCT. SNOW
REAL(KIND=JPRB) :: RVZ0M_BARE     ! Roughness length for momentum - bare soil
REAL(KIND=JPRB) :: RVZ0M_SNOW     ! Roughness length for momentum - snow
REAL(KIND=JPRB) :: RVZ0H_BARE     ! Roughness length for heat - bare soil
REAL(KIND=JPRB) :: RVZ0H_SNOW     ! Roughness length for heat - snow

END TYPE TVEG

END MODULE YOS_VEG
