MODULE YOS_AGS
USE PARKIND1  ,ONLY : JPIM     ,JPRB, JPRD

IMPLICIT NONE
SAVE
! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     -----------------------------------------------------------------
!*    ** *AGS* - 
!     -----------------------------------------------------------------

TYPE :: TAGS
REAL(KIND=JPRB) :: RCO2
REAL(KIND=JPRB) :: RCO2FRAC
REAL(KIND=JPRB) :: RMAIR
REAL(KIND=JPRB) :: RMH2O
REAL(KIND=JPRB) :: RMCO2
REAL(KIND=JPRB) :: RMC
REAL(KIND=JPRB) :: RDMAX_AGS
REAL(KIND=JPRB) :: RPARCF
REAL(KIND=JPRB) :: RRACCF
REAL(KIND=JPRB) :: RPCCO2
REAL(KIND=JPRB) :: RIAOPT
REAL(KIND=JPRB) :: RDSPOPT
REAL(KIND=JPRB) :: RXGT
REAL(KIND=JPRB) :: RDIFRACF
REAL(KIND=JPRB) :: RXBOMEGA
REAL(KIND=JPRB) :: RRDCF
REAL(KIND=JPRB) :: RAMMIN
REAL(KIND=JPRB) :: RCONDCTMIN
REAL(KIND=JPRB) :: RCONDSTMIN
REAL(KIND=JPRB) :: RANFMINIT
REAL(KIND=JPRB) :: RAIRTOH2O
REAL(KIND=JPRB) :: RCO2TOH2O
REAL(KIND=JPRB) :: RAW
REAL(KIND=JPRB) :: RASW
REAL(KIND=JPRB) :: RBW
REAL(KIND=JPRB) :: RDMAXN
REAL(KIND=JPRB) :: RDMAXX
REAL(KIND=JPRB) :: RRESPFACTOR_NIT
REAL(KIND=JPRB) :: RCNS_NIT
REAL(KIND=JPRB) :: RCA_1x_CO2_NIT
REAL(KIND=JPRB) :: RCA_2x_CO2_NIT
REAL(KIND=JPRB) :: RCC_NIT

REAL(KIND=JPRB) :: RABC(3)
REAL(KIND=JPRB) :: RPOI(3) 

REAL(KIND=JPRB) :: RQ10 !Q10 value in respiration parameterization
REAL(KIND=JPRB) :: RTAULIM !percentage of limitation of efolding time

! Parameters depending on latitudinal band (North/Tropics/South)
REAL(KIND=JPRB) :: RVCH4S(3)

! shape parameter temperature response functions for Am,max and gmes
! see Sellers et al 1996
REAL(KIND=JPRB) :: RSHP1AMMAX
REAL(KIND=JPRB) :: RSHP2AMMAX
REAL(KIND=JPRB) :: RSHP1GMES
REAL(KIND=JPRB) :: RSHP2GMES

! max f zero
! see documentation (8.101) and collatz 2004
REAL(KIND=JPRB) :: RMAXFZERO

! LAIB_NITRO function coefficients
REAL(KIND=JPRB) :: RLAIB_NITROA
REAL(KIND=JPRB) :: RLAIB_NITROB
REAL(KIND=JPRB) :: RLAIB_NITROMIN

! other new:
REAL(KIND=JPRB) :: RCC_NIT_BASE_DIL
REAL(KIND=JPRB) :: RVGMES_FLDEXP
REAL(KIND=JPRB) :: RESPBSTR_COEF
REAL(KIND=JPRD) :: REPS_COEF
REAL(KIND=JPRD) :: RGSC_COEF

! Parameters depending on photosynthesis mechanism (C3 or c4)

REAL(KIND=JPRB),ALLOCATABLE :: RVTOPT(:)     
REAL(KIND=JPRB),ALLOCATABLE :: RVFZERO(:) 
REAL(KIND=JPRB),ALLOCATABLE :: RVFZEROST(:)     
REAL(KIND=JPRB),ALLOCATABLE :: RVEPSO(:)    
REAL(KIND=JPRB),ALLOCATABLE :: RVGAMM(:)       
REAL(KIND=JPRB),ALLOCATABLE :: RVQDGAMM(:)      
REAL(KIND=JPRB),ALLOCATABLE :: RVQDGMES(:)      
REAL(KIND=JPRB),ALLOCATABLE :: RVT1GMES(:)       
REAL(KIND=JPRB),ALLOCATABLE :: RVT2GMES(:) 
REAL(KIND=JPRB),ALLOCATABLE :: RVAMMAX(:)         
REAL(KIND=JPRB),ALLOCATABLE :: RVQDAMMAX(:)      
REAL(KIND=JPRB),ALLOCATABLE :: RVT1AMMAX(:)
REAL(KIND=JPRB),ALLOCATABLE :: RVT2AMMAX(:)      
REAL(KIND=JPRB),ALLOCATABLE :: RVAH(:)         
REAL(KIND=JPRB),ALLOCATABLE :: RVBH(:)        

! Parameters depending on vegetation type

LOGICAL,ALLOCATABLE :: LVSTRESS(:)      
 
REAL(KIND=JPRB),ALLOCATABLE :: RVBSLAI(:)        
REAL(KIND=JPRB),ALLOCATABLE :: RVLAIMIN(:)      
REAL(KIND=JPRB),ALLOCATABLE :: RVSEFOLD(:)       
REAL(KIND=JPRB),ALLOCATABLE :: RVGMES(:)       
REAL(KIND=JPRB),ALLOCATABLE :: RVGC(:)           
REAL(KIND=JPRB),ALLOCATABLE :: RVDMAX(:)         
REAL(KIND=JPRB),ALLOCATABLE :: RVF2I(:)
REAL(KIND=JPRB),ALLOCATABLE :: RVCE(:)         
REAL(KIND=JPRB),ALLOCATABLE :: RVCF(:)         
REAL(KIND=JPRB),ALLOCATABLE :: RVCNA(:)
REAL(KIND=JPRB),ALLOCATABLE :: RVBSLAI_NITRO(:)
REAL(KIND=JPRB),ALLOCATABLE :: RXBOMEGAM(:)

REAL(KIND=JPRB),ALLOCATABLE :: RVR0VT(:,:) !Reference respiration tabulated by vegetation type
REAL(KIND=JPRB),ALLOCATABLE :: RVCH4QVT(:) !CH4 Q10 temperature dependency by vegetation type

!calculated:
REAL(KIND=JPRB),ALLOCATABLE :: RVANMAX(:)      

CONTAINS

PROCEDURE :: UPDATE_DEVICE => TAGS_UPDATE_DEVICE
PROCEDURE :: WIPE_DEVICE => TAGS_WIPE_DEVICE
END TYPE TAGS

! NAME               TYPE     DESCRIPTION
! ----               ----     -----------
! *RCO2*             REAL     atmospheric CO2 concentration (kgCO2 kgAir-1)
! *RMAIR*            REAL     molecular mass of air (kg mol-1)
! *RMH2O*            REAL     molecular mass of water (kg mol-1)
! *RMCO2*            REAL     molecular mass of CO2 (kg mol-1)
! *RMC*              REAL     molecular mass of C (kg mol-1)
! *RDMAX_AGS*        REAL     maximum specific humidity deficit tolerated by
!                             vegetation (kg kg-1)
!                             for AGS and LAI
! *RPARCF*           REAL     coefficient: PAR fraction of incoming solar 
!                             radiation (0-1)
! *RRACCF*           REAL     factor for aerodynamic resistance for CO2
! *RPCCO2*           REAL     proportion of Carbon in dry plant biomass (0-1)
! *RIAOPT*           REAL     optimum value for absorbed global radiation 
!                             (W m-2)
! *RDSPOPT*          REAL     optimum value for specific humidity deficit 
!                             (kg kg-1)
! *RXGT*             REAL     distribution of leaves
! *RDIFRACF*         REAL     coefficient used in computation of fraction 
!                             of diffusion
! *RXBOMEGA*         REAL     foliage scattering coefficient
! *RXBOMEGAM         REAL     foliage scattering coefficient depending on vegetation type
! *RRDCF*            REAL     dark respiration factor/coefficient
! *RAMMIN*           REAL     minimum criteria for maximum net assimilation 
!                             (kgCO2 kgAir-1 m s-1)
! *RCONDCTMIN*       REAL     minimum canopy conductance (m s-1)
! *RCONDSTMIN*       REAL     minimum stomatal conductance for CO2 (m s-1)
! *RANFMINIT*        REAL     initial maximum leaf assimilation (kgCO2 m2 s-1)
! *RAIRTOH2O*        REAL     ratio of molecular masses of air and H2O
! *RCO2TOH2O*        REAL     ratio of the binary diffusivities of CO2 and H2O 
!                             in air
! *RAW*              REAL     coefficient for stress response of woody species 
! *RASW*             REAL     coefficient for stress response of woody species 
! *RBW*              REAL     coefficient for stress response of woody species 
! *RDMAXN*           REAL     minimum air deficit stress parameters for
!                             herbaceous species (kg kg-1)
! *RDMAXX*           REAL     maximum air deficit stress parameters for
!                             herbaceous species (kg kg-1)
! *RRESPFACTOR_NIT*  REAL     maintenance respiration rate (% per day)
!                             of structural biomass (Fauri�, 1994) (s-1)
! *RCNS_NIT*         REAL     Nitrogen concentration of structural biomass (0-1)
! *RCA_1x_CO2_NIT*   REAL     rate of nitrogen dilution of above-ground biomass
!                             at ambiant (1x) [CO2] (Calvet and Soussana 2001)
! *RCA_2x_CO2_NIT*   REAL     rate of nitrogen dilution of above-ground biomass
!                             at doubled (2x) [CO2] (Calvet and Soussana 2001)
! *RCC_NIT*          REAL     proportion of active biomass for 1t ha-1 of total
!                             above-ground biomass (0-1)

! *RABC       REAL     abscissa needed for integration of net assimilation and 
!                      stomatal conductance over canopy depth (-)
! *RPOI*      REAL     Gaussian weights for integration of net assimilation and 
!                      stomatal conductance over canopy depth (-)

! *RVTOPT*    REAL     optimum temperature for evaluating compensation point (C)
! *RVFZERO*   REAL     ideal value of f (-) (no photorespiration or specific 
!                      humidity deficit)
!                      for AGS and LAI
! *RVFZERO*   REAL     ideal value of f (-) (no photorespiration or specific
!                      humidity deficit)
!                      for AST, LST and NIT 
! *RVEPSO*    REAL     maximum initial quantum use efficiency 
!                      (kgCO2 J-1 PAR m3 kgAir-1)
! *RVGAMM*    REAL     CO2 compensation concentration (kgCO2 kgAir-1)
! *RVQDGAMM*  REAL     Q10 function for CO2 compensation concentration (-)
!  RVQDGMES   REAL     Q10 function for mesophyll conductance (-)
!  RVT1GMES   REAL     minimum reference temperature for computing temperature 
!                      response function for mesophyll conductance (C)
!  RVT2GMES   REAL     maximum reference temperature for computing temperature 
!                      response function for mesophyll conductance (C)
!  RVAMMAX    REAL     leaf photosynthetic capacity (kgCO2 kgAir-1 m s-1)
!  RVQDAMMAX  REAL     Q10 function for leaf photosynthetic capacity (-)
!  RVT1AMMAX  REAL     minimum reference temperature for computing temperature
!                      response function for leaf photosynthetic capacity (C)
!  RVT2AMMAX  REAL     maximum reference temperature for computing temperature
!                      response function for leaf photosynthetic capacity (C)
!  RVAH       REAL     coefficient for herbaceous water stress response 
!  RVBH       REAL     coefficient for herbaceous water stress response 

!  LVSTRESS   LOGICAL  vegetation response type to water stress 
!                      (true:defensive false:offensive) 
!  RVBSLAI    REAL     ratio d(biomass)/d(lai) (kg m-2)
!  RVLAIMIN   REAL     minimum LAI (Leaf Area Index) (m2 m-2)
!  RVSEFOLD   REAL     e-folding time for senescence (s)
!  RVGMES     REAL     mesophyll conductance (m s-1)
!  RVGC       REAL     cuticular conductance (m s-1)
!  RVDMAX     REAL     maximum specific humidity deficit tolerated by 
!                      vegetation (kg kg-1)
!                      for AST, LST and NIT
!  RVF2I      REAL     critical normilized soil water content for stress 
!                      parameterisation (m3 m-3)
!  RVCE       REAL     specific leaf area (SLA) sensitivity to nitrogen 
!                      concentration (m2 kg-1 %-1)
!  RVCF       REAL     lethal minimum value of SLA (m2 kg-1)
!  RVCNA      REAL     nitrogen concentration of active biomass (=leaf biomass)
!                      (%: 0-1)
!  RVBSLAI_NITRO REAL  ratio d(biomass)/d(lai) from nitrogen decline theory 
!                      (kg m-2)

!  RVANMAX    REAL     maximum photosynthesis rate (kgCO2 kgAir-1 m s-1) 
!  RVR0VT     REAL     Reference respiration tabulated by vegetation type and C4 type
!     -----------------------------------------------------------------
CONTAINS

SUBROUTINE TAGS_UPDATE_DEVICE(SELF, LCREATED)
  CLASS(TAGS) :: SELF
  LOGICAL, OPTIONAL, INTENT(IN) :: LCREATED
  LOGICAL :: LLCREATED

  LLCREATED = .FALSE.
  IF(PRESENT(LCREATED)) LLCREATED = LCREATED
  IF(.NOT. LLCREATED)THEN
    !$acc enter data create(SELF)
    !$acc update device(SELF)
  ENDIF

  !$acc enter data create(SELF%RVTOPT)
  !$acc enter data create(SELF%RVFZERO)
  !$acc enter data create(SELF%RVFZEROST)
  !$acc enter data create(SELF%RVEPSO)
  !$acc enter data create(SELF%RVGAMM)
  !$acc enter data create(SELF%RVQDGAMM)
  !$acc enter data create(SELF%RVQDGMES)
  !$acc enter data create(SELF%RVT1GMES)
  !$acc enter data create(SELF%RVT2GMES)
  !$acc enter data create(SELF%RVAMMAX)
  !$acc enter data create(SELF%RVQDAMMAX)
  !$acc enter data create(SELF%RVT1AMMAX)
  !$acc enter data create(SELF%RVT2AMMAX)
  !$acc enter data create(SELF%RVAH)
  !$acc enter data create(SELF%RVBH)
  !$acc enter data create(SELF%LVSTRESS)
  !$acc enter data create(SELF%RVBSLAI)
  !$acc enter data create(SELF%RVLAIMIN)
  !$acc enter data create(SELF%RVSEFOLD)
  !$acc enter data create(SELF%RVGMES)
  !$acc enter data create(SELF%RVGC)
  !$acc enter data create(SELF%RVDMAX)
  !$acc enter data create(SELF%RVF2I)
  !$acc enter data create(SELF%RVCE)
  !$acc enter data create(SELF%RVCF)
  !$acc enter data create(SELF%RVCNA)
  !$acc enter data create(SELF%RVBSLAI_NITRO)
  !$acc enter data create(SELF%RXBOMEGAM)
  !$acc enter data create(SELF%RVR0VT)
  !$acc enter data create(SELF%RVCH4QVT)
  !$acc enter data create(SELF%RVANMAX)

  !$acc update device(SELF%RVTOPT)
  !$acc update device(SELF%RVFZERO)
  !$acc update device(SELF%RVFZEROST)
  !$acc update device(SELF%RVEPSO)
  !$acc update device(SELF%RVGAMM)
  !$acc update device(SELF%RVQDGAMM)
  !$acc update device(SELF%RVQDGMES)
  !$acc update device(SELF%RVT1GMES)
  !$acc update device(SELF%RVT2GMES)
  !$acc update device(SELF%RVAMMAX)
  !$acc update device(SELF%RVQDAMMAX)
  !$acc update device(SELF%RVT1AMMAX)
  !$acc update device(SELF%RVT2AMMAX)
  !$acc update device(SELF%RVAH)
  !$acc update device(SELF%RVBH)
  !$acc update device(SELF%LVSTRESS)
  !$acc update device(SELF%RVBSLAI)
  !$acc update device(SELF%RVLAIMIN)
  !$acc update device(SELF%RVSEFOLD)
  !$acc update device(SELF%RVGMES)
  !$acc update device(SELF%RVGC)
  !$acc update device(SELF%RVDMAX)
  !$acc update device(SELF%RVF2I)
  !$acc update device(SELF%RVCE)
  !$acc update device(SELF%RVCF)
  !$acc update device(SELF%RVCNA)
  !$acc update device(SELF%RVBSLAI_NITRO)
  !$acc update device(SELF%RXBOMEGAM)
  !$acc update device(SELF%RVR0VT)
  !$acc update device(SELF%RVCH4QVT)
  !$acc update device(SELF%RVANMAX)

  !$acc enter data attach(SELF%RVTOPT)
  !$acc enter data attach(SELF%RVFZERO)
  !$acc enter data attach(SELF%RVFZEROST)
  !$acc enter data attach(SELF%RVEPSO)
  !$acc enter data attach(SELF%RVGAMM)
  !$acc enter data attach(SELF%RVQDGAMM)
  !$acc enter data attach(SELF%RVQDGMES)
  !$acc enter data attach(SELF%RVT1GMES)
  !$acc enter data attach(SELF%RVT2GMES)
  !$acc enter data attach(SELF%RVAMMAX)
  !$acc enter data attach(SELF%RVQDAMMAX)
  !$acc enter data attach(SELF%RVT1AMMAX)
  !$acc enter data attach(SELF%RVT2AMMAX)
  !$acc enter data attach(SELF%RVAH)
  !$acc enter data attach(SELF%RVBH)
  !$acc enter data attach(SELF%LVSTRESS)
  !$acc enter data attach(SELF%RVBSLAI)
  !$acc enter data attach(SELF%RVLAIMIN)
  !$acc enter data attach(SELF%RVSEFOLD)
  !$acc enter data attach(SELF%RVGMES)
  !$acc enter data attach(SELF%RVGC)
  !$acc enter data attach(SELF%RVDMAX)
  !$acc enter data attach(SELF%RVF2I)
  !$acc enter data attach(SELF%RVCE)
  !$acc enter data attach(SELF%RVCF)
  !$acc enter data attach(SELF%RVCNA)
  !$acc enter data attach(SELF%RVBSLAI_NITRO)
  !$acc enter data attach(SELF%RXBOMEGAM)
  !$acc enter data attach(SELF%RVR0VT)
  !$acc enter data attach(SELF%RVCH4QVT)
  !$acc enter data attach(SELF%RVANMAX)

END SUBROUTINE TAGS_UPDATE_DEVICE

SUBROUTINE TAGS_WIPE_DEVICE(SELF, LDELETED)
  CLASS(TAGS) :: SELF
  LOGICAL, OPTIONAL, INTENT(IN) :: LDELETED
  LOGICAL :: LLDELETED

  LLDELETED = .FALSE.
  IF(PRESENT(LDELETED)) LLDELETED = LDELETED

  !$acc exit data detach(SELF%RVTOPT) finalize
  !$acc exit data detach(SELF%RVFZERO) finalize
  !$acc exit data detach(SELF%RVFZEROST) finalize
  !$acc exit data detach(SELF%RVEPSO) finalize
  !$acc exit data detach(SELF%RVGAMM) finalize
  !$acc exit data detach(SELF%RVQDGAMM) finalize
  !$acc exit data detach(SELF%RVQDGMES) finalize
  !$acc exit data detach(SELF%RVT1GMES) finalize
  !$acc exit data detach(SELF%RVT2GMES) finalize
  !$acc exit data detach(SELF%RVAMMAX) finalize
  !$acc exit data detach(SELF%RVQDAMMAX) finalize
  !$acc exit data detach(SELF%RVT1AMMAX) finalize
  !$acc exit data detach(SELF%RVT2AMMAX) finalize
  !$acc exit data detach(SELF%RVAH) finalize
  !$acc exit data detach(SELF%RVBH) finalize
  !$acc exit data detach(SELF%LVSTRESS) finalize
  !$acc exit data detach(SELF%RVBSLAI) finalize
  !$acc exit data detach(SELF%RVLAIMIN) finalize
  !$acc exit data detach(SELF%RVSEFOLD) finalize
  !$acc exit data detach(SELF%RVGMES) finalize
  !$acc exit data detach(SELF%RVGC) finalize
  !$acc exit data detach(SELF%RVDMAX) finalize
  !$acc exit data detach(SELF%RVF2I) finalize
  !$acc exit data detach(SELF%RVCE) finalize
  !$acc exit data detach(SELF%RVCF) finalize
  !$acc exit data detach(SELF%RVCNA) finalize
  !$acc exit data detach(SELF%RVBSLAI_NITRO) finalize
  !$acc exit data detach(SELF%RXBOMEGAM) finalize
  !$acc exit data detach(SELF%RVR0VT) finalize
  !$acc exit data detach(SELF%RVCH4QVT) finalize
  !$acc exit data detach(SELF%RVANMAX) finalize

  !$acc exit data delete(SELF%RVTOPT) finalize
  !$acc exit data delete(SELF%RVFZERO) finalize
  !$acc exit data delete(SELF%RVFZEROST) finalize
  !$acc exit data delete(SELF%RVEPSO) finalize
  !$acc exit data delete(SELF%RVGAMM) finalize
  !$acc exit data delete(SELF%RVQDGAMM) finalize
  !$acc exit data delete(SELF%RVQDGMES) finalize
  !$acc exit data delete(SELF%RVT1GMES) finalize
  !$acc exit data delete(SELF%RVT2GMES) finalize
  !$acc exit data delete(SELF%RVAMMAX) finalize
  !$acc exit data delete(SELF%RVQDAMMAX) finalize
  !$acc exit data delete(SELF%RVT1AMMAX) finalize
  !$acc exit data delete(SELF%RVT2AMMAX) finalize
  !$acc exit data delete(SELF%RVAH) finalize
  !$acc exit data delete(SELF%RVBH) finalize
  !$acc exit data delete(SELF%LVSTRESS) finalize
  !$acc exit data delete(SELF%RVBSLAI) finalize
  !$acc exit data delete(SELF%RVLAIMIN) finalize
  !$acc exit data delete(SELF%RVSEFOLD) finalize
  !$acc exit data delete(SELF%RVGMES) finalize
  !$acc exit data delete(SELF%RVGC) finalize
  !$acc exit data delete(SELF%RVDMAX) finalize
  !$acc exit data delete(SELF%RVF2I) finalize
  !$acc exit data delete(SELF%RVCE) finalize
  !$acc exit data delete(SELF%RVCF) finalize
  !$acc exit data delete(SELF%RVCNA) finalize
  !$acc exit data delete(SELF%RVBSLAI_NITRO) finalize
  !$acc exit data delete(SELF%RXBOMEGAM) finalize
  !$acc exit data delete(SELF%RVR0VT) finalize
  !$acc exit data delete(SELF%RVCH4QVT) finalize
  !$acc exit data delete(SELF%RVANMAX) finalize

  IF(.NOT. LLDELETED)THEN
     !$acc exit data delete(SELF) finalize
  ENDIF

END SUBROUTINE TAGS_WIPE_DEVICE

END MODULE YOS_AGS
