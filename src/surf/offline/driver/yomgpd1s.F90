MODULE YOMGPD1S
! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE
SAVE

!*    Grid point array for one timelevel physics fields

! GPD    : generic for one timelevel physics fields

! VFMASK  - catchment mask
! VFZ0F   - roughness length for momentum (constant) (NB: units M)
! VFALBF  - surface shortwave albedo
! VFALUVP - MODIS ALBEDO UV-VIS PARALLEL (DIRECT) RADIATION
! VFALUVD - MODIS ALBEDO UV-VIS DIFFUSE RADIATION
! VFALNIP - MODIS ALBEDO NEAR IR PARALLEL (DIRECT) RADIATION
! VFALNID - MODIS ALBEDO NEAR IR DIFFUSE RADIATION
! VFAL[UV|NI][I|V|G] - 6-component MODIS albedo coefficients
! VFITM   - land-sea mask
! VFGEO   - surface geopotential
! VFZ0H   - roughness length for heat (constant)  (NB: units M)
! VFCVL   - low vegetation cover
! VFCVH   - high vegetation cover
! VFCUR   - urban cover (PASSIVE)
! VFTVL   - low vegetation type
! VFTVH   - high vegetation type
! VFLAIL  - low vegetation lai
! VFLAIH  - high vegetation lai
! VFCO2TYP- photosynthesis pathway (C3/C4) (only applicable to low vegetation types)
! VFRSML  - low  vegetation minimum stomatal resistance
! VFRSMH  - high vegetation minimum stomatal resistance
! VFCOVVT - vegetation coverage (Cveg) of each vegetation type
! VFR0VT  - reference respiration at 25 

! VFVAROR - orographic variance

! VFSOTY  - soil type
! VFSDOR  - orographic standard deviation
! VFSST   - Sea Surface temperature
! VFCI    - sea ice fraction
! VFCIL   - land ice fraction
! VFLDEPTH- LAKE DEPTH                                           !FLAKE
! VFCLAKE - LAKE FRACTION                                        !FLAKE
! VFZO    - vertical layer depth of ocean mixed layer model      !KPP
! VFHO    - vertical layer thickness of ocean mixed layer model  !KPP
! VFDO    - interface layer depth of ocean mixed layer model     !KPP
! VFOCDEPTH - ocean depth for ocean mixed layer model            !KPP
! VFADVT  - temperature advection correction term of KPP model   !KPP
! VFADVS  - salinity advection correction term of KPP model      !KPP
! VFTRI0  - trigonal matrix for diff. eq. of KPP model           !KPP
! VFTRI1  - trigonal matrix for diff. eq. of KPP model           !KPP
! VFHO_INV - 1/VFHO                                              !KPP 
! VFSWDK_SAVE - radiative coefficient for KPP model              !KPP
! VDLSP   - large scale precipitation             
! VDCP    - convective precipitation              
! VDSF    - snow fall                             
! VDSSHF  - surface sensible heat flux            
! VDSLHF  - surface latent heat flux              
! VDMSL   - mean sea level pressure               
! VDSSR   - surface solar radiation               
! VDSTR   - surface thermal radiation             
! VDEWSS  - U stress                 
! VDNSSS  - V stress
! VDE     - evaporation                            
! VDRO    - runoff                                 
! VDROS   - surface runoff                                 
! VDMLT   - snow melt                                 
! VDALB   - surface shortwave albedo as seen by model 
! VDIEWSS - instantaneous U stress                 
! VDINSSS - instantaneous V stress
! VDISSHF - instantaneous sensible heat flux
! VDIE    - instantaneous evaporation
! VDCSF   - convective snow fall
! VDLSSF  - large scale snow fall
! VDZ0F   - roughness length for momentum (varying over sea)  (NB: units M)
! VDZ0H   - roughness length for heat (varying over sea)  (NB: units M)
! VDSSRD  - surface solar radiation downwards               
! VDSTRD  - surface thermal radiation downwards            
! VDIEWSSTL- Instantaneous U stress for each tile                 
! VDINSSSTL- Intantaneous V stress for each tile
! VDISSHFTL- Instantaneous sensible heat flux for each tile
! VDIETL  - instantaneous evaporation for each tile
! VDTSKTL - skin temperature for each tile

! VDANDAYVT- daily net CO2 assimilation (sum) for each vegetation type
! VDANFMVT- daily maximum leaf assimilation (highest value) for each vegetation 
!           type
! VDRESPBSTR        - respiration of above ground structural biomass per VT
! VDRESPBSTR2       - respiration of below ground structural biomass per VT
! VDBIOMASS_LAST    - (active) leaf biomass of previous day per VT
! NB: value only after nitro_decline, not after laigain!!!!
! VDBLOSSVT- active biomass loss per VT
! VDBGAINVT- active biomass gain per VT

! VFPGLOB - global grid indices gaussian reduced

! GPD_SDP2 - Generic group for 2D calibrated surface spatially distributed parameters

! S2RVCOVH2D      - Cover vegetation for high veg.
! S2RVCOVL2D      - Cover vegetation for low veg.
! S2RVHSTRH2D     - Param. in humidity stress function high veg.
! S2RVHSTRL2D     - Param. in humidity stress function low veg.
! S2RVLAMSKH2D    - Unstable skin layer conduct. High veg.
! S2RVLAMSKL2D    - Unstable skin layer conduct. Low veg.
! S2RVLAMSKSH2D   - Stable skin layer conduct. High veg.
! S2RVLAMSKSL2D   - Stable skin layer conduct. Low veg.
! S2RVRSMINH2D    - Min. Stomatal resistance. High veg.
! S2RVRSMINL2D    - Min. Stomatal resistance. Low veg.
! S2RVZ0HH2D      - Roughness length of heat high veg.
! S2RVZ0HL2D      - Roughness length of heat low veg.
! S2RVZ0MH2D      - Roughness length of momentum high veg.
! S2RVZ0ML2D      - Roughness length of momentum low veg.
! S2RVAHL2D       - Coef. herbaceous water stress response low veg. 
! S2RVAMMAXH2D    - Leaf photosynthetic capacity high veg. 
! S2RVAMMAXL2D    - Leaf photosynthetic capacity low veg.
! S2RVBHL2D       - Coef. herbaceous water stress response low veg.
! S2RVCEH2D       - SLA sensitivity to nitrogen concentration high veg.
! S2RVCEL2D       - SLA sensitivity to nitrogen concentration low veg. 
! S2RVCFH2D       - Lethal min value of SLA high veg. 
! S2RVCFL2D       - Lethal min value of SLA low veg.
! S2RVCNAH2D      - Nitrogen concentration of active biomass high veg.
! S2RVCNAL2D      - Nitrogen concentration of active biomass low veg.
! S2RVDMAXH2D     - Max. especific humidity deficit tolerated by high veg. 
! S2RVDMAXL2D     - Max. especific humidity deficit tolerated by low veg.
! S2RVEPSOH2D     - Max. initial quantum use efficiency high veg. 
! S2RVEPSOL2D     - Max. initial quantum use efficiency low veg.
! S2RVF2IH2D      - Critical normalized swc for stress parameterisation high veg. 
! S2RVF2IL2D      - Critical normalized swc for stress parameterisation low veg. 
! S2RVFZEROSTH2D
! S2RVFZEROSTL2D
! S2RVGAMMH2D     - CO2 compensation concentration high veg. 
! S2RVGAMML2D     - CO2 compensation concentration low veg.  
! S2RVGCH2D       - Cuticular conductance high veg.
! S2RVGCL2D       - Cuticular conductance low veg.
! S2RVGMESH2D     - Mesophyll conductance high veg.
! S2RVGMESL2D     - Mesophyll conductance low veg.
! S2RVLAIMINH2D   - Min. LAI high veg.
! S2RVLAIMINL2D   - Min. LAI low veg. 
! S2RVQDAMMAXH2D  - Q10 function leaf photosynt. capacity high veg. 
! S2RVQDAMMAXL2D  - Q10 function leaf photosynt. capacity low veg.
! S2RVQDGAMMH2D   - Q10 funct. CO2 compensation concentration high veg.
! S2RVQDGAMML2D   - Q10 funct. CO2 compensation concentration low veg. 
! S2RVQDGMESH2D   - Q10 funct. mesophyll conductance high veg.
! S2RVQDGMESL2D   - Q10 funct. mesophyll conductance low veg.
! S2RVSEFOLDH2D   - E-folding time senescense high veg.
! S2RVSEFOLDL2D   - E-folding time senescense low veg.
! S2RVT1AMMAXH2D  - Min. ref. temp. response funct. leaf photosynt. capacity high veg.
! S2RVT1AMMAXL2D  - Min. ref. temp. response funct. leaf photosynt. capacity low veg.
! S2RVT1GMESH2D   - Min. ref. temp. response funct. mesophyll conductance high veg
! S2RVT1GMESL2D   - Min. ref. temp. response funct. mesophyll conductance low veg.
! S2RVT2AMMAXH2D  - Max. ref. temp. response funct. leaf photosynt. capacity high veg.
! S2RVT2AMMAXL2D  - Max. ref. temp. response funct. leaf photosynt. capacity low veg.
! S2RVT2GMESH2D   - Max. ref. temp. response funct. mesophyll conductance high veg.
! S2RVT2GMESL2D   - Max. ref. temp. response funct. mesophyll conductance low veg.
! S2RVTOPTH2D     - Optimum temp. compensation point high veg.
! S2RVTOPTL2D     - Optimum temp. compensation point low veg.
! S2RXBOMEGAMH2D  - Foliage scattering coeff. high veg.
! S2RXBOMEGAML2D  - Foliage scattering coeff. low veg.
! S2RVRSMINB2D    - Bare soil minimum resistance
! S2RVANMAXH2D    - derived SDP
! S2RVANMAXL2D    - derived SDP
! S2RVBSLAI_NITROH2D - derived SDP
! S2RVBSLAI_NITROL2D - derived SDP 

! GPD_SDP3 - Generic group for 3D calibrated surface spatially distributed parameters

! S3RCGDRYM3D    - Heat capacity of soil
! S3RLAMBDAM3D   - Lambda factor for van Genuchten
! S3RMVGALPHA3D  - Alpha in van Genuchten
! S3RNFACM3D     - N exponent in van Genuchten
! S3RWCONSM3D    - Water conductivity at saturation
! S3RWRESTM3D    - van Genuchten rwrst
! S3RWSATM3D     - Soil water content at saturation
! S3RVROOTSAH3D  - Percentage of roots in each soil layer high veg.
! S3RVROOTSAL3D  - Percentage of roots in each soil layer low veg.
! S3RLAMBDADRYM3D  - derived SDP
! S3RLAMSAT1M3D  - derived SDP
! S3RRCSOILM3D   - derived SDP
! S3RWCAPM3D     - derived SDP
! S3RWPWPM3D     - derived SDP


REAL(KIND=JPRB),ALLOCATABLE,TARGET:: GPD(:,:)

REAL(KIND=JPRB),POINTER:: VFMASK(:)
REAL(KIND=JPRB),POINTER:: VFZ0F(:)
REAL(KIND=JPRB),POINTER:: VFALBF(:)
REAL(KIND=JPRB),POINTER:: VFALUVP(:)
REAL(KIND=JPRB),POINTER:: VFALUVD(:)
REAL(KIND=JPRB),POINTER:: VFALNIP(:)
REAL(KIND=JPRB),POINTER:: VFALNID(:)
REAL(KIND=JPRB),POINTER:: VFALUVI(:)
REAL(KIND=JPRB),POINTER:: VFALUVV(:)
REAL(KIND=JPRB),POINTER:: VFALUVG(:)
REAL(KIND=JPRB),POINTER:: VFALNII(:)
REAL(KIND=JPRB),POINTER:: VFALNIV(:)
REAL(KIND=JPRB),POINTER:: VFALNIG(:)
REAL(KIND=JPRB),POINTER:: VFITM(:)
REAL(KIND=JPRB),POINTER:: VFGEO(:)
REAL(KIND=JPRB),POINTER:: VFZ0H(:)
REAL(KIND=JPRB),POINTER:: VFFWET(:)
REAL(KIND=JPRB),POINTER:: VFCVL(:)
REAL(KIND=JPRB),POINTER:: VFCVH(:)
REAL(KIND=JPRB),POINTER:: VFCUR(:)
REAL(KIND=JPRB),POINTER:: VFTVL(:)
REAL(KIND=JPRB),POINTER:: VFTVH(:)
REAL(KIND=JPRB),POINTER:: VFLAIL(:)
REAL(KIND=JPRB),POINTER:: VFLAIH(:)
REAL(KIND=JPRB),POINTER:: VFCO2TYP(:)
REAL(KIND=JPRB),POINTER:: VFRSML(:)
REAL(KIND=JPRB),POINTER:: VFRSMH(:)
REAL(KIND=JPRB),POINTER:: VFCVT(:,:)
REAL(KIND=JPRB),POINTER:: VFLAIVT(:,:)
REAL(KIND=JPRB),POINTER:: VFCOVVT(:,:)
REAL(KIND=JPRB),POINTER:: VFR0VT(:,:)


REAL(KIND=JPRB),POINTER:: VFSOTY(:)
REAL(KIND=JPRB),POINTER:: VFSDOR(:)
REAL(KIND=JPRB),POINTER:: VFSST(:)
REAL(KIND=JPRB),POINTER:: VFCI(:)
REAL(KIND=JPRB),POINTER:: VFCIL(:)
REAL(KIND=JPRB),POINTER:: VDLSP(:)
REAL(KIND=JPRB),POINTER:: VDCP(:)
REAL(KIND=JPRB),POINTER:: VDSF(:)
REAL(KIND=JPRB),POINTER:: VDSSHF(:)
REAL(KIND=JPRB),POINTER:: VDSLHF(:)
REAL(KIND=JPRB),POINTER:: VDSSR(:)
REAL(KIND=JPRB),POINTER:: VDSTR(:)
REAL(KIND=JPRB),POINTER:: VDEWSS(:)
REAL(KIND=JPRB),POINTER:: VDNSSS(:)
REAL(KIND=JPRB),POINTER:: VDE(:)
REAL(KIND=JPRB),POINTER:: VDRO(:)
REAL(KIND=JPRB),POINTER:: VDROS(:)
REAL(KIND=JPRB),POINTER:: VDMLT(:)
REAL(KIND=JPRB),POINTER:: VDALB(:)
REAL(KIND=JPRB),POINTER:: VDIEWSS(:)
REAL(KIND=JPRB),POINTER:: VDINSSS(:)
REAL(KIND=JPRB),POINTER:: VDISSHF(:)
REAL(KIND=JPRB),POINTER:: VDIE(:)
REAL(KIND=JPRB),POINTER:: VDCSF(:)
REAL(KIND=JPRB),POINTER:: VDLSSF(:)
REAL(KIND=JPRB),POINTER:: VDZ0F(:)
REAL(KIND=JPRB),POINTER:: VDZ0H(:)
REAL(KIND=JPRB),POINTER:: VDSSRD(:)
REAL(KIND=JPRB),POINTER:: VDSTRD(:)
REAL(KIND=JPRB),POINTER:: VDIEWSSTL(:,:)
REAL(KIND=JPRB),POINTER:: VDINSSSTL(:,:)
REAL(KIND=JPRB),POINTER:: VDISSHFTL(:,:)
REAL(KIND=JPRB),POINTER:: VDIETL(:,:)
REAL(KIND=JPRB),POINTER:: VDTSKTL(:,:)

REAL(KIND=JPRB),POINTER:: VFLDEPTH(:)       !FLAKE
REAL(KIND=JPRB),POINTER:: VFCLAKE(:)        !FLAKE 
REAL(KIND=JPRB),POINTER:: VFCLAKEF(:)        !FLAKE + Flood

REAL(KIND=JPRB),POINTER:: VFZO(:,:)         !KPP
REAL(KIND=JPRB),POINTER:: VFHO(:,:)         !KPP
REAL(KIND=JPRB),POINTER:: VFDO(:,:)         !KPP
REAL(KIND=JPRB),POINTER:: VFOCDEPTH(:)      !KPP
REAL(KIND=JPRB),POINTER:: VFADVT(:,:)       !KPP
REAL(KIND=JPRB),POINTER:: VFADVS(:,:)       !KPP
REAL(KIND=JPRB),POINTER:: VFTRI0(:,:)       !KPP
REAL(KIND=JPRB),POINTER:: VFTRI1(:,:)       !KPP
REAL(KIND=JPRB),POINTER:: VFHO_INV(:,:)     !KPP
REAL(KIND=JPRB),POINTER:: VFSWDK_SAVE(:,:)  !KPP

REAL(KIND=JPRB),POINTER:: VDANDAYVT(:,:)
REAL(KIND=JPRB),POINTER:: VDANFMVT(:,:)
REAL(KIND=JPRB),POINTER:: VDRESPBSTR(:,:)
REAL(KIND=JPRB),POINTER:: VDRESPBSTR2(:,:)
REAL(KIND=JPRB),POINTER:: VDBIOMASS_LAST(:,:)
REAL(KIND=JPRB),POINTER:: VDBLOSSVT(:,:)
REAL(KIND=JPRB),POINTER:: VDBGAINVT(:,:)

REAL(KIND=JPRB),POINTER:: VFPGLOB(:) 

! Calibrated 2D spatially distributed parameters
REAL(KIND=JPRB),ALLOCATABLE,TARGET:: GPD_SDP2(:,:)
REAL(KIND=JPRB),POINTER :: S2RVCOVH2D(:)
REAL(KIND=JPRB),POINTER :: S2RVCOVL2D(:)
REAL(KIND=JPRB),POINTER :: S2RVHSTRH2D(:)
REAL(KIND=JPRB),POINTER :: S2RVHSTRL2D(:)
REAL(KIND=JPRB),POINTER :: S2RVLAMSKH2D(:)
REAL(KIND=JPRB),POINTER :: S2RVLAMSKL2D(:)
REAL(KIND=JPRB),POINTER :: S2RVLAMSKSH2D(:)
REAL(KIND=JPRB),POINTER :: S2RVLAMSKSL2D(:)
REAL(KIND=JPRB),POINTER :: S2RVRSMINH2D(:)
REAL(KIND=JPRB),POINTER :: S2RVRSMINL2D(:)
REAL(KIND=JPRB),POINTER :: S2RVZ0HH2D(:)
REAL(KIND=JPRB),POINTER :: S2RVZ0HL2D(:)
REAL(KIND=JPRB),POINTER :: S2RVZ0MH2D(:)
REAL(KIND=JPRB),POINTER :: S2RVZ0ML2D(:)
REAL(KIND=JPRB),POINTER :: S2RVAHL2D(:)      
REAL(KIND=JPRB),POINTER :: S2RVAMMAXH2D(:)   
REAL(KIND=JPRB),POINTER :: S2RVAMMAXL2D(:)  
REAL(KIND=JPRB),POINTER :: S2RVBHL2D(:)     
REAL(KIND=JPRB),POINTER :: S2RVCEH2D(:)     
REAL(KIND=JPRB),POINTER :: S2RVCEL2D(:)      
REAL(KIND=JPRB),POINTER :: S2RVCFH2D(:)      
REAL(KIND=JPRB),POINTER :: S2RVCFL2D(:)     
REAL(KIND=JPRB),POINTER :: S2RVCNAH2D(:)    
REAL(KIND=JPRB),POINTER :: S2RVCNAL2D(:)    
REAL(KIND=JPRB),POINTER :: S2RVDMAXH2D(:)    
REAL(KIND=JPRB),POINTER :: S2RVDMAXL2D(:)   
REAL(KIND=JPRB),POINTER :: S2RVEPSOH2D(:)    
REAL(KIND=JPRB),POINTER :: S2RVEPSOL2D(:)   
REAL(KIND=JPRB),POINTER :: S2RVF2IH2D(:)     
REAL(KIND=JPRB),POINTER :: S2RVF2IL2D(:)     
REAL(KIND=JPRB),POINTER :: S2RVFZEROSTH2D(:)
REAL(KIND=JPRB),POINTER :: S2RVFZEROSTL2D(:)
REAL(KIND=JPRB),POINTER :: S2RVGAMMH2D(:)    
REAL(KIND=JPRB),POINTER :: S2RVGAMML2D(:)     
REAL(KIND=JPRB),POINTER :: S2RVGCH2D(:)     
REAL(KIND=JPRB),POINTER :: S2RVGCL2D(:)     
REAL(KIND=JPRB),POINTER :: S2RVGMESH2D(:)   
REAL(KIND=JPRB),POINTER :: S2RVGMESL2D(:)   
REAL(KIND=JPRB),POINTER :: S2RVLAIMINH2D(:) 
REAL(KIND=JPRB),POINTER :: S2RVLAIMINL2D(:)  
REAL(KIND=JPRB),POINTER :: S2RVQDAMMAXH2D(:) 
REAL(KIND=JPRB),POINTER :: S2RVQDAMMAXL2D(:)
REAL(KIND=JPRB),POINTER :: S2RVQDGAMMH2D(:) 
REAL(KIND=JPRB),POINTER :: S2RVQDGAMML2D(:)  
REAL(KIND=JPRB),POINTER :: S2RVQDGMESH2D(:) 
REAL(KIND=JPRB),POINTER :: S2RVQDGMESL2D(:) 
REAL(KIND=JPRB),POINTER :: S2RVSEFOLDH2D(:) 
REAL(KIND=JPRB),POINTER :: S2RVSEFOLDL2D(:) 
REAL(KIND=JPRB),POINTER :: S2RVT1AMMAXH2D(:)
REAL(KIND=JPRB),POINTER :: S2RVT1AMMAXL2D(:)
REAL(KIND=JPRB),POINTER :: S2RVT1GMESH2D(:) 
REAL(KIND=JPRB),POINTER :: S2RVT1GMESL2D(:) 
REAL(KIND=JPRB),POINTER :: S2RVT2AMMAXH2D(:)
REAL(KIND=JPRB),POINTER :: S2RVT2AMMAXL2D(:)
REAL(KIND=JPRB),POINTER :: S2RVT2GMESH2D(:) 
REAL(KIND=JPRB),POINTER :: S2RVT2GMESL2D(:) 
REAL(KIND=JPRB),POINTER :: S2RVTOPTH2D(:)   
REAL(KIND=JPRB),POINTER :: S2RVTOPTL2D(:)   
REAL(KIND=JPRB),POINTER :: S2RXBOMEGAMH2D(:)
REAL(KIND=JPRB),POINTER :: S2RXBOMEGAML2D(:)
REAL(KIND=JPRB),POINTER :: S2RVRSMINB2D(:)  
REAL(KIND=JPRB),POINTER :: S2RVANMAXH2D(:)  ! derived SDP
REAL(KIND=JPRB),POINTER :: S2RVANMAXL2D(:)  ! derived SDP
REAL(KIND=JPRB),POINTER :: S2RVBSLAI_NITROH2D(:) ! derived SDP
REAL(KIND=JPRB),POINTER :: S2RVBSLAI_NITROL2D(:) ! derived SDP 

! Calibrated 3D spatially distributed parameters
REAL(KIND=JPRB),ALLOCATABLE,TARGET:: GPD_SDP3(:,:,:)
REAL(KIND=JPRB),POINTER :: S3RCGDRYM3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RLAMBDAM3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RMVGALPHA3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RNFACM3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RWCONSM3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RWRESTM3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RWSATM3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RVROOTSAH3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RVROOTSAL3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RLAMBDADRYM3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RLAMSAT1M3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RRCSOILM3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RWCAPM3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RWPWPM3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RMFACM3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RDMAXM3D(:,:)
REAL(KIND=JPRB),POINTER :: S3RDMINM3D(:,:)
   
END MODULE YOMGPD1S
