MODULE YOMGP1S0
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

!*    Grid point array at time t in the surface physics

REAL(KIND=JPRB),ALLOCATABLE,TARGET:: GP0(:,:)

! -   Splitting of the array GP0

!     TSLNU*  -  SOIL TEMPERATURE  
!     QLINU*  -  SOIL WETNESS
!     TILNU*  -  SEA-ICE TEMPERATURE  
!     FSNNU*  -  SNOW DEPTH
!     TSNNU* -  SNOW TEMPERATURE
!     WSNNU*  - SNOW WATER CONTENT
!     ASNNU* -  SNOW ALBEDO
!     RSNNU* -  SNOW DENSITY
!     TRENU*  -  SKIN TEMPERATURE
!     WRENU*  -  SKIN RESERVOIR WATER CONTENT

!     CTESSEL
!     LAINU*  -  LAI
!     BSTRNU* - ABOVE GROUND STRUCTURAL BIOMASS
!     BSTR2NU*- BELOW GROUND STRUCTURAL BIOMASS
!     CTESSEL

!* <ENDUTRA 
!     TLICENU* - LAKE ICE TEMPERATURE
!     TLMNWNU* - LAKE MEAN WATER TEMPERATURE
!     TLWMLNU* - LAKE MIXED LAYER TEMPERATURE
!     TLBOTNU* - LAKE BOTTOM TEMPERATURE 
!     TLSFNU*  - LAKE TEMPERATURE SHAPE FACTOR 
!     HLICENU* - LAKE ICE THICKNESS 
!     HLMLNU*  -  LAKE MIXED LAYER THICKNESS
!* ENDUTRA > 

!     UONU*     - VELOCITY OF OCEAN MIXED LAYER MODEL    !KPP
!     VONU*     - VELOCITY OF OCEAN MIXED LAYER MODEL    !KPP
!     TONU*     - TEMPERATURE OF OCEAN MIXED LAYER MODEL !KPP
!     SONU*     - SALINITY OF OCEAN MIXED LAYER MODEL    !KPP

!     FIRST DIMENSION: GRID POINTS
!     SECND DIMENSION: PROGN QUANTITY

REAL(KIND=JPRB),POINTER:: TSLNU0(:,:)
REAL(KIND=JPRB),POINTER:: QLINU0(:,:)
REAL(KIND=JPRB),POINTER:: TILNU0(:,:)
REAL(KIND=JPRB),POINTER:: FSNNU0(:,:)
REAL(KIND=JPRB),POINTER:: TSNNU0(:,:)
REAL(KIND=JPRB),POINTER:: WSNNU0(:,:)
REAL(KIND=JPRB),POINTER:: ASNNU0(:)
REAL(KIND=JPRB),POINTER:: RSNNU0(:,:)
REAL(KIND=JPRB),POINTER:: TRENU0(:)
REAL(KIND=JPRB),POINTER:: WRENU0(:)

REAL(KIND=JPRB),POINTER:: TLICENU0(:)
REAL(KIND=JPRB),POINTER:: TLMNWNU0(:)
REAL(KIND=JPRB),POINTER:: TLWMLNU0(:)
REAL(KIND=JPRB),POINTER:: TLBOTNU0(:)
REAL(KIND=JPRB),POINTER:: TLSFNU0(:)
REAL(KIND=JPRB),POINTER:: HLICENU0(:)
REAL(KIND=JPRB),POINTER:: HLMLNU0(:)

REAL(KIND=JPRB),POINTER:: UONU0(:,:)  !KPP
REAL(KIND=JPRB),POINTER:: VONU0(:,:)  !KPP
REAL(KIND=JPRB),POINTER:: TONU0(:,:)  !KPP
REAL(KIND=JPRB),POINTER:: SONU0(:,:)  !KPP
!REAL(KIND=JPRB),POINTER:: UONUC(:,:)  !KPP
!REAL(KIND=JPRB),POINTER:: VONUC(:,:)  !KPP
!REAL(KIND=JPRB),POINTER:: USTRCNU(:,:)  !KPP
!REAL(KIND=JPRB),POINTER:: VSTRNUC(:,:)  !KPP

REAL(KIND=JPRB),POINTER:: LAINU0(:,:)
REAL(KIND=JPRB),POINTER:: BSTRNU0(:,:)
REAL(KIND=JPRB),POINTER:: BSTR2NU0(:,:)



END MODULE YOMGP1S0
