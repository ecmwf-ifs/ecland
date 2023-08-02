INTERFACE
SUBROUTINE SURFBC    (YDSURF,KIDIA,KFDIA,KLON,KTILES,KLEVSN,&
 & PSSDP2, PTVL   , PCO2TYP,PTVH   ,PSOTY  ,PSDOR,PCVLC  ,PCVHC, PCURC, &
 & PLAILC  ,PLAIHC, PLAILI, PLAIHI,&
 & PLAILCP, PLAIHCP, PAVGPARC, &
 & PLSM   ,PCI    ,PCIL, PCLAKE ,PHLICE,&
 & PGEMU  ,PSNM1M ,PWLM1M ,PRSNM1M,LESNICE, &  
 & LDLAND ,LDSICE , LDLICE, LDLAKE ,LDNH, LDOCN_KPP,&
 & KTVL   ,KCO2TYP, KTVH   ,KSOTY,&
 & PCVL   ,PCVH, PCUR, PLAIL, PLAIH, PLAILP, PLAIHP, PAVGPAR,&
 & PWLMX  ,PFRTI, PCSN)

! (C) Copyright 1999- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!     ------------------------------------------------------------------
!**   *SURFBC* - CREATES BOUNDARY CONDITIONS CHARACTERIZING THE SURFACE

!     PURPOSE
!     -------
!     CREATES AUXILIARY FIELDS NEEDED BY VDFMAIN, SURFTSTP, AND SURFRAD

!     INTERFACE
!     ---------
!     *SURFBC* IS CALLED BY *CALLPAR* AND *RADPAR*

!     INPUT PARAMETERS (INTEGER):
!     *KIDIA*        START POINT
!     *KFDIA*        END POINT
!     *KLON*         NUMBER OF GRID POINTS PER PACKET
!     *KTILES*       TILE INDEX
!     *KLEVSN*       NUMBER OF SNOW LAYERS
!     *PTVL*         LOW VEGETATION TYPE (REAL)
!     *PCO2TYP*       TYPE OF PHOTOSYNTHETIC PATHWAY FOR LOW VEGETATION (C3/C4) (REAL)
!     *PTVH*         HIGH VEGETATION TYPE (REAL)
!     *PSOTY*        SOIL TYPE (REAL)                               (1-7)
!     *PSDOR*        STANDARD DEV. OF OROGRAPHY (REAL)              (m)

!     INPUT PARAMETERS (REAL):
!     *PCVLC*        LOW VEGETATION COVER  (CLIMATE)                (0-1)
!     *PCVHC*        HIGH VEGETATION COVER (CLIMATE)                (0-1)
!     *PCURC*        URBAN COVER (PASSIVE - CLIMATE)                (0-1)
!     *PLAILC*        LOW LAI (CLIMATE)                             m2/m2
!     *PLAIHC*        HIGH LAI (CLIMATE)                            m2/m2
!     *PAVGPARC*     Average PAR for use in BVOC emissions module   ?
!     *PLSM*         LAND-SEA MASK                                  (0-1)
!     *PCI*          SEA-ICE FRACTION                               (0-1)
!     *PCIL*         LAND-ICE FRACTION                                  (0-1)
!     *PCLAKE*       LAKE FRACTION                                  (0-1)
!     *PHLICE*       LAKE ICE THICKNESS                               m 
!     *PGEMU*        COSINE OF LATITUDE
!     *PSNM1M*       SNOW MASS (per unit area)                      kg/m**2
!     *PWLM1M*       INTERCEPTION RESERVOIR CONTENTS                kg/m**2
!     *PRSNM1M*      SNOW DENSITY                                   kg/m**3

!     OUTPUT PARAMETERS (LOGICAL):
!     *LDLAND*       LAND INDICATOR
!     *LDSICE*       SEA-ICE INDICATOR
!     *LDLICE*       LAND-ICE INDICATOR
!     *LDLAKE*       LAKE INDICATOR
!     *LDNH*         NORTHERN HEMISPHERE INDICATOR
!     *LDOCN_KPP*    KPP MODEL INDICATOR 

!     OUTPUT PARAMETERS (REAL):
!     *PCVL*         LOW VEGETATION COVER  (CORRECTED)              (0-1)
!     *PCVH*         HIGH VEGETATION COVER (CORRECTED)              (0-1)
!     *PCUR*         URBAN COVER (CORRECTED)                        (0-1)
!     *PLAIL*        LOW LAI (REAL)                                 m2/m2
!     *PLAIH*        HIGH LAI (REAL)                                m2/m2
!     *PAVGPAR*      Average PAR for use in BVOC emissions module   ?
!     *PWLMX*        MAXIMUM SKIN RESERVOIR CAPACITY                kg/m**2
!     *PFRTI*        TILE FRACTIONS                                 (0-1)
!            1 : WATER                  5 : SNOW ON LOW-VEG+BARE-SOIL
!            2 : ICE                    6 : DRY SNOW-FREE HIGH-VEG
!            3 : WET SKIN               7 : SNOW UNDER HIGH-VEG
!            4 : DRY SNOW-FREE LOW-VEG  8 : BARE SOIL
!     *PCSN*         SNOW COVER FRACTION (diagnostic)                (0-1)

!     OUTPUT PARAMETERS (INTEGER):
!     *KTVL*         LOW VEGETATION TYPE
!     *KCO2TYP*       TYPE OF PHOTOSYNTHETIC PATHWAY FOR LOW VEGETATION (C3/C4)
!     *KTVH*         HIGH VEGETATION TYPE
!     *KSOTY*        SOIL TYPE                                      (1-7)

!     METHOD
!     ------
!     IT IS NOT ROCKET SCIENCE, BUT CHECK DOCUMENTATION

!     Modifications
!     P. VITERBO       E.C.M.W.F.         18-02-99
!     J.F. Estrade *ECMWF* 03-10-01 move in surf vob
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!     P. Viterbo       24-05-2004       Change surface units         
!     G. Balsamo       15-01-2007       Add soil type
!     E. Dutra/G. Balsamo 01-05-2008    Add lake tile
!     Y. Takaya        07-10-2008       Add flag for ocean mixed layer model
!     S. Boussetta/G.Balsamo May 2009 Add lai
!     I. Ayan-Miguez June 2023        Add object with spatailly distributed parameters
!     G. Arduini     Jan 2024        snow over sea-ice
!     G. Arduini     Sept 2024        Land-ice fraction
!     ------------------------------------------------------------------

USE PARKIND1, ONLY : JPIM, JPRB
USE, INTRINSIC :: ISO_C_BINDING


IMPLICIT NONE

! Declaration of arguments

TYPE(C_PTR)       ,INTENT(IN)    :: YDSURF
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSSDP2(:,:)
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTILES 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVSN
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTVL(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCO2TYP(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTVH(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSOTY(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSDOR(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVLC(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCVHC(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCURC(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAILC(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAIHC(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAILCP(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAIHCP(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAVGPARC(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAILI(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAIHI(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSM(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCI(KLON) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PCIL(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGEMU(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSNM1M(KLON,KLEVSN) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PWLM1M(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRSNM1M(KLON,KLEVSN)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCLAKE(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PHLICE(KLON)
LOGICAL           ,INTENT(IN)    :: LESNICE 
LOGICAL           ,INTENT(OUT)   :: LDLAND(KLON) 
LOGICAL           ,INTENT(OUT)   :: LDSICE(KLON) 
LOGICAL           ,INTENT(OUT)   :: LDLICE(KLON) 
LOGICAL           ,INTENT(OUT)   :: LDNH(KLON) 
LOGICAL           ,INTENT(OUT)   :: LDLAKE(KLON)  
LOGICAL           ,INTENT(OUT)   :: LDOCN_KPP(KLON)  
INTEGER(KIND=JPIM),INTENT(OUT)   :: KTVL(KLON) 
INTEGER(KIND=JPIM),INTENT(OUT)   :: KCO2TYP(KLON) 
INTEGER(KIND=JPIM),INTENT(OUT)   :: KTVH(KLON) 
INTEGER(KIND=JPIM),INTENT(OUT)   :: KSOTY(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCVL(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCVH(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCUR(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLAIL(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLAIH(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLAILP(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLAIHP(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PAVGPAR(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PWLMX(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PFRTI(KLON,KTILES) 

REAL(KIND=JPRB),    INTENT(OUT) :: PCSN(KLON)

!     ------------------------------------------------------------------

END SUBROUTINE SURFBC
END INTERFACE
