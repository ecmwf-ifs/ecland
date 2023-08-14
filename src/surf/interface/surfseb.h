INTERFACE
SUBROUTINE SURFSEB   (YSURF,KIDIA,KFDIA,KLON,KTILES,KTVL,KTVH,&
 & PTMST,PSSKM1M,PTSKM1M,PQSKM1M,PDQSDT,PRHOCHU,PRHOCQU,&
 & PALPHAL,PALPHAS,PSSRFL,PFRTI,PTSRF,PLAMSK,&
 & PSNM,PRSN,PHLICE, & 
 & PSLRFL,PTSKRAD,PEMIS,PASL,PBSL,PAQL,PBQL,&
 & PTHKICE,PSNTICE,&
 !out
 & PJS,PJQ,PSSK,PTSK,PSSH,PSLH,PSTR,PG0,&
 & PSL,PQL,&
 & LNEMOLIMTHK)  

USE PARKIND1, ONLY : JPIM, JPRB
USE YOS_SURF, ONLY : TSURF
USE ISO_C_BINDING

! (C) Copyright 2003- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!------------------------------------------------------------------------

!  PURPOSE:
!    Routine SURFSEB computes surface energy balance and skin temperature 
!    for each tile. 

!  SURFSEB is called by VDFDIFH

!  METHOD:
!    A linear relation between lowest model level dry static 
!    energy and moisture and their fluxes is specified as input. 
!    The surface energy balance equation is used to eliminate 
!    the skin temperature as in the derivation of the 
!    Penmann-Monteith equation. 

!    The routine can also be used in stand alone simulations by
!    putting PASL and PAQL to zero and by specifying for PBSL and PBQL 
!    the forcing with dry static energy and specific humidity. 

!  AUTHOR:
!    A. Beljaars       ECMWF April 2003   

!  REVISION HISTORY:
!    J.F. Estrade *ECMWF* 03-10-01 move in surf vob
!    E. Dutra/G. Balsamo  01-05-08 lake tile
!    Linus Magnusson   10-09-28 Sea-ice 
!    G. Balsamo/A. Beljaars 09-08-2013 snow scheme stability fix
!      I. Sandu    24-02-2014  Lambda skin values by vegetation type instead of tile

!  INTERFACE: 

!    Integers (In):
!      KIDIA   :    Begin point in arrays
!      KFDIA   :    End point in arrays
!      KLON    :    Length of arrays
!      KTILES  :    Number of tiles
!      KTVL    :    Low vegetation type 
!      KTVH    :    High vegetation type

!    Reals with tile index (In): 
!      PTMST   :    Time Step
!      PSSKM1M :    Dry static energy of skin at T-1           (J/kg)
!      PTSKM1M :    Skin temperature at T-1                    (K)
!      PQSKM1M :    Saturation specific humidity at PTSKM1M    (kg/kg)
!      PDQSDT  :    dqsat/dT at PTSKM1M                        (kg/kg K)
!      PRHOCHU :    Rho*Ch*|U|                                 (kg/m2s)
!      PRHOCQU :    Rho*Cq*|U|                                 (kg/m2s)
!      PALPHAL :    multiplier of ql in moisture flux eq.      (-)
!      PALPHAS :    multiplier of qs in moisture flux eq.      (-)
!      PSSRFL  :    Net short wave radiation at the surface    (W/m2)
!      PFRTI   :    Fraction of surface area of each tile      (-)
!      PTSRF   :    Surface temp. below skin (e.g. Soil or SST)(K) 
!      PLAMSK  :    Skin conductivity                          ( W m-2 K

!    Reals independent of tiles (In):
!      PSLRFL  :    Net long wave radiation at the surface     (W/m2) 
!      PTSKRAD :    Mean skin temp. at radiation time level    (K)
!      PEMIS   :    Surface emissivity                         (-)
!      PASL    :    Asl in Sl=Asl*Js+Bsl                       (m2s/kg)
!      PBSL    :    Bsl in Sl=Asl*Js+Bsl                       (J/kg)
!      PAQL    :    Aql in Ql=Aql*Jq+Bql                       (m2s/kg)
!      PBQL    :    Bql in Ql=Aql*Jq+Bql                       (kg/kg)
!      PTHKICE :    Sea ice thickness                          (m)
!      PSNTICE :    Thickness of snow layer on sea ice         (m)
!      PSNM    :    Snow mass per unit area                    (kg/m2)

!    Reals with tile index (Out):
!      PJS     :    Flux of dry static energy                  (W/m2)
!      PQS     :    Moisture flux                              (kg/m2s)
!      PSSK    :    New dry static energy of skin              (J/kg)
!      PTSK    :    New skin temperature                       (K)
!      PSSH    :    Surface sensible heat flux                 (W/m2)
!      PSLH    :    Surface latent heat flux                   (W/m2)
!      PSTR    :    Surface net thermal radiation              (W/m2)
!      PG0     :    Surface ground heat flux (solar radiation  (W/m2)
!                   leakage is not included in this term)

!    Reals independent of tiles (Out):
!      PSL     :    New lowest model level dry static energy   (J/kg)
!      PQL     :    New lowest model level specific humidity   (kg/kg)

!  DOCUMENTATION:
!    See Physics Volume of IFS documentation
!    This routine uses the method suggested by Polcher and Best
!    (the basic idea is to start with a linear relation between 
!     the lowest model level varibles and their fluxes, which is 
!     obtained after the downward elimination of the vertical 
!     diffusion tridiagonal matrix). 

!------------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments

TYPE(TSURF)       ,INTENT(IN)    :: YSURF
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTILES 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTVL(KLON)
INTEGER(KIND=JPIM),INTENT(IN)    :: KTVH(KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTMST

REAL(KIND=JPRB),    INTENT(IN)  :: PSSKM1M(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(IN)  :: PTSKM1M(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(IN)  :: PQSKM1M(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(IN)  :: PDQSDT(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(IN)  :: PRHOCHU(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(IN)  :: PRHOCQU(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(IN)  :: PALPHAL(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(IN)  :: PALPHAS(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(IN)  :: PSSRFL(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(IN)  :: PFRTI(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(IN)  :: PTSRF(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(IN)  :: PLAMSK(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(IN)  :: PSLRFL(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PSNM(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PRSN(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PTSKRAD(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PEMIS(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PASL(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PBSL(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PAQL(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PBQL(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PHLICE(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PTHKICE(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PSNTICE(KLON)
REAL(KIND=JPRB),    INTENT(OUT) :: PJS(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(OUT) :: PJQ(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(OUT) :: PSSK(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(OUT) :: PTSK(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(OUT) :: PSSH(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(OUT) :: PSLH(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(OUT) :: PSTR(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(OUT) :: PG0(KLON,KTILES)
REAL(KIND=JPRB),    INTENT(OUT) :: PSL(KLON)
REAL(KIND=JPRB),    INTENT(OUT) :: PQL(KLON)
LOGICAL           ,INTENT(IN)    :: LNEMOLIMTHK


!     ------------------------------------------------------------------

END SUBROUTINE SURFSEB
END INTERFACE
