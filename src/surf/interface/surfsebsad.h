INTERFACE
SUBROUTINE SURFSEBSAD(YDSURF,KIDIA,KFDIA,KLON,KLEVSN,KTILES,LDSICE,KTVL,KTVH,&
           &PSSDP2,PTMST,PSSKM1M5,PTSKM1M5,PQSKM1M5,PDQSDT5,PRHOCHU5,PRHOCQU5,&
           &PALPHAL5,PALPHAS5,PSSRFL5,PFRTI5,PTSRF5,PSNS5,PRSN5,&
           &PSLRFL5,PTSKRAD5,PEMIS5,PASL5,PBSL5,PAQL5,PBQL5,&
           &PJS5,PJQ5,PSSK5,PTSK5,PSSH5,PSLH5,PSTR5,PG05,&
           &PSL5,PQL5, &
           &PSSKM1M,PTSKM1M,PQSKM1M,PDQSDT,PRHOCHU,PRHOCQU,&
           &PALPHAL,PALPHAS,PSSRFL,PTSRF,&
           &PSLRFL,PTSKRAD,PASL,PBSL,PAQL,PBQL,&
!out
           &PJS,PJQ,PSSK,PTSK,PSSH,PSLH,PSTR,PG0,&
           &PSL,PQL)

USE PARKIND1, ONLY : JPIM, JPRB
USE, INTRINSIC :: ISO_C_BINDING

! (C) Copyright 2003- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!------------------------------------------------------------------------

!  PURPOSE:
!    VDFSEBAD computes surface energy balance and skin temperature 
!             for each tile. 
!             (Adjoint)

!  VDFSEBAD is called by VDFDIFHAD

!  METHOD:
!    A linear relation between lowest model level dry static 
!    energy and moisture and their fluxes is specified as input. 
!    The surface energy balance equation is used to eliminate 
!    the skin temperature as in the derivation of the 
!    Penmann-Monteith equation. 

!    The routine can also be used in stand alone simulations by
!    putting PASL and PAQL to zero and by specifying for PBSL and PBQL 
!    the forcing with dry static energy and specific humidity. 

!  AUTHOR of AD routine:
!    M. Janiskova       ECMWF September 2003   

!  REVISION HISTORY:
!    P. Viterbo         ECMWF March 2004      move to surf vob


!  INTERFACE: 

!    Integers (In):
!      KIDIA   :    Begin point in arrays
!      KFDIA   :    End point in arrays
!      KLON    :    Length of arrays
!      KTILES  :    Number of tiles
!      KTVL    :    Dominant low vegetation type
!      KTVH    :    Dominant high vegetation type

!    INPUT PARAMETERS 

!    Reals with tile index (In):
!      PTMST    :    Time Step
!      PSSKM1M5 :    Dry static energy of skin at T-1           (Trajectory)
!      PTSKM1M5 :    Skin temperature at T-1                    (Trajectory)
!      PQSKM1M5 :    Saturation specific humidity at PTSKM1M    (Trajectory)
!      PDQSDT5  :    dqsat/dT at PTSKM1M                        (Trajectory)
!      PRHOCHU5 :    Rho*Ch*|U|                                 (Trajectory)
!      PRHOCQU5 :    Rho*Cq*|U|                                 (Trajectory)
!      PALPHAL5 :    multiplier of ql in moisture flux eq.      (Trajectory)
!      PALPHAS5 :    multiplier of qs in moisture flux eq.      (Trajectory)
!      PSSRFL5  :    Net short wave radiation at the surface    (Trajectory)
!      PFRTI5   :    Fraction of surface area of each tile      (Trajectory)
!      PTSRF5   :    Surface temp. below skin (e.g. Soil or SST)(Trajectory) 
!      PSNS5    :    Snow mass per unit area                    (Trajectory)
!      PSNS5    :    Snow density                               (Trajectory)

!    Reals independent of tiles (In):
!      PSLRFL5  :    Net long wave radiation at the surface     (Trajectory) 
!      PTSKRAD5 :    Mean skin temp. at radiation time level    (Trajectory)
!      PEMIS5   :    Surface emissivity                         (Trajectory)
!      PASL5    :    Asl in Sl=Asl*Js+Bsl                       (Trajectory)
!      PBSL5    :    Bsl in Sl=Asl*Js+Bsl                       (Trajectory)
!      PAQL5    :    Aql in Ql=Aql*Jq+Bql                       (Trajectory)
!      PBQL5    :    Bql in Ql=Aql*Jq+Bql                       (Trajectory)


!    Reals with tile index (In): 
!      PSSKM1M :    Dry static energy of skin at T-1           (J/kg)
!      PTSKM1M :    Skin temperature at T-1                    (K)
!      PQSKM1M :    Saturation specific humidity at PTSKM1M    (kg/kg)
!      PDQSDT  :    dqsat/dT at PTSKM1M                        (kg/kg K)
!      PRHOCHU :    Rho*Ch*|U|                                 (kg/m2s)
!      PRHOCQU :    Rho*Cq*|U|                                 (kg/m2s)
!      PALPHAL :    multiplier of ql in moisture flux eq.      (-)
!      PALPHAS :    multiplier of qs in moisture flux eq.      (-)
!      PSSRFL  :    Net short wave radiation at the surface    (W/m2)
!      PTSRF   :    Surface temp. below skin (e.g. Soil or SST)  (K) 

!    Reals independent of tiles (In):
!      PSLRFL  :    Net long wave radiation at the surface     (W/m2) 
!      PTSKRAD :    Mean skin temp. at radiation time level    (K)
!      PASL    :    Asl in Sl=Asl*Js+Bsl                       (m2s/kg)
!      PBSL    :    Bsl in Sl=Asl*Js+Bsl                       (J/kg)
!      PAQL    :    Aql in Ql=Aql*Jq+Bql                       (m2s/kg)
!      PBQL    :    Bql in Ql=Aql*Jq+Bql                       (kg/kg)


!     OUTPUT PARAMETERS 

!    Reals with tile index (Out):
!      PJS5     :    Flux of dry static energy                  (Trajectory)
!      PQS5     :    Moisture flux                              (Trajectory)
!      PSSK5    :    New dry static energy of skin              (Trajectory)
!      PTSK5    :    New skin temperature                       (Trajectory)
!      PSSH5    :    Surface sensible heat flux                 (Trajectory)
!      PSLH5    :    Surface latent heat flux                   (Trajectory)
!      PSTR5    :    Surface net thermal radiation              (Trajectory)
!      PG05     :    Surface ground heat flux (solar radiation  (Trajectory)
!                   leakage is not included in this term)

!    Reals independent of tiles (Out):
!      PSL5     :    New lowest model level dry static energy   (Trajectory)
!      PQL5     :    New lowest model level specific humidity   (Trajectory)

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

! Declaration of arguments

TYPE(C_PTR)       , INTENT(IN)  :: YDSURF
INTEGER(KIND=JPIM), INTENT(IN)  :: KLON
INTEGER(KIND=JPIM), INTENT(IN)  :: KLEVSN
INTEGER(KIND=JPIM), INTENT(IN)  :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)  :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)  :: KTILES
INTEGER(KIND=JPIM), INTENT(IN)  :: KTVL(KLON) 
INTEGER(KIND=JPIM), INTENT(IN)  :: KTVH(KLON)
REAL(KIND=JPRB)   , INTENT(IN)  :: PSSDP2(:,:)
LOGICAL           , INTENT(IN)  :: LDSICE(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PTMST

REAL(KIND=JPRB), INTENT(IN) :: PSSKM1M5(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PTSKM1M5(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PQSKM1M5(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PDQSDT5(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PRHOCHU5(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PRHOCQU5(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PALPHAL5(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PALPHAS5(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PSSRFL5(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PFRTI5(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PTSRF5(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PSNS5(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PRSN5(:,:)
REAL(KIND=JPRB), INTENT(IN) :: PSLRFL5(:)
REAL(KIND=JPRB), INTENT(IN) :: PTSKRAD5(:)
REAL(KIND=JPRB), INTENT(IN) :: PEMIS5(:)
REAL(KIND=JPRB), INTENT(IN) :: PASL5(:)
REAL(KIND=JPRB), INTENT(IN) :: PBSL5(:)
REAL(KIND=JPRB), INTENT(IN) :: PAQL5(:)
REAL(KIND=JPRB), INTENT(IN) :: PBQL5(:)
REAL(KIND=JPRB), INTENT(OUT) :: PJS5(:,:)
REAL(KIND=JPRB), INTENT(OUT) :: PJQ5(:,:)
REAL(KIND=JPRB), INTENT(OUT) :: PSSK5(:,:)
REAL(KIND=JPRB), INTENT(OUT) :: PTSK5(:,:)
REAL(KIND=JPRB), INTENT(OUT) :: PSSH5(:,:)
REAL(KIND=JPRB), INTENT(OUT) :: PSLH5(:,:)
REAL(KIND=JPRB), INTENT(OUT) :: PSTR5(:,:)
REAL(KIND=JPRB), INTENT(OUT) :: PG05(:,:)
REAL(KIND=JPRB), INTENT(OUT) :: PSL5(:)
REAL(KIND=JPRB), INTENT(OUT) :: PQL5(:)

REAL(KIND=JPRB), INTENT(INOUT) :: PSSKM1M(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PTSKM1M(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PQSKM1M(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PDQSDT(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PRHOCHU(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PRHOCQU(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PALPHAL(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PALPHAS(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PSSRFL(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PTSRF(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PSLRFL(:)
REAL(KIND=JPRB), INTENT(INOUT) :: PTSKRAD(:)
REAL(KIND=JPRB), INTENT(INOUT) :: PASL(:)
REAL(KIND=JPRB), INTENT(INOUT) :: PBSL(:)
REAL(KIND=JPRB), INTENT(INOUT) :: PAQL(:)
REAL(KIND=JPRB), INTENT(INOUT) :: PBQL(:)
REAL(KIND=JPRB), INTENT(INOUT) :: PJS(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PJQ(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PSSK(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PTSK(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PSSH(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PSLH(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PSTR(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PG0(:,:)
REAL(KIND=JPRB), INTENT(INOUT) :: PSL(:)
REAL(KIND=JPRB), INTENT(INOUT) :: PQL(:)

!     ------------------------------------------------------------------

END SUBROUTINE SURFSEBSAD
END INTERFACE
