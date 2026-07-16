MODULE SURFWS_INIT_SL_MOD
CONTAINS

SUBROUTINE SURFWS_INIT_SL(KIDIA, KFDIA, KLON, KLEVSN, PMU0, PSDOR,         &  ! Input
                     &  PTSOIL, PTSKIN,LDLAND,                             &
                     &  ZDSNTOT, ZSNDEPTH,                                 &
                     &  ZSNPERT,                                           &  ! Input
                     &  ZDSNR,PTSN, PRSN, PSSN, PWSN,PASN,                 &  ! Input
                     &  PTSNWS,PSSNWS,PRSNWS,PWSNWS,                       & ! Output
                     &  PTSNTOP, PTSNBOTTOM, PTSNMIDDLE,                   & ! Output
                     &  PRSNMAX, ZSADEPTH, KLEVSNA, KLEVMID, ZACTDEPTH,    & ! Output
                     &  PTMINCL,PRMINCL,                                   & ! Output
                     &  PTCONSTAVG, PTCONSTSTD,                            & ! Output
                     &  PRCONSTAVG, PRCONSTSTD, PRSNTOP,                   & ! Output
                     &  YDCST, YDSOIL )

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_CST  , ONLY : TCST, JPNCL, TCENTRADAY2, TCENTRBDAY2, TCENTRCDAY2, TCENTRADAY3, TCENTRBDAY3,  &
  &                  TCENTRCDAY3, TCENTRADAY4, TCENTRBDAY4, TCENTRCDAY4, TCENTRADAY5,  &
  &                  TCENTRBDAY5, TCENTRCDAY5, TCENTRADAY5M, TCENTRBDAY5M, TCENTRCDAY5M,  &
  &                  TCENTRADAY5G, TCENTRBDAY5G, TCENTRCDAY5G, TCENTRANIGHT2, TCENTRBNIGHT2,  &
  &                  TCENTRCNIGHT2, TCENTRANIGHT3, TCENTRBNIGHT3, TCENTRCNIGHT3, TCENTRANIGHT4,  &
  &                  TCENTRBNIGHT4, TCENTRCNIGHT4, TCENTRANIGHT5, TCENTRBNIGHT5, TCENTRCNIGHT5,  &
  &                  TCENTRANIGHT5M, TCENTRBNIGHT5M, TCENTRCNIGHT5M, TCENTRANIGHT5G,  &
  &                  TCENTRBNIGHT5G, TCENTRCNIGHT5G, TCONSTAVGDAY2, TCONSTSTDDAY2, TCONSTAVGDAY3,  &
  &                  TCONSTSTDDAY3, TCONSTAVGDAY4, TCONSTSTDDAY4, TCONSTAVGDAY5, TCONSTSTDDAY5,  &
  &                  TCONSTAVGDAY5M, TCONSTSTDDAY5M, TCONSTAVGDAY5G, TCONSTSTDDAY5G,  &
  &                  TCONSTAVGNIGHT2, TCONSTSTDNIGHT2, TCONSTAVGNIGHT3, TCONSTSTDNIGHT3,  &
  &                  TCONSTAVGNIGHT4, TCONSTSTDNIGHT4, TCONSTAVGNIGHT5, TCONSTSTDNIGHT5,  &
  &                  TCONSTAVGNIGHT5M, TCONSTSTDNIGHT5M, TCONSTAVGNIGHT5G, TCONSTSTDNIGHT5G,  &
  &                  TMLRIDAY2, TMLRADAY2, TMLRBDAY2, TMLRCDAY2, TMLRDDAY2, TMLREDAY2,  &
  &                  TMLRINIGHT2, TMLRANIGHT2, TMLRBNIGHT2, TMLRCNIGHT2, TMLRDNIGHT2,  &
  &                  TMLRENIGHT2, TMLRIDAY3, TMLRADAY3, TMLRBDAY3, TMLRCDAY3, TMLRDDAY3,  &
  &                  TMLREDAY3, TMLRINIGHT3, TMLRANIGHT3, TMLRBNIGHT3, TMLRCNIGHT3, TMLRDNIGHT3,  &
  &                  TMLRENIGHT3, RCENTRA2, RCENTRB2, RCENTRC2, RCENTRA3, RCENTRB3, RCENTRC3,  &
  &                  RCENTRA4, RCENTRB4, RCENTRC4, RCENTRA5, RCENTRB5, RCENTRC5, RCENTRA5M,  &
  &                  RCENTRB5M, RCENTRC5M, RCONSTAVG2, RCONSTSTD2, RCONSTAVG3, RCONSTSTD3,  &
  &                  RCONSTAVG4, RCONSTSTD4, RCONSTAVG5, RCONSTSTD5, RCONSTAVG5M, RCONSTSTD5M,  &
  &                  RMLRI2, RMLRA2, RMLRB2, RMLRC2, RMLRD2, RMLRE2, RMLRI3, RMLRA3, RMLRB3,  &
  &                  RMLRC3, RMLRD3, RMLRE3, RMLRI4, RMLRA4, RMLRB4, RMLRC4, RMLRD4, RMLRE4,  &
  &                  RMLRI5, RMLRA5, RMLRB5, RMLRC5, RMLRD5, RMLRE5, RMLRI5M, RMLRA5M, RMLRB5M,  &
  &                  RMLRC5M, RMLRD5M, RMLRE5M
USE YOS_SOIL , ONLY : TSOIL

USE ABORT_SURF_MOD

! (C) Copyright 2017- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SURFWS_INIT_SL* - Snow warm start multi-layer 
!     PURPOSE.
!     --------
!          THIS ROUTINE SETUP PARAMETERS USED IN THE
!          OTHER WARM START ROUTINE BASED ON NSNMLWS VALUE

!**   INTERFACE.
!     ----------
!          *SURFWS_INIT_SL* IS CALLED FROM *SURFWS_CTL*.

!     PARAMETER   DESCRIPTION                                    UNITS
!     ---------   -----------                                    -----
                     
!     INPUT PARAMETERS (INTEGER):
!    *KIDIA*      START POINT
!    *KFDIA*      END POINT
!    *KLON*       Length of arrays
!    *KLEVSN*     Snow vertical levels
!    *KLEVMID*    Snow middle levels (surfws_init)
!    *NCL*        Number of clusters


!     INPUT PARAMETERS (REAL):
!    *ZSNPERT*    snow depth threshold for glaciers
!    *PMU0*       Cosine of solar zenith angle
!    *PSDOR*      sub grid scale orography   (m)
!    *ZDSNTOT*    Total snow depth                            (m)
!    *ZSNDEPTH*   Snow depth of each layer wrt 0              (m)
!    *ZDSNR*      Snow depth per layer (full) wrt 0           (m)

!     INPUT PARAMETERS (LOGICAL):
!    *LDLAND*     LAND/SEA MASK (TRUE/FALSE)

!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!    *PTSOIL*     soil temperature top layer t-1       (K)
!    *PTSKIN*     skin temperature t-1                 (K)
!    *PTSN*       SNOW TEMPERATURE single layer               (K)
!    *PSSN*       SNOW MASS        single layer               (kg m-2)
!    *PRSN*       SNOW DENSITY     single layer               (kg m-3)
!    *PASN*       Snow albedo                                 (K)

!     OUTPUT PARAMETERS (REAL)
!    *PTSNTOP*    Snow temperature top layer                  (K)
!    *PTSNBOTTOM* Snow temperature bottom layer               (K)
!    *PTSNMIDDLE* Snow temperature KLEVMID layer              (K)
!    *PRSNTOP*    Snow density top layer                      (kg m-3)
!    *PRSNMAX*    Snow density MAX allowed                    (kg m-3)
!    *PSADEPTH*   Soil depth top layer                        (m)
!    *PACTDEPTH*  Active snow depth                           (m)
!    *PTCONSTAVG* Constants for temperature exp function
!    *PTCONSTSTD* Constants for temperature energy adj
!    *PRCONSTAVG* Constants for density exp function
!    *PRCONSTSTD* Constants for density energy adj
!    *PRSNTOP*    Snow density top layer

!    OUTPUT PARAMETERS (INTEGER)
!    *KLEVMID*    Snow middle levels 
!    *PTMINCL*    Cluster index for temperature exp function
!    *PRMINCL*    Cluster index for density exp function

!     OUTPUT PARAMETERS (REAL, WARM START):
!    *PTSNWS*        Snow tempeature warm start (initialised)
!    *PRSNWS*        Snow density    warm start (initialised)
!    *PSSNWS*        Snow mass       warm start (initialised)
!    *PWSNWS*        Snow liq water  warm start (initialised)

!     INPUT/OUTPUT PARAMETERS
!    *KLEVSNA*    Snow vertical Active levels

!     METHOD.
!     -------
!     Values of exp functions are pre-computed with k-cluster 
!     algorithm, see Arduini et al. 2019. 
!     Snow density top layer is computed with linear regression,
!     with pre-computed parameters.

!     EXTERNALS.
!     ----------
!          NONE.

!     REFERENCE.
!     ----------
!          Arduini et al. (2019)

!     Modifications:
!     Original   G. Arduini      ECMWF     28/07/2017

!     ------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments 
INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN) :: KLON
INTEGER(KIND=JPIM), INTENT(IN) :: KLEVSN
LOGICAL,            INTENT(IN) :: LDLAND(:)

REAL(KIND=JPRB), INTENT(IN)    :: ZDSNTOT(:), PTSN(:), PRSN(:), PSSN(:), PWSN(:)
REAL(KIND=JPRB), INTENT(IN)    :: ZSNDEPTH(:,:)
REAL(KIND=JPRB), INTENT(IN)    :: ZDSNR(:,:)
REAL(KIND=JPRB), INTENT(IN)    :: PSDOR(:)

REAL(KIND=JPRB), INTENT(IN)    :: PMU0(:)
REAL(KIND=JPRB), INTENT(IN)    :: PTSOIL(:)
REAL(KIND=JPRB), INTENT(IN)    :: PTSKIN(:)
REAL(KIND=JPRB), INTENT(IN)    :: PASN(:)

TYPE(TCST)     , INTENT(IN) :: YDCST
TYPE(TSOIL)    , INTENT(IN) :: YDSOIL

! Output Variables
REAL(KIND=JPRB), INTENT(OUT)    :: PTSNWS(:,:)
REAL(KIND=JPRB), INTENT(OUT)    :: PSSNWS(:,:)
REAL(KIND=JPRB), INTENT(OUT)    :: PRSNWS(:,:)
REAL(KIND=JPRB), INTENT(OUT)    :: PWSNWS(:,:)

REAL(KIND=JPRB), INTENT(OUT)    :: PTSNTOP(:), PTSNBOTTOM(:), PTSNMIDDLE(:)
REAL(KIND=JPRB), INTENT(OUT)    :: PRSNTOP(:)

REAL(KIND=JPRB), INTENT(OUT)    :: PRSNMAX(:)
REAL(KIND=JPRB), INTENT(OUT)    :: ZSADEPTH(:)

! Active number of layers:
INTEGER(KIND=JPIM),INTENT(INOUT)  :: KLEVSNA(:)

! Active depth layer (glaciers):
REAL(KIND=JPRB), INTENT(OUT)    :: ZACTDEPTH(:)

! Cluster values:
INTEGER(KIND=JPIM),            INTENT(OUT)  :: KLEVMID(:)
INTEGER(KIND=JPIM),            INTENT(OUT)  :: PTMINCL(:), PRMINCL(:) 
REAL(KIND=JPRB),               INTENT(OUT)  :: PRCONSTAVG(KLON,JPNCL), PRCONSTSTD(KLON,JPNCL)
REAL(KIND=JPRB),               INTENT(OUT)  :: PTCONSTAVG(KLON,JPNCL), PTCONSTSTD(KLON,JPNCL)

REAL(KIND=JPRB)                 :: ZSNPERT

! Local variables:
INTEGER(KIND=JPIM)              :: JL,JK,KNACC
INTEGER(KIND=JPIM)              :: KCL
REAL(KIND=JPRB), DIMENSION(JPNCL) :: ZTDIST, ZRDIST
REAL(KIND=JPRB)                 :: ZTDIST_MIN, ZRDIST_MIN
REAL(KIND=JPRB)                 :: ZFEATA, ZFEATB, ZFEATC 

! Look-up tables for warm-start

REAL(KIND=JPRB), DIMENSION(JPNCL)  :: ZTCONSTAVG, ZRCONSTAVG, ZTCONSTSTD, ZRCONSTSTD
REAL(KIND=JPRB), DIMENSION(JPNCL)  ::  ZTCENTRA, ZTCENTRB, ZTCENTRC
REAL(KIND=JPRB), DIMENSION(JPNCL)  ::  ZRCENTRA, ZRCENTRB, ZRCENTRC


REAL(KIND=JPRB), DIMENSION(JPNCL)  ::  ZTMLRA, ZTMLRB, ZTMLRC, ZTMLRD, ZTMLRE, ZTMLRI
REAL(KIND=JPRB), DIMENSION(JPNCL)  :: ZRMLRI, ZRMLRA, ZRMLRB, ZRMLRC, ZRMLRD, ZRMLRE
REAL(KIND=JPRB)                  :: ZP1, ZP2, ZP3, ZP4, ZP5
REAL(KIND=JPRB)                  :: ZSDORTHR
REAL(KIND=JPRB)                  :: ZEPSILON

REAL(KIND=JPHOOK)                  :: ZHOOK_HANDLE


!    -----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SURFWS_INIT_SL_MOD:SURFWS_INIT_SL',0,ZHOOK_HANDLE)

!    -----------------------------------------------------------------

ASSOCIATE(RTT=>YDCST%RTT,RLMLT=>YDCST%RLMLT, RPI=>YDCST%RPI,    &
        & RLWCSWEA=>YDSOIL%RLWCSWEA, RLWCSWEB=>YDSOIL%RLWCSWEB, &
        & RLWCSWEC=>YDSOIL%RLWCSWEC, RTEMPAMP=>YDSOIL%RTEMPAMP, &
        & RDSNMAX=>YDSOIL%RDSNMAX, RHOMINSND=>YDSOIL%RHOMINSND, &
        & RHOMAXSN_NEW=>YDSOIL%RHOMAXSN_NEW, RDAT=>YDSOIL%RDAT  )
ZSDORTHR=50._JPRB

ZEPSILON  = 10E4*EPSILON(ZEPSILON)

! 0.1 Define centroids 
! A: soT-skt B: soT-snT ; C: rsn/rsnmin


! 0.3 Define constants for multi-linear regression of temp 1st layer (only ! dsn<0.20):


! 0.4 Define centroids for Density: no distinction day and night...
! A: soT-skt B: soT-snT ; C: rsn/rsnmin


! mid-point first soil level
ZSADEPTH(KIDIA:KFDIA)=0.5_JPRB*RDAT(1)

!*******************************************************************************
! 2. Start snow parametrizations
!    Here we start the snow temperature and density parametrisation:
!    both are simple exponential function. Temperature relaxes to first soil layer
!    at the bottom, while the density relaxes to mean density. 
!    The missing snow mass it is added to the bottom layer simply increasing the
!    density of this layer keeping the depth fixed.
!*******************************************************************************
  DO JL=KIDIA, KFDIA
    IF (PMU0(JL) > ZEPSILON ) THEN ! Daytime
        ! Initialise values to avoid floating point errors
        ZTCENTRA=TCENTRADAY2
        ZTCENTRB=TCENTRBDAY2
        ZTCENTRC=TCENTRCDAY2
        ZTCONSTAVG=TCONSTAVGDAY2
        ZTCONSTSTD=TCONSTSTDDAY2
        KLEVMID(JL)=MAX(KLEVSNA(JL)-1,1)

    IF ( PSSN(JL) < ZSNPERT .AND. LDLAND(JL) ) THEN ! seasonal snow
      IF ( ZDSNTOT(JL) < 0.15_JPRB ) THEN 
        ZTCENTRA=TCENTRADAY2
        ZTCENTRB=TCENTRBDAY2
        ZTCENTRC=TCENTRCDAY2

        ZTCONSTAVG=TCONSTAVGDAY2
        ZTCONSTSTD=TCONSTSTDDAY2

        ZTMLRI=TMLRIDAY2
        ZTMLRA=TMLRADAY2
        ZTMLRB=TMLRBDAY2
        ZTMLRC=TMLRCDAY2
        ZTMLRD=TMLRDDAY2
        ZTMLRE=TMLREDAY2

      ELSEIF ( ZDSNTOT(JL) < 0.20_JPRB ) THEN 
        ZTCENTRA=TCENTRADAY3
        ZTCENTRB=TCENTRBDAY3
        ZTCENTRC=TCENTRCDAY3

        ZTCONSTAVG=TCONSTAVGDAY3
        ZTCONSTSTD=TCONSTSTDDAY3
        
        ZTMLRI=TMLRIDAY3
        ZTMLRA=TMLRADAY3
        ZTMLRB=TMLRBDAY3
        ZTMLRC=TMLRCDAY3
        ZTMLRD=TMLRDDAY3
        ZTMLRE=TMLREDAY3

      ELSEIF ( ZDSNTOT(JL) < 0.25_JPRB ) THEN 
        ZTCENTRA=TCENTRADAY4
        ZTCENTRB=TCENTRBDAY4
        ZTCENTRC=TCENTRCDAY4
        
        ZTCONSTAVG=TCONSTAVGDAY4
        ZTCONSTSTD=TCONSTSTDDAY4

      ELSEIF ( ZDSNTOT(JL) < 0.50_JPRB ) THEN 
        IF (PSDOR(JL)<ZSDORTHR)THEN
          ZTCENTRA=TCENTRADAY5
          ZTCENTRB=TCENTRBDAY5
          ZTCENTRC=TCENTRCDAY5

          ZTCONSTAVG=TCONSTAVGDAY5
          ZTCONSTSTD=TCONSTSTDDAY5
        ELSEIF (KLEVSNA(JL)==2)THEN
          ZTCENTRA=TCENTRADAY2
          ZTCENTRB=TCENTRBDAY2
          ZTCENTRC=TCENTRCDAY2

          ZTCONSTAVG=TCONSTAVGDAY2
          ZTCONSTSTD=TCONSTSTDDAY2
        ELSEIF (KLEVSNA(JL)==3)THEN
          ZTCENTRA=TCENTRADAY3
          ZTCENTRB=TCENTRBDAY3
          ZTCENTRC=TCENTRCDAY3

          ZTCONSTAVG=TCONSTAVGDAY3
          ZTCONSTSTD=TCONSTSTDDAY3

        ELSEIF (KLEVSNA(JL)==4)THEN
          ZTCENTRA=TCENTRADAY4
          ZTCENTRB=TCENTRBDAY4
          ZTCENTRC=TCENTRCDAY4

          ZTCONSTAVG=TCONSTAVGDAY4
          ZTCONSTSTD=TCONSTSTDDAY4
        ELSEIF (KLEVSNA(JL)==5)THEN
          ZTCENTRA=TCENTRADAY5
          ZTCENTRB=TCENTRBDAY5
          ZTCENTRC=TCENTRCDAY5

          ZTCONSTAVG=TCONSTAVGDAY5
          ZTCONSTSTD=TCONSTSTDDAY5
        ENDIF
      ELSEIF ( ZDSNTOT(JL) >= 0.50_JPRB ) THEN 
        ZTCENTRA=TCENTRADAY5M
        ZTCENTRB=TCENTRBDAY5M
        ZTCENTRC=TCENTRCDAY5M

        ZTCONSTAVG=TCONSTAVGDAY5M
        ZTCONSTSTD=TCONSTSTDDAY5M
      ENDIF
    ELSE ! Glaciers, daytime
        ZTCENTRA=TCENTRADAY5G
        ZTCENTRB=TCENTRBDAY5G
        ZTCENTRC=TCENTRCDAY5G

        ZTCONSTAVG=TCONSTAVGDAY5G
        ZTCONSTSTD=TCONSTSTDDAY5G
    ENDIF
    ELSEIF (PMU0(JL)<=ZEPSILON) THEN !nighttime
        ! Initialise values to avoid floating point errors
        ZTCENTRA=TCENTRANIGHT2
        ZTCENTRB=TCENTRBNIGHT2
        ZTCENTRC=TCENTRCNIGHT2
        ZTCONSTAVG=TCONSTAVGNIGHT2
        ZTCONSTSTD=TCONSTSTDNIGHT2
        KLEVMID(JL)=MAX(KLEVSNA(JL)-1,1)

    IF ( PSSN(JL) < ZSNPERT .AND. LDLAND(JL) ) THEN ! seasonal snow
      IF ( ZDSNTOT(JL) < 0.15_JPRB ) THEN 
        ZTCENTRA=TCENTRANIGHT2
        ZTCENTRB=TCENTRBNIGHT2
        ZTCENTRC=TCENTRCNIGHT2

        ZTCONSTAVG=TCONSTAVGNIGHT2
        ZTCONSTSTD=TCONSTSTDNIGHT2

        ZTMLRI=TMLRINIGHT2
        ZTMLRA=TMLRANIGHT2
        ZTMLRB=TMLRBNIGHT2
        ZTMLRC=TMLRCNIGHT2
        ZTMLRD=TMLRDNIGHT2
        ZTMLRE=TMLRENIGHT2
        
      ELSEIF ( ZDSNTOT(JL) < 0.20_JPRB ) THEN 
        ZTCENTRA=TCENTRANIGHT3
        ZTCENTRB=TCENTRBNIGHT3
        ZTCENTRC=TCENTRCNIGHT3

        ZTCONSTAVG=TCONSTAVGNIGHT3
        ZTCONSTSTD=TCONSTSTDNIGHT3
        
        ZTMLRI=TMLRINIGHT3
        ZTMLRA=TMLRANIGHT3
        ZTMLRB=TMLRBNIGHT3
        ZTMLRC=TMLRCNIGHT3
        ZTMLRD=TMLRDNIGHT3
        ZTMLRE=TMLRENIGHT3

      ELSEIF ( ZDSNTOT(JL) < 0.25_JPRB ) THEN 
        ZTCENTRA=TCENTRANIGHT4
        ZTCENTRB=TCENTRBNIGHT4
        ZTCENTRC=TCENTRCNIGHT4

        ZTCONSTAVG=TCONSTAVGNIGHT4
        ZTCONSTSTD=TCONSTSTDNIGHT4
        
      ELSEIF ( ZDSNTOT(JL) < 0.50_JPRB ) THEN 
        IF (PSDOR(JL)<ZSDORTHR)THEN
          ZTCENTRA=TCENTRANIGHT5
          ZTCENTRB=TCENTRBNIGHT5
          ZTCENTRC=TCENTRCNIGHT5

          ZTCONSTAVG=TCONSTAVGNIGHT5
          ZTCONSTSTD=TCONSTSTDNIGHT5
        ELSEIF (KLEVSNA(JL)==2)THEN
          ZTCENTRA=TCENTRANIGHT2
          ZTCENTRB=TCENTRBNIGHT2
          ZTCENTRC=TCENTRCNIGHT2

          ZTCONSTAVG=TCONSTAVGNIGHT2
          ZTCONSTSTD=TCONSTSTDNIGHT2
        ELSEIF (KLEVSNA(JL)==3)THEN
          ZTCENTRA=TCENTRANIGHT3
          ZTCENTRB=TCENTRBNIGHT3
          ZTCENTRC=TCENTRCNIGHT3

          ZTCONSTAVG=TCONSTAVGNIGHT3
          ZTCONSTSTD=TCONSTSTDNIGHT3
        ELSEIF (KLEVSNA(JL)==4)THEN
          ZTCENTRA=TCENTRANIGHT4
          ZTCENTRB=TCENTRBNIGHT4
          ZTCENTRC=TCENTRCNIGHT4

          ZTCONSTAVG=TCONSTAVGNIGHT4
          ZTCONSTSTD=TCONSTSTDNIGHT4
        ELSEIF (KLEVSNA(JL)==5)THEN
          ZTCENTRA=TCENTRANIGHT5
          ZTCENTRB=TCENTRBNIGHT5
          ZTCENTRC=TCENTRCNIGHT5

          ZTCONSTAVG=TCONSTAVGNIGHT5
          ZTCONSTSTD=TCONSTSTDNIGHT5
        ENDIF
        
      ELSEIF ( ZDSNTOT(JL) >= 0.50_JPRB ) THEN 
        ZTCENTRA=TCENTRANIGHT5M
        ZTCENTRB=TCENTRBNIGHT5M
        ZTCENTRC=TCENTRCNIGHT5M

        ZTCONSTAVG=TCONSTAVGNIGHT5M
        ZTCONSTSTD=TCONSTSTDNIGHT5M
      ENDIF
    ELSE !Glaciers, nighttime
        ZTCENTRA=TCENTRANIGHT5G
        ZTCENTRB=TCENTRBNIGHT5G
        ZTCENTRC=TCENTRCNIGHT5G

        ZTCONSTAVG=TCONSTAVGNIGHT5G
        ZTCONSTSTD=TCONSTSTDNIGHT5G
    ENDIF
    ENDIF

    PTCONSTAVG(JL,1:JPNCL)=ZTCONSTAVG(1:JPNCL)
    PTCONSTSTD(JL,1:JPNCL)=ZTCONSTSTD(1:JPNCL)

! Assign density depending on snow depth::
    ! Initialise arrays, to avoid undesired effects.
    ZRMLRA(1:JPNCL)=RMLRA2(1:JPNCL)
    ZRMLRB(1:JPNCL)=RMLRB2(1:JPNCL)
    ZRMLRC(1:JPNCL)=RMLRC2(1:JPNCL)
    ZRMLRD(1:JPNCL)=RMLRD2(1:JPNCL)
    ZRMLRE(1:JPNCL)=RMLRE2(1:JPNCL)
    IF ( ZDSNTOT(JL) < 0.15_JPRB ) THEN 
      ZRCENTRA=RCENTRA2
      ZRCENTRB=RCENTRB2
      ZRCENTRC=RCENTRC2

      ZRCONSTAVG=RCONSTAVG2
      ZRCONSTSTD=RCONSTSTD2

      ZRMLRI=RMLRI2
      ZRMLRA=RMLRA2
      ZRMLRB=RMLRB2
      ZRMLRC=RMLRC2
      ZRMLRD=RMLRD2
      ZRMLRE=RMLRE2
      
      KLEVMID(JL)=2_JPIM
    ELSEIF ( ZDSNTOT(JL) < 0.20_JPRB ) THEN 
      ZRCENTRA=RCENTRA3
      ZRCENTRB=RCENTRB3
      ZRCENTRC=RCENTRC3

      ZRCONSTAVG=RCONSTAVG3
      ZRCONSTSTD=RCONSTSTD3
      
      ZRMLRI=RMLRI3
      ZRMLRA=RMLRA3
      ZRMLRB=RMLRB3
      ZRMLRC=RMLRC3
      ZRMLRD=RMLRD3
      ZRMLRE=RMLRE3

      KLEVMID(JL)=2_JPIM

    ELSEIF ( ZDSNTOT(JL) < 0.25_JPRB ) THEN 
      ZRCENTRA=RCENTRA4
      ZRCENTRB=RCENTRB4
      ZRCENTRC=RCENTRC4

      ZRCONSTAVG=RCONSTAVG4
      ZRCONSTSTD=RCONSTSTD4
      
      ZRMLRI=RMLRI4
      ZRMLRA=RMLRA4
      ZRMLRB=RMLRB4
      ZRMLRC=RMLRC4
      ZRMLRD=RMLRD4
      ZRMLRE=RMLRE4
 
      KLEVMID(JL)=3_JPIM

    ELSEIF ( ZDSNTOT(JL) < 0.50_JPRB ) THEN 
      IF (PSDOR(JL)<ZSDORTHR)THEN
        ZRCENTRA=RCENTRA5
        ZRCENTRB=RCENTRB5
        ZRCENTRC=RCENTRC5

        ZRCONSTAVG=RCONSTAVG5
        ZRCONSTSTD=RCONSTSTD5
        
        ZRMLRI=RMLRI5
        ZRMLRA=RMLRA5
        ZRMLRB=RMLRB5
        ZRMLRC=RMLRC5
        ZRMLRD=RMLRD5
        ZRMLRE=RMLRE5
      ELSEIF (KLEVSNA(JL)==2)THEN
        ZRCENTRA=RCENTRA2
        ZRCENTRB=RCENTRB2
        ZRCENTRC=RCENTRC2

        ZRCONSTAVG=RCONSTAVG2
        ZRCONSTSTD=RCONSTSTD2
        
        ZRMLRI=RMLRI2
        ZRMLRA=RMLRA2
        ZRMLRB=RMLRB2
        ZRMLRC=RMLRC2
        ZRMLRD=RMLRD2
        ZRMLRE=RMLRE2
      ELSEIF (KLEVSNA(JL)==3)THEN
        ZRCENTRA=RCENTRA3
        ZRCENTRB=RCENTRB3
        ZRCENTRC=RCENTRC3

        ZRCONSTAVG=RCONSTAVG3
        ZRCONSTSTD=RCONSTSTD3
        
        ZRMLRI=RMLRI3
        ZRMLRA=RMLRA3
        ZRMLRB=RMLRB3
        ZRMLRC=RMLRC3
        ZRMLRD=RMLRD3
        ZRMLRE=RMLRE3
      ELSEIF (KLEVSNA(JL)==4)THEN
        ZRCENTRA=RCENTRA4
        ZRCENTRB=RCENTRB4
        ZRCENTRC=RCENTRC4

        ZRCONSTAVG=RCONSTAVG4
        ZRCONSTSTD=RCONSTSTD4
        
        ZRMLRI=RMLRI4
        ZRMLRA=RMLRA4
        ZRMLRB=RMLRB4
        ZRMLRC=RMLRC4
        ZRMLRD=RMLRD4
        ZRMLRE=RMLRE4
      ELSEIF (KLEVSNA(JL)==5)THEN
        ZRCENTRA=RCENTRA5
        ZRCENTRB=RCENTRB5
        ZRCENTRC=RCENTRC5

        ZRCONSTAVG=RCONSTAVG5
        ZRCONSTSTD=RCONSTSTD5
        
        ZRMLRI=RMLRI5
        ZRMLRA=RMLRA5
        ZRMLRB=RMLRB5
        ZRMLRC=RMLRC5
        ZRMLRD=RMLRD5
        ZRMLRE=RMLRE5
      ENDIF

      IF (PSDOR(JL)<ZSDORTHR)THEN
        KLEVMID(JL)=3_JPIM
      ELSEIF (KLEVSNA(JL)<KLEVSN .AND. KLEVSNA(JL)>2)THEN
        KLEVMID(JL)=KLEVSNA(JL)-1
      ELSEIF (KLEVSNA(JL)==2)THEN
        KLEVMID(JL)=KLEVSNA(JL)
      ELSE
        KLEVMID(JL)=3_JPIM
      ENDIF

    ELSEIF ( ZDSNTOT(JL) >= 0.50_JPRB ) THEN 
      ZRCENTRA=RCENTRA5M
      ZRCENTRB=RCENTRB5M
      ZRCENTRC=RCENTRC5M

      ZRCONSTAVG=RCONSTAVG5M
      ZRCONSTSTD=RCONSTSTD5M
      
      ZRMLRI=RMLRI5M
      ZRMLRA=RMLRA5M
      ZRMLRB=RMLRB5M
      ZRMLRC=RMLRC5M
      ZRMLRD=RMLRD5M
      ZRMLRE=RMLRE5M

      KLEVMID(JL)=MAX(KLEVSNA(JL)-1,1)
     
    ENDIF
! Final assignment:
!  RCENTRA(JL)=ZRCENTRA
!  RCENTRB(JL)=ZRCENTRB
!  RCENTRC(JL)=ZRCENTRC

   PRCONSTAVG(JL,1:JPNCL)=ZRCONSTAVG(1:JPNCL)
   PRCONSTSTD(JL,1:JPNCL)=ZRCONSTSTD(1:JPNCL)
   
!  RMLRI(JL)=ZRMLRI
!  RMLRA(JL)=ZRMLRA
!  RMLRB(JL)=ZRMLRB
!  RMLRC(JL)=ZRMLRC
!  RMLRD(JL)=ZRMLRD
!  RMLRE(JL)=ZRMLRE

!*****************************************
! 2.1 Find closest cluster centre using euclidean metric:
!     Features are common to temp and dens
   ZFEATA=PTSOIL(JL)-PTSKIN(JL)
   ZFEATB=PTSOIL(JL)-PTSN(JL)
   ZFEATC=PRSN(JL)/RHOMINSND

! 2.1.1 Temperature
   KCL = 1
   ZTDIST(KCL)=SQRT( (ZFEATA-ZTCENTRA(KCL))**2_JPRB + (ZFEATB-ZTCENTRB(KCL))**2_JPRB + (ZFEATC-ZTCENTRC(KCL))**2_JPRB )
   ZTDIST_MIN = ZTDIST(KCL)
   PTMINCL(JL) = KCL
   DO KCL=2,JPNCL
     ZTDIST(KCL)=SQRT( (ZFEATA-ZTCENTRA(KCL))**2_JPRB + (ZFEATB-ZTCENTRB(KCL))**2_JPRB + (ZFEATC-ZTCENTRC(KCL))**2_JPRB )
     IF( ZTDIST(KCL) < ZTDIST_MIN )THEN
       PTMINCL(JL) = KCL
       ZTDIST_MIN = ZTDIST(KCL)
     ENDIF
   ENDDO

! 2.1.2 Density:
   KCL = 1
   ZRDIST(KCL)=SQRT( (ZFEATA-ZRCENTRA(KCL))**2_JPRB + (ZFEATB-ZRCENTRB(KCL))**2_JPRB + (ZFEATC-ZRCENTRC(KCL))**2_JPRB )
   ZRDIST_MIN = ZRDIST(KCL)
   PRMINCL(JL) = KCL
   DO KCL=2,JPNCL
     ZRDIST(KCL)=SQRT( (ZFEATA-ZRCENTRA(KCL))**2_JPRB + (ZFEATB-ZRCENTRB(KCL))**2_JPRB + (ZFEATC-ZRCENTRC(KCL))**2_JPRB )
     IF( ZRDIST(KCL) < ZRDIST_MIN )THEN
       PRMINCL(JL) = KCL
       ZRDIST_MIN = ZRDIST(KCL)
     ENDIF
   ENDDO

!*****************************************
! 2.1 Initialize warm start (WS) variables
    IF ( PSSN(JL) < ZSNPERT .AND. LDLAND(JL)) THEN
        PTSNWS(JL, 1)          = MIN(RTT,PTSKIN(JL))
        PTSNWS(JL, 2:KLEVSN-1) = PTSN(JL)
        PTSNWS(JL, KLEVSN)     = MIN(RTT,PTSOIL(JL))

        PRSNWS(JL, 1:KLEVSN) = PRSN(JL)
        PRSNMAX(JL)          = RHOMAXSN_NEW

        PSSNWS(JL, 1)        = PSSN(JL)
        PWSNWS(JL, 1)        = PWSN(JL)
        PSSNWS(JL, 2:KLEVSN) = 0._JPRB
        PWSNWS(JL, 2:KLEVSN) = 0._JPRB

!----- Density
        ZP1=ZRMLRI(PRMINCL(JL))+ZRMLRA(PRMINCL(JL))*PASN(JL)
        ZP2=                    ZRMLRB(PRMINCL(JL))*PRSN(JL)
        ZP3=                    ZRMLRC(PRMINCL(JL))*PTSN(JL)
        ZP4=                    ZRMLRD(PRMINCL(JL))*(PRSN(JL))**2_JPRB
        ZP5=                    ZRMLRE(PRMINCL(JL))*(PASN(JL))**2_JPRB

        PRSNTOP(JL)=ZP1+ZP2+ZP3+ZP4+ZP5

        PTSNBOTTOM(JL)=MIN(RTT,PTSOIL(JL))
        PTSNMIDDLE(JL)=MIN(RTT,0.5_JPRB*(PTSN(JL)+PTSOIL(JL)))
        IF ( ZDSNTOT(JL) < 0.20_JPRB ) THEN 
          PTSNTOP(JL) = PTSN(JL)
        ELSE
          PTSNTOP(JL) = PTSKIN(JL)
        ENDIF
        IF ( ZDSNTOT(JL) < 0.20_JPRB ) THEN
          ZSADEPTH(JL)=0._JPRB
        ELSE
          ZSADEPTH(JL)=0.5_JPRB*RDAT(1)
          IF (PTSNTOP(JL)>=PTSN(JL)) THEN
            ZACTDEPTH(JL)=ZSNDEPTH(JL, MAX(KLEVSNA(JL)-1, 1))
          ELSE
            ZACTDEPTH(JL)=ZDSNTOT(JL)
          ENDIF
        ENDIF

    ELSE ! Glacier or sea-ice ini
        KLEVSNA(JL)=KLEVSN !KSNACC+1

        PTSNWS(JL, 1)        = MIN(RTT,PTSKIN(JL))
        PTSNWS(JL, 2:KLEVSN) = MIN(RTT,PTSN(JL))

        PRSNMAX(JL)              = 300._JPRB
        PRSNWS(JL,1:KLEVSN)      = PRSNMAX(JL)
       !PRSNWS(JL,KSNACC:KLEVSN) = PRSNMAX(JL)
        PSSNWS(JL,1:KLEVSN)      = PRSNMAX(JL)*ZDSNR(JL,1:KLEVSN)
        PWSNWS(JL,1:KLEVSN)      = 0._JPRB
!----- Active depth
      PTSNTOP(JL)   = PTSKIN(JL)
      IF (PTSNTOP(JL)>=PTSN(JL)) THEN
        ZACTDEPTH(JL)=ZSNDEPTH(JL, KLEVSNA(JL))
      ELSE
        ZACTDEPTH(JL)=ZDSNTOT(JL)
      ENDIF

        PTSNMIDDLE(JL)= MIN(RTT,0.5_JPRB*(PTSN(JL)+PTSOIL(JL)))
        PTSNBOTTOM(JL)= MIN(PTSOIL(JL),RTT)
!----- Density
      PRSNTOP(JL)=300._JPRB
    ENDIF
  ENDDO 

END ASSOCIATE

!    -----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('SURFWS_INIT_SL_MOD:SURFWS_INIT_SL',1,ZHOOK_HANDLE)

END SUBROUTINE SURFWS_INIT_SL
END MODULE SURFWS_INIT_SL_MOD


