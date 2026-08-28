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
        DO KCL=1,JPNCL
          ZTCENTRA(KCL)=TCENTRADAY2(KCL)
          ZTCENTRB(KCL)=TCENTRBDAY2(KCL)
          ZTCENTRC(KCL)=TCENTRCDAY2(KCL)
          ZTCONSTAVG(KCL)=TCONSTAVGDAY2(KCL)
          ZTCONSTSTD(KCL)=TCONSTSTDDAY2(KCL)
        ENDDO
        KLEVMID(JL)=MAX(KLEVSNA(JL)-1,1)

    IF ( PSSN(JL) < ZSNPERT .AND. LDLAND(JL) ) THEN ! seasonal snow
      IF ( ZDSNTOT(JL) < 0.15_JPRB ) THEN
        DO KCL=1,JPNCL
          ZTCENTRA(KCL)=TCENTRADAY2(KCL)
          ZTCENTRB(KCL)=TCENTRBDAY2(KCL)
          ZTCENTRC(KCL)=TCENTRCDAY2(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTCONSTAVG(KCL)=TCONSTAVGDAY2(KCL)
          ZTCONSTSTD(KCL)=TCONSTSTDDAY2(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTMLRI(KCL)=TMLRIDAY2(KCL)
          ZTMLRA(KCL)=TMLRADAY2(KCL)
          ZTMLRB(KCL)=TMLRBDAY2(KCL)
          ZTMLRC(KCL)=TMLRCDAY2(KCL)
          ZTMLRD(KCL)=TMLRDDAY2(KCL)
          ZTMLRE(KCL)=TMLREDAY2(KCL)
        ENDDO

      ELSEIF ( ZDSNTOT(JL) < 0.20_JPRB ) THEN
        DO KCL=1,JPNCL
          ZTCENTRA(KCL)=TCENTRADAY3(KCL)
          ZTCENTRB(KCL)=TCENTRBDAY3(KCL)
          ZTCENTRC(KCL)=TCENTRCDAY3(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTCONSTAVG(KCL)=TCONSTAVGDAY3(KCL)
          ZTCONSTSTD(KCL)=TCONSTSTDDAY3(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTMLRI(KCL)=TMLRIDAY3(KCL)
          ZTMLRA(KCL)=TMLRADAY3(KCL)
          ZTMLRB(KCL)=TMLRBDAY3(KCL)
          ZTMLRC(KCL)=TMLRCDAY3(KCL)
          ZTMLRD(KCL)=TMLRDDAY3(KCL)
          ZTMLRE(KCL)=TMLREDAY3(KCL)
        ENDDO

      ELSEIF ( ZDSNTOT(JL) < 0.25_JPRB ) THEN
        DO KCL=1,JPNCL
          ZTCENTRA(KCL)=TCENTRADAY4(KCL)
          ZTCENTRB(KCL)=TCENTRBDAY4(KCL)
          ZTCENTRC(KCL)=TCENTRCDAY4(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTCONSTAVG(KCL)=TCONSTAVGDAY4(KCL)
          ZTCONSTSTD(KCL)=TCONSTSTDDAY4(KCL)
        ENDDO

      ELSEIF ( ZDSNTOT(JL) < 0.50_JPRB ) THEN
        IF (PSDOR(JL)<ZSDORTHR)THEN
          DO KCL=1,JPNCL
            ZTCENTRA(KCL)=TCENTRADAY5(KCL)
            ZTCENTRB(KCL)=TCENTRBDAY5(KCL)
            ZTCENTRC(KCL)=TCENTRCDAY5(KCL)
          ENDDO

          DO KCL=1,JPNCL
            ZTCONSTAVG(KCL)=TCONSTAVGDAY5(KCL)
            ZTCONSTSTD(KCL)=TCONSTSTDDAY5(KCL)
          ENDDO
        ELSEIF (KLEVSNA(JL)==2)THEN
          DO KCL=1,JPNCL
            ZTCENTRA(KCL)=TCENTRADAY2(KCL)
            ZTCENTRB(KCL)=TCENTRBDAY2(KCL)
            ZTCENTRC(KCL)=TCENTRCDAY2(KCL)
          ENDDO

          DO KCL=1,JPNCL
            ZTCONSTAVG(KCL)=TCONSTAVGDAY2(KCL)
            ZTCONSTSTD(KCL)=TCONSTSTDDAY2(KCL)
          ENDDO
        ELSEIF (KLEVSNA(JL)==3)THEN
          DO KCL=1,JPNCL
            ZTCENTRA(KCL)=TCENTRADAY3(KCL)
            ZTCENTRB(KCL)=TCENTRBDAY3(KCL)
            ZTCENTRC(KCL)=TCENTRCDAY3(KCL)
          ENDDO

          DO KCL=1,JPNCL
            ZTCONSTAVG(KCL)=TCONSTAVGDAY3(KCL)
            ZTCONSTSTD(KCL)=TCONSTSTDDAY3(KCL)
          ENDDO

        ELSEIF (KLEVSNA(JL)==4)THEN
          DO KCL=1,JPNCL
            ZTCENTRA(KCL)=TCENTRADAY4(KCL)
            ZTCENTRB(KCL)=TCENTRBDAY4(KCL)
            ZTCENTRC(KCL)=TCENTRCDAY4(KCL)
          ENDDO

          DO KCL=1,JPNCL
            ZTCONSTAVG(KCL)=TCONSTAVGDAY4(KCL)
            ZTCONSTSTD(KCL)=TCONSTSTDDAY4(KCL)
          ENDDO
        ELSEIF (KLEVSNA(JL)==5)THEN
          DO KCL=1,JPNCL
            ZTCENTRA(KCL)=TCENTRADAY5(KCL)
            ZTCENTRB(KCL)=TCENTRBDAY5(KCL)
            ZTCENTRC(KCL)=TCENTRCDAY5(KCL)
          ENDDO

          DO KCL=1,JPNCL
            ZTCONSTAVG(KCL)=TCONSTAVGDAY5(KCL)
            ZTCONSTSTD(KCL)=TCONSTSTDDAY5(KCL)
          ENDDO
        ENDIF
      ELSEIF ( ZDSNTOT(JL) >= 0.50_JPRB ) THEN
        DO KCL=1,JPNCL
          ZTCENTRA(KCL)=TCENTRADAY5M(KCL)
          ZTCENTRB(KCL)=TCENTRBDAY5M(KCL)
          ZTCENTRC(KCL)=TCENTRCDAY5M(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTCONSTAVG(KCL)=TCONSTAVGDAY5M(KCL)
          ZTCONSTSTD(KCL)=TCONSTSTDDAY5M(KCL)
        ENDDO
      ENDIF
    ELSE ! Glaciers, daytime
        DO KCL=1,JPNCL
          ZTCENTRA(KCL)=TCENTRADAY5G(KCL)
          ZTCENTRB(KCL)=TCENTRBDAY5G(KCL)
          ZTCENTRC(KCL)=TCENTRCDAY5G(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTCONSTAVG(KCL)=TCONSTAVGDAY5G(KCL)
          ZTCONSTSTD(KCL)=TCONSTSTDDAY5G(KCL)
        ENDDO
    ENDIF
    ELSEIF (PMU0(JL)<=ZEPSILON) THEN !nighttime
        ! Initialise values to avoid floating point errors
        DO KCL=1,JPNCL
          ZTCENTRA(KCL)=TCENTRANIGHT2(KCL)
          ZTCENTRB(KCL)=TCENTRBNIGHT2(KCL)
          ZTCENTRC(KCL)=TCENTRCNIGHT2(KCL)
          ZTCONSTAVG(KCL)=TCONSTAVGNIGHT2(KCL)
          ZTCONSTSTD(KCL)=TCONSTSTDNIGHT2(KCL)
        ENDDO
        KLEVMID(JL)=MAX(KLEVSNA(JL)-1,1)

    IF ( PSSN(JL) < ZSNPERT .AND. LDLAND(JL) ) THEN ! seasonal snow
      IF ( ZDSNTOT(JL) < 0.15_JPRB ) THEN
        DO KCL=1,JPNCL
          ZTCENTRA(KCL)=TCENTRANIGHT2(KCL)
          ZTCENTRB(KCL)=TCENTRBNIGHT2(KCL)
          ZTCENTRC(KCL)=TCENTRCNIGHT2(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTCONSTAVG(KCL)=TCONSTAVGNIGHT2(KCL)
          ZTCONSTSTD(KCL)=TCONSTSTDNIGHT2(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTMLRI(KCL)=TMLRINIGHT2(KCL)
          ZTMLRA(KCL)=TMLRANIGHT2(KCL)
          ZTMLRB(KCL)=TMLRBNIGHT2(KCL)
          ZTMLRC(KCL)=TMLRCNIGHT2(KCL)
          ZTMLRD(KCL)=TMLRDNIGHT2(KCL)
          ZTMLRE(KCL)=TMLRENIGHT2(KCL)
        ENDDO

      ELSEIF ( ZDSNTOT(JL) < 0.20_JPRB ) THEN
        DO KCL=1,JPNCL
          ZTCENTRA(KCL)=TCENTRANIGHT3(KCL)
          ZTCENTRB(KCL)=TCENTRBNIGHT3(KCL)
          ZTCENTRC(KCL)=TCENTRCNIGHT3(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTCONSTAVG(KCL)=TCONSTAVGNIGHT3(KCL)
          ZTCONSTSTD(KCL)=TCONSTSTDNIGHT3(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTMLRI(KCL)=TMLRINIGHT3(KCL)
          ZTMLRA(KCL)=TMLRANIGHT3(KCL)
          ZTMLRB(KCL)=TMLRBNIGHT3(KCL)
          ZTMLRC(KCL)=TMLRCNIGHT3(KCL)
          ZTMLRD(KCL)=TMLRDNIGHT3(KCL)
          ZTMLRE(KCL)=TMLRENIGHT3(KCL)
        ENDDO

      ELSEIF ( ZDSNTOT(JL) < 0.25_JPRB ) THEN
        DO KCL=1,JPNCL
          ZTCENTRA(KCL)=TCENTRANIGHT4(KCL)
          ZTCENTRB(KCL)=TCENTRBNIGHT4(KCL)
          ZTCENTRC(KCL)=TCENTRCNIGHT4(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTCONSTAVG(KCL)=TCONSTAVGNIGHT4(KCL)
          ZTCONSTSTD(KCL)=TCONSTSTDNIGHT4(KCL)
        ENDDO
        
      ELSEIF ( ZDSNTOT(JL) < 0.50_JPRB ) THEN
        IF (PSDOR(JL)<ZSDORTHR)THEN
          DO KCL=1,JPNCL
            ZTCENTRA(KCL)=TCENTRANIGHT5(KCL)
            ZTCENTRB(KCL)=TCENTRBNIGHT5(KCL)
            ZTCENTRC(KCL)=TCENTRCNIGHT5(KCL)
          ENDDO

          DO KCL=1,JPNCL
            ZTCONSTAVG(KCL)=TCONSTAVGNIGHT5(KCL)
            ZTCONSTSTD(KCL)=TCONSTSTDNIGHT5(KCL)
          ENDDO
        ELSEIF (KLEVSNA(JL)==2)THEN
          DO KCL=1,JPNCL
            ZTCENTRA(KCL)=TCENTRANIGHT2(KCL)
            ZTCENTRB(KCL)=TCENTRBNIGHT2(KCL)
            ZTCENTRC(KCL)=TCENTRCNIGHT2(KCL)
          ENDDO

          DO KCL=1,JPNCL
            ZTCONSTAVG(KCL)=TCONSTAVGNIGHT2(KCL)
            ZTCONSTSTD(KCL)=TCONSTSTDNIGHT2(KCL)
          ENDDO
        ELSEIF (KLEVSNA(JL)==3)THEN
          DO KCL=1,JPNCL
            ZTCENTRA(KCL)=TCENTRANIGHT3(KCL)
            ZTCENTRB(KCL)=TCENTRBNIGHT3(KCL)
            ZTCENTRC(KCL)=TCENTRCNIGHT3(KCL)
          ENDDO

          DO KCL=1,JPNCL
            ZTCONSTAVG(KCL)=TCONSTAVGNIGHT3(KCL)
            ZTCONSTSTD(KCL)=TCONSTSTDNIGHT3(KCL)
          ENDDO
        ELSEIF (KLEVSNA(JL)==4)THEN
          DO KCL=1,JPNCL
            ZTCENTRA(KCL)=TCENTRANIGHT4(KCL)
            ZTCENTRB(KCL)=TCENTRBNIGHT4(KCL)
            ZTCENTRC(KCL)=TCENTRCNIGHT4(KCL)
          ENDDO

          DO KCL=1,JPNCL
            ZTCONSTAVG(KCL)=TCONSTAVGNIGHT4(KCL)
            ZTCONSTSTD(KCL)=TCONSTSTDNIGHT4(KCL)
          ENDDO
        ELSEIF (KLEVSNA(JL)==5)THEN
          DO KCL=1,JPNCL
            ZTCENTRA(KCL)=TCENTRANIGHT5(KCL)
            ZTCENTRB(KCL)=TCENTRBNIGHT5(KCL)
            ZTCENTRC(KCL)=TCENTRCNIGHT5(KCL)
          ENDDO

          DO KCL=1,JPNCL
            ZTCONSTAVG(KCL)=TCONSTAVGNIGHT5(KCL)
            ZTCONSTSTD(KCL)=TCONSTSTDNIGHT5(KCL)
          ENDDO
        ENDIF

      ELSEIF ( ZDSNTOT(JL) >= 0.50_JPRB ) THEN
        DO KCL=1,JPNCL
          ZTCENTRA(KCL)=TCENTRANIGHT5M(KCL)
          ZTCENTRB(KCL)=TCENTRBNIGHT5M(KCL)
          ZTCENTRC(KCL)=TCENTRCNIGHT5M(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTCONSTAVG(KCL)=TCONSTAVGNIGHT5M(KCL)
          ZTCONSTSTD(KCL)=TCONSTSTDNIGHT5M(KCL)
        ENDDO
      ENDIF
    ELSE !Glaciers, nighttime
        DO KCL=1,JPNCL
          ZTCENTRA(KCL)=TCENTRANIGHT5G(KCL)
          ZTCENTRB(KCL)=TCENTRBNIGHT5G(KCL)
          ZTCENTRC(KCL)=TCENTRCNIGHT5G(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZTCONSTAVG(KCL)=TCONSTAVGNIGHT5G(KCL)
          ZTCONSTSTD(KCL)=TCONSTSTDNIGHT5G(KCL)
        ENDDO
    ENDIF
    ENDIF

    DO KCL=1,JPNCL
      PTCONSTAVG(JL,KCL)=ZTCONSTAVG(KCL)
      PTCONSTSTD(JL,KCL)=ZTCONSTSTD(KCL)
    ENDDO

! Assign density depending on snow depth::
    ! Initialise arrays, to avoid undesired effects.
    DO KCL=1,JPNCL
      ZRMLRA(KCL)=RMLRA2(KCL)
      ZRMLRB(KCL)=RMLRB2(KCL)
      ZRMLRC(KCL)=RMLRC2(KCL)
      ZRMLRD(KCL)=RMLRD2(KCL)
      ZRMLRE(KCL)=RMLRE2(KCL)
    ENDDO
    IF ( ZDSNTOT(JL) < 0.15_JPRB ) THEN
      DO KCL=1,JPNCL
        ZRCENTRA(KCL)=RCENTRA2(KCL)
        ZRCENTRB(KCL)=RCENTRB2(KCL)
        ZRCENTRC(KCL)=RCENTRC2(KCL)
      ENDDO

      DO KCL=1,JPNCL
        ZRCONSTAVG(KCL)=RCONSTAVG2(KCL)
        ZRCONSTSTD(KCL)=RCONSTSTD2(KCL)
      ENDDO

      DO KCL=1,JPNCL
        ZRMLRI(KCL)=RMLRI2(KCL)
        ZRMLRA(KCL)=RMLRA2(KCL)
        ZRMLRB(KCL)=RMLRB2(KCL)
        ZRMLRC(KCL)=RMLRC2(KCL)
        ZRMLRD(KCL)=RMLRD2(KCL)
        ZRMLRE(KCL)=RMLRE2(KCL)
      ENDDO

      KLEVMID(JL)=2_JPIM
    ELSEIF ( ZDSNTOT(JL) < 0.20_JPRB ) THEN
      DO KCL=1,JPNCL
        ZRCENTRA(KCL)=RCENTRA3(KCL)
        ZRCENTRB(KCL)=RCENTRB3(KCL)
        ZRCENTRC(KCL)=RCENTRC3(KCL)
      ENDDO

      DO KCL=1,JPNCL
        ZRCONSTAVG(KCL)=RCONSTAVG3(KCL)
        ZRCONSTSTD(KCL)=RCONSTSTD3(KCL)
      ENDDO

      DO KCL=1,JPNCL
        ZRMLRI(KCL)=RMLRI3(KCL)
        ZRMLRA(KCL)=RMLRA3(KCL)
        ZRMLRB(KCL)=RMLRB3(KCL)
        ZRMLRC(KCL)=RMLRC3(KCL)
        ZRMLRD(KCL)=RMLRD3(KCL)
        ZRMLRE(KCL)=RMLRE3(KCL)
      ENDDO

      KLEVMID(JL)=2_JPIM

    ELSEIF ( ZDSNTOT(JL) < 0.25_JPRB ) THEN
      DO KCL=1,JPNCL
        ZRCENTRA(KCL)=RCENTRA4(KCL)
        ZRCENTRB(KCL)=RCENTRB4(KCL)
        ZRCENTRC(KCL)=RCENTRC4(KCL)
      ENDDO

      DO KCL=1,JPNCL
        ZRCONSTAVG(KCL)=RCONSTAVG4(KCL)
        ZRCONSTSTD(KCL)=RCONSTSTD4(KCL)
      ENDDO

      DO KCL=1,JPNCL
        ZRMLRI(KCL)=RMLRI4(KCL)
        ZRMLRA(KCL)=RMLRA4(KCL)
        ZRMLRB(KCL)=RMLRB4(KCL)
        ZRMLRC(KCL)=RMLRC4(KCL)
        ZRMLRD(KCL)=RMLRD4(KCL)
        ZRMLRE(KCL)=RMLRE4(KCL)
      ENDDO

      KLEVMID(JL)=3_JPIM

    ELSEIF ( ZDSNTOT(JL) < 0.50_JPRB ) THEN
      IF (PSDOR(JL)<ZSDORTHR)THEN
        DO KCL=1,JPNCL
          ZRCENTRA(KCL)=RCENTRA5(KCL)
          ZRCENTRB(KCL)=RCENTRB5(KCL)
          ZRCENTRC(KCL)=RCENTRC5(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZRCONSTAVG(KCL)=RCONSTAVG5(KCL)
          ZRCONSTSTD(KCL)=RCONSTSTD5(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZRMLRI(KCL)=RMLRI5(KCL)
          ZRMLRA(KCL)=RMLRA5(KCL)
          ZRMLRB(KCL)=RMLRB5(KCL)
          ZRMLRC(KCL)=RMLRC5(KCL)
          ZRMLRD(KCL)=RMLRD5(KCL)
          ZRMLRE(KCL)=RMLRE5(KCL)
        ENDDO
      ELSEIF (KLEVSNA(JL)==2)THEN
        DO KCL=1,JPNCL
          ZRCENTRA(KCL)=RCENTRA2(KCL)
          ZRCENTRB(KCL)=RCENTRB2(KCL)
          ZRCENTRC(KCL)=RCENTRC2(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZRCONSTAVG(KCL)=RCONSTAVG2(KCL)
          ZRCONSTSTD(KCL)=RCONSTSTD2(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZRMLRI(KCL)=RMLRI2(KCL)
          ZRMLRA(KCL)=RMLRA2(KCL)
          ZRMLRB(KCL)=RMLRB2(KCL)
          ZRMLRC(KCL)=RMLRC2(KCL)
          ZRMLRD(KCL)=RMLRD2(KCL)
          ZRMLRE(KCL)=RMLRE2(KCL)
        ENDDO
      ELSEIF (KLEVSNA(JL)==3)THEN
        DO KCL=1,JPNCL
          ZRCENTRA(KCL)=RCENTRA3(KCL)
          ZRCENTRB(KCL)=RCENTRB3(KCL)
          ZRCENTRC(KCL)=RCENTRC3(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZRCONSTAVG(KCL)=RCONSTAVG3(KCL)
          ZRCONSTSTD(KCL)=RCONSTSTD3(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZRMLRI(KCL)=RMLRI3(KCL)
          ZRMLRA(KCL)=RMLRA3(KCL)
          ZRMLRB(KCL)=RMLRB3(KCL)
          ZRMLRC(KCL)=RMLRC3(KCL)
          ZRMLRD(KCL)=RMLRD3(KCL)
          ZRMLRE(KCL)=RMLRE3(KCL)
        ENDDO
      ELSEIF (KLEVSNA(JL)==4)THEN
        DO KCL=1,JPNCL
          ZRCENTRA(KCL)=RCENTRA4(KCL)
          ZRCENTRB(KCL)=RCENTRB4(KCL)
          ZRCENTRC(KCL)=RCENTRC4(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZRCONSTAVG(KCL)=RCONSTAVG4(KCL)
          ZRCONSTSTD(KCL)=RCONSTSTD4(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZRMLRI(KCL)=RMLRI4(KCL)
          ZRMLRA(KCL)=RMLRA4(KCL)
          ZRMLRB(KCL)=RMLRB4(KCL)
          ZRMLRC(KCL)=RMLRC4(KCL)
          ZRMLRD(KCL)=RMLRD4(KCL)
          ZRMLRE(KCL)=RMLRE4(KCL)
        ENDDO
      ELSEIF (KLEVSNA(JL)==5)THEN
        DO KCL=1,JPNCL
          ZRCENTRA(KCL)=RCENTRA5(KCL)
          ZRCENTRB(KCL)=RCENTRB5(KCL)
          ZRCENTRC(KCL)=RCENTRC5(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZRCONSTAVG(KCL)=RCONSTAVG5(KCL)
          ZRCONSTSTD(KCL)=RCONSTSTD5(KCL)
        ENDDO

        DO KCL=1,JPNCL
          ZRMLRI(KCL)=RMLRI5(KCL)
          ZRMLRA(KCL)=RMLRA5(KCL)
          ZRMLRB(KCL)=RMLRB5(KCL)
          ZRMLRC(KCL)=RMLRC5(KCL)
          ZRMLRD(KCL)=RMLRD5(KCL)
          ZRMLRE(KCL)=RMLRE5(KCL)
        ENDDO
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
      DO KCL=1,JPNCL
        ZRCENTRA(KCL)=RCENTRA5M(KCL)
        ZRCENTRB(KCL)=RCENTRB5M(KCL)
        ZRCENTRC(KCL)=RCENTRC5M(KCL)
      ENDDO

      DO KCL=1,JPNCL
        ZRCONSTAVG(KCL)=RCONSTAVG5M(KCL)
        ZRCONSTSTD(KCL)=RCONSTSTD5M(KCL)
      ENDDO

      DO KCL=1,JPNCL
        ZRMLRI(KCL)=RMLRI5M(KCL)
        ZRMLRA(KCL)=RMLRA5M(KCL)
        ZRMLRB(KCL)=RMLRB5M(KCL)
        ZRMLRC(KCL)=RMLRC5M(KCL)
        ZRMLRD(KCL)=RMLRD5M(KCL)
        ZRMLRE(KCL)=RMLRE5M(KCL)
      ENDDO

      KLEVMID(JL)=MAX(KLEVSNA(JL)-1,1)

    ENDIF
! Final assignment:
!  RCENTRA(JL)=ZRCENTRA
!  RCENTRB(JL)=ZRCENTRB
!  RCENTRC(JL)=ZRCENTRC

   DO KCL=1,JPNCL
     PRCONSTAVG(JL,KCL)=ZRCONSTAVG(KCL)
     PRCONSTSTD(JL,KCL)=ZRCONSTSTD(KCL)
   ENDDO
   
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
        DO JK=2,KLEVSN-1
          PTSNWS(JL, JK) = PTSN(JL)
        ENDDO
        PTSNWS(JL, KLEVSN)     = MIN(RTT,PTSOIL(JL))

        DO JK=1,KLEVSN
          PRSNWS(JL, JK) = PRSN(JL)
        ENDDO
        PRSNMAX(JL)          = RHOMAXSN_NEW

        PSSNWS(JL, 1)        = PSSN(JL)
        PWSNWS(JL, 1)        = PWSN(JL)
        DO JK=2,KLEVSN
          PSSNWS(JL, JK) = 0._JPRB
          PWSNWS(JL, JK) = 0._JPRB
        ENDDO

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
        DO JK=2,KLEVSN
          PTSNWS(JL, JK) = MIN(RTT,PTSN(JL))
        ENDDO

        PRSNMAX(JL)              = 300._JPRB
        DO JK=1,KLEVSN
          PRSNWS(JL,JK)      = PRSNMAX(JL)
         !PRSNWS(JL,KSNACC:KLEVSN) = PRSNMAX(JL)
          PSSNWS(JL,JK)      = PRSNMAX(JL)*ZDSNR(JL,JK)
          PWSNWS(JL,JK)      = 0._JPRB
        ENDDO
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


