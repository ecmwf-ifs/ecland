SUBROUTINE SURFBC    (YSURF,KIDIA,KFDIA,KLON,KTILES,KLEVSN,&
 & PTVL   , PCO2TYP, PTVH   ,PSOTY  ,PSDOR ,PCVLC ,PCVHC ,PCURC,&
 & PLAILC  ,PLAIHC, PLAILI, PLAIHI,&
 & PLSM   ,PCI    ,PCLAKE ,PHLICE,&
 & PGEMU  ,PSNM1M ,PWLM1M ,PRSNM1M,&  
 & LDLAND ,LDSICE ,LDLAKE ,LDNH, LDOCN_KPP,&
 & KTVL   ,KCO2TYP, KTVH   ,KSOTY,&
 & PCVL   ,PCVH, PCUR, PLAIL, PLAIH,  PWLMX  ,PFRTI)

! (C) Copyright 2001- ECMWF.
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
!     *PCO2TYP*     TYPE OF PHOTOSYNTHETIC PATHWAY FOR LOW VEGETATION (C3/C4) (REAL)
!     *PTVH*         HIGH VEGETATION TYPE (REAL)
!     *PSOTY*        SOIL TYPE (REAL)                               (1-7)
!     *PSDOR*        STANDARD DEV. OF OROGRAPHY (REAL)              (m)

!     INPUT PARAMETERS (REAL):
!     *PCVLC*        LOW VEGETATION COVER  (CLIMATE)                (0-1)
!     *PCVHC*        HIGH VEGETATION COVER (CLIMATE)                (0-1)
!     *PCURC*        URBAN COVER (CLIMATE - PASSIVE)                (0-1)
!     *PLAILC*        LOW LAI (CLIMATE)                             m2/m2
!     *PLAIHC*        HIGH LAI (CLIMATE)                            m2/m2
!     *PLSM*         LAND-SEA MASK                                  (0-1)
!     *PCI*          SEA-ICE FRACTION                               (0-1)
!     *PCLAKE*       LAKE FRACTION                                  (0-1)
!     *PHLICE*       LAKE ICE THICKNESS                               m 
!     *PGEMU*        COSINE OF LATITUDE
!     *PSNM1M*       SNOW MASS (per unit area)                      kg/m**2
!     *PWLM1M*       INTERCEPTION RESERVOIR CONTENTS                kg/m**2
!     *PRSNM1M*      SNOW DENSITY                                   kg/m**3

!     OUTPUT PARAMETERS (LOGICAL):
!     *LDLAND*       LAND INDICATOR
!     *LDSICE*       SEA-ICE INDICATOR
!     *LDLAKE*       LAKE INDICATOR
!     *LDNH*         NORTHERN HEMISPHERE INDICATOR
!     *LDOCN_KPP*    KPP MODEL INDICATOR 

!     OUTPUT PARAMETERS (REAL):
!     *PCVL*         LOW VEGETATION COVER  (CORRECTED)              (0-1)
!     *PCVH*         HIGH VEGETATION COVER (CORRECTED)              (0-1)
!     *PCUR*         URBAN COVER (CORRECTED)                        (0-1)
!     *PLAIL*        LOW LAI (REAL)                                 m2/m2
!     *PLAIH*        HIGH LAI (REAL)                                m2/m2
!     *PWLMX*        MAXIMUM SKIN RESERVOIR CAPACITY                kg/m**2
!     *PFRTI*        TILE FRACTIONS                                 (0-1)
!            1 : WATER                  5 : SNOW ON LOW-VEG+BARE-SOIL
!            2 : ICE                    6 : DRY SNOW-FREE HIGH-VEG
!            3 : WET SKIN               7 : SNOW UNDER HIGH-VEG
!            4 : DRY SNOW-FREE LOW-VEG  8 : BARE SOIL

!     OUTPUT PARAMETERS (INTEGER):
!     *KTVL*         LOW VEGETATION TYPE
!     *KCO2TYP*     TYPE OF PHOTOSYNTHETIC PATHWAY (C3/C4)
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
!     ------------------------------------------------------------------

USE PARKIND1, ONLY : JPIM, JPRB, JPRD
USE, INTRINSIC :: ISO_C_BINDING
USE MPL_MODULE
!ifndef INTERFACE

USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_SURF, ONLY : TSURF

USE ABORT_SURF_MOD
USE SURFBC_CTL_MOD
!endif INTERFACE

IMPLICIT NONE

! Declaration of arguments

TYPE(TSURF)       ,INTENT(IN)    :: YSURF
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTILES 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVSN

REAL(KIND=JPRB),    INTENT(IN)  :: PTVL(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PCO2TYP(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PTVH(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PSOTY(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PSDOR(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PCVLC(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PCVHC(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PCURC(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PLAILC(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PLAIHC(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PLAILI(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PLAIHI(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PLSM(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PCI(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PGEMU(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PSNM1M(KLON,KLEVSN)
REAL(KIND=JPRB),    INTENT(IN)  :: PWLM1M(KLON)
REAL(KIND=JPRB),    INTENT(IN)  :: PRSNM1M(KLON,KLEVSN)
REAL(KIND=JPRB),    INTENT(IN)  :: PCLAKE(KLON) 
REAL(KIND=JPRB),    INTENT(IN)  :: PHLICE(KLON)

LOGICAL,   INTENT(OUT) :: LDLAND(KLON)
LOGICAL,   INTENT(OUT) :: LDSICE(KLON)
LOGICAL,   INTENT(OUT) :: LDNH(KLON)
LOGICAL,   INTENT(OUT) :: LDLAKE(KLON)  
LOGICAL,   INTENT(OUT) :: LDOCN_KPP(KLON) 

INTEGER(KIND=JPIM), INTENT(OUT) :: KTVL(KLON)
INTEGER(KIND=JPIM), INTENT(OUT) :: KCO2TYP(KLON)
INTEGER(KIND=JPIM), INTENT(OUT) :: KTVH(KLON)
INTEGER(KIND=JPIM), INTENT(OUT) :: KSOTY(KLON)

REAL(KIND=JPRB),    INTENT(OUT) :: PCVL(KLON)
REAL(KIND=JPRB),    INTENT(OUT) :: PCVH(KLON)
REAL(KIND=JPRB),    INTENT(OUT) :: PLAIL(KLON)
REAL(KIND=JPRB),    INTENT(OUT) :: PLAIH(KLON)
REAL(KIND=JPRB),    INTENT(OUT) :: PWLMX(KLON)
REAL(KIND=JPRB),    INTENT(OUT) :: PFRTI(KLON,KTILES)

REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCUR(:)
!ifndef INTERFACE

REAL(KIND=JPRB) :: ZTHRESH, ZEPS, ZSUM(KLON), ZSUM2(KLON)
INTEGER(KIND=JPIM) :: IK, JK
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE



IF (LHOOK) CALL DR_HOOK('SURFBC',0,ZHOOK_HANDLE)

CALL SURFBC_CTL   (KIDIA,KFDIA,KLON,KTILES,KLEVSN,&
 & PTVL   ,PCO2TYP, PTVH   ,PSOTY  ,PSDOR,PCVLC  ,PCVHC, PCURC,&
 & PLAILC  ,PLAIHC, PLAILI, PLAIHI,&
 & PLSM   ,PCI    ,PCLAKE ,PHLICE, PGEMU  ,PSNM1M ,PWLM1M,PRSNM1M,&  
 & LDLAND ,LDSICE ,LDLAKE ,LDNH   ,LDOCN_KPP,&
 & KTVL   ,KCO2TYP, KTVH   ,KSOTY  ,&
 & PCVL   ,PCVH   ,PCUR, PLAIL  , PLAIH ,  PWLMX  ,PFRTI,&
 & YSURF%YSOIL, YSURF%YVEG, YSURF%YFLAKE, YSURF%YOCEAN_ML)  

! check correctness of tile fraction computation
ZSUM(:)=0.0_JPRB
ZSUM2(:)=0.0_JPRB
ZEPS=0.01_JPRB
ZTHRESH=1._JPRB+ZEPS
DO IK=1,KTILES
  DO JK=KIDIA,KFDIA
    ZSUM(JK)=ZSUM(JK)+PFRTI(JK,IK)
    ZSUM2(JK)=1._JPRB-PCVL(JK)-PCVH(JK)
  ENDDO
  IF( ANY(PFRTI(KIDIA:KFDIA,IK) < 0._JPRB) ) THEN
    DO JK=KIDIA,KFDIA
      IF( PFRTI(JK,IK) < 0._JPRB ) THEN
        WRITE(*,'(A,I6,A,F8.4)') 'PROBLEM TILE:',IK,' FRACTION:',PFRTI(JK,IK)
      ENDIF
    ENDDO
    CALL ABORT_SURF('SURFBC: TILING FRACTION IS NEGATIVE!')
  ENDIF
ENDDO
IF( ANY(ZSUM(KIDIA:KFDIA) > ZTHRESH) .or. ANY(ZSUM2(KIDIA:KFDIA) < 0._JPRB) ) THEN
  WRITE(*,'(A,F8.4,A,F8.4,A)') ' TILING FRACTION PROBLEM MAX MIN ',&
     & MAXVAL(ZSUM),' ',MINVAL(ZSUM)
  DO JK=KIDIA,KFDIA
    IF( ZSUM(JK) > ZTHRESH .or. ZSUM2(JK) < 0._JPRB ) THEN
      DO IK=1,KTILES
        WRITE(*,'(A,I6,A,6F8.4,4I6)') 'PROBLEM TILE:',IK,' FRACTION:',PFRTI(JK,IK), &
       & PCVL(JK), PCVH(JK) ,PLAIL(JK), PLAIH(JK), PWLMX(JK), &
       & KTVL(JK)   ,KCO2TYP(JK), KTVH(JK)   ,KSOTY(JK)
      ENDDO
    ENDIF
  ENDDO
  CALL ABORT_SURF('SURFBC: TILING FRACTION IS WRONG!')
ENDIF

IF (LHOOK) CALL DR_HOOK('SURFBC',1,ZHOOK_HANDLE)

!endif INTERFACE

!     ------------------------------------------------------------------

END SUBROUTINE SURFBC
