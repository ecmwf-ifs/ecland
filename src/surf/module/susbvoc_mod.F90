MODULE SUSBVOC_MOD
CONTAINS
SUBROUTINE SUSBVOC(LD_LBVOC,KBVOC_EMIS,KBVOC_DELTA_DAY_LAI,BVOC_NAMES,YDBVOC)
!**   *SUSBVOC* IS THE SET-UP ROUTINE FOR COMMON BLOCK *YOS_BVOC*

!     PURPOSE
!     -------
!          THIS ROUTINE INITIALIZES THE CONSTANTS IN COMMON BLOCK
!     *YOS_BVOC*

!     INTERFACE.
!     ----------
!     CALLLED FROM *SUSURF*

!     METHOD.
!     This routine sets up variables for the computation of online BVOC emissions

!     EXTERNALS.
!     ----------

!     REFERENCE.
!     BVOC scheme based on MEGAN, provided to CAMS through Sindelarova et al.
!
!     Original    V. Huijnen      January 2023

!     MODIFICATIONS
!     -------------
!
!
!==============================================================================

! Modules used : 
USE PARKIND1  , ONLY : JPIM, JPRB
USE YOMHOOK   , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_BVOC  , ONLY : TBVOC
!USE EC_LUN    , ONLY : NULERR
USE ABORT_SURF_MOD

IMPLICIT NONE

LOGICAL,        INTENT(IN)      :: LD_LBVOC
INTEGER(KIND=JPIM),INTENT(IN)   :: KBVOC_EMIS 
INTEGER(KIND=JPIM),INTENT(IN)   :: KBVOC_DELTA_DAY_LAI 
CHARACTER(LEN=8)  ,INTENT(IN)   :: BVOC_NAMES(:)
TYPE(TBVOC),    INTENT(INOUT)   :: YDBVOC

INTEGER(KIND=JPIM) :: JK

REAL(KIND=JPHOOK)  :: ZHOOK_HANDLE


! REAL (KIND = JPRB):: r_bulk    ! Resistance vent. wall

IF (LHOOK) CALL DR_HOOK('SUSBVOC_MOD:SUSBVOC',0,ZHOOK_HANDLE)
ASSOCIATE( LEMIS_BVOC=>YDBVOC%LEMIS_BVOC, &
 & NBVOC_DELTA_DAY_LAI=>YDBVOC%NBVOC_DELTA_DAY_LAI, &
 & NEMIS_BVOC=>YDBVOC%NEMIS_BVOC,NPFT=>YDBVOC%NPFT, &
 & IC5H8=>YDBVOC%IC5H8, RW_TO_MOL_BVOC=>YDBVOC%RW_TO_MOL_BVOC)


! Switches
LEMIS_BVOC=LD_LBVOC

! AH - Needs to be set before return otherwise all compo cases fail when
!      LEMIS_BVOC is false, due to uninitialised array dimension in surfexcdriver
NEMIS_BVOC=KBVOC_EMIS ! Number of trace gases for which BVOC emissions are computed.

IF (.NOT. LEMIS_BVOC) THEN
  NEMIS_BVOC=0 ! This will still be used to dimension arrays on the stack - do not leave uninitialised!
  IF (LHOOK) CALL DR_HOOK('SUSBVOC_MOD:SUSBVOC',1,ZHOOK_HANDLE)
  RETURN
ENDIF


!==============================================================================
 
! Defined constants

RW_TO_MOL_BVOC = 4.6             ! W_to_mmol * RG_to_PAR = 2.3, see also sufarquhar_mod.F90


! Switches
LEMIS_BVOC=LD_LBVOC

! Number of days for historic LAI
NBVOC_DELTA_DAY_LAI = KBVOC_DELTA_DAY_LAI


!==============================================================================
! AH - Moved to before the return, to prevent issues with gcc
!NEMIS_BVOC=KBVOC_EMIS ! Number of trace gases for which BVOC emissions are computed.
NPFT=16               ! number of plant functional types defined in MEGAN

IF(.NOT.ALLOCATED(YDBVOC%NAME))     ALLOCATE (YDBVOC%NAME(NEMIS_BVOC))
IF(.NOT.ALLOCATED(YDBVOC%EMIS_FAC)) ALLOCATE (YDBVOC%EMIS_FAC(NEMIS_BVOC,0:NPFT))
IF(.NOT.ALLOCATED(YDBVOC%LDF))      ALLOCATE (YDBVOC%LDF(NEMIS_BVOC))

! Emission factors, input units ug/m2/hour, converted below to kg/m2/sec
YDBVOC%EMIS_FAC(:,:)=0.0_JPRB

! Light dependent fractions affecting the canopy environment factor
YDBVOC%LDF(:)=0.0

! Test entries
!YDBVOC%EMIS_FAC(1,1:NPFT)=(/ 1.,   2. ,  3., 4., 5., 6., 7., 8., 9., 10.01, 11., 12., 13.,   14.,    15.,    16./)
!YDBVOC%EMIS_FAC(1:NEMIS_BVOC,1:NPFT)=1.0_JPRB
  ! MATCHING BATS table with MEGAN PFT categories  
  
  ! (1)  ! LOW  - Crops, Mixed Farming              =>! 15 Other CROPS
  ! (2)  ! LOW  - Short Grass                       =>! 12/13/14 (cold/cool/warm) C3 GRASS. Default: cool (13)
  ! (3)  ! HIGH - Evergreen Needleleaf Trees        =>! 1/3  Needleleaf evergr. temperate /evergr boreal tree. Default: temperate (1)
  ! (4)  ! HIGH - Deciduous Needleleaf Trees        =>! 2  Needleleaf deciduous boreal tree. NOTE that we follow K. Sindelarova in order of PF types!
  ! (5)  ! HIGH - Deciduous Broadleaf Trees         =>! 6/7/8 Broadleaf evergreen/deciduous trop/temperate/boreal tree. default: temperate (7)
  ! (6)  ! HIGH - Evergreen Broadleaf Trees         =>! 4/5 broadleaf evergreene tropical/temperate tree. Default: tropical (4)
  ! (7)  pre-49r1 ! LOW  - Tall Grass                       =>! 12/13/14 cold/cool/warm GRASS. Default: cool (13)
  ! (7)  49r1     ! mixed crops                     =>! 15: Other crops
  ! (8)  !      - Desert                            =>! N/A
  ! (9)  ! LOW  - Tundra                            =>! 12 cold grass
  ! (10) ! LOW  - Irrigated Crops                   =>! 15 other crops
  ! (11) ! LOW  - Semidesert                        =>! 13 cool C3 grass
  ! (12) !      - Ice Caps and Glaciers             =>! N/A
  ! (13) ! LOW  - Bogs and Marshes                  =>! 13 cool grass?
  ! (14) !      - Inland Water                      =>! N/A
  ! (15) !      - Ocean                             =>! N/A
  ! (16) ! LOW  - Evergreen Shrubs                  =>! 9 broadleaf evergreen temperate shroub
  ! (17) ! LOW  - Deciduous Shrubs                  =>! 10/11 broadleaf dediduous temperate/boreal shrub. Default: temparate (10)
  ! (18) pre-49r1 ! HIGH - Mixed Forest/woodland    =>! 7 broadleaf deciduous temperate tree
  ! (18) 49r1     ! HIGH - Broad Savana             =>! 6/7/8 Broadleaf evergreen/deciduous trop/temperate/boreal tree. default: temperate (7)
  ! (19) pre-49r1 ! HIGH - Interrupted Forest       =>! 7 broadleaf deciduous temperate tree
  ! (19) 49r1     ! N/A                             =>! 0 
  ! (20) ! LOW  - Water and Land Mixtures           =>! N/A
  
  ! Corresponding base conversion table from ECLAND to MEGAN PFT:
  !                     ECLAND index:         1  2  3 4 5 6 7  8 9  10 11 12 13 14 15 16 17 18 19 20
!  INTEGER(KIND=JPIM) :: ECLAND_TO_MEGAN(20)=(/15,13,1,2,7,4,15,0,12,15,13,0, 13, 0, 0,9, 10, 7, 0, 0 /)

DO JK=1,NEMIS_BVOC
  YDBVOC%NAME(JK)=BVOC_NAMES(JK)
  SELECT CASE (TRIM(BVOC_NAMES(JK)))

!                            EF1   EF2    EF3     EF4    EF5     EF6    EF7      EF8    EF9    EF10   EF11   EF12   EF13   EF14   EF15   EF16
  CASE ('    ISOP')
    ! Isoprene:
    IC5H8 = JK ! Update the index pointing at isoprene
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 600.,   1. ,  3000., 7000., 10000., 7000., 10000., 11000., 2000., 4000., 4000., 1600., 800.,   200.,    1.,    1./)
    YDBVOC%LDF(JK) = 1.0_JPRB

  CASE (' MYRCENE')
    ! myrcene
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 70.,  60. ,   70.,   80.,     30.,   80.,    30.,    30.,   30.,   50.,   30.,    0.3,    0.3,   0.3,  0.3,  0.3/)
    YDBVOC%LDF(JK) = 0.6_JPRB
  CASE ('SABINENE')
    ! Sabinene
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 70.,  40. ,   70.,   80.,     50.,   80.,    50.,    50.,   50.,   70.,   50.,    0.7,    0.7,   0.7,  0.7,  0.7/)
    YDBVOC%LDF(JK) = 0.6_JPRB
  CASE ('LIMONENE')
    ! Limonene
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/100., 130. ,  100.,   80.,     80.,   80.,    80.,    80.,   60.,  100.,   60.,    0.7,    0.7,   0.7,  0.7,  0.7/)
    YDBVOC%LDF(JK) = 0.4_JPRB
  CASE ('3DCARENE')
    ! 3-Delta-Carenene
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 160., 80. ,  160.,   40.,     30.,   40.,    30.,    30.,   30.,  100.,   30.,    0.3,    0.3,   0.3,  0.3,  0.3/)
    YDBVOC%LDF(JK) = 0.4_JPRB
  CASE ('TBOCIMEN')
    ! Trans-beta-ocimene
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/  70., 60. ,   70.,  150.,    120.,  150.,   1200.,  120.,   90.,  150.,   90.,     2.,     2.,    2.,   2.,   2./)
    YDBVOC%LDF(JK) = 0.4_JPRB


  CASE ('    APIN')
    ! alpha-pinene 
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 373., 698. ,  373.,  386.,    380.,  386.,   204.,   259.,  200.,  300.,  200.,    2.,   2.,    2.,   2.,    2./)
    YDBVOC%LDF(JK) = 0.6_JPRB
  CASE ('    BPIN')
    ! beta-pinene 
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 300., 200. ,  300.,  120.,    130.,  120.,   130.,   130.,  100.,  150.,  100.,   1.5,  1.5,    1.5,   1.5,   1.5/)
    YDBVOC%LDF(JK) = 0.4_JPRB
  CASE ('   OTERP')
    ! Other monoterpenes - default suggestion
    ! YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 180., 170. ,  180.,  150.,    150.,  150.,   150.,   150.,  110.,  200.,  110.,    5.,     5.,    5.,   5.,   5./)
    ! YDBVOC%LDF(JK) = 0.4_JPRB
    ! Other monoterpenes - sum of EF's from myrcene, sabenine, limonene, 3Dcarenene and trans-beta-ocimine,
    !   This only makes sense if the emission processing (esp. emission factor maps) is identical between compounds, which actually currently is the case
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 70.,  60. ,   70.,   80.,     30.,   80.,    30.,    30.,   30.,   50.,   30.,    0.3,    0.3,   0.3,  0.3,  0.3/) + &
      &                        (/ 70.,  40. ,   70.,   80.,     50.,   80.,    50.,    50.,   50.,   70.,   50.,    0.7,    0.7,   0.7,  0.7,  0.7/) + &
      &                        (/100., 130. ,  100.,   80.,     80.,   80.,    80.,    80.,   60.,  100.,   60.,    0.7,    0.7,   0.7,  0.7,  0.7/) + &
      &                        (/160.,  80. ,  160.,   40.,     30.,   40.,    30.,    30.,   30.,  100.,   30.,    0.3,    0.3,   0.3,  0.3,  0.3/) + &
      &                        (/ 70.,  60. ,   70.,  150.,    120.,  150.,   1200.,  120.,   90.,  150.,   90.,     2.,     2.,    2.,   2.,   2./)
    ! Use avg LDF (ranging between 0.4 and 0.6)
    YDBVOC%LDF(JK) = 0.5_JPRB
  CASE ('   CH3OH')
    ! methanol 
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 900., 900. ,  900.,  500.,    900.,  500.,   900.,   900.,  900.,  900.,  900.,   500.,   500.,  500.,  900.,  900./)
    YDBVOC%LDF(JK) = 0.8_JPRB
  CASE ('CH3COCH3')
    ! acetone 
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 240., 240. ,  240.,  240.,    240.,  240.,   240.,   240.,  240.,  240.,  240.,    80.,    80.,   80.,   80.,   80./)
    YDBVOC%LDF(JK) = 0.2_JPRB
  CASE ('      CO')
    ! carbon monoxide
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 600., 600. ,  600.,  600.,    600.,  600.,   600.,   600.,  600.,  600.,  600.,   600.,   600.,  600.,  600.,  600./)
    YDBVOC%LDF(JK) = 1.0_JPRB
  CASE ('STRESVOC') 
    ! Stress VOC
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 300., 300. ,  300.,  300.,    300.,  300.,   300.,   300.,  300.,  300.,  300.,   300.,   300.,  300.,  300.,  300./)
    YDBVOC%LDF(JK) = 0.8_JPRB
  CASE ('BIDIRVOC') 
    ! Bidirectional VOC
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 500., 500. ,  500.,  500.,    500.,  500.,   500.,   500.,  500.,  500.,  500.,   500.,   500.,  500.,  500.,  500./)
    YDBVOC%LDF(JK) = 0.8_JPRB
  CASE ('OTHERVOC') 
    ! Other VOC
    YDBVOC%EMIS_FAC(JK,1:NPFT)=(/ 140., 140. ,  140.,  140.,    140.,  140.,   140.,   140.,  140.,  140.,  140.,   140.,   140.,  140.,  140.,  140./)
    YDBVOC%LDF(JK) = 0.2_JPRB
  CASE DEFAULT
    CALL ABORT_SURF('No BVOC ?? emission specification available for '//BVOC_NAMES(JK))
  END SELECT
ENDDO

! Conversion from ug/m2/hour to kg/m2/sec:
YDBVOC%EMIS_FAC(:,:)=YDBVOC%EMIS_FAC(:,:)*1E-9_JPRB / 3600._JPRB


END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SUSBVOC_MOD:SUSBVOC',1,ZHOOK_HANDLE)

!     ------------------------------------------------------------------

END SUBROUTINE SUSBVOC
END MODULE SUSBVOC_MOD
