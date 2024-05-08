MODULE YOS_BVOC

USE PARKIND1,ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!       ----------------------------------------------------------------
!*     ** *YOS_BVOC* BVOC EMISSION PARAMETERS
!       ----------------------------------------------------------------

TYPE :: TBVOC

  LOGICAL :: LEMIS_BVOC  ! High-level switch to activate BVOC emission computation

  INTEGER(KIND=JPIM) :: NEMIS_BVOC               !   Number of biogenic VOC emission types
  INTEGER(KIND=JPIM) :: NBVOC_DELTA_DAY_LAI      !   Number of days for preceding LAI information
  INTEGER(KIND=JPIM) :: NPFT                     !   NUMBER OF PLANT FUNCTIONAL TYPE (1-16)

  CHARACTER(LEN=8),ALLOCATABLE :: NAME(:)        !   Name of species for which BVOC emissions are required
  REAL(KIND=JPRB) ,ALLOCATABLE :: EMIS_FAC(:,:)  !   Emission factor
  REAL(KIND=JPRB) ,ALLOCATABLE :: LDF(:)         !   Light dependent fractions

  INTEGER(KIND=JPIM)   :: IC5H8=-999             !   Identify isoprene (if existing)


  REAL(KIND=JPRB) :: RW_TO_MOL_BVOC              ! W_to_mmol * RG_to_PAR = 2.3
  REAL(KIND=JPRB) :: FUDGE_FAC_BVOC

END TYPE TBVOC

END MODULE YOS_BVOC
