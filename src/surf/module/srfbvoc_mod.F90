MODULE SRFBVOC_MOD
CONTAINS
SUBROUTINE SRFBVOC(KIDIA,KFDIA,KLON,KTILES,&
 & PTSTEP, &
 & PFRTI,&
 & YDBVOC,&
 & PBVOCFLUXVT, &
! more data..
 & PBVOCFLUX, &
 & PDHBVOCS )
 !     ------------------------------------------------------------------

!**   *SRFBVOC* - DOES THE POST-PROCESSING OF Biogenic VOC EMISSIONS

!     PURPOSE
!     -------

!     POST-PROCESSING OF THE BVOC EMISSIONS FROM VDFSURF

!     INTERFACE
!     ---------

!     *SRFBVOC* IS CALLED BY *SURFEXCDRIVER*

!     INPUT PARAMETERS (INTEGER):

!     *KIDIA*        START POINT
!     *KFDIA*        END POINT
!     *KLON*         NUMBER OF GRID POINTS PER PACKET
!     *KTILES*       NUMBER OF TILES (I.E. SUBGRID AREAS WITH DIFFERENT 
!                    OF SURFACE BOUNDARY CONDITION)

!     INPUT PARAMETERS (REAL):

!     *PTSTEP*       TIMESTEP

!     *PFRTI*        TILE FRACTION                              (0-1)
!            1 : WATER                  5 : SNOW ON LOW-VEG+BARE-SOIL
!            2 : ICE                    6 : DRY SNOW-FREE HIGH-VEG
!            3 : WET SKIN               7 : SNOW UNDER HIGH-VEG
!            4 : DRY SNOW-FREE LOW-VEG  8 : BARE SOIL

!     *PBVOCFLUXVT*    Tiled input biogenic VOC FLUXES                      KG_BVOC/M2/S



!     UPDATED PARAMETERS (REAL):


!     *PBVOCFLUX*     Grid-box average biogenic VOC FLUXES                  KG_BVOC/M2/S

!     *PDHBVOC*      Diagnostic array for BVOC (see module yomcdh) 
!                    (kgBVOC m-2 s-1 for fluxes)

!     MODIFICATIONS
!     -------------
!     Original    V. Huijnen      January 2023
!
!
!==============================================================================

USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_BVOC , ONLY : TBVOC


IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KTILES
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSTEP 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PFRTI(:,:) 
TYPE(TBVOC)       ,INTENT(IN)    :: YDBVOC

REAL(KIND=JPRB)   ,INTENT(IN)    :: PBVOCFLUXVT(:,:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBVOCFLUX(:,:)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDHBVOCS(:,:,:)


!*         0.     LOCAL VARIABLES.
!                 ----- ----------

INTEGER(KIND=JPIM) :: JL, JTILE, JVT, JS, IVT
! REAL(KIND=JPRB)   ::  ZBVOCFLUXVT(KLON,YDBVOC%NEMIS_BVOC,KTILES)
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
 


IF (LHOOK) CALL DR_HOOK('SRFBVOC_MOD:SRFBVOC',0,ZHOOK_HANDLE)
! ASSOCIATE( NVTILES=>YDVEG%NVTILES)

!*         1.     INITIALIZE Local Variables
!                 ---------- ----------

! ZBVOCFLUXVT(KIDIA:KFDIA,:,:)=0._JPRB
PDHBVOCS(KIDIA:KFDIA,:,:)=0._JPRB


! DO JL=KIDIA,KFDIA
!   DO JVT=1,KTILES ! Check the loop over tiles: KTILES or NVTILES?!
!     ZBVOCFLUXVT(JL,:,JVT)=PBVOCFLUXVT(JL,:,JVT)
!   ENDDO
! ENDDO


!*         2.      GRID AVERAGED VALUES 
!                  --------------------

PBVOCFLUX(KIDIA:KFDIA,:)=0._JPRB
DO JVT=1,KTILES 

  DO JL=KIDIA,KFDIA
    ! Multiply with tile fraction
    !sign changed to ECMWF convention (flux positive downward) ??
    PBVOCFLUX(JL,1:YDBVOC%NEMIS_BVOC)=PBVOCFLUX(JL,1:YDBVOC%NEMIS_BVOC)+PFRTI(JL,JVT)*PBVOCFLUXVT(JL,1:YDBVOC%NEMIS_BVOC,JVT) *(-1._JPRB)
  ENDDO

ENDDO


!*         3.      DIAGNOSTICS
!                  -----------

! Diagnostis in PDH-arrays are NOT normalised to grid box quantities, so values
! are NOT multiplied by the vegetation type fraction (PCVT).
DO JVT=1,KTILES

  IVT=1
  IF (JVT == 3) THEN
    ! Need to make this wet tile more advanced..
    IVT=1
  ! ELSEIF ((JVT == 4) .OR. (JVT == 5)) THEN
  !  IVT=1
  ELSEIF ((JVT == 6) .OR. (JVT == 7)) THEN
    IVT=2
  ENDIF
 
  DO JL=KIDIA,KFDIA
    ! not normalised to grid square
    ! PDHBVOCS(JL,IVT,1)=PDHBVOCS(JL,IVT,1)+PBVOCFLUXVT(JL,1,JVT) !VH *(-1._JPRB) 
    ! PDHBVOCS(JL,IVT,2)=PDHBVOCS(JL,IVT,2)+PBVOCFLUXVT(JL,2,JVT) !VH *(-1._JPRB) 
    ! VH test diagnostics fields..
    IF (IVT == 1_JPIM) THEN
      PDHBVOCS(JL,IVT,1)=1.
      PDHBVOCS(JL,IVT,2)=2.
    ELSE
      PDHBVOCS(JL,IVT,1)=3.
      PDHBVOCS(JL,IVT,2)=4.
    ENDIF
  ENDDO

 ENDDO



IF (LHOOK) CALL DR_HOOK('SRFBVOC_MOD:SRFBVOC',1,ZHOOK_HANDLE)
END SUBROUTINE SRFBVOC
END MODULE SRFBVOC_MOD
