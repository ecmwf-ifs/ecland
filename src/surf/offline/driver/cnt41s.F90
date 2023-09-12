SUBROUTINE CNT41S
! (C) Copyright 1995- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
USE PARKIND1  ,ONLY : JPIM     ,JPRB,JPRD
USE YOMHOOK   ,ONLY : LHOOK    ,DR_HOOK, JPHOOK

USE YOMLUN1S , ONLY : NULOUT   ,NULFOR
USE YOMCT01S , ONLY : NSTART   ,NSTOP
USE YOMDYN1S , ONLY : NSTEP    ,TSTEP    ,TDT ,TCOUPFREQ
USE YOMLOG1S,  ONLY : IDBGS1
USE YOEPHY,    ONLY : LECMF1WAY, LECMF2LAKEC

USE YOMGDI1S, ONLY : D1STRO2  ,D1STSRO2,D1STIEVAP2,DCMFCOM,D1STIEVAPU2
USE YOMGC1S,  ONLY : LMASK
USE CMF_CTRL_FORCING_MOD,     ONLY: CMF_FORCING_PUT,CMF_FORCING_COM  
USE CMF_DRV_ADVANCE_MOD, ONLY: CMF_DRV_ADVANCE 
USE YOS_CMF_INPUT,       ONLY: NXIN,NYIN,DT
USE MPL_MODULE


USE YOMLOG1S , ONLY: NDIMCDF
USE YOMDPHY  , ONLY: NLALO ,NLAT     ,NLON, NPOI, NPOIP
USE YOMGPD1S , ONLY: VFPGLOB, VFCLAKE, VFCLAKEF,  VFITM
USE OMP_LIB
USE MPL_MODULE
USE YOS_CMF_INPUT,           ONLY: LOGNAM

#ifdef UseMPI_CMF
USE YOS_CMF_MAP, ONLY : REGIONALL, MPI_COMM_CAMA
#endif

#ifdef DOC

!**** *CNT41S*  - Controls integration job at level 4

!     Purpose.
!     --------
!              Controls integration at level 4

!**   Interface.
!     ----------
!        *CALL* *CNT41S

!        Explicit arguments :
!        --------------------
!        None

!        Implicit arguments :
!        --------------------
!        None

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!        Called by CNT31S.

!     Reference.
!     ----------
!        ECMWF Research Department documentation 
!        of the one-column surface model

!     Author.
!     -------
!        Jean-Francois Mahfouf and Pedro Viterbo  *ECMWF*

!     Modifications.
!     --------------
!        Original : 95-03-21
!        E. Dutra : Added coupling to Cama-Flood 
!        E. Dutra : Dec 2019 2-way coupling with Cama-Flood 
!        G. Balsamo: Oct 2021 MPI compatible for Cama-Flood 1-way
!        I. Ayan-Miguez: Dec 2022: Added CaMa-Flood MPI coupling 

#endif
!     ------------------------------------------------------------------

IMPLICIT NONE

INTEGER (KIND=JPIM) :: ZYYYYMMDD,ZHHMM
INTEGER (KIND=JPIM) :: JL,JLL,ISTEPADV,IMAX,IMIN
REAL (KIND=JPRD) :: ZJUL
REAL (KIND=JPRD) :: ZTT0,ZTT1,ZTT2,ZTT1C,ZTT2C
REAL (KIND=JPRB),ALLOCATABLE :: ZBUFFO(:,:,:),ZFLD(:,:),ZBUFFI(:,:,:),ZBUFFIAUX(:),ZTMP(:)
REAL (KIND=JPRB),ALLOCATABLE :: ZD1STSRO2(:), ZD1STRO2(:), ZD1STIEVAPU2(:), ZVFPGLOB(:), ZBUFFOAUX(:,:)
REAL (KIND=JPRD) :: ZFDPD
REAL(KIND=JPHOOK)  :: ZHOOK_HANDLE
INTEGER(KIND=JPIM) :: MYPROC, NPROC, NPROC_CMF

#include "dattim.intfb.h"
#include "updtim1s.intfb.h"
#include "dtforc.intfb.h"
#include "stepo1s.intfb.h"

!      -----------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('CNT41S',0,ZHOOK_HANDLE)

MYPROC = MPL_MYRANK()
NPROC  = MPL_NPROC()

ALLOCATE(ZBUFFO(NXIN,NYIN,3))
ALLOCATE(ZBUFFOAUX(NPOI,3))
ALLOCATE(ZBUFFI(NXIN,NYIN,1))
ALLOCATE(ZBUFFIAUX(NPOI))
ALLOCATE(ZFLD(NXIN,NYIN))
ALLOCATE(ZTMP(NXIN*NYIN))
ALLOCATE(ZD1STSRO2(NLALO))
ALLOCATE(ZD1STRO2(NLALO))
ALLOCATE(ZD1STIEVAPU2(NLALO))
ALLOCATE(ZVFPGLOB(NLALO))
ALLOCATE(DCMFCOM(NLALO,1))

ZBUFFO(:,:,:)=0._JPRB
ZBUFFOAUX(:,:)=0._JPRB
ZBUFFI(:,:,:)=0._JPRB
ZBUFFIAUX(:)=0._JPRB
ZFLD(:,:)=0._JPRB
ZD1STSRO2(:)=0._JPRB
ZD1STRO2(:)=0._JPRB
ZD1STIEVAPU2(:)=0._JPRB
ZVFPGLOB(:)=0._JPRB
DCMFCOM(:,:)=0._JPRB

IF (LECMF1WAY) THEN
  CALL MPL_ALLGATHERV(PRECVBUF=ZVFPGLOB(:),PSENDBUF=VFPGLOB(:),KRECVCOUNTS=NPOIP(:),CDSTRING="CNT41S:gaussianIndex")
  IF( MYPROC == 1 ) THEN
#ifdef UseMPI_CMF
    NPROC_CMF=REGIONALL
#else
    NPROC_CMF=1
#endif
  ! Sanity checks
    IF (NDIMCDF == 2)THEN
      IF (NLAT .NE. NYIN .OR. NLON .NE. NXIN ) THEN
        WRITE(NULOUT,*)"NLAT .NE. NYIN .OR. NLON .NE. NXIN ",NLAT,NYIN,NLON,NXIN
        CALL ABOR1('Error in dimensions coupling with CMF')
      ENDIF
      WRITE(NULOUT,*)"Coupling with CMF using 2D: NXIN,NYIN:",NXIN,NYIN
    ELSE
      IF ( (1 .NE. NYIN) .OR. (NLON .GT. NXIN) .OR. (NLAT .NE. 0) .OR. (NLON .LT. 1) ) THEN
        WRITE(NULOUT,*) "(1 .NE. NYIN) .OR. (NLON .GT. NXIN) .OR. (NLAT .NE. 0) .OR. (NLON .LT. 1)"&
                         &,1,NYIN,NLON,NXIN,NLAT,0
        CALL ABOR1('Error in dimensions coupling with CMF')
      ENDIF
      
      ! Check gaussian reduced index limits
      IMIN=MINVAL(ZVFPGLOB)
      IMAX=MAXVAL(ZVFPGLOB)
      IF (IMIN .LE. 0 .OR. IMAX .GT. NXIN ) THEN
        WRITE(NULOUT,*)"IMIN .LE. 0 .OR. IMAX .GT. NXIN",IMIN,0,IMAX,NXIN
        CALL ABOR1("Error in limits of gaussian reduced global index")
      ENDIF
      WRITE(NULOUT,*)"Coupling with CMF using 1D: NXIN,NYIN:",NXIN,NYIN
    ENDIF
  ENDIF ! MYPROC == 1 
  CALL MPL_BROADCAST(NPROC_CMF, KROOT=1, KTAG=100, CDSTRING='CNT41S:NPROC_CMF')
ENDIF

!*       1.    MAIN TIME LOOP.
!              ---------------
ZTT1 = OMP_GET_WTIME()
ZTT0 = ZTT1 

IF (LECMF1WAY ) THEN
  ISTEPADV=INT(TCOUPFREQ/DT,JPIM)
ENDIF

DO NSTEP=NSTART,NSTOP


!*       1.1   CURRENT VALUE OF TIME STEP LENGTH.
!              ----------------------------------
  IF ( IDBGS1 > 0 ) THEN
    CALL DATTIM(ZJUL,ZYYYYMMDD,ZHHMM)
    WRITE(NULOUT,'(a5,i10,a6,i9,a1,i4)') 'STEP=',NSTEP,' DATE=',ZYYYYMMDD, ' ',ZHHMM
  ENDIF
  

  TDT=TSTEP


!*       1.2   RESET OF TIME DEPENDENT CONSTANTS.
!              ----------------------------------

  CALL UPDTIM1S(NSTEP,TDT,TSTEP)

!*       1.3    TIME INTERPOLATION OF THE FORCING.
!               ----------------------------------
!   CALL SUFCDF  !! Testing for slot read of forcing ! 
  CALL DTFORC


!*       1.4   COMPUTATION OF THE ENTIRE TIME STEP.
!              ------------------------------------

  CALL STEPO1S
  
  IF (LECMF1WAY) THEN
    !* Accumulate runoff fields and potential water evaporation
    DO JL=1,NPOI
      ZBUFFOAUX(JL,1)=ZBUFFOAUX(JL,1)+ D1STSRO2(JL,1)*TSTEP*0.001_JPRB   ! m of water
      ZBUFFOAUX(JL,2)=ZBUFFOAUX(JL,2)+(D1STRO2(JL,1)-D1STSRO2(JL,1))*TSTEP*0.001_JPRB   ! m of water
      ZBUFFOAUX(JL,3)=ZBUFFOAUX(JL,3)+(-D1STIEVAPU2(JL,9,1))*TSTEP*0.001_JPRB   ! m of water
    ENDDO        
      
    !* Coupling: 
    IF ( MOD(NSTEP*TSTEP,TCOUPFREQ) .EQ. 0 .AND. NSTEP .NE. NSTART ) THEN
      ZTT1C = OMP_GET_WTIME()
      WRITE(NULOUT,*)' CMF_COUPLING: CALLING DRV_PUT & DRV_ADVANCE:',ISTEPADV

      ! Gather the runoffs to send to CaMa-Flood
      CALL MPL_ALLGATHERV(PRECVBUF=ZD1STSRO2(:),PSENDBUF=ZBUFFOAUX(:,1),KRECVCOUNTS=NPOIP(:),CDSTRING="CNT41S:SRO")
      CALL MPL_ALLGATHERV(PRECVBUF=ZD1STRO2(:),PSENDBUF=ZBUFFOAUX(:,2),KRECVCOUNTS=NPOIP(:),CDSTRING="CNT41S:SSRO")
      CALL MPL_ALLGATHERV(PRECVBUF=ZD1STIEVAPU2(:),PSENDBUF=ZBUFFOAUX(:,3),KRECVCOUNTS=NPOIP(:),CDSTRING="CNT41S:LakeEVAP")

      IF (NDIMCDF .NE. 2)THEN
        DO JL=1,NLALO
          JLL=INT(ZVFPGLOB(JL))
          ZBUFFO(JLL,1,1)= ZD1STSRO2(JL)   
          ZBUFFO(JLL,1,2)=ZD1STRO2(JL)  
          ZBUFFO(JLL,1,3)=ZD1STIEVAPU2(JL)   
        ENDDO
      ELSE
        ! Surface runoff
        ZBUFFO(:,:,1) = RESHAPE( UNPACK(ZD1STSRO2(:),LMASK(:),0._JPRB),&
                     (/NXIN,NYIN/))
        
        ! Sub-surface runoff
        ZBUFFO(:,:,2) = RESHAPE( UNPACK(ZD1STRO2(:),LMASK(:),0._JPRB),&
                       (/NXIN,NYIN/))
        
        ! Estimate of potential water evaporation 
        ZBUFFO(:,:,3) = RESHAPE( UNPACK(ZD1STIEVAPU2(:),LMASK(:),0._JPRB),&
                      (/NXIN,NYIN/))       
      ENDIF

      !*  Send runoff to CaMa-Flood 
      !    -------------------------
      IF ( MYPROC .LE. MIN(NPROC, NPROC_CMF) ) THEN 
        !*  - Must be MYPROC <= 16 only, and data is global
        CALL CMF_FORCING_PUT(ZBUFFO(:,:,:))
 
        !*  Advance model 
        !   -------------
        CALL CMF_DRV_ADVANCE(ISTEPADV)
      
        !* Get data from Cama-Flood 
        !  -------------------
        IF (LECMF2LAKEC .NE. 0) THEN
          CALL CMF_FORCING_COM(ZBUFFI(:,:,:))
        ENDIF
      ENDIF

      !* All NPROC need to wait for Cama to finish 
      CALL MPL_BARRIER()
      
      !* Transfor into HTESSEL structure
      !* -------

      IF (LECMF2LAKEC==0) THEN
        ! no coupling 
        VFCLAKEF(:) = VFCLAKE(:)
      ELSE 
        IF (MYPROC == 1) THEN
          IF (NDIMCDF == 2)THEN
            ZTMP = RESHAPE(ZBUFFI(:,:,1),(/NXIN*NYIN/))
            DCMFCOM(:,1) = PACK(ZTMP,LMASK(:))
          ELSE
            DO JL=1,NLALO
              JLL=INT(ZVFPGLOB(JL))
              DCMFCOM(JL,1) = ZBUFFI(JLL,1,1)
            ENDDO
          ENDIF
        ENDIF

        CALL MPL_SCATTERV(PRECVBUF=ZBUFFIAUX(:),KROOT=1,PSENDBUF=DCMFCOM(:,1),KSENDCOUNTS=NPOIP(:),CDSTRING="CNT41S:ZBUFFI")

        IF (LECMF2LAKEC==1) THEN 
          ! replace lake cover by flood plain fraction over land 
          DO JL=1,NPOI
            IF ( VFITM(JL) > 0.5_JPRB ) THEN
              ! Update lake fraction only over land points
              VFCLAKEF(JL) = MAX(0._JPRB,MIN(0.99_JPRB,ZBUFFIAUX(JL)))
            ENDIF
          ENDDO
        ELSEIF (LECMF2LAKEC==2) THEN 
          ! add flooplain fraction to lake cover over land 
          DO JL=1,NPOI
            IF ( VFITM(JL) > 0.5_JPRB ) THEN
              ! Update lake fraction only over land points
              VFCLAKEF(JL) = MAX(0._JPRB,MIN(0.99_JPRB,VFCLAKE(JL)+ZBUFFIAUX(JL)))
            ENDIF
          ENDDO
        ELSE
          WRITE(NULOUT,*) "LECMF2LAKEC can be only 0,1 or 2 but it is",LECMF2LAKEC
          CALL ABOR1('LECMF2LAKEC can only be 0,1 or 2')
        ENDIF
      ENDIF
      !* Reset fluxes 
      ZBUFFO(:,:,:) = 0._JPRB
      ZBUFFOAUX(:,:)= 0._JPRB
      ZBUFFI(:,:,:) = 0._JPRB
      ZBUFFIAUX(:)  = 0._JPRB
      ZTT2C = OMP_GET_WTIME()
         
      WRITE(NULOUT,'(a22,f8.3)') 'CMF_COUPLING CWALsec: ',ZTT2C-ZTT1C    
      
    ENDIF 
  ENDIF 
  
  IF ( IDBGS1 > 0 ) THEN
    ZTT2 = OMP_GET_WTIME()
    WRITE(NULOUT,'(a5,i10,a5,f8.3,a7,f8.3)') 'STEP=',NSTEP,' WALsec: ',ZTT2-ZTT1
    ZTT1 = OMP_GET_WTIME()
  ENDIF

ENDDO

IF (ALLOCATED(ZBUFFO))  DEALLOCATE(ZBUFFO)
IF (ALLOCATED(ZFLD))    DEALLOCATE(ZFLD)
IF (ALLOCATED(DCMFCOM)) DEALLOCATE(DCMFCOM)
IF (ALLOCATED(ZTMP))    DEALLOCATE(ZTMP)
IF (ALLOCATED(ZBUFFOAUX)) DEALLOCATE(ZBUFFOAUX)
IF (ALLOCATED(ZBUFFI)) DEALLOCATE(ZBUFFI)
IF (ALLOCATED(ZBUFFIAUX)) DEALLOCATE(ZBUFFIAUX)

IF ( IDBGS1 > 0 ) THEN
  ZTT2 = OMP_GET_WTIME()
  ZFDPD = REAL(NSTOP-NSTART,JPRD)*TSTEP/(ZTT2-ZTT0)
  WRITE(NULOUT,'(a27,f8.3)') 'CNT41S: Time step loop in: ',ZTT2-ZTT0
  WRITE(NULOUT,'(A,F10.1)')'FORECAST DAYS PER DAY ',ZFDPD
ENDIF

!*       2.    CLOSE FILES.
!              ------------

!*       2.1   CLOSE ATMOSPHERIC FORCING INPUT FILE.
!              -------------------------------------

CLOSE(NULFOR)

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('CNT41S',1,ZHOOK_HANDLE)

RETURN

END SUBROUTINE CNT41S
