SUBROUTINE STEPO1S
USE PARKIND1  ,ONLY : JPIM     ,JPRB      ,JPRD
USE YOMHOOK   ,ONLY : LHOOK    ,DR_HOOK, JPHOOK
USE YOMGP1S0 , ONLY : GP0
USE YOMGP1SA , ONLY : GPA      ,QLQNUA   
USE YOMDYN1S , ONLY : NSTEP
USE YOMCT01S , ONLY : NFRPOS   ,NSTOP    ,NSTART   ,NFRRES
USE YOMLOG1S , ONLY : LACCUMW  ,CFFORC   ,CFOUT    ,LRESET ,LWROCR
USE YOMGDI1S , ONLY : GDI1S    ,N2DDI    ,GDIAUX1S ,N2DDIAUX ,D1SWAFR
USE YOMDPHY  , ONLY : NPOI     ,NGPP     ,NGPA, NBLOCKS
USE YOMDIM1S , ONLY : NPROMA

#ifdef DOC
! (C) Copyright 1995- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *STEPO1S*  - Controls integration job at lowest level

!     Purpose.
!     --------
!     Controls integration at lowest level

!**   Interface.
!     ----------
!        *CALL* *STEPO1S

!        Explicit arguments :
!        --------------------


!        Implicit arguments :
!        --------------------
!        None

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!                 WRTP1S -  Write out prognostic variables
!                 CPG1S  -  Grid point computations

!        Called by CNT41S

!     Reference.
!     ----------
!        ECMWF Research Department documentation 
!        of the one column surface model

!     Author.
!     -------
!        Jean-Francois Mahfouf and Pedro Viterbo  *ECMWF*

!     Modifications.
!     --------------
!        Original : 95-03-22
!        26-06-2005 S. Lafont : choice format diagnostic output (text or  netCDF)
!        25-07-2005 G. Balsamo : liquid soil moisture accumulation (add T0 contrib.)

!     ------------------------------------------------------------------
#endif

IMPLICIT NONE

INTEGER(KIND=JPIM) :: IA,J,IST,IEND,IBL,IPROMA
REAL(KIND=JPRB) :: ZFAC

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "wrtpcdf.intfb.h"
#include "wrtp1s.intfb.h"
#include "wrtclim.intfb.h"
#include "wrtdcdf.intfb.h"
#include "wrtres.intfb.h"
#include "wrtd1s.intfb.h"
#include "cpg1s.intfb.h"
#include "wrtd2cdf.intfb.h"

IF (LHOOK) CALL DR_HOOK('STEPO1S',0,ZHOOK_HANDLE)


!     ------------------------------------------------------------------


!*       1.    WRITE OUT PROGNOSTIC VARIABLES.
!              -------------------------------

IA=MOD(NSTEP-NSTART,NFRPOS)

IF (IA == 0) THEN
  ZFAC=0.5_JPRB
  IF (CFOUT=='netcdf') THEN
     CALL WRTPCDF
    ELSE
     CALL WRTP1S
  ENDIF
ELSE
   ZFAC=1._JPRB
ENDIF

IF(NSTEP == NSTART+1) THEN
!The Clim file is written at NSTART+1 since the VG soil
!properties are not yet available at NSTART
   IF (CFOUT=='netcdf') THEN
      CALL WRTCLIM
   ENDIF
ENDIF


!*       2.   ACCUMULATE PROGNOSTIC VARIABLES
!             -------------------------------

IF(NSTEP == NSTART)THEN
  !$OMP PARALLEL DO PRIVATE(IST,IEND,IBL,IPROMA)
  DO IST = 1, NPOI, NPROMA
    IEND = MIN(IST+NPROMA-1,NPOI)
    IBL = (IST-1)/NPROMA + 1
    IPROMA = IEND-IST+1

    GPA(IST:IEND,1:NGPP)=GP0(1:IPROMA,1:NGPP,IBL)
  ENDDO
  !$OMP END PARALLEL DO
  GPA(:,NGPP+1:NGPA)=0._JPRB
ELSE
  !$OMP PARALLEL DO PRIVATE(IST,IEND,IBL,IPROMA)
  DO IST = 1, NPOI, NPROMA
    IEND = MIN(IST+NPROMA-1,NPOI)
    IBL = (IST-1)/NPROMA + 1
    IPROMA = IEND-IST+1

    GPA(IST:IEND,1:NGPP)=GPA(IST:IEND,1:NGPP)+ZFAC*GP0(1:IPROMA,1:NGPP,IBL)
  ENDDO
  !$OMP END PARALLEL DO
!Note since Soil Liquid Water is not available at NSTART
!the value at NSTART+1 is used.
  IF(NSTEP == NSTART+1)THEN
    QLQNUA(:,:)=0.5_JPRB*D1SWAFR(:,:)+ZFAC*D1SWAFR(:,:)
  ELSE
    QLQNUA(:,:)=QLQNUA(:,:)+ZFAC*D1SWAFR(:,:)
  ENDIF
ENDIF

!*       3.   WRITE OUT DIAGNOSTIC VARIABLES AND TENDENCIES.
!             ----------------------------------------------

IF (LACCUMW) THEN
  IA=MOD(NSTEP-NSTART,NFRPOS)

  IF (IA == 0) THEN

    IF(NSTEP /= NSTART) THEN 
       IF (CFOUT=='netcdf') THEN 
          CALL WRTDCDF
       ELSE 
          CALL WRTD1S
       ENDIF
    ENDIF

!*       3a.   RESET ACCUMULATION ARRAY IF ACCUMULATION IS OVER
!              OUTPUT INTERVAL
!              -----------------------------------------------

    IF(LRESET)THEN
      !$OMP PARALLEL DO PRIVATE(IBL)
      DO IBL=1,NBLOCKS
        DO J=1,N2DDI
          GDI1S(:,J,2,IBL)=0._JPRB
        ENDDO
      ENDDO
      !$OMP END PARALLEL DO
      DO J=1,N2DDIAUX
        GDIAUX1S(1:NPOI,J,2)=0._JPRB
      ENDDO
    ENDIF

!*      3b.   Re-initialize average prognostic quantities
!             -------------------------------------------

    !$OMP PARALLEL DO PRIVATE(IST,IEND,IBL,IPROMA)
    DO IST = 1, NPOI, NPROMA
      IEND = MIN(IST+NPROMA-1,NPOI)
      IBL = (IST-1)/NPROMA + 1
      IPROMA = IEND-IST+1

      GPA(IST:IEND,1:NGPP) = 0.5_JPRB*GP0(1:IPROMA,1:NGPP,IBL)
    ENDDO
    !$OMP END PARALLEL DO
    QLQNUA(:,:)=0.5_JPRB*D1SWAFR(:,:)

  ENDIF
ENDIF

!     ------------------------------------------------------------------

!*       3.    Write restart file
!              ---------------
if (CFOUT=='netcdf') THEN
   IF(NFRRES <= 0._JPRB)THEN
      IA=-1
   ELSE
      IA=MOD(NSTEP,NFRRES)
   ENDIF
   IF(NSTEP == NSTOP .OR. IA == 0) THEN
      CALL WRTRES
   ENDIF
ENDIF
!     ------------------------------------------------------------------

!*       4.    GRID POINT COMPUTATIONS.
!              ------------------------

CALL CPG1S

!     ------------------------------------------------------------------

!*      5.    Write out diagnostics 
!              ------------------------
IA=MOD(NSTEP-NSTART,NFRPOS)
IF (IA == 0) THEN
  IF (.NOT. LACCUMW) THEN
    IF (CFOUT=='netcdf') THEN
      CALL WRTDCDF
    ELSE
      CALL WRTD1S
    ENDIF
  ENDIF
  ! write t2m, d2m, special case for inst. data
  IF(NSTEP /= NSTART .AND. CFOUT=='netcdf' ) THEN 
    CALL WRTD2CDF
  ENDIF
ENDIF

IF (LHOOK) CALL DR_HOOK('STEPO1S',1,ZHOOK_HANDLE)

RETURN
END SUBROUTINE STEPO1S
