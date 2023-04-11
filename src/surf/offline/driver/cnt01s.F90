SUBROUTINE CNT01S
USE PARKIND1  ,ONLY : JPIM     ,JPRB,   JPRD
USE YOMHOOK   ,ONLY : LHOOK   , DR_HOOK, JPHOOK
USE YOMLUN1S , ONLY : NULOUT
USE YOEPHY,    ONLY : LECMF1WAY, LECMF2LAKEC
USE MPL_MODULE
USE CMF_DRV_CONTROL_MOD,  ONLY: CMF_DRV_INIT, CMF_DRV_INPUT

#ifdef UseMPI_CMF
USE YOS_CMF_MAP, ONLY : REGIONALL
#endif

#ifdef DOC
! (C) Copyright 1995- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.


!**** *CNT01S*  - Routine which controls the job at level 0.

!     Purpose.
!     --------
!          Controls the job at level 0, the lowest level.

!***  Interface.
!     ----------
!        *CALL* *CNT01S

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
!        *SU0YOM1S
!        *CNT21S

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the 1D surface model

!     Author.
!     -------
!        Pedro Viterbo and Jean-Francois Mahfouf  *ECMWF*

!     Modifications.
!     --------------
!        Original : 95-03-01
!        Bart vd Hurk (KNMI): Writing of restart file
!        E. Dutra : May 2019: added Cama-Flood coupling initialization
!        I. Ayan-Miguez: Dec 2022: Added CaMa-Flood MPI coupling 
!     ------------------------------------------------------------------
#endif

IMPLICIT NONE

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
INTEGER(KIND=JPIM) :: MYPROC, NPROC, NPROC_CMF
#include "su0yom1s.intfb.h"
#include "cnt21s.intfb.h"
#include "cntend.intfb.h"

IF (LHOOK) CALL DR_HOOK('CNT01S',0,ZHOOK_HANDLE)

MYPROC = MPL_MYRANK()
NPROC  = MPL_NPROC()

!     ------------------------------------------------------------------

!*       1.    Initialize commons.
!              -------------------

CALL SU0YOM1S

!*    Initialize Cama-flood if necessary 
IF (LECMF1WAY) THEN
  IF (MYPROC == 1) THEN
#ifdef UseMPI_CMF
    NPROC_CMF=REGIONALL
#else
    NPROC_CMF=1
#endif
  ENDIF
  CALL MPL_BROADCAST(NPROC_CMF, KROOT=1, KTAG=100, CDSTRING='CNT01S:NPROC_CMF')

  WRITE(NULOUT,*)' CMF_COUPLING: CALLING CMF_DRV_INPUT'
  CALL CMF_DRV_INPUT()
  IF(MYPROC .LE. MIN(NPROC, NPROC_CMF)) THEN
    WRITE(NULOUT,*)' CMF_COUPLING: CALLING CMF_DRV_INIT'
    CALL CMF_DRV_INIT(LECMF2LAKEC)
  ENDIF
  CALL MPL_BARRIER()
ENDIF


!     ------------------------------------------------------------------

!*       2.    Proceed with the work.
!              ----------------------

CALL CNT21S

!     ------------------------------------------------------------------

!*       3.    Close datafiles.
!              ---------------

CALL CNTEND

!      -----------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CNT01S',1,ZHOOK_HANDLE)

RETURN
END SUBROUTINE CNT01S


