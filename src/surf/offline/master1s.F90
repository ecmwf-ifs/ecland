! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

PROGRAM MASTER1S
USE PARKIND1  ,ONLY : JPRD, JPIM, JPRB
USE YOMLUN1S , ONLY : NULOUT
USE MPL_MODULE
USE OMP_LIB

#ifdef UseMPI_CMF
USE CMF_CTRL_MPI_MOD, ONLY : CMF_MPI_INIT
#endif


IMPLICIT NONE

REAL (KIND=JPRD) :: ZTT0,ZTT1
REAL (KIND=JPIM) :: IERR, NPROC, MYPROC

!For CMF:
#ifdef UseMPI_CMF
INTEGER (KIND=JPIM) :: NPROC_CMF, ICOMM_CMF, ICOL_CMF, IERR_CMF
#endif

#include "cnt01s.intfb.h"

!  Driver for offline version of surface code

CALL MPL_INIT()

NPROC=MPL_NPROC()
MYPROC=MPL_MYRANK()

#ifdef UseMPI_CMF
! Create a communicator for CaMa-Flood
! CaMa-Flood only works up to 16 MPI processes or with 30 MPI processes.

IF (NPROC < 30) THEN
  NPROC_CMF=MIN(NPROC,16.0_JPIM)
ELSE
  NPROC_CMF=30
ENDIF

IF (MYPROC <= NPROC_CMF) THEN
  ICOL_CMF=1
ELSE
  ICOL_CMF=-1
ENDIF

CALL MPL_COMM_SPLIT(MPI_COMM_WORLD, ICOL_CMF, INT(MYPROC), ICOMM_CMF, IERR_CMF)

IF (INT(MYPROC) <= MIN(INT(NPROC),NPROC_CMF)) THEN
  CALL CMF_MPI_INIT(ICOMM_CMF)
ENDIF
#endif

ZTT0 = OMP_GET_WTIME()
CALL CNT01S
ZTT1 = OMP_GET_WTIME()

WRITE(NULOUT,'(a22,f12.3)') 'MASTER1s: Time total: ',ZTT1-ZTT0
CALL MPL_END()

END PROGRAM MASTER1S
