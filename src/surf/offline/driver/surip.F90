SUBROUTINE SURIP(KULOUT)
USE PARKIND1  ,ONLY : JPIM     ,JPRB,   JPRD
USE YOMHOOK   ,ONLY : LHOOK    ,DR_HOOK, JPHOOK
USE YOMLUN1S , ONLY : NULNAM
USE YOMRIP   , ONLY : NINDAT   ,NSSSSS   ,NSTADD   ,NSTASS   ,&
            &RTIMST   ,RSTATI   ,RTIMTR   ,RHGMT    ,REQTIM   ,&
            &RSOVR    ,RDEASO   ,RDECLI   ,RWSOVR   ,RIP0     ,&
            &RCODEC   ,RSIDEC   ,RCOVSR   ,RSIVSR   ,RDTSA    ,&
            &RDTSA2   ,RDTS62   ,RDTS22   ,RTDT

#ifdef DOC
! (C) Copyright 1987- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SURIP * - Routine to initialize the common YOMRIP

!     Purpose.
!     --------
!           Initialize and print the common YOMRIP

!**   Interface.
!     ----------
!        *CALL* *SURIP(KULOUT)

!        Explicit arguments :
!        --------------------
!        KULOUT : Logical unit of the output

!        Implicit arguments :
!        --------------------
!        COMMON YOMRIP

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!        Mats Hamrud and Philippe Courtier  *ECMWF*

!     Modifications.
!     --------------
!        Original : 87-10-15
!        Modified DEC 1992 by K. YESSAD: changing RDTFLS2 into RTDT (=PTDT).
!        Modified by R. EL Khatib : 93-04-02 Set-up defaults controled by LECMWF
!        Modified by R. El Khatib  :93-05-06 Set-up through command line
!     ------------------------------------------------------------------
#endif

IMPLICIT NONE

INTEGER(KIND=JPIM) :: KULOUT

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "namrip.h"

IF (LHOOK) CALL DR_HOOK('SURIP',0,ZHOOK_HANDLE)

!     ------------------------------------------------------------------

!*       1.    Initialize YOMRIP.
!              ------------------

!        1.1 Set implicit default values

NINDAT=19591204
NSSSSS=43200

!*       1.3   READ NAMELIST.
REWIND (NULNAM)
READ(NULNAM,NAMRIP)

!*       1.5   SET REST OF COMMON TO 0
NSTADD=0
NSTASS=0
RSTATI=0._JPRB
!* RTIMST AND RTIMTR ARE INITIALIZED IN SUCST
RTIMST=0._JPRD
RTIMTR=0._JPRD
!* ALL THOSE VARIABLES ARE UPDATED BY UPDTIM
RHGMT=0._JPRB
REQTIM=0._JPRB
RSOVR=0._JPRB
RDEASO=0._JPRB
RDECLI=0._JPRB
RWSOVR=0._JPRB
RIP0=0._JPRB
RCODEC=0._JPRB
RSIDEC=0._JPRB
RCOVSR=0._JPRB
RSIVSR=0._JPRB

RDTSA=0._JPRB
RDTSA2=0._JPRB
RDTS62=0._JPRB
RDTS22=0._JPRB

RTDT=0._JPRB

!      ----------------------------------------------------------------

!*       2.    PRINT PART OF YOMRIP.
!              ---------------------

WRITE(UNIT=KULOUT,FMT='('' NINDAT='',I10,'' NSSSSS='',I6)')NINDAT,NSSSSS

!     ------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('SURIP',1,ZHOOK_HANDLE)

RETURN
END SUBROUTINE SURIP
