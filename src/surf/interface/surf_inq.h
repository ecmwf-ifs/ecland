INTERFACE
SUBROUTINE SURF_INQ(YDSURF,&
                  & KNVTYPES,KNSOTY,PRRCSOIL,PRWSAT,PRWCAP,PRWPWP,PRQWEVAP,PRQWSBCR,&
                  & PRQSNCR,PRWLMAX,PRWSATM,PRWCAPM,PRWPWPM,PRTF1,PRTF2,PRTF3,PRTF4,&
                  & PRTFREEZSICE,PRTMELTSICE,PRCIMIN,PRALFMINSN,&
                  & PRALFMAXSN,PRHOMINSN,PRHOMAXSN,PRHOMINSND,PRHOMAXSN_NEW,PRDAT,&
                  & PRDAW,PRRCSICE,PRALBSEAD,PREPALB,PRVCOV,PRVLAI,PRVRSMIN,&
                  & PRVROOTSA,PRVLAMSK,PRVLAMSKS,PRVTRSR,PRNU,PRNUM,PRNUH,PRNUQ,PRCHAR,PREPUST,&
                  & PRDSCALE_KPP,PRVC_KPP,PRVD_KPP,PRVE_KPP,PRVDEL_KPP,LDVL_NEW_KPP, &
                  & LD_LESNML, RLEVSNMIN, RLEVSNMAX,&
                  & RLEVSNMIN_GL, RLEVSNMAX_GL )

! (C) Copyright 2001- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!**   *SURF_INQ*  Extract information from the surface package

!     Purpose.
!     --------
!     Interface routine for extracting information from the surf pack.

!**   Interface.
!     ----------
!     CALL SURFINQ(...)
!     Explicit arguments : All arguments are optional.
!     --------------------

!     Method.
!     -------

!     Externals: none

!     Author.
!     -------
!        JF Estrade *ECMWF*

!     Modifications.
!     --------------
!        Original : 03-10-01
!        G. Balsamo : Adding soil type relative information
!     ------------------------------------------------------------------

USE PARKIND1, ONLY : JPIM, JPRB
USE, INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

! Declaration of arguments

TYPE(C_PTR),                  INTENT(IN)  :: YDSURF
INTEGER(KIND=JPIM) ,OPTIONAL, INTENT(OUT) :: KNSOTY
INTEGER(KIND=JPIM) ,OPTIONAL, INTENT(OUT) :: KNVTYPES
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRRCSOIL
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRWSAT
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRQWSBCR
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRWCAP
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRWPWP
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRQWEVAP
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRQSNCR
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRWLMAX
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRWSATM(:)
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRWCAPM(:)
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRWPWPM(:)
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRTFREEZSICE
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRTMELTSICE
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRCIMIN
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRALFMINSN
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRALFMAXSN
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRHOMINSN
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRHOMAXSN
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRHOMINSND
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRHOMAXSN_NEW
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRDAT(:)
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRDAW(:)
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRALBSEAD
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PREPALB
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRVCOV(:) 
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRVLAI(:) 
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRVRSMIN(0:)
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRNU
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRNUM
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRNUH
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRNUQ
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRCHAR
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PREPUST
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRTF1
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRTF2
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRTF3
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRTF4
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRRCSICE
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRVROOTSA(:,:) 
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRVLAMSK(:)
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRVLAMSKS(:)
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRVTRSR(:)
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRDSCALE_KPP
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRVC_KPP
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRVD_KPP
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRVE_KPP
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: PRVDEL_KPP
LOGICAL            ,OPTIONAL, INTENT(OUT) :: LDVL_NEW_KPP
LOGICAL            ,OPTIONAL, INTENT(OUT) :: LD_LESNML
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: RLEVSNMIN(:)
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: RLEVSNMAX(:)
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: RLEVSNMIN_GL(:)
REAL(KIND=JPRB)    ,OPTIONAL, INTENT(OUT) :: RLEVSNMAX_GL(:)

!     ------------------------------------------------------------------

END SUBROUTINE SURF_INQ
END INTERFACE
