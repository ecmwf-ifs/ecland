SUBROUTINE SUSDP_DFLT    (YSURF, PTVL, PTVH, PSLT, PSSDP2, PSSDP3, KLEVS3D)

! (C) Copyright 2025- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     ------------------------------------------------------------------
!**   *SUSDP_DFLT* - COMPUTES DEFAULT VALUES FOR THE CALIBRATED SURFACE SPATIALLY DISTRIBUTES PARAMETERS

!     PURPOSE
!     -------
!     COMPUTES DEFAULT VALUES FOR CALIBRATED SPATIALLY DISTRIBUTED FIELDS

!     INTERFACE
!     ---------
!     *SUSDP_DFLT* IS CALLED BY *SUGRIDG* 

!     INPUT PARAMETERS:
!     *PTVL*         LOW VEGETATION TYPE (REAL)
!     *PTVH*         HIGH VEGETATION TYPE (REAL)
!     *PSLT*         SOIL TYPE (REAL)                               (1-7)
!     *KLEVS3D*      SOIL LEVELS (INTEGER)
!     
!     OUTPUT PARAMETERS:
!     *PSSDP2*      OBJECT WITH SPATIALLY DISTRIB. FIELDS 2D     
!     *PSSDP3*      OBJECT WITH SPATIALLY DISTRIB. FIELDS 3D     

!     METHOD
!     ------
!     IT IS NOT ROCKET SCIENCE, BUT CHECK DOCUMENTATION

!     Original  I. Ayan-Miguez May 2023
!     ------------------------------------------------------------------

USE PARKIND1, ONLY : JPIM, JPRB, JPRD
USE, INTRINSIC :: ISO_C_BINDING

!ifndef INTERFACE

USE YOMHOOK,  ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_SURF, ONLY : TSURF

USE ABORT_SURF_MOD
USE SUSDP_DFLT_CTL_MOD
!endif INTERFACE

IMPLICIT NONE

! Declaration of arguments

TYPE(TSURF)       ,INTENT(IN)    :: YSURF
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTVL(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTVH(:) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSLT(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSSDP2(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSSDP3(:,:,:) 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVS3D

!ifndef INTERFACE

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

INTEGER(KIND=JPIM) :: JL
IF (LHOOK) CALL DR_HOOK('SUSDP_DFLT',0,ZHOOK_HANDLE)


CALL SUSDP_DFLT_CTL   (PTVL, PTVH, PSLT, KLEVS3D, PSSDP2, PSSDP3, YSURF%YVEG, YSURF%YSOIL, YSURF%YAGS)  

IF (LHOOK) CALL DR_HOOK('SUSDP_DFLT',1,ZHOOK_HANDLE)

!endif INTERFACE

!     ------------------------------------------------------------------

END SUBROUTINE SUSDP_DFLT
