INTERFACE
SUBROUTINE SUSDP_DERIV    (YDSURF, KIDIA, KFDIA, PSLT, PSSDP2, PSSDP3, KLEVS3D)

! (C) Copyright 2025- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     ------------------------------------------------------------------
!**   *SUSDP_DERIV* - COMPUTES DERIVED VALUES FOR THE CALIBRATED SURFACE SPATIALLY DISTRIBUTED PARAMETERS

!     PURPOSE
!     -------
!     COMPUTES DERIVED VALUES FOR CALIBRATED SPATIALLY DISTRIBUTED FIELDS

!     INTERFACE
!     ---------
!     *SUSDP_DERIV* IS CALLED BY *SUGRIDG* AND *SUINIF1S*

!     INPUT PARAMETERS:
!     *KIDIA*        START POINT
!     *KFDIA*        END POINT
!     *PSLT*         SOIL TYPE (REAL)                               (1-7)  
!     *KLEVS3D*      SOIL LEVELS (INTEGER)
!	
!     OUTPUT PARAMETERS:
!     *PSSDP2*       OBJECT WITH 2D SURFACE SPATIALLY DISTRIBUTED PARAMETERS
!     *PSSDP3*       OBJECT WITH 3D SURFACE SPATIALLY DISTRIBUTED PARAMETERS

!     METHOD
!     ------
!     IT IS NOT ROCKET SCIENCE, BUT CHECK DOCUMENTATION

!     Original I. Ayan-Miguez September 2023
!     ------------------------------------------------------------------

USE PARKIND1, ONLY : JPIM, JPRB
USE, INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

! Declaration of arguments

TYPE(C_PTR)       ,INTENT(IN)    :: YDSURF
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
REAL(KIND=JPRB)   ,INTENT(IN)    :: PSLT(:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSSDP2(:,:) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PSSDP3(:,:,:)
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVS3D	


!     ------------------------------------------------------------------

END SUBROUTINE SUSDP_DERIV
END INTERFACE
