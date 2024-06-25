INTERFACE
SUBROUTINE SURFDEALLO(YDSURF)

!**** *SURFDEALLO * - Routine to deallocate space for global variables
!                     from surface subroutine
! (C) Copyright 2001- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     Purpose.
!     --------
!           Deallocate space for the global fields.

!**   Interface.
!     ----------
!        *CALL* *SURFDEALLO*

!     Explicit arguments :  None
!     --------------------
!        Called by DEALLO

!     Implicit arguments :
!     --------------------
!        Pointers of comdecks

!     Method.
!     -------
!     -------
!        Extraction from DEALLO

!     Externals.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the IFS

!     Author.
!     -------
!        J.F. Estrade *ECMWF* 03-10-01
!     Modifications.
!     --------------
!        Original : 03-10-01
!     ------------------------------------------------------------------

USE PARKIND1, ONLY : JPIM, JPRB
USE, INTRINSIC :: ISO_C_BINDING

IMPLICIT NONE

! Declaration of arguments

TYPE(C_PTR), INTENT(INOUT) :: YDSURF

!     ------------------------------------------------------------------

END SUBROUTINE SURFDEALLO
END INTERFACE
