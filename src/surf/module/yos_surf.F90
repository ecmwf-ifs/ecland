MODULE YOS_SURF
 
USE YOS_AGS     , ONLY : TAGS
USE YOS_AGF     , ONLY : TAGF
USE YOS_CST     , ONLY : TCST
USE YOS_DIM     , ONLY : TDIM
USE YOS_EXC     , ONLY : TEXC
USE YOS_FLAKE   , ONLY : TFLAKE
USE YOS_LW      , ONLY : TLW
USE YOS_MLM     , ONLY : TMLM
USE YOS_OCEAN_ML, ONLY : TOCEAN_ML
USE YOS_RAD     , ONLY : TRAD
USE YOS_RDI     , ONLY : TRDI
USE YOS_SOIL    , ONLY : TSOIL
USE YOS_SW      , ONLY : TSW
USE YOS_VEG     , ONLY : TVEG
USE YOS_BVOC    , ONLY : TBVOC
USE YOS_URB     , ONLY : TURB

USE ISO_C_BINDING
USE ABORT_SURF_MOD

! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

IMPLICIT NONE

PRIVATE

PUBLIC :: ALLO_SURF, DEALLO_SURF, GET_SURF

TYPE, PUBLIC :: TSURF
  TYPE(TAGS)      :: YAGS 
  TYPE(TAGF)      :: YAGF 
  TYPE(TCST)      :: YCST
  TYPE(TDIM)      :: YDIM
  TYPE(TEXC)      :: YEXC
  TYPE(TFLAKE)    :: YFLAKE
  TYPE(TLW)       :: YLW
  TYPE(TMLM)      :: YMLM
  TYPE(TOCEAN_ML) :: YOCEAN_ML
  TYPE(TRAD)      :: YRAD
  TYPE(TRDI)      :: YRDI
  TYPE(TSOIL)     :: YSOIL
  TYPE(TSW)       :: YSW
  TYPE(TVEG)      :: YVEG
  TYPE(TBVOC)     :: YBVOC
  TYPE(TURB)      :: YURB
END TYPE TSURF

! M. Lange: This construct has been removed in favour of storing the
! storage object on the model and keeping a single local pointer to it
! here in the module. This now implies that the implicit localisation
! attempted via the linked list is defunct and any remaining use cases
! need to switch to implicit OpenMP thread localtion.

!TYPE :: TLIST
!  TYPE(TSURF) :: YSURF
!  TYPE(C_PTR) :: CPTR 
!  TYPE(TLIST),  POINTER :: NEXT
!END TYPE TLIST

! TYPE(TLIST), POINTER, SAVE :: LIST => NULL()

! An explicitly typed pointer to the object and an implicitly typed
! C_PTR for backward compatibility.
TYPE(TSURF), POINTER, SAVE :: YSURF
TYPE(C_PTR), SAVE :: YDSURF

CONTAINS

SUBROUTINE ALLO_SURF(ZYDSURF, ZYSURF)
IMPLICIT NONE
TYPE(C_PTR), INTENT(OUT) :: ZYDSURF
TYPE(TSURF), TARGET, INTENT(IN), OPTIONAL :: ZYSURF

! Backward compatbility mode that allows the associated C_PTR to be
! associated with a given target object. Note that this used to handle
! a global linked list of objects, only accessible via C_PTR
! indirection, which has been removed now!
IF (PRESENT(ZYSURF)) THEN
  YSURF => ZYSURF
ELSE
  ALLOCATE(YSURF)
END IF
YDSURF = C_LOC(YSURF)
ZYDSURF = YDSURF

END SUBROUTINE ALLO_SURF

SUBROUTINE DEALLO_SURF(ZYDSURF)
IMPLICIT NONE

TYPE(C_PTR), INTENT(INOUT) :: ZYDSURF
DEALLOCATE(YSURF)
NULLIFY(YSURF)
END SUBROUTINE DEALLO_SURF


FUNCTION GET_SURF(ZYDSURF)
IMPLICIT NONE

TYPE(TSURF), POINTER    :: GET_SURF
TYPE(C_PTR), INTENT(IN) :: ZYDSURF

! Backward compatbility mode that returns the surface object from a
! C_PTR. Strictly speaking this is no longer required, but the
! behaviour is maintained for now.

! TODO: Add check that C pointer is correctly associated.
GET_SURF => YSURF

END FUNCTION GET_SURF

END MODULE YOS_SURF

