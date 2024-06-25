! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

MODULE MPL_MODULE
CONTAINS
SUBROUTINE ABOR1(CDTEXT)

CHARACTER(len=*),INTENT(IN) :: CDTEXT

WRITE(0,'(A)')CDTEXT
WRITE(0,'(A)')"ABORTING"
FLUSH(0)
STOP (1)
END SUBROUTINE ABOR1
END MODULE MPL_MODULE
