! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     ------------------------------------------------------------------
NAMELIST/NAMPHY/&
! * GROUPE DES CLES GENERALES.
  &LMPHYS,LSLC &
! * GROUPE DES CLES NE COMMANDANT PAS L'APPEL AUX ROUTINES AM-----.
  &,LCAPE,LCONDWT,LCVCAS,LCVDD,LCVLIS,LCVPP,LHUNEG,LNEIGE &
  &,LRNUMX,LSRCON,LSRCONT,L2PHYS,LRRGUST,LO3ABC,LNEBNXR &
! * GROUPE DES CLES COMMANDANT L'APPEL AUX ROUTINES AM-----.
  &,LCVRA,LGWD,LGWDC,LNEBCO,LNEBN,LNEBR,LNEBT &
  &,LOZONE,LRAY,LRAYLU,LREWS,LRAYPL &
  &,LRAYFM,LRAYFM15,LRRMES,LSFHYD,LSNV,LSOLV,LFGEL,LSTRA,LSTRAS &
  &,LVDIF &
! * GROUPE DES INDICES DE CALCUL.
  &,NBITER,NOIR,NDPSFI,NPHYREP,LVGSN,LFPCOR,LNOIAS
!     ------------------------------------------------------------------
