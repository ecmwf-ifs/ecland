! (C) Copyright 2016- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!     ------------------------------------------------------------------

!     *FCSURF** CONTAINS STATEMENT FUNCTIONS USED BY THE SURFACE MODEL

!     E. DUTRA    E.C.M.W.F.      05/04/2016

!     Modifications:
!     I. Ayan-Miguez (BSC) Oct 2023: Add refactorization of Global and 
!                                    Spatially distributed parameters

!     ------------------------------------------------------------------

REAL(KIND=JPRB) :: ZZSNM ! Snow mass (kg m-2)
REAL(KIND=JPRB) :: ZZRSN ! Snow density (kg m-3)
REAL(KIND=JPRB) :: ZZWSA ! volumetric soil moisture (m3 m-3)
REAL(KIND=JPRB) :: ZZF   ! frozen soil fraction
 
REAL(KIND=JPRB) :: ZZRLAMBDADRYM
REAL(KIND=JPRB) :: ZZRWSAT
REAL(KIND=JPRB) :: ZZRLAMSAT


! ==========================================
! Snow liquid water capacity 
! Note that YDSOIL buffer must be defined in the calling routine 
! =========================================
REAL(KIND=JPRB) :: FLWC
FLWC(ZZSNM,ZZRSN) = ZZSNM*(YDSOIL%RLWCSWEB+(YDSOIL%RLWCSWEC-YDSOIL%RLWCSWEB)*&
                      &MAX(0._JPRB,(YDSOIL%RLWCSWEA-ZZRSN))/YDSOIL%RLWCSWEA)


!============================================
! Snow thermal conductivity 
! From  doi:10.1029/2011GL049234
!============================================
REAL(KIND=JPRB) :: FSNTCOND
  FSNTCOND(ZZRSN) = YDSOIL%FSNTCONDC*ZZRSN*ZZRSN &
               & + YDSOIL%FSNTCONDB*ZZRSN+YDSOIL%FSNTCONDA
               
               
!=============================================
! Soil Thermal conductivity using VG 
! See srft_mod.F90
!=============================================
REAL(KIND=JPRB) :: FSOILTCOND
FSOILTCOND(ZZWSA,ZZRLAMBDADRYM,ZZRWSAT,ZZRLAMSAT,ZZF) =ZZRLAMBDADRYM+ &
                        (YDSOIL%RKERST2*LOG10(MAX(YDSOIL%RKERST1,ZZWSA/ZZRWSAT))+YDSOIL%RKERST3) * &
                        (ZZRLAMSAT* &
                        (YDSOIL%RLAMBDAICE**(ZZRWSAT*ZZF))* &
                        (YDSOIL%RLAMBDAWAT**(ZZRWSAT*(1.0_JPRB-ZZF)))- &
                         ZZRLAMBDADRYM)

