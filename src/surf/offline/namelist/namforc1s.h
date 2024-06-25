! (C) Copyright 2005- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!*    -----------------------------------------------------------------
!     FORCING PROPERTIES

!     ZPHISTA	REFERENCE LEVEL (FOR T, Q)
!     ZUV	REFERENCE LEVEL FOR WIND
!     ZDTFORC	FORCING TIME STEP
!     IFYYYY	REFERENCE YEAR
!     IFMM	REFERENCE MONTH
!     IFDD	REFERENCE DAY
!     IFTIM	REFERENCE TIME (HHMM)
!     ISTYYYY	STARTYEAR OF READING
!     ISTMM	STARTMONTH OF READING
!     ISTDD	STARTDAY OF READING
!     ISTTIM	STARTTIME OF READING
!     INSTFC	NUMBER OF TIME STEPS TO BE READ (0 = ALL)
!     NDIMFORC  NUMBER OF GRID DIMENSIONS in FORCING
!     LOADIAB	FLAG FOR APPLYING ADIABATIC HEIGHT CORRECTION

!     CFORCU    NAME OF FILE CONTAINING U
!     CFORCV    NAME OF FILE CONTAINING V
!     CFORCT    NAME OF FILE CONTAINING T
!     CFORCQ    NAME OF FILE CONTAINING Q
!     CFORCC    NAME OF FILE CONTAINING CO2
!     CFORCP    NAME OF FILE CONTAINING PRESSURE
!     CFORCG    NAME OF FILE CONTAINING SURFACE GEOPOTENTIAL
!     CFORCRAIN NAME OF FILE CONTAINING PRECIPITATION
!     CFORCSNOW NAME OF FILE CONTAINING PRECIPITATION
!     CFORCSW   NAME OF FILE CONTAINING SHORTWAVE
!     CFORCLW   NAME OF FILE CONTAINING LONGWAVE

!     ZDISPL    DISPLACEMENT HEIGHT
!     IPSTPFC   LENGTH OF FORCING DATA


REAL(KIND=JPRB) :: ZPHISTA,ZDTFORC,ZUV,ZDISPL
INTEGER(KIND=JPIM) :: IFYYYY,IFMM,IFDD,IFTIM,&
                 &ISTYYYY,ISTMM,ISTDD,ISTTIM,INSTFC,NDIMFORC,IPSTPFC

CHARACTER*255 CFORCU,CFORCV,CFORCT,CFORCQ,CFORCC,CFORCP,CFORCRAIN,&
                 &CFORCSNOW,CFORCSW,CFORCLW,CFORCG
LOGICAL LOADIAB
                 
NAMELIST/NAMFORC/ ZPHISTA,ZDTFORC,ZUV,ZDISPL,IFYYYY,IFMM,IFDD,IFTIM,&
                 &ISTYYYY,ISTMM,ISTDD,ISTTIM,INSTFC,NDIMFORC,&
                 &CFORCU,CFORCV,CFORCT,CFORCQ,CFORCC,CFORCP,CFORCRAIN,&
             &CFORCSNOW,CFORCSW,CFORCLW,CFORCG,LOADIAB,IPSTPFC
!     -----------------------------------------------------------------
