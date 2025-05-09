SUBROUTINE SUGDI1S
USE PARKIND1  ,ONLY : JPIM     ,JPRB,  JPRD
USE YOMHOOK   ,ONLY : LHOOK    ,DR_HOOK, JPHOOK
USE YOMLUN1S  ,ONLY : NULOUT
USE YOMDPHY  , ONLY : NCSS, NPOI     ,NTILES ,NVHILO 
USE YOMGDI1S , ONLY : GDI1S    ,GDIAUX1S ,&
            &D1STISRD ,D1STISRU ,D1STITRD ,D1STITRU ,&
            &D1STIH   ,D1STILE  ,D1STIGFL ,D1STII   ,D1STIEVAP,&
            &D1STILES ,D1SNFR   ,D1SNTFR  ,D1SNAFR  ,&
            &D1SRFLD  ,D1SRFLU  ,D1TRFLD  ,D1TRFLU  ,&
            &D1AHFS   ,D1AHFL   ,D1MOMU   ,D1MOMV   ,&
            &D1STNSRD ,D1STNSRU ,D1STNTRD ,D1STNTRU ,&
            &D1STNGFL ,D1STNDH  ,D1STNM   ,&
            &D1ST1SRD ,D1ST1SRU ,D1ST1TRD ,D1ST1TRU ,&
            &D1ST1H   ,D1ST1LE  ,D1STAGFL ,D1STASF  ,&
            &D1SSFL   ,D1SSFC   ,D1SWNJQ  ,D1SWNM   ,&
            &D1SWLIT  ,D1SWLJQ  ,&
            &D1SW1JBG ,D1SW1TF  ,D1SWAGFL ,D1SW1RI  ,&
            &D1SWARS  ,D1SWAEXT ,D1SW1M   ,D1SWAC   ,D1SW1C    ,&
            &D1STISRD2,D1STISRU2,D1STITRD2,D1STITRU2,&
            &D1STIH2  ,D1STILE2 ,D1STIGFL2,D1STII2  ,D1STIEVAP2,&
            &D1STILES2,D1SNFR2  ,D1SNTFR2 ,D1SNAFR2 ,&
            &D1SRFLD2 ,D1SRFLU2 ,D1TRFLD2 ,D1TRFLU2 ,&
            &D1AHFS2  ,D1AHFL2  ,&
            &D1STNSRD2,D1STNSRU2,D1STNTRD2,D1STNTRU2,&
            &D1STNGFL2,D1STNDH2 ,D1STNM2  ,&
            &D1ST1SRD2,D1ST1SRU2,D1ST1TRD2,D1ST1TRU2,&
            &D1ST1H2  ,D1ST1LE2 ,D1STAGFL2,D1STASF2 , &
            &D1SSFL2  ,D1SSFC2  ,D1SWNJQ2 ,D1SWNM2  ,&
            &D1SWLIT2 ,D1SWLJQ2 ,&
            &D1SW1JBG2,D1SW1TF2 ,D1SWAGFL2,D1SW1RI2 ,&
            &D1SWARS2 ,D1SWAEXT2,D1SW1M2  ,D1SWAC2  ,D1SW1C2  ,&
            &D1STIFR  ,D1STITK  ,D1STITK2 ,D1STIALB ,&
            &D1SNDEPTH,D1SWAFR  ,D1SRADTK ,D1SRADTK2,&
            &D1SAVTK  ,D1SAVTK2 ,D1SVEGTK ,D1SVEGTK2,&
            &D1SGHF   ,D1SGHF2  ,D1SALB   ,D1SALB2  ,&
            &D1SVEGEV ,D1SVEGEV2,D1STIFR2 ,D1SDSH   ,D1SDSH2  ,&
            &D1SWDS   ,D1SWDS2  ,&
            &D1SSDS   ,D1SSDS2  ,D1SWLDS  ,D1SWLDS2 ,&
            &D1SLSRF  ,D1SCRF   ,D1SLSSF  ,D1SCSF   ,&
            &D1STE    ,D1STRO   ,D1STSRO  ,D1SMLT   ,&
            &D1SLSRF2 ,D1SCRF2  ,D1SLSSF2 ,D1SCSF2  ,&
            &D1STE2   ,D1STRO2  ,D1STSRO2 ,D1SMLT2  ,&
            &D1SSDSSL ,D1SSDSSL2,&
            &D1SVTRC  ,D1SVTRA  ,D1SVTRC2 ,D1SVTRA2 ,&
            &N2DDI    ,N2DDIAUX ,D1T2M  ,D1D2M, D1T2M2 ,D1D2M2,&
            &D1WFLUXRF2,D1HFLUXRF2,D1WFLUXRF,D1HFLUXRF,&
            &D1WSN,D1WSN2,&
            &D1SWFR,&
            &D1SVTAN  ,D1SVTAG  ,D1SVTRD  ,D1SVTRSOIL_STR,&
            &D1SVTRECO,D1SVTCO2FLUX,D1SVTRNOQ10,&
            &D1SAN    ,D1SAG    ,D1SRD    ,D1SRSOIL_STR,&
            &D1SRECO  ,D1SCO2FLUX,D1SCH4FLUX,&
            &D1SWFR2  ,&
            &D1SVTAN2 ,D1SVTAG2 ,D1SVTRD2 ,D1SVTRSOIL_STR2,&
            &D1SVTRECO2,D1SVTCO2FLUX2,D1SVTRNOQ102,&    
            &D1SAN2   ,D1SAG2   ,D1SRD2   ,D1SRSOIL_STR2,&
            &D1SRECO2 ,D1SCO2FLUX2,D1SCH4FLUX2,&
            &D1SLAI   ,D1SBIOM  ,D1SBLOSS ,D1SBGAIN ,&
            &D1SBIOMSTR,D1SBIOMSTRB ,&
            &D1SVTLAI ,D1SVTBIOM,D1SVTBLOSS,D1SVTBGAIN,&
            &D1SVTBIOMSTR,D1SVTBIOMSTRB,&
            &D1SVTGC  ,D1SVTGA  ,D1SVTF2  ,D1SVTDS,&
            &D1SVTDMAX, D1SVTLE  ,D1SVTFR,&        
            &D1SLAI2  ,D1SBIOM2 ,D1SBLOSS2,D1SBGAIN2,&
            &D1SBIOMSTR2,D1SBIOMSTRB2 ,&
            &D1SVTLAI2,D1SVTBIOM2,D1SVTBLOSS2,D1SVTBGAIN2,&
            &D1SVTBIOMSTR2,D1SVTBIOMSTRB2,&
            &D1SVTGC2 ,D1SVTGA2 ,D1SVTF22 ,D1SVTDS2,&
            &D1SVTDMAX2,D1SVTLE2 ,D1SVTFR2,&
            &D1STAIR  ,D1SQAIR  ,D1SCO2AIR, D1SUWIND ,D1SVWIND ,&
            &D1STAIR2 ,D1SQAIR2 ,D1SCO2AIR2, D1SUWIND2,D1SVWIND2,&
            &D1STIEVAPU,D1STIEVAPU2,&
            &D1STISKC,D1STISKC2,D1SPSURF,D1SPSURF2

#ifdef DOC
! (C) Copyright 1997- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SUDIM1S * - Allocates space for the diagnostics
!                  in the surface one-column model

!     Purpose.
!     --------
!            dimensioning the diagnostics of the surface one-column model

!**   Interface.
!     ----------
!        *CALL* *SUDIM1S*

!        Explicit arguments :
!        --------------------


!        Implicit arguments :
!        --------------------


!     Method.
!     -------

!     Externals.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the one-column model

!     Author.
!     -------
!        Pedro Viterbo  *ECMWF*

!     Modifications.
!     --------------
!        Original : 97-03-11

!     ------------------------------------------------------------------

#endif

IMPLICIT NONE
INTEGER(KIND=JPIM) :: I1D,I1DVA,I2DVA,I3DVA,I3D,I4DVA, &
     &  I1DAUX,I1DVAAUX,I2DVAAUX,I3DVAAUX,I4DVAAUX
INTEGER(KIND=JPIM) :: M1STISRD,M1STISRU,M1STITRD,M1STITRU,M1STIH,&
     &  M1STILE,M1STILES,M1STIGFL,M1STII,M1STIEVAP,M1SRFLD,M1SRFLU,&
     &  M1TRFLD,M1TRFLU,M1AHFS,M1AHFL,M1MOMU,M1MOMV,M1STNSRD,&
     &  M1STNSRU,M1STNTRD,M1STNTRU,M1STNGFL,M1STNM,M1ST1SRD,&
     &  M1ST1SRU,M1ST1TRD,M1ST1TRU,M1ST1H,M1ST1LE,M1STAGFL,M1STASF,&
     &  M1SSFL,M1SSFC,M1SWNJQ,M1SWNM,M1SWLIT,M1SWLJQ,M1SW1JBG,M1SW1TF,&
     &  M1SWAGFL,M1SW1RI,M1SWARS,M1SWAEXT,M1SW1M,M1SWAC,M1SW1C,M1STIFR,&
     &  M1STITK,M1STIALB,M1SNDEPTH,M1SNFR,M1SNTFR,M1SNAFR,M1SWAFR,&
     &  M1SAVTK,M1SVEGTK,M1SGHF,M1SRADTK,M1SALB,M1SVEGEV,M1SDSH,&
     &  M1STNDH,M1SWDS,M1SSDS,M1SWLDS,&
     &  M1SLSRF,M1SCRF,M1SLSSF,M1SCSF,M1STE,M1STRO,M1STSRO,M1SMLT,&
     &  M1SSDSSL,M1SVTRC,M1SVTRA,&
     &  i2DDi,I2DDIAUX,M1SCREEN,&
     &  M1WFLUXRF,M1HFLUXRF,M1WSN,&
     &  M1SWFR,&
     &  M1SVTAN,M1SVTAG,&
     &  M1SVTRD,M1SVTRSOIL_STR,M1SVTRECO,M1SVTCO2FLUX,M1SVTRNOQ10,M1SAN,M1SAG,M1SRD,&
     &  M1SRSOIL_STR,M1SRECO,M1SCO2FLUX,M1SCH4FLUX,&
     &  M1SLAI,M1SBIOM,M1SBLOSS,M1SBGAIN,M1SBIOMSTR,M1SBIOMSTRB,&
     &  M1SVTLAI,M1SVTBIOM,M1SVTBLOSS,M1SVTBGAIN,&
     &  M1SVTBIOMSTR,M1SVTBIOMSTRB,&
     &  M1SVTGC,M1SVTGA,M1SVTF2,M1SVTDS,M1SVTDMAX,M1SVTLE,M1SVTFR,&
     &  M1STAIR,M1SQAIR,M1SUWIND,M1SVWIND,M1STIEVAPU,M1STISKC,M1SPSURF,&
     &  M1SCO2AIR

REAL(KIND=JPHOOK)      :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('SUGDI1S',0,ZHOOK_HANDLE)


!*       1.  SIZE OF FIRST, SECOND AND THIRD DIMENSIONS.
!            ------------------------------------------

I1D=NPOI
I1DVA=31
I1DVA=I1DVA+3 ! 3 new diagnostics for snow
I1DVA=I1DVA+6+1 !CO2: 6, CH4: 1


I2DVA=6
I3DVA=10+1+1 ! instressed vegetation
I4DVA=6+1 !CO2: 6, RNOQ10: 1
N2DDI=I4DVA*NVHILO+I3DVA*NTILES+I2DVA*NCSS+I1DVA

I3D=2

I1DAUX=NPOI
I1DVAAUX=23+7+5+2+1
! 7: LAI, biomass, biom loss, biom gain, above struct biom, below struct biom,
! interception fraction
! 5: air temp, air specific humidity, u wind speed, v wind speed,psurf
! 2 : T2m q2m
! 1 : air CO2

I2DVAAUX=2
I3DVAAUX=3
I4DVAAUX=2+13 ! 13:LAI, biomass, biom loss, biom gain, above struct biom, below struct

! biom, gc, ga, F2, Ds, Dmax, LE, fraction
!N2DDIAUX=I4DVAAUX*2+I3DVAAUX*NTILES+I2DVAAUX*NCSS+I1DVAAUX
N2DDIAUX=(2*2+13*NVHILO)+I3DVAAUX*NTILES+I2DVAAUX*NCSS+I1DVAAUX

!*       2.  ALLOCATE TOTAL SIZE.
!            --------------------
ALLOCATE (GDI1S(I1D,N2DDI,I3D))
ALLOCATE (GDIAUX1S(I1DAUX,N2DDIAUX,I3D))
GDI1S(:,:,:)=0.
GDIAUX1S(:,:,:)=0.

!*       3.  POINTER STUFF.
!            --------------

! Ti
M1STISRD=1
M1STISRU=M1STISRD+NTILES
M1STITRD=M1STISRU+NTILES
M1STITRU=M1STITRD+NTILES
M1STIH=M1STITRU+NTILES
M1STILE=M1STIH+NTILES
M1STILES=M1STILE+NTILES
M1STIGFL=M1STILES+NTILES
M1STII=M1STIGFL+NTILES
M1STIEVAP=M1STII+NTILES
M1STIEVAPU=M1STIEVAP+NTILES
M1STISKC=M1STIEVAPU+NTILES

D1STISRD => GDI1S(:,M1STISRD:M1STISRD+NTILES-1,1)
D1STISRU => GDI1S(:,M1STISRU:M1STISRU+NTILES-1,1)
D1STITRD => GDI1S(:,M1STITRD:M1STITRD+NTILES-1,1)
D1STITRU => GDI1S(:,M1STITRU:M1STITRU+NTILES-1,1)
D1STIH => GDI1S(:,M1STIH:M1STIH+NTILES-1,1)
D1STILE => GDI1S(:,M1STILE:M1STILE+NTILES-1,1)
D1STILES => GDI1S(:,M1STILES:M1STILES+NTILES-1,1)
D1STIGFL => GDI1S(:,M1STIGFL:M1STIGFL+NTILES-1,1)
D1STII => GDI1S(:,M1STII:M1STII+NTILES-1,1)
D1STIEVAP => GDI1S(:,M1STIEVAP:M1STIEVAP+NTILES-1,1)
D1STIEVAPU => GDI1S(:,M1STIEVAPU:M1STIEVAPU+NTILES-1,1)
D1STISRD2 => GDI1S(:,M1STISRD:M1STISRD+NTILES-1,:)
D1STISRU2 => GDI1S(:,M1STISRU:M1STISRU+NTILES-1,:)
D1STITRD2 => GDI1S(:,M1STITRD:M1STITRD+NTILES-1,:)
D1STITRU2 => GDI1S(:,M1STITRU:M1STITRU+NTILES-1,:)
D1STIH2 => GDI1S(:,M1STIH:M1STIH+NTILES-1,:)
D1STILE2 => GDI1S(:,M1STILE:M1STILE+NTILES-1,:)
D1STILES2 => GDI1S(:,M1STILES:M1STILES+NTILES-1,:)
D1STIGFL2 => GDI1S(:,M1STIGFL:M1STIGFL+NTILES-1,:)
D1STII2 => GDI1S(:,M1STII:M1STII+NTILES-1,:)
D1STIEVAP2 => GDI1S(:,M1STIEVAP:M1STIEVAP+NTILES-1,:)
D1STIEVAPU2 => GDI1S(:,M1STIEVAPU:M1STIEVAPU+NTILES-1,:)
D1STISKC   => GDI1S(:,M1STISKC:M1STISKC+NTILES-1,1)
D1STISKC2   => GDI1S(:,M1STISKC:M1STISKC+NTILES-1,:)
! K
M1SRFLD=M1STISKC+NTILES
M1SRFLU=M1SRFLD+1
M1TRFLD=M1SRFLU+1
M1TRFLU=M1TRFLD+1
M1AHFS=M1TRFLU+1
M1AHFL=M1AHFS+1
M1MOMU=M1AHFL+1
M1MOMV=M1MOMU+1
D1SRFLD => GDI1S(:,M1SRFLD,1)
D1SRFLU => GDI1S(:,M1SRFLU,1)
D1TRFLD => GDI1S(:,M1TRFLD,1)
D1TRFLU => GDI1S(:,M1TRFLU,1)
D1AHFS => GDI1S(:,M1AHFS,1)
D1AHFL => GDI1S(:,M1AHFL,1)
D1MOMU => GDI1S(:,M1MOMU,1)
D1MOMV => GDI1S(:,M1MOMV,1)
D1SRFLD2 => GDI1S(:,M1SRFLD,:)
D1SRFLU2 => GDI1S(:,M1SRFLU,:)
D1TRFLD2 => GDI1S(:,M1TRFLD,:)
D1TRFLU2 => GDI1S(:,M1TRFLU,:)
D1AHFS2 => GDI1S(:,M1AHFS,:)
D1AHFL2 => GDI1S(:,M1AHFL,:)
! NT
M1STNSRD=M1MOMV+1
M1STNSRU=M1STNSRD+1
M1STNTRD=M1STNSRU+1
M1STNTRU=M1STNTRD+1
M1STNGFL=M1STNTRU+1
M1STNM=M1STNGFL+1
M1WFLUXRF=M1STNM+1 
M1HFLUXRF=M1WFLUXRF+1
M1WSN=M1HFLUXRF+1
D1STNSRD => GDI1S(:,M1STNSRD,1)
D1STNSRU => GDI1S(:,M1STNSRU,1)
D1STNTRD => GDI1S(:,M1STNTRD,1)
D1STNTRU => GDI1S(:,M1STNTRU,1)
D1STNGFL => GDI1S(:,M1STNGFL,1)
D1STNM => GDI1S(:,M1STNM,1)
D1STNSRD2 => GDI1S(:,M1STNSRD,:)
D1STNSRU2 => GDI1S(:,M1STNSRU,:)
D1STNTRD2 => GDI1S(:,M1STNTRD,:)
D1STNTRU2 => GDI1S(:,M1STNTRU,:)
D1STNGFL2 => GDI1S(:,M1STNGFL,:)
D1STNM2 => GDI1S(:,M1STNM,:)
D1WFLUXRF => GDI1S(:,M1WFLUXRF,1)
D1HFLUXRF => GDI1S(:,M1HFLUXRF,1)
D1WFLUXRF2 => GDI1S(:,M1WFLUXRF,:)
D1HFLUXRF2 => GDI1S(:,M1HFLUXRF,:)
D1WSN=>GDI1S(:,M1WSN,1)
D1WSN2=>GDI1S(:,M1WSN,:)
! T
M1ST1SRD=M1WSN+1
M1ST1SRU=M1ST1SRD+1
M1ST1TRD=M1ST1SRU+1
M1ST1TRU=M1ST1TRD+1
M1ST1H=M1ST1TRU+1
M1ST1LE=M1ST1H+1
M1STAGFL=M1ST1LE+1
M1STASF=M1STAGFL+NCSS
D1ST1SRD => GDI1S(:,M1ST1SRD,1)
D1ST1SRU => GDI1S(:,M1ST1SRU,1)
D1ST1TRD => GDI1S(:,M1ST1TRD,1)
D1ST1TRU => GDI1S(:,M1ST1TRU,1)
D1ST1H => GDI1S(:,M1ST1H,1)
D1ST1LE => GDI1S(:,M1ST1LE,1)
D1STAGFL => GDI1S(:,M1STAGFL:M1STAGFL+NCSS-1,1)
D1STASF => GDI1S(:,M1STASF:M1STASF+NCSS-1,1)
D1ST1SRD2 => GDI1S(:,M1ST1SRD,:)
D1ST1SRU2 => GDI1S(:,M1ST1SRU,:)
D1ST1TRD2 => GDI1S(:,M1ST1TRD,:)
D1ST1TRU2 => GDI1S(:,M1ST1TRU,:)
D1ST1H2 => GDI1S(:,M1ST1H,:)
D1ST1LE2 => GDI1S(:,M1ST1LE,:)
D1STAGFL2 => GDI1S(:,M1STAGFL:M1STAGFL+NCSS-1,:)
D1STASF2 => GDI1S(:,M1STASF:M1STASF+NCSS-1,:)
! NW
M1SSFL=M1STASF+NCSS
M1SSFC=M1SSFL+1
M1SWNJQ=M1SSFC+1
M1SWNM=M1SWNJQ+1
D1SSFL => GDI1S(:,M1SSFL,1)
D1SSFC => GDI1S(:,M1SSFC,1)
D1SWNJQ => GDI1S(:,M1SWNJQ,1)
D1SWNM => GDI1S(:,M1SWNM,1)
D1SSFL2 => GDI1S(:,M1SSFL,:)
D1SSFC2 => GDI1S(:,M1SSFC,:)
D1SWNJQ2 => GDI1S(:,M1SWNJQ,:)
D1SWNM2 => GDI1S(:,M1SWNM,:)
! L
M1SWLIT=M1SWNM+1
M1SWLJQ=M1SWLIT+1
D1SWLIT => GDI1S(:,M1SWLIT,1)
D1SWLJQ => GDI1S(:,M1SWLJQ,1)
D1SWLIT2 => GDI1S(:,M1SWLIT,:)
D1SWLJQ2 => GDI1S(:,M1SWLJQ,:)
! W
M1SW1JBG=M1SWLJQ+1
M1SW1TF=M1SW1JBG+1
M1SWAGFL=M1SW1TF+1
M1SW1RI=M1SWAGFL+NCSS
M1SWARS=M1SW1RI+1
M1SWAEXT=M1SWARS+NCSS
M1SW1M=M1SWAEXT+NCSS
M1SWAC=M1SW1M+1
M1SW1C=M1SWAC+NCSS
!I2DDI=M1SW1C
D1SW1JBG => GDI1S(:,M1SW1JBG,1)
D1SW1TF => GDI1S(:,M1SW1TF,1)
D1SWAGFL => GDI1S(:,M1SWAGFL:M1SWAGFL+NCSS-1,1)
D1SW1RI => GDI1S(:,M1SW1RI,1)
D1SWARS => GDI1S(:,M1SWARS:M1SWARS+NCSS-1,1)
D1SWAEXT => GDI1S(:,M1SWAEXT:M1SWAEXT+NCSS-1,1)
D1SW1M => GDI1S(:,M1SW1M,1)
D1SWAC => GDI1S(:,M1SWAC:M1SWAC+NCSS-1,1)
D1SW1C => GDI1S(:,M1SW1C,1)
D1SW1JBG2 => GDI1S(:,M1SW1JBG,:)
D1SW1TF2 => GDI1S(:,M1SW1TF,:)
D1SWAGFL2 => GDI1S(:,M1SWAGFL:M1SWAGFL+NCSS-1,:)
D1SW1RI2 => GDI1S(:,M1SW1RI,:)
D1SWARS2 => GDI1S(:,M1SWARS:M1SWARS+NCSS-1,:)
D1SWAEXT2 => GDI1S(:,M1SWAEXT:M1SWAEXT+NCSS-1,:)
D1SW1M2 => GDI1S(:,M1SW1M,:)
D1SWAC2 => GDI1S(:,M1SWAC:M1SWAC+NCSS-1,:)
D1SW1C2 => GDI1S(:,M1SW1C,:)

! CO2 per vegetation type
M1SVTAN=M1SW1C+1
M1SVTAG=M1SVTAN+NVHILO
M1SVTRD=M1SVTAG+NVHILO
M1SVTRSOIL_STR=M1SVTRD+NVHILO
M1SVTRECO=M1SVTRSOIL_STR+NVHILO
M1SVTCO2FLUX=M1SVTRECO+NVHILO
M1SVTRNOQ10=M1SVTCO2FLUX+NVHILO
D1SVTAN => GDI1S(:,M1SVTAN:M1SVTAN+NVHILO-1,1)
D1SVTAG => GDI1S(:,M1SVTAG:M1SVTAG+NVHILO-1,1)
D1SVTRD => GDI1S(:,M1SVTRD:M1SVTRD+NVHILO-1,1)
D1SVTRSOIL_STR => GDI1S(:,M1SVTRSOIL_STR:M1SVTRSOIL_STR+NVHILO-1,1)
D1SVTRECO => GDI1S(:,M1SVTRECO:M1SVTRECO+NVHILO-1,1)
D1SVTCO2FLUX => GDI1S(:,M1SVTCO2FLUX:M1SVTCO2FLUX+NVHILO-1,1)
D1SVTRNOQ10 => GDI1S(:,M1SVTRNOQ10:M1SVTRNOQ10+NVHILO-1,1)
D1SVTAN2 => GDI1S(:,M1SVTAN:M1SVTAN+NVHILO-1,:)
D1SVTAG2 => GDI1S(:,M1SVTAG:M1SVTAG+NVHILO-1,:)
D1SVTRD2 => GDI1S(:,M1SVTRD:M1SVTRD+NVHILO-1,:)
D1SVTRSOIL_STR2 => GDI1S(:,M1SVTRSOIL_STR:M1SVTRSOIL_STR+NVHILO-1,:)
D1SVTRECO2 => GDI1S(:,M1SVTRECO:M1SVTRECO+NVHILO-1,:)
D1SVTCO2FLUX2 => GDI1S(:,M1SVTCO2FLUX:M1SVTCO2FLUX+NVHILO-1,:)
D1SVTRNOQ102 => GDI1S(:,M1SVTRNOQ10:M1SVTRNOQ10+NVHILO-1,:)
! CO2 GRID      
M1SAN=M1SVTRNOQ10+NVHILO
M1SAG=M1SAN+1
M1SRD=M1SAG+1
M1SRSOIL_STR=M1SRD+1
M1SRECO=M1SRSOIL_STR+1
M1SCO2FLUX=M1SRECO+1
M1SCH4FLUX=M1SCO2FLUX+1

I2DDI=M1SCH4FLUX

D1SAN => GDI1S(:,M1SAN,1)
D1SAG => GDI1S(:,M1SAG,1)
D1SRD => GDI1S(:,M1SRD,1)
D1SRSOIL_STR => GDI1S(:,M1SRSOIL_STR,1)
D1SRECO => GDI1S(:,M1SRECO,1)
D1SCO2FLUX => GDI1S(:,M1SCO2FLUX,1)
D1SCH4FLUX => GDI1S(:,M1SCH4FLUX,1)
D1SAN2 => GDI1S(:,M1SAN,:)
D1SAG2 => GDI1S(:,M1SAG,:)
D1SRD2 => GDI1S(:,M1SRD,:)
D1SRSOIL_STR2 => GDI1S(:,M1SRSOIL_STR,:)
D1SRECO2 => GDI1S(:,M1SRECO,:)
D1SCO2FLUX2 => GDI1S(:,M1SCO2FLUX,:)
D1SCH4FLUX2 => GDI1S(:,M1SCH4FLUX,:)
!Aux
!AuxTI
M1STIFR=1
M1STITK=M1STIFR+NTILES
M1STIALB=M1STITK+NTILES
D1STIFR => GDIAUX1S(:,M1STIFR:M1STIFR+NTILES-1,1)
D1STITK => GDIAUX1S(:,M1STITK:M1STITK+NTILES-1,1)
D1STIALB => GDIAUX1S(:,M1STIALB:M1STIALB+NTILES-1,1)
D1STIFR2 => GDIAUX1S(:,M1STIFR:M1STIFR+NTILES-1,:)
D1STITK2 => GDIAUX1S(:,M1STITK:M1STITK+NTILES-1,:)
!AuxSN
M1SNDEPTH=M1STIALB+NTILES
D1SNDEPTH => GDIAUX1S(:,M1SNDEPTH,1)
M1SNFR=M1SNDEPTH+1
D1SNFR => GDIAUX1S(:,M1SNFR,1)
D1SNFR2 => GDIAUX1S(:,M1SNFR,:)
M1SNTFR=M1SNFR+1
D1SNTFR => GDIAUX1S(:,M1SNTFR,1)
D1SNTFR2 => GDIAUX1S(:,M1SNTFR,:)
M1SNAFR=M1SNTFR+1
D1SNAFR => GDIAUX1S(:,M1SNAFR,1)
D1SNAFR2 => GDIAUX1S(:,M1SNAFR,:)
!AuxW
M1SWAFR=M1SNAFR+1
D1SWAFR => GDIAUX1S(:,M1SWAFR:M1SWAFR+NCSS-1,1)

M1SWFR=M1SWAFR+1
D1SWFR => GDIAUX1S(:,M1SWFR,1)
D1SWFR2 => GDIAUX1S(:,M1SWFR,:)



!AuxT
M1SAVTK=M1SWFR+NCSS
D1SAVTK => GDIAUX1S(:,M1SAVTK,1)
D1SAVTK2 => GDIAUX1S(:,M1SAVTK,:)
M1SVEGTK=M1SAVTK+1
D1SVEGTK => GDIAUX1S(:,M1SVEGTK,1)
D1SVEGTK2 => GDIAUX1S(:,M1SVEGTK,:)
M1SGHF=M1SVEGTK+1
D1SGHF => GDIAUX1S(:,M1SGHF,1)
D1SGHF2 => GDIAUX1S(:,M1SGHF,:)
M1SRADTK=M1SGHF+1
D1SRADTK => GDIAUX1S(:,M1SRADTK,1)
D1SRADTK2 => GDIAUX1S(:,M1SRADTK,:)
M1SALB=M1SRADTK+1
D1SALB => GDIAUX1S(:,M1SALB,1)
D1SALB2 => GDIAUX1S(:,M1SALB,:)
M1SVEGEV=M1SALB+1
D1SVEGEV => GDIAUX1S(:,M1SVEGEV,1)
D1SVEGEV2 => GDIAUX1S(:,M1SVEGEV,:)
M1SDSH=M1SVEGEV+1
D1SDSH => GDIAUX1S(:,M1SDSH,1)
D1SDSH2 => GDIAUX1S(:,M1SDSH,:)
M1STNDH=M1SDSH+1
D1STNDH => GDIAUX1S(:,M1STNDH,1)
D1STNDH2 => GDIAUX1S(:,M1STNDH,:)
M1SWDS=M1STNDH+1
D1SWDS => GDIAUX1S(:,M1SWDS,1)
D1SWDS2 => GDIAUX1S(:,M1SWDS,:)
M1SSDS=M1SWDS+1
D1SSDS => GDIAUX1S(:,M1SSDS,1)
D1SSDS2 => GDIAUX1S(:,M1SSDS,:)
M1SSDSSL=M1SSDS+1
D1SSDSSL => GDIAUX1S(:,M1SSDSSL:M1SSDSSL+NCSS-1,1)
D1SSDSSL2 => GDIAUX1S(:,M1SSDSSL:M1SSDSSL+NCSS-1,:)
M1SWLDS=M1SSDSSL+NCSS
D1SWLDS => GDIAUX1S(:,M1SWLDS,1)
D1SWLDS2 => GDIAUX1S(:,M1SWLDS,:)
M1SLSRF=M1SWLDS+1
D1SLSRF => GDIAUX1S(:,M1SLSRF,1)
D1SLSRF2 => GDIAUX1S(:,M1SLSRF,:)
M1SCRF=M1SLSRF+1
D1SCRF => GDIAUX1S(:,M1SCRF,1)
D1SCRF2 => GDIAUX1S(:,M1SCRF,:)
M1SLSSF=M1SCRF+1
D1SLSSF => GDIAUX1S(:,M1SLSSF,1)
D1SLSSF2 => GDIAUX1S(:,M1SLSSF,:)
M1SCSF=M1SLSSF+1
D1SCSF => GDIAUX1S(:,M1SCSF,1)
D1SCSF2 => GDIAUX1S(:,M1SCSF,:)
M1STE=M1SCSF+1
D1STE => GDIAUX1S(:,M1STE,1)
D1STE2 => GDIAUX1S(:,M1STE,:)
M1STRO=M1STE+1
D1STRO => GDIAUX1S(:,M1STRO,1)
D1STRO2 => GDIAUX1S(:,M1STRO,:)
M1STSRO=M1STRO+1
D1STSRO => GDIAUX1S(:,M1STSRO,1)
D1STSRO2 => GDIAUX1S(:,M1STSRO,:)
M1SMLT=M1STSRO+1
D1SMLT => GDIAUX1S(:,M1SMLT,1)
D1SMLT2 => GDIAUX1S(:,M1SMLT,:)
! Aux resistances (rc and ra for low and high)
M1SVTRC=M1SMLT+1
D1SVTRC => GDIAUX1S(:,M1SVTRC:M1SVTRC+1,1)
D1SVTRC2 => GDIAUX1S(:,M1SVTRC:M1SVTRC+1,:)
M1SVTRA=M1SVTRC+2
D1SVTRA => GDIAUX1S(:,M1SVTRA:M1SVTRA+1,1)
D1SVTRA2 => GDIAUX1S(:,M1SVTRA:M1SVTRA+1,:)
M1SCREEN=M1SVTRA+2
D1T2M => GDIAUX1S(:,M1SCREEN,1)
D1T2M2 => GDIAUX1S(:,M1SCREEN,:)
D1D2M => GDIAUX1S(:,M1SCREEN+1,1)
D1D2M2 => GDIAUX1S(:,M1SCREEN+1,:)

!Aux vegetation
M1SLAI=M1SCREEN+2
M1SBIOM=M1SLAI+1
M1SBLOSS=M1SBIOM+1
M1SBGAIN=M1SBLOSS+1
M1SBIOMSTR=M1SBGAIN+1
M1SBIOMSTRB=M1SBIOMSTR+1
M1SVTFR=M1SBIOMSTRB+1
M1SVTLAI=M1SVTFR+NVHILO
M1SVTBIOM=M1SVTLAI+NVHILO
M1SVTBLOSS=M1SVTBIOM+NVHILO
M1SVTBGAIN=M1SVTBLOSS+NVHILO
M1SVTBIOMSTR=M1SVTBGAIN+NVHILO
M1SVTBIOMSTRB=M1SVTBIOMSTR+NVHILO
M1SVTGC=M1SVTBIOMSTRB+NVHILO
M1SVTGA=M1SVTGC+NVHILO
M1SVTF2=M1SVTGA+NVHILO
M1SVTDS=M1SVTF2+NVHILO
M1SVTDMAX=M1SVTDS+NVHILO
M1SVTLE=M1SVTDMAX+NVHILO
M1STAIR=M1SVTLE+NVHILO
M1SQAIR=M1STAIR+1
M1SUWIND=M1SQAIR+1
M1SVWIND=M1SUWIND+1
M1SPSURF=M1SVWIND+1
M1SCO2AIR=M1SPSURF+1


D1SLAI => GDIAUX1S(:,M1SLAI,1)
D1SLAI2 => GDIAUX1S(:,M1SLAI,:)
D1SBIOM => GDIAUX1S(:,M1SBIOM,1)
D1SBIOM2 => GDIAUX1S(:,M1SBIOM,:)
D1SBLOSS => GDIAUX1S(:,M1SBLOSS,1)
D1SBLOSS2 => GDIAUX1S(:,M1SBLOSS,:)
D1SBGAIN => GDIAUX1S(:,M1SBGAIN,1)
D1SBGAIN2 => GDIAUX1S(:,M1SBGAIN,:)
D1SBIOMSTR => GDIAUX1S(:,M1SBIOMSTR,1)
D1SBIOMSTR2 => GDIAUX1S(:,M1SBIOMSTR,:)
D1SBIOMSTRB => GDIAUX1S(:,M1SBIOMSTRB,1)
D1SBIOMSTRB2 => GDIAUX1S(:,M1SBIOMSTRB,:)
D1SVTFR => GDIAUX1S(:,M1SVTFR:M1SVTFR+NVHILO-1,1)
D1SVTFR2 => GDIAUX1S(:,M1SVTFR:M1SVTFR+NVHILO-1,:)
D1SVTLAI => GDIAUX1S(:,M1SVTLAI:M1SVTLAI+NVHILO-1,1)
D1SVTLAI2 => GDIAUX1S(:,M1SVTLAI:M1SVTLAI+NVHILO-1,:)
D1SVTBIOM => GDIAUX1S(:,M1SVTBIOM:M1SVTBIOM+NVHILO-1,1)
D1SVTBIOM2 => GDIAUX1S(:,M1SVTBIOM:M1SVTBIOM+NVHILO-1,:)
D1SVTBLOSS => GDIAUX1S(:,M1SVTBLOSS:M1SVTBLOSS+NVHILO-1,1)
D1SVTBLOSS2 => GDIAUX1S(:,M1SVTBLOSS:M1SVTBLOSS+NVHILO-1,:)
D1SVTBGAIN => GDIAUX1S(:,M1SVTBGAIN:M1SVTBGAIN+NVHILO-1,1)
D1SVTBGAIN2 => GDIAUX1S(:,M1SVTBGAIN:M1SVTBGAIN+NVHILO-1,:)
D1SVTBIOMSTR => GDIAUX1S(:,M1SVTBIOMSTR:M1SVTBIOMSTR+NVHILO-1,1)
D1SVTBIOMSTR2 => GDIAUX1S(:,M1SVTBIOMSTR:M1SVTBIOMSTR+NVHILO-1,:)
D1SVTBIOMSTRB => GDIAUX1S(:,M1SVTBIOMSTRB:M1SVTBIOMSTRB+NVHILO-1,1)
D1SVTBIOMSTRB2 => GDIAUX1S(:,M1SVTBIOMSTRB:M1SVTBIOMSTRB+NVHILO-1,:)
D1SVTGC => GDIAUX1S(:,M1SVTGC:M1SVTGC+NVHILO-1,1)
D1SVTGC2 => GDIAUX1S(:,M1SVTGC:M1SVTGC+NVHILO-1,:)
D1SVTGA => GDIAUX1S(:,M1SVTGA:M1SVTGA+NVHILO-1,1)
D1SVTGA2 => GDIAUX1S(:,M1SVTGA:M1SVTGA+NVHILO-1,:)
D1SVTF2 => GDIAUX1S(:,M1SVTF2:M1SVTF2+NVHILO-1,1)
D1SVTF22 => GDIAUX1S(:,M1SVTF2:M1SVTF2+NVHILO-1,:)
D1SVTDS => GDIAUX1S(:,M1SVTDS:M1SVTDS+NVHILO-1,1)
D1SVTDS2 => GDIAUX1S(:,M1SVTDS:M1SVTDS+NVHILO-1,:)
D1SVTDMAX => GDIAUX1S(:,M1SVTDMAX:M1SVTDMAX+NVHILO-1,1)
D1SVTDMAX2 => GDIAUX1S(:,M1SVTDMAX:M1SVTDMAX+NVHILO-1,:)
D1SVTLE => GDIAUX1S(:,M1SVTLE:M1SVTLE+NVHILO-1,1)
D1SVTLE2 => GDIAUX1S(:,M1SVTLE:M1SVTLE+NVHILO-1,:)
D1STAIR => GDIAUX1S(:,M1STAIR,1)
D1STAIR2 => GDIAUX1S(:,M1STAIR,:)
D1SQAIR => GDIAUX1S(:,M1SQAIR,1)
D1SQAIR2 => GDIAUX1S(:,M1SQAIR,:)
D1SUWIND => GDIAUX1S(:,M1SUWIND,1)
D1SUWIND2 => GDIAUX1S(:,M1SUWIND,:)
D1SVWIND => GDIAUX1S(:,M1SVWIND,1)
D1SVWIND2 => GDIAUX1S(:,M1SVWIND,:)
D1SPSURF => GDIAUX1S(:,M1SPSURF,1)
D1SPSURF2 => GDIAUX1S(:,M1SPSURF,:)
!I2DDIAUX=M1SPSURF

D1SCO2AIR => GDIAUX1S(:,M1SCO2AIR,1)
D1SCO2AIR2 => GDIAUX1S(:,M1SCO2AIR,:)
I2DDIAUX=M1SCO2AIR


! final checking

IF (I2DDI /= N2DDI) THEN
  WRITE(NULOUT,*) ' SUGDI1S: ARRAY GDI1S'
  WRITE(NULOUT,*) ' INCONSISTENT SIZE FOR DIAGNOSTICS GENERAL ARRAY'
  write(NULOUT,*) ' I2DDI=',i2ddi,' N2DDI=',n2ddi
  WRITE(NULOUT,*) ' CHANGE THE SIZE OF N2DDI'
  CALL ABORT
ENDIF

IF (I2DDIAUX /= N2DDIAUX) THEN
  WRITE(NULOUT,*) ' SUGDIAUX1S: ARRAY GDIAUX1S'
  WRITE(NULOUT,*) ' INCONSISTENT SIZE FOR DIAGNOSTICS AUXILIARY ARRAY'
  write(NULOUT,*) ' I2DDIAUX=',i2ddiaux,' N2DDIAUX=',n2ddiaux
  WRITE(NULOUT,*) ' CHANGE THE SIZE OF N2DDIAUX'
  CALL ABORT
ENDIF

IF (LHOOK) CALL DR_HOOK('SUGDI1S',1,ZHOOK_HANDLE)

RETURN
END SUBROUTINE SUGDI1S
