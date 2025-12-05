SUBROUTINE UPDTIM1S(KSTEP,PTDT,PTSTEP)


USE YOMLUN1S , ONLY : NULOUT
USE YOMLOG1S , ONLY : CFFORC
USE YOMRIP   , ONLY : NSSSSS   ,NSTADD   ,NSTASS   ,RTIMST   , &
     &            RSTATI   ,RTIMTR   ,RHGMT    ,REQTIM   ,RSOVR    ,&
     &            RDEASO   ,RDECLI   ,RWSOVR   ,RIP0     ,RCODEC   ,&
     &            RSIDEC   ,RCOVSR   ,RSIVSR   ,RTDT
USE YOMCST   , ONLY : RPI      ,RDAY     ,REA      ,RI0, REPSM
USE YOERIP   , ONLY : RIP0M    ,RCODECM  ,RSIDECM  ,RCOVSRM  ,&
     &            RSIVSRM
USE YOMDYN1S , ONLY : NSTEP    ,TSTEP
USE YOMCC1S  , ONLY : VCALB ,VCLAIL   ,VCLAIH,VCFWET,VCAVGPAR

USE YOMGPD1S , ONLY : VFALBF   ,&
     &                VFALUVP,VFALUVD,VFALNIP,VFALNID , &
     &                VFALUVV,VFALUVG, &
     &                VFALNIV,VFALNIG, &
     &                VFLAIL,VFLAIH ,VFFWET,VFTVL,VFTVH, VFBVOCLAIL, VFBVOCLAIH, &
     &                VFAVGPAR

USE YOMDPHY  , ONLY : NPOI
USE YOEPHY, ONLY: LECTESSEL, LECLIM10D
USE YOMCT01S , ONLY : NSTART
USE YOMDIM1S , ONLY : NPROMA

#ifdef DOC
! (C) Copyright 1995- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *UPDTIM1S* - UPDATE TIME OF THE ONE COLUMN SURFACE MODEL 

!     Purpose.
!     --------
!     UPDATE TIME OF THE ONE COLUMN SURFACE MODEL 

!**   Interface.
!     ----------
!        *CALL* *UPDTIM1S(KSTEP,PTDT,PTSTEP)

!        Explicit arguments :
!        --------------------
!        KSTEP : TIME STEP INDEX
!        PTDT  : TIME STEP LEAPFROG
!        PTSTEP: TIME STEP

!        Implicit arguments :
!        --------------------
!        YOMRIP

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the 
!        one column surface model 

!     Author.
!     -------
!        Jean-Francois Mahfouf and Pedro Viterbo  *ECMWF*

!     Modifications.
!     --------------
!        Original : 95-03-22

!     ------------------------------------------------------------------
#endif

USE PARKIND1  ,ONLY : JPIM     ,JPRB , JPRD, JPIB
USE YOMHOOK   ,ONLY : LHOOK    ,DR_HOOK, JPHOOK

IMPLICIT NONE

!* Arguments
INTEGER(KIND=JPIM) :: KSTEP
INTEGER(KIND=JPIM) :: IST,IEND,IBL,IPROMA
REAL(KIND=JPRB) :: PTDT,PTSTEP

!* Local variables
INTEGER(KIND=JPIM) :: ITIME,IPR,NRADFR,ISTADD,ISTASS,IYMD,IHM,IDD, &
     &      IMM,IYYYY,IHH,ISS,IMT1,IMT2,IYT1,IYT2,JL,IMT11,IMT12,IDD1,IDD2, &
     &      IDD_BVOC, IMM_BVOC, IYYYY_BVOC, IMT11_BVOC,IMT12_BVOC
INTEGER(KIND=JPIB) :: IZT
REAL(KIND=JPRD) :: ZTETA,ZSTATI,ZHGMT,ZDEASOM,ZDECLIM,ZEQTIMM,ZSOVRM,&
     &      ZWSOVRM,ZJUL,ZTIMTR,ZT1,ZT2,ZT,ZT_BVOC,ZWEI1,ZWEI2,ZWEI1_BVOC,ZWEI2_BVOC, &
     &      ZT1_BVOC,ZT2_BVOC
!          ,ZRVCOV(0:20) !original CTESSEL (0:7)

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

#include "fctast.h"
#include "fcttim.h"
#include "surf_inq.h"

IF (LHOOK) CALL DR_HOOK('UPDTIM1S',0,ZHOOK_HANDLE)
 

ITIME=NINT(PTSTEP)
IZT=NINT(REAL(PTSTEP,KIND=JPRD)*(REAL(KSTEP,KIND=JPRD)+0.5_JPRD),KIND=JPIB)
!CBH      IZT=NINT(PTSTEP*(REAL(KSTEP)))
RSTATI=REAL(IZT,KIND=JPRD)
NSTADD=IZT/NINT(RDAY)
NSTASS=MOD(IZT,NINT(RDAY))
RTIMTR=RTIMST+RSTATI
IPR=0
IF(IPR.EQ.1)THEN
  WRITE(UNIT=NULOUT,FMT='(1X,'' TIME OF THE MODEL '',E21.14,&
     & '' TIME SINCE START '',E21.14)') RTIMTR,RSTATI
ENDIF
RHGMT=REAL(MOD(NINT(RSTATI)+NSSSSS,NINT(RDAY)),KIND=JPRD)

ZTETA=RTETA(RTIMTR)
RDEASO=RRS(ZTETA)
RDECLI=RDS(ZTETA)
REQTIM=RET(ZTETA)
RSOVR =REQTIM+RHGMT
RWSOVR=RSOVR*2._JPRD*REAL(RPI/RDAY,KIND=JPRD)
RIP0=RI0*REA*REA/(RDEASO*RDEASO)

RCODEC=COS(RDECLI)
RSIDEC=SIN(RDECLI)

RCOVSR=COS(RWSOVR)
RSIVSR=SIN(RWSOVR)

RTDT=PTDT


!          2.   PARAMETERS FOR ECMWF-STYLE INTERMITTENT RADIATION 
!               -------------------------------------------------

NRADFR=1
ITIME=NINT( TSTEP)
IZT=NINT( REAL(TSTEP,KIND=JPRD)*(REAL(KSTEP,KIND=JPRD)+0.5_JPRD),KIND=JPIB)
!CBH      IZT=NINT( TSTEP*(REAL(KSTEP)))
ZSTATI=REAL(IZT,KIND=JPRD)+REAL(0.5_JPRD*NRADFR*ITIME,KIND=JPRD)
ISTADD=IZT/NINT(RDAY)
ISTASS=MOD(IZT,NINT(RDAY))
ZTIMTR=RTIMST+ZSTATI
ZHGMT=REAL(MOD(NINT(ZSTATI,KIND=JPIB)+NSSSSS,NINT(RDAY)),KIND=JPRD)

ZTETA=RTETA(ZTIMTR)
ZDEASOM=RRS(ZTETA)
ZDECLIM=RDS(ZTETA)
ZEQTIMM=RET(ZTETA)
ZSOVRM =ZEQTIMM+ZHGMT                                 
ZWSOVRM=ZSOVRM*2._JPRD*REAL(RPI/RDAY,KIND=JPRD)
RIP0M=RI0*REA*REA/(ZDEASOM*ZDEASOM)

RCODECM=COS(ZDECLIM)
RSIDECM=SIN(ZDECLIM)

RCOVSRM=COS(ZWSOVRM)
RSIVSRM=SIN(ZWSOVRM)


!          2.   MODIFY SEASONALLY VARYING FIELDS 
!               --------------------------------

call dattim(zjul,iymd,ihm)
if (nstep == 0 .OR. ihm == 0000 .or. nstep==nstart) then
!  Update albedo at 00 GMT, or define it for the first time step
  idd=ndd(iymd)
  imm=nmm(iymd)
  iyyyy=nccaa(iymd)

!START BVOC EMISSION MODULE: compute approximately 10 days prior to current date

if (idd <= 10) then ! if subtracting 10 days gets us in the prior month/year
	! compute new day
	if (imm == 3) then
		idd_bvoc=18+idd !omit leap years
	else if ((imm == 5) .OR. (imm == 7) .OR. (imm == 10) .OR. (imm == 12)) then
		idd_bvoc=20+idd
	else
		idd_bvoc=21+idd
	endif
	!compute new month and year
     	if(imm == 1) then
		imm_bvoc=12
		iyyyy_bvoc=iyyyy-1
     	else
		imm_bvoc=imm-1
		iyyyy_bvoc=iyyyy
     	endif
else				! simple case when day > 10
  	idd_bvoc=idd-10
  	imm_bvoc=imm
  	iyyyy_bvoc=iyyyy
endif

!END NEW BVOC EMISSION MODULE
  
  ihh=ihm/100
  iss=60*ihh+60*mod(ihm,100)


IF (LECLIM10D) THEN !LECLIM10D TRUE
  if (idd >= 5 .and. idd < 15) then
      idd1=5
      idd2=15
      imt1=imm
      imt2=imm
      iyt1=iyyyy
      iyt2=iyyyy
      zt1=RTIME(iyt1,imt1,5,0)
      zt2=RTIME(iyt2,imt2,15,0)
      imt11=(imm-1)*3+2
      imt12=(imm-1)*3+3

  else if (idd >= 15 .and. idd < 25) then
      idd1=15
      idd2=25
      imt1=imm
      imt2=imm
      iyt1=iyyyy
      iyt2=iyyyy
      zt1=RTIME(iyt1,imt1,15,0)
      zt2=RTIME(iyt2,imt2,25,0)
      imt11=(imm-1)*3+3
      imt12=(imm-1)*3+4

  else if (idd < 5 ) then
      idd1=25
      idd2=5
      imt1=1+mod(imm+10,12)
      imt2=imm
      if(imt1 == 12) then
        iyt1=iyyyy-1
      else
        iyt1=iyyyy
      endif 
      iyt2=iyyyy

      zt1=RTIME(iyt1,imt1,25,0)
      zt2=RTIME(iyt2,imt2,5,0)

      imt11=(imm-1)*3+1
      imt12=(imm-1)*3+2

  else if (idd >= 25 ) then
      idd1=25
      idd2=5
      imt1=imm
      imt2=1+mod(imm,12)
      iyt1=iyyyy
      if(imt2 == 1) then
        iyt2=iyt1+1
      else
        iyt2=iyt1
      endif
      zt1=RTIME(iyt1,imt1,25,0)
      zt2=RTIME(iyt2,imt2,5,0)

      imt11=(imm-1)*3+4
      imt12=(imm-1)*3+5


  endif
ELSE ! LECLIM10D FALSE

   if (idd >= 15) then
      imt1=imm
      imt2=1+mod(imm,12)
      iyt1=iyyyy
     if(imt2 == 1) then
      iyt2=iyt1+1
     else
      iyt2=iyt1
     endif
  else
    imt1=1+mod(imm+10,12)
    imt2=imm
    if(imt1 == 12) then
      iyt1=iyyyy-1
    else
      iyt1=iyyyy
    endif
    iyt2=iyyyy
  endif
  zt1=RTIME(iyt1,imt1,15,0)
  zt2=RTIME(iyt2,imt2,15,0)

  imt11=imt1
  imt12=imt2

  ! START NEW BVOC EMISSION MODULE
     if (idd_bvoc >= 15) then
      imt1=imm_bvoc		!current month
      imt2=1+mod(imm_bvoc,12)	!month after
      iyt1=iyyyy_bvoc
     if(imt2 == 1) then
      iyt2=iyt1+1
     else
      iyt2=iyt1
     endif
  else
    imt1=1+mod(imm_bvoc+10,12)  ! this gives the month before
    imt2=imm_bvoc		! this gives the current month
    if(imt1 == 12) then
      iyt1=iyyyy_bvoc-1
    else
      iyt1=iyyyy_bvoc
    endif
    iyt2=iyyyy_bvoc
  endif
  zt1_bvoc=RTIME(iyt1,imt1,15,0)       !mid of month before or current
  zt2_bvoc=RTIME(iyt2,imt2,15,0)       !mid of month current or after

  imt11_bvoc=imt1
  imt12_bvoc=imt2
  !END NEW BVOC EMISSION MODULE

WRITE(NULOUT,*) 'imt11_bvoc,imt12_bvoc  = ', imt11_bvoc,imt12_bvoc
  
ENDIF  ! LECLIM10D


! zt=RTIME(iyyyy,imm,idd,iss)
zt=RTIME(iyyyy,imm,idd,0)  !updated assuming we're at 00UTC
zwei1=(zt2-zt)/(zt2-zt1)
zwei2=1._JPRB-zwei1

zt_bvoc=RTIME(iyyyy_bvoc,imm_bvoc,idd_bvoc,0) ! new bvoc emission module
zwei1_bvoc=(zt2_bvoc-zt_bvoc)/(zt2_bvoc-zt1_bvoc)
zwei2_bvoc=1._JPRB-zwei1_bvoc


!$OMP PARALLEL DO PRIVATE(IST,IEND,IBL,IPROMA)
DO IST = 1, NPOI, NPROMA
  IEND = MIN(IST+NPROMA-1,NPOI)
  IBL = (IST-1)/NPROMA + 1
  IPROMA = IEND-IST+1

  VFALBF(1:IPROMA,IBL)=ZWEI1*VCALB(IST:IEND,IMT11)+ZWEI2*VCALB(IST:IEND,IMT12)
  VFALUVP(1:IPROMA,IBL)=ZWEI1*VCALB(IST:IEND,IMT11)+ZWEI2*VCALB(IST:IEND,IMT12)
  VFALUVD(1:IPROMA,IBL)=ZWEI1*VCALB(IST:IEND,IMT11)+ZWEI2*VCALB(IST:IEND,IMT12)
  VFALNIP(1:IPROMA,IBL)=ZWEI1*VCALB(IST:IEND,IMT11)+ZWEI2*VCALB(IST:IEND,IMT12)
  VFALNID(1:IPROMA,IBL)=ZWEI1*VCALB(IST:IEND,IMT11)+ZWEI2*VCALB(IST:IEND,IMT12)
  VFLAIL(1:IPROMA,IBL)=ZWEI1*VCLAIL(IST:IEND,IMT11)+ZWEI2*VCLAIL(IST:IEND,IMT12)
  VFLAIH(1:IPROMA,IBL)=ZWEI1*VCLAIH(IST:IEND,IMT11)+ZWEI2*VCLAIH(IST:IEND,IMT12)
  VFFWET(1:IPROMA,IBL)=ZWEI1*VCFWET(IST:IEND,IMT11)+ZWEI2*VCFWET(IST:IEND,IMT12)
  VFAVGPAR(1:IPROMA,IBL)=ZWEI1*VCAVGPAR(IST:IEND,IMT11)+ZWEI2*VCAVGPAR(IST:IEND,IMT12)
  VFBVOCLAIL(1:IPROMA,IBL)=ZWEI1_BVOC*VCLAIL(IST:IEND,IMT11_BVOC)+ZWEI2_BVOC*VCLAIL(IST:IEND,IMT12_BVOC)
  VFBVOCLAIH(1:IPROMA,IBL)=ZWEI1_BVOC*VCLAIH(IST:IEND,IMT11_BVOC)+ZWEI2_BVOC*VCLAIH(IST:IEND,IMT12_BVOC)
ENDDO
!$OMP END PARALLEL DO

! 6-component MODIS albedo: to use an albedo independent of solar
! zenith angle, set only the isotropic component
VFALUVV(:,:)=0.0_JPRB
VFALUVG(:,:)=0.0_JPRB
VFALNIV(:,:)=0.0_JPRB
VFALNIG(:,:)=0.0_JPRB

!IF (LECTESSEL) THEN
! update LAIL LAIH BCL BCH 
!  CALL SURF_INQ(PRVCOV=ZRVCOV)
!
!    ! crop biome cover is dependant on LAI (not used in this version, rcov is kept cte according to lookup table)
!  DO JL=1,NPOI 
!    IF (VFTVL(JL)>=6) THEN
!      VFBCL(JL)=1._JPRB-EXP(-0.6_JPRB*VFLAIL(JL))
!   ELSE
!      VFBCL(JL) =ZRVCOV(INT(VFTVL(JL)))
!   ENDIF 
!   !high vegetation
!   VFBCH(JL) = ZRVCOV(INT(VFTVH(JL)))
!  END DO
! ENDIF

endif

IF (LHOOK) CALL DR_HOOK('UPDTIM1S',1,ZHOOK_HANDLE)

RETURN
END SUBROUTINE UPDTIM1S
