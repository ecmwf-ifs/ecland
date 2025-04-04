SUBROUTINE CPG1S

USE YOMDPHY  ,  ONLY : NCSS     ,NLEV     ,NGPP     ,NPOI     ,NTILES   ,&
     &                 NTRAC   ,NVHILO    ,NCOM     ,YSURF   ,NCSNEC   ,&
     &                 NBLOCKS
USE YOMCDH1S  , ONLY : NLEVI  , &
     &  NDHVTLS,NDHFTLS,NDHVTSS,NDHFTSS, &
     &  NDHVTTS,NDHFTTS,NDHVTIS,NDHFTIS, &
     &  NDHVSSS,NDHFSSS,NDHVIIS,NDHFIIS, &
     &  NDHVWLS,NDHFWLS,NDHVRESS,NDHFRESS, &
     &  NDHVCO2S,NDHFCO2S, &
     &  NDHVBIOS,NDHFBIOS,NDHVVEGS,NDHFVEGS
 
USE YOMDYN1S , ONLY : NSTEP    ,TDT,TSTEP
USE YOMGPD1S , ONLY : VFZ0F    ,VFALBF   ,&
     &            VFALUVP  ,VFALUVD  ,VFALNIP  ,VFALNID  , &
     &            VFALUVI  ,VFALUVV  ,VFALUVG  , &
     &            VFALNII  ,VFALNIV  ,VFALNIG  , &
     &            VFITM    , &
     &            VFZ0H    ,VFCVL    ,VFCVH    ,VFCUR    ,VFTVL    ,VFTVH    , &
     &            VFLAIL   ,VFLAIH   ,VFFWET   ,VFRSML   ,VFRSMH   , &
     &            VFSOTY   ,VFCI     ,VFSST    ,VFSDOR   , &
     &            VFCO2TYP, &
     &            VDALB    ,VDZ0F    ,VDZ0H    , &
     &            VDIEWSSTL,VDINSSSTL,VDISSHFTL,VDIETL   ,VDTSKTL,&
     &            VFLDEPTH,VFCLAKEF   ,& ! FLAKE
     &            VFZO     ,VFHO     ,VFHO_INV ,VFDO     ,VFOCDEPTH, & !KPP 
     &            VFADVT ,VFADVS  ,VFTRI0   ,VFTRI1   ,VFSWDK_SAVE,&  !KPP
     &            VDANDAYVT,VDANFMVT , &
     &            VDRESPBSTR,VDRESPBSTR2,VDBIOMASS_LAST,&
     &            VDBLOSSVT,VDBGAINVT

USE YOMGF1S  , ONLY : UNLEV0   ,VNLEV0   ,TNLEV0   ,QNLEV0   , CNLEV0  ,&
     &            PNLP0    ,UNLEV1   ,VNLEV1   ,TNLEV1   ,QNLEV1   , PNLP1, &
     &            CNLEV1   ,FSSRD    ,FSTRD    ,FLSRF    ,FCRF     , &
     &            FLSSF    , &
     &            FCSF     ,RALT
USE YOMGP1S0 , ONLY : GP0      ,TSLNU0   ,QLINU0   ,FSNNU0   , &
     &            TSNNU0   ,ASNNU0   ,RSNNU0   , WSNNU0,&
     &            TRENU0   ,WRENU0   ,TILNU0,&
     &            TLICENU0,TLMNWNU0,TLWMLNU0,TLBOTNU0,TLSFNU0,& ! FLAKE
     &            HLICENU0,HLMLNU0 ,&                           ! FLAKE
     &            UONU0    ,VONU0    ,TONU0    ,SONU0,&         ! KPP
!     &            UONUC    ,VONUC    ,USTRCNU  ,VSTRNUC,&       ! KPP
     &            LAINU0, BSTRNU0, BSTR2NU0
USE YOMGP1S1 , ONLY : GP1
USE PTRGP1S  , ONLY : MTSLNU   ,MQLINU   ,MFSNNU   , &
     &            MTSNNU   ,MASNNU   ,MRSNNU   , &
     &            MTRENU   ,MWRENU   ,MTILNU, & 
     &            MTLICENU,MTLMNWNU,MTLWMLNU,MTLBOTNU,MTLSFNU,& ! FLAKE
     &            MHLICENU,MHLMLNU,&                            ! FLAKE 
     &            MUONU    ,MVONU    ,MTONU    ,MSONU,&           !KPP
     &            MLAINU , MBSTRNU, MBSTR2NU,MWSNNU
USE YOMLOG1S,   ONLY: IDBGS1

USE YOMGC1S  , ONLY : GEMU     ,GELAM    ,GELAT
USE YOMCT01S , ONLY : NSTART
USE YOMCST   , ONLY : RG
USE YOERIP   , ONLY : RCODECM  ,RSIDECM  ,RCOVSRM  ,RSIVSRM
USE YOMDIM1S , ONLY : NPROMA

! USE YOS_OCEAN_ML, ONLY : LEOCML
! USE YOS_CST , ONLY : RTT

USE YOMRIP   , ONLY :  RCODEC  ,RSIDEC  ,RCOVSR  ,RSIVSR
USE YOEPHY   , ONLY :  RLAIINT

USE YOMGDI1S , ONLY : GDI1S    ,N2DDI ,&
                      &GDIAUX1S ,N2DDIAUX

USE YOS_SURF, ONLY : TSURF, GET_SURF
USE YOMLUN1S, ONLY : NULOUT
USE OMP_LIB
USE MPL_MODULE

#ifdef DOC
! (C) Copyright 1995- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *CPG1S* - Grid point calculations.

!     Purpose.
!     --------
!           Grid point calculations .

!**   Interface.
!     ----------
!        *CALL* *CPG1S*

!        Explicit arguments :
!        --------------------

!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!        Called by STEPO1S.

!     Reference.
!     ----------
!        ECMWF Research Department documentation of the one column model

!     Author.
!     -------
!        Pedro Viterbo   *ECMWF*

!     Modifications.
!     --------------
!        Original : 95-03-21
!        Bart vd Hurk (KNMI): multi-column setup
!     S. Boussetta/G.Balsamo May 2010 Add CTESSEL based on:
!        Marita Voogt (KNMI) "C-Tessel" 09/2005
!        Sebastien Lafont (ECMWF) "C-TESSEL"

!     ------------------------------------------------------------------
#endif
USE PARKIND1  ,ONLY : JPIM     ,JPRB,    JPRD
USE YOMHOOK   ,ONLY : LHOOK    ,DR_HOOK, JPHOOK
IMPLICIT NONE


CHARACTER*1 :: CDCONF 
REAL(KIND=JPRB) ::  ZAPRS(NPROMA,0:NLEV,NBLOCKS),ZAPHIF(NPROMA,NLEV,NBLOCKS)
REAL(KIND=JPRB) ::  ZDIFTQ(NPROMA,0:NLEV,NBLOCKS),ZDIFTS(NPROMA,0:NLEV,NBLOCKS)
REAL(KIND=JPRB) ::  ZFRSO(NPROMA,0:NLEV,NBLOCKS),ZFRTH(NPROMA,0:NLEV,NBLOCKS)
REAL(KIND=JPRB) ::  ZSTRTU(NPROMA,0:NLEV,NBLOCKS),ZSTRTV(NPROMA,0:NLEV,NBLOCKS)

REAL(KIND=JPRB), DIMENSION(NPROMA,NBLOCKS) ::  ZAN,ZAG,ZRD &
     &   , ZRSOIL_STR,ZRECO,ZCO2FLUX,ZCH4FLUX
REAL(KIND=JPRB), DIMENSION(NPROMA,NBLOCKS) ::  ZLAI,ZBIOM,ZBLOSS,ZBGAIN &
     &   , ZBIOMSTR,ZBIOMSTR2


REAL(KIND=JPRB) ::  ZMU0M(NPROMA,NBLOCKS),   ZFTG12(NPROMA,NBLOCKS)  &
     &   , ZFWEV(NPROMA,NBLOCKS)   ,ZFWSB(NPROMA,NBLOCKS)   ,ZFWG12(NPROMA,NBLOCKS) &
     &   , ZFWMLT(NPROMA,NBLOCKS)  ,ZFWROD(NPROMA,NBLOCKS)  ,ZFWRO1(NPROMA,NBLOCKS) &
     &   , ZFTLHEV(NPROMA,NBLOCKS) ,ZFTLHSB(NPROMA,NBLOCKS) ,ZVDIS(NPROMA,NBLOCKS)  &
     &   , ZCFLX(NPROMA,NTRAC,NBLOCKS)
REAL(KIND=JPRB) :: ZDHTLS(NPROMA,NTILES,NDHVTLS+NDHFTLS,NBLOCKS) 
REAL(KIND=JPRB) :: ZDHTSS(NPROMA,NCSNEC,NDHVTSS+NDHFTSS,NBLOCKS) 
REAL(KIND=JPRB) :: ZDHSSS(NPROMA,NCSNEC,NDHVSSS+NDHFSSS,NBLOCKS) 
REAL(KIND=JPRB) :: ZDHTTS(NPROMA,NCSS  ,NDHVTTS+NDHFTTS,NBLOCKS) 
REAL(KIND=JPRB) :: ZDHWLS(NPROMA,NCSS  ,NDHVWLS+NDHFWLS,NBLOCKS) 
REAL(KIND=JPRB) :: ZDHTIS(NPROMA,NLEVI ,NDHVTIS+NDHFTIS,NBLOCKS) 
REAL(KIND=JPRB) :: ZDHIIS(NPROMA,       NDHVIIS+NDHFIIS,NBLOCKS)
REAL(KIND=JPRB) :: ZDHRESS(NPROMA,2    ,NDHVRESS+NDHFRESS,NBLOCKS) 

REAL(KIND=JPRB) :: ZDIFM(NPROMA,0:NCOM,NBLOCKS)     !KPP
REAL(KIND=JPRB) :: ZDIFT(NPROMA,0:NCOM,NBLOCKS)     !KPP
REAL(KIND=JPRB) :: ZDIFS(NPROMA,0:NCOM,NBLOCKS)     !KPP
REAL(KIND=JPRB) :: ZOTKE(NPROMA,0:NCOM,NBLOCKS)     !TKE
REAL(KIND=JPRB) :: UONUC(NPOI,0:NCOM)     !KPP 
REAL(KIND=JPRB) :: VONUC(NPOI,0:NCOM)     !KPP
REAL(KIND=JPRB) :: USTRCNU(NPOI)          !KPP
REAL(KIND=JPRB) :: VSTRNUC(NPOI)          !KPP

REAL(KIND=JPRB) :: ZDHCO2S(NPROMA,NVHILO,NDHVCO2S+NDHFCO2S,NBLOCKS)
REAL(KIND=JPRB) :: ZDHBIOS(NPROMA,NVHILO,NDHVBIOS+NDHFBIOS,NBLOCKS)
REAL(KIND=JPRB) :: ZDHVEGS(NPROMA,NVHILO,NDHVVEGS+NDHFVEGS,NBLOCKS)

REAL(KIND=JPRB) ::  ZLAIHI(NPOI)
REAL(KIND=JPRB) ::  ZLAILI(NPOI)

REAL(KIND=JPRB) ::  ZEVAPTIU(NPROMA,NTILES,NBLOCKS)

 
!    *ZU10M*        U-COMPONENT WIND AT 10 M                      M/S
!    *ZV10M*        V-COMPONENT WIND AT 10 M                      M/S
!    *ZT2M*         TEMPERATURE AT 2M                                K
!    *ZD2M*         DEW POINT TEMPERATURE AT 2M                      K
!    *ZQ2M*         SPECIFIC HUMIDITY AT 2M                       KG/KG
!    *ZMEAN*        AREA AVERAGED WIND SP. AT 10 M INT. FROM KLEV   M/S
!    *ZGUST*        GUST AT 10 M                                    M/S
!    *ZZIDLWV*      Zi/L used for gustiness in wave model           m/m
!                   (NOTE: Positive values of Zi/L are set to ZERO)
!    *ZBLH*         BOUNDARY LAYER HEIGHT                            M
REAL(KIND=JPRB), DIMENSION(NPROMA,NBLOCKS) :: ZU10M,ZV10M,ZT2M,ZD2M,&
     &    ZQ2M,ZMEAN,ZGUST,ZZIDLWV,ZBLH
     
REAL(KIND=JPRB),ALLOCATABLE,TARGET:: ZGPE(:,:,:) 
REAL(KIND=JPRB), POINTER:: ZTSAE1(:,:,:),ZWSAE1(:,:,:) &
     &   , ZSNSE1(:,:,:)  , ZTSNE1(:,:,:)  , ZASNE1(:,:)  , ZRSNE1(:,:,:)  ,ZWLE1(:,:) &
     &   , ZTLE1(:,:)   ,ZTILE1(:,:,:),ZWSNE1(:,:,:)
REAL(KIND=JPRB),POINTER :: ZTLICENE1(:,:),ZTLMNWNE1(:,:),ZTLWMLNE1(:,:) &        ! FLAKE
     &   ,ZTLBOTNE1(:,:),ZTLSFNE1(:,:),ZHLICENE1(:,:),ZHLMLNE1(:,:)                ! FLAKE 
REAL(KIND=JPRB),POINTER :: ZUOE1(:,:,:),ZVOE1(:,:,:), ZTOE1(:,:,:), ZSOE1(:,:,:)   !KPP

REAL(KIND=JPRB), POINTER:: ZLAIE1(:,:,:) , ZBSTRE1(:,:,:) , ZBSTR2E1(:,:,:)

INTEGER(KIND=JPIM) :: IST,IEND,IPROMA,IFLEV, IBL, NTHREADS
INTEGER(KIND=JPIM) :: JL,J
! INTEGER(KIND=JPIM) :: OMP_GET_MAX_THREADS

REAL(KIND=JPHOOK)    :: ZHOOK_HANDLE

#include "callpar1s.intfb.h"
#include "upddiag.intfb.h"

IF (LHOOK) CALL DR_HOOK('CPG1S',0,ZHOOK_HANDLE)


!     ------------------------------------------------------------------

ASSOCIATE(LEOCML=>YSURF%YOCEAN_ML%LEOCML,RTT=>YSURF%YCST%RTT)

! Some initialisations
ZDHRESS(:,:,:,:)=0._JPRB
ZDHTTS(:,:,:,:)=0._JPRB
ZDHVEGS(:,:,:,:)=0._JPRB

! Allocate variables
ALLOCATE (ZGPE(NPROMA,NGPP,NBLOCKS))
ZTSAE1 => ZGPE(:,MTSLNU:MTSLNU+NCSS-1,:)
ZWSAE1 => ZGPE(:,MQLINU:MQLINU+NCSS-1,:)
ZSNSE1 => ZGPE(:,MFSNNU:MFSNNU+NCSNEC-1,:)
ZTSNE1 => ZGPE(:,MTSNNU:MTSNNU+NCSNEC-1,:)
ZWSNE1 => ZGPE(:,MWSNNU:MWSNNU+NCSNEC-1,:)
ZRSNE1 => ZGPE(:,MRSNNU:MRSNNU+NCSNEC-1,:)
ZASNE1 => ZGPE(:,MASNNU,:)
ZTLE1  => ZGPE(:,MTRENU,:)
ZWLE1  => ZGPE(:,MWRENU,:)
ZTILE1 => ZGPE(:,MTILNU:MTILNU+NCSS-1,:)

!* < FLAKE
ZTLICENE1 => ZGPE(:,MTLICENU,:)
ZTLMNWNE1 => ZGPE(:,MTLMNWNU,:)
ZTLWMLNE1 => ZGPE(:,MTLWMLNU,:)
ZTLBOTNE1 => ZGPE(:,MTLBOTNU,:)
ZTLSFNE1  => ZGPE(:,MTLSFNU,:)
ZHLICENE1 => ZGPE(:,MHLICENU,:)
ZHLMLNE1  => ZGPE(:,MHLMLNU,:)
!* FLAKE > 

ZUOE1 => ZGPE(:,MUONU:MUONU+(NCOM+1)-1,:)  !KPP
ZVOE1 => ZGPE(:,MVONU:MVONU+(NCOM+1)-1,:)  !KPP
ZTOE1 => ZGPE(:,MTONU:MTONU+(NCOM+1)-1,:)  !KPP
ZSOE1 => ZGPE(:,MSONU:MSONU+(NCOM+1)-1,:)  !KPP
UONUC(:,:)=0.0     !KPP 
VONUC(:,:)=0.0     !KPP
USTRCNU(:)=0.0     !KPP
VSTRNUC(:)=0.0     !KPP

!* < CTESSEL
ZLAIE1 => ZGPE(:,MLAINU:MLAINU+NVHILO-1,:)
ZBSTRE1 => ZGPE(:,MBSTRNU:MBSTRNU+NVHILO-1,:)
ZBSTR2E1 => ZGPE(:,MBSTR2NU:MBSTR2NU+NVHILO-1,:)
!* CTESSEL >



!*       1.    INITIAL COMPUTATIONS.
!              ---------------------


!     TIME STEP




!*       1.1  INITIALIZING SURFACE VARIABLES AT T-DT (FIRST TIMESTEP).
!             --------------------------------------------------------    

! --- original setting without openMP
!IST=1
!IEND=NPOI
!IPROMA=NPOI
! ---
IFLEV=NLEV
!$OMP PARALLEL
NTHREADS=OMP_GET_MAX_THREADS()
!$OMP END PARALLEL 

IF ( IDBGS1 > 1 ) THEN
  WRITE(NULOUT,*) 'CPG1S:',NPROMA,NPOI,NTHREADS
ENDIF

!$OMP PARALLEL DO SCHEDULE(DYNAMIC,1) PRIVATE(IST,IEND,IBL,IPROMA)
DO IST = 1, NPOI, NPROMA
  IEND = MIN(IST+NPROMA-1,NPOI)
  IBL = (IST-1)/NPROMA + 1
  IPROMA = IEND-IST+1

  GP1(:,1:NGPP,IBL)=GP0(:,1:NGPP,IBL)

!     STAFF FOR CALLPAR1S

  ZAPHIF(:,:,IBL)=RG*RALT
  ZAPRS(:,IFLEV,IBL)=PNLP0(:,IBL)
  ZAPRS(:,IFLEV-1,IBL)=PNLP0(:,IBL)-RALT*0.0065
! Added "MIN(1._JPRB , ... ) to computation of ZMU0M. This ensures ZMU0M is between physical boundaries.
  ZMU0M(:,IBL)=MIN(1._JPRB, MAX( RSIDECM*GEMU(:,IBL) &
     & -RCODECM*RCOVSRM*SQRT(1._JPRB-GEMU(:,IBL)**2)*COS(GELAM(:,IBL)) &
     & +RCODECM*RSIVSRM*SQRT(1._JPRB-GEMU(:,IBL)**2)*SIN(GELAM(:,IBL)) &
     & ,0._JPRB) )

  IF(LEOCML) THEN
! replace SST with the top layer temp. of the ocean mixed layer (~ 1m depth)
    VFSST(:,IBL) = TONU0(:,1,IBL) + RTT ! The unit of PSST(VFSST) is K.  
  ENDIF

!! init 
  ZCFLX(:,:,IBL) = 0._JPRB 

!*      2.   CALL PHYSICS.
!            -------------
  CDCONF='F'
  CALL CALLPAR1S (CDCONF &
     & , 1, IPROMA   , NPROMA , NCSS  , NTILES, NVHILO & 
     & , IFLEV  ,NSTART ,NSTEP  ,NTRAC   &
     & , NLEVI  ,NCOM   ,NCSNEC          &                 !KPP
     & , NDHVTLS,NDHFTLS,NDHVTSS,NDHFTSS &
     & , NDHVTTS,NDHFTTS,NDHVTIS,NDHFTIS &
     & , NDHVSSS,NDHFSSS,NDHVIIS,NDHFIIS &
     & , NDHVWLS,NDHFWLS,NDHVRESS,NDHFRESS &
     & , NDHVCO2S,NDHFCO2S &
     & , NDHVBIOS,NDHFBIOS,NDHVVEGS,NDHFVEGS  &
     & , TDT &
!-----------------------------------------------------------------------
! - INPUT .
     & , ZAPHIF(:,:,IBL) , ZAPRS(:,:,IBL) &
     & , UNLEV0(:,:,IBL) , VNLEV0(:,:,IBL) , TNLEV0(:,:,IBL) , QNLEV0(:,:,IBL), CNLEV0(:,:,IBL) &
     & , FSNNU0(:,:,IBL) &
     & , ASNNU0(:,IBL) , RSNNU0(:,:,IBL) , TSNNU0(:,:,IBL),WSNNU0(:,:,IBL) &
     & , TSLNU0(:,:,IBL) , WRENU0(:,IBL) , TRENU0(:,IBL) , QLINU0(:,:,IBL) &
     & , TILNU0(:,:,IBL) &
     & , TLICENU0(:,IBL),TLMNWNU0(:,IBL),TLWMLNU0(:,IBL),TLBOTNU0(:,IBL),TLSFNU0(:,IBL) & ! FLAKE
     & , HLICENU0(:,IBL),HLMLNU0(:,IBL),VFLDEPTH(:,IBL),VFCLAKEF(:,IBL) &   ! FLAKE 
     & , VFALBF(:,IBL) &
     & , VFALUVP(:,IBL),VFALUVD(:,IBL) ,VFALNIP(:,IBL) ,VFALNID(:,IBL) &
     & , VFALUVI(:,IBL),VFALUVV(:,IBL) ,VFALUVG(:,IBL) &
     & , VFALNII(:,IBL),VFALNIV(:,IBL) ,VFALNIG(:,IBL) &
     & , VFCVL(:,IBL)  , VFCVH(:,IBL)  , VFCUR(:,IBL) , VFTVL(:,IBL)  , VFCO2TYP(:,IBL), VFTVH(:,IBL)  &
     & , VFLAIL(:,IBL) , VFLAIH(:,IBL) , VFFWET(:,IBL) , VFRSML(:,IBL) , VFRSMH(:,IBL) &
     & , VFSOTY(:,IBL) , VFSDOR(:,IBL) &
     & , VFCI(:,IBL)   , VFSST(:,IBL)   , GEMU(:,IBL),  GELAT(:,IBL), ZCFLX(:,:,IBL) &
     & , VDIEWSSTL(:,:,IBL),VDINSSSTL(:,:,IBL),VDISSHFTL(:,:,IBL),VDIETL(:,:,IBL)   ,VDTSKTL(:,:,IBL) &
     & , VDANDAYVT(:,:,IBL),VDANFMVT(:,:,IBL) &
     & , VDRESPBSTR(:,:,IBL),VDRESPBSTR2(:,:,IBL),VDBIOMASS_LAST(:,:,IBL),BSTRNU0(:,:,IBL),BSTR2NU0(:,:,IBL) & 
     & , VDBLOSSVT(:,:,IBL),VDBGAINVT(:,:,IBL) &
     & , VFITM(:,IBL) &
     & , VFZ0F(:,IBL)  , ZMU0M(:,IBL) &
     & , VFZ0H(:,IBL) &
     & , VFZO(:,:,IBL)     ,VFHO(:,:,IBL)     ,VFHO_INV(:,:,IBL)  ,VFDO(:,:,IBL)      ,VFOCDEPTH(:,IBL)  &!KPP
     & , VFADVT(:,:,IBL)   ,VFADVS(:,:,IBL)   ,VFTRI0(:,:,IBL)    ,VFTRI1(:,:,IBL)    ,VFSWDK_SAVE(:,:,IBL)&!KPP
!     & , UONUC    ,VONUC    ,USTRCNU   ,VSTRNUC               &!KPP
     & , UONU0(:,:,IBL)    ,VONU0(:,:,IBL)    ,TONU0(:,:,IBL)     ,SONU0(:,:,IBL)                 &!KPP
! - INPUT AT T+1, AND/OR ATMOSPHERIC FORCING
     & , UNLEV1(:,:,IBL) , VNLEV1(:,:,IBL) , TNLEV1(:,:,IBL) , QNLEV1(:,:,IBL) , CNLEV1(:,:,IBL) &
     & , FSSRD(:,IBL)  , FSTRD(:,IBL) &
     & , FCRF(:,IBL)   , FCSF(:,IBL)   , FLSRF(:,IBL)  , FLSSF(:,IBL) &
! - OUTPUT .
     & , ZEVAPTIU(:,:,IBL)&  
     &  ,ZU10M(:,IBL)  ,ZV10M(:,IBL)   ,ZT2M(:,IBL)    ,ZD2M(:,IBL)  &
     &  ,ZQ2M(:,IBL)   ,ZMEAN(:,IBL)   ,ZGUST(:,IBL)   ,ZZIDLWV(:,IBL)   ,ZBLH(:,IBL)  &
     & , ZDIFTQ(:,:,IBL) , ZDIFTS(:,:,IBL) &
     & , ZFRSO(:,:,IBL) &
     & , ZFRTH(:,:,IBL) &
     & , ZSTRTU(:,:,IBL) , ZSTRTV(:,:,IBL) &
     & , ZAN(:,IBL), ZAG(:,IBL), ZRD(:,IBL), ZRSOIL_STR(:,IBL), ZRECO(:,IBL), ZCO2FLUX(:,IBL),ZCH4FLUX(:,IBL) &
     & , ZLAI(:,IBL),LAINU0(:,1,IBL),LAINU0(:,2,IBL), ZBIOM(:,IBL), ZBLOSS(:,IBL), ZBGAIN(:,IBL), &
           & ZBIOMSTR(:,IBL), ZBIOMSTR2(:,IBL) &
     & , VDALB(:,IBL) &
     & , ZFTG12(:,IBL) , ZFWEV(:,IBL)  , ZFWSB(:,IBL)  , ZFWG12(:,IBL) , ZFWMLT(:,IBL) , VDZ0F(:,IBL)  , &
           & VDZ0H(:,IBL) &
     & , ZFWROD(:,IBL) , ZFWRO1(:,IBL) , ZFTLHEV(:,IBL), ZFTLHSB(:,IBL) &
     & , ZVDIS(:,IBL) &
     & , ZDIFM(:,:,IBL)  , ZDIFT(:,:,IBL)  , ZDIFS(:,:,IBL), ZOTKE(:,:,IBL) &             !KPP/TKE
     & , ZSNSE1(:,:,IBL) , ZASNE1(:,IBL) , ZRSNE1(:,:,IBL) , ZTSNE1(:,:,IBL),ZWSNE1(:,:,IBL) &
     & , ZTSAE1(:,:,IBL) &
     & , ZWLE1(:,IBL)  , ZTLE1(:,IBL)  , ZWSAE1(:,:,IBL) &
     & , ZTILE1(:,:,IBL) &
     & , ZTLICENE1(:,IBL),ZTLMNWNE1(:,IBL),ZTLWMLNE1(:,IBL)&          ! FLAKE 
     & , ZTLBOTNE1(:,IBL),ZTLSFNE1(:,IBL),ZHLICENE1(:,IBL),ZHLMLNE1(:,IBL) & ! FLAKE 
     & , ZUOE1(:,:,IBL)  , ZVOE1(:,:,IBL)  , ZTOE1(:,:,IBL)  , ZSOE1(:,:,IBL) &            !KPP
     & , ZLAIE1(:,:,IBL), ZBSTRE1(:,:,IBL), ZBSTR2E1(:,:,IBL) &
     & , ZDHTLS(:,:,:,IBL),ZDHTSS(:,:,:,IBL),ZDHTTS(:,:,:,IBL),ZDHTIS(:,:,:,IBL),ZDHIIS(:,:,IBL), &
           & ZDHSSS(:,:,:,IBL),ZDHWLS(:,:,:,IBL),ZDHRESS(:,:,:,IBL) &
     & , ZDHCO2S(:,:,:,IBL),ZDHBIOS(:,:,:,IBL),ZDHVEGS(:,:,:,IBL),GELAT(:,IBL),GELAM(:,IBL))

!*           UPDATE DIAGNOSTICS
!     ------------------------------------------------------------------

  CALL UPDDIAG( &
   & 1,IPROMA,NPROMA,IFLEV,NCSS,NCSNEC,NTILES,NVHILO,&   
   & NDHVTLS,NDHFTLS,NDHVTSS,NDHFTSS, &
   & NDHVTTS,NDHFTTS,NDHVSSS,NDHFSSS, &
   & NDHVIIS,NDHFIIS,NDHVWLS,NDHFWLS, &
   & NDHVRESS,NDHFRESS, &
   & NDHVCO2S,NDHFCO2S,NDHVBIOS,NDHFBIOS, &
   & NDHVVEGS,NDHFVEGS, & 
   & TDT,IBL, &
   & ZEVAPTIU(:,:,IBL),&
   & ZAN(:,IBL),ZAG(:,IBL),ZRD(:,IBL),ZRSOIL_STR(:,IBL),ZRECO(:,IBL),ZCO2FLUX(:,IBL),ZCH4FLUX(:,IBL), &
   & ZLAI(:,IBL),ZBIOM(:,IBL),ZBLOSS(:,IBL),ZBGAIN(:,IBL),ZBIOMSTR(:,IBL),ZBIOMSTR2(:,IBL), &
   & TNLEV1(:,:,IBL),QNLEV1(:,:,IBL),UNLEV1(:,:,IBL),VNLEV1(:,:,IBL),PNLP1(:,IBL),&
   & ZFRSO(:,:,IBL),FSSRD(:,IBL),ZFRTH(:,:,IBL),FSTRD(:,IBL),VDALB(:,IBL),ZFTLHEV(:,IBL),ZFTLHSB(:,IBL), &
   & ZDIFTQ(:,:,IBL),ZDIFTS(:,:,IBL),ZSTRTU(:,:,IBL),ZSTRTV(:,:,IBL),ZWSAE1(:,:,IBL),ZSNSE1(:,:,IBL), &
       & ZWLE1(:,IBL), &
   & FCRF(:,IBL)    ,FCSF(:,IBL)   ,FLSRF(:,IBL),FLSSF(:,IBL), ZFWRO1(:,IBL),ZFWROD(:,IBL),ZFWMLT(:,IBL),&
   & ZDHTLS(:,:,:,IBL),ZDHTSS(:,:,:,IBL),ZDHTTS(:,:,:,IBL),ZDHSSS(:,:,:,IBL),ZDHIIS(:,:,IBL),ZDHWLS(:,:,:,IBL), &
       & ZDHRESS(:,:,:,IBL),&
   & ZT2M(:,IBL),ZD2M(:,IBL),ZDHCO2S(:,:,:,IBL),ZDHBIOS(:,:,:,IBL),ZDHVEGS(:,:,:,IBL))
 

!*       3.  COMPUTATION OF T+DT VALUES FOR SURFACE VARIABLES.
!            -------------------------------------------------
! WARM START FIX
  IF (NSTEP == 0) THEN 
    GP1(:, MRSNNU:MRSNNU+NCSNEC-1,IBL) = RSNNU0(:, 1:NCSNEC, IBL) 
    GP1(:, MTSNNU:MTSNNU+NCSNEC-1,IBL) = TSNNU0(:, 1:NCSNEC, IBL)
    GP1(:, MFSNNU:MFSNNU+NCSNEC-1,IBL) = FSNNU0(:, 1:NCSNEC, IBL) 
    GP1(:, MWSNNU:MWSNNU+NCSNEC-1,IBL) = WSNNU0(:, 1:NCSNEC, IBL) 
  END IF


  GP1(:,1:NGPP,IBL)=GP1(:,1:NGPP,IBL) + TDT*ZGPE(:,1:NGPP,IBL)
!*       4.  SWAPPING OF SURFACE VARIABLES.
!            ------------------------------

  GP0(:,1:NGPP,IBL)=GP1(:,1:NGPP,IBL)

!*       5.   ACCUMULATE 1D DIAGNOSTIC TENDENCIES.
!             ------------------------------------

  DO J=1,N2DDI
    GDI1S(:,J,2,IBL)=GDI1S(:,J,2,IBL)+TSTEP*GDI1S(:,J,1,IBL)
  ENDDO

!*       6.   AVERAGE AUXILIARY DIAGNOSTICS
!             ------------------------------------

  DO J=1,N2DDIAUX
    GDIAUX1S(:,J,2,IBL)=GDIAUX1S(:,J,2,IBL)+TSTEP*GDIAUX1S(:,J,1,IBL)
  ENDDO

ENDDO !IST LOOP
!$OMP END PARALLEL DO 


!     ------------------------------------------------------------------

DEALLOCATE (ZGPE)
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('CPG1S',1,ZHOOK_HANDLE)
RETURN
END SUBROUTINE CPG1S
