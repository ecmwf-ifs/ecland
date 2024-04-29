MODULE BVOC_EMIS_MOD
CONTAINS
SUBROUTINE BVOC_EMIS(KIDIA,KFDIA,KLON,KVTYPE,&
     & PPPFD_TOA, &
     & PTM1,PCM1, PTSKM1M, PTSOIL,&
     & PLAI, PLAIP, PSRFD, PMU0, PLAT, PAVGPAR, &
     & YDBVOC,YDAGF,PBVOCFLUX)

  !**   *BVOC_EMIS* - CALCULATES NET Biogenic VOC emissions per land use type  

  !     v. Huijnen  * KNMI *     20/12/2022 
  !     MODIFIED BY
  !     xxx, date, comment
   
  !     PURPOSE
  !     -------
  !     Calculates net BVOC emission fluxes

  !     INTERFACE
  !     ---------
  !     BVOC_EMIS IS CALLED BY *VSURF_MOD* 

  !     PARAMETER     DESCRIPTION                                   UNITS
  !     ---------     -----------                                   -----
  !     INPUT PARAMETERS (INTEGER):

  !     *KVTYPE*       VEGETATION TYPE CORRESPONDING TO TILE 

  !     INPUT PARAMETERS (REAL)
  !     *PFRTI*      TILE FRACTIONS                                   (0-1)
  !            1 : WATER                  5 : SNOW ON LOW-VEG+BARE-SOIL
  !            2 : ICE                    6 : DRY SNOW-FREE HIGH-VEG
  !            3 : WET SKIN               7 : SNOW UNDER HIGH-VEG
  !            4 : DRY SNOW-FREE LOW-VEG  8 : BARE SOIL
  !     *PTM1*         TEMPERATURE AT T-1                            K
  !     *PQM1*         SPECIFIC HUMIDITY AT T-1                      KG/KG 
  !     *PCM1*         ATMOSPHERIC CO2 AT T-1                      KG/KG 
  !     *PAPHM1*       PRESSURE AT T-1				   PA
  !     *PTSKM1M*      SURFACE TEMPERATURE                           K
  !     *PTSOIL*       SOIL TEMPERATURE LEVEL 3 (28 - 100cm)         K
  !     *PEVAP*        PRELIMINARY MOISTURE FLUX                     KG/M2/S
  !     *PLAI*         LEAF AREA INDEX                               M2/M2
  !     *PLAIP*        LEAF AREA INDEX Previous time step            M2/M2
  !     *PSRFD*        DOWNWARD SHORT WAVE RADIATION FLUX AT SURFACE W/M2
  !     *PRAQ*         PRELIMINARY AERODYNAMIC RESISTANCE            S/M
  !     *PMU0*         LOCAL COSINE OF INSTANTANEOUS MEAN SOLAR ZENITH ANGLE
  !     *PLAT*         LATITUDE (Radians)
  !     *PAVGPAR*      (Climatological) AVG PHOTOSYNTHETIC ACTIVE RADIATION W/M2
  !     *PF2*	       SOIL MOISTURE STRESS FUNCTION 	           -
  !     *PQS*          SATURATION Q AT SURFACE			   KG/KG

  !     OUTPUT PARAMETERS (REAL):

  !     *PBVOCFLUX*    net tile-specific BVOC emissions,     KG_BVOC/M2/S
  !                    positive downwards, to be changed for diagnostic output

  !     METHOD
  !     ------
  !     ??

  !     REFERENCE
  !     ---------
  !     (Sindelarova?)

  !     ------------------------------------------------------------------------

  USE PARKIND1   ,ONLY : JPIM, JPRB
  USE YOMHOOK    ,ONLY : LHOOK, DR_HOOK, JPHOOK
  USE YOS_CST    ,ONLY : TCST
  USE YOS_AGS    ,ONLY : TAGS
  USE YOS_AGF    ,ONLY : TAGF
  USE YOS_VEG    ,ONLY : TVEG
  USE YOS_BVOC   ,ONLY : TBVOC
  USE YOS_FLAKE  ,ONLY : TFLAKE
  USE ABORT_SURF_MOD

  IMPLICIT NONE
  INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
  INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
  INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
  INTEGER(KIND=JPIM),INTENT(IN)    :: KVTYPE(:)
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PPPFD_TOA
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PTM1(:)
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PCM1(:)
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSKM1M(:)
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PTSOIL(:) 
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAI(:)
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAIP(:)
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PSRFD(:)
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PMU0(:)
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PLAT(:)
  REAL(KIND=JPRB)   ,INTENT(IN)    :: PAVGPAR(:)
  TYPE(TBVOC)       ,INTENT(IN)    :: YDBVOC
  TYPE(TAGF)        ,INTENT(IN)    :: YDAGF
  REAL(KIND=JPRB)   ,INTENT(OUT)   :: PBVOCFLUX(:,:)

  !*         0.     LOCAL VARIABLES.
  !                 ----- ----------

  INTEGER(KIND=JPIM) :: JL,JSP   ! index for loops

  INTEGER(KIND=JPIM) :: KVTYPE_MEGAN(KLON)

  REAL(KIND=JPRB)    :: ZBVOCFLUX(KLON,YDBVOC%NEMIS_BVOC)
  REAL(KIND=JPRB)    :: ZLAT(KLON) ! Latitude, converted to degree

  ! gamma's: Activity factors 
  REAL(KIND=JPRB)    :: ZGAMMA_OVERALL(YDBVOC%NEMIS_BVOC)      ! Overall activity factor
  REAL(KIND=JPRB)    :: ZGAMMA_CE(KLON,YDBVOC%NEMIS_BVOC)      ! Canopy Environment activity factor
  REAL(KIND=JPRB)    :: ZGAMMA_AGE(KLON)                       ! Maturity of foliage activity factor for deciduous PFTs
  REAL(KIND=JPRB)    :: ZGAMMA_CO2(KLON,YDBVOC%NEMIS_BVOC)     ! emission inhibition due to ambient CO2 conc
  REAL(KIND=JPRB)    :: ZGAMMA_LAI(KLON)                       ! LAI activity factor
  REAL(KIND=JPRB)    :: ZGAMMA_T(KLON)                         ! Temperature activity factor
  REAL(KIND=JPRB)    :: ZGAMMA_P(KLON)                         ! light response activity factor

  LOGICAL            :: LDLAND(KLON)
  

! VEGETATION TYPES ARE:
! 1  DECIDUOUS
! 2  CONIFEROUS
! 3  EVERGREEN
! 4  C3 GRASS
! 5  C4 GRASS
! 6  C3 CROPS
! 7  C4 CROPS 

! MATCHING BATS table with MEGAN PFT categories  

! (1)  ! LOW  - Crops, Mixed Farming		  =>! 15 Other CROPS
! (2)  ! LOW  - Short Grass			  =>! 12/13/14 (cold/cool/warm) C3 GRASS. Default: cool (13)
! (3)  ! HIGH - Evergreen Needleleaf Trees	  =>! 1/3  Needleleaf evergr. temperate /evergr boreal tree. Default: temperate (1)
! (4)  ! HIGH - Deciduous Needleleaf Trees	  =>! 2  Needleleaf deciduous boreal tree. NOTE that we follow K. Sindelarova in order of PF types!
! (5)  ! HIGH - Deciduous Broadleaf Trees	  =>! 6/7/8 Broadleaf evergreen/deciduous trop/temperate/boreal tree. default: temperate (7)
! (6)  ! HIGH - Evergreen Broadleaf Trees	  =>! 4/5 broadleaf evergreene tropical/temperate tree. Default: tropical (4)
! (7)  pre-49r1 ! LOW  - Tall Grass			  =>! 12/13/14 cold/cool/warm GRASS. Default: cool (13)
! (7)  49r1     ! mixed crops			  =>! 15: Other crops
! (8)  !      - Desert				  =>! N/A
! (9)  ! LOW  - Tundra				  =>! 12 cold grass
! (10) ! LOW  - Irrigated Crops			  =>! 15 other crops
! (11) ! LOW  - Semidesert			  =>! 13 cool C3 grass
! (12) !      - Ice Caps and Glaciers		  =>! N/A
! (13) ! LOW  - Bogs and Marshes		  =>! 13 cool grass?
! (14) !      - Inland Water			  =>! N/A
! (15) !      - Ocean				  =>! N/A
! (16) ! LOW  - Evergreen Shrubs		  =>! 9 broadleaf evergreen temperate shroub
! (17) ! LOW  - Deciduous Shrubs		  =>! 10/11 broadleaf dediduous temperate/boreal shrub. Default: temparate (10)
! (18) pre-49r1 ! HIGH - Mixed Forest/woodland	  =>! 7 broadleaf deciduous temperate tree
! (18) 49r1     ! HIGH - Broad Savana		  =>! 6/7/8 Broadleaf evergreen/deciduous trop/temperate/boreal tree. default: temperate (7)
! (19) pre-49r1 ! HIGH - Interrupted Forest	  =>! 7 broadleaf deciduous temperate tree
! (19) 49r1     ! N/A 	   			  =>! 0 
! (20) ! LOW  - Water and Land Mixtures		  =>! N/A

! Corresponding base conversion table from ECLAND to MEGAN PFT:
!                     ECLAND index:         1  2  3 4 5 6 7  8 9  10 11 12 13 14 15 16 17 18 19 20
INTEGER(KIND=JPIM) :: ECLAND_TO_MEGAN(20)=(/15,13,1,2,7,4,15,0,12,15,13,0, 13, 0, 0,9, 10, 7, 0, 0 /)

!Indices associated to Deciduous PFTs in MEGAN classification
INTEGER(KIND=JPIM) :: MEGAN_DECIDUOUS_PFT(6) = (/ 2, 6, 7, 8, 10, 11 /) 
! aura change to 3 according to Alex's paper
!INTEGER(KIND=JPIM) :: MEGAN_DECIDUOUS_PFT(6) = (/ 3, 6, 7, 8, 10, 11 /)


! Variables
REAL(KIND=JPRB)   :: ZT_DAILY   ! Daily average air temperature representative of "the simulation period"
REAL(KIND=JPRB)   :: ZT_HR      ! Hourly average air temperature
REAL(KIND=JPRB)   :: ZSZA       ! Solar Zenith Angle (radians)
REAL(KIND=JPRB)   :: ZSINA      ! SIN(ZSZA)
REAL(KIND=JPRB)   :: ZT_OPT, ZE_OPT, ZX
REAL(KIND=JPRB)   :: ZP_DAILY, ZP_TOA ! daily avg. above canopy PPFD ;  Top-of-atmosphere PPFD (umol/m2/sec)
REAL(KIND=JPRB)   :: ZPPFD      ! Above canopy PPFD (photosynthetic photon flux density, umol/m2/sec)
REAL(KIND=JPRB)   :: ZPHI       ! above canopy PPFD transmission 
REAL(KIND=JPRB)   :: ZF_NEW, ZF_GRO, ZF_MAT, ZF_OLD ! fraction (?) of new / growing / mature and old leafs in deciduous PFT
REAL(KIND=JPRB)   :: ZT, ZT_I, ZT_M ! Time parameters associated to leaf ageing
REAL(KIND=JPRB)   :: ZLAI_C, ZLAI_P ! Current and previous time step LAI
REAL(KIND=JPRB)   :: ZCA        ! Ambient concentration of CO2 (ppmv)

! Various constants
REAL(KIND=JPRB), PARAMETER :: ZC_T1=80. ! Aura eq 14 Guenther 2006
REAL(KIND=JPRB), PARAMETER :: ZC_T2=200. ! eq 14 Guenther 2006
REAL(KIND=JPRB), PARAMETER :: ZA_NEW=0.05 ! eq 16 Guenther 2006
REAL(KIND=JPRB), PARAMETER :: ZA_GRO=0.6 ! eq 16 Guenther 2006
REAL(KIND=JPRB), PARAMETER :: ZA_MAT=1.125 ! eq 16 Guenther 2006
REAL(KIND=JPRB), PARAMETER :: ZA_OLD=1.0 ! eq 16 Guenther 2006

! 
REAL(KIND=JPRB), PARAMETER :: ZINH_MAX = 1.344
REAL(KIND=JPRB), PARAMETER :: ZH = 1.4614
REAL(KIND=JPRB), PARAMETER :: ZCSTAR = 585
REAL(KIND=JPRB), PARAMETER :: ZMMR_TO_VMR = 28.96 / 44.0 * 1E6_JPRB   ! Conversion of CO2 from kg/kg to ppmv
REAL(KIND=JPRB), PARAMETER :: ZSRFD_FUDGE_FAC=1./2.2_JPRB ! Division of IFS SSRD by factor 2.2, proposed by Katerina Sindelarova, May 2023


  !     -------------------------------------------------------------------------
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK('BVOC_EMIS_MOD:BVOC_EMIS',0,ZHOOK_HANDLE)

ASSOCIATE( EMIS_FAC=>YDBVOC%EMIS_FAC, NEMIS_BVOC=>YDBVOC%NEMIS_BVOC, &
         & LDF => YDBVOC%LDF, IC5H8 => YDBVOC%IC5H8, RW_TO_MOL_BVOC=>YDBVOC%RW_TO_MOL_BVOC, &
	 & NBVOC_DELTA_DAY_LAI=>YDBVOC%NBVOC_DELTA_DAY_LAI)

!*       1.     INITIALIZE Local Variables
!               ---------- ----------

DO JL=KIDIA,KFDIA

  ! Initialize activity activity factors to unity (no inhibition)
  ZGAMMA_CE (JL,1:NEMIS_BVOC)=1.0_JPRB
  ZGAMMA_AGE(JL)             =1.0_JPRB
  ZGAMMA_CO2(JL,1:NEMIS_BVOC)=1.0_JPRB

  ! Fill latitude with input latitude converted to degree
  ZLAT(JL)=PLAT(JL)*57.29_JPRB ! 180/3.1415
ENDDO

!*       2.     Identify MEGAN PFT type for different ECLand vegetation types.
!               ---------- ----------
! Remember:
! KVTYPE is filled with ZERO for tile KVEG=3 (Wet Skin) - and currently this routine is not even called for KVEG=3
! KVTYPE is filled with LOW  vegetation type for tile KVEG=4 (DRY SNOW-FREE LOW-VEG)
! KVTYPE is filled with HIGH vegetation type for tile KVEG=6 (DRY SNOW-FREE HIGH-VEG)
! KVTYPE is filled with HIGH vegetation type for tile KVEG=7 (SNOW UNDER HIGH-VEG)

KVTYPE_MEGAN(KIDIA:KFDIA)=0_JPIM
DO JL=KIDIA,KFDIA

  IF (KVTYPE(JL) > 0_JPIM) THEN
    !
    ! Select default class
    !
    KVTYPE_MEGAN(JL)=ECLAND_TO_MEGAN(KVTYPE(JL))
    !
    ! Handle exceptions:
    !
    IF (KVTYPE(JL) == 2 .AND. ABS(ZLAT(JL))<30._JPRB) THEN
      ! C4 grass (14)
      KVTYPE_MEGAN(JL) = 14
    ELSEIF (KVTYPE(JL) == 2 .AND. ZLAT(JL)>60._JPRB) THEN
      ! C3 arctic grass (12)
      KVTYPE_MEGAN(JL) = 12

    ELSEIF (KVTYPE(JL) == 3 .AND. ZLAT(JL)>50._JPRB) THEN
      ! Evergreen needle leaf, boreal (3)
      KVTYPE_MEGAN(JL) = 3

    ELSEIF (KVTYPE(JL) == 5 .AND. ABS(ZLAT(JL))<30._JPRB) THEN
      ! Broad leaf Deciduous tropical (6)
      KVTYPE_MEGAN(JL) = 6
    ELSEIF (KVTYPE(JL) == 5 .AND. ZLAT(JL)>50._JPRB) THEN
      ! Broad leaf Deciduous boreal (8)
      KVTYPE_MEGAN(JL) = 8

    ELSEIF (KVTYPE(JL) == 6 .AND. ABS(ZLAT(JL))>30._JPRB) THEN
      ! temperate evergreen broadleaf trees (5)
      KVTYPE_MEGAN(JL) = 5

    ! Only relevant for pre-49r1 experiments!
    ! ELSEIF (KVTYPE(JL) == 7 .AND. ABS(ZLAT(JL))<30._JPRB) THEN
    !   ! C4 grass (14)
    !   KVTYPE_MEGAN(JL) = 14
    ! ELSEIF (KVTYPE(JL) == 7 .AND. ZLAT(JL)>60.) THEN
    !   ! C3 arctic grass (12)
    !  KVTYPE_MEGAN(JL) = 12

    ELSEIF (KVTYPE(JL) == 17 .AND. ZLAT(JL)>50._JPRB) THEN
      ! boreal shrub (11)
      KVTYPE_MEGAN(JL) = 11

    ELSEIF (KVTYPE(JL) == 18 .AND. ABS(ZLAT(JL))<30._JPRB) THEN
      ! Broad leaf Deciduous tropical (6)
      KVTYPE_MEGAN(JL) = 6
    ELSEIF (KVTYPE(JL) == 18 .AND. ZLAT(JL)>50._JPRB) THEN
      ! Broad leaf Deciduous boreal (8)
      KVTYPE_MEGAN(JL) = 8
    ENDIF

  ENDIF ! KVTYPE(JL) > 0

ENDDO

!*       3.     Specify BVOC emissions under standard conditions
!               ---------- ----------


DO JSP=1,NEMIS_BVOC
  DO JL=KIDIA,KFDIA
      ZBVOCFLUX(JL,JSP)=EMIS_FAC(JSP,KVTYPE_MEGAN(JL))
  ENDDO
ENDDO

!*       4.1     Evaluate local modification of activity factors: Canopy environment
!               ---------- ----------
DO JL=KIDIA,KFDIA

  ! Activity factor due to temperature:
  ZT_DAILY=MAX(240._JPRB,PTSOIL(JL))
  ZT_HR=PTM1(JL)
  ZT_OPT=313._JPRB+(0.6*(ZT_DAILY-297._JPRB)) ! eq 8 20006
  ZX=(1._JPRB/ZT_OPT - 1._JPRB/ZT_HR)/0.00831_JPRB ! eq 14
  ZE_OPT=1.75_JPRB*EXP(0.08_JPRB*(ZT_DAILY-297._JPRB))   !  eq 14
  ZGAMMA_T(JL)= ( ZE_OPT*ZC_T2*EXP(ZC_T1*ZX) ) / ( ZC_T2 - ZC_T1*(1-EXP(ZC_T2*ZX))  ) ! eq 14

  ! Activity factor due to LAI
  ZGAMMA_LAI(JL)=0.49_JPRB * PLAI(JL) / SQRT(1._JPRB + 0.2*PLAI(JL)*PLAI(JL))  ! eq 15

  ! Light activity factor..
  ZSZA = ACOS(PMU0(JL))

  ZSINA=SIN(ZSZA)
  ! Check Definition of SZA?!
  ! IF (ZSINA > 0_JPRB) THEN
  IF (PMU0(JL) > 0_JPRB) THEN
    ZP_DAILY=PAVGPAR(JL)*RW_TO_MOL_BVOC
    ! ZP_DAILY=100._JPRB*RW_TO_MOL_BVOC ! Possible default value for testing,
                                        ! omitting seasonal and latitudinal variation.
    !ZP_TOA=3000._JPRB ! WARNING!! THIS REQUIRES REVISION !!! currently annual avg constant is used. 
    ZP_TOA=PPPFD_TOA  !      photosynthetic flux at top of atmosphere, umol/m2/sec
                      !      at least seasonal variation should be introduced 
                      !      following eqn. 11 of Sindelarova (2017)
    ZPPFD=PSRFD(JL)*RW_TO_MOL_BVOC*ZSRFD_FUDGE_FAC ! Application of fudge factor - check if this is still needed.
    ZPHI=ZPPFD/(PMU0(JL) * ZP_TOA)
    ZGAMMA_P(JL)=MAX(0._JPRB,PMU0(JL)*(2.46*ZPHI*(1+0.0005*(ZP_DAILY-400._JPRB))-0.9*ZPHI*ZPHI )) ! eq 11b
  ELSE 
    ZGAMMA_P(JL)=0._JPRB
  ENDIF
ENDDO

DO JSP=1,NEMIS_BVOC
  DO JL=KIDIA,KFDIA
      ZGAMMA_CE(JL,JSP)=ZGAMMA_LAI(JL)*ZGAMMA_T(JL)*((1._JPRB-LDF(JSP)+ZGAMMA_P(JL)*LDF(JSP))) ! eq 10
     !  ((1._JPRB-LDF(JSP)+ZGAMMA_P(JL)*LDF(JSP))  is the overall adjustement factor from corrigendum to Alex 2006
  ENDDO
ENDDO

!*       4.2     Evaluate local modification of activity factors: leaf age activity factor
!  

DO JL=KIDIA,KFDIA
  ! For deciduous PFTs the gamma_age should be devided into four parts
  IF ( ANY(MEGAN_DECIDUOUS_PFT == KVTYPE_MEGAN(JL) ) ) THEN
    ! Current LAI 
    ZLAI_C = PLAI(JL)
    !  LAI of previous time
    ZLAI_P = PLAIP(JL) ! Updated using Previous time step (default 10 days earlier, see script inter_fp in ifs-scripts)
    IF (ZLAI_C == ZLAI_P ) THEN  ! condition taken from Guenther 2006
      ZF_NEW=0.0_JPRB
      ZF_GRO=0.1_JPRB
      ZF_MAT=0.8_JPRB
      ZF_OLD=0.1_JPRB
    ELSEIF (ZLAI_C < ZLAI_P) THEN
      ZF_NEW=0.0_JPRB
      ZF_GRO=0.0_JPRB
      ZF_OLD=(ZLAI_P-ZLAI_C)/ZLAI_P
      ZF_MAT=1.0_JPRB-ZF_OLD
    ELSE ! ZLAI_C > ZLAI_P
      ! daily mean air temperature: take soil temp as proxy:
      ZT_DAILY = PTSOIL(JL)
      ! time parameters t, t_i, t_m [days]
      IF (ZT_DAILY <= 303) THEN 
        ZT_I = 5._JPRB + 0.7 * (300 - ZT_DAILY) ! eq 18a
      ELSE
        ZT_I = 2.9_JPRB   ! eq 18b
      ENDIF
      ZT_M=2.3_JPRB * ZT_I ! eq 19
      ZT = NBVOC_DELTA_DAY_LAI ! Standard a 10-day update of LAI is assumed..
      IF (ZT <= ZT_I) THEN
              ZF_NEW=1.0_JPRB-(ZLAI_P/ZLAI_C)  ! eq 17 a)-e) for below conditions
      ELSE
        ZF_NEW=ZT_I/ZT * ( 1.0_JPRB-(ZLAI_P/ZLAI_C) )
      ENDIF
      IF (ZT <= ZT_M) THEN
        ZF_MAT=ZLAI_P/ZLAI_C
      ELSE
        ZF_MAT=(ZLAI_P/ZLAI_C) + ( (ZT - ZT_M) / ZT  ) * ( 1._JPRB - (ZLAI_P/ZLAI_C) ) 
      ENDIF
      ZF_GRO=1.0_JPRB - ZF_NEW - ZF_MAT
      ZF_OLD=0.0_JPRB
    ENDIF
    ZGAMMA_AGE(JL)=ZF_NEW*ZA_NEW + ZF_GRO*ZA_GRO + ZF_MAT*ZA_MAT + ZF_OLD*ZA_OLD  ! eq 16
  ELSE
    ! For evergreen PFTs the gamma_age is unity 
    ZGAMMA_AGE(JL)=1.0_JPRB
  ENDIF
ENDDO

!*       4.3     Evaluate local modification of activity factors: CO2 inhibition 
!                Only to be applied to isoprene
!  

IF (IC5H8 > 0 ) THEN 
  DO JL=KIDIA,KFDIA
    ZCA = PCM1(JL) * ZMMR_TO_VMR
    ZGAMMA_CO2(JL,IC5H8)=ZINH_MAX - ( ZINH_MAX * (0.7 * ZCA)**ZH / ( ZCSTAR**ZH + (0.7*ZCA)**ZH  )  )  ! eq 14
  ENDDO
ENDIF

!*       5.     Compute net BVOC emissions, accounting for activity factors
!               ---------- ----------


  ! Net emission over canopy (kgBVOC m-2 s-1) 
  DO JL=KIDIA,KFDIA
     IF (KVTYPE_MEGAN(JL)>0_JPIM) THEN
	ZGAMMA_OVERALL(1:NEMIS_BVOC)=ZGAMMA_CE(JL,1:NEMIS_BVOC)*ZGAMMA_AGE(JL)*ZGAMMA_CO2(JL,1:NEMIS_BVOC)
        PBVOCFLUX(JL,1:NEMIS_BVOC)=ZGAMMA_OVERALL(1:NEMIS_BVOC)*ZBVOCFLUX(JL,1:NEMIS_BVOC)
     ELSE
        PBVOCFLUX(JL,1:NEMIS_BVOC)=0._JPRB
     ENDIF
  ENDDO

END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('BVOC_EMIS_MOD:BVOC_EMIS',1,ZHOOK_HANDLE)
END SUBROUTINE BVOC_EMIS
END MODULE BVOC_EMIS_MOD
