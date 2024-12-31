MODULE SRFIL_MOD
CONTAINS
SUBROUTINE SRFIL(KIDIA  , KFDIA  , KLON , KLEVS  , KLEVI  ,LDLAND, PCIL,PSDOR,&
 & PTMST  ,PTIAM1M ,PFRTI , PAHFSTI, PEVAPTI,PGSNICE,&
 & PSLRFL ,PSSRFLTI, PTSOIL, LDICE  , LDNH   ,&
 & YDCST  ,YDSOIL  ,&
 & PTIA   ,PMELT, PGICE, PDHTIS)  
 
USE PARKIND1 , ONLY : JPIM, JPRB
USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK
USE YOS_CST  , ONLY : TCST
USE YOS_SOIL , ONLY : TSOIL

USE SRFWDIF_MOD

!**** *SRFIL* - Computes temperature changes in land ice, adapted from SRFI

!     PURPOSE.
!     --------
!**   Computes temperature evolution of land ice  

!**   INTERFACE.
!     ----------
!          *SRFIL* IS CALLED FROM *SURF*.

!     PARAMETER   DESCRIPTION                                    UNITS
!     ---------   -----------                                    -----
!     INPUT PARAMETERS (INTEGER):
!    *KIDIA*      START POINT
!    *KFDIA*      END POINT
!    *KLON*       NUMBER OF GRID POINTS PER PACKET
!    *KLEVS*      NUMBER OF SOIL LAYERS
!    *KTILES*     NUMBER OF SURFACE TILES
!    *KLEVI*      Number of ice layers 

!     INPUT PARAMETERS (REAL):
!    *PTMST*      TIME STEP                                      S

!     INPUT PARAMETERS (LOGICAL):
!    *LDLAND*     LAND INDICATOR (True for land point)
!    *LDICE*      ICE MASK (TRUE for land ice)
!    *LDNH*       TRUE FOR NORTHERN HEMISPHERE

!     INPUT PARAMETERS AT T-1 OR CONSTANT IN TIME (REAL):
!    *PTIAM1M*    ICE TEMPERATURE                            K
!    *PSLRFL*     NET LONGWAVE  RADIATION AT THE SURFACE        W/M**2
!    *PFRTI*      TILE FRACTIONS                              (0-1)
!            1 : WATER                  5 : SNOW ON LOW-VEG+BARE-SOIL
!            2 : ICE                    6 : DRY SNOW-FREE HIGH-VEG
!            3 : WET SKIN               7 : SNOW UNDER HIGH-VEG
!            4 : DRY SNOW-FREE LOW-VEG  8 : BARE SOIL
!            9 : LAKE                  10 : URBAN
!    *PAHFSTI*    TILE SURFACE SENSIBLE HEAT FLUX                 W/M2
!    *PEVAPTI*    TILE SURFACE MOISTURE FLUX                     KG/M2/S
!    *PSSRFLTI*   TILE NET SHORTWAVE RADIATION FLUX AT SURFACE    W/M2
!    *PGSN* .     SNOW basal heat flux between snow and ice       W/M2
!     UPDATED PARAMETERS AT T+1 (UNFILTERED,REAL):
!    *PTIA*       ICE TEMPERATURE                               K
!    *PMELT*      MELTWATER FLUX FROM LAND ICE                    kg/m2/s
!    *PGICE*      BASAL HEAT FLUX FROM LAND ICE  to soil          W/m2

!     OUTPUT PARAMETERS (DIAGNOSTIC):
!    *PDHTIS*     Diagnostic array for ice T (see module yomcdh)

!     METHOD.
!     -------
!          Parameters are set and the tridiagonal solver is called. 
!          Solved only over land-ice points (LDICE=LDLICE=.TRUE.)

!     EXTERNALS.
!     ----------
!     *SRFWDIF*

!     REFERENCE.
!     ----------
!          See documentation.
!     G. Arduini                2024        Adapted from SRFI for land-ice

!     ------------------------------------------------------------------

IMPLICIT NONE

! Declaration of arguments

INTEGER(KIND=JPIM), INTENT(IN)   :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN)   :: KLON
INTEGER(KIND=JPIM), INTENT(IN)   :: KLEVS
INTEGER(KIND=JPIM), INTENT(IN)   :: KLEVI
LOGICAL, INTENT(IN)   :: LDLAND(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PCIL(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSDOR(:)

REAL(KIND=JPRB),    INTENT(IN)   :: PTMST
REAL(KIND=JPRB),    INTENT(IN)   :: PTIAM1M(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PFRTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PAHFSTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PEVAPTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSLRFL(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PGSNICE(:)
REAL(KIND=JPRB),    INTENT(IN)   :: PSSRFLTI(:,:)
REAL(KIND=JPRB),    INTENT(IN)   :: PTSOIL(:)
LOGICAL,            INTENT(IN)   :: LDICE(:)
LOGICAL,            INTENT(IN)   :: LDNH(:)
TYPE(TCST),         INTENT(IN)   :: YDCST
TYPE(TSOIL),        INTENT(IN)   :: YDSOIL
REAL(KIND=JPRB),    INTENT(OUT)  :: PTIA(:,:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PMELT(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PGICE(:)
REAL(KIND=JPRB),    INTENT(OUT)  :: PDHTIS(:,:,:)

!      LOCAL VARIABLES

REAL(KIND=JPRB) :: ZSURFL(KLON)
REAL(KIND=JPRB) :: ZRHS(KLON,KLEVS), ZCDZ(KLON,KLEVS),&
 & ZLST(KLON,KLEVS),&
 & ZTIA(KLON,KLEVS)  
REAL(KIND=JPRB) :: ZDAI(KLON,KLEVS)
REAL(KIND=JPRB) :: ZDARLICE
REAL(KIND=JPRB) :: ZCSN_I

REAL(KIND=JPRB) :: ZEPSILON
LOGICAL :: LLDOICE(KLON)

INTEGER(KIND=JPIM) :: JK, JL

REAL(KIND=JPRB) :: ZCONS1, ZCONS2, ZSLRFL, ZSSRFL, ZTHFL, ZTMST
REAL(KIND=JPRB) :: ZTMP0
REAL(KIND=JPRB) :: zc1, zc2, zc3, ZTAU_ICE
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

!*         1. SET UP SOME CONSTANTS.
!             --- -- ---- ----------

!*    PHYSICAL CONSTANTS.
!     -------- ----------


IF (LHOOK) CALL DR_HOOK('SRFIL_MOD:SRFIL',0,ZHOOK_HANDLE)
ASSOCIATE(RLSTT=>YDCST%RLSTT, RTT=>YDCST%RTT, RLMLT=>YDCST%RLMLT, &
 & RCONDSICE=>YDSOIL%RCONDSICE, RDAI=>YDSOIL%RDAI, RDANSICE=>YDSOIL%RDANSICE, &
 & RDARSICE=>YDSOIL%RDARSICE, RRCSICE=>YDSOIL%RRCSICE, RSIMP=>YDSOIL%RSIMP, &
 & RTFREEZSICE=>YDSOIL%RTFREEZSICE, RTMELTSICE=>YDSOIL%RTMELTSICE, &
 & RHOCI=>YDSOIL%RHOCI,RHOICE=>YDSOIL%RHOICE)


DO JK=1,KLEVS-1
  DO JL=KIDIA,KFDIA
    ZDAI(JL,JK)=RDAI(JK)
  ENDDO
ENDDO

ZDARLICE=10.86_JPRB ! equivalent to 10000/920, previous tests: 30._JPRB
DO JL=KIDIA,KFDIA
  ZDAI(JL,KLEVS)=ZDARLICE-(RDAI(1)+RDAI(2)+RDAI(3))
ENDDO

!*    COMPUTATIONAL CONSTANTS.
!     ------------- ----------

ZTMST=1.0_JPRB/PTMST
ZCONS1=PTMST*RSIMP*2.0_JPRB
ZCONS2=1.0_JPRB-1.0_JPRB/RSIMP
ZEPSILON=EPSILON(ZEPSILON)

!*         2. Compute net heat flux at the surface.
!             -------------------------------------

DO JL=KIDIA,KFDIA
  LLDOICE(JL)=LDICE(JL) !.and. LDLAND(JL)

  IF (LLDOICE(JL)) THEN
    ! This need to be weighted properly:
      ZSSRFL=PFRTI(JL,2)*PSSRFLTI(JL,2)
      ZSLRFL=PFRTI(JL,2)*PSLRFL(JL)
      ZTHFL=PFRTI(JL,2)*PAHFSTI(JL,2)+RLSTT*PFRTI(JL,2)*PEVAPTI(JL,2)
    ! PGSNICE(JL) only applies to the fraction of snow over the ice fraction.
      ZSURFL(JL)=(PGSNICE(JL)+ZSSRFL+ZSLRFL+ZTHFL)/MAX(ZEPSILON,PCIL(JL))
  ELSE
    ZSURFL(JL)=0.0_JPRB
  ENDIF
ENDDO

!*         3. Set arrays
!             ----------

PGICE(KIDIA:KFDIA)=0.0_JPRB
PMELT(KIDIA:KFDIA)=0.0_JPRB
!     Layer 1

JK=1
DO JL=KIDIA,KFDIA
  IF (LLDOICE(JL)) THEN
    ZLST(JL,JK)=ZCONS1*RCONDSICE/(ZDAI(JL,JK)+ZDAI(JL,JK+1))
    ZCDZ(JL,JK)=RRCSICE*ZDAI(JL,JK)
    ZRHS(JL,JK)=PTMST*ZSURFL(JL)/ZCDZ(JL,JK)
  ENDIF
ENDDO

!     Layers 2 to KLEVS-1

DO JK=2,KLEVS-1
  DO JL=KIDIA,KFDIA
    IF (LLDOICE(JL)) THEN
      ZLST(JL,JK)=ZCONS1*RCONDSICE/(ZDAI(JL,JK)+ZDAI(JL,JK+1))
      ZCDZ(JL,JK)=RRCSICE*ZDAI(JL,JK)
      ZRHS(JL,JK)=0.0_JPRB
    ENDIF
  ENDDO
ENDDO

!     Layers KLEVS

JK=KLEVS
DO JL=KIDIA,KFDIA
  IF (LLDOICE(JL)) THEN
    ! We use soil temperature as bottom boundary condition
      ! This we can do better and compute average conductivity with
      ! half soil layer like in srfsn_
      ZLST(JL,JK)=ZCONS1*RCONDSICE/(2.*ZDAI(JL,JK))
      ZCDZ(JL,JK)=RRCSICE*ZDAI(JL,JK)
      ZRHS(JL,JK)=(PTSOIL(JL)/RSIMP)*ZLST(JL,JK)/ZCDZ(JL,JK)
  ENDIF
ENDDO

!*         4. Call tridiagonal solver
!             -----------------------

CALL SRFWDIF(KIDIA,KFDIA,KLON,KLEVS,PTIAM1M,ZLST,ZRHS,ZCDZ,YDSOIL,ZTIA,LLDOICE)


!* 4.1 Ice meltwater flux for land-ice. Still no mass balance:
DO JL=KIDIA,KFDIA
  PMELT(JL) = 0.0_JPRB
  IF (LLDOICE(JL)) THEN
    ZTMP0 = (RHOCI*ZDAI(JL,1)*ZTMST)*(ZTIA(JL,1)-RTT)
    PMELT(JL) = MAX(0._JPRB, MIN( ZTMP0 , RLMLT*ZTMST*(ZDAI(JL,1)*RHOICE) ) )
    ! Temperature update
    ZTIA(JL,1) = MIN( RTT, ZTIA(JL,1) - PMELT(JL)/(RHOCI*ZDAI(JL,1)*ZTMST) )
    ! Water flux
    PMELT(JL)=PMELT(JL)/RLMLT
  ENDIF

!***
!*zc1=0.30_JPRB
!*zc2=25.0_JPRB
!*zc3=140.0_JPRB ! Not used
!*ZTAU_ICE=zc1+zc2*exp(-2.0*PSDOR(JL)/50.0_JPRB)
!*PMELT(JL)=MIN(PMELT(JL), PMELT(JL)/(86400.0*ZTAU_ICE)) ! in seconds
!*** 
ENDDO



!*          5. DDH diagnostics
!              ---------------

IF (SIZE(PDHTIS) > 0) THEN
  DO JL=KIDIA,KFDIA
    IF (LLDOICE(JL)) THEN
! Sensible heat flux
      PDHTIS(JL,1,9)=PFRTI(JL,2)*PAHFSTI(JL,2)
! Latent heat flux
      PDHTIS(JL,1,10)=RLSTT*PFRTI(JL,2)*PEVAPTI(JL,2)
    ELSE
      PDHTIS(JL,1,9)=0.0_JPRB
      PDHTIS(JL,1,10)=0.0_JPRB
      PDHTIS(JL,1,13)=0.0_JPRB
    ENDIF
  ENDDO
  
  DO JK=2,KLEVI
    DO JL=KIDIA,KFDIA
      PDHTIS(JL,JK,9)=0.0_JPRB
      PDHTIS(JL,JK,10)=0.0_JPRB
    ENDDO
  ENDDO
! Flux ice-soil
  DO JK=1,KLEVI
    DO JL=KIDIA,KFDIA
      PDHTIS(JL,JK,13)=0.0_JPRB
    ENDDO
  ENDDO

  DO JK=1,KLEVI
    DO JL=KIDIA,KFDIA
      IF (LLDOICE(JL)) THEN
! Heat capacity per unit surface
        PDHTIS(JL,JK,1)=RRCSICE*ZDAI(JL,JK)
! Soil temperature
        PDHTIS(JL,JK,2)=PTIAM1M(JL,JK)
! Layer energy per unit surface
        PDHTIS(JL,JK,3)=RRCSICE*ZDAI(JL,JK)*PTIAM1M(JL,JK)
! Layer depth
        PDHTIS(JL,JK,4)=ZDAI(JL,JK)
      ELSE
        PDHTIS(JL,JK,1:4)=0.0_JPRB
      ENDIF
    ENDDO
  ENDDO
! Ice water phase changes
  DO JK=1,KLEVI
    DO JL=KIDIA,KFDIA
      PDHTIS(JL,JK,12)=0.0_JPRB
    ENDDO
  ENDDO

  DO JK=1,KLEVI-1
    DO JL=KIDIA,KFDIA
      IF (LLDOICE(JL)) THEN
! Ground heat flux
        PDHTIS(JL,JK,11)=RSIMP*(ZTIA(JL,JK)-ZTIA(JL,JK+1))*ZLST(JL,JK)*ZTMST
      ELSE
        PDHTIS(JL,JK,11)=0.0_JPRB
      ENDIF
    ENDDO
  ENDDO
  DO JL=KIDIA,KFDIA
    IF (LLDOICE(JL)) THEN
      PDHTIS(JL,KLEVS,11)=RSIMP*(ZTIA(JL,KLEVS)-RTFREEZSICE)*ZLST(JL,JK)*ZTMST
    ELSE
      PDHTIS(JL,KLEVS,11)=0.0_JPRB
    ENDIF
  ENDDO
ENDIF

!*         6. New temperatures
!             ----------------

DO JK=1,KLEVI
  DO JL=KIDIA,KFDIA
    IF (LLDOICE(JL)) THEN
      PTIA(JL,JK)=PTIAM1M(JL,JK)*ZCONS2+ZTIA(JL,JK)
      PTIA(JL,JK)=MIN(PTIA(JL,JK),RTT)
    ELSEIF (LDLAND(JL)) THEN
      PTIA(JL,JK)=RTT
    ELSE
      PTIA(JL,JK)=RTFREEZSICE ! keep the same value as srfi for safety.
                              ! it should be anyhow overwrite afterwards.
    ENDIF
    ! 6.1 Compute amount of ice temperature flux to the soil underneath.
    !     this is scaled by gridbox fraction as for the snowpack and passed to srft.
    IF (JK==KLEVI)THEN
      PGICE(JL)=PCIL(JL)*RCONDSICE*(PTIA(JL,KLEVS)-PTSOIL(JL))
    ENDIF
  ENDDO
ENDDO
END ASSOCIATE
IF (LHOOK) CALL DR_HOOK('SRFIL_MOD:SRFIL',1,ZHOOK_HANDLE)

END SUBROUTINE SRFIL
END MODULE SRFIL_MOD
