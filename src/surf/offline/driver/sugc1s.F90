SUBROUTINE SUGC1S(NCID,LOPLEFT)
USE PARKIND1  ,ONLY : JPIM ,JPRB, JPRD
USE YOMHOOK   ,ONLY : LHOOK     ,DR_HOOK, JPHOOK
USE YOMDPHY  , ONLY : NCSS,NLON ,NLAT ,NLALO,NPOI,DZLAT,DZLON, NVHILO,NTILES,&
                      NCLIMDAT,NCSNEC,NPOIG,NPOIP,NPOIALL, NPOIPALL,NPOIOFF
USE YOMCST   , ONLY : RPI      ,ROMEGA
USE YOMGC1S  , ONLY : RCORI    ,GEMU     ,GSQM2    ,GELAM    ,&
            &GELAT    ,GECLO    ,GESLO    ,LMASK
USE YOMLOG1S , ONLY : NDIMCDF &
           &,LWREFL   ,LWRWAT   ,LWRSUB   ,LWRSUS   ,LWREVA &
           &,LWRCLD   ,LWRGG    ,LWRCLM   &
           &,CFSURF,CFOUT,LWRLKE &
           &,LWROCP   ,LWROCD   ,LWROCR &
           &,LWRCO2   ,LWRVEG   ,LWRVTY &
           &,LWREXT   ,LWRBIO   ,LWRTIL,LWRD2M,LWRGGD
USE YOESOIL1S, ONLY : RDAW
USE YOMLUN1S , ONLY : NPOSGG ,NPOSEFL   ,NPOSRES, NPOSWAT &
                     &,NPOSSUS,NPOSSUB   ,NPOSEVA   ,NPOSCLD &
                     &,NPOSCLM, NULNAM, NULOUT,NPOSLKE &
                     &,NPOSRESO ,NPOSOCP  ,NPOSOCD &
                     &,NPOSCO2  ,NPOSVEG  ,NPOSEXT &
                     &,NPOSBIO  ,NPOSTIL  ,NPOSVTY,NPOSD2M,NPOSGGD
USE YOEPHY   , ONLY : LESNML
USE NETCDF
USE NETCDF_UTILS, ONLY : NCERROR
USE MPL_MODULE

#ifdef DOC
! (C) Copyright 1995- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *SUGC1S*  - Routine to initialize geometry parameters

!     Purpose.
!     --------
!           Initialize geometry parameters of the one-column surface model

!**   Interface.
!     ----------

!     *CALL* SUGC1S(NCID)

!     Explicit arguments :
!     --------------------
!        NCID : NETCDF INPUT FILE UNIT
!         LOPLEFT: FLAG INDICATING WHETHER MORE POINTS NEED TO BE DONE

!        Implicit arguments :
!        --------------------

!     Method.
!     -------


!     Externals.
!     ----------

!     Reference.
!     ----------
!        ECMWF Research Department documentation 
!        of the one-column surface model

!     Author.
!     -------
!        Jean-Francois Mahfouf and Pedro Viterbo  *ECMWF*

!     Modifications.
!     --------------
!        Original : 95-03-22
!        BART VD HURK (KNMI) READING COORDINATES FROM NETCDF
!        E. DUTRA - NEW OUTPUT FILE - LAKES : 04/07/2008
!        Y. Takaya  Ocean mixed layer output file : 07/10/2008
!        E. Dutra: June 2014: netcdf4 inferface 
!     ------------------------------------------------------------------
#endif
IMPLICIT NONE

INTEGER NCID
LOGICAL LOPLEFT   ! FLAG FOR RUNNING MORE GRID POINTS

! Local variables
REAL(KIND=JPRB) :: ZLAT,ZLON,ZZZ,ZDLO,ZDLA
INTEGER(KIND=JPIM) :: ICT,JLA,JLO,JK,J,JVT,JTILE
INTEGER(KIND=JPRB), PARAMETER :: JPNCDF=21

!* Netcdf interface
INTEGER(KIND=JPIM) :: NPOS,NVARID,IERR,NX,NY
INTEGER(KIND=JPIM) :: ISTART1(1),ICOUNT1(1)
INTEGER(KIND=JPIM) :: IPOS(JPNCDF)
LOGICAL LPOS(JPNCDF)
INTEGER(KIND=JPIM),ALLOCATABLE :: IVT(:),ITILE(:),ILEV(:),IMONTH(:),ILEVSN(:)
INTEGER(KIND=JPIM) :: MYPROC, NPROC
INTEGER(KIND=JPIM) :: NPOIL, IREM, IOFF
INTEGER(KIND=JPIM) :: NPOILALL,IREMALL

REAL(KIND=JPRB),ALLOCATABLE :: GELAMG(:)
REAL(KIND=JPRB),ALLOCATABLE :: GELATG(:)
REAL(KIND=JPRB),ALLOCATABLE :: DZLATG(:)
REAL(KIND=JPRB),ALLOCATABLE :: DZLONG(:)
REAL(KIND=JPRB),ALLOCATABLE :: RCORIG(:)
REAL(KIND=JPRB),ALLOCATABLE :: GEMUG(:)
REAL(KIND=JPRB),ALLOCATABLE :: GSQM2G(:)
REAL(KIND=JPRB),ALLOCATABLE :: GECLOG(:)
REAL(KIND=JPRB),ALLOCATABLE :: GESLOG(:)
LOGICAL        ,ALLOCATABLE :: LMASKG(:)

INTEGER(KIND=JPIM)           :: ISTP, IENP

REAL(KIND=JPHOOK)    :: ZHOOK_HANDLE


#include "namgc1s.h"
#include "rdcoor.intfb.h"

IF (LHOOK) CALL DR_HOOK('SUGC1S',0,ZHOOK_HANDLE)

NPROC = MPL_NPROC()
MYPROC = MPL_MYRANK()

!     ------------------------------------------------------------------

IF (CFSURF == 'netcdf')THEN

  CALL RDCOOR(NCID,LOPLEFT)

ELSE

!*         1.  DEFAULTS VALUES.
!              ----------------

!     RLAT : Latitude
!     RLON : Longitude

  NPOI = NLALO
  ALLOCATE (GELAM(NPOI))
  ALLOCATE (GELAT(NPOI))
  ALLOCATE (LMASK(NPOI))

  ALLOCATE (DZLAT(NPOI))
  ALLOCATE (DZLON(NPOI))

  RLAT = 45.00
  RLON = 00.00
  RMASK= 1.

!      ----------------------------------------------------------------

!*       2.    READ NAMELIST.
!              --------------

  REWIND(NULNAM)
  READ(NULNAM,NAMGC1S)

! no more points left to read
  LOPLEFT=.FALSE.

! fudge for mixt formats ASCII/NETCDF
  DZLAT=RLAT
  DZLON=RLON



!*        3.  CONVERSION FROM DEGREES TO RADIANS.
!             -----------------------------------

  GELAT(1)=RLAT*RPI/180.
  GELAM(1)=RLON*RPI/180.

  IF (GELAM(1) < 0.) THEN
    GELAM(1)=GELAM(1)+2.*RPI
  ENDIF

!*        4.  MASK.
!             -----------------------------------

 
  LMASK(1)=(ABS(RMASK-1.) < 0.0001)

!      ----------------------------------------------------------------

!*       5.    PRINT PART OF YOMGC1S.
!              ----------------------

  WRITE(UNIT=NULOUT,FMT='('' GELAT='',E12.5,'' GELAM='',E12.5)')&
   &GELAT(1),GELAM(1)

ENDIF

!*       6.    WRITE COORDINATES TO NETCDF OUTPUT FILES.
!              ----------------------


IF (CFOUT == 'netcdf')THEN

  ALLOCATE (ILEV(NCSS))
  DO JK=1,NCSS
    ILEV(JK)=JK
  ENDDO
  ALLOCATE (ILEVSN(NCSNEC))
  DO JK=1,NCSNEC
    ILEVSN(JK)=JK
  ENDDO

  ALLOCATE (IVT(NVHILO))
  DO JVT=1,NVHILO
    IVT(JVT)=JVT
  ENDDO

  ALLOCATE (ITILE(NTILES))
  DO JTILE=1,NTILES
    ITILE(JTILE)=JTILE
  ENDDO  

  ALLOCATE (IMONTH(NCLIMDAT))
  DO JTILE=1,NCLIMDAT
    IMONTH(JTILE)=JTILE
  ENDDO  

  IPOS(1)=NPOSGG
  IPOS(2)=NPOSEFL
  IPOS(3)=NPOSWAT
  IPOS(4)=NPOSRES
  IPOS(5)=NPOSSUS
  IPOS(6)=NPOSSUB
  IPOS(7)=NPOSEVA
  IPOS(8)=NPOSCLD
  IPOS(9)=NPOSCLM
  IPOS(10)=NPOSLKE
  IPOS(11)=NPOSOCD
  IPOS(12)=NPOSOCP
  IPOS(13)=NPOSRESO
  IPOS(14)=NPOSCO2
  IPOS(15)=NPOSBIO
  IPOS(16)=NPOSVEG
  IPOS(17)=NPOSEXT
  IPOS(18)=NPOSTIL
  IPOS(19)=NPOSVTY
  IPOS(20)=NPOSD2M
  IPOS(21)=NPOSGGD


  LPOS(1)=LWRGG
  LPOS(2)=LWREFL
  LPOS(3)=LWRWAT
  LPOS(4)=.TRUE.
  LPOS(5)=LWRSUS
  LPOS(6)=LWRSUB
  LPOS(7)=LWREVA
  LPOS(8)=LWRCLD
  LPOS(9)=LWRCLM
  LPOS(10)=LWRLKE
  LPOS(11)=LWROCD
  LPOS(12)=LWROCP
  LPOS(13)=LWROCR
  LPOS(14)=LWRCO2 !CTESSEL
  LPOS(15)=LWRBIO !CTESSEL
  LPOS(16)=LWRVEG !CTESSEL
  LPOS(17)=LWREXT !CTESSEL
  LPOS(18)=LWRTIL !CTESSEL
  LPOS(19)=LWRVTY !CTESSEL
  LPOS(20)=LWRD2M 
  LPOS(21)=LWRGGD 
  
  IF(NDIMCDF == 2)THEN
    NX=NLON
    NY=NLAT
  ELSE
    NX=NLALO
    NY=NLALO
  ENDIF
  IF( MYPROC == 1 ) THEN
    DO J=1,JPNCDF
      IF(LPOS(J))THEN
        NPOS = IPOS(J)
        !* soil levels as interger 1...NCSS 
        IERR = NF90_INQ_VARID(NPOS,'nlevs',NVARID)
        IF ( IERR == NF90_NOERR ) THEN
          CALL NCERROR( NF90_PUT_VAR(NPOS, NVARID, ILEV(:)),'WRITING nlevs')
        ENDIF
      
        IF (LESNML) THEN
          !* snow levels as interger 1...NCSNEC
          IERR = NF90_INQ_VARID(NPOS,'nlevsn',NVARID)
          IF ( IERR == NF90_NOERR ) THEN
            CALL NCERROR( NF90_PUT_VAR(NPOS, NVARID, ILEVSN(:)),'WRITING nlevsn')
          ENDIF
        ENDIF

        !* vegetation type 
        IERR = NF90_INQ_VARID(NPOS,'vtype',NVARID)
        IF ( IERR == NF90_NOERR ) THEN
          CALL NCERROR( NF90_PUT_VAR(NPOS, NVARID, IVT(:)),'WRITING vtype')
        ENDIF

        !* tile
        IERR = NF90_INQ_VARID(NPOS,'tile',NVARID)
        IF ( IERR == NF90_NOERR ) THEN
          CALL NCERROR( NF90_PUT_VAR(NPOS, NVARID, ITILE(:)),'WRITING tile')
        ENDIF

        !* month
        IERR = NF90_INQ_VARID(NPOS,'month',NVARID)
        IF ( IERR == NF90_NOERR ) THEN
          CALL NCERROR( NF90_PUT_VAR(NPOS, NVARID, IMONTH(:)),'WRITING month')
        ENDIF

        !* lat
        CALL NCERROR( NF90_INQ_VARID(NPOS,'lat',NVARID) )
        CALL NCERROR( NF90_PUT_VAR(NPOS, NVARID, DZLAT(:)),'WRITING lat')

        !* lon
        CALL NCERROR( NF90_INQ_VARID(NPOS,'lon',NVARID) )
        CALL NCERROR( NF90_PUT_VAR(NPOS, NVARID, DZLON),'WRITING lon')

      ENDIF
    ENDDO
  ENDIF

  DEALLOCATE (ILEV)
  DEALLOCATE (ILEVSN)
  DEALLOCATE (ITILE)
  DEALLOCATE (IVT)
  DEALLOCATE (IMONTH)

ENDIF


!*        7.  COMPUTATION OF GEOMETRY PARAMETERS.
!             -----------------------------------

ALLOCATE (RCORI(NPOI))
ALLOCATE (GEMU(NPOI))
ALLOCATE (GSQM2(NPOI))
ALLOCATE (GECLO(NPOI))
ALLOCATE (GESLO(NPOI))

DO ICT = 1,NPOI
  GEMU(ICT)=SIN(GELAT(ICT))
  GSQM2(ICT)=COS(GELAT(ICT))
  GESLO(ICT)=SIN(GELAM(ICT))
  GECLO(ICT)=COS(GELAM(ICT))
  RCORI(ICT)=2.*ROMEGA*GEMU(ICT)
ENDDO


NPOIG=NPOI

IF(.NOT. ALLOCATED(NPOIP))   ALLOCATE(NPOIP(NPROC))
IF(.NOT. ALLOCATED(NPOIPALL)) ALLOCATE(NPOIPALL(NPROC))
IF(.NOT. ALLOCATED(NPOIOFF)) ALLOCATE(NPOIOFF(NPROC+1))

NPOIL=NPOIG/NPROC
NPOILALL=NLALO/NPROC ! total array

IF( NPOIL <= 0 ) THEN
  WRITE(NULOUT,'("SUGC1S: NPROC LARGER THAN NPOI, REDUCE NPROC AND RESUBMIT")')
  CALL ABOR1("SUGC1S: NPROC LARGER THAN NPOI, REDUCE NPROC AND RESUBMIT")
ENDIF

NPOIPALL(:)=NPOILALL

IREMALL=NLALO-(NPOILALL*NPROC)


DO J=1,IREMALL
  NPOIPALL(NPROC-J+1)=NPOIPALL(NPROC-J+1)+1
ENDDO

!NPOIOFF includes all grid-points before mask is applied
IOFF=0
DO J=1,NPROC
  NPOIOFF(J)=IOFF
  !**IOFF=IOFF+NPOIP(J)
  IOFF=IOFF+NPOIPALL(J)
ENDDO
!**NPOIOFF(NPROC+1)=NPOIG
NPOIOFF(NPROC+1)=NLALO

CALL MPL_BARRIER()

!* Recompute NPOIP based on NPOIOFF and Mask
DO J=1,NPROC
  ISTP = NPOIOFF(J)+1
  IENP = NPOIOFF(J+1)
  NPOIP(J)=COUNT(LMASK(ISTP:IENP))
ENDDO
CALL MPL_BARRIER()

IREM=NPOIG-SUM(NPOIP)
IF (IREM .NE. 0) THEN
DO J=1,IREM
  NPOIP(NPROC-J+1)=NPOIP(NPROC-J+1)+1
ENDDO
ENDIF

IF( MYPROC==1 )THEN
  WRITE(NULOUT,'("SUGC1S: NPOIGLOB,NPOIGLOBALL=",I10,I10)')NPOIG,NLALO
  DO J=1,NPROC
    WRITE(NULOUT,'("SUGC1S: J=",I6," NPOIOFF(1)=",I10," NPOIOFF(2)=",I10," NPOIP=",I10, " CMASK=",I10,I10," ISTP,IEND,IREM=",I10,I10,I10)')&
         &J,NPOIOFF(J),NPOIOFF(J+1),NPOIP(J),SIZE(LMASK),COUNT(LMASK((NPOIOFF(J)+1):NPOIOFF(J+1))),&
         &(NPOIOFF(J)+1),NPOIOFF(J+1),IREM
  ENDDO
ENDIF

ALLOCATE (GELAMG(NPOIG))
ALLOCATE (GELATG(NPOIG))
ALLOCATE (DZLATG(NPOIG))
ALLOCATE (DZLONG(NPOIG))
ALLOCATE (RCORIG(NPOIG))
ALLOCATE (GEMUG (NPOIG))
ALLOCATE (GSQM2G(NPOIG))
ALLOCATE (GECLOG(NPOIG))
ALLOCATE (GESLOG(NPOIG))

GELAMG(:)=GELAM(:)
GELATG(:)=GELAT(:)
! DZLATG and DZLONG has not much sense in regular lat/lon and masked
! Those are not used anywhere else in the code expect rdcoor
! So must be revisited if future implementation requires these fields
IF (NDIMCDF /= 2)THEN
  DZLATG(:)=DZLAT(:)
  DZLONG(:)=DZLON(:)
ELSE
  DZLATG(:)=0._JPRB
  DZLONG(:)=0._JPRB
ENDIF
RCORIG(:)=RCORI(:)
GEMUG (:)=GEMU (:)
GSQM2G(:)=GSQM2(:)
GECLOG(:)=GECLO(:)
GESLOG(:)=GESLO(:)

DEALLOCATE (GELAM)
DEALLOCATE (GELAT)
DEALLOCATE (DZLAT)
DEALLOCATE (DZLON)
DEALLOCATE (RCORI)
DEALLOCATE (GEMU )
DEALLOCATE (GSQM2)
DEALLOCATE (GECLO)
DEALLOCATE (GESLO)

NPOI=NPOIP(MYPROC)
NPOIALL=NPOIPALL(MYPROC)


ALLOCATE (GELAM(NPOI))
ALLOCATE (GELAT(NPOI))
ALLOCATE (DZLAT(NPOI))
ALLOCATE (DZLON(NPOI))
ALLOCATE (RCORI(NPOI))
ALLOCATE (GEMU (NPOI))
ALLOCATE (GSQM2(NPOI))
ALLOCATE (GECLO(NPOI))
ALLOCATE (GESLO(NPOI))

CALL MPL_SCATTERV(PRECVBUF=GELAM(:),KROOT=1,PSENDBUF=GELAMG(:),KSENDCOUNTS=NPOIP(:),CDSTRING="SUG1S:GELAM")
CALL MPL_SCATTERV(PRECVBUF=GELAT(:),KROOT=1,PSENDBUF=GELATG(:),KSENDCOUNTS=NPOIP(:),CDSTRING="SUG1S:GELAT")
CALL MPL_SCATTERV(PRECVBUF=DZLAT(:),KROOT=1,PSENDBUF=DZLATG(:),KSENDCOUNTS=NPOIP(:),CDSTRING="SUG1S:DZLAT")
CALL MPL_SCATTERV(PRECVBUF=DZLON(:),KROOT=1,PSENDBUF=DZLONG(:),KSENDCOUNTS=NPOIP(:),CDSTRING="SUG1S:DZLON")
CALL MPL_SCATTERV(PRECVBUF=RCORI(:),KROOT=1,PSENDBUF=RCORIG(:),KSENDCOUNTS=NPOIP(:),CDSTRING="SUG1S:RCORI")
CALL MPL_SCATTERV(PRECVBUF=GEMU (:),KROOT=1,PSENDBUF=GEMUG (:),KSENDCOUNTS=NPOIP(:),CDSTRING="SUG1S:GEMU ")
CALL MPL_SCATTERV(PRECVBUF=GSQM2(:),KROOT=1,PSENDBUF=GSQM2G(:),KSENDCOUNTS=NPOIP(:),CDSTRING="SUG1S:GSQM2")
CALL MPL_SCATTERV(PRECVBUF=GECLO(:),KROOT=1,PSENDBUF=GECLOG(:),KSENDCOUNTS=NPOIP(:),CDSTRING="SUG1S:GECLO")
CALL MPL_SCATTERV(PRECVBUF=GESLO(:),KROOT=1,PSENDBUF=GESLOG(:),KSENDCOUNTS=NPOIP(:),CDSTRING="SUG1S:GESLO")

DEALLOCATE (GELAMG)
DEALLOCATE (GELATG)
DEALLOCATE (DZLATG)
DEALLOCATE (DZLONG)
DEALLOCATE (RCORIG)
DEALLOCATE (GEMUG )
DEALLOCATE (GSQM2G)
DEALLOCATE (GECLOG)
DEALLOCATE (GESLOG)
CALL MPL_BARRIER()

IF (LHOOK) CALL DR_HOOK('SUGC1S',1,ZHOOK_HANDLE)

RETURN
END SUBROUTINE SUGC1S
