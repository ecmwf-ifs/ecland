SUBROUTINE RDSSDP(NCID)

USE YOMDPHY   ,ONLY : NPOI
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMDIM1S  ,ONLY : NPROMA
USE YOMHOOK   ,ONLY : LHOOK    ,DR_HOOK, JPHOOK
USE YOMSURF_SSDP_MOD
USE YOMGPD1S , ONLY : GPD_SDP2, GPD_SDP3
#ifdef DOC

!**** *RDSSDP  * - Reading netCDF file containing surface climate fields from surf_param.nc file

!     Purpose.
!     --------
!            initialization surface characteristics

!**   Interface.
!     ----------
!        *CALL* *RDSSDP(NCID)

!     Explicit arguments :
!     --------------------
!     NCID      INT     NetCDF file code


!        Implicit arguments :
!        --------------------


!     Method.
!     -------
!       Opens a file called 'surf_param.nc' to read relevant fields


!     Externals.
!     ----------
!       NETCDF-utilities

!     Reference.
!     ----------

!     Author.
!     -------
!        Matthias Kelbling, UFZ

!     Modifications.
!     --------------
!        Original : 2019-07-31
!        I. Ayan-Miguez: June 2023

!     ------------------------------------------------------------------

#endif

USE YOMLUN1S , ONLY : NULOUT
USE YOMDPHY  , ONLY : NLON, NLAT, NLALO, NPOIP, NPOIOFF
USE YOMLOG1S , ONLY : NDIMCDF
USE YOMGC1S  , ONLY : LMASK
USE NETCDF
USE NETCDF_UTILS, ONLY: NCERROR

USE MPL_MODULE

IMPLICIT NONE

#include "netcdf.inc"
#include "minmax.intfb.h"

INTEGER(KIND=JPIM),INTENT(IN) :: NCID

INTEGER                     :: ISTART2(2),ICOUNT2(2)
INTEGER                     :: ISTART3(3),ICOUNT3(3)
REAL(KIND=JPRB),ALLOCATABLE :: ZREAL2D(:)
REAL(KIND=JPRB),ALLOCATABLE :: ZREAL3D(:,:)
REAL(KIND=JPRB),ALLOCATABLE :: ZBUF(:)
INTEGER                     :: NILON,NILAT,IERR,NVARID,NILEV
INTEGER                     :: NDIM, NVARS
CHARACTER*100               :: CNAME
INTEGER(KIND=JPIM)          :: NMX,NMY,IVAR,JVT,JD,DIMLEN,STATUS
INTEGER(KIND=JPIM)          :: NVARS3D, NVARS2D
CHARACTER(LEN=20)           :: CVARS3D(40), CVARS2D(70), CVAR
INTEGER(KIND=JPIM)          :: MYPROC, NPROC, ISTP, IENP
REAL(KIND=JPHOOK)           :: ZHOOK_HANDLE  

IF (LHOOK) CALL DR_HOOK('RDSSDP',0,ZHOOK_HANDLE)    
DATA ISTART2/1,1/
DATA ISTART3/1,1,1/

MYPROC = MPL_MYRANK()
NPROC  = MPL_NPROC() 

ISTP  = NPOIOFF(MYPROC)+1
IENP = NPOIOFF(MYPROC+1)

!* check the dimensions
NILON=-999
NILAT=-999
 
IF( MYPROC == 1 ) THEN
  CALL NCERROR (NF90_INQUIRE(NCID, nDimensions=NDIM,nVariables=NVARS),'FINDING N DIMS')
  DO JD=1,NDIM
     CALL NCERROR(NF90_INQUIRE_DIMENSION(NCID, JD, name=CNAME, len=DIMLEN),'INQ DIMENSION')
     SELECT CASE (TRIM(CNAME))
     CASE('x','lon')
        NILON=DIMLEN
        WRITE(NULOUT,*)'DIMENSION x ',TRIM(CNAME),' FOUND WITH LEN=',NILON
     CASE('y','lat')
        NILAT=DIMLEN
        WRITE(NULOUT,*)'DIMENSION y ',TRIM(CNAME),' FOUND WITH LEN=',NILAT
     CASE('nlevs')
        NILEV=DIMLEN
        WRITE(NULOUT,*)'DIMENSION z ',TRIM(CNAME),' FOUND WITH LEN=',NILEV
     END SELECT
  END DO
ENDIF 

CALL MPL_BROADCAST(NDIM, KROOT=1,KTAG=100,CDSTRING='NDIM')
CALL MPL_BROADCAST(NILON,KROOT=1,KTAG=200,CDSTRING='NILON')
CALL MPL_BROADCAST(NILAT,KROOT=1,KTAG=300,CDSTRING='NILAT')
CALL MPL_BROADCAST(NILEV,KROOT=1,KTAG=400,CDSTRING='NILEV')

IF (NDIMCDF == 1 ) THEN
   NILAT = 0
ENDIF

IF(NILON /= NLON .OR. NILAT /= NLAT) THEN
   WRITE(NULOUT,*)'NLON OR NLAT NOT SPECIFIED CORRECTLY:'
   WRITE(NULOUT,*)'NLON IN NAMELIST AND surf_param.nc: ',NLON,NILON
   WRITE(NULOUT,*)'NLAT IN NAMELIST AND surf_param.nc: ',NLAT,NILAT
   CALL ABORT
ENDIF

IF(NDIMCDF == 2)THEN
   ICOUNT2(1)=NILON
   ICOUNT2(2)=NILAT

   ICOUNT3(1)=NILON
   ICOUNT3(2)=NILAT
   ICOUNT3(3)=NILEV

   NMX=NLON
   NMY=NLAT
ELSE
   ICOUNT2(1)=NILON
   
   ICOUNT3(1)=NILON
   ICOUNT3(2)=NILEV
   
   NMX=NLALO
   NMY=1
ENDIF

ALLOCATE (ZREAL2D(NLALO))
ALLOCATE (ZREAL3D(NLALO,NILEV))
ALLOCATE (ZBUF(NPOI))

CALL MPL_BARRIER()  

NVARS2D=13
CVARS2D(1:NVARS2D)=(/'lvegcov', 'hvegcov', 'hlamsk ', 'llamsk ', 'hlamsks', 'llamsks', 'hvegrsm', 'lvegrsm', &
        & 'lrvz0m ', 'hrvz0m ', 'lrvz0h ', 'hrvz0h ', 'bvegrsm'/)

DO IVAR=1,NVARS2D
   CVAR=TRIM(CVARS2D(IVAR))
   IF( MYPROC == 1 ) THEN
     STATUS = NF90_INQ_VARID(NCID, CVAR, NVARID)
     IF ( STATUS /= 0 ) THEN
        WRITE(NULOUT,'(A)') CVAR//' NOT PRESENT IN surf_param.nc FILE'
     ELSE
        CALL NCERROR( NF90_GET_VAR(NCID,NVARID,ZREAL2D,ISTART2,ICOUNT2),'READING '//CVAR)
        CALL MINMAX(CVAR,ZREAL2D(:),NMX,NMY,LMASK,NULOUT)
     ENDIF
   ENDIF

   CALL MPL_BROADCAST(STATUS,KROOT=1,KTAG=100,CDSTRING='STATUS')
   CALL MPL_SCATTERV(PRECVBUF=ZBUF(:),KROOT=1,PSENDBUF=ZREAL2D(:),KSENDCOUNTS=NPOIP(:),CDSTRING='RDSSDP2D: '//CVAR)

 
   SELECT CASE(CVAR)
     CASE('lvegcov ')
       IF ( STATUS /= 0 ) THEN 
         WRITE(NULOUT,*) CVAR, 'Not calibrated'
       ELSE
         GPD_SDP2(1:NPOI, SSDP2D_ID%NRVCOVL2D)=PACK(ZBUF,LMASK(ISTP:IENP))
         WRITE(NULOUT,*) CVAR, 'Calibrated value from surf_param.nc'
       ENDIF
     CASE('hvegcov ')
       IF ( STATUS /= 0 ) THEN 
         WRITE(NULOUT,*) CVAR, 'Not calibrated'
       ELSE
         GPD_SDP2(1:NPOI, SSDP2D_ID%NRVCOVH2D)=PACK(ZBUF,LMASK(ISTP:IENP))
         WRITE(NULOUT,*) CVAR, 'Calibrated value from surf_param.nc'
       ENDIF
     CASE('hlamsk ')
       IF ( STATUS /= 0 ) THEN 
         WRITE(NULOUT,*) CVAR, 'Not calibrated'
       ELSE
         GPD_SDP2(1:NPOI, SSDP2D_ID%NRVLAMSKH2D)=PACK(ZBUF,LMASK(ISTP:IENP))
         WRITE(NULOUT,*) CVAR, 'Calibrated value from surf_param.nc'
       ENDIF
     CASE('llamsk ')
       IF ( STATUS /= 0 ) THEN 
         WRITE(NULOUT,*) CVAR, 'Not calibrated'
       ELSE
         GPD_SDP2(1:NPOI, SSDP2D_ID%NRVLAMSKL2D)=PACK(ZBUF,LMASK(ISTP:IENP))
         WRITE(NULOUT,*) CVAR, 'Calibrated value from surf_param.nc'
       ENDIF
     CASE('hlamsks ')
       IF ( STATUS /= 0 ) THEN 
         WRITE(NULOUT,*) CVAR, 'Not calibrated'
       ELSE
         GPD_SDP2(1:NPOI, SSDP2D_ID%NRVLAMSKSH2D)=PACK(ZBUF,LMASK(ISTP:IENP))
         WRITE(NULOUT,*) CVAR, 'Calibrated value from surf_param.nc'
       ENDIF
     CASE('llamsks ')
       IF ( STATUS /= 0 ) THEN 
         WRITE(NULOUT,*) CVAR, 'Not calibrated'
       ELSE
         GPD_SDP2(1:NPOI, SSDP2D_ID%NRVLAMSKSL2D)=PACK(ZBUF,LMASK(ISTP:IENP))
         WRITE(NULOUT,*) CVAR, 'Calibrated value from surf_param.nc'
       ENDIF
     CASE('hvegrsm ')
       IF ( STATUS /= 0 ) THEN 
         WRITE(NULOUT,*) CVAR, 'Not calibrated'
       ELSE
         GPD_SDP2(1:NPOI, SSDP2D_ID%NRVRSMINH2D)=PACK(ZBUF,LMASK(ISTP:IENP))
         WRITE(NULOUT,*) CVAR, 'Calibrated value from surf_param.nc'
       ENDIF
     CASE('lvegrsm ')
       IF ( STATUS /= 0 ) THEN 
         WRITE(NULOUT,*) CVAR, 'Not calibrated'
       ELSE
         GPD_SDP2(1:NPOI, SSDP2D_ID%NRVRSMINL2D)=PACK(ZBUF,LMASK(ISTP:IENP))
         WRITE(NULOUT,*) CVAR, 'Calibrated value from surf_param.nc'
       ENDIF
     CASE('lrvz0m ')
       IF ( STATUS /= 0 ) THEN 
         WRITE(NULOUT,*) CVAR, 'Not calibrated'
       ELSE
         GPD_SDP2(1:NPOI, SSDP2D_ID%NRVZ0ML2D)=PACK(ZBUF,LMASK(ISTP:IENP))
         WRITE(NULOUT,*) CVAR, 'Calibrated value from surf_param.nc'
       ENDIF
     CASE('hrvz0m ')
       IF ( STATUS /= 0 ) THEN
          WRITE(NULOUT,*) CVAR, 'Not calibrated'
       ELSE
         GPD_SDP2(1:NPOI, SSDP2D_ID%NRVZ0MH2D)=PACK(ZBUF,LMASK(ISTP:IENP))
         WRITE(NULOUT,*) CVAR, 'Calibrated value from surf_param.nc'
       ENDIF 
     CASE('lrvz0h ')
       IF ( STATUS /= 0 ) THEN
         WRITE(NULOUT,*) CVAR, 'Not calibrated'
       ELSE
         GPD_SDP2(1:NPOI, SSDP2D_ID%NRVZ0HL2D)=PACK(ZBUF,LMASK(ISTP:IENP))
         WRITE(NULOUT,*) CVAR, 'Calibrated value from surf_param.nc'
       ENDIF
     CASE('hrvz0h ')
       IF ( STATUS /= 0 ) THEN 
         WRITE(NULOUT,*) CVAR, 'Not calibrated'
       ELSE
         GPD_SDP2(1:NPOI, SSDP2D_ID%NRVZ0HH2D)=PACK(ZBUF,LMASK(ISTP:IENP))
         WRITE(NULOUT,*) CVAR, 'Calibrated value from surf_param.nc'
       ENDIF
     CASE('bvegrsm ')
       IF ( STATUS /= 0 ) THEN 
         WRITE(NULOUT,*) CVAR, 'Not calibrated'
       ELSE
         GPD_SDP2(1:NPOI, SSDP2D_ID%NRVRSMINB2D)=PACK(ZBUF,LMASK(ISTP:IENP))
         WRITE(NULOUT,*) CVAR, 'Calibrated value from surf_param.nc'
       ENDIF
     CASE DEFAULT
         WRITE(NULOUT,*) CVAR, ' Not defined in RDSSDP'
         CALL ABORT()
   END SELECT
ENDDO

NVARS3D=1
CVARS3D(1:NVARS3D)=(/'vgalpha '/)

DO IVAR=1,NVARS3D
  CVAR=TRIM(CVARS3D(IVAR))
  IF( MYPROC == 1) THEN
    STATUS = NF90_INQ_VARID(NCID, CVAR, NVARID)
    IF ( STATUS /= 0 ) THEN
      WRITE(NULOUT,'(A)') CVAR//' NOT PRESENT IN surf_param.nc FILE'
    ELSE
      CALL NCERROR( NF90_GET_VAR(NCID,NVARID,ZREAL3D,ISTART3,ICOUNT3),'READING '//CVAR)
    ENDIF
  ENDIF
  DO JVT=1,NILEV
    IF( MYPROC == 1) THEN
      CALL MINMAX(CVAR,ZREAL3D(:,JVT),NMX,NMY,LMASK,NULOUT)
    ENDIF
    CALL MPL_BROADCAST(STATUS,KROOT=1,KTAG=100,CDSTRING='STATUS')
    CALL MPL_SCATTERV(PRECVBUF=ZBUF(:),KROOT=1,PSENDBUF=ZREAL3D(:,JVT),KSENDCOUNTS=NPOIP(:),CDSTRING='RDSSDP3D: '//CVAR)
 
    SELECT CASE(CVAR)
    CASE('vgalpha')
      IF ( STATUS /= 0 ) THEN
        WRITE(NULOUT,*) CVAR, JVT, 'Not calibrated'
      ELSE
        GPD_SDP3(1:NPOI, JVT, SSDP3D_ID%NRMVGALPHA3D)=PACK(ZBUF,LMASK(ISTP:IENP))
        WRITE(NULOUT,*) CVAR, JVT, 'Calibrated value from surf_param.nc'
      ENDIF
    CASE DEFAULT
      WRITE(NULOUT,*) CVAR, JVT, ' Not defined in RDSSDP'
      CALL ABORT()
    END SELECT
  ENDDO
ENDDO

DEALLOCATE (ZREAL2D)
DEALLOCATE (ZREAL3D)
DEALLOCATE (ZBUF)
  
CALL MPL_BARRIER()

IF (LHOOK) CALL DR_HOOK('RDSSDP',1,ZHOOK_HANDLE)    

END SUBROUTINE RDSSDP
