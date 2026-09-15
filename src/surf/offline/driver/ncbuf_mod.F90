MODULE NCBUF_MOD
! (C) Copyright 2026- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

!**** *NCBUF_MOD* - write-behind record buffering for the offline netCDF output

!     Purpose.
!     --------
!       The offline writers (WRTDCDF, WRTPCDF, WRTD2CDF) emit one netCDF
!       put_var per output variable per output step. For a single-point
!       site-year at 30-minute output that is ~166 variables x 17518 steps
!       = ~2.9 million library calls, each writing a handful of bytes. With
!       netCDF-4/HDF5 each such call also extends the dataset and re-selects
!       a hyperslab, which measured at ~9 microseconds a call -- i.e. I/O
!       dominated the run (WRTDCDF alone was 65% of wallclock on AR-SLu).
!
!       This module intercepts those calls. Values are accumulated in memory
!       per (file,variable) and written out with a single put_var covering
!       many records at once, aligned with the variable's time chunk so that
!       HDF5 writes whole chunks. The bytes written are identical; only the
!       number of library calls changes.
!
!       The writers are unchanged apart from a USE statement: the module
!       exposes NCVPT/NCVPT1 with the same signatures as the netCDF-2
!       Fortran entry points they already call, so the compiler resolves the
!       existing call sites to the buffered versions.
!
!       Buffering applies only to a variable whose LAST dimension is the
!       file's unlimited (record) dimension and that is written one record
!       at a time in increasing, contiguous record order -- which is what
!       the writers do. Anything else falls through to a direct write, so
!       the restart and climate files are untouched.

!**   Interface.
!     ----------
!       CALL NCBUF_SETUP(KNBUF)   -- once, from SULUN1S, with NIOBUF
!       CALL NCVPT / NCVPT1       -- from the writers (drop-in)
!       CALL NCBUF_FLUSH_ALL()    -- from CNTEND, before closing the files

!     Author.
!     -------
!       Gianpaolo Balsamo, ECMWF

!     Modifications.
!     --------------
!       Original : 2026-09

USE PARKIND1   ,ONLY : JPIM     ,JPRM     ,JPRD
USE NETCDF
USE NETCDF_UTILS ,ONLY : NCERROR

IMPLICIT NONE
SAVE
PRIVATE

PUBLIC :: NCVPT, NCVPT1, NCBUF_SETUP, NCBUF_FLUSH_ALL, NCBUF_FLUSH_FILE

!* -- Tunables -------------------------------------------------------------
INTEGER(KIND=JPIM),PARAMETER :: JPSLOT     = 1024      ! max buffered variables
INTEGER(KIND=JPIM),PARAMETER :: JPHASH     = 4096      ! hash table size (power of 2)
INTEGER(KIND=JPIM),PARAMETER :: JPMAXDIM   = 7         ! max variable rank handled
INTEGER(KIND=JPIM),PARAMETER :: JPSLOTBYTE = 4194304   ! per-variable buffer cap (4 MB)
INTEGER(KIND=JPIM),PARAMETER :: JPTOTBYTE  = 536870912 ! total buffer cap (512 MB)
INTEGER(KIND=JPIM),PARAMETER :: JPDEFREC   = 512       ! records/buffer when no chunking info

!* -- Source data kinds ----------------------------------------------------
INTEGER(KIND=JPIM),PARAMETER :: JPK_R4 = 1
INTEGER(KIND=JPIM),PARAMETER :: JPK_R8 = 2
INTEGER(KIND=JPIM),PARAMETER :: JPK_I4 = 3

TYPE TNCBUF
  INTEGER(KIND=JPIM) :: IPOS   = -1          ! netCDF id of the file
  INTEGER(KIND=JPIM) :: IVAR   = -1          ! netCDF id of the variable
  INTEGER(KIND=JPIM) :: INDIMS = 0           ! rank of the variable
  INTEGER(KIND=JPIM) :: IKIND  = 0           ! JPK_R4 / JPK_R8 / JPK_I4
  INTEGER(KIND=JPIM) :: ISTART(JPMAXDIM) = 1 ! start of the non-record dimensions
  INTEGER(KIND=JPIM) :: ICOUNT(JPMAXDIM) = 1 ! count of the non-record dimensions
  INTEGER(KIND=JPIM) :: IREC0  = 0           ! record index of the first buffered record
  INTEGER(KIND=JPIM) :: INREC  = 0           ! records currently buffered
  INTEGER(KIND=JPIM) :: INCAP  = 0           ! buffer capacity, 0 = pass through
  INTEGER(KIND=JPIM) :: INVALS = 0           ! values per record
  REAL(KIND=JPRM)   ,ALLOCATABLE :: Z4(:)
  REAL(KIND=JPRD)   ,ALLOCATABLE :: Z8(:)
  INTEGER(KIND=JPIM),ALLOCATABLE :: J4(:)
END TYPE TNCBUF

TYPE(TNCBUF)       :: YLBUF(JPSLOT)
INTEGER(KIND=JPIM) :: IHASH(0:JPHASH-1) = 0  ! 0 = empty, else slot index
INTEGER(KIND=JPIM) :: NSLOT   = 0            ! slots in use
INTEGER(KIND=JPIM) :: NTOTB   = 0            ! bytes currently allocated
INTEGER(KIND=JPIM) :: NIOBUFL = 0            ! <0 auto, 0 off, >0 forced records
LOGICAL            :: LLSETUP = .FALSE.

INTERFACE NCVPT
  MODULE PROCEDURE NCVPT_R4_1, NCVPT_R4_2, NCVPT_R8_1, NCVPT_R8_2, &
                 & NCVPT_R8_0, NCVPT_I4_0
END INTERFACE NCVPT

INTERFACE NCVPT1
  MODULE PROCEDURE NCVPT1_R8, NCVPT1_I4
END INTERFACE NCVPT1

CONTAINS

!=========================================================================
SUBROUTINE NCBUF_SETUP(KNBUF)
!* -- Enable/configure buffering. KNBUF <0 auto, 0 off, >0 records per buffer.
INTEGER(KIND=JPIM),INTENT(IN) :: KNBUF
NIOBUFL = KNBUF
LLSETUP = .TRUE.
END SUBROUTINE NCBUF_SETUP

!=========================================================================
INTEGER(KIND=JPIM) FUNCTION IFIND_SLOT(KPOS,KVAR)
!* -- Return the slot for (KPOS,KVAR), creating and sizing it on first use.
!     A returned slot with INCAP == 0 means "do not buffer this variable".
INTEGER(KIND=JPIM),INTENT(IN) :: KPOS,KVAR
INTEGER(KIND=JPIM) :: IH,IS,INDIMS,IUNLIM,IDIMS(JPMAXDIM),ITYPE,IELEM
INTEGER(KIND=JPIM) :: ICH(JPMAXDIM),ISTAT,ICAP,JD,ILEN,INVALS,IFMT

!* -- mix the file and variable ids without ever forming a large product:
!     netCDF-4 file ids are themselves large, so KPOS*const would overflow.
IH = IAND(ABS(IEOR(KPOS,ISHFT(KVAR,7))),JPHASH-1)

!* -- open addressing, linear probe
DO
  IS = IHASH(IH)
  IF ( IS == 0 ) EXIT
  IF ( YLBUF(IS)%IPOS == KPOS .AND. YLBUF(IS)%IVAR == KVAR ) THEN
    IFIND_SLOT = IS
    RETURN
  ENDIF
  IH = IAND(IH+1,JPHASH-1)
ENDDO

!* -- not seen before: decide whether it can be buffered
IF ( NSLOT >= JPSLOT ) THEN
  IFIND_SLOT = 0
  RETURN
ENDIF
NSLOT = NSLOT + 1
IS    = NSLOT
IHASH(IH) = IS
YLBUF(IS)%IPOS  = KPOS
YLBUF(IS)%IVAR  = KVAR
YLBUF(IS)%INCAP = 0
IFIND_SLOT = IS

!* -- the last dimension must be the file's unlimited dimension
ISTAT = NF90_INQUIRE(KPOS,UNLIMITEDDIMID=IUNLIM)
IF ( ISTAT /= NF90_NOERR ) RETURN
IF ( IUNLIM < 0 ) RETURN
ISTAT = NF90_INQUIRE_VARIABLE(KPOS,KVAR,XTYPE=ITYPE,NDIMS=INDIMS)
IF ( ISTAT /= NF90_NOERR ) RETURN
IF ( INDIMS < 1 .OR. INDIMS > JPMAXDIM ) RETURN
ISTAT = NF90_INQUIRE_VARIABLE(KPOS,KVAR,DIMIDS=IDIMS(1:INDIMS))
IF ( ISTAT /= NF90_NOERR ) RETURN
IF ( IDIMS(INDIMS) /= IUNLIM ) RETURN
!  ... and no other dimension may be unlimited
DO JD=1,INDIMS-1
  IF ( IDIMS(JD) == IUNLIM ) RETURN
ENDDO

!* -- values per record, from the declared extents of the fixed dimensions
INVALS = 1
DO JD=1,INDIMS-1
  ISTAT = NF90_INQUIRE_DIMENSION(KPOS,IDIMS(JD),LEN=ILEN)
  IF ( ISTAT /= NF90_NOERR ) RETURN
  INVALS = INVALS*MAX(1,ILEN)
ENDDO
YLBUF(IS)%INDIMS = INDIMS
YLBUF(IS)%INVALS = INVALS

!* -- buffer length: match the time chunk so a flush writes whole chunks.
!     The chunk enquiry is only asked of a netCDF-4 file: on a classic file
!     nc_inq_var_chunking asserts rather than returning an error status.
IF ( NIOBUFL > 0 ) THEN
  ICAP = NIOBUFL
ELSE
  ICAP  = JPDEFREC
  ISTAT = NF90_INQUIRE(KPOS,FORMATNUM=IFMT)
  IF ( ISTAT == NF90_NOERR .AND. &
     & ( IFMT == NF90_FORMAT_NETCDF4 .OR. IFMT == NF90_FORMAT_NETCDF4_CLASSIC ) ) THEN
    ISTAT = NF90_INQUIRE_VARIABLE(KPOS,KVAR,CHUNKSIZES=ICH(1:INDIMS))
    IF ( ISTAT == NF90_NOERR ) ICAP = MAX(1,ICH(INDIMS))
  ENDIF
ENDIF

!* -- The buffer holds the values in the kind the writer supplies, which can
!     be wider than the variable's external type (NACCUR=2 feeds JPRD into a
!     float variable), so budget for the widest case rather than for ITYPE.
IELEM = 8
!* -- honour the per-variable and total memory budgets
ICAP = MAX(1,MIN(ICAP,JPSLOTBYTE/MAX(1,INVALS*IELEM)))
IF ( ICAP <= 1 ) RETURN
IF ( NTOTB > JPTOTBYTE - ICAP*INVALS*IELEM ) RETURN

YLBUF(IS)%INCAP = ICAP
NTOTB = NTOTB + ICAP*INVALS*IELEM

END FUNCTION IFIND_SLOT

!=========================================================================
SUBROUTINE NCBUF_FLUSH(KS)
!* -- Write the records held for slot KS with a single put_var.
INTEGER(KIND=JPIM),INTENT(IN) :: KS
INTEGER(KIND=JPIM) :: ISTART(JPMAXDIM),ICOUNT(JPMAXDIM),IND,IN

IF ( KS <= 0 ) RETURN
IF ( YLBUF(KS)%INREC <= 0 ) RETURN
IND = YLBUF(KS)%INDIMS
ISTART(1:IND) = YLBUF(KS)%ISTART(1:IND)
ICOUNT(1:IND) = YLBUF(KS)%ICOUNT(1:IND)
ISTART(IND)   = YLBUF(KS)%IREC0
ICOUNT(IND)   = YLBUF(KS)%INREC
IN            = YLBUF(KS)%INREC*YLBUF(KS)%INVALS

SELECT CASE ( YLBUF(KS)%IKIND )
CASE ( JPK_R4 )
  CALL NCERROR( NF90_PUT_VAR(YLBUF(KS)%IPOS,YLBUF(KS)%IVAR,YLBUF(KS)%Z4(1:IN), &
              & START=ISTART(1:IND),COUNT=ICOUNT(1:IND)), "NCBUF_FLUSH (r4)" )
CASE ( JPK_R8 )
  CALL NCERROR( NF90_PUT_VAR(YLBUF(KS)%IPOS,YLBUF(KS)%IVAR,YLBUF(KS)%Z8(1:IN), &
              & START=ISTART(1:IND),COUNT=ICOUNT(1:IND)), "NCBUF_FLUSH (r8)" )
CASE ( JPK_I4 )
  CALL NCERROR( NF90_PUT_VAR(YLBUF(KS)%IPOS,YLBUF(KS)%IVAR,YLBUF(KS)%J4(1:IN), &
              & START=ISTART(1:IND),COUNT=ICOUNT(1:IND)), "NCBUF_FLUSH (i4)" )
END SELECT

YLBUF(KS)%INREC = 0

END SUBROUTINE NCBUF_FLUSH

!=========================================================================
SUBROUTINE NCBUF_FLUSH_ALL()
!* -- Flush every slot. MUST be called before the output files are closed.
INTEGER(KIND=JPIM) :: JS
DO JS=1,NSLOT
  CALL NCBUF_FLUSH(JS)
ENDDO
END SUBROUTINE NCBUF_FLUSH_ALL

!=========================================================================
SUBROUTINE NCBUF_FLUSH_FILE(KPOS)
!* -- Flush every slot belonging to one file (used before an explicit sync).
INTEGER(KIND=JPIM),INTENT(IN) :: KPOS
INTEGER(KIND=JPIM) :: JS
DO JS=1,NSLOT
  IF ( YLBUF(JS)%IPOS == KPOS ) CALL NCBUF_FLUSH(JS)
ENDDO
END SUBROUTINE NCBUF_FLUSH_FILE

!=========================================================================
LOGICAL FUNCTION LLACCEPT(KS,KSTART,KCOUNT,KKIND)
!* -- Decide whether this put can join slot KS, flushing it first if the
!     new record is not the contiguous successor of what is held.
INTEGER(KIND=JPIM),INTENT(IN) :: KS,KSTART(:),KCOUNT(:),KKIND
INTEGER(KIND=JPIM) :: IND,JD
LOGICAL :: LLSAME

LLACCEPT = .FALSE.
IF ( KS <= 0 ) RETURN
IF ( YLBUF(KS)%INCAP <= 0 ) RETURN
IND = YLBUF(KS)%INDIMS
IF ( SIZE(KSTART) /= IND .OR. SIZE(KCOUNT) /= IND ) RETURN
IF ( KCOUNT(IND) /= 1 ) RETURN                 ! one record at a time only
IF ( PRODUCT(KCOUNT(1:IND)) /= YLBUF(KS)%INVALS ) RETURN

IF ( YLBUF(KS)%INREC > 0 ) THEN
  LLSAME = ( KSTART(IND) == YLBUF(KS)%IREC0+YLBUF(KS)%INREC ) .AND. &
         & ( KKIND == YLBUF(KS)%IKIND )
  DO JD=1,IND-1
    IF ( KSTART(JD) /= YLBUF(KS)%ISTART(JD) ) LLSAME = .FALSE.
    IF ( KCOUNT(JD) /= YLBUF(KS)%ICOUNT(JD) ) LLSAME = .FALSE.
  ENDDO
  IF ( .NOT.LLSAME ) CALL NCBUF_FLUSH(KS)
ENDIF

IF ( YLBUF(KS)%INREC == 0 ) THEN
  YLBUF(KS)%IKIND = KKIND
  YLBUF(KS)%IREC0 = KSTART(IND)
  YLBUF(KS)%ISTART(1:IND) = KSTART(1:IND)
  YLBUF(KS)%ICOUNT(1:IND) = KCOUNT(1:IND)
ENDIF
LLACCEPT = .TRUE.

END FUNCTION LLACCEPT

!=========================================================================
SUBROUTINE NCBUF_PUT_R4(KPOS,KVAR,KSTART,KCOUNT,PVAL,KERR)
INTEGER(KIND=JPIM),INTENT(IN)  :: KPOS,KVAR,KSTART(:),KCOUNT(:)
REAL(KIND=JPRM)   ,INTENT(IN)  :: PVAL(*)
INTEGER(KIND=JPIM),INTENT(OUT) :: KERR
INTEGER(KIND=JPIM) :: IS,IN,IO

KERR = NF90_NOERR
IN   = PRODUCT(KCOUNT)
IF ( .NOT.LLSETUP .OR. NIOBUFL == 0 ) THEN
  KERR = NF90_PUT_VAR(KPOS,KVAR,PVAL(1:IN),START=KSTART,COUNT=KCOUNT)
  RETURN
ENDIF
IS = IFIND_SLOT(KPOS,KVAR)
IF ( .NOT.LLACCEPT(IS,KSTART,KCOUNT,JPK_R4) ) THEN
  KERR = NF90_PUT_VAR(KPOS,KVAR,PVAL(1:IN),START=KSTART,COUNT=KCOUNT)
  RETURN
ENDIF
IF ( .NOT.ALLOCATED(YLBUF(IS)%Z4) ) ALLOCATE(YLBUF(IS)%Z4(YLBUF(IS)%INCAP*YLBUF(IS)%INVALS))
IO = YLBUF(IS)%INREC*YLBUF(IS)%INVALS
YLBUF(IS)%Z4(IO+1:IO+IN) = PVAL(1:IN)
YLBUF(IS)%INREC = YLBUF(IS)%INREC + 1
IF ( YLBUF(IS)%INREC >= YLBUF(IS)%INCAP ) CALL NCBUF_FLUSH(IS)

END SUBROUTINE NCBUF_PUT_R4

!=========================================================================
SUBROUTINE NCBUF_PUT_R8(KPOS,KVAR,KSTART,KCOUNT,PVAL,KERR)
INTEGER(KIND=JPIM),INTENT(IN)  :: KPOS,KVAR,KSTART(:),KCOUNT(:)
REAL(KIND=JPRD)   ,INTENT(IN)  :: PVAL(*)
INTEGER(KIND=JPIM),INTENT(OUT) :: KERR
INTEGER(KIND=JPIM) :: IS,IN,IO

KERR = NF90_NOERR
IN   = PRODUCT(KCOUNT)
IF ( .NOT.LLSETUP .OR. NIOBUFL == 0 ) THEN
  KERR = NF90_PUT_VAR(KPOS,KVAR,PVAL(1:IN),START=KSTART,COUNT=KCOUNT)
  RETURN
ENDIF
IS = IFIND_SLOT(KPOS,KVAR)
IF ( .NOT.LLACCEPT(IS,KSTART,KCOUNT,JPK_R8) ) THEN
  KERR = NF90_PUT_VAR(KPOS,KVAR,PVAL(1:IN),START=KSTART,COUNT=KCOUNT)
  RETURN
ENDIF
IF ( .NOT.ALLOCATED(YLBUF(IS)%Z8) ) ALLOCATE(YLBUF(IS)%Z8(YLBUF(IS)%INCAP*YLBUF(IS)%INVALS))
IO = YLBUF(IS)%INREC*YLBUF(IS)%INVALS
YLBUF(IS)%Z8(IO+1:IO+IN) = PVAL(1:IN)
YLBUF(IS)%INREC = YLBUF(IS)%INREC + 1
IF ( YLBUF(IS)%INREC >= YLBUF(IS)%INCAP ) CALL NCBUF_FLUSH(IS)

END SUBROUTINE NCBUF_PUT_R8

!=========================================================================
SUBROUTINE NCBUF_PUT_I4(KPOS,KVAR,KSTART,KCOUNT,KVAL,KERR)
INTEGER(KIND=JPIM),INTENT(IN)  :: KPOS,KVAR,KSTART(:),KCOUNT(:)
INTEGER(KIND=JPIM),INTENT(IN)  :: KVAL(*)
INTEGER(KIND=JPIM),INTENT(OUT) :: KERR
INTEGER(KIND=JPIM) :: IS,IN,IO

KERR = NF90_NOERR
IN   = PRODUCT(KCOUNT)
IF ( .NOT.LLSETUP .OR. NIOBUFL == 0 ) THEN
  KERR = NF90_PUT_VAR(KPOS,KVAR,KVAL(1:IN),START=KSTART,COUNT=KCOUNT)
  RETURN
ENDIF
IS = IFIND_SLOT(KPOS,KVAR)
IF ( .NOT.LLACCEPT(IS,KSTART,KCOUNT,JPK_I4) ) THEN
  KERR = NF90_PUT_VAR(KPOS,KVAR,KVAL(1:IN),START=KSTART,COUNT=KCOUNT)
  RETURN
ENDIF
IF ( .NOT.ALLOCATED(YLBUF(IS)%J4) ) ALLOCATE(YLBUF(IS)%J4(YLBUF(IS)%INCAP*YLBUF(IS)%INVALS))
IO = YLBUF(IS)%INREC*YLBUF(IS)%INVALS
YLBUF(IS)%J4(IO+1:IO+IN) = KVAL(1:IN)
YLBUF(IS)%INREC = YLBUF(IS)%INREC + 1
IF ( YLBUF(IS)%INREC >= YLBUF(IS)%INCAP ) CALL NCBUF_FLUSH(IS)

END SUBROUTINE NCBUF_PUT_I4

!=========================================================================
!* -- Drop-in replacements for the netCDF-2 Fortran entry points.
!     The generic is resolved on the type and rank of the value argument.
!=========================================================================
SUBROUTINE NCVPT_R4_1(KPOS,KVAR,KSTART,KCOUNT,PVAL,KERR)
INTEGER(KIND=JPIM),INTENT(IN)  :: KPOS,KVAR,KSTART(:),KCOUNT(:)
REAL(KIND=JPRM)   ,INTENT(IN)  :: PVAL(:)
INTEGER(KIND=JPIM),INTENT(OUT) :: KERR
CALL NCBUF_PUT_R4(KPOS,KVAR,KSTART,KCOUNT,PVAL,KERR)
END SUBROUTINE NCVPT_R4_1

SUBROUTINE NCVPT_R4_2(KPOS,KVAR,KSTART,KCOUNT,PVAL,KERR)
INTEGER(KIND=JPIM),INTENT(IN)  :: KPOS,KVAR,KSTART(:),KCOUNT(:)
REAL(KIND=JPRM)   ,INTENT(IN),CONTIGUOUS :: PVAL(:,:)
INTEGER(KIND=JPIM),INTENT(OUT) :: KERR
CALL NCBUF_PUT_R4(KPOS,KVAR,KSTART,KCOUNT,PVAL,KERR)
END SUBROUTINE NCVPT_R4_2

SUBROUTINE NCVPT_R8_1(KPOS,KVAR,KSTART,KCOUNT,PVAL,KERR)
INTEGER(KIND=JPIM),INTENT(IN)  :: KPOS,KVAR,KSTART(:),KCOUNT(:)
REAL(KIND=JPRD)   ,INTENT(IN)  :: PVAL(:)
INTEGER(KIND=JPIM),INTENT(OUT) :: KERR
CALL NCBUF_PUT_R8(KPOS,KVAR,KSTART,KCOUNT,PVAL,KERR)
END SUBROUTINE NCVPT_R8_1

SUBROUTINE NCVPT_R8_2(KPOS,KVAR,KSTART,KCOUNT,PVAL,KERR)
INTEGER(KIND=JPIM),INTENT(IN)  :: KPOS,KVAR,KSTART(:),KCOUNT(:)
REAL(KIND=JPRD)   ,INTENT(IN),CONTIGUOUS :: PVAL(:,:)
INTEGER(KIND=JPIM),INTENT(OUT) :: KERR
CALL NCBUF_PUT_R8(KPOS,KVAR,KSTART,KCOUNT,PVAL,KERR)
END SUBROUTINE NCVPT_R8_2

SUBROUTINE NCVPT_R8_0(KPOS,KVAR,KSTART,KCOUNT,PVAL,KERR)
INTEGER(KIND=JPIM),INTENT(IN)  :: KPOS,KVAR,KSTART(:),KCOUNT(:)
REAL(KIND=JPRD)   ,INTENT(IN)  :: PVAL
INTEGER(KIND=JPIM),INTENT(OUT) :: KERR
REAL(KIND=JPRD) :: ZV(1)
ZV(1) = PVAL
CALL NCBUF_PUT_R8(KPOS,KVAR,KSTART,KCOUNT,ZV,KERR)
END SUBROUTINE NCVPT_R8_0

SUBROUTINE NCVPT_I4_0(KPOS,KVAR,KSTART,KCOUNT,KVAL,KERR)
INTEGER(KIND=JPIM),INTENT(IN)  :: KPOS,KVAR,KSTART(:),KCOUNT(:)
INTEGER(KIND=JPIM),INTENT(IN)  :: KVAL
INTEGER(KIND=JPIM),INTENT(OUT) :: KERR
INTEGER(KIND=JPIM) :: IV(1)
IV(1) = KVAL
CALL NCBUF_PUT_I4(KPOS,KVAR,KSTART,KCOUNT,IV,KERR)
END SUBROUTINE NCVPT_I4_0

!=========================================================================
SUBROUTINE NCVPT1_R8(KPOS,KVAR,KINDEX,PVAL,KERR)
!* -- Single-element write: same as NCVPT with COUNT == 1 everywhere.
INTEGER(KIND=JPIM),INTENT(IN)  :: KPOS,KVAR,KINDEX(:)
REAL(KIND=JPRD)   ,INTENT(IN)  :: PVAL
INTEGER(KIND=JPIM),INTENT(OUT) :: KERR
INTEGER(KIND=JPIM) :: ICOUNT(SIZE(KINDEX))
REAL(KIND=JPRD)    :: ZV(1)
ICOUNT(:) = 1
ZV(1)     = PVAL
CALL NCBUF_PUT_R8(KPOS,KVAR,KINDEX,ICOUNT,ZV,KERR)
END SUBROUTINE NCVPT1_R8

SUBROUTINE NCVPT1_I4(KPOS,KVAR,KINDEX,KVAL,KERR)
INTEGER(KIND=JPIM),INTENT(IN)  :: KPOS,KVAR,KINDEX(:)
INTEGER(KIND=JPIM),INTENT(IN)  :: KVAL
INTEGER(KIND=JPIM),INTENT(OUT) :: KERR
INTEGER(KIND=JPIM) :: ICOUNT(SIZE(KINDEX))
INTEGER(KIND=JPIM) :: IV(1)
ICOUNT(:) = 1
IV(1)     = KVAL
CALL NCBUF_PUT_I4(KPOS,KVAR,KINDEX,ICOUNT,IV,KERR)
END SUBROUTINE NCVPT1_I4

END MODULE NCBUF_MOD
