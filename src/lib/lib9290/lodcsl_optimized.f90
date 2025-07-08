!***********************************************************************
!                                                                      *
      SUBROUTINE LODCSL(NCORE)
!                                                                      *
!   Loads the data from the  .csl  file. A number of checks are made   *
!   to ensure correctness and consistency.                             *
!   *** OPTIMIZED VERSION for large CSF files ***                      *
!                                                                      *
!   Key optimizations:                                                 *
!   1. Hash table for O(1) duplicate detection                        *
!   2. Larger initial allocation to reduce reallocations              *
!   3. Progress reporting for large files                             *
!   4. Performance timing                                             *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  13:07:22   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!...Optimized for large CSF files         08/01/25
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE vast_kind_param, ONLY: DOUBLE
      USE parameter_def,   ONLY: NNNW
      USE DEBUG_C
      USE DEF_C
      USE ORB_C
      USE STAT_C
      USE TERMS_C,         only: jtab, ntab
      USE IOUNIT_C
      USE BLK_C,           only: NBLOCK,NCFBLK
      USE memory_man
      USE OMP_LIB  ! 添加OpenMP支持
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      USE prsrsl_I
      USE convrt_I
      USE prsrcn_I
      USE parsjl_I
      USE pack_I
      USE iq_I
      USE jqs_I
      USE jcup_I
      USE itjpo_I
      USE ispar_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER,  INTENT(OUT) :: NCORE
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: NW2 = 2*NNNW
!   Hash table parameters for duplicate detection
      INTEGER, PARAMETER :: HASH_SIZE = 262144  ! 2^18
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER, DIMENSION(NNNW) :: IOCC
      INTEGER, DIMENSION(NW2)  :: IQSUB
      INTEGER, DIMENSION(NNNW) :: JX
!   Hash table for duplicate detection
      INTEGER, DIMENSION(HASH_SIZE) :: hash_table
      INTEGER, DIMENSION(:), ALLOCATABLE :: hash_chain
      INTEGER :: I
      INTEGER :: NCORP1, NPEEL, NPEEL2, J, NPJ, NAKJ, LENTH, NCFD, NREC &
         , IOS, IERR, LOC, NQS, NEWSIZ, ISPARC, NJX, IOC, IPTY, NQSN    &
         , NJXN, NPEELN, NOPEN, JLAST, ILAST, IOCCI, NKJI, IFULLI, NU   &
         , JSUB, IQT, NBEG, NEND, JXN, JPI, II, ITEMP, NCOREL
      LOGICAL :: EMPTY, FULL, is_duplicate
      CHARACTER          :: RECL
      CHARACTER(LEN=256) :: RECORD
      REAL(DOUBLE) :: start_time, end_time
!-----------------------------------------------
!
!   初始化计时和哈希表
      start_time = OMP_GET_WTIME()
      hash_table = 0
!
!   Entry message
!
      WRITE (6, *) 'Loading Configuration Symmetry List File (Optimized)...'
