!
!***********************************************************************
!                                                                      *
      MODULE m_C
      USE vast_kind_param, ONLY:  DOUBLE
      USE parameter_def,   ONLY:  NNNW
!...Created by Pacific-Sierra Research 77to90  4.3E  06:33:54  12/28/06
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!...Modified for OpenMP thread safety     December 2024
      INTEGER, DIMENSION(NNNW) :: NQ1, NQ2
      INTEGER, DIMENSION(NNNW) :: JJC1, JJC2
      INTEGER, DIMENSION(3,NNNW) :: JJQ1, JJQ2
      INTEGER, DIMENSION(NNNW) :: JLIST, KLIST
      INTEGER :: NPEEL, NCORE
!
!   Make global variables thread-private for OpenMP safety
!$OMP THREADPRIVATE(NQ1, NQ2, JJC1, JJC2, JJQ1, JJQ2, JLIST, KLIST, NPEEL, NCORE)
!
      END MODULE m_C
