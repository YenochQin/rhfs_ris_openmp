      MODULE dumx_C
      use cons_C
!...Created by Pacific-Sierra Research 77to90  4.3E  10:50:24   2/14/04
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
!...Modified for OpenMP thread safety     December 2024
      USE parameter_def,   ONLY:  NNNW
      INTEGER, DIMENSION(NNNW) :: JLIS, JC1S, JC2S
!
!   Make global variables thread-private for OpenMP safety
!$OMP THREADPRIVATE(JLIS, JC1S, JC2S)
!
      END MODULE dumx_C
