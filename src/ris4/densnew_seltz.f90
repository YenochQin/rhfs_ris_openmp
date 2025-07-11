!***********************************************************************
!                                                                      *
!JE   SUBROUTINE DENSNEW(DOIT,DINT1,DINT2,DINT3,DINT4,DINT5,DINT6,DINT7)
       SUBROUTINE DENSNEW_SELTZ(DOIT,DINT1,DINT2,DINT3,                &
                          DINT4,DINT5,DINT6,DINT7,                     &
                          DINT1VEC,DENS1VEC,NRNUC)
!                                                                      *
!   IF angular coefficients must be calculated                         *
!   This routine controls combines the radial and angular parts for the*
!   calculation of the NMS parameter, the electron density at the      *
!   origin and radial expectation values.
!   Modified to support OpenMP parallelization                         *
!                                                                      *
!   Call(s) to: [LIB92]: ALCBUF, CONVRT, GETYN                         *
!                        ITJPO, ONESCALAR                              *
!                                                                      *
!   Written by Per Jonsson                                             *
!                                                                      *
!                                         Last revision: 10 Nov 1995   *
!                                                                      *
!   Modified by C. Naz\'e  Feb. 2012                                   *
!   Modified by J. Ekman   Nov. 2013                                   *
!   Modified for OpenMP parallelization   Last revision:    Dec 2024   *
!                                                                      *
!***********************************************************************
!...Translated by Gediminas Gaigalas 11/18/19
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE vast_kind_param,  ONLY: DOUBLE
      USE parameter_def,    ONLY: KEYORB, NNNW, NNNP
      USE blk_C
      USE debug_C
      USE decide_C
      USE DEF_C
      USE eigv_C
      USE foparm_C
      USE grid_C
      USE JLABL_C
      USE npar_C
      USE orb_C
      USE prnt_C
      USE TEILST_C
      USE BUFFER_C
      USE ris_C
      USE syma_C
      USE prnt_C,           ONLY : NVEC
!-----------------------------------------------
!   OpenMP module for parallel processing
!-----------------------------------------------
      USE OMP_LIB
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      USE alcbuf_I
      USE convrt_I
      USE getyn_I
      USE itjpo_I
      USE onescalar_I
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      REAL(DOUBLE), DIMENSION(NNNW,NNNW), INTENT(IN) :: DINT1, DINT2, &
                                                        DINT3, DINT4, &
                                                        DINT5, DINT6, &
                                                        DINT7
      REAL(DOUBLE), DIMENSION(NVEC,NRNUC), INTENT(OUT)     :: DENS1VEC !  JE ADD
      REAL(DOUBLE), DIMENSION(NNNW,NNNW,NRNUC), INTENT(IN) :: DINT1VEC !  JE ADD
      INTEGER, INTENT(IN) :: DOIT
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
      INTEGER, PARAMETER :: KEY = KEYORB
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      REAL(DOUBLE), DIMENSION(NNNW) :: TSHELL, TSHELL_S
      REAL(DOUBLE), DIMENSION(NRNUC) :: CONTRI1VEC, ELEMNT1VEC
      REAL(DOUBLE), DIMENSION(:), pointer :: EMT1, EMT2, EMT3, EMT4,   &
                                             EMT5, EMT6
      REAL(DOUBLE) :: ELEMNT1, ELEMNT2, ELEMNT3, ELEMNT4, ELEMNT5,     &
                      ELEMNT6, ELEMNT7, CONTRI1, CONTRI2, CONTRI3,     &
                      CONTRI4, CONTRI5, CONTRI6, CONTRI7
      LOGICAL :: VSH, NUCDE, SMSSH, YES
      CHARACTER :: CNUM*11, CK*2
      INTEGER, DIMENSION(NNNW) :: IA_S
      INTEGER :: KA, IOPAR, INCOR, IC, LCNUM, ITJPOC, IR, IA, IB, I, II, J
      Integer :: L, LOC, NCONTR, LAB, NRNUC
      INTEGER :: NUM_THREADS, THREAD_ID
!-----------------------------------------------
!
! DOIT: IF DOIT=1 angular coefficients will be stored after creation
! DINT1 contain the density
! DINT2 contain the uncorrected NMS parameter: K^1_NMS
! DINT3 contain the expect. value <r>
! DINT4 contain the expect. value <r2>
! DINT5 contain the expect. value <r-1>
! DINT6 contain the expect. value <r-2>
! DINT7 contain the sum of NMS parameters: K^1_NMS+K^2_NMS+K^3_NMS
!
      DENS1VEC(:,:) = 0.0D00                                     ! JE ADD
!
!   Set the rank (zero) and parity (even) for the one-particle
!   coefficients
!
      KA = 0
      IOPAR = 1
      INCOR = 1
!
!   Print OpenMP information
!
      NUM_THREADS = OMP_GET_MAX_THREADS()
      PRINT *, 'RIS4 DENSNEW_SELTZ: Using OpenMP with ', NUM_THREADS, ' threads'
!
!   Allocate storage for the arrays in BUFFER
!
      CALL ALCBUF (1)
!
!   Sweep through the Hamiltonian matrix to determine the
!   sms parameter
!   PARALLELIZED: Outer loop (IC) with careful synchronization
!
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(IC, IR, LCNUM, ITJPOC, IA, IB, TSHELL, &
!$OMP& ELEMNT1, ELEMNT2, ELEMNT3, ELEMNT4, ELEMNT5, ELEMNT6, ELEMNT7, ELEMNT1VEC, &
!$OMP& J, LOC, CONTRI1, CONTRI2, CONTRI3, CONTRI4, CONTRI5, CONTRI6, CONTRI7, &
!$OMP& CONTRI1VEC, NCONTR, TSHELL_S, IA_S, I, LAB, L)
!$OMP DO SCHEDULE(DYNAMIC)
         DO 13 IC = 1,NCF
!
!   Output IC on the screen to show how far the calculation has preceede
!   Only from master thread to avoid output conflicts
!
        CALL CONVRT (IC,CNUM,LCNUM)
        if (OMP_GET_THREAD_NUM() == 0 .AND. mod(IC,100).eq.0) then
          PRINT *, 'Column '//CNUM(1:LCNUM)//' complete;'
        end if
!
        ITJPOC = ITJPO (IC)

        DO 12 IR = IC,NCF
!
!   Matrix elements are diagonal in J
!
          IF (ITJPO(IR) .EQ. ITJPOC) THEN
!
!   Initialise the accumulator
!
            ELEMNT1 = 0.0D00
            ELEMNT2 = 0.0D00
            ELEMNT3 = 0.0D00
            ELEMNT4 = 0.0D00
            ELEMNT5 = 0.0D00
            ELEMNT6 = 0.0D00
            ELEMNT7 = 0.0D00
            ELEMNT1VEC(:) = 0.0D00                                      ! JE ADD
!
!   Call the MCT package to compute T coefficients
!   ONESCALAR now safe to call - global state is thread-private
!
           CALL ONESCALAR(IC,IR,IA,IB,TSHELL)
!GG            CALL TNSRJJ (KA,IOPAR,IC,IR,IA,IB,TSHELL)
            IF (IA .NE. 0) THEN
              IF (IA .EQ. IB) THEN
                    NCONTR = 0
                DO 8 IA = 1,NW
                  IF (ABS (TSHELL(IA)) .GT. CUTOFF) THEN
                    NCONTR = NCONTR + 1
                    TSHELL_S(NCONTR) = TSHELL(IA)
                    IA_S(NCONTR) = IA
                    ELEMNT1 = ELEMNT1 + DINT1(IA,IA)*TSHELL(IA)
                    DO 21 L = 2,NRNUC                                    ! JE ADD
                      ELEMNT1VEC(L) = ELEMNT1VEC(L)+DINT1VEC(IA,IA,L)*& ! JE ADD
                                      TSHELL(IA)                        ! JE ADD
   21               CONTINUE                                            ! JE ADD
                    ELEMNT2 = ELEMNT2 + DINT2(IA,IA)*TSHELL(IA)
                    ELEMNT3 = ELEMNT3 + DINT3(IA,IA)*TSHELL(IA)
                    ELEMNT4 = ELEMNT4 + DINT4(IA,IA)*TSHELL(IA)
                    ELEMNT5 = ELEMNT5 + DINT5(IA,IA)*TSHELL(IA)
                    ELEMNT6 = ELEMNT6 + DINT6(IA,IA)*TSHELL(IA)
                    ELEMNT7 = ELEMNT7 + DINT7(IA,IA)*TSHELL(IA)
                  ENDIF
    8           CONTINUE
!$OMP CRITICAL(FILE_WRITE)
                IF (DOIT.EQ.1) WRITE(50) IC,IR,NCONTR
                DO I = 1,NCONTR
                  LAB = IA_S(I)*(KEY + 1)
                  IF (DOIT.EQ.1) WRITE(50) TSHELL_S(I),LAB
                END DO
!$OMP END CRITICAL(FILE_WRITE)
              ELSE
                IF (ABS (TSHELL(1)) .GT. CUTOFF) THEN
                  IF (NAK(IA).EQ.NAK(IB)) THEN
!$OMP CRITICAL(FILE_WRITE)
                    IF (DOIT.EQ.1) WRITE(50) IC,IR,1
                    LAB = IA*KEY + IB
                    IF (DOIT.EQ.1) WRITE(50) TSHELL(1),LAB
!$OMP END CRITICAL(FILE_WRITE)
                    ELEMNT1 = ELEMNT1 + DINT1(IA,IB)*TSHELL(1)
                    DO 23 L = 2,NRNUC                                   ! JE ADD
                      ELEMNT1VEC(L) = ELEMNT1VEC(L)+DINT1VEC(IA,IB,L)*& ! JE ADD
                                      TSHELL(1)                         ! JE ADD
   23               CONTINUE                                            ! JE ADD
                    ELEMNT2 = ELEMNT2 + DINT2(IA,IB)*TSHELL(1)
                    ELEMNT3 = ELEMNT3 + DINT3(IA,IB)*TSHELL(1)
                    ELEMNT4 = ELEMNT4 + DINT4(IA,IB)*TSHELL(1)
                    ELEMNT5 = ELEMNT5 + DINT5(IA,IB)*TSHELL(1)
                    ELEMNT6 = ELEMNT6 + DINT6(IA,IB)*TSHELL(1)
                    ELEMNT7 = ELEMNT7 + DINT7(IA,IB)*TSHELL(1)
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
!   Critical section: Updating shared result arrays
!$OMP CRITICAL(RESULT_UPDATE)
            DO 9 J = 1,NVEC
              LOC = (J-1)*NCF
              CONTRI1 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT1
              CONTRI1VEC(:) = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT1VEC(:)   ! JE ADD
              CONTRI2 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT2
              CONTRI3 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT3
              CONTRI4 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT4
              CONTRI5 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT5
              CONTRI6 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT6
              CONTRI7 = EVEC(IC+LOC)*EVEC(IR+LOC)*ELEMNT7
              IF (IR.NE.IC) THEN
                CONTRI1 = 2.0D00 * CONTRI1
                CONTRI1VEC(:) = 2.0D00 * CONTRI1VEC(:)                  ! JE ADD
                CONTRI2 = 2.0D00 * CONTRI2
                CONTRI3 = 2.0D00 * CONTRI3
                CONTRI4 = 2.0D00 * CONTRI4
                CONTRI5 = 2.0D00 * CONTRI5
                CONTRI6 = 2.0D00 * CONTRI6
                CONTRI7 = 2.0D00 * CONTRI7
              ENDIF
              DENS1(J) = DENS1(J) + CONTRI1
              DO 22 L = 2,NRNUC                                         ! JE ADD
                DENS1VEC(J,L) = DENS1VEC(J,L) + CONTRI1VEC(L)           ! JE ADD
   22         CONTINUE                                                  ! JE ADD
              DENS2(J) = DENS2(J) + CONTRI2
              DENS3(J) = DENS3(J) + CONTRI3
              DENS4(J) = DENS4(J) + CONTRI4
              DENS5(J) = DENS5(J) + CONTRI5
              DENS6(J) = DENS6(J) + CONTRI6
              DENS7(J) = DENS7(J) + CONTRI7
    9       CONTINUE
!$OMP END CRITICAL(RESULT_UPDATE)
          ENDIF
   12   CONTINUE
   13 CONTINUE
!$OMP END DO
!$OMP END PARALLEL
      IF (DOIT.EQ.1) WRITE(50) -1
!
! Empty the buffer and close file
      IF (DOIT.EQ.1) CLOSE(50)
!
!   Deallocate storage for the arrays in BUFFER
      CALL ALCBUF (3)
      RETURN
      END SUBROUTINE DENSNEW_SELTZ
