!***********************************************************************
!                                                                      *
      SUBROUTINE SETDBG
!-----------------------------------------------
!                                                                      *
!   This subroutine sets the arrays that control debug printout from   *
!   the radial and angular modules of the GRASP92 suite.               *
!                                                                      *
!   Call(s) to: [LIB92]: GETYN, LENGTH, OPENFL.                        *
!                                                                      *
!   Written by Farid A Parpia               Last update: 24 Dec 1992   *
!                                                                      *
!***********************************************************************
!...Translated by Pacific-Sierra Research 77to90  4.3E  14:06:03   1/ 3/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  11/01/17
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      USE debug_C
      USE default_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      USE getyn_I
      USE openfl_I
      IMPLICIT NONE
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER   :: I, IERR
      LOGICAL   :: YES
      CHARACTER :: FILNAM*256, DEFNAM*11, FORM*11, STATUS*3
!-----------------------------------------------
!
!   Initialise the arrays that control the debug printout
!
      LDBPA = .FALSE.
!
      LDBPG = .FALSE.
!
      LDBPR = .FALSE.
!
      IF (NDEF == 0) RETURN

      WRITE (6, *) 'Generate debug printout?'
      YES = GETYN()
      IF (YES) THEN
!
!   The  .dbg  file is formatted; open it on unit 99
!
         DEFNAM = 'hfs92.dbg'
         FORM = 'FORMATTED'
         STATUS = 'NEW'
!
         WRITE (6, *) 'File  hfs92.dbg  will be created as the'
         WRITE (6, *) ' HFS92 DeBuG Printout File; enter another'
         WRITE (6, *) ' file name if this is not acceptable;'
         WRITE (6, *) ' null otherwise:'
         READ (*, '(A)') FILNAM
!
         IF (LEN_TRIM(FILNAM) == 0) FILNAM = DEFNAM
!
    4    CONTINUE
         CALL OPENFL (99, FILNAM, FORM, STATUS, IERR)
         IF (IERR /= 0) THEN
    5       CONTINUE
            WRITE (6, *) 'Enter a name for the HFS92 DeBuG Printout'
            WRITE (6, *) ' file that is to be created:'
            READ (*, '(A)') FILNAM
            IF (LEN_TRIM(FILNAM) == 0) GO TO 5
            GO TO 4
         ENDIF
!
!   Set options for general printout
!
         WRITE (6, *) ' Print out the machine constants used?'
         YES = GETYN()
         IF (YES) LDBPG(1) = .TRUE.
         WRITE (6, *) ' Print out the physical constants used?'
         YES = GETYN()
         IF (YES) LDBPG(2) = .TRUE.
!
!   Set options for radial modules
!
         WRITE (6, *) ' Printout from radial modules?'
         YES = GETYN()
         IF (YES) THEN
            WRITE (6, *) ' Printout from RADGRD?'
            YES = GETYN()
            IF (YES) LDBPR(1) = .TRUE.
            WRITE (6, *) ' Printout from NUCPOT?'
            YES = GETYN()
            IF (YES) LDBPR(2) = .TRUE.
            WRITE (6, *) ' Printout from LODRWF?'
            YES = GETYN()
            IF (YES) LDBPR(3) = .TRUE.
!
         ENDIF
!
!   Set options for angular modules
!
         WRITE (6, *) ' Printout from angular modules?'
         YES = GETYN()
         IF (YES) THEN
            WRITE (6, *) ' Printout from LODCSL?'
            YES = GETYN()
            IF (YES) LDBPA(1) = .TRUE.
            WRITE (6, *) ' Print out T coefficients?'
            YES = GETYN()
            IF (YES) LDBPA(2) = .TRUE.
         ENDIF
!
      ENDIF
!
      RETURN
      END SUBROUTINE SETDBG
