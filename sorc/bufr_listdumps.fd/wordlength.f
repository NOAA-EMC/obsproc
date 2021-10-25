C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    WORDLENGTH
C   PRGMMR: KEYSER           ORG: NP22       DATE: 2013-02-14
C
C ABSTRACT: DETERMINES THE NUMBER OF BYTES IN A FULL WORD, BOTH
C   INTEGER AND REAL, FOR THE PARTICULAR MACHINE.
C
C PROGRAM HISTORY LOG:
C 1999-08-04  KEYSER      ADAPTED FROM W3LIB ROUTINE W3FI01, BUT
C   MODIFIED TO RETURN WORD LENGTH FOR REALS AS WELL AS INTEGERS
C   (W3FI01 RETURNED WORD LENGTH ONLY FOR INTEGERS) - THE IBM SP
C   ALLOWS REAL WORD LENGTH TO BE DIFFERENT FROM INTEGER WORD LENGTH
C 2012-11-30  J. WOOLLEN  INITIAL PORT TO WCOSS 
C 2013-02-14  D. A. KEYSER -- FINAL CHANGES TO RUN ON WCOSS
C
C USAGE:    CALL WORDLENGTH(LWI,LWR)
C   OUTPUT ARGUMENT LIST:
C     LWI      - MACHINE WORD LENGTH FOR INTEGERS (IN BYTES)
C     LWR      - MACHINE WORD LENGTH FOR REALS (IN BYTES)
C
C REMARKS: SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE WORDLENGTH(LWI,LWR)

      CHARACTER*8  CTEST1,CTEST2,CTEST3,CTEST4

      INTEGER      ITEST1,ITEST2

      REAL         RTEST3,RTEST4

      EQUIVALENCE  (CTEST1,ITEST1),(CTEST2,ITEST2)

      EQUIVALENCE  (CTEST3,RTEST3),(CTEST4,RTEST4)

      DATA  CTEST1/'12345678'/,CTEST3/'12345678'/

      ITEST2 = ITEST1
      IF (CTEST1 .EQ. CTEST2) THEN
        LWI = 8
      ELSE
        LWI = 4
      END IF

      RTEST4 = RTEST3
      IF (CTEST3 .EQ. CTEST4) THEN
        LWR = 8
      ELSE
        LWR = 4
      END IF

      RETURN
      END
