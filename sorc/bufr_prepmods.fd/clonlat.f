C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE CLONLAT(LUBFR,CLONH,CLATH)
 
      REAL*8 BMISS,CLONH,CLATH,CLL(4)
      DATA   BMISS/10e10/
 
      CALL UFBINT(LUBFR,CLL,4,1,IRET,'CLON CLONH CLAT CLATH')
      IF(CLL(1).LT.BMISS) CLONH = CLL(1)
      IF(CLL(2).LT.BMISS) CLONH = CLL(2)
      IF(CLL(3).LT.BMISS) CLATH = CLL(3)
      IF(CLL(4).LT.BMISS) CLATH = CLL(4)
 
      RETURN
      END
