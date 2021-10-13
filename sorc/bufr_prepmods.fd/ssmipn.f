C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE SSMIPN(LUBFR,LUBFO,SUBSET,IDATE,NRPT)
 
      CHARACTER*8 SUBSET
      REAL*8      DATE(6),REGI(3),SLCA(4,255),SNNP(4,255)
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  READ THE SSMI REGISTRATION AND SCAN LINE REPORT
C  -----------------------------------------------
 
      CALL UFBINT(LUBFR,DATE,6,1,IRET,'YEAR MNTH DAYS HOUR MINU SECO')
      CALL UFBINT(LUBFR,REGI,3,1,IRET,'SAID ORBN SCNN')
 
      CALL UFBINT(LUBFR,SLCA,4,255,NSC1,'CLAT CLON SFTG POSN')
      CALL UFBINT(LUBFR,SNNP,4,255,NSC2,'WSOS PH2O CH2O SST1')

 
C  KEEP EACH SCAN IN ONE HOUR OF ONE DAY EVEN IF SECO IS ADJUSTED
C  --------------------------------------------------------------
 
      CALL OPENMB(LUBFO,SUBSET,IDATE)
 
C  GENERATE A REPORT FOR EACH SCAN POINT
C  -------------------------------------
 
      DO N=1,MIN(NSC1,NSC2)
 
      CALL UFBINT(LUBFO,DATE,6,1,IRET,'YEAR MNTH DAYS HOUR MINU SECO')
      nrpt = nrpt + 1
      CALL UFBINT(LUBFO,REGI,3,1,IRET,'SAID ORBN SCNN')
 
      CALL UFBINT(LUBFO,SLCA(1,N),4,1,IRET,'CLATH CLONH SFTG POSN')
      CALL UFBINT(LUBFO,SNNP(1,N),4,1,IRET,'WSOS  PH2O  CH2O SST1')
 
c Disable compression (due to problem porting to Cirrus; fails on wrcmps)
c  (this should be revisited after update to Cirrus BLib)
ccc   CALL WRITCP(LUBFO)
      CALL WRITSB(LUBFO)
 
      ENDDO
 
      RETURN
      END
