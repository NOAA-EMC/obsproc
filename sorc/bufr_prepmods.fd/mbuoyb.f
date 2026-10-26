c mbuoyb.f
c  process BUFR format moored buoy data for MODSBUFR
c
c modification history
c  19 Oct 2021  production implementation
c---
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE MBUOYB(LUBFR,LUBFO,SUBSET,IDATE,NRPT)
 
c      implicit none

      COMMON /MNEMONIC/ NEMOS(10),NEMS,COMPRESS
 
      CHARACTER*80 NEMOS
      CHARACTER*8  SUBSET
      LOGICAL      COMPRESS,NOTIME
      REAL*8             CLONH,CLATH,ADATE,DATE(5)
      REAL*8       BBYSTSL(3,255),ARR(10,255)
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  READ THE TIME PERIOD INFORMATION AND HIGH RES LON/LAT
C  -----------------------------------------------------
 
c      CALL CLONLAT(LUBFR,CLONH,CLATH)
 
C  CHECK IF THERE IS A TIME FRAME HERE AND SETUP FIRST IDATE
C  ---------------------------------------------------------
 

 

C  CREATE TIDE GAUGE REPORTS
C  -------------------------

      ADATE = IDATE
      JDATE = ADATE
c      DATE(1) = JDATE/1000000
c      DATE(2) = MOD(JDATE/10000,100)
c      DATE(3) = MOD(JDATE/100  ,100)
c      DATE(4) = MOD(JDATE      ,100)
c      DATE(5) = (ADATE-JDATE)*60.          

      CALL OPENMB(LUBFO,SUBSET,IDATE)
 
c      CALL UFBINT(LUBFO,CLONH,1,1,IRET,'CLONH')
c      CALL UFBINT(LUBFO,CLATH,1,1,IRET,'CLATH')

      CALL UFBINT(LUBFR,DATE,5,1,IRET,'YEAR MNTH DAYS HOUR MINU')
      CALL UFBINT(LUBFO,DATE,5,1,IRET,'YEAR MNTH DAYS HOUR MINU')
 
      DO N=1,NEMS
         CALL UFBINT(LUBFR,ARR,10,255,NRET,NEMOS(N))
         IF(NRET.GT.0) CALL UFBINT(LUBFO,ARR,10,NRET,IRET,NEMOS(N))
      ENDDO
 
      CALL UFBSEQ(LUBFR,BBYSTSL,3,255,IRET,'BBYSTSL')
      NLVL = 0
      DO I=1,255
         IF (BBYSTSL(1,I) .LT. 9999.) THEN
            NLVL = NLVL + 1
         ENDIF
      ENDDO
      IF (NLVL .EQ. 0) THEN
         DO I=1,3
            BBYSTSL(I,1) = 1.0E+10
         ENDDO
c         NLVL = 1
      ENDIF
      CALL DRFINI(LUBFO,NLVL,1,'{BBYSTSL}')
c      if (NLVL .gt. 1) then
c         NLVL = 1
c      endif
      IRET = NLVL
      CALL UFBSEQ(LUBFO,BBYSTSL,3,NLVL,IRET,'BBYSTSL')
 
 
c Disable compression (due to problem porting to Cirrus; fails on wrcmps)
c  (this should be revisited after update to Cirrus BLib)
ccc   IF(.NOT.COMPRESS) CALL WRITSB(LUBFO)
ccc   IF(     COMPRESS) CALL WRITCP(LUBFO)
      CALL WRITSB(LUBFO)

      NRPT = 1
  
      RETURN
      END
