c tideg.f
c  process tide gauge data for MODSBUFR
c
c modification history
c   6 Feb 2013  producion implementation
c  22 Feb 2019  bug fix: trapped case of single missing tide time 
c                 field; NRPT_NCO added to arg list to support printout
c               added ibfms() checks on missing values; rm'd BMISS.
c---
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE TIDEG(LUBFR,LUBFO,SUBSET,IDATE,NRPT,NRPT_NCO)
 
      COMMON /MNEMONIC/ NEMOS(10),NEMS,COMPRESS
 
      CHARACTER*80 NEMOS
      CHARACTER*8  SUBSET
      LOGICAL      COMPRESS,NOTIME
      REAL*8             CLONH,CLATH,ADATE,DATE(5)
      REAL*8       TIMEP(2),TIDES(2,255),ARR(10,255)
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  READ THE TIME PERIOD INFORMATION AND HIGH RES LON/LAT
C  -----------------------------------------------------
 
      CALL UFBINT(LUBFR,TIMEP,2,  1,IRET,'TPMI  TIMI')
      CALL UFBINT(LUBFR,TIDES,2,255,NTID,'TIDER TERC')
      CALL CLONLAT(LUBFR,CLONH,CLATH)
 
C  CHECK IF THERE IS A TIME FRAME HERE AND SETUP FIRST IDATE
C  ---------------------------------------------------------
 
      NOTIME = ibfms(TIMEP(1)).NE.0.AND.ibfms(TIMEP(2)).NE.0 ! both missing

      IF(NOTIME.AND.NTID.GT.1) THEN
         PRINT*,'>>>>>TIDEG - NO TIMES FOR MULTIPLE READINGS<<<<<'
      ENDIF

      IF(NOTIME) THEN          ! both timep() fields missing
         ADATE = IDATE
         DHR = 0
      ELSE                     ! at least 1 timep() fld non-missing

c --- trap case where only one of the time field is missing
         if ( ibfms(timep(1)).ne.0.or.ibfms(timep(2)).ne.0 ) then
           write(*,*) 'bad tide times: ',timep,notime, NRPT_NCO
         endif
         if ( ibfms(timep(1)).ne.0 ) timep(1)=0.d0
         if ( ibfms(timep(2)).ne.0 ) timep(2)=0.d0

         ADATE = IDATE
         DHR = (TIMEP(1)+TIMEP(2))/60.
         CALL RADDATE(ADATE,DHR,ADATE)
         DHR = TIMEP(2)/60.
      ENDIF
 

C  CREATE TIDE GAUGE REPORTS
C  -------------------------
 
      DO M=1,MAX(NTID,1)
 
      IF(ibfms(TIDES(1,M)).eq.0.OR.ibfms(TIDES(2,M)).eq.0.OR.M.EQ.1)THEN
         JDATE = ADATE
         DATE(1) = JDATE/1000000
         DATE(2) = MOD(JDATE/10000,100)
         DATE(3) = MOD(JDATE/100  ,100)
         DATE(4) = MOD(JDATE      ,100)
         DATE(5) = (ADATE-JDATE)*60.          

         CALL OPENMB(LUBFO,SUBSET,IDATE)
 
         CALL UFBINT(LUBFO,CLONH,1,1,IRET,'CLONH')
         CALL UFBINT(LUBFO,CLATH,1,1,IRET,'CLATH')

         CALL UFBINT(LUBFO,DATE,5,1,IRET,'YEAR MNTH DAYS HOUR MINU')
         nrpt = nrpt + 1
 
         DO N=1,NEMS
         CALL UFBINT(LUBFR,ARR,10,255,NRET,NEMOS(N))
         IF(NRET.GT.0) CALL UFBINT(LUBFO,ARR,10,NRET,IRET,NEMOS(N))
         ENDDO
 
         CALL UFBINT(LUBFO,TIDES(1,M),2,1,IRET,'TIDER TERC')
 
         CALL RADDATE(ADATE,DHR,ADATE)  ! APPLY TIME INCREMENT
      ENDIF
 
c Disable compression (due to problem porting to Cirrus; fails on wrcmps)
c  (this should be revisited after update to Cirrus BLib)
ccc   IF(.NOT.COMPRESS) CALL WRITSB(LUBFO)
ccc   IF(     COMPRESS) CALL WRITCP(LUBFO)
      CALL WRITSB(LUBFO)
 
 
      ENDDO
 
      RETURN
      END
