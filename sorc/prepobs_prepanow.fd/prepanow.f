C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C  
C MAIN PROGRAM:  PREPOBS_PREPANOW
C   PRGMMR: Whiting          ORG: EMC         DATE: 2014-10-17
C
C ABSTRACT: REPROCESSES THE DUMP OF "AIRNOW" OZONE CONCENTRATION AND 
C   PARTICULATE MATTER (PM) DATA INTO A PREPBUFR LOOK-A-LIKE FILE.  USED  
C   BY VERIFICATION CODES TO GENERATE A "PREPFITS" COMPARISON FILES.
C
C PROGRAM HISTORY LOG:
C 2004-09-12  D. A. KEYSER -- ORIGINAL AUTHOR
C 2005-02-02  D. A. KEYSER -- MODIFIED TO HANDLE DUMP FILES CONTAINING
C            NEW, NOT-YET-OPERATIONAL, SEPARATE 1-HOUR AND 8-HOUR
C            BACKWARD-AVERAGED DATA WITH MESSAGE TYPES NC008021 AND
C            NC008022, RESPECTIVELY, IT STILL PROPERLY HANDLES THE
C            OPERATIONAL DUMP FILES CONTAINING COMBINED 1-HOUR AND 8-
C            HOUR FORWARD-AVERAGED DATA WITH MESSAGE TYPE NC008020
C 2012-03-14  D. A. KEYSER - IN RESPONSE TO THE LATEST VERSION OF
C            BUFRLIB WHICH CAN HANDLE EMBEDDED DICTIONARY MESSAGES::
C            INCREASES DEGREE OF BUFRLIB PRINTOUT SUCH THAT CODE WILL
C            PRINT A DIAGNOSTIC IF ANY EMBEDDED DICTIONARY MESSAGES ARE
C            FOUND WHEN READING IN MESSAGES; REPLACES SINGLE CALL TO
C            OPENMG AFTER OPENING INPUT AND OUTPUT BUFR FILES WITH CALL
C            TO OPENMB IN INNER DO-LOOP WHERE INFORMATION FROM EACH
C            SUBSET READ IN IS TRANSFERRED TO THE OUTPUT BUFR FILE,
C            THIS FIXES A BUG WHICH HAD CAUSED A BUFRLIB ABORT HERE
C            WHEN THE INPUT BUFR FILE CONTAINS EMBEDDED BUFR DICTIONARY
C            MESSAGES (NOT SURE WHY!); REPLACED CALL TO BUFRLIB ROUTINE
C            UFBSEQ WITH TWO CALLS TO UFBINT TO READ IN SUBSETS (UFBSEQ
C            ASSUMES A PRE-DEFINED ORDER TO PARAMETERS IN A SUBSET,
C            UFBINT DOES NOT)
C 2013-06-21 JWhiting - updated for use on WCOSS (linux) platforms:
C            updated doc block and w3tagb args; no logic changes.
C 2014-10-17 JWhiting - added Particulate Matter (PM) processing
C ------
C
C USAGE:
C   INPUT FILES:
C     UNIT 21  - "AIRNOW" BUFR DATA DUMP FILE
C     UNIT 25  - LOOK-A-LIKE PREPBUFR MNEMONIC BUFRTABLE (FOR ONLY
C                AIRNOW DATA)
C     UNIT 30  - CENTRAL DUMP AND PREPBUFR PROCESSING DATE IN FORM
C                YYYYMMDDHH
C
C   OUTPUT FILES: 
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - LOOK-A-LIKE PREPBUFR FILE CONTAINING ONLY AIRNOW DATA
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       W3NCO    - W3TAGB   W3DIFDAT W3TAGE
C       BUFRLIB  - DATELEN  MESGBC   OPENBF   OPENMB   UFBINT
C                  WRITSB   CLOSBF
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          >   0 - ABNORMAL TERMINATION
C
C REMARKS:
C
C   INPUT REPORT STRUCTURE FROM AIRNOW BUFR DATA DUMP FILE (RDATA_8):
C
C    FOR MESSAGE TYPE NC008020 WHICH CONTAINS 1- AND 8-HOUR FORWARD
C     AVERAGED DATA IN A SINGLE SUBSET:
C
C      WORD    MNEMONIC
C      ----    --------
C        1       YEAR  \
C        2       MNTH  - starting time from which ozone concentration
C        3       DAYS  - average is caluculated
C        4       HOUR  /
C        5       PTID
C        6       CLATH
C        7       CLONH
C        8       TYPO
C        9       TSIG
C       10       TPHR  - 1-hour average follows
C       11       QCIND    - q.c. indicator
C       12       COPO     - ozone concentration
C       13       TPHR  - 8-hour average follows
C       14       QCIND    - q.c. indicator
C       15       COPO     - ozone concentration
C
C   OUTPUT REPORT STRUCTURE IN AIRNOW LOOK-A-LIKE PREPBUFR FILE:
C      WORD    MNEMONIC
C      ----    --------
C        1       SID
C        2       XOB
C        3       YOB
C        4       DHR   - Observation time (i.e., starting time
C                        from which average is caluclated) minus
C                        cycle (central dump/PREPBUFR) time
C        5       TYP   - PREPBUFR REPORT TYPE (101)
C        6       T29   - INPUT DUMP REPORT TYPE (591)
C        7       SQN
C        8       PROCN - Process number (for MPI) - always 0
C                        (program does not run in "pieces")
C        9       RPT
C       10       CAT   - PREPBUFR data level category (always 6)
C       11       TYPO
C       12       TSIG
C       13       TPHR  - 1-hour average follows
C       14       QCIND    - q.c. indicator
C       15       COPO     - ozone concentration
C       16       TPHR  - 8-hour average follows
C       17       QCIND    - q.c. indicator
C       18       COPO     - ozone concentration
C
C
C    FOR MESSAGE TYPE NC008021 WHICH CONTAINS ONLY 1-HOUR BACKWARD
C     AVERAGED DATA IN A SINGLE SUBSET AND MESSAGE TYPE NC008022 WHICH
C     CONTAINS ONLY 8-HOUR BACKWARD AVERAGED DATA IN A SINGLE SUBSET:
C
C      WORD    MNEMONIC
C      ----    --------
C        1       YEAR  \
C        2       MNTH  - ending time from which ozone concentration
C        3       DAYS  - average is caluculated
C        4       HOUR  /
C        5       PTID
C        6       CLATH
C        7       CLONH
C        8       TYPO
C        9       TSIG
C       10       TPHR  - 1-hour or 8-hour average follows
C       11       QCIND    - q.c. indicator
C       12       COPO     - ozone concentration
C
C   OUTPUT REPORT STRUCTURE IN AIRNOW LOOK-A-LIKE PREPBUFR FILE:
C      WORD    MNEMONIC
C      ----    --------
C        1       SID
C        2       XOB
C        3       YOB
C        4       DHR   - Observation time (i.e., ending time
C                        from which average is caluclated) minus
C                        cycle (central dump/PREPBUFR) time
C        5       TYP   - PREPBUFR REPORT TYPE (101)
C        6       T29   - INPUT DUMP REPORT TYPE (591)
C        7       SQN
C        8       PROCN - Process number (for MPI) - always 0
C                        (program does not run in "pieces")
C        9       RPT
C       10       CAT   - PREPBUFR data level category (always 6)
C       11       TYPO
C       12       TSIG
C       13       TPHR  - 1-hour or 8-hour average follows
C       14       QCIND    - q.c. indicator
C       15       COPO     - ozone concentration
C
C
C    FOR MESSAGE TYPE NC008031 WHICH CONTAINS ONLY 1-HOUR BACKWARD
C     AVERAGED DAILY PARTICULATE MATTER DATA IN A SINGLE SUBSET:
C
C      WORD    MNEMONIC
C      ----    --------
C      1-11      same as for the ozone message types above
C       12       COPOPM   - Concentration of Pollutant
C
C   OUTPUT REPORT STRUCTURE IN AIRNOW LOOK-A-LIKE PREPBUFR FILE:
C      WORD    MNEMONIC
C      ----    --------
C      1-15      same as for the ozone message types above
C       16       TPHR  - 1-hour or 8-hour average follows
C       17       QCIND    - q.c. indicator
C       18       COPOPM   - Concentration of Pollutant
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM CCS
C
C$$$
      PROGRAM PREPOBS_PREPANOW

      CHARACTER*8 SUBOUT,SID,SUBSET, SUBPM
      REAL(8)     RDATA_8(15),RID,DHR,RPT,T29,TYP,CAT,PROCN,SQN
      REAL(8)     PDATA_8(3,2)

      real(8)     rd7_8   ! jaw db debug
      character*3 s3      ! jaw db debug

      DIMENSION   IDAT(8),JDAT(8),RINC(5)

      EQUIVALENCE (RID,SID),(RDATA_8(10),PDATA_8(1,1))

      DATA  SUBOUT/'AIRNOW  '/,IREP/0/, SUBPM/'ANOWPM  '/
      DATA  LUBFI/21/,LINDX/25/,LDATE/30/,LUBFO/51/,LUBPM/52/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('PREPOBS_PREPANOW',2014,0290,0050,'EMC')

      print *
      print * ,'---> Welcome to PREPOBS_PREPANOW - Version 10-17-2014'
      print *

      READ(LDATE,'(I10)') IDATE

      CALL DATELEN(10)

      CALL MESGBC(LUBFI,MSGT,ICOMP)
      IF(ICOMP.EQ.1) THEN
         PRINT'(/"INPUT BUFR FILE MESSAGES   C O M P R E S S E D"/'//
     .    '"FIRST MESSAGE TYPE FOUND IS",I5/)', MSGT
      ELSE  IF(ICOMP.EQ.0) THEN
         PRINT'(/"INPUT BUFR FILE MESSAGES   '//
     .    'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",I5/)',
     .    MSGT
      ELSE IF(ICOMP.EQ.-1)  THEN
         PRINT'(//"ERROR READING INPUT BUFR FILE - MESSAGE '//
     .    'COMPRESSION UNKNOWN"/)'
      ELSE  IF(ICOMP.EQ.-3)  THEN
         PRINT'(/"INPUT BUFR FILE DOES NOT EXIST"/)'
      ELSE  IF(ICOMP.EQ.-2)  THEN
         PRINT'(/"INPUT BUFR FILE HAS NO DATA MESSAGES"/"FIRST '//
     .    'MESSAGE TYPE FOUND IS",I5/)', MSGT
      ENDIF

      CALL OPENBF(LUBFI,'IN',LUBFI)
      CALL OPENBF(0,'QUIET',1) ! incr. degree of diag. prnt from BUFRLIB

      CALL OPENBF(LUBFO,'OUT',LINDX)       ! Ozone output
      CALL OPENBF(LUBPM,'OUT',LINDX)       ! PM output

      IDAT    = 0
      IDAT(1) = IDATE/1000000
      IDAT(2) = MOD((IDATE/10000),100)
      IDAT(3) = MOD((IDATE/100),100)
      IDAT(5) = MOD(IDATE,100)

      nmsg=0   ! debug
      nsb=0    ! debug
      DO WHILE(IREADMG(LUBFI,SUBSET,JDATE).EQ.0)
        if ( nmsub(lubfi).ne.0 ) nmsg=nmsg+1    ! debug
        npr=0    ! debug

        DO WHILE(IREADSB(LUBFI).EQ.0)
          npr=npr+1    ! debug
          nsb=nsb+1    ! debug

cdak      CALL UFBSEQ(LUBFI,RDATA_8,15,1,IRET,SUBSET)
          CALL UFBINT(LUBFI,RDATA_8, 9,1,ILVL,
     .              'YEAR MNTH DAYS HOUR PTID CLATH CLONH TYPO TSIG')
          IF (SUBSET.NE.'NC008031') THEN 
            CALL UFBINT(LUBFI,PDATA_8,3,2,ILVL,'TPHR QCIND COPO')
          ELSE
            CALL UFBINT(LUBFI,PDATA_8,3,1,ILVL,'TPHR QCIND COPOPM')
          ENDIF ! SUBSET NOT PM (NC008031)

cpppppppppp
cc        print 100, ilvl,rdata_8
cc100 format(1x,'ILVL=',I3,'; RPTIN:',4F6.0,F11.0,2F8.2,2F4.0,2(2F4.0,
cc   . F13.9))
cpppppppppp

C  CONVERT REPORT TO PREPBUFR
C  --------------------------

          IF (SUBSET.NE.'NC008031') THEN 
            LUN = LUBFO                       ! Ozone output logical unit
            CALL OPENMB(LUBFO,SUBOUT,IDATE)
          ELSE
            LUN = LUBPM                       ! PM output logical unit
            CALL OPENMB(LUBPM,SUBPM,IDATE)
          ENDIF ! SUBSET NOT PM (NC008031)

c   Convert PLATFORM TRANSMITTER ID NUMBER to STNID (change dec to hex)
c   -------------------------------------------------------------------

          WRITE(SID,'(Z8.8)') NINT(RDATA_8(5))
          CALL UFBINT(LUN  ,RID,1,1,IRET,'SID')

c YEAR MNTH DAYS HOUR PTID CLATH CLONH TYPO TSIG TPHR QCIND COPO[PM] 
      if (npr.le.5 .and. .true.) then 
        s3=subset(6:8)
        write(*,101) 'db: ',s3,nmsg,nsb,(int(rdata_8(i)),i=1,4)
        write(*,102) int(rdata_8(5)), sid
      endif ! npr <=5

c   Convert REPORT DATE to OBSERVATION TIME MINUS CYCLE TIME and to
c    observation time
c   ---------------------------------------------------------------

          JDAT      = 0
          JDAT(1:3) = RDATA_8(1:3)
          JDAT(5)   = RDATA_8(4)
          CALL W3DIFDAT(JDAT,IDAT,2,RINC)
          DHR = RINC(2)
          CALL UFBINT(LUN  ,DHR,1,1,IRET,'DHR')
          RPT =  RDATA_8(4)
          CALL UFBINT(LUN  ,RPT,1,1,IRET,'RPT')

c   Convert HIGH-RES LAT/LON to LOW-RES LAT/LON (LON from -W, +E to E)
c   -----------------------------------------------------------------

      rd7_8 = rdata_8(7)  ! debug

          XOB = RDATA_8(7)
          RDATA_8(7) = 360. - MOD(720.-XOB,360.)
          IF(RDATA_8(7).EQ.360.0)  RDATA_8(7) = 0.0
          CALL UFBINT(LUN  ,RDATA_8(6),2,1,IRET,'YOB XOB')

c   Set DUMP report type & PREPBUFR report type
c         DUMP report type:  ozone = 591   particulate = 592
c     PREPBUFR report type:  ozone = 101   particulate = 102
c   For all types, set Category, Process number and Sequence number:
c             Category = 6        Process number = 0
c      Sequence number = next report number
c   ------------------------------------------------------------

          IF (SUBSET.NE.'NC008031') THEN 
            T29 = 591                         ! Ozone dump rpt type
            TYP = 101                         ! Ozone prepbufr rpt type
          ELSE
            T29 = 592                         ! PM dump rpt type
            TYP = 102                         ! PM prepbufr rpt type
          ENDIF ! SUBSET NOT PM (NC008031)

          CAT = 6                             ! Category
          PROCN = 0                           ! Process number
          IREP = IREP + 1 
          SQN  = IREP                         ! Sequence number

          CALL UFBINT(LUN  ,T29,1,1,IRET,'T29')
          CALL UFBINT(LUN  ,TYP,1,1,IRET,'TYP')
          CALL UFBINT(LUN  ,CAT,1,1,IRET,'CAT')
          CALL UFBINT(LUN  ,PROCN,1,1,IRET,'PROCN')
          CALL UFBINT(LUN  ,SQN,1,1,IRET,'SQN')


c  Finally encode the observational data in PREPBUFR
c  -------------------------------------------------

          CALL UFBINT(LUN  ,RDATA_8(8),2,1,IRET,'TYPO TSIG')
          IF(SUBSET.EQ.'NC008020')  THEN
             NLEV = 2
          ELSE
             NLEV = 1
          ENDIF

          IF (SUBSET.NE.'NC008031') THEN 
            CALL UFBINT(LUBFO,PDATA_8,3,NLEV,IRET,'TPHR QCIND COPO')
          ELSE
            CALL UFBINT(LUBPM,PDATA_8,3,NLEV,IRET,'TPHR QCIND COPOPM')
          ENDIF ! NOT PM MESSAGE TYPE (NC008031)

          CALL WRITSB(LUN  )

      if (npr.le.5) then 
        write(*,103) rdata_8(6), rd7_8, rdata_8(7)
        write(*,104) (int(rdata_8(j)),j=8,11)
        write(*,105) rdata_8(12)*1d9
      endif ! npr <= 5
  101 format(a4,1x,a3,i4,i6,1x,i4,2i2.2,".",i2.2, $) ! s3, mg,sb,ymd.h
  102 format( 1x,i9, 1x,a8, $)                       ! ptid, sid
  103 format( 1x,f10.5,1x,f9.5, 1x,f7.2, $)          ! lat, lon, xob
  104 format( 4(1x,i2), $)                           ! typo,tsig,tphr,qcind
  105 format( 1x,f6.2)                               ! copo | copopm

        END DO ! WHILE IREADSB
      END DO ! WHILE IREADMG

      CALL CLOSBF(LUBFI)
      CALL CLOSBF(LUBFO)
      CALL CLOSBF(LUBPM)
            
      CALL W3TAGE('PREPOBS_PREPANOW')
      STOP
      END

