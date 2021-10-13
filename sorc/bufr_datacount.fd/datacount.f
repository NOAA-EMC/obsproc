C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_DATACOUNT
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2017-01-10
C
C ABSTRACT: PREPARES THE NUMBER OF OBSERVATIONS DUMPED FROM THE BUFR
C   DATABASE FOR PRINTING.  THIS PRINTOUT IS USED BY THE SDM TO TRACK
C   THE RECEIPT OF DATA.  A CURRENT RECEIPT AVERAGE (NORMALLY OVER 
C   30-DAYS) FOR EACH BUFR TYPE AND SUBTYPE IS RETRIEVED AND COMPARED
C   TO THE CURRENT COUNT.  IF THE CURRENT COUNT IS LOW, A MESSAGE
C   IS WRITTEN TO UNIT 52.  THE SCRIPT CAN THEN POST THIS TO EITHER
C   THE ALERTLOG OR JOBLOG FILE (OR BOTH) IN ORDER TO NOTIFY THE SDM.
C   IN ADDITION, THIS PROGRAM WILL COMPLETE WITH RETURN CODE 4, 5 OR 6
C   DEPENDING ON THE SERIOUSNESS OF THE DEFICIENCY.  ALTERNATIVELY,
C   IF THE CURRENT COUNT IS HIGH, A MESSAGE IS WRITTEN TO UNIT 53.
C   THE SCRIPT CAN THEN POST THIS TO EITHER THE JOBLOG OR ALERTLOG
C   FILE (OR BOTH).  THIS ABUNDANCE IN A DATA TYPE WILL NOT CAUSE
C   THE PROGRAM TO COMPLETE WITH RETURN CODE 4, 5 OR 6 (THIS IS
C   RESERVED ONLY FOR LOW DATA COUNTS).  AN "UPDATED" STATUS TABLE IS
C   GENERATED CONTAINING BOTH THE COUNTS FOR EACH TYPE AND SUBTYPE
C   DUMPED AND THE CURRENT 30-DAY AVERAGE.  THOSE TYPE/SUBTYPES WHICH
C   ARE DEEMED TO BE EITHER DEFICIENT OR EXCESSIVE ARE NOTED IN THE
C   UPDATED STATUS FILE.  THE UPDATED STATUS FILE INCLUDES ONLY THOSE
C   BUFR TYPES/SUBTYPES WHICH WERE ACTUALLY DUMPED.
C           SECONDLY, THIS PROGRAM COMPARES THE CURRENT 30-DAY
C   RECEIPT AVERAGE FOR EACH BUFR TYPE/SUBTYPE DUMPED TO THE 30-DAY
C   AVERAGE FROM 3-, 6-, 9-, AND 12-MONTHS AGO.  IF THE CURRENT 30-
C   DAY AVERAGE IS ABOVE OR BELOW THE PAST 30-DAY AVERAGE BY A USER-
C   SPECIFED PERCENTAGE (DEFAULT IS 20%) FROM 3-, 6-, 9-, OR 12-MONTHS
C   AGO, A MESSAGE IS WRITTEN TO UNIT 54, 55, 56, OR 57, RESPECTIVELY.
C   THE SCRIPT CAN THEN POST THIS TO EITHER THE JOBLOG OR ALERTLOG
C   FILE (OR BOTH).  THIS SHOULD IDENTIFY LONGER-TERM TRENDS IN THE
C   DUMPS COUNTS (I.E, IF THE NUMBER OF RECEIPTS FOR A PARTICULAR
C   TYPE/SUBTYPE IS GRADUALLY DECREASING OVER TIME, THIS WILL BE
C   DETECTED IN THE 30-DAY AVERAGE RECEIPT COMPARISON; IT MAY NOT BE
C   DETECTED IN THE COMPARISON OF THE INDIVIDUAL DAY'S RECEIPT COUNT
C   TO THE CURRENT 30-DAY AVERAGE). IT SHOULD BE NOTED THAT EITHER A
C   HIGH OR LOW CURRENT AVERAGE IN A DATA TYPE WILL NOT CAUSE THE
C   PROGRAM TO COMPLETE WITH RETURN CODE 4, 5 OR 6 (THIS IS RESERVED
C   ONLY FOR LOW DATA COUNTS IN THE CURRENT RUN VS. THE CURRENT 30-DAY
C   AVERAGE).
C
C PROGRAM HISTORY LOG:
C 
C 1999-06-24  L, SAGER    ORIGINAL VERSION FOR IMPLEMENTATION
C 1999-07-24  L. SAGER    30-DAY AVERAGE FILE ADDED
C 1999-11-20  L. SAGER    CONVERT TO IBM AND ADD SATELLITES
C 2000-03-05  L. SAGER    OVERHAUL OF OUTPUT FORMAT
C 2000-07-01  L. SAGER    OVERHAUL FOR STATUS FILE CHANGES
C 2000-07-26  D. KEYSER   FIXED BUG WHICH PREVENTED W3TAGE FROM
C      BEING CALLED AT END OF PROGRAM; NOW READS DUMPJB DUMPLIST FILE
C      IN UNIT 10; 1B (SAT. RADIANCE TYPES) NOW CONTAIN 30-DAY AVERAGE
C      IN UPDATED STATUS FILE, WITH ALERTS IN PLACE AS FOR OTHER DATA
C      TYPES
C 2000-09-21  D. KEYSER   COMPLETE REDESIGN OF THIS PROGRAM (E.G.,
C      MORE ROBUST; DOES COMPARISON OF CURRENT 30-DAY AVERAGE FILE
C      AGAINST 30-DAY AVERAGE FILES FROM 3-, 6-, 9-, AND 12-MONTHS AGO;
C      MORE FLEXIBILITY IN PROGRAM, NOTES DATA TYPES WITH HIGH COUNTS
C      VS. 3-DAY AVERAGE, ETC.)
C 2000-12-05  D. KEYSER   NOW WRITES OUTPUT FROM COMPARISON TO 3-, 6-,
C      9-, AND 12-MONTHS AGO TO SEPARATE OUTPUT FILES (UNITS 54-57)
C      RATHER THAN ALL TO ONE FILE (UNIT 54); THE LIMIT FOR GENERATING
C      A MESSAGE OF HIGH OR LOW COUNTS IN THE COMPARISON BETWEEN THE
C      CURRENT 30-DAY AVERAGE AND THE PREVIOUS 30-DAY AVERAGES IS NOW
C      READ IN AS A NAMELIST SWITCH RATHER THAN BEING HARDWIRED TO 20%
C      (THE DEFAULT IS 20%)
C 2001-04-02  D. KEYSER   ADDED LOGICAL ARRAY VARIABLE "WEEK_END" TO
C      INPUT NAMELIST SWITCHES, WHEN WEEK_END IS TRUE FOR A TYPE/
C      SUBTYPE THEN THE THRESHOLD FOR GENERATING A CURRENT COUNT DUMP
C      ALERT IS REDUCED BY 10% FROM THAT READ IN FROM DUMPLIST IF
C      TODAY IS CONSIDERED TO BE A WEEKEND AND/OR REDUCED BY 15% IF
C      TODAY IS IN THE LAST WEEK OF THE YEAR (HOLIDAY SEASON) OR TODAY
C      IS THANKSGIVING (THE REASONING: CERTAIN DATA TYPES, E.G.
C      AIRCRAFT, MAY EXHIBIT LOWER COUNTS ON WEEKENDS AND/OR OVER
C      HOLIDAYS); ADDED OPTION TO RETURN WITH NEW CODE 4 WHICH
C      INDICATES A DEFICIENCY IN A "NON-CRITICAL" DATA TYPE (E.G.,
C      A TYPE THAT IS DUMPED BUT NOT ASSIMILATED BY THE ANALYSIS)
C 2002-03-19  D. KEYSER   INCREASED PARAMETER IMAX_BTYPE FROM 101
C      TO 255 TO ACCOUNT FOR NEW MESONET DATA IN BUFR TYPE 255
C 2003-03-03  D. KEYSER   CORRECTED DAY OF MONTH RANGE FOR FRIDAY
C      AFTER THANKSGIVING FROM NOV. 22-28 TO NOV. 23-29.; HOLIDAY
C      THRESHOLD REDUCTION INCREASED FROM 15% TO 20%; ADDED NEW
C      SWITCH TO ALLOW GOES (SOUNDING/RETRIEVAL/WIND/RADIANCE)
C      REPORTS TO GET A 15% THRESHOLD REDUCTION DURING ECLIPSE
C      SEASONS (AND WINDS GET SKIPPED IN NAM NETWORK)
C 2004-08-25  D. KEYSER   OFF-TIME (06, 18Z) FIXED LAND RAOBS REDUCE
C      THE CURRENT COUNT DUMP ALERT PERCENT THRESHOLD BY 10% (PREVENTS
C      SPECIAL FIELD EXPERIMENTS FROM TRIPPING ALERTS (ABUNDANCE DURING
C      EXPERIMENT, DEFICIENCY AFTER EXPERIMENT)
C 2006-03-08  D. KEYSER  REMOVED PROCESSING OF 1B SATELLITE REPORTS
C      WHICH WERE IN THE LONG-OBSOLETE IEEE DUMP STATUS FILE (THESE
C      HAVE LONG BEEN INCLUDED IN THE BUFR DUMP STATUS FILE ALONG WITH
C      ALL OTHER REPORTS); INPUT NAME LIST CHARACTER STRINGS IN
C      VARIABLES "NETWORK", "TMMARK" AND "CYCLE" ARE NOW EXPECTED TO BE
C      IN UPPER-CASE RATHER THAN IN LOWER-CASE
C 2013-03-15 JWhiting - port to WCOSS (no logic changes)
C 2017-01-10 D. Keyser  Minor changes to account for fact that NAM can
C      run at time marks other than tm00 after NAMv4 implementation
C      (only tm00 expected here).
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT (DATA CARDS - SEE NAMELIST
C              - DOCUMENTATION BELOW)
C     UNIT 10  - TABLE CONTAINING MNEMONIC NAME (A6), BUFR TYPE (I3),
C                BUFR SUBTYPE (I3), DESCRIPTION (A49) AND FLAGS FOR
C                EACH NETWORK (3(2X,A3,1X,I2,1X,I4,1X,4A1)) FOR ALL
C                CURRENTLY VALID BUFR TYPES AND SUBTYPES (THIS IS
C                GENERATED FROM THE bufr_dumplist FILE, SEE SCRIPT),
C                FORMAT:
C                '(1X,A6,8X,2(I3),5X,A49,3(2X,A3,1X,I2,1X,I4,1X,4A1))
C     UNIT 11  - TABLE CONTAINING BUFR TYPE (I3), BUFR SUBTYPE (I3) AND
C                DUMP COUNT (I7) FOR ALL OBSERVATIONS IN THIS CYCLE'S
C                NETWORK DATA DUMP RUN (THIS IS GENERATED FROM THE
C                DUMP STATUS FILE IN /com, SEE SCRIPT),
C                FORMAT: '(I3,1X,I3,1X,I7)'
C     UNIT 13  - TABLE CONTAINING THE MOST RECENT (CURRENT) 30-DAY
C                AVERAGE COUNTS AND NUMBER OF DAYS BY CYCLE (4(1X,I7,
C                1X,I3,1X)) FOR EACH BUFR TYPE (I3) AND SUBTYPE (I3)
C                (THIS IS GENERATED FROM THE FILE
C                obcount_30davg.*.current, WHERE THE SECOND QUALIFIER
C                IS THE NETWORK, SEE SCRIPT),
C                FORMAT:'(7X,I3,1X,I3,4(1X,I7,1X,I3,1X))'
C     UNIT 14  - TABLE CONTAINING 30-DAY AVERAGE COUNTS AND NUMBER OF
C                DAYS BY CYCLE FOR 3-MONTHS AGO (4(1X,I7,1X,I3,1X)) FOR
C                EACH BUFR TYPE (I3) AND SUBTYPE (I3) (THIS IS
C                GENERATED FROM THE FILE obcount_30davg.*.YYYYMM, WHERE
C                THE SECOND QUALIFIER IS THE NETWORK AND "YYYYMM" IS
C                3-MONTHS AGO, SEE SCRIPT),
C                FORMAT: '(7X,I3,1X,I3,4(1X,I7,1X,I3,1X))'
C     UNIT 15  - TABLE CONTAINING 30-DAY AVERAGE COUNTS AND NUMBER OF
C                DAYS BY CYCLE FOR 6-MONTHS AGO (4(1X,I7,1X,I3,1X)) FOR
C                EACH BUFR TYPE (I3) AND SUBTYPE (I3) (THIS IS
C                GENERATED FROM THE FILE obcount_30davg.*.YYYYMM, WHERE
C                THE SECOND QUALIFIER IS THE NETWORK AND "YYYYMM" IS
C                6-MONTHS AGO, SEE SCRIPT),
C                FORMAT: '(7X,I3,1X,I3,4(1X,I7,1X,I3,1X))'
C     UNIT 16  - TABLE CONTAINING 30-DAY AVERAGE COUNTS AND NUMBER OF
C                DAYS BY CYCLE FOR 9-MONTHS AGO (4(1X,I7,1X,I3,1X)) FOR
C                EACH BUFR TYPE (I3) AND SUBTYPE (I3) (THIS IS
C                GENERATED FROM THE FILE obcount_30davg.*.YYYYMM, WHERE
C                THE SECOND QUALIFIER IS THE NETWORK AND "YYYYMM" IS
C                9-MONTHS AGO, SEE SCRIPT),
C                FORMAT: '(7X,I3,1X,I3,4(1X,I7,1X,I3,1X))'
C     UNIT 17  - TABLE CONTAINING 30-DAY AVERAGE COUNTS AND NUMBER OF
C                DAYS BY CYCLE FOR 12-MONTHS AGO (4(1X,I7,1X,I3,1X))
C                FOR EACH BUFR TYPE (I3) AND SUBTYPE (I3) (THIS IS
C                GENERATED FROM THE FILE obcount_30davg.*.YYYYMM, WHERE
C                THE SECOND QUALIFIER IS THE NETWORK AND "YYYYMM" IS
C                12-MONTHS AGO, SEE SCRIPT),
C                FORMAT: '(7X,I3,1X,I3,4(1X,I7,1X,I3,1X))'
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 51  - UPDATED DATA COUNT (STATUS) TABLE FOR THIS CYCLE AND
C                NETWORK CONTAINING TODAY'S DUMP RECEIPTS AND THE
C                CURRENT 30-DAY AVERAGE VALUES FOR EACH BUFR TYPE AND
C                SUBTYPE DUMPED
C     UNIT 52  - ALERTLOG AND JOBLOG MESSAGE FILE FOR RESULTS OF
C                COMPARISON BETWEEN THIS CYCLE'S DUMP COUNTS AND THE
C                CURRENT 30-DAY AVERAGES - CONTAINS ONLY THOSE TYPES
C                WITH A DEFICIENCY IN THIS CYCLE'S DUMP
C     UNIT 53  - JOBLOG MESSAGE FILE FOR RESULTS OF COMPARISON BETWEEN
C                THIS CYCLE'S DUMP COUNTS AND THE CURRENT 30-DAY
C                AVERAGES - CONTAINS ONLY THOSE TYPES WITH AN
C                ABUNDANCE IN THIS CYCLE'S DUMP
C     UNIT 54  - JOBLOG MESSAGE FILE FOR RESULTS OF COMPARISON BETWEEN
C                THE CURRENT 30-DAY AVERAGES AND 30-DAY AVERAGES FROM
C                3-MONTHS AGO
C     UNIT 55  - JOBLOG MESSAGE FILE FOR RESULTS OF COMPARISON BETWEEN
C                THE CURRENT 30-DAY AVERAGES AND 30-DAY AVERAGES FROM
C                6-MONTHS AGO
C     UNIT 56  - JOBLOG MESSAGE FILE FOR RESULTS OF COMPARISON BETWEEN
C                THE CURRENT 30-DAY AVERAGES AND 30-DAY AVERAGES FROM
C                9-MONTHS AGO
C     UNIT 57  - JOBLOG MESSAGE FILE FOR RESULTS OF COMPARISON BETWEEN
C                THE CURRENT 30-DAY AVERAGES AND 30-DAY AVERAGES FROM
C                12-MONTHS AGO
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - COMPARE_COUNTS
C     LIBRARY:
C       W3LIB    - W3TAGB W3TAGE W3DOXDAT ERREXIT
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN, NO DEFICIENCY EXISTS IN ANY DATA
C                   TYPE DUMPED IN THIS NETWORK RUN WHEN COMPARED TO
C                   THE CURRENT 30-DAY AVERAGE FILE
C          =   4 - SUCCESSFUL RUN, A DEFICIENCY EXISTS IN ONE OR MORE
C                   NON-CRITICAL DATA TYPES DUMPED IN THIS NETWORK RUN
C                   WHEN COMPARED TO THE CURRENT 30-DAY AVERAGE FILE,
C                   HOWEVER NONE OF THE DATA TYPES ARE CONSIDERED TO
C                   BE EITHER MODERATELY-CRITICAL OR CRITICAL
C          =   5 - SUCCESSFUL RUN, A DEFICIENCY EXISTS IN ONE OR MORE
C                   MODERATELY-CRITICAL DATA TYPES DUMPED IN THIS
C                   NETWORK RUN WHEN COMPARED TO THE CURRENT 30-DAY
C                   AVERAGE FILE, HOWEVER NONE OF THE DATA TYPES ARE
C                   CONSIDERED TO BE CRITICAL
C          =   6 - SUCCESSFUL RUN, A DEFICIENCY EXISTS IN ONE OR MORE
C                   CRITICAL DATA TYPES DUMPED IN THIS NETWORK RUN WHEN
C                   COMPARED TO THE CURRENT 30-DAY AVERAGE FILE
C          =  20 - FAILED RUN, BAD HOUR READ FROM STANDARD INPUT
C          =  21 - FAILED RUN, INVALID NETWORK READ FROM STANDARD INPUT
C          =  30 - FAILED RUN, ERROR READING SUBSET OF DUMPLIST FILE IN
C                   UNIT 10
C          =  31 - FAILED RUN, INVALID BUFR TYPE READ FROM SUBSET OF
C                   DUMPLIST FILE IN UNIT 10
C          =  32 - FAILED RUN, INVALID BUFR SUBTYPE READ FROM SUBSET OF
C                   DUMPLIST FILE IN UNIT 10
C          =  40 - FAILED RUN, ERROR READING SUBSET OF DUMP STATUS
C                   FILE IN UNIT 11
C          =  41 - FAILED RUN, INVALID BUFR TYPE READ FROM SUBSET OF
C                   DUMP STATUS FILE IN UNIT 11
C          =  42 - FAILED RUN, INVALID BUFR SUBTYPE READ FROM SUBSET OF
C                   DUMP STATUS FILE IN UNIT 11
C          =  60 - FAILED RUN, ERROR READING SUBSET OF EITHER THE
C                   CURRENT 30-DAY AVERAGE OR ONE OF THE PREVIOUS
C                   30-DAY AVERAGE FILES IN UNITS 13-17
C          =  61 - FAILED RUN, INVALID BUFR TYPE READ FROM SUBSET OF
C                   EITHER THE CURRENT 30-DAY AVERAGE FILE OR ONE OF
C                   THE PREVIOUS 30-DAY AVERAGE FILES IN UNIT 13-17
C          =  62 - FAILED RUN, INVALID BUFR SUBTYPE READ FROM SUBSET OF
C                   EITHER THE CURRENT 30-DAY AVERAGE FILE OR ONE OF
C                   THE PREVIOUS 30-DAY AVERAGE FILES IN UNITS 13-17
C
C REMARKS:
C     VARIABLES IN NAMELIST "INPUT" READ IN MAIN PROGRAM
C       CDATE10  - C*10, CURRENT DUMPTIME DATE ('YYYYMMDDHH')
C       NETWORK  - C*4,  NETWORK {either 'NAM ' (tm00), 'GFS ', or 'GDAS'}
C       TMMARK   - C*4,  DUMP TIME RELATIVE TO CYCLE TIME (e.g., 'TM00')
C       CYCLE    - C*3,  ANALYSIS CYCLE (e.g., '06Z')
C       JOB      - C*21, JOB NAME (e.g., 'gfs_dump_post_06' or
C                                        'nam_dump_post_tm00_06')
C       CURRDATE - C*32, CURRENT DATE
C                        (e.g.,'Wed Mar  9 08:48:44 GMT 2006')
C       KLIMIT   - THE LIMITING PERCENT FOR GENERATING A MESSAGE IN THE
C                   COMPARISON BETWEEN THE CURRENT 30-DAY AVERAGE AND
C                   THE PREVIOUS 30-DAY AVERAGES (I.E., IF THE CURRENT
C                   30-DAY AVERAGE IS ABOVE OR BELOW THE PAST 30-DAY
C                   AVERAGE BY "KLIMIT"% A MESSAGE IS WRITTEN TO
C                   APPROPRIATE OUTPUT UNIT (DEFAULT IS KLIMIT=20)
C       SKIP     - LOGICAL 5-DIM ARRAY WHERE:
C                    FIRST ELEMENT IS BUFR TYPE
C                    SECOND ELEMENT IS BUFR SUBTYPE
C                    THIRD ELEMENT IS NETWORK:
C                      1 - NAM (tm00)
C                      2 - GFS
C                      3 - GDAS
C                    FOURTH ELEMENT IS CYCLE:
C                      1 - 00Z
C                      2 - 06Z
C                      3 - 12Z
C                      4 - 18Z
C                    FIFTH ELEMENT IS DATA COUNT CHECK TYPE:
C                      1 - CURRENT COUNT VS. CURRENT 30-DAY AVERAGE FILE
C                      2 - CURRENT 30-DAY AVERAGE FILE VS. PREVIOUS
C                          30-DAY AVERAGE FILES
C                      3 - BOTH CHECKS)
C                  NO DATA COUNT CHECKING OF ANY TYPE IS PERFORMED FOR
C                   A PARTICULAR TYPE (I), SUBTYPE (J), NETWORK (K),
C                   AND CYCLE (L) IF SKIP(I,J,K,L,3) IS TRUE
C                   {DEFAULT IS SKIP(:,:,:,:,:)=FALSE}
C       WEEK_END - LOGICAL 5-DIM ARRAY WHERE:
C                    FIRST ELEMENT IS BUFR TYPE
C                    SECOND ELEMENT IS BUFR SUBTYPE
C                    THIRD ELEMENT IS NETWORK:
C                      1 - NAM (tm00)
C                      2 - GFS
C                      3 - GDAS
C                    FOURTH ELEMENT IS CYCLE:
C                      1 - 00Z
C                      2 - 06Z
C                      3 - 12Z
C                      4 - 18Z
C                    FIFTH ELEMENT IS TYPE SWITCH:
C                      1 - WEEKEND
C                      2 - HOLIDAY
C                      3 - BOTH
C                  REDUCES THE CURRENT COUNT DUMP ALERT PERCENT
C                   THRESHOLD BY 10% FOR A PARTICULAR TYPE (I), SUBTYPE
C                   (J), NETWORK (K), AND CYCLE (L) (OVER THAT READ
C                   FROM DUMPLIST) IF WEEK_END(I,J,K,L,1) OR
C                   WEEK_END(I,J,K,L,3) IS TRUE AND TODAY IS A WEEKEND
C                       -- OR --
C                  REDUCES THE CURRENT COUNT DUMP ALERT PERCENT
C                   THRESHOLD BY 20% FOR A PARTICULAR TYPE (I), SUBTYPE
C                   (J), NETWORK (K), AND CYCLE (L) (OVER THAT READ
C                   FROM DUMPLIST) IF WEEK_END(I,J,K,L,2) OR
C                   WEEK_END(I,J,K,L,3) IS TRUE AND TODAY IS EITHER:
C                     - IN THE LAST WEEK OF THE YEAR (HOLIDAY PERIOD)
C                     - THANKSGIVING
C
C                  SPECIAL CASE FOR GOES REPORTS (SOUNDINGS, WINDS,
C                   RETRIEVALS, RADIANCES):
C                    IF THE CURRENT DAY IS DURING AN ECLIPSE PERIOD,
C                     THEN TYPE SWITCH (FIFTH ELEMENT) = 4 WILL REDUCE
C                     THE DUMP ALERT THRESHOLD BY 15% IF
C                     WEEK_END(I,J,K,L,4) IS TRUE {if NAM (tm00)
C                     network, will SKIP data count checking for GOES
C                     WINDS}
C
C                   {DEFAULT IS WEEK_END(:,:,:,:,:)=FALSE}
C
C                   (NOTE1: This is useful for data types such as
C                           aircraft which may contain fewer reports on
C                           weekends and holidays and GOES soundings/
C                           winds/radiances which may contain fewer (or
C                           no) reports at certain cycles during an
C                           eclipse period)
C                   (NOTE2: For a particular I,J,K,L only one of
C                           WEEK_END(I,J,K,L,1), WEEK_END(I,J,K,L,2),
C                           WEEK_END(I,J,K,L,3), OR WEEK_END(I,J,K,L,4)
C                           should ever be TRUE)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM-SP
C
C$$$

      PROGRAM BUFR_DATACOUNT

      PARAMETER (IMAX_BTYPE=255)
      PARAMETER (IMAX_STYPE=255)
      PARAMETER (IMAX_CMNEM=27)
      PARAMETER (IMAX_CAVGS=27)

      PARAMETER (IMAX_CMNEM_ALL=(IMAX_CMNEM * 2 * 3))
      PARAMETER (IMAX_CAVGS_ALL=(IMAX_CAVGS * 2 * 4))

      PARAMETER (IMAX_BS_TYPE=(IMAX_BTYPE + 1) * (IMAX_STYPE + 1))

      COMMON /DATA/iavg_count(000:imax_btype,000:imax_stype,5),
     $ iavg_ndays(000:imax_btype,000:imax_stype,5),
     $ icur_count(000:imax_btype,000:imax_stype),
     $ iper(000:imax_btype,000:imax_stype),
     $ ilimit(000:imax_btype,000:imax_stype),
     $ ivalid_st(000:imax_btype,000:imax_stype)

      COMMON /CDATA6/mnemonic(000:imax_btype,000:imax_stype)
      COMMON /CDATA3/cred(000:imax_btype,000:imax_stype)
      COMMON /CDATA2/cflag(000:imax_btype,000:imax_stype)
      COMMON /CDATA1/clong(000:imax_btype,000:imax_stype,2:5)

      COMMON /SKIPIT/skip(000:imax_btype,000:imax_stype,3,4,3)
      COMMON /WKENDS/week_end(000:imax_btype,000:imax_stype,3,4,4)

      CHARACTER*49  description(000:imax_btype,000:imax_stype),desc
      CHARACTER*32  currdate
      CHARACTER*21  job
      CHARACTER*19  csevere(3)
      CHARACTER*10  cdate10
      CHARACTER*6   mnemonic,
     $              cmnem(imax_cmnem,2,3),cavgs(imax_cavgs,2,2:5),mnemon
      CHARACTER*4   tmmark,clohi_l(2),network,network_types(3)
      CHARACTER*3   cred,cr(3),cycle
      CHARACTER*2   cflag
      CHARACTER*1   clong,cl(3,2:5)

      LOGICAL       skip,week_end,eclipse

      INTEGER       ip(3),il(3),jreps_30d(4),ndays(4),idat(8),
     $              isub_per(4,4)

      NAMELIST      /INPUT/CDATE10,NETWORK,TMMARK,CYCLE,JOB,CURRDATE,
     $                     KLIMIT,SKIP,WEEK_END

      DATA          cmnem          /imax_cmnem_all*'      '/
      DATA          cavgs          /imax_cavgs_all*'      '/
      DATA          network_types  /'NAM ','GFS ','GDAS'/
      DATA          clohi_l        /'low ','high'/
      DATA          csevere        /'...... Non-critical',
     $                              'MODERATELY-CRITICAL',
     $                              '***** CRITICAL ****'/
      DATA  description
     $/imax_bs_type*'                                                 '/

      DATA
C  Alert threshold reduction (%)
C   week_end(i,j,k,l,M)=TRUE,
C                --weekend--   --holiday--   ---both----   --eclipse--
C     where M=    1     3          2  3       1  2  3                4
C                -- -- -- --   -- -- -- --   -- -- -- --   -- -- -- --
     $ isub_per/ 10, 0,10, 0,   0,20,20, 0,  10,20,20, 0,   0, 0, 0,15/

      CALL W3TAGB('BUFR_DATACOUNT',2017,0010,0079,'NP22')

      ierr = 0

      klimit = 20
      read(5,input)

      PRINT 101, cdate10,network,tmmark
  101 FORMAT(/'#####  Welcome to DATACOUNT - Version 01/10/2017'/3x,
     $ '--->  DUMPTIME DATE IS: ',a10/3x,'--->  NETWORK IS:',7x,a4,3x,
     $ a4//)

      idat = 0

      READ(cdate10(1:8),'(i4,i2,i2)') (idat(ii),ii=1,3)
      CALL W3DOXDAT(idat,jdow,jdoy,jday)

      READ(cdate10(9:10),'(i2)') ihour

      icycle = ihour/6 + 1

C  iwkend_fl: =0 Today is neither a weekend nor a holiday
C             =1 Today is a weekend but not a holiday
C             =2 Today is a holiday but not a weekend
C             =3 Today is a holiday and a weekend
C             =4 Today is an eclipse day {GOES radiance/sounding/
C                retrieval/wind data only (wind only if GFS or GDAS
C                network), overrides any pervious value of
C                iwkend_fl - this cannot be determined until a report
C                type/subtype is actually read in later}
C             =5 Today is an eclipse day {GOES wind data in NAM (tm00)
C                network only, overrides any previous value of iwkend_fl
C                - this cannot be determined until a report type/subtype
C                is actually read in later)


      iwkend_fl = 0
      if(ihour.eq.00) then                ! Sun, Mon for 00Z
         if(jdow.eq.1.or.jdow.eq.2) iwkend_fl = 1
      else if(ihour.eq.06) then           ! Sun, Mon for 06Z
         if(jdow.eq.1.or.jdow.eq.2) iwkend_fl = 1
      else if(ihour.eq.12) then           ! Sun, Mon for 12Z
         if(jdow.eq.1.or.jdow.eq.2) iwkend_fl = 1
      else if(ihour.eq.18) then           ! Sun, Sat for 18Z
         if(jdow.eq.1.or.jdow.eq.7) iwkend_fl = 1
      end if
      if(jdoy.gt.355 .or. jdoy.lt.3) then ! Last week of yr holiday
         if(iwkend_fl.eq.0)  then
            iwkend_fl = 2
         else
            iwkend_fl = 3
         end if
      else if(idat(2).eq.11)  then
         if(idat(3).gt.21 .and. idat(3).lt.29)  then
            if(ihour.eq.12) then
               if(jdow.eq.5)  then  ! Thanksgiving Day 12Z
                  if(iwkend_fl.eq.0)  then
                     iwkend_fl = 2
                  else
                     iwkend_fl = 3
                  end if
               end if
            end if
         end if
         if(idat(3).gt.22 .and. idat(3).lt.30)  then
            if(ihour.eq.00) then
               if(jdow.eq.6)  then  ! Fri after Thanksgiving 00Z
                  if(iwkend_fl.eq.0)  then
                     iwkend_fl = 2
                  else
                     iwkend_fl = 3
                  end if
               end if
            else if(ihour.eq.06) then
               if(jdow.eq.6)  then  ! Fri after Thanksgiving 06Z
                  if(iwkend_fl.eq.0)  then
                     iwkend_fl = 2
                  else
                     iwkend_fl = 3
                  end if
               end if
            end if
         end if
      end if
      iwkend_fl_ORIG = iwkend_fl

C  The eclipse window is expanded 10-days on each side to account for
C   NESDIS "keep-out" zones
C  ------------------------------------------------------------------

      eclipse=((jdoy.gt. 45 .and. jdoy.lt.114)!Spring Eclipse(2/25-4/13)
     $    .or. (jdoy.gt.221 .and. jdoy.lt.297))!Fall Eclipse(8/20-10/13)

      print *, ' '
      print *, 'Integer Day of Week  is ',jdow
      print *, 'Integer Day of Year  is ',jdoy
      print *, 'Weekend/holiday Flag is ',iwkend_fl,
     $ '(0-neither; 1-weekend only; 2-holiday only; 3-both)'
      if(eclipse) then
         if(network.ne.'NAM ')  then
            print *, '  - this will later be reset to 4 (eclipse) for ',
     $       'GOES retr/sndgs/rad/wind since we are in an eclipse ',
     $       'season'
         else
            print *, '  - this will later be reset to 4 (eclipse) for ',
     $       'GOES retr/sndgs/rad or 5 for GOES winds since we are in ',
     $       'an eclipse season'
         end if
      end if
      print *, ' '

      inet = 0
      DO I=1,3
         IF(network.eq.network_types(i))  THEN
            inet = i
            exit
         END IF
      END DO

      IF(mod(ihour,6).ne.0) THEN
         PRINT 102, ihour
  102 FORMAT(/' ### BAD HOUR (=',i3.2,') READ IN FROM STANDARD INPUT ',
     $ 'DATA CARDS -- STOP 20'/)
         CALL W3TAGE('BUFR_DATACOUNT')
         CALL ERREXIT(20)
      ELSE IF(inet.eq. 0) THEN
         PRINT 103, network
  103 FORMAT(/' ### INVALID NETWORK (="',a4,'") READ IN FROM STANDARD',
     $ ' INPUT DATA CARDS -- STOP 21'/)
         CALL W3TAGE('BUFR_DATACOUNT')
         CALL ERREXIT(21)
      END IF

C     Parse out information from the subset of the dumplist file
C     ----------------------------------------------------------

      PRINT 104
  104 FORMAT('  ==> Parsing through the subset of the dumplist file'/)

      DO WHILE (ierr .eq. 0)
         READ(10,105,end=10,err=996) mnemon,jtype,jsub,desc,
     $    (cr(i),ip(i),il(i),(cl(i,j),j=2,5),i=1,3)
  105 FORMAT(1x,a6,8x,i3,i3,5x,a49,3(2x,a3,1x,i2,1x,i4,1x,4a1))
         IF(jtype.lt.0. .or. jtype.gt.imax_btype) THEN
            PRINT 106,jtype
  106 FORMAT(' ###  INVALID VALUE FOR BUFR TYPE IN SUBSET OF DUMPLIST ',
     $ 'FILE (=',i3.3,') -- STOP 31')
            CALL W3TAGE('BUFR_DATACOUNT')
            CALL ERREXIT(31)
         ELSE IF(jsub.lt.0. .or. jsub.gt.imax_stype) THEN
            PRINT 107, jsub
  107 FORMAT(' ### INVALID VALUE FOR BUFR SUBTYPE IN SUBSET OF ',
     $ 'DUMPLIST FILE (=',i3.3,') -- STOP 32')
            CALL W3TAGE('BUFR_DATACOUNT')
            CALL ERREXIT(32)
         END IF

         iwkend_fl = iwkend_fl_ORIG
         if(eclipse)  then  ! Test for proper data type if eclipse
            if(jtype.eq.3)  then
               if(jsub.lt.10)  iwkend_fl = 4 ! GOES soundings all nets
            else  if(jtype.eq.5)  then   
               if(jsub.lt.21)  then
                  iwkend_fl = 4            ! GOES winds GFS or GDAS net
                  if(inet.eq.1) iwkend_fl= 5 ! GOES winds NAM (tm00) net
               end if
            else  if(jtype.eq.21) then
               if(jsub.eq.41)  iwkend_fl = 4 ! GOES imager radiances
            end if
         end if

         mnemonic(jtype,jsub)    = mnemon
         description(jtype,jsub) = desc
         cred(jtype,jsub)        = cr(inet)

         iper(jtype,jsub)        = ip(inet)
         jwkend_fl = 0
         if(iwkend_fl.eq.1 .or. iwkend_fl.eq.2)  then
            if(week_end(jtype,jsub,inet,icycle,iwkend_fl))  then
               jwkend_fl = iwkend_fl
            else  if(week_end(jtype,jsub,inet,icycle,3))  then
               jwkend_fl = 3
            end if
         else  if(iwkend_fl.eq.3)  then
            do ifl = 1,3
               if(week_end(jtype,jsub,inet,icycle,ifl)) then
                  jwkend_fl = ifl
                  exit
               end if
            end do
         else  if(iwkend_fl.eq.4)  then
            if(week_end(jtype,jsub,inet,icycle,iwkend_fl))
     $       jwkend_fl = iwkend_fl
         end if
         if(min(iwkend_fl,jwkend_fl).gt.0)  then
            if(iper(jtype,jsub).ge.isub_per(jwkend_fl,iwkend_fl))
     $       iper(jtype,jsub) = iper(jtype,jsub) -
     $                          isub_per(jwkend_fl,iwkend_fl)
         end if

C Off-time (06, 18Z) fixed land RAOBs reduce the current count dump
C  alert percent threshold by 10% (prevents special field experiments
C  from tripping alerts (abundance during exp., deficiency after exp.)
C --------------------------------------------------------------------

         if(ihour.eq.06.or.ihour.eq.18)  then
            if(jtype.eq.002.and.jsub.eq.001)  then
               if(iper(002,001).ge.10) iper(002,001) = iper(002,001) -10
            end if
         end if


         ilimit(jtype,jsub)      = il(inet)
         clong(jtype,jsub,:)     = cl(inet,:)

         PRINT 109, mnemonic(jtype,jsub),jtype,jsub,cred(jtype,jsub),
     $   iper(jtype,jsub),ilimit(jtype,jsub),(clong(jtype,jsub,i),
     $   i=2,5),jwkend_fl
  109 FORMAT(32('-')/' mnemonic "',a6,'"'/' type ',i4.3,
     $' subtype ',i4.3/' red flag "',a3,'"'/' percent ',i3/' limit ',i5,
     $ /' long-term ',4a1/' jwkend_fl ',i2)
         if(jwkend_fl.eq.1) then
            PRINT 2109
 2109 FORMAT(' ===> The percent limit has been reduced by 10% for this',
     $ ' type because it satisfies the weekend criterion')
         else  if(jwkend_fl.eq.2) then
            PRINT 2110
 2110 FORMAT(' ===> The percent limit has been reduced by 20% for this',
     $ ' type because it satisfies the holiday criterion')
         else  if(jwkend_fl.eq.3) then
            if(iwkend_fl.eq.1)  then
               PRINT 2109
            else
               PRINT 2110
            end if
         else  if(jwkend_fl.eq.4) then
            PRINT 2111
 2111 FORMAT(' ===> The percent limit has been reduced by 15% for this',
     $ ' type because it satisfies the eclipse criterion')
         else  if(iwkend_fl.eq.5)  then
            if(week_end(jtype,jsub,inet,icycle,4))  then
               SKIP(jtype,jsub,inet,icycle,1) = .TRUE.
               PRINT 2112
 2112 FORMAT(' ===> This type will not be checked because it is a GOES',
     $ ' WIND report which satisfies the eclipse criterion')
            end if
         end if
      END DO

   10 CONTINUE

      PRINT 110
  110 FORMAT('--------------------------------'/)

C     Parse out information from the subset of the dump status file
C     -------------------------------------------------------------

      DO WHILE (ierr .eq. 0)
         READ(11,*,end=20,err=997) jtype,jsub,jreps
         IF(jtype.lt.0. .or. jtype.gt.imax_btype) THEN
            PRINT 111, jtype
  111 FORMAT(' ### INVALID VALUE FOR BUFR TYPE IN SUBSET OF DUMP ',
     $ 'STATUS FILE (=',i3.3,') -- STOP 41')
            CALL W3TAGE('BUFR_DATACOUNT')
            CALL ERREXIT(41)
         ELSE IF(jsub.lt.0. .or. jsub.gt.imax_stype) THEN
            PRINT 112, jsub
  112 FORMAT(' ### INVALID VALUE FOR BUFR SUBTYPE IN SUBSET OF DUMP ',
     $ 'STATUS FILE (=',i3.3,') -- STOP 42')
            CALL W3TAGE('BUFR_DATACOUNT')
            CALL ERREXIT(42)
         END IF
C     .. this will take care of things if a type/subtype is ever split up
         icur_count(jtype,jsub) = icur_count(jtype,jsub) + jreps
         ivalid_st(jtype,jsub)  = 1
      END DO

   20 CONTINUE

C     Parse out information from the five 30-day average files
C     --------------------------------------------------------

      DO iavg = 1,5
         months_ago = (iavg - 1) * 3
         ifile = 12 + iavg
         iflag = 0

         IF(iavg.gt.1)  THEN
            PRINT 113, months_ago
  113 FORMAT(/'  ==> Parsing through the subset of the 30-day average ',
     $ 'file from ',i2,' months ago'/)
         ELSE
            PRINT 114
  114 FORMAT(/'  ==> Parsing through the subset of the current 30-day ',
     $ 'average file'/)
         END IF

         DO WHILE (ierr .eq. 0)
            READ(ifile,*,end=40,err=999) jtype,jsub,(jreps_30d(i),
     $       ndays(i),i=1,4)
            iflag = 1
            IF(jtype.lt.0. .or. jtype.gt.imax_btype) THEN
               IF(iavg.gt.1)  THEN
                  PRINT 115, months_ago,jtype
  115 FORMAT(' ### INVALID VALUE FOR BUFR TYPE IN SUBSET OF 30-DAY ',
     $ 'AVERAGE FILE FROM ',i2,' MONTHS AGO (=',i3.3,') -- STOP 61')
               ELSE
                  PRINT 116, jtype
  116 FORMAT(' ### INVALID VALUE FOR BUFR TYPE IN SUBSET OF CURRENT ',
     $ '30-DAY AVERAGE FILE (=',i3.3,') -- STOP 61')
               END IF
               CALL W3TAGE('BUFR_DATACOUNT')
               CALL ERREXIT(61)
            ELSE IF(jsub.lt.0. .or. jsub.gt.imax_stype) THEN
               IF(iavg.gt.1)  THEN
                  PRINT 117, months_ago,jsub
  117 FORMAT(' ### INVALID VALUE FOR BUFR SUBTYPE IN SUBSET OF 30-DAY ',
     $ 'AVERAGE FILE FROM ',i2,' MONTHS AGO (=',i3.3,') -- STOP 62')
               ELSE
                  PRINT 118, jsub
  118 FORMAT(' ### INVALID VALUE FOR BUFR SUBTYPE IN SUBSET OF CURRENT',
     $ ' 30-DAY AVERAGE FILE (=',i3.3,') -- STOP 62')
               END IF
               CALL W3TAGE('BUFR_DATACOUNT')
               CALL ERREXIT(62)
            END IF
            PRINT 119, jtype,jsub,(jreps_30d(i),ndays(i),i=1,4)
  119 FORMAT(32('-')/' type ',i4.3,' subtype ',i4.3/
     $ ' 00Z     ',i8,'/',i3/' 06Z     ',i8,'/',i3/' 12Z     ',i8,'/',
     $ i3/' 18Z     ',i8,'/',i3)   
            if(ndays(icycle).gt.0)
     $       iavg_count(jtype,jsub,iavg) = jreps_30d(icycle)
            iavg_ndays(jtype,jsub,iavg) = ndays(icycle)
         END DO 

   40    CONTINUE

         IF(iflag.eq.0)  PRINT 120
  120    FORMAT(7x,'-- This file is empty!!'/)

         PRINT 110

      END DO

C     Compare counts and set the return code
C     --------------------------------------

      CALL COMPARE_COUNTS(klimit,cmnem,cavgs,inet,icycle,istop)  

C     Generate the updated status file
C     --------------------------------

C        --> Headings

      WRITE(51,121) tmmark,cycle,network,job,cdate10(1:4),cdate10(5:6),
     $ cdate10(7:8),cdate10(9:10),currdate
  121 FORMAT(80('*')/'*',78(' '),'*'/'*',12x,'### ',a4,' OBSERVATIONAL',
     $ ' DATA DUMP FOR ',a3,1x,a4,' RUN ###',13x,'*'/'*',78(' '),'*'/'*'
     $,22x,'SCRIPT IS: ',a21,24x,'*'/'*',20x,'Run Date is ',a4,'/',a2,
     $ '/',a2,2x,a2,'Z',31x,'*'/'*',15x,'FILE CREATED ',a32,18x,'*'/'*',
     $ 78(' '),'*')

      nflag = 0
      LOOP1: DO isevere=3,1,-1
         IF(cmnem(1,1,isevere) .eq. '      ') THEN
            WRITE(51,122) csevere(isevere)
  122       FORMAT(80('*')/'* ',a19,' low data counts encountered: ',
     $       'none',24x,'*')
         ELSE
            WRITE(51,123) csevere(isevere)
  123       FORMAT(80('*')/'* ',a19,' low data counts encountered:',
     $       29x,'*')
            nflag = 1
            LOOP2: DO k = 1,3
               kk = (k-1)*9
               IF(cmnem(kk+1,1,isevere).ne.'      ') then
                  WRITE(51,124) (cmnem(kk+i,1,isevere),i=1,9)
  124             FORMAT('*',2x,9(2x,a6),4x,'*')
               ELSE
                  EXIT LOOP2
               END IF
            END DO LOOP2
         END IF
      END DO LOOP1 
      IF(nflag .eq. 1)  THEN
         WRITE(51,125)
  125 FORMAT(80('*')/'*   Below,  "+" next to mnemonic means ',
     $ 'deficiency in non-critical data type,   *'/'*',11x,'"*" next ',
     $ 'to mnemonic means deficiency in moderately-critical type, *'/
     $ '*',10x,'"**" next to mnemonic means deficiency in CRITICAL ',
     $ 'data type.',7x,'*')
      END IF
      IF(cmnem(1,2,1) .eq. '      ') THEN
         WRITE(51,1223)
 1223    FORMAT(80('*')/'* High data counts encountered: none',43x,
     $    '*')
      ELSE
         WRITE(51,1233)
 1233    FORMAT(80('*')/'* High data counts encountered:',48x,'*')
         LOOP3: DO k = 1,3
            kk = (k-1)*9
            IF(cmnem(kk+1,2,1).ne.'      ') then
               WRITE(51,1243) (cmnem(kk+i,2,1),i=1,9)
 1243          FORMAT('*',2x,9(2x,a6),4x,'*')
            ELSE
               EXIT LOOP3
            END IF
         END DO LOOP3
         WRITE(51,126)
  126 FORMAT('*',78('-'),'*'/'*   Below,  "!" next to mnemonic means ',
     $ 'abundance in non- or mod-critical type, *'/'*',10x,'"!!" next ',
     $ 'to mnemonic means abundance in CRITICAL data type.',8x,'*')
      END IF

      LOOP4: DO iavg = 2,5
         months_ago = (iavg - 1) * 3
         LOOP5: DO ilohi = 1,2
            IF(cavgs(1,ilohi,iavg) .ne. '      ') THEN
               WRITE(51,127) clohi_l(ilohi),months_ago
  127 FORMAT(80('*')/'* Current 30-day avg ',a4,' vs. avg from ',i2,
     $ ' months ago: ',25x,'*')
               LOOP6: DO k = 1,3
                  kk = (k-1)*9
                  IF(cavgs(kk+1,ilohi,iavg).ne.'      ') THEN
                     WRITE(51,128) (cavgs(kk+i,ilohi,iavg),i=1,9)
  128                FORMAT('*',2x,9(2x,a6),4x,'*')
                  ELSE
                     EXIT LOOP6
                  END IF
               END DO LOOP6
            END IF
         END DO LOOP5
      END DO LOOP4
      WRITE(51,129)
  129 FORMAT(80('*')/'*',78(' '),'*'/'* todays    30-day',61(' '),'*'/
     $ '* receipt   average                Data Type',35(' '),'*'/
     $ '*',78(' '),'*')

C        --> Updated data for today

      LOOP7: DO jtype = 0,imax_btype
         LOOP8: DO jsub = 0,imax_stype
            IF(mnemonic(jtype,jsub).eq.'      ')  CYCLE LOOP8
            IF(ivalid_st(jtype,jsub).eq.0) CYCLE LOOP8
            WRITE(51,130) icur_count(jtype,jsub),
     $       iavg_count(jtype,jsub,1),cflag(jtype,jsub),
     $       mnemonic(jtype,jsub),description(jtype,jsub)
  130       FORMAT('*',i7,3x,i7,1x,a2,1x,a6,1x,a49,' *')
         END DO LOOP8
      END DO LOOP7

      WRITE(51,131)
  131 FORMAT('*',78(' '),'*'/80('*'))

C     All done
C     --------

      PRINT 132, istop
  132 FORMAT(//' ** DATACOUNT ** ENDING WITH RETURN CODE ',i3/)

      CALL W3TAGE('BUFR_DATACOUNT')
      CALL ERREXIT(istop)

      STOP

C-----------------------------------------------------------------------
  996 CONTINUE
      PRINT 133
  133 FORMAT(' ###  ERROR READING SUBSET OF DUMPLIST FILE IN UNIT 10 ',
     $ '-- STOP 30')
      CALL W3TAGE('BUFR_DATACOUNT')
      CALL ERREXIT(30)
C-----------------------------------------------------------------------
  997 CONTINUE
      PRINT 134
  134 FORMAT(' ###  ERROR READING SUBSET OF DUMP STATUS FILE IN UNIT',
     $ ' 11 -- STOP 40')
      CALL W3TAGE('BUFR_DATACOUNT')
      CALL ERREXIT(40)
C-----------------------------------------------------------------------
  999 CONTINUE
      IF(iavg.gt.1)  THEN
         PRINT 136, months_ago,ifile
  136 FORMAT(' ###  ERROR READING SUBSET OF 30-DAY AVERAGE FILE FROM ',
     $ i2,' MONTHS AGO IN UNIT ',i2,' -- STOP 60')
      ELSE
         PRINT 137
  137 FORMAT(' ###  ERROR READING SUBSET OF CURRENT 30-DAY AVERAGE ',
     $ 'FILE IN UNIT 13 -- STOP 60')
      END IF
      CALL W3TAGE('BUFR_DATACOUNT')
      CALL ERREXIT(60)
C-----------------------------------------------------------------------

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    COMPARE_COUNTS
C   PRGMMR: KEYSER           ORG: NP22       DATE: 2002-03-19
C
C ABSTRACT: COMPARES THE ACTUAL NUMBER OF OBSERVATIONS DUMPED IN THE
C   CURRENT OPERATIONAL RUN AGAINST THE CURRENT 30-DAY AVERAGES.  IF
C   THERE IS A DEFICIENCY, A MESSAGE FOR THE ALERTLOG AND JOBLOG IS
C   PREPARED AND A FLAG IS SET WHICH WILL LATER RESULT IN AN EXIT
C   CODE OF EITHER 4, 5 OR 6.  IF THERE IS AN ABUNDANCE, A MESSAGE FOR
C   THE JOBLOG IS PREPARED BUT NO FUTURE RETURN CODE FLAG IS SET.
C            IN ADDITION, COMPARES THE CURRENT 30-DAY AVERAGES AGAINST
C   30-DAY AVERAGES FROM 3-, 6-, 9-, AND 12-MONTHS AGO.  IF EITHER A
C   DEFICIENCY OR ABUNDANCE OCCURS IN ANY DATA TYPE IN THE CURRENT
C   OPERATIONAL RUN, A MESSAGE FOR THE JOBLOG IS PREPARED BUT NO FUTURE
C   RETURN CODE FLAG IS SET.
C
C PROGRAM HISTORY LOG:
C 1999-06-24  L. SAGER  
C 2000-09-21  D. KEYSER   COMPLETE REDESIGN OF THIS PROGRAM (E.G.,
C      MORE ROBUST; DOES COMPARISON OF CURRENT 30-DAY AVERAGE FILE
C      AGAINST 30-DAY AVERAGE FILES FROM 3-, 6-, 9-, AND 12-MONTHS AGO;
C      MORE FLEXIBILITY IN PROGRAM, NOTES DATA TYPES WITH HIGH COUNTS
C      VS. 3-DAY AVERAGE, ETC.)
C 2000-12-05  D. KEYSER   NOW WRITES OUTPUT FROM COMPARISON TO 3-, 6-,
C      9-, AND 12-MONTHS AGO TO SEPARATE OUTPUT FILES (UNITS 54-57)
C      RATHER THAN ALL TO ONE FILE (UNIT 54); THE LIMIT FOR GENERATING
C      A MESSAGE OF HIGH OR LOW COUNTS IN THE COMPARISON BETWEEN THE
C      CURRENT 30-DAY AVERAGE AND THE PREVIOUS 30-DAY AVERAGES IS NOW
C      READ IN AS A NAMELIST SWITCH RATHER THAN BEING HARDWIRED TO 20%
C      (THE DEFAULT IS 20%)
C 2001-04-02  D. KEYSER   ADDED OPTION TO RETURN WITH NEW CODE 4 WHICH
C      INDICATES A DEFICIENCY IN A "NON-CRITICAL" DATA TYPE (E.G.,
C      A TYPE THAT IS DUMPED BUT NOT ASSIMILATED BY THE ANALYSIS)
C 2002-03-19  D. KEYSER   INCREASED PARAMETER IMAX_BTYPE FROM 101
C      TO 255 TO ACCOUNT FOR NEW MESONET DATA IN BUFR TYPE 255
C
C USAGE:    CALL COMPARE_COUNTS(KLIMIT,CMNEM,CAVGS,INET,ICYCLE,ISTOP)
C   INPUT ARGUMENT LIST:
C     KLIMIT   - THE LIMITING PERCENT FOR GENERATING A MESSAGE IN THE
C              - COMPARISON BETWEEN THE CURRENT 30-DAY AVERAGE AND THE
C              - PREVIOUS 30-DAY AVERAGES (I.E., IF THE CURRENT 30-DAY
C              - AVERAGE IS ABOVE OR BELOW THE PAST 30-DAY AVERAGE BY
C              - "KLIMIT"% A MESSAGE IS WRITTEN TO APPROPRIATE OUTPUT
C              - UNIT
C
C   OUTPUT ARGUMENT LIST:
C     CMNEM    - 3-DIM STRING ARRAY CONTAINING MNEMONIC NAME FOR ALL
C              - BUFR TYPE AND SUBTYPES WHICH ARE EITHER DEFICIENT OR
C              - EXCESSIVE (ELEMENT 2) FOR CURRENT CYCLE DUMP VS.
C              - CURRENT 30-DAY AVERAGE (ELEMENT 3) FOR THE THREE
C              - DEGREES OF DEFICIENCY SEVERITY (FOR EXCESSIVE CASES,
C              - ONLY WORD 1 IN ELEMENT 3 USED) (C*6)
C     CAVGS    - 3-DIM STRING ARRAY CONTAINING MNEMONIC NAME FOR ALL
C              - BUFR TYPE AND SUBTYPES WHICH ARE EITHER DEFICIENT OR
C              - EXCESSIVE (ELEMENT 2) FOR CURRENT 30-DAY AVERAGE VS.
C              - 30-DAY AVERAGE FROM 3-, 6-, 9-, AND 12-MONTHS AGO
C              - (ELEMENT 3) (C*6)
C     INET     - NETWORK INDICATOR {=1 - NAM (tm00); =2 - GFS;
C              -  =3 - GDAS}
C     ICYCLE   - CYCLE INDICATOR (=1 - 00Z; =2 - 06Z; =3 - 12Z;
C              - =4 - 18Z)
C     ISTOP    - FLAG WHICH WILL LATER DEFINE THE PROGRAM RETURN CODE:
C              -  =0 - SUCCESSFUL RUN, NO DEFICIENCY EXISTS IN ANY DATA
C              -        TYPE DUMPED IN THIS NETWORK RUN WHEN COMPARED
C              -        TO THE CURRENT 30-DAY AVERAGE FILE
C              -  =4 - SUCCESSFUL RUN, A DEFICIENCY EXISTS IN ONE OR
C              -        MORE NON-CRITICAL DATA TYPES DUMPED IN THIS
C              -        NETWORK RUN WHEN COMPARED TO THE CURRENT 30-DAY
C              -        AVERAGE FILE, HOWEVER NONE OF THE DATA TYPES
C              -        ARE CONSIDERED TO BE EITHER MODERATELY-CRITICAL
C              -        OR CRITICAL
C              -  =5 - SUCCESSFUL RUN, A DEFICIENCY EXISTS IN ONE OR
C              -        MORE MODERATELY-CRITICAL DATA TYPES DUMPED IN
C              -        THIS NETWORK RUN WHEN COMPARED TO THE CURRENT
C              -        30-DAY AVERAGE FILE, HOWEVER NONE OF THE DATA
C              -        TYPES ARE CONSIDERED TO BE CRITICAL
C              -  =6 - SUCCESSFUL RUN, A DEFICIENCY EXISTS IN ONE OR
C              -        MORE CRITICAL DATA TYPES DUMPED IN THIS NETWORK
C              -        WHEN COMPARED TO THE CURRENT 30-DAY AVERAGE
C              -        FILE
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 52  - ALERTLOG AND JOBLOG MESSAGE FILE FOR RESULTS OF
C              - COMPARISON BETWEEN THIS CYCLE'S DUMP COUNTS AND THE
C              - CURRENT 30-DAY AVERAGES - CONTAINS ONLY THOSE TYPES
C              - WITH A DEFICIENCY IN THIS CYCLE'S DUMP
C     UNIT 53  - JOBLOG MESSAGE FILE FOR RESULTS OF COMPARISON BETWEEN
C              - THIS CYCLE'S DUMP COUNTS AND THE CURRENT 30-DAY
C              - AVERAGES - CONTAINS ONLY THOSE TYPES WITH AN
C              - ABUNDANCE IN THIS CYCLE'S DUMP
C     UNIT 54  - JOBLOG MESSAGE FILE FOR RESULTS OF COMPARISON BETWEEN
C              - THE CURRENT 30-DAY AVERAGES AND 30-DAY AVERAGES FROM
C              - 3-MONTHS AGO
C     UNIT 55  - JOBLOG MESSAGE FILE FOR RESULTS OF COMPARISON BETWEEN
C              - THE CURRENT 30-DAY AVERAGES AND 30-DAY AVERAGES FROM
C              - 6-MONTHS AGO
C     UNIT 56  - JOBLOG MESSAGE FILE FOR RESULTS OF COMPARISON BETWEEN
C              - THE CURRENT 30-DAY AVERAGES AND 30-DAY AVERAGES FROM
C              - 9-MONTHS AGO
C     UNIT 57  - JOBLOG MESSAGE FILE FOR RESULTS OF COMPARISON BETWEEN
C              - THE CURRENT 30-DAY AVERAGES AND 30-DAY AVERAGES FROM
C              - 12-MONTHS AGO
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$

      SUBROUTINE COMPARE_COUNTS(klimit,cmnem,cavgs,inet,icycle,istop)  
     
      PARAMETER (IMAX_BTYPE=255)
      PARAMETER (IMAX_STYPE=255)
      PARAMETER (IMAX_CMNEM=27)
      PARAMETER (IMAX_CAVGS=27)

      COMMON /DATA/iavg_count(000:imax_btype,000:imax_stype,5),
     $ iavg_ndays(000:imax_btype,000:imax_stype,5),
     $ icur_count(000:imax_btype,000:imax_stype),
     $ iper(000:imax_btype,000:imax_stype),
     $ ilimit(000:imax_btype,000:imax_stype),
     $ ivalid_st(000:imax_btype,000:imax_stype)

      COMMON /CDATA3/cred(000:imax_btype,000:imax_stype)
      COMMON /CDATA6/mnemonic(000:imax_btype,000:imax_stype)
      COMMON /CDATA2/cflag(000:imax_btype,000:imax_stype)
      COMMON /CDATA1/clong(000:imax_btype,000:imax_stype,2:5)

      COMMON /SKIPIT/skip(000:imax_btype,000:imax_stype,3,4,3)

      CHARACTER*7   redstring
      CHARACTER*6   cmnem(imax_cmnem,2,3),cavgs(imax_cavgs,2,2:5),
     $ mnemonic
      CHARACTER*3   cred
      CHARACTER*2   cflag
      CHARACTER*1   clong

      LOGICAL       skip

      INTEGER       isc(2,3),isa(2,2:5)

      isc   = 0
      isa   = 0
      istop = 0

      alimit_low = 1.0 - (real(klimit) * 0.01)
      IF(alimit_low .gt. 0) THEN
         alimit_hi = 1.0/alimit_low
      ELSE
         alimit_hi = 1000.
      END IF

      PRINT 201
  201 FORMAT('  ==> Ready to compare todays counts against current ',
     $ '30-day averages'/6x,'and to compare current 30-day averages ',
     $ 'against 30-day averages from 3-, 6-, 9- and 12-months ago'/)
      LOOP1: DO jtype = 0,imax_btype
         LOOP2: DO jsub = 0,imax_stype
            IF(mnemonic(jtype,jsub).eq.'      ')  CYCLE LOOP2
            IF(ivalid_st(jtype,jsub).eq.0) CYCLE LOOP2
            PRINT 202, jtype,jsub,mnemonic(jtype,jsub)
  202 FORMAT(' Type   ',i3.3,2x,i3.3,' "',a6,'" located in subset of ',
     $ 'todays status file')
            IF(skip(jtype,jsub,inet,icycle,3))  THEN
               PRINT 203, jtype,jsub
  203 FORMAT(4x,'--> type   ',i3.3,2x,i3.3,' not tested in any way ',
     $ 'because input data cards indicate it should be SKIPPED for ',
     $ 'this network and cycle')
            ELSEIF(iavg_ndays(jtype,jsub,1) .lt. 15)  THEN
               PRINT 204, jtype,jsub,iavg_ndays(jtype,jsub,1)
  204 FORMAT(4x,'--> type   ',i3.3,2x,i3.3,' not tested in any way ',
     $ 'because number of days that went into current 30-day average ',
     $ '(=',i3,') .lt. limit of 15')

            ELSEIF(iavg_count(jtype,jsub,1) .gt. ilimit(jtype,jsub))THEN
               IF(.not.skip(jtype,jsub,inet,icycle,1))  THEN
                  ilimit_count_lo = 
     $            nint(iavg_count(jtype,jsub,1)*(iper(jtype,jsub)*0.01))
                  IF(iper(jtype,jsub) .gt. 0) THEN
                     per_hi=100./iper(jtype,jsub)
                  ELSE
                     per_hi=1000.
                  END IF
                  ilimit_count_hi= nint(iavg_count(jtype,jsub,1)*per_hi)
                  IF(icur_count(jtype,jsub).lt.ilimit_count_lo)  THEN
                     IF(cred(jtype,jsub).eq.'RED') then
                        isc(1,3) = isc(1,3) + 1
                        IF(isc(1,3) .lt. 28) cmnem(isc(1,3),1,3) =
     $                   mnemonic(jtype,jsub)
                        istop = max(istop,6)
                        cflag(jtype,jsub) = '**'
                        redstring = ' !!RED!'
                        PRINT 205, mnemonic(jtype,jsub),
     $                   icur_count(jtype,jsub),
     $                   iavg_count(jtype,jsub,1),
     $                   iavg_ndays(jtype,jsub,1)
  205 FORMAT(3x,125('#')/3x,'*****>  !! R E D !!  DEFICIENCY in ',
     $ 'CRITICAL data type: "',a6,'"; Todays count : ',i7,', Average/#',
     $ ' days : ',i7,'/',i3,' ###'/3x,125('#'))
                     ELSE  IF(cred(jtype,jsub).eq.'grn') then
                        isc(1,1) = isc(1,1) + 1
                        IF(isc(1,1) .lt. 28) cmnem(isc(1,1),1,1) =
     $                   mnemonic(jtype,jsub)
                        istop = max(istop,4)
                        cflag(jtype,jsub) = ' +'
                        redstring = ' green'
                        PRINT 4206, mnemonic(jtype,jsub),
     $                   icur_count(jtype,jsub),
     $                   iavg_count(jtype,jsub,1),
     $                   iavg_ndays(jtype,jsub,1)
 4206 FORMAT(3x,115('#')/3x,'*****>  Deficiency in non-critical data ',
     $ 'type: "',a6,'"; Todays count : ',i7,', Average/# days : ',i7,
     $ '/',i3,' ###'/3x,115('#'))
                     ELSE
                        isc(1,2) = isc(1,2) + 1
                        IF(isc(1,2) .lt. 28) cmnem(isc(1,2),1,2) =
     $                   mnemonic(jtype,jsub)
                        istop = max(istop,5)
                        cflag(jtype,jsub) = ' *'
                        redstring = ' yellow'
                        PRINT 206, mnemonic(jtype,jsub),
     $                   icur_count(jtype,jsub),
     $                   iavg_count(jtype,jsub,1),
     $                   iavg_ndays(jtype,jsub,1)
  206 FORMAT(3x,121('#')/3x,'*****> DEFICIENCY in moderately-critical ',
     $ 'data type: "',a6,'"; Todays count : ',i7,', Average/# days : ',
     $ i7,'/',i3,' ###'/3x,121('#'))
                     END IF
                     WRITE(52,207) mnemonic(jtype,jsub),
     $                icur_count(jtype,jsub),iper(jtype,jsub),
     $                iavg_count(jtype,jsub,1),redstring
  207 FORMAT('DATACOUNT-deficiency in type ',a6,';',i7,' rpts ',
     $ 'dumped, <',i3,'% of 30-day avg',i7,a7)
                  ELSE IF(icur_count(jtype,jsub).gt.ilimit_count_hi)THEN
                     isc(2,1) = isc(2,1) + 1
                     IF(isc(2,1) .lt. 28) cmnem(isc(2,1),2,1) =
     $                mnemonic(jtype,jsub)
                     cflag(jtype,jsub) = ' !'
                     redstring = ' '
                     IF(cred(jtype,jsub).eq.'RED') then
                        cflag(jtype,jsub) = '!!'
                        redstring = ' !!RED!'
                        PRINT 208, mnemonic(jtype,jsub),
     $                   icur_count(jtype,jsub),
     $                   iavg_count(jtype,jsub,1),
     $                   iavg_ndays(jtype,jsub,1)
  208 FORMAT(3x,106('#')/3x,'*****>  !!!  R E D  !!!  ABUNDANCE  in ',
     $ 'CRITICAL data type: "',a6,'"; Todays count : ',i7,', Average/#',
     $ ' days : ',i7,'/',i3,' ###'/3x,106('#'))
                     ELSE
                        PRINT 209, mnemonic(jtype,jsub),
     $                   icur_count(jtype,jsub),
     $                   iavg_count(jtype,jsub,1),
     $                   iavg_ndays(jtype,jsub,1)
  209 FORMAT(3x,90('#')/3x,'*****> ABUNDANCE  in: "',a6,'"; Todays ',
     $ 'count : ',i7,', Average/# days : ',i7,'/',i3,' ###'/3x,90('#'))
                     END IF
                     WRITE(53,210) mnemonic(jtype,jsub),
     $                icur_count(jtype,jsub),nint(per_hi*100.),
     $                iavg_count(jtype,jsub,1),redstring
  210 FORMAT('DATACOUNT-abundance  in type ',a6,';',i7,' rpts ',
     $ 'dumped, >',i3,'% of 30-day avg',i7,a7)
                  END IF
               ELSE
                  PRINT 211, jtype,jsub
  211 FORMAT(4x,'--> type   ',i3.3,2x,i3.3,'- current counts vs. ',
     $ 'current average not tested; input data cards set to SKIPPED ',
     $ 'for this network and cycle')
               END IF
               IF(.not.skip(jtype,jsub,inet,icycle,2))  THEN
                  LOOP3: DO iavg = 2,5
                     months_ago = (iavg - 1) * 3
                     iunit = 52 + iavg
                     IF(clong(jtype,jsub,iavg).ne.'Y')  CYCLE LOOP3
                     IF((iavg_count(jtype,jsub,iavg).le.
     $                   ilimit(jtype,jsub)) .or.
     $                  (iavg_ndays(jtype,jsub,iavg).le.14)) CYCLE LOOP3
                        ilimit_count_hi =
     $                   nint(iavg_count(jtype,jsub,iavg)*alimit_hi)
                        ilimit_count_lo =
     $                   nint(iavg_count(jtype,jsub,iavg)*alimit_low)
                     IF(iavg_count(jtype,jsub,1) .lt. ilimit_count_lo)
     $                THEN
                        isa(1,iavg) = isa(1,iavg) + 1
                        IF(isa(1,iavg) .lt. 28)
     $                   cavgs(isa(1,iavg),1,iavg)= mnemonic(jtype,jsub)
                        PRINT 212, mnemonic(jtype,jsub),klimit,
     $                   months_ago,iavg_count(jtype,jsub,1),
     $                   iavg_ndays(jtype,jsub,1),
     $                   iavg_count(jtype,jsub,iavg),
     $                   iavg_ndays(jtype,jsub,iavg)
  212 FORMAT(3x,125('#')/3x,'=-LO-====> AVG for: "',a6,'" is >',i2,'% ',
     $'BELOW AVG FROM ',i2,'-MOS AGO; Avg/# days : Current:',i7,'/',i3,
     $ '; Previous:',i7,'/',i3,' ###'/3x,125('#'))
                        WRITE(IUNIT,213) mnemonic(jtype,jsub),
     $                   iavg_count(jtype,jsub,1),klimit,months_ago,
     $                   iavg_count(jtype,jsub,iavg)
  213 FORMAT('DATACOUNT-Avg for ',a6,' (',i7,') is >',i2,'% BELOW ',
     $ 'avg from ',i2,' mnths ago (',i7,')')
                     ELSE  IF(iavg_count(jtype,jsub,1) .gt.
     $                        ilimit_count_hi)  THEN
                        isa(2,iavg) = isa(2,iavg) + 1
                        IF(isa(2,iavg) .lt. 28)
     $                   cavgs(isa(2,iavg),2,iavg)= mnemonic(jtype,jsub)
                        PRINT 214, mnemonic(jtype,jsub),klimit,
     $                   months_ago,iavg_count(jtype,jsub,1),
     $                   iavg_ndays(jtype,jsub,1),
     $                   iavg_count(jtype,jsub,iavg),
     $                   iavg_ndays(jtype,jsub,iavg)
  214 FORMAT(3x,125('#')/3x,'=-HI-====> AVG for: "',a6,'" is >',i2,'% ',
     $'above AVG FROM ',i2,'-MOS AGO; Avg/# days : Current:',i7,'/',i3,
     $ '; Previous:',i7,'/',i3,' ###'/3x,125('#'))
                        WRITE(IUNIT,215) mnemonic(jtype,jsub),
     $                   iavg_count(jtype,jsub,1),klimit,months_ago,
     $                   iavg_count(jtype,jsub,iavg)
  215 FORMAT('DATACOUNT-Avg for ',a6,' (',i7,') is >',i2,'% above ',
     $ 'avg from ',i2,' mnths ago (',i7,')')
                     END IF
                  END DO LOOP3
               ELSE
                  PRINT 216, jtype,jsub
  216 FORMAT(4x,'--> type   ',i3.3,2x,i3.3,'- current average vs. ',
     $ 'previous average not tested; input data cards set to SKIPPED ',
     $ 'for this network & cycle')
               END IF
            ELSE
               IF(iavg_count(jtype,jsub,1).gt.-1)  THEN
                  PRINT 217, jtype,jsub,iavg_count(jtype,jsub,1),
     $             iavg_ndays(jtype,jsub,1),ilimit(jtype,jsub)
  217 FORMAT(4x,'--> type   ',i3.3,2x,i3.3,' not tested in any way ',
     $ 'because current 30-day average {=',i6,' (over',i3,' days)} ',
     $ '.lt. limit of',i6)
               ELSE
                  PRINT 218, jtype,jsub
  218 FORMAT(4x,'--> type   ',i3.3,2x,i3.3,' not tested in any way ',
     $ 'because current 30-day average is not available')
               END IF
            END IF
         END DO LOOP2
      END DO LOOP1

      RETURN

      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    BLOCK DATA  INITIALIZES COMMON BLOCK PARAMETERS
C   PRGMMR: KEYSER           ORG: NP22       DATE: 2000-09-21
C
C ABSTRACT: INITIALIZES COMMON BLOCK PARAMETERS.
C
C PROGRAM HISTORY LOG:
C   UNKNOWN
C 2000-09-21  D. A. KEYSER -- ORIGINAL AUTHOR
C                "FWINDO(5)" FROM 300. TO 600.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM-SP
C
C$$$
      BLOCK DATA

      PARAMETER (IMAX_BTYPE=255)
      PARAMETER (IMAX_STYPE=255)

      PARAMETER (IMAX_BS_TYPE=(IMAX_BTYPE + 1) * (IMAX_STYPE + 1))
      PARAMETER (IMAX_BS_TYPE_AVG=(IMAX_BS_TYPE * 5))
      PARAMETER (IMAX_BS_TYPE_LONG=(IMAX_BS_TYPE * 4))
      PARAMETER (IMAX_BS_TYPE_SKIP=(IMAX_BS_TYPE * 36))
      PARAMETER (IMAX_BS_TYPE_WEEKEND=(IMAX_BS_TYPE * 48))

      COMMON /DATA/iavg_count(000:imax_btype,000:imax_stype,5),
     $ iavg_ndays(000:imax_btype,000:imax_stype,5),
     $ icur_count(000:imax_btype,000:imax_stype),
     $ iper(000:imax_btype,000:imax_stype),
     $ ilimit(000:imax_btype,000:imax_stype),
     $ ivalid_st(000:imax_btype,000:imax_stype)

      COMMON /CDATA6/mnemonic(000:imax_btype,000:imax_stype)
      COMMON /CDATA3/cred(000:imax_btype,000:imax_stype)
      COMMON /CDATA2/cflag(000:imax_btype,000:imax_stype)
      COMMON /CDATA1/clong(000:imax_btype,000:imax_stype,2:5)

      COMMON /SKIPIT/skip(000:imax_btype,000:imax_stype,3,4,3)
      COMMON /WKENDS/week_end(000:imax_btype,000:imax_stype,3,4,4)

      CHARACTER*6   mnemonic
      CHARACTER*3   cred
      CHARACTER*2   cflag
      CHARACTER*1   clong

      LOGICAL       skip,week_end

      DATA  iavg_count/imax_bs_type_avg*-9999999/,
     $      iavg_ndays/imax_bs_type_avg*0/,
     $      icur_count/imax_bs_type*0/,
     $      iper/imax_bs_type*0/,ilimit/imax_bs_type*0/,
     $      ivalid_st/imax_bs_type*0/

      DATA  mnemonic  /imax_bs_type*'      '/
      DATA  cred      /imax_bs_type*'   '/
      DATA  cflag     /imax_bs_type*'  '/
      DATA  clong     /imax_bs_type_long*'N'/

      DATA  skip      /imax_bs_type_skip*.FALSE./

      DATA  week_end  /imax_bs_type_weekend*.FALSE./

      END

