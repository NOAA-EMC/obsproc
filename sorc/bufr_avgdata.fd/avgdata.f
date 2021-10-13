C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_AVGDATA
C   PRGMMR: Whiting          ORG: EMC         DATE: 2017-11-20
C
C ABSTRACT: GENERATES A TABLE CONTAINING THE AVERAGE COUNTS FOR EACH
C   BUFR TYPE/SUBTYPE DUMPED IN A PARTICULAR NETWORK BY CYCLE OVER A
C   PERIOD OF TIME (NORMALLY 30-DAYS).  ALL OF THE INDIVIDUAL DAYS'
C   COUNTS ARE READ IN.  THE USH SCRIPT CONTROLS HOW MANY DAYS ARE
C   ACCUMULATED IN THESE INPUT FILES.  THIS TABLE IS READ BY THE
C   PROGRAM BUFR_DATACOUNT.
C
C PROGRAM HISTORY LOG:
C 2000-08-22  D. KEYSER   ORIGINAL AUTHOR
C 2001-01-05  D. KEYSER   CORRECTED ERROR WHICH LED TO PREVIOUS
C     TYPE/SUBTYPE AVERAGE FOR SAME CYCLE BEING PRINTED IN UNIT 51
C     WHEN THE NUMBER OF DAYS IN AVERAGE IS ZERO (CORRECTED AVERAGE
C     TO ALWAYS BE ZERO IN THIS CASE); CAN NOW GENERATE AVERAGE FOR
C     HOURLY DUMPS WHICH RUN AS PART OF A NEW MONITORING SYSTEM ("DUMP"
C     NETWORK), ACCUMULATED DUMP COUNT FILES (BY CYCLE) NOW OPENED BY
C     THIS PGM RATHER THAN BEING ASSIGNED TO UNIT NUMBERS IN USH
C     SCRIPT, GENERALIZED TO HANDLE 24 CYCLES FOR "DUMP" NETWORK AND
C     STILL 4 CYCLES FOR OTHER NETWORKS
C 2001-05-16  D. KEYSER   ADDED RUC2A, RUCS1 (EARLY CUTOFF SURFACE
C     RUC) AND RUCS2 (LATE CUTOFF SURFACE RUC), ALL 24 CYCLES
C 2002-03-19  D. KEYSER   INCREASED PARAMETER IMAX_BTYPE FROM 101
C     TO 255 TO ACCOUNT FOR NEW MESONET DATA IN BUFR TYPE 255
C 2006-03-08  D. KEYSER  REMOVED PROCESSING OF 1B SATELLITE REPORTS
C     WHICH WERE IN THE LONG-OBSOLETE IEEE DUMP STATUS FILE (THESE
C     HAVE LONG BEEN INCLUDED IN THE BUFR DUMP STATUS FILE ALONG WITH
C     ALL OTHER REPORTS); INPUT NAME LIST CHARACTER STRING IN VARIABLE
C     "NETWORK" IS NOW EXPECTED TO BE IN UPPER-CASE RATHER THAN IN
C     LOWER-CASE
C 2006-07-14  D. KEYSER   ADDED RTMA (REAL TIME MESOSCALE ANALYSIS),
C     ALL 24 CYCLES
C 2011-10-15  D. A. KEYSER -- RAP (RAPID REFRESH) NETWORK REPLACES
C     RUC2A (RAPID UPDATE CYCLE)
C 2013-01-10  D. A. KEYSER -- REMOVED RUCS1 (EARLY CUTOFF SURFACE RUC)
C     AND RUCS2 (LATE CUTOFF SURFACE RUC) AS THIS MODEL NO LONGER RUNS
C     IN PRODUCTION; ADDED URMA (UnRestricted Mesoscale Analysis)
C 2013-03-20  JWhiting  ported for use on WCOSS (linux) platforms:
C     Updated documentation (no logic changes necessary).
C 2017-11-20  D. A. KEYSER -- ACCOUNTS FOR NAM NOW HAVING tm06 THROUGH
C     tm00 TIME MARKERS (ONLY tm00 IS EXPECTED HERE FOR THIS AND ALL
C     oTHER NETWORKS).
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT (DATA CARDS - SEE NAMELIST
C                DOCUMENTATION BELOW)
C     UNIT 10  - TABLE CONTAINING MNEMONIC NAME (A6), BUFR TYPE (I3)
C                AND BUFR SUBTYPE (I3) FOR ALL CURRENTLY VALID BUFR
C                TYPES AND SUBTYPES (THIS IS GENERATED FROM THE
C                bufr_dumplist FILE, SEE SCRIPT), FMT:'(1X,A6,8X,I3,I3)'
C     UNIT 21  - TABLE CONTAINING BUFR TYPE (A3), BUFR SUBTYPE (A3) AND
C                DUMP COUNT (I7) FOR ALL OBSERVATIONS IN A CYCLE OVER A
C                SPECIFIED NUMBER OF DAYS (THIS IS GENERATED FROM AN
C                ARCHIVE OF THE INDIVIDUAL DAYS' DUMP STATUS FILES, SEE
C                SCRIPT), FORMAT: '(A3,1X,A3,1X,I7)', WITH THE STRING
C                'EOF @@@' IN COLUMNS 1-7 SEPARATING TWO DIFFERENT DAYS
C                (EACH INDIVIDUAL FILE FOR A CYCLE IS CONNECTED TO THIS
C                UNIT)
C
C   OUTPUT FILES:
C     UNIT 06  - PRINTOUT
C     UNIT 51  - TABLE CONTAINING AVERAGE COUNTS AND NUMBER OF DAYS BY
C                CYCLE TIME (4(1X,I7,'/',I3,1X)) FOR EACH BUFR TYPE
C                AND SUBTYPE READ IN FROM ALL VALID CYCLES IN UNIT 21,
C                FMT:'('#',3X,A6,2X,I3.3,'/',I3.3,4(1X,I7,'/',I3,1X))'
C                (MNEMONIC NAME, BUFR TYPE, BUFR SUBTYPE, 00Z AVERAGE/
C                # OF DAYS, 06Z AVERAGE/# OF DAYS, 12Z AVERAGE/# OF 
C                DAYS, 18Z AVERAGE/# OF DAYS); ALSO CONTAINS A HEADING
C     
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       W3NCO    - W3TAGB W3TAGE ERREXIT
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN, ALL DATA RECEIPT NORMAL
C          =  30 - FAILED RUN, ERROR READING SUBSET OF DUMPLIST FILE IN
C                   UNIT 10
C          =  31 - FAILED RUN, INVALID BUFR TYPE READ FROM SUBSET OF
C                   DUMPLIST FILE IN UNIT 10
C          =  32 - FAILED RUN, INVALID BUFR SUBTYPE READ FROM SUBSET OF
C                   DUMPLIST FILE IN UNIT 10
C          =  40 - FAILED RUN, ERROR READING SUBSET OF ACCUMULATED
C                   DUMP STATUS FILES IN UNIT 21
C          =  41 - FAILED RUN, INVALID BUFR TYPE READ FROM SUBSET OF
C                   ACCUMULATED DUMP STATUS FILES IN UNIT 21
C          =  42 - FAILED RUN, INVALID BUFR SUBTYPE READ FROM SUBSET OF
C                   ACCUMULATED DUMP STATUS FILES IN UNIT 21
C
C REMARKS:
C     VARIABLES IN NAMELIST "INPUT" READ IN MAIN PROGRAM
C       NETWORK  - C*5,  NETWORK {either 'NAM  '(tm00), 'GFS  ',
C                                 'GDAS ', 'RAP', 'RTMA', 'URMA' or
C                                 'DUMP'}
C                        (NOTE: 'DUMP' is the dump monitoring network
C                               which runs hourly)
C       CURRDATE - C*10, CURRENT DATE IN FORM "MM/DD/YYYY"
C
C ATTRIBUTES:
C   LANGUAGE: Intel Fortran
C   MACHINE:  WCOSS (linux)
C
C$$$

      PROGRAM BUFR_AVGDATA

      PARAMETER (IMAX_BTYPE=255)
      PARAMETER (IMAX_STYPE=255)

      PARAMETER (IMAX_BS_TYPE=(IMAX_BTYPE + 1) * (IMAX_STYPE + 1))

      CHARACTER*21  filename
      CHARACTER*10  currdate
      CHARACTER*6   mnemon,mnemonic(000:imax_btype,000:imax_stype)
      CHARACTER*5   network
      CHARACTER*3   atype,asub
      CHARACTER*2   cyc

      INTEGER         isum(000:imax_btype,000:imax_stype,0:23),
     $              icount(000:imax_btype,000:imax_stype,0:23),
     $            isum_day(000:imax_btype,000:imax_stype),
     $          icount_day(000:imax_btype,000:imax_stype),
     $           ivalid_st(000:imax_btype,000:imax_stype)

      REAL          avg_count(0:23)

      NAMELIST/INPUT/NETWORK,CURRDATE

      DATA    infile /21/

      CALL W3TAGB('BUFR_AVGDATA',2017,0324,0079,'NP22')

      READ(5,INPUT)

      print *, ' '
      print *, '#####  Welcome to AVGDATA - Version 11/20/2017'
      print *, ' '
      print *, '   --> Network is: ',network,' tm00'
      print *, ' '

      ierr = 0

      isum       = 0
      icount     = 0
      isum_day   = 0
      icount_day = 0
      mnemonic   = '      '
      ivalid_st  = 0

C     Parse out information from the subset of the dumplist file
C     ----------------------------------------------------------

      print *, ' '
      print *,'  ==> Parsing through the subset of the dumplist file'
      print *, ' '

      DO WHILE (ierr .eq. 0)
         READ(10,101,end=10,err=997) mnemon,jtype,jsub
  101 FORMAT(1x,a6,8x,i3,i3)
         IF(jtype.lt.0. .or. jtype.gt.imax_btype) THEN
            print *,' ###  INVALID VALUE FOR BUFR TYPE IN SUBSET OF ',
     $       'DUMPLIST FILE (=',jtype,') -- STOP 31'
            CALL W3TAGE('BUFR_AVGDATA')
            CALL ERREXIT(31)
         ELSE IF(jsub.lt.0. .or. jsub.gt.imax_stype) THEN
            print *,' ### INVALID VALUE FOR BUFR SUBTYPE IN SUBSET OF ',
     $       'DUMPLIST FILE (=',jsub,') -- STOP 32'
            CALL W3TAGE('BUFR_AVGDATA')
            CALL ERREXIT(32)
         END IF
         mnemonic(jtype,jsub) = mnemon
         PRINT 103, mnemonic(jtype,jsub),jtype,jsub
  103    FORMAT(32('-')/' mnemonic "',a6,'"'/' type ',i4.3,' subtype ',
     $    i4.3)
      END DO

   10 CONTINUE

      print *, '--------------------------------'
      print *, ' '

C     Parse out information from the subset of the accumulated
C     dump status files
C     --------------------------------------------------------

      print *, ' '
      print *,'  ==> Parsing through the subsets of the accumulated ',
     $ 'dump status files'
      print *, ' '

      if(network.eq.'DUMP'.or.network.eq.'RAP'.or.network.eq.'RTMA'.or.
     $   network.eq.'URMA') then
         icycle_iter = 1
      else
         icycle_iter = 6
      end if

      loop1: do icycle=0,23,icycle_iter
         write(cyc,'(i2.2)') icycle
         filename='accum.obs.counts.t'//cyc//'z'
         open(infile,file=filename,form='FORMATTED')

         loop2: do while (ierr .eq. 0)
            read(infile,*,end=20,err=998) atype,asub,jreps
cppppp
cdak        print *, 'atype,asub,jreps = ',atype,asub,jreps
cppppp
            if(atype.eq.'EOF'.and.asub.eq.'@@@') then

C     --> Finished reading counts for this cycle for a particular day
C         (this logic is needed if a type/subtype is ever split up)

               isum(:,:,icycle)   = isum(:,:,icycle)   + isum_day(:,:)
               icount(:,:,icycle) = icount(:,:,icycle) + icount_day(:,:)
               isum_day   = 0
               icount_day = 0
               cycle loop2
            end if
            read(atype,'(i3)') jtype
            read(asub ,'(i3)') jsub
            if(jtype.lt.0. .or. jtype.gt.imax_btype)   then
               print *,' ### INVALID VALUE FOR BUFR TYPE IN SUBSET OF ',
     $          'ACCUMULATED DUMP STATUS FILES FOR CYCLE ',cyc,'Z (=',
     $          jtype,') -- STOP 41'
               call w3tage('BUFR_AVGDATA')
               call errexit(41)
            else if(jsub.lt.0. .or. jsub.gt.imax_stype) THEN
               print *,' ### INVALID VALUE FOR BUFR SUBTYPE IN SUBSET ',
     $          'OF ACCUMULATED DUMP STATUS FILES FOR CYCLE ',cyc,
     $          'Z (=',jsub,') -- STOP 42'
               call w3tage('BUFR_AVGDATA')
               call errexit(42)
            end if
            isum_day(jtype,jsub)   = isum_day(jtype,jsub) + jreps
            icount_day(jtype,jsub) = 1
            ivalid_st(jtype,jsub)  = 1
         end do loop2

   20    continue

         close(infile)

      end do loop1

C     Generate the new 30-day average file
C     ------------------------------------

      print *, ' '
      print *,'  ==> Ready to generate new 30-day average file for ',
     $ network,' network - tm00'
      print *, ' '

C        --> Headings

      if(network.ne.'DUMP'.and.network.ne.'RAP'.and.network.ne.'RTMA'
     $ .and.network.ne.'URMA') then
         write(51,104) network,currdate
  104 format(4x,69('-')/4x,'Mean dump counts at each cycle for ',a5,
     $ ' tm00 network, ','listed by'/5x,'BUFR message type/subtype.'//
     $ 4x,'Averages calculated through (and including): ',a10,' .'//4x,
     $ 'This file is generated by the program BUFR_AVGDATA.'/4x,
     $ 'This file is input to the program BUFR_DATACOUNT.'//4x,
     $ 'WARNING: LINES WITH "#" IN COLUMN 1 ARE READ BY ',
     $ 'BUFR_DATACOUNT'/13x,'PROGRAM.  DO NOT CHANGE THE COLUMNAR ',
     $ 'POSITION OF ANY'/13x,'CHARACTERS IN THESE LINES!!!'//4x,69('-')/
     $ 4x,'-----type------ ------00Z-- -------06Z-- -------12Z-- ',
     $ '-------18Z--'/4x,'mnemon  typ sub',4(5x,'avg/days')/4x,
     $ '------  --- ---',4(' ------- --- '))
      else
         write(51,5104) network,currdate
 5104 format(4x,69('-')/4x,'Mean dump counts at each cycle for ',a5,
     $ ' network, ','listed by'/5x,'BUFR message type/subtype.'//4x,
     $ 'Averages calculated through (and including): ',a10,' .'//4x,
     $ 'This file is generated by the program BUFR_AVGDATA.'/4x,
     $ 'This file is input to the program BUFR_DATACOUNT.'//4x,
     $ 'WARNING: LINES WITH "#" IN COLUMN 1 ARE READ BY ',
     $ 'BUFR_DATACOUNT'/13x,'PROGRAM.  DO NOT CHANGE THE COLUMNAR ',
     $ 'POSITION OF ANY'/13x,'CHARACTERS IN THESE LINES!!!'//4x,69('-')/
     $ 4x,'-----type------ ------00Z-- -------01Z-- -------02Z-- ',
     $ '-------03Z-- -------04Z-- -------05Z-- -------06Z-- ',
     $ '-------07Z-- -------08Z-- -------09Z-- -------10Z-- ',
     $ '-------11Z-- -------12Z-- -------13Z-- -------14Z-- ',
     $ '-------15Z-- -------16Z-- -------17Z-- -------18Z-- ',
     $ '-------19Z-- -------20Z-- -------21Z-- -------22Z-- ',
     $ '-------23Z--'/4x,'mnemon  typ sub',24(5x,'avg/days')/4x,
     $ '------  --- ---',24(' ------- --- '))
      end if

C        --> Calculated averages

      LOOP5: DO jtype = 0,imax_btype
         new_jtype = 1
         LOOP6: DO jsub = 0,imax_stype
            IF(mnemonic(jtype,jsub).eq.'      ')  CYCLE LOOP6
            IF(ivalid_st(jtype,jsub).eq.0) CYCLE LOOP6
            PRINT 105, jtype,jsub,mnemonic(jtype,jsub)
  105 FORMAT(32('-')/' Type   ',i3.3,2x,i3.3,' "',a6,'" located in ',
     $ 'subset of accumulated status files')
            avg_count = 0.0
            do icycle=0,23,icycle_iter
               write(cyc,'(i2.2)') icycle
               if(icount(jtype,jsub,icycle).gt.0)  avg_count(icycle)=
     $    real(isum(jtype,jsub,icycle))/real(icount(jtype,jsub,icycle))
               PRINT 106, jtype,jsub,cyc,isum(jtype,jsub,icycle),
     $          icount(jtype,jsub,icycle),nint(avg_count(icycle))
  106 FORMAT(' -- for BUFR type =',i4.3,', subtype =',i4.3,', cycle = ',
     $ a2,'Z -- isum =',i10,', icount =',i4,'; average =',i7)
            end do
            if(new_jtype.eq.1)  write(51,107)
  107       format(1x)
            write(51,108) mnemonic(jtype,jsub),jtype,jsub,(nint
     $       (avg_count(icycle)),icount(jtype,jsub,icycle),
     $       icycle=0,23,icycle_iter)
  108       format('#',3x,a6,2x,i3.3,'/',i3.3,24(1x,i7,'/',i3,1x))
            new_jtype = 0
         END DO LOOP6
      END DO LOOP5

      print *, '--------------------------------'
      print *, ' '

      write(51,107)

C     All done
C     --------

      print *, ' '
      print *, ' '
      print *,' ** AVGDATA COMPLETED SUCCESSFULLY **'
      print *, ' '

      CALL W3TAGE('BUFR_AVGDATA')

      STOP

C-----------------------------------------------------------------------
  997 CONTINUE
      print *,' ###  ERROR READING SUBSET OF DUMPLIST FILE IN UNIT 10 ',
     $ '-- STOP 30'
      CALL W3TAGE('BUFR_AVGDATA')
      CALL ERREXIT(30)
C-----------------------------------------------------------------------
  998 CONTINUE
      print *,' ###  ERROR READING SUBSET OF ACCUMULATED DUMP STATUS',
     $ ' FILES IN FILE ',filename,' FOR CYCLE ',cyc,'Z -- STOP 40'
      CALL W3TAGE('BUFR_AVGDATA')
      CALL ERREXIT(40)
C-----------------------------------------------------------------------

      END

