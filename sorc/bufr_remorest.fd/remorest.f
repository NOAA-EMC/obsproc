C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM:  BUFR_REMOREST
C   PRGMMR: Keyser           ORG: EMC         DATE: 2017-10-20
C
C ABSTRACT: THIS PROGRAM READS THROUGH AN INPUT BUFR FILE (NORMALLY
C   EITHER A PREPBUFR OR A DATA DUMP FILE) WHICH CAN CONTAIN A MIXTURE
C   OF REPORTS WHICH ARE UNRESTRICTED OR RESTRICTED (W.R.T.
C   REDISTRIBUTION OUTSIDE OF NCEP) AND EITHER WRITES OUT (TO AN
C   OTHERWISE IDENTICAL BUFR FILE) ONLY THOSE REPORTS WHICH ARE
C   UNRESTRICTED OR WRITES OUT ALL REPORTS BUT WITH MASKED REPORT ID's
C   FOR THOSE REPORTS WHICH ARE RESTRICTED (WHAT IT DOES IS BASED ON
C   NAMELIST SWITCHES).  IT DETERMINES WHICH REPORTS ARE RESTRICTED
C   BASED ON EITHER THE MESSAGE TYPE AND SUBTYPE (MAKING UP THE TABLE A
C   ENTRY IN DATA DUMP FILES), THE PREPBUFR TABLE A ENTRY (PREPBUFR
C   FILES) (IN EITHER CASE WHEN THE MESSAGE IS KNOWN TO CONTAIN EITHER
C   ALL RESTRICTED OR ALL UNRESTRICTED REPORTS), THE DUMP REPORT TYPE
C   (WHEN REPORTS IN A PARTICULAR PREPBUFR TABLE A ENTRY ARE MASKED) OR
C   THE RESTRICTED FLAG (MNEMONIC "RSRD") WITHIN EACH REPORT IN A
C   MESSAGE (WHEN THE MESSAGE MAY CONTAIN A MIXTURE OF RESTRICTED AND
C   UNRESTRICTED REPORTS). (NOTE: THE CASE OF MASKING REPORT ID's IN
C   DATA DUMP FILES CAN ONLY BE DONE CURRENTLY FOR TABLE A ENTRIES
C   WHERE ALL REPORTS ARE CONSIDERED TO BE RESTRICTED.  THE CASE OF
C   MASKING REPORT ID's IN PREPBUFR FILES CAN ONLY BE DONE CURRENTLY
C   FOR TABLE A ENTRIES WHERE ALL REPORTS WITH A PARTICULAR SET OF DUMP
C   REPORT TYPES ARE CONSIDERED TO BE RESTRICTED.)
C
C PROGRAM HISTORY LOG:
C 2003-07-14  D. A. KEYSER -- ORIGINAL AUTHOR
C 2007-11-21  D. A. KEYSER -- ADDED OPTION TO MASK THE REPORT ID FOR
C     SPECIFIC TABLE A ENTRIES (AND POSSIBLY FOR SPECIFIC DUMP REPORT
C     TYPES WITHIN TABLE A ENTRIES WHEN PROCESSING A PREPBUFR FILE),
C     WHICH RETAINS RESTRICTED REPORTS OF THIS TYPE (RATHER THAN
C     TOSSING RESTRICTED REPORTS AS IN THE OTHER, PREVIOUS OPTIONS) BUT
C     CHANGES THEIR TRUE REPORT ID TO "MASKSTID" WHERE THE ID IS STORED
C     BY ITSELF (I.E., IN PREPBUFR FILES, MNEMONIC "SID"; IN DUMP
C     FILES, MNEMONIC "RPID" FOR ALL TYPES PLUS FOR SHIPS IN TABLE A
C     ENTRY "NC001001" MNEMONIC "SHPC8") AND REPLACES THE REPORT ID
C     WITH ALL "X"'s WHERE THE ID IS EMBEDDED IN THE REPLICATED RAW
C     BULLETIN HEADER STRING; USES NEW NAMELIST SWITCHES "MSG_MASKA"
C     TO IDENTIFY THOSE TABLE A ENTRIES FOR WHICH SOME OR ALL REPORTS
C     ARE CONSIDERED TO BE RESTRICTED AND MUST HAVE THEIR REPORT ID
C     MASKED AND, IN PREPBUFR FILES ONLY, "IMASK_T29" TO IDENTIFY THOSE
C     SPECIFIC DUMP REPORT TYPES WITHIN "MSG_MASKA" WHICH ARE
C     RESTRICTED AND MUST HAVE THEIR REPORT ID MASKED
C 2008-09-12  D. A. KEYSER -- INCREASED THE NUMBER OF MESSAGE TYPE
C     ENTRIES IN NAMELIST SWITCHES "MSG_RESTR", "MSG_MIXED" AND
C     "MSG_MASKA" FROM 10 TO 20 AND SET UP THEIR DEFAULT VALUES
C     (8 BLANK CHARACTERS) SO THAT ALL 20 VALUES DO NOT HAVE TO BE
C     SPECIFIED IN THE NAMELIST PASSED IN FROM THE EXECUTING SCRIPT
C 2013-03-15  JWhiting -- ported to WCOSS (no logic changes)
C 2017-01-06  DStokes  -- Added call of bufrlib routine SETBMISS to set
C      the missing value used for bufr reads and writes to 10E8_8.  
C      Function GETBMISS is then used to define variable BMISS rather
C      than using a hardwired value of 10E10. This change was made
C      to reduce the risk of integer overflows on WCOSS.
C 2017-10-20  D. A. KEYSER
C       - Added minutes to print statement everytime a new BUFR message
C         is read.  In the case of a PREPBUFR file this is the central
C         (analysis) time (which is the same) for every message.  In the
C         case of a DUMP file, this is the central dump time for the
C         first (dummy) message read in, the dump file creation time for
C         the second (dummy) message read in, and the message YYYYMMDDHH
C         (with minutes always zero) for all data messages read in.  The
C         minutes are obtained from Section 1 of the message read in via
C         call to BUFRLIB routine IUPVS01.
C         BENEFIT: Until now, central dump time and PREPBUFR (analysis)
C                  time minutes was always zero.  However, with the
C                  implementation of the new RTMA_RU, these times can
C                  now also have minutes = 15, 30 or 45 (since the
C                  RTMA_RU runs 4 times per hour). This change allows
C                  this print statement to reflect this new message time
C                  format.
C       -  Fixed a bug in cases where individual subsets in an input
C          message must be read in and checked/updated before being
C          written back out if not rejected altogether (see * below for
C          cases). Here code was opening output message via OPENMG which
C          does not take into account input message's Section 1 minutes
C          value (it thus opens a message with default of zero minutes
C          in Section 1 date). Updated to call BUFRLIB routine MINIMG in
C          such cases to encode non-zero minutes value (previously read
C          in via BUFRLIB routine IUPVS01, see first change above) into
C          Section 1 date when OPENMB actually does open a new message.
C          * - either the input message contains a mixture of restricted
C              and non-restricted reports or contains restricted reports
C              which can be made non-restricted by masking their id
C          BENEFIT: Applies only to PREPBUFR files for RTMA_RU runs at 
C                   15, 30 or 45 minutes past the hour: ensures that all
C                   messages in output file have the same (correct)
C                   center date out to minutes in Section 1 {rather than
C                   messages created via OPENMB (e.g., ADPSFC, SFCSHP in
C                   RTMA_RU runs) having Section 1 minutes of zero}.
C 2020-12-16  S. Melchior -- added call to bufrlib routine maxout for
C          gpsro data.  The default max message size of 10000 bytes is
C          not sufficient for GPS-RO data.
C          BENEFIT: Commercial GPS-RO data will be stripped from gpsro
C                   BUFR dump files for safe external user
C                   dissemination.
C 2021-01-26  S. MELCHIOR
C      - Included subset NC001101 in logic section that replaces station
C        ID w/ MASKSTID.
C        BENEFIT: BUFR format restricted ships data can be properly
C                 dispensed to public users.
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - DATA CARDS CONTAINING NAMELIST SWITCHES (SEE REMARKS)
C     UNIT 11  - INPUT BUFR FILENAME (IN CHARACTER) (USED ONLY FOR
C                DIAGNOSTIC PRINT INFO)
C     UNIT 21  - BUFR FILE (PREPBUFR OR DUMP) CONTAINING A MIXTURE OF
C                RESTRICTED AND UNRESTRICTED REPORTS
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - BUFR FILE (PREPBUFR OR DUMP) CONTAINING EITHER ONLY
C                UNRESTRICTED REPORTS OR UNRESTRICTED REPORTS AND
C                RESTRICTED REPORTS WHOSE REPORT ID's HAVE BEEN MASKED
C                {I.E., ALL OCCURRENCES OF ID IN A REPORT ARE
C                UNILATERALLY CHANGED TO EITHER "MASKSTID" (WHERE THE
C                ID IS STORED BY ITSELF) OR TO ALL "X"'s WHERE THE
C                NUMBER OF "X"'s CORRESPONDS TO THE THE NUMBER OF
C                CHARACTERS IN THE ORIGINAL REPORT ID (WHERE THE ID IS
C                EMBEDDED IN THE REPLICATED RAW REPORT BULLETIN HEADER
C                STRING), THE LATTER APPLY ONLY TO DUMP FILES}
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C
C     UNIQUE:    - NONE
C     LIBRARY:
C       W3LIB    - W3TAGB   W3TAGE   ERREXIT
C       BUFR     - DATELEN  OPENBF   IREADMG  UFBCNT  NMSUB
C                  SETBMISS GETBMISS iupvs01  minimg  maxout
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C            > 0 - PROBLEM
C
C REMARKS:
C   CONTENTS OF INPUT NAMELIST "SWITCHES":
C     MSG_RESTR - 20-WORD CHARACTER*8 ARRAY CONTAINING UP TO 20 BUFR
C                 MESSAGE TABLE A ENTRIES FOR WHICH ALL REPORTS ARE
C                 RESTRICTED AND WILL BE REMOVED (These messages are
C                 skipped over without unpacking any reports)
C     MSG_MIXED - 20-WORD CHARACTER*8 ARRAY CONTAINING UP TO 20 BUFR
C                 MESSAGE TABLE A ENTRIES WHICH CONTAIN A MIXTURE OF
C                 RESTRICTED AND UNRESTRICTED REPORTS BASED ON BUFR
C                 MNEMONIC "RSRD" - ALL RESTRICTED REPORTS WILL BE
C                 REMOVED (These messages must be unpacked and every
C                 report must be checked to see if it is restricted -
C                 unrestricted reports are copied, restricted reports
C                 are skipped over)
C     MSG_MASKA - 20-WORD CHARACTER*8 ARRAY CONTAINING UP TO 20 BUFR
C                 MESSAGE TABLE A ENTRIES FOR WHICH ALL REPORTS ARE
C                 RESTRICTED {BUT, FOR PREPBUFR FILES, ONLY IF THEIR
C                 DUMP REPORT TYPE IS ONE OF UP TO 10 POSSIBLE LISTED
C                 IN SWITCH IMASK_T29 (EACH LINE IN IMASK_T29 APPLIES
C                 TO THE TABLE A ENTRY IN THE SAME LINE NUMBER HERE);
C                 FOR DATA DUMP FILES, IMASK_T29 IS NOT USED, ALL
C                 REPORTS IN THE TABLE A ENTRIES HERE WILL BE
C                 RESTRICTED)} - THESE WILL NOT BE REMOVED, BUT ALL
C                 OCCURRENCES OF THEIR REPORT ID's WILL BE UNILATERALLY
C                 CHANGED TO EITHER "MASKSTID" (WHERE THE ID IS STORED
C                 BY ITSELF, PREPBUFR OR DUMP FILES) OR TO ALL "X"'s
C                 WHERE THE NUMBER OF "X"'s CORRESPONDS TO THE NUMBER
C                 OF CHARACTERS IN THE ORIGINAL REPORT ID (WHERE THE ID
C                 IS EMBEDDED IN THE REPLICATED RAW REPORT BULLETIN
C                 HEADER STRING, DUMP FILES ONLY) (These messages must
C                 be unpacked and every occurrence of every report's id
C                 must be changed to either "MASKSTID" or "X"'s before
C                 the report is copied)
C     IMASK_T29 - (10,20) INTEGER ARRAY CONTAINING UP TO 10 POSSIBLE
C                 DUMP REPORT TYPES (1ST DIMENSION) FOR THE UP TO 20
C                 POSSIBLE PREPBUFR TABLE A ENTRIES LISTED IN SWITCH
C                 MSG_MASKA (2ND DIMENSION) (APPLIES ONLY TO PREPBUFR
C                 FILES)
C
C    Note 1: A particular Table A entry should NEVER appear in more
C            than one of MSG_RESTR, MSG_MIXED or MSG_MASKA.
C    Note 2: Any Table A entry not in either MSG_RESTR, MSG_MIXED or
C            MSG_MASKA is assumed to be a Table A entry for BUFR
C            messages for which ALL reports are UNRESTRICTED (these
C            messages are copied intact, no reports are unpacked).
C    Note 3: Always fill in these arrays MSG_RESTR, MSG_MIXED and
C            MSG_MASKA beginning with word 1.  If there are less than
C            20 words filled in an array, either set the extra words to
C            "        " (8 blank characters) or do not specify them
C            here (they default to "        ").
C    Note 4: For data dump Table A entries in the form "NCtttsss",
C            where "ttt" is the BUFR message type and "sss" is the
C            BUFR message subtype, if the last three characters (the
C            subtype) is specified as 'xxx', then ALL BUFR messages
C            of that type are either treated as having all restricted
C            data all which is to be removed (if in MSG_RESTR), mixed
C            data some of which is to be removed (if in MSG_MIXED) or
C            all restricted data all of which is to have its report id
C            masked (if in MSG_MASKA), regardless of the message
C            subtype. (For example, if MSG_RESTR(1)='NC255xxx', then
C            ALL mesonet BUFR messages are considered to have all
C            restricted data and are all removed regardless of their
C            subtype.)
C    Note 5: For PREPBUFR files, a value of "99999" in array IMASK_T29
C            means not applicable whereas a value of "000" means
C            reports in all dump report types in the corresponding
C            Table A entry in MSG_MASKA should be restricted (masked)
C            {in this case IMASK_T29(1,x) should be set to 000 and
C            IMASK_T29(2:10,x) should be set to 99999 for all reports
C            in Table A entry MSG_MASKA(x) since they would all be
C            ignored - this is the default for all Table A entries
C            MSG_MASKA(1:20) if this is not set (i.e., for data dump
C            files)}
C      
C   LIST OF REPORT ID MNEMONICS IN EACH REPORT WHICH ARE CURRENTLY
C    MASKED WHEN TABLE A ENTRY IS FOUND IN MSG_MASKA (AND FOR PREPBUFR
C    FILES DUMP REPORT TYPE MATCHES ONE OF THE TYPES IN IMASK_T29):
C
C   PREPBUFR file: "SID"   - chgd to "MASKSTID" (all Tbl A entries)
C   DUMP file:     "RPID"  - chgd to "MASKSTID" (all Tbl A entries)
C   DUMP file:     "SHPC8" - chgd to "MASKSTID" (Tbl A entry NC001001)
C   DUMP file:     "RRSTG" - chgd to "X" (where the number of "X"'s
C                            corresponds to the the number of
C                            characters in the original report id )
C                            (all applicable Tbl A entries)
C
C   Note: Currently for dump files, the only Table A entry where all
C         occurrences of report id in a report are known to be masked
C         is NC001001.  This code may have to be modified to add this
C         ability to mask all occurrences of report id for other Table
C         A entries.
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS (iDataPlex and Cray-XC40)
C
C$$$

      PROGRAM BUFR_REMOREST

      CHARACTER*2040 RAWRPT_STG
      CHARACTER*80 FILE
      CHARACTER*8  SUBSET,SID,MSG_RESTR(20),MSG_MIXED(20),MSG_MASKA(20),
     $             RAWRPT(255),SID_orig
      REAL*8       RID_8(6),GETBMISS,BMISS,RASTR_8(255)
      INTEGER      IMASK_T29(10,20)

      EQUIVALENCE (RID_8(1),SID),(RAWRPT,RASTR_8)

      DATA  LUBFI/21/,LUBFJ/51/,IREC/0/,IRSUB/0/,IMSUB/0/,
     $ IUSUB/0/,ireco_last/0/

      NAMELIST/SWITCHES/MSG_RESTR,MSG_MIXED,MSG_MASKA,IMASK_T29

      CALL W3TAGB('BUFR_REMOREST',2021,0175,0012,'NP22')

      READ(11,'(Q,A)',END=1) NBYTES,FILE(1:NBYTES)

    1 CONTINUE

      PRINT 100, FILE(1:NBYTES)
  100 FORMAT(/15X,'WELCOME TO THE BUFR_REMOREST - THE PROGRAM THAT ',
     $ 'REMOVES OR MASKS RESTRICTED REPORTS FROM A BUFR FILE'/42X,
     $ 'LAST REVISION 24 Jun 2021'//30X,'INPUT BUFR FILENAME IS: ',A)

C  Set BUFRLIB missing (BMISS) to 10E8_8 to avoid integer*4 overflows
C  ------------------------------------------------------------------
      CALL SETBMISS(10E8_8)
      BMISS=GETBMISS()
      print'(1X)'
      print'(" BUFRLIB value for missing is: ",G0)', bmiss
      print'(1X)'

      IMASK_T29 = 99999
      IMASK_T29(1,:) = 000
      MSG_RESTR = '        '
      MSG_MIXED = '        '
      MSG_MASKA = '        '
      READ(5,SWITCHES)

      IF(MSG_RESTR(1).NE.'        ')  THEN
         PRINT 107
  107 FORMAT(//' ANY BUFR MESSAGES READ IN WITH THE FOLLOWING TABLE A ',
     $ 'ENTRIES ARE SKIPPED (WITHOUT UNPACKING) BECAUSE THEY CONTAIN ',
     $ 'ONLY'/' RESTRICTED REPORTS ALL OF WHICH ARE TO BE REMOVED:'/)
         DO I = 1,20
            IF(MSG_RESTR(I).EQ.'        ')  EXIT
            PRINT *, MSG_RESTR(I)
         ENDDO
      END IF
      IF(MSG_MIXED(1).NE.'        ')  THEN
         PRINT 108
  108 FORMAT(//' ANY BUFR MESSAGES READ IN WITH THE FOLLOWING TABLE A ',
     $ 'ENTRIES ARE UNPACKED REPORT BY REPORT BECAUSE THEY CAN'/
     $ ' CONTAIN A MIXTURE OF BOTH UNRESTRICTED AND RESTRICTED REPORTS',
     $ ' - ALL RESTRICTED REPORTS ARE TO BE REMOVED:'/)
         DO I = 1,20
            IF(MSG_MIXED(I).EQ.'        ')  EXIT
            PRINT *, MSG_MIXED(I)
         ENDDO
      END IF
      IF(MSG_MASKA(1).NE.'        ')  THEN
         PRINT 1107
 1107 FORMAT(//' ANY BUFR MESSAGES READ IN WITH THE FOLLOWING TABLE A ',
     $ 'ENTRIES ARE UNPACKED REPORT BY REPORT BECAUSE THEY CAN'/
     $ ' CONTAIN EITHER ALL OR SOME RESTRICTED REPORTS WHOSE REPORT ',
     $ 'ID''s (ALL OCCURRENCES IN A REPORT) ARE ALL CHANGED'/' TO ',
     $ '"MASKSTID" OR "X"''S (MASKED) PRIOR TO THEIR BEING WRITTEN ',
     $ 'BACK OUT:'/)
         DO I = 1,20
            IF(MSG_MASKA(I).EQ.'        ')  EXIT
            PRINT *, MSG_MASKA(I)
         ENDDO
      END IF

      PRINT 109
  109 FORMAT(//' ALL OTHER BUFR MESSAGES READ IN ARE COPIED INTACT ',
     $ '(WITHOUT UNPACKING) BECAUSE THEY CONTAIN ONLY UNRESTRICTED ',
     $ 'REPORTS'//)

      CALL DATELEN(10)
      CALL OPENBF(LUBFI,'IN',LUBFI)
      PRINT 101, LUBFI,LUBFI
  101 FORMAT(/5X,'===> BUFR FILE IN UNIT',I3,' SUCCESSFULLY OPENED ',
     $ 'FOR INPUT; BUFR TABLE IS OBTAINED INTERNALLY FROM UNIT',I3/)
      CALL OPENBF(LUBFJ,'OUT',LUBFI)
      PRINT 102, LUBFJ,LUBFI,LUBFJ
  102 FORMAT(/5X,'===> BUFR FILE IN UNIT',I3,' SUCCESSFULLY OPENED FOR',
     $ ' OUTPUT; BUFR TABLE IS OBTAINED FROM UNIT',I3,' AND STORED'/10X,
     $ 'INTERNALLY INTO UNIT',I3/)

C  CHECK TO MAKE SURE THE SAME TABLE A ENTRY DOES NOT APPEAR IN MORE
C   THAN ONE OF THE ARRAYS MSG_RESTR, MSG_MIXED OR MSG_MASKA
C  -----------------------------------------------------------------

      DO I = 1,20
         IF(MSG_RESTR(I).EQ.'        ')  EXIT
         DO J = 1,20
            IF(MSG_MIXED(J).EQ.'        ')  EXIT
            IF(MSG_RESTR(I).EQ.MSG_MIXED(J))  THEN
               PRINT 105, I,MSG_RESTR(I),J
  105 FORMAT('#####BUFR_REMOREST: WORD ',I2,' OF NAMELIST VARIABLE ',
     $ 'MSG_RESTR CONTAINS THE SAME TABLE A ENTRY (=',A,') AS WORD ',I2,
     $  'OF NAMELIST VARIABLE MSG_MIXED - STOP 99')
               CALL W3TAGE('BUFR_REMOREST')
               CALL ERREXIT(99)
            END IF
            IF(MSG_RESTR(I).EQ.MSG_MASKA(J))  THEN
               PRINT 1105, I,MSG_RESTR(I),J
 1105 FORMAT('#####BUFR_REMOREST: WORD ',I2,' OF NAMELIST VARIABLE ',
     $ 'MSG_RESTR CONTAINS THE SAME TABLE A ENTRY (=',A,') AS WORD ',I2,
     $  'OF NAMELIST VARIABLE MSG_MASKA - STOP 99')
               CALL W3TAGE('BUFR_REMOREST')
               CALL ERREXIT(99)
            END IF
         ENDDO
      ENDDO
      DO I = 1,20
         IF(MSG_MIXED(I).EQ.'        ')  EXIT
         DO J = 1,20
            IF(MSG_MASKA(J).EQ.'        ')  EXIT
            IF(MSG_MIXED(I).EQ.MSG_MASKA(J))  THEN
               PRINT 2105, I,MSG_MIXED(I),J
 2105 FORMAT('#####BUFR_REMOREST: WORD ',I2,' OF NAMELIST VARIABLE ',
     $ 'MSG_MIXED CONTAINS THE SAME TABLE A ENTRY (=',A,') AS WORD ',I2,
     $  'OF NAMELIST VARIABLE MSG_MASKA - STOP 99')
               CALL W3TAGE('BUFR_REMOREST')
               CALL ERREXIT(99)
            END IF
         ENDDO
      ENDDO

C  READ IN NEXT INPUT BUFR MESSAGE FROM BUFR FILE
C  ----------------------------------------------

      LOOP1: DO WHILE(IREADMG(LUBFI,SUBSET,IDATE).EQ.0)
 
C  For GPS-RO, increase max message length
         if(subset.eq.'NC003010') then
           call maxout(200000)
         end if

         CALL UFBCNT(LUBFI,IREC,ISUB)
         ISUB = NMSUB(LUBFI)
         iminu = iupvs01(lubfi,'MINU')
         PRINT 103, IREC,SUBSET,IDATE,iminu,ISUB
  103    FORMAT(/5X,'===> READ IN BUFR DATA MESSAGE NO. ',I5,' - TABLE',
     $    ' A ENTRY IS ',A8,' DATE IS',I11,':',i2.2,'  NO. OF RPTS IN ',
     $    'MESSAGE IS',I6)

         LOOP1n1: DO I = 1,20
            IF(MSG_RESTR(I).EQ.MSG_MIXED(I) .AND.
     $         MSG_MIXED(I).EQ.MSG_MASKA(I))  THEN
               EXIT LOOP1n1 ! All are "        " - no need to test
C**********************************************************************
            ELSE IF(IREC.LE.2.AND.SUBSET(1:2).EQ.'NC'.AND.ISUB.EQ.0)
     $       THEN
               PRINT 111
  111 FORMAT(' This is a dummy message at the top of a data dump BUFR ',
     $ 'file containing the center dump time (record 1) or the dump'/
     $ ' processing time (record 2); irregardless of any restriction ',
     $ 'switch on this message, copy it, intact, to output BUFR file')
               CALL CLOSMG(LUBFJ)
               CALL COPYMG(LUBFI,LUBFJ)
               CYCLE LOOP1
C**********************************************************************
            ELSE IF(SUBSET.EQ.MSG_RESTR(I) .OR. (MSG_RESTR(I)(6:8).EQ.
     $       'xxx'.AND.SUBSET(1:5).EQ.MSG_RESTR(I)(1:5))) THEN
               PRINT 112
  112 FORMAT(' #####>>>> ALL reports in this message are RESTRICTED ',
     $ 'and are REMOVED - do NOT copy this message to output BUFR file')
               IRSUB = IRSUB + ISUB
               CYCLE LOOP1
C**********************************************************************
            ELSE IF(SUBSET.EQ.MSG_MIXED(I) .OR. (MSG_MIXED(I)(6:8).EQ.
     $       'xxx'.AND.SUBSET(1:5).EQ.MSG_MIXED(I)(1:5))) THEN
               PRINT 113
  113 FORMAT(' #####>>>> This msg has mixture of restricted/non-',
     $ 'restricted data, restricted data are REMOVED- unpk each rpt & ',
     $ 'test on mnem. "RSRD"')

C  READ A SUBSET (REPORT) IN MESSAGE
C  ---------------------------------

               LOOP1n2: DO WHILE(IREADSB(LUBFI).EQ.0)

C  DECODE THE SUBSET (REPORT) LOOKING FOR RESTRICTED FLAG
C   (mnemonic "RSRD")
C  ------------------------------------------------------

                  CALL UFBINT(LUBFI,RID_8,6,1,NLV,
     $                        'SID RPT YOB XOB TYP RSRD')
                  IF(RID_8(6).GT.0.AND.RID_8(6).LT.BMISS/2)  THEN
                     IF(RID_8(5).LT.BMISS/2)  THEN

C  Normally for PREPBUFR files
C  ---------------------------

                        PRINT 104, SID,(RID_8(II),II=2,4),
     $                   (NINT(RID_8(II)),II=5,6)
  104 FORMAT(10X,'- Skip report ',A8,' at ',F6.2,' UTC, ',F6.2,
     $ ' (N+/S-) LAT, ',F7.2,'(E) LON, RTYP= ',I3,', RSRD=',I5)
                     ELSE

C  Normally for DATA DUMP files
C  ----------------------------

                        CALL UFBINT(LUBFI,RID_8,5,1,NLV,
     $                              'RPID HOUR MINU CLAT CLON')
                           PRINT 110, SID,(NINT(RID_8(II)),II=2,3),
     $                      (RID_8(II),II=4,5), NINT(RID_8(6))
  110 FORMAT(10X,'- Skip report ',A8,' at ',2(I2.2),' UTC, ',F6.2,
     $ ' (N+/S-) LAT, ',F7.2,'(E+/W-) LON, RSRD=',I5)
                     END IF
                     IRSUB = IRSUB + 1
                     CYCLE  LOOP1n2
                  END IF

                  CALL OPENMB(LUBFJ,SUBSET,IDATE)
                  call ufbcnt(lubfj,ireco,isubo)
                  if(ireco.ne.ireco_last) then
C Encode minutes into Sec. 1 of new output message header if non-zero
                     if(iminu.ne.0) call minimg(lubfj,iminu)
                  end if
                  ireco_last  = ireco
                  CALL UFBCPY(LUBFI,LUBFJ)
                  CALL WRITSB(LUBFJ)
                  IUSUB = IUSUB + 1
               ENDDO  LOOP1n2
               CYCLE  LOOP1
C**********************************************************************
            ELSE IF(SUBSET.EQ.MSG_MASKA(I) .OR. (MSG_MASKA(I)(6:8).EQ.
     $       'xxx'.AND.SUBSET(1:5).EQ.MSG_MASKA(I)(1:5))) THEN
               IF(IMASK_T29(1,I).EQ.000)  THEN
                  PRINT 1113
 1113 FORMAT(' #####>>>> ALL rpts in msg RESTRICTED - unpk each rpt, ',
     $ 'MASK all occurrences of id (chg to "MASKSTID" or "X"''s), copy',
     $ ' to output file')
               ELSE
                  PRINT 1114
 1114 FORMAT(' #####>>>> SOME or ALL rpts in msg RESTRICTED:')
                  DO J = 1,10
                     IF(IMASK_T29(J,I).NE.99999)  PRINT 1115,
     $                IMASK_T29(J,I)
 1115 FORMAT('  -- ALL rpts in dump type ',I3,' RESTRICTED- unpk',
     $ ' each rpt, MASK all occurrences of id (chg to "MASKSTID" or ',
     $ '"X"s), copy to o-put file')
                  ENDDO
               END IF

C  READ A SUBSET (REPORT) IN MESSAGE
C  ---------------------------------

               LOOP1n3: DO WHILE(IREADSB(LUBFI).EQ.0)

C  DECODE THE SUBSET (REPORT) IN ORDER TO OBTAIN THE REPORT ID
C  -----------------------------------------------------------

                  CALL OPENMB(LUBFJ,SUBSET,IDATE)
                  call ufbcnt(lubfj,ireco,isubo)
                  if(ireco.ne.ireco_last) then
C Encode minutes into Sec. 1 of new output message header if non-zero
                     if(iminu.ne.0) call minimg(lubfj,iminu)
                  end if
                  ireco_last  = ireco
                  CALL UFBCPY(LUBFI,LUBFJ)
                  CALL UFBINT(LUBFI,RID_8,6,1,NLV,
     $                        'SID RPT YOB XOB TYP T29')
                  IF(RID_8(5).LT.BMISS/2)  THEN

C  Come here for PREPBUFR files (report id is in mnemonic "SID")
C   -- check for a match of dump report type
C  -------------------------------------------------------------

                     DO J = 1,10
                        IF(IMASK_T29(1,I).EQ.000.OR.
     $                     IMASK_T29(J,I).EQ.NINT(RID_8(6)))  THEN

C    .... dump report types match - this report is restricted
C         ---------------------------------------------------

                           PRINT 1104, SID,(RID_8(II),II=2,4),
     $                      (NINT(RID_8(II)),II=5,6)
 1104 FORMAT(10X,'- Change id of report ',A8,' at ',F6.2,' UTC, ',F6.2,
     $ ' (N+/S-) LAT, ',F7.2,'(E) LON, RTYP= ',I3,', DTYP=',I5,
     $ ' TO "MASKSTID"')
                           SID = 'MASKSTID'

C    .... update report id to masked value - "MASKSTID"
C         ---------------------------------------------

                           CALL UFBINT(LUBFJ,RID_8(1),1,1,IRET,'SID')
                           EXIT
                        END IF
                     END DO
                  ELSE

C  Come here for DATA DUMP files where all reports are restricted
C   {report id is in mnemonic "RPID" (and for surface ship reports in
C   Table A entry 'NC001001' also in mnemonic "SHPC8"), report id may
C   also be embedded in replicated raw bulletin header string if it is
C   present)
C  -------------------------------------------------------------------

                     IUPDATE_RAWRPT = 0
                     CALL UFBINT(LUBFI,RID_8,6,1,NLV,
     $                           'RPID HOUR MINU CLAT CLON RSRD')
                     PRINT 1110, SID,(NINT(RID_8(II)),II=2,3),
     $                (RID_8(II),II=4,5), NINT(RID_8(6))
 1110 FORMAT('  - Chg all occurrences of id of rpt ',A8,' at ',2(I2.2),
     $ ' UTC, ',F6.2,' (N+/S-) LAT, ',F7.2,'(E+/W-) LON, RSRD=',I5,
     $ ' TO "MASKSTID" OR "X"''s')
                     SID_orig = SID
                     SID = 'MASKSTID'

C    .... update report id in output file to masked value - "MASKSTID"
C         ------------------------------------------------------------

                     CALL UFBINT(LUBFJ,RID_8(1),1,1,IRET,'RPID')
                     IF(SUBSET.EQ.'NC001001'.or.
     $                  subset.eq.'NC001101') THEN

C    .... update 2nd rpt id in ship rpts in output file to masked value
C         - "MASKSTID"
C         -------------------------------------------------------------

                        CALL UFBINT(LUBFJ,RID_8(1),1,1,IRET,'SHPC8')
                     END IF

C    .... see if a replicated raw report bulletin header string is
C         present
C         --------------------------------------------------------

                     CALL UFBINT(LUBFI,RASTR_8,1,255,NLV,'RRSTG')
                     IST = 1
                     IF(NLV.GT.0) THEN

C        .... it is, store entire string in character array RAWRPT_STG
C             --------------------------------------------------------

                        DO II = 1,NLV
                           IEN = IST+7
                           RAWRPT_STG(IST:IEN) = RAWRPT(II)
                           IST = IST + 8
                        ENDDO
cppppp
ccc   print *, 'orig RAWRPT_STG: "',RAWRPT_STG(1:IEN),'", IEN = ',ien
cppppp

C        .... next determine character length of report id (ICHAR_id)
C             for later check to see if it is embedded one or more
C             times in raw report bulletin header string
C             -------------------------------------------------------

                        DO II = 1,8
                           IF(SID_orig(II:II).EQ.' ') THEN
                              ICHAR_id = II - 1
                              EXIT
                           END IF
                        ENDDO
cppppp
ccc   print *, 'original sid had ',ICHAR_id,' characters'
cppppp
                        IF(ICHAR_id.GE.1) THEN

C        .... next parse through entire raw report bulletin header
C             string looking for one or more occurrences of report id
C             and "X" out the id
C             -------------------------------------------------------

                           DO II = 1,IEN
                              IF(II+ICHAR_id-1.GT.IEN)  EXIT
                              IF(RAWRPT_STG(II:II+ICHAR_id-1).EQ.
     $                           SID_orig(1:ICHAR_id)) THEN
cppppp
ccc   print *, 'Found a match to orig sid in RAWRPT_STG',' - in bytes ',
ccc  $ II,' to ',II+ICHAR_id-1,' - set to "X"'
cppppp
                                 DO JJ = II,II+ICHAR_id-1
                                    RAWRPT_STG(JJ:JJ) = 'X'
                                 ENDDO
                                 IUPDATE_RAWRPT = 1
                              END IF
                           ENDDO

C        .... reconstruct replicated raw report bulletin header string,
C             but now with the id "X"'d out
C             ---------------------------------------------------------

                           IF(IUPDATE_RAWRPT.EQ.1)  THEN
                              IST = 1
                              DO II = 1,NLV
                                 IEN = IST+7
                                 RAWRPT(II) = RAWRPT_STG(IST:IEN)
                                 IST = IST + 8
                              ENDDO

C        .... finally, update replicated raw report bulletin header
C             string with the id "X"'d out in output file
C             -----------------------------------------------------

                              CALL UFBINT(LUBFJ,RASTR_8,1,NLV,IRET,
     $                                    'RRSTG')
cppppp
ccc   print *, 'updt RAWRPT_STG: "',RAWRPT_STG(1:IEN),'", IEN = ',ien
cppppp
                           END IF
                        END IF
                     END IF
                  END IF
                  IF(SID.EQ.'MASKSTID') THEN
                     IMSUB = IMSUB + 1
                  ELSE
                     IUSUB = IUSUB + 1
                  END IF
                  CALL WRITSB(LUBFJ)
               ENDDO  LOOP1n3
               CYCLE  LOOP1
            END IF
         ENDDO  LOOP1n1
C**********************************************************************
         PRINT 114
  114 FORMAT(' ALL reports in this message are UNRESTRICTED - copy',
     $ ' this message, intact, to output BUFR file')
         IUSUB = IUSUB + ISUB
         CALL CLOSMG(LUBFJ)
         CALL COPYMG(LUBFI,LUBFJ)
C**********************************************************************
      ENDDO  LOOP1
            
C  ALL MESSAGES IN INPUT PREPBUFR/DATA DUMP FILE HAVE BEEN READ AND
C   PROCESSED
C  ----------------------------------------------------------------

      CALL CLOSBF(LUBFI)
      CALL CLOSBF(LUBFJ)

      PRINT 106, IRSUB,IMSUB,IUSUB
  106 FORMAT(//'==> A TOTAL OF',I11,' RESTRICTED   REPORTS WERE SKIPPED'
     $        /'==> A TOTAL OF',I11,' RESTRICTED   REPORTS WERE MASKED',
     $ ' (ALL OCCURRENCES OF REPORT ID) THEN COPIED'
     $        /'==> A TOTAL OF',I11,' UNRESTRICTED REPORTS WERE COPIED'/
     $ /'PROGRAM COMPLETED SUCCESSFULLY'/)

      CALL W3TAGE('BUFR_REMOREST')

      STOP

      END

