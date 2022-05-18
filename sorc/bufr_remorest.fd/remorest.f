C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM:  BUFR_REMOREST
C   PRGMMR: Keyser           ORG: EMC         DATE: 2017-10-20
C
C ABSTRACT: THIS PROGRAM READS THROUGH AN INPUT BUFR FILE (NORMALLY
C   EITHER A PREPBUFR OR A DATA DUMP FILE) WHICH CAN CONTAIN A MIXTURE
C   OF REPORTS WHICH ARE UNRESTRICTED OR RESTRICTED FOR AT LEAST
C   SOME PERIOD OF TIME (W.R.T. REDISTRIBUTION OUTSIDE OF NCEP) AND
C   EITHER WRITES OUT (TO AN OTHERWISE IDENTICAL BUFR FILE) ONLY THOSE
C   REPORTS WHICH ARE NON-RESTRICTED OR WRITES OUT ALL REPORTS BUT WITH
C   MASKED REPORT ID's FOR THOSE REPORTS WHICH ARE RESTRICTED (WHAT IT
C   DOES IS BASED ON NAMELIST SWITCHES). {NOTE: WHEN A RESTRICTED
C   REPORT IS WRITTEN OUT WITH A MASKED REPORT ID, ITS RESTRICTION FLAG
C   (MNEMONIC "RSRD") AND ITS NUMBER OF HOURS UNTIL THE RESTRICTION
C   EXPIRES (MNEMONIC "EXPRSRD") ARE RE-SET TO MISSING SO THAT THE
C   REPORT IS NO LONGER CONSIDERED TO BE RESTRICTED.} IT DETERMINES 
C   WHICH REPORTS ARE RESTRICTED BASED ON EITHER THE MESSAGE TYPE AND
C   SUBTYPE (MAKING UP THE TABLE A ENTRY IN DATA DUMP FILES), THE
C   PREPBUFR TABLE A ENTRY (PREPBUFR FILES) (IN EITHER CASE WHEN THE
C   MESSAGE IS KNOWN TO CONTAIN EITHER ALL RESTRICTED OR ALL NON-
C   RESTRICTED REPORTS), THE DUMP REPORT TYPE (WHEN REPORTS IN A
C   PARTICULAR PREPBUFR TABLE A ENTRY ARE MASKED) OR THE REPORT'S
C   VALUE FOR "RSRD") AND, IF "RSRD" IS SET, ITS VALUE FOR "EXPRSRD"
C   WITHIN EACH REPORT IN A MESSAGE (WHEN THE MESSAGE MAY CONTAIN A
C   MIXTURE OF RESTRICTED AND NON-RESTRICTED REPORTS). (NOTE: THE CASE
C   OF MASKING REPORT ID's IN DATA DUMP FILES CAN ONLY BE DONE
C   CURRENTLY FOR TABLE A ENTRIES WHERE ALL REPORTS ARE CONSIDERED TO
C   BE RESTRICTED.
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
C XXXX-XX-XX  D. A. KEYSER -- All reports in message types in namelist
C     "MSG_MIXED" are now also tested for their value for EXPRSRD
C     (number of hours until the restriction expires) when their
C     restriction flag (RSRD) is set - any reports with a non-missing
C     EXPRSRD less than the difference in hours between the current
C     UTC wall-clock date and the BUFR file center time ("DIFF_HR")
C     minus 4 are now not considered restricted and are copied (prior
C     to this, the value of EXPRSRD was ignored and all reports with
C     RSRD set were restricted and skipped), "DIFF_HR" is a new
C     imported script environment variable; Improved information that
C     is printed out for each report that is either skipped or retained
C     (latter is currently commented out); Improved information printed
C     out at end summarizing counts of reports retained, skipped or
C     masked
C XXXX-XX-XX  D. A. KEYSER -- 
C          - For PREPBUFR files only, all reports in message types in
C     namelist "MSG_MASK" (if their dump report type is listed in
C     namelist "IMASK_T29") are now tested for their values for both
C     RSRD and EXPRSRD and are only considered to be restricted if
C     RSRD is set and EXPRSRD is .GE. the difference in hours between
C     the current UTC wall-clock date and the BUFR file center time
C     (read in via imported script environment variable "DIFF_HR")
C     minus 4.  (Prior to this RSRD and EXPRSRD were not examined, ALL
C     reports from the message type having the listed dump report type
C     were considered to be restricted.  This is still the case for
C     message types in namelist "MSG_MASK" for DUMP files.)
C          - When a report coming out of "MSG_MASK" is deemed to be
C     restricted (either PREPBUFR or DUMP) and its id is masked to be
C     "MASKSTID", the report's value for RSRD is now re-set to MISSING
C     when copied to non-restricted file (EXPRSRD is also set to
C     MISSING, but it likely was already MISSING).
C          - Improved documentation and printout.
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - DATA CARDS CONTAINING NAMELIST SWITCHES (SEE REMARKS)
C     UNIT 11  - INPUT BUFR FILENAME (IN CHARACTER) (USED ONLY FOR
C                DIAGNOSTIC PRINT INFO)
C     UNIT 21  - BUFR FILE (PREPBUFR OR DUMP) CONTAINING A MIXTURE OF
C                RESTRICTED (AT LEAST FOR SOME PERIOD FO TIME) AND 
C                NON-RESTRICTED REPORTS
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - BUFR FILE (PREPBUFR OR DUMP) CONTAINING EITHER ONLY
C                NON-RESTRICTED REPORTS OR NON-RESTRICTED REPORTS AND
C                PREVIOUSLY RESTRICTED REPORTS WHOSE REPORT ID's HAVE 
C                BEEN MASKED {I.E., ALL OCCURRENCES OF ID IN A REPORT
C                ARE UNILATERALLY CHANGED TO EITHER "MASKSTID" (WHERE
C                THE ID IS STORED BY ITSELF) OR TO ALL "X"'s WHERE THE
C                NUMBER OF "X"'s CORRESPONDS TO THE THE NUMBER OF
C                CHARACTERS IN THE ORIGINAL REPORT ID (WHERE THE ID IS
C                EMBEDDED IN THE REPLICATED RAW REPORT BULLETIN HEADER
C                STRING), THE LATTER APPLY ONLY TO DUMP FILES}
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C
C     UNIQUE:    - NONE
C     SYSTEM:    - GET_ENVIRONMENT_VARIABLE
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
C                 CONSIDERED TO BE RESTRICTED AND WILL ALWAYS BE
C                 REMOVED (These messages are skipped over without
C                 unpacking any reports)
C     MSG_MIXED - 20-WORD CHARACTER*8 ARRAY CONTAINING UP TO 20 BUFR
C                 MESSAGE TABLE A ENTRIES WHICH MAY CONTAIN A MIXTURE
C                 OF REPORTS WITH AND WITHOUT THEIR RESTRICTION
C                 INDICATOR (BUFR MNEMONIC "RSRD") BEING SET. IF "RSRD"
C                 IS NOT SET -OR- IT IS SET AND THE TIME IN HOURS FOR
C                 THE EXPIRATION ON RESTRICTION (BUFR MNEMONIC
C                 "EXPRSRD") IS ALSO SET AND HAS A VALUE LESS THAN
C                 "DIFF_HR" (THE DIFFERENCE IN HOURS BETWEEN THE
C                 CURRENT UTC WALL-CLOCK DATE AND THE BUFR FILE CENTER
C                 TIME) MINUS 4, THE REPORT WILL BE RETAINED.
C                 OTHERWISE, IT WILL BE REMOVED. (These messages must
C                 be unpacked and values for "RSRD" and "EXPRSRD" must
C                 be checked for every report. If "EXPRSRD is missing,
C                 it is set to 99999999 hours essentially meaning the
C                 report is restricted for all time if "RSRD" is set.)
C     MSG_MASKA - FOR PREPBUFR FILES:
C                 20-WORD CHARACTER*8 ARRAY CONTAINING UP TO 20 BUFR
C                 MESSAGE TABLE A ENTRIES WHICH, IF THEIR DUMP REPORT
C                 TYPE IS ONE OF UP TO 10 POSSIBLE LISTED IN SWITCH
C                 IMASK_T29 (WHERE EACH LINE IN IMASK_T29 APPLIES TO
C                 THE TABLE A ENTRY IN THE SAME LINE NUMBER HERE), MAY
C                 CONTAIN A MIXTURE OF REPORTS WITH AND WITHOUT THEIR
C                 RESTRICTION INDICATOR (BUFR MNEMONIC "RSRD") BEING
C                 SET. IF "RSRD" IS NOT SET FOR A REPORT -OR- IT IS SET
C                 AND THE TIME IN HOURS FOR THE EXPIRATION ON
C                 RESTRICTION (BUFR MNEMONIC "EXPRSRD") IS ALSO SET AND
C                 HAS A VALUE LESS THAN "DIFF_HR" (THE DIFFERENCE IN
C                 HOURS BETWEEN THE CURRENT UTC WALL-CLOCK DATE AND THE
C                 PREPBUFR FILE CENTER TIME) MINUS 4, THE REPORT WILL
C                 BE COPIED WITHOUT ANY CHANGES.  OTHERWISE, THE REPORT
C                 WILL NOT BE REMOVED, BUT ALL OCCURRENCES OF ITS ID
C                 WILL BE CHANGED TO "MASKSTID". IN ADDITION, ITS
C                 VALUES FOR "RSRD" AND "EXPRSRD" WILL BE RE-SET TO
C                 MISSING SO THAT THE REPORT WILL NO LONGER BE
C                 CONSIDERED AS RESTRICTED.  REPORTS WITH A DUMP REPORT
C                 TYPE NOT LISTED IN SWITCH IMASK_T29 ARE CONSIDERED TO
C                 BE NON-RESTRICTED AND THEIR REPORT IDS ARE NOT
C                 CHANGED (MASKED OUT) WHEN COPIED.  (These m essages
C                 must be unpacked and the values for "T29", "RSRD" and
C                 "EXPRSRD" must be checked for every report. If
C                 "EXPRSRD" is missing, it is set to 99999999 hours
C                 essentially meaning the report is restricted for all
C                 time if "RSRD" is set.)
C                 FOR DATA DUMP FILES:
C                 20-WORD CHARACTER*8 ARRAY CONTAINING UP TO 20 BUFR
C                 MESSAGE TABLE A ENTRIES FOR WHICH ALL REPORTS ARE
C                 CONSIDERED TO BE RESTRICTED. THEY WILL NOT BE
C                 REMOVED, BUT ALL OCCURRENCES OF THEIR REPORT IDS WILL
C                 BE UNILATERALLY CHANGED TO EITHER "MASKSTID" (WHERE
C                 THE ID IS STORED BY ITSELF) OR TO ALL "X"'s WHERE THE
C                 NUMBER OF "X"'s CORRESPONDS TO THE NUMBER OF
C                 CHARACTERS IN THE ORIGINAL REPORT ID (WHERE THE ID IS
C                 EMBEDDED IN THE RAW REPORT BULLETIN HEADER STRING).
C                 IN ADDITION, THEIR VALUES FOR "RSRD" AND "EXPRSRD"
C                 WILL BE RE-SET TO MISSING SO THAT THE REPORTS WILL NO
C                 LONGER BE CONSIDERED AS RESTRICTED.  (These messages
C                 must be unpacked and every occurrence of every
C                 report's id must be changed to either "MASKSTID" or
C                 "X"'s and every report's "RSRD" and "EXPRSRD" values
C                 must be changed to missing before the report is
C                 copied.  Switch IMASK_T29 is not considered here.)
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
C            messages for which ALL reports are NON-RESTRICTED (these
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
C            Table A entry in MSG_MASKA should be considered {in this
C            case IMASK_T29(1,x) should be set to 000 and
C            IMASK_T29(2:10,x) should be set to 99999 for all reports
C            in Table A entry MSG_MASKA(x) since they would all be
C            ignored - this is the default for all Table A entries
C            MSG_MASKA(1:20) if this is not set (i.e., for data dump
C            files)}
C      
C   LIST OF REPORT ID MNEMONICS IN EACH REPORT WHICH ARE CURRENTLY
C    MASKED WHEN TABLE A ENTRY IS FOUND IN MSG_MASKA (AND FOR PREPBUFR
C    FILES DUMP REPORT TYPE MATCHES ONE OF THE TYPES IN IMASK_T29
C    AND THE REPORT IS CONSIDERED TO BE RESTRICTED BASED ON ITS VALUES
C    FOR "RSRD" and "EXPRSRD"):
C
C   PREPBUFR file: "SID"   - chgd to "MASKSTID" (all Tbl A entries)
C  (PREPBUFR file  "RSRD" and "EXPRSRD" also set to missing)
C   DUMP file:     "RPID"  - chgd to "MASKSTID" (all Tbl A entries)
C   DUMP file:     "SHPC8" - chgd to "MASKSTID" (Tbl A entry NC001001
C                            and NC001101)
C   DUMP file:     "RRSTG" - chgd to "X" (where the number of "X"'s
C                            corresponds to the the number of
C                            characters in the original report id )
C                            (all applicable Tbl A entries)
C  (DUMP file:     "RSRD" and "EXPRSRD" also set to missing)
C
C   Note: Currently for dump files, the only Table A entry where all
C         occurrences of report id in a report are known to be masked
C         is NC001001 and NC001101.  This code may have to be modified
C         to add this ability to mask all occurrences of report id for
C         other Table A entries.
C
C
C ONE SCRIPT ENVIRONMENT VARIABLE IS READ IN:
C            DIFF_HR - The difference in hours between the current UTC
C                      wall-clock date and the BUFR file center time
C                      (should always be a positive number!). This
C                      is used (after subtracting 4 hours***) to
C                      determine if a BUFR subset that is marked as
C                      restricted (via mnemonic "RSRD") is past the
C                      expiration time of the restriction (mnemonic
C                      "EXPRSRD") and should thus not be filtered out.
C                      (Note: used only for BUFR DUMP and PREPBUFR
C                      subsets in message types listed in namelist
C                      switch MSG_MIXED and for PREPBUFR subsets in
C                      applicalbe dump types and message types listed
C                      in namelist switches IMASK_T29 and MSG_MASKA.)
C                      Defaults to ZERO if not found (i.e., not
C                      exported by the executing script).
C                 *** Four hours is subtracted from DIFF_HR prior to
C                     testing against the BUFR file center time in
C                     order to account for some reports having an obx
C                     time as much as 3-4 hours prior to the center
C                     time in either a dump or PEPBUFR file.  This
C                     ensures that these reports are not inadvertently
C                     retained if the difference between the current
C                     wall-clock date and the BUFR file center time is
C                     very close to the time period of the restriction.
C                     DIFF_HR minus 4 can never be less than zero.
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
      CHARACTER*8  DIFF_HR,PREPBUFR_MSGTYP(21),PRVSTG_prep,SPRVSTG_prep,
     $             PRVSTG_dump,SPRVSTG_dump
      CHARACTER*8  SUBSET,SID,MSG_RESTR(20),MSG_MIXED(20),MSG_MASKA(20),
     $             RAWRPT(255),SID_orig
      REAL*8       RID_8(8),RASTR_8(255),LALOH_8(2),ACRN_8,ACID_8,SID_8,
     $             RPID_8,PRV_prep_8(2),PRV1_dump_8(255),
     $             PRV2_dump_8(255),BMISS,GETBMISS,rsrd_8(2)
      INTEGER      IMASK_T29(10,20),IRSUB_this_MR(20),IRSUB_this_MM(20),
     $             IMSUB_this(10,20),IRSUB_this_sub_MR(20,0:255),
     $             IRSUB_this_sub_MM(20,0:255),IUSUB_this(0:255,0:256)

      EQUIVALENCE (RID_8(1),SID),(RAWRPT,RASTR_8),
     $            (PRV_prep_8(1),PRVSTG_prep),
     $            (PRV_prep_8(2),SPRVSTG_prep),
     $            (PRV1_dump_8(1),PRVSTG_dump),
     $            (PRV2_dump_8(1),SPRVSTG_dump)

      DATA  LUBFI/21/,LUBFJ/51/,IREC/0/,IRSUB/0/,IMSUB/0/,
     $ IRSUB_this_MR/20*0/,IRSUB_this_MM/20*0/,IMSUB_this/200*0/,
     $ IUSUB/0/,IRSUB_this_sub_MR/5120*0/,IRSUB_this_sub_MM/5120*0/,
     $ IUSUB_this/65792*0/,ireco_last/0/

      DATA PREPBUFR_MSGTYP/'ADPUPA  ','AIRCAR  ','AIRCFT  ','SATWND  ',
     $                     'PROFLR  ','VADWND  ','SATEMP  ','ADPSFC  ',
     $                     'SFCSHP  ','SFCBOG  ','SPSSMI  ','SYNDAT  ',
     $                     'ERS1DA  ','GOESND  ','QKSWND  ','MSONET  ',
     $                     'GPSIPW  ','RASSDA  ','WDSATR  ','ASCATW  ',
     $                     'unknown '/
      NAMELIST/SWITCHES/MSG_RESTR,MSG_MIXED,MSG_MASKA,IMASK_T29

      CALL W3TAGB('BUFR_REMOREST',2021,0175,0012,'NP22')

      READ(11,'(Q,A)',END=1) NBYTES,FILE(1:NBYTES)

    1 CONTINUE

      PRINT 100, FILE(1:NBYTES)
  100 FORMAT(/15X,'WELCOME TO THE BUFR_REMOREST - THE PROGRAM THAT ',
     $ 'REMOVES OR MASKS RESTRICTED REPORTS FROM A BUFR FILE'/42X,
     $ 'LAST REVISION 24 Jun 2021'//30X,'INPUT BUFR FILENAME IS: ',A)

      IMASK_T29 = 99999
      IMASK_T29(1,:) = 000
      MSG_RESTR = '        '
      MSG_MIXED = '        '
      MSG_MASKA = '        '
      IDIFF_HR = 0
      READ(5,SWITCHES)

C  Set BUFRLIB missing (BMISS) to 10E8_8 to avoid integer*4 overflows
C  ------------------------------------------------------------------
      CALL SETBMISS(10E8_8)
      BMISS=GETBMISS()
      print'(1X)'
      print'(" BUFRLIB value for missing is: ",G0)', bmiss
      print'(1X)'

C  .... store rsrd_8 array as "almost" missing, will later encode back
C       into output *.nr PRPEBUFR file for reports with id masked,
C       overwriting original value (won't overwrite if exactly missing)
C       ---------------------------------------------------------------
      rsrd_8 = bmiss - 0.01_8

      IF(MSG_RESTR(1).NE.'        ')  THEN
         PRINT 107
  107 FORMAT(//' ALL BUFR MESSAGES READ IN WITH THE FOLLOWING TABLE A ',
     $ 'ENTRIES ARE SKIPPED (WITHOUT UNPACKING) BECAUSE THEY'/
     $ ' CONTAIN ONLY RESTRICTED REPORTS (FOR SOME PERIOD OF TIME) ALL',
     $ ' OF WHICH ARE TO BE REMOVED:'/)
         DO I = 1,20
            IF(MSG_RESTR(I).EQ.'        ')  EXIT
            PRINT *, MSG_RESTR(I)
         ENDDO
      END IF
      IF(MSG_MIXED(1).NE.'        ')  THEN
         PRINT 108
  108 FORMAT(//' ALL BUFR MESSAGES READ IN WITH THE FOLLOWING TABLE A ',
     $ 'ENTRIES ARE UNPACKED REPORT BY REPORT BECAUSE THEY CAN'/
     $ ' CONTAIN A MIXTURE OF BOTH NON-RESTRICTED AND RESTRICTED ',
     $ 'REPORTS - ALL RESTRICTED REPORTS WITHIN THE TIME'/' PERIOD OF ',
     $ 'THE RESTRICTION ARE TO BE REMOVED:'/)
         DO I = 1,20
            IF(MSG_MIXED(I).EQ.'        ')  EXIT
            PRINT *, MSG_MIXED(I)
         ENDDO
         CALL GET_ENVIRONMENT_VARIABLE('DIFF_HR',DIFF_HR)
         READ(DIFF_HR,'(I8)',END=88,ERR=88) IDIFF_HR
cppppp
ccc      print *
ccc      print *, 'DIFF_HR, IDIFF_HR : ',DIFF_HR, IDIFF_HR
ccc      print *
cppppp
         GO TO 89
   88    CONTINUE
         PRINT 115
  115 FORMAT(/'+++++BUFR_REMOREST: WARNING: ERROR OBTAINING IDIFF_HR -',
     $ ' SET TO ZERO AND CONTINUE'/)
         IDIFF_HR = 0
   89    CONTINUE
         IDIFF_HR_m4 = IDIFF_HR - 4
         IDIFF_HR_m4 = MAX(IDIFF_HR_m4,0)
         PRINT 118, IDIFF_HR,IDIFF_HR_m4
  118 FORMAT(/' ===> The difference between the current wall-clock ',
     $ 'date and the BUFR file center time is',I8,' hours.'/6X,
     $ 'Will consider the difference here to be only',I8,' hours when ',
     $ 'comparing against the time period of the'/6X,'restriction ',
     $ '(this takes into account that some reports may have obs times ',
     $ 'as much as 3-4 hours prior to'/6X,'the BUFR file center time, ',
     $ 'and so ensures that these reports are not inadvertently ',
     $ 'retained if the'/6X,'difference between the current wall-clock',
     $ ' date and the BUFR file center time is very close to the'/
     $ 6X,'the time period of the restriction).'/)
      END IF
      IF(MSG_MASKA(1).NE.'        ')  THEN
         if(MSG_MASKA(1)(1:2).eq.'NC') then

c .. DUMP file case
c    --------------
           PRINT 1107
 1107 FORMAT(//' ALL BUFR MESSAGES READ IN WITH THE FOLLOWING TABLE A ',
     $ 'ENTRIES CONTAIN REPORTS ALL CONSIDERED TO BE RESTRICTED. THEY '/
     $ 'ARE'/' UNPACKED REPORT BY REPORT AND EACH REPORT''S ID (ALL ',
     $ 'OCCURRENCES IN A REPORT) IS CHANGED TO "MASKSTID" OR "X"''S ',
     $ '(MASKED)'/' AND IT''S VALUES FOR "RSRD" AND "EXPRSRD" ARE RE-',
     $ 'SET TO MISSING PRIOR TO THEIR BEING WRITTEN BACK OUT:'/)

         else  

c .. PREPBUFR file case
c    ------------------
           print 3108
 3108 FORMAT(//' ALL BUFR MESSAGES READ IN WITH THE FOLLOWING TABLE A ',
     $ 'ENTRIES ARE UNPACKED REPORT BY REPORT BECAUSE THEY CAN'/
     $ ' CONTAIN A MIXTURE OF BOTH NON-RESTRICTED AND RESTRICTED ',
     $ 'REPORTS - THE ID''s FOR ALL RESTRICTED REPORTS WITHIN THE'/
     $ ' TIME PERIOD OF THE RESTRICTION ARE ALL CHANGED TO "MASKSTID" ',
     $ '(MASKED)  AND THEIR VALUES FOR "RSRD" AND "EXPRSRD"'/' ARE RE-',
     $ 'SET TO MISSING PRIOR TO THEIR BEING WRITTEN BACK OUT:'/)
         end if

         DO I = 1,20
            IF(MSG_MASKA(I).EQ.'        ')  EXIT
            PRINT *, MSG_MASKA(I)
         ENDDO

         if(MSG_MASKA(1)(1:2).ne.'NC') then

c .. PREPBUFR file case
c    ------------------
           CALL GET_ENVIRONMENT_VARIABLE('DIFF_HR',DIFF_HR)
           READ(DIFF_HR,'(I8)',END=988,ERR=988) IDIFF_HR
cppppp
ccc        print *
ccc        print *, 'DIFF_HR, IDIFF_HR : ',DIFF_HR, IDIFF_HR
ccc        print *
cppppp
           GO TO 989
  988      CONTINUE
           PRINT 115
           IDIFF_HR = 0
  989      CONTINUE
           IDIFF_HR_m4 = IDIFF_HR - 4
           IDIFF_HR_m4 = MAX(IDIFF_HR_m4,0)
           PRINT 118, IDIFF_HR,IDIFF_HR_m4
         end if 
      END IF

      PRINT 109
  109 FORMAT(//' ALL OTHER BUFR MESSAGES READ IN ARE COPIED INTACT ',
     $ '(WITHOUT UNPACKING) BECAUSE THEY CONTAIN ONLY NON-RESTRICTED ',
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
     $       'xxx'.AND.SUBSET(1:5).EQ.MSG_RESTR(I)(1:5)))  THEN
               PRINT 112
  112 FORMAT(' #####>>>> ALL reports in this message are RESTRICTED ',
     $ 'and are REMOVED - do NOT copy this message to output BUFR file')
               IRSUB_this_MR(I) = IRSUB_this_MR(I) + ISUB
              IF(SUBSET(1:2).EQ.'NC'.AND.MSG_RESTR(I)(6:8).EQ.'xxx')THEN
                  READ(SUBSET(6:8),'(I3)') ISUBSET_678
                  IF(ISUBSET_678.GE.0.AND.ISUBSET_678.LE.255) THEN
                     IRSUB_this_sub_MR(I,ISUBSET_678) =
     $                          IRSUB_this_sub_MR(I,ISUBSET_678) + ISUB
                  ELSE
                     PRINT 7115, ISUBSET_678,SUBSET
 7115 FORMAT(/'+++++BUFR_REMOREST: WARNING: INVALID BUFR MESSAGE ',
     $ 'SUBTYPE READ IN: ',I5.3,', SUBSET = ',A,' CANNOT INCREMENT ',
     $ 'RESTRICTED REPORT'/29X,'SKIPPED COUNTER FOR THIS SUBSET'/)
                  END IF
               END IF
               IRSUB = IRSUB + ISUB
               CYCLE LOOP1
C**********************************************************************
            ELSE IF(SUBSET.EQ.MSG_MIXED(I) .OR. (MSG_MIXED(I)(6:8).EQ.
     $       'xxx'.AND.SUBSET(1:5).EQ.MSG_MIXED(I)(1:5)))  THEN
               PRINT 113
  113 FORMAT(' #####>>>> rpts in this msg mixed restr/non-restr- restr',
     $ 'data w/i exp. time REMOVED- unpk each rpt & test mnems. ',
     $ '"RSRD" & "EXPRSRD"')

C  READ A SUBSET (REPORT) IN MESSAGE
C  ---------------------------------

               LOOP1n2: DO WHILE(IREADSB(LUBFI).EQ.0)

C  DECODE THE SUBSET (REPORT) LOOKING FOR RESTRICTED FLAG (MNEMONIC
C   "RSRD") AND TIME OF EXPIRATION ON RESTRICTION (MNEMONIC "EXPRSRD")
C   (if "EXPRSRD" is missing set it to 99999999 hours essentially
C    meaning the report is restricted for all time)
C  -------------------------------------------------------------------

                  CALL UFBINT(LUBFI,RID_8,7,1,NLV,
     $                               'SID RPT YOB XOB TYP RSRD EXPRSRD')
                  IF(RID_8(6).GT.0.AND.IBFMS(RID_8(6)).EQ.0)  THEN
                     IF(IBFMS(RID_8(7)).NE.0)  RID_8(7) = 99999999.
                     IF(IDIFF_HR_m4.LE.RID_8(7))  THEN
                        IF(IBFMS(RID_8(5)).EQ.0)  THEN

C  Normally for PREPBUFR files
C  ---------------------------

                           IF(SUBSET.EQ.'MSONET  ') THEN
                              CALL UFBINT(LUBFI,PRV_prep_8,2,1,NLV,
     $                                                 'PRVSTG SPRVSTG')
                              PRINT 8104, SID,(RID_8(II),II=2,4),
     $                         (NINT(RID_8(II)),II=5,7),PRVSTG_prep,
     $                         SPRVSTG_prep
 8104 FORMAT(5X,'- **Skip ',A8,F7.2,'UTC',F7.2,'(N+/S-) LAT',F7.2,' E ',
     $ 'LON, RTYP=',I3,', RSRD=',I5,' EXPRSRD=',I5,', PRVID=',A8,
     $ ' SPRVID=',A8)
                           ELSE
                              PRINT 104, SID,(RID_8(II),II=2,4),
     $                         (NINT(RID_8(II)),II=5,7)
  104 FORMAT(5X,'- **Skip report ',A8,' at ',F6.2,' UTC, ',F6.2,
     $ ' (N+/S-) LAT, ',F7.2,' E LON, RTYP= ',I3,', RSRD=',I5,
     $ ' EXPRSRD=',I5)
                           END IF
                        ELSE

C  Normally for DATA DUMP files
C  ----------------------------

                           CALL UFBINT(LUBFI,RID_8,7,1,NLV,
     $                          'RPID HOUR MINU CLAT CLON RSRD EXPRSRD')
                           IF(IBFMS(RID_8(4)).NE.0) THEN
                              CALL UFBINT(LUBFI,LALOH_8,2,1,NLV,
     $                                                    'CLATH CLONH')
                              RID_8(4:5) = LALOH_8
                           END IF
                           IF(IBFMS(RID_8(1)).NE.0) THEN
                              IF(SUBSET.EQ.'NC004004' .OR.
     $                           SUBSET.EQ.'NC004006' .OR.
     $                           SUBSET.EQ.'NC004009' .OR.
     $                           SUBSET.EQ.'NC004010' .OR.
     $                           SUBSET.EQ.'NC004011' .OR.
     $                           SUBSET.EQ.'NC004014') THEN
                                CALL UFBINT(LUBFI,ACRN_8,1,1,NLV,'ACRN')
                                 RID_8(1) = ACRN_8
                              ELSE IF(SUBSET.EQ.'NC004008' .OR.
     $                                SUBSET.EQ.'NC004012' .OR.
     $                                SUBSET.EQ.'NC004013') THEN
                                CALL UFBINT(LUBFI,ACID_8,1,1,NLV,'ACID')
                                 RID_8(1) = ACID_8
                              ELSE IF(SUBSET.EQ.'NC007001' .OR.
     $                                SUBSET.EQ.'NC007002') THEN
                                 SID = '        '
                              ELSE
                                 SID = 'MISSING '
                              END IF
                           END IF
                           IF(SUBSET(3:5).EQ.'255') THEN
                              CALL UFBINT(LUBFI,PRV1_dump_8,1,255,NLV,
     $                                                         'PRVSTG')
                              IF(NLV.LT.1) PRVSTG_dump = '        '
                              CALL UFBINT(LUBFI,PRV2_dump_8,1,255,NLV,
     $                                                        'SPRVSTG')
                              IF(NLV.LT.1) SPRVSTG_dump = '        '
                              PRINT 8105, SID,(NINT(RID_8(II)),II=2,3),
     $                         (RID_8(II),II=4,5), NINT(RID_8(6)),
     $                         NINT(RID_8(7)),PRVSTG_dump,SPRVSTG_dump
 8105 FORMAT(5X,'- **Skip ',A8,' at ',2(I2.2),' UTC',F7.2,' (N+/S-) ',
     $ 'LAT',F8.2,' (E+/W-) LON, RSRD=',I5,' EXPRSRD=',I5,', PRVID=',A8,
     $ ' SPRVID=',A8)
                           ELSE
                              PRINT 110, SID,(NINT(RID_8(II)),II=2,3),
     $                         (RID_8(II),II=4,5), NINT(RID_8(6)),
     $                         NINT(RID_8(7))
  110 FORMAT(5X,'- **Skip report ',A8,' at ',2(I2.2),' UTC, ',F6.2,
     $ ' (N+/S-) LAT, ',F7.2,'(E+/W-) LON, RSRD=',I5,' EXPRSRD=',I5)
                           END IF
                        END IF
                        IRSUB_this_MM(I) = IRSUB_this_MM(I) + 1
                        IF(SUBSET(1:2).EQ.'NC'.AND.
     $                   MSG_MIXED(I)(6:8).EQ.'xxx') THEN
                           READ(SUBSET(6:8),'(I3)') ISUBSET_678
                           IF(ISUBSET_678.GE.0.AND.ISUBSET_678.LE.255)
     $                      THEN
                              IRSUB_this_sub_MM(I,ISUBSET_678) =
     $                             IRSUB_this_sub_MM(I,ISUBSET_678) + 1
                           ELSE
                              PRINT 7115, ISUBSET_678,SUBSET
                           END IF
                        END IF
                        IRSUB = IRSUB + 1
                        CYCLE  LOOP1n2
                     END IF
                  END IF

cppppp
ccc               IF(IBFMS(RID_8(5)).EQ.0)  THEN
ccc                  IF(SUBSET.EQ.'MSONET  ') THEN
ccc                     CALL UFBINT(LUBFI,PRV_prep_8,2,1,NLV,
ccc  $                                                 'PRVSTG SPRVSTG')
ccc                     PRINT 9104, SID,(RID_8(II),II=2,4),
ccc  $                   (NINT(RID_8(II)),II=5,7),PRVSTG_prep,
ccc  $                   SPRVSTG_prep
 9104 FORMAT(5X,'- Retain ',A8,F7.2,'UTC',F7.2,'(N+/S-) LAT',F7.2,' E ',
     $ 'LON, RTYP=',I3,', RSRD=',I5,' EXPRSRD=',I5,', PRVID=',A8,
     $ ' SPRVID=',A8)
ccc                        ELSE
ccc                           PRINT 214, SID,(RID_8(II),II=2,4),
ccc  $                        (NINT(RID_8(II)),II=5,7)
  214 FORMAT(5X,'- Retain report ',A8,' at ',F6.2,' UTC, ',F6.2,
     $ ' (N+/S-) LAT, ',F7.2,' E LON, RTYP= ',I3,', RSRD=',I5,
     $ ' EXPRSRD=',I5)
ccc                        END IF
ccc               ELSE
ccc                  CALL UFBINT(LUBFI,RID_8,7,1,NLV,
ccc  $                          'RPID HOUR MINU CLAT CLON RSRD EXPRSRD')
ccc                  IF(IBFMS(RID_8(4)).NE.0) THEN
ccc                     CALL UFBINT(LUBFI,LALOH_8,2,1,NLV,'CLATH CLONH')
ccc                     RID_8(4:5) = LALOH_8
ccc                  END IF
ccc                  IF(IBFMS(RID_8(1)).NE.0) THEN
ccc                     IF(SUBSET.EQ.'NC004004' .OR.
ccc  $                     SUBSET.EQ.'NC004006' .OR.
ccc  $                     SUBSET.EQ.'NC004009' .OR.
ccc  $                     SUBSET.EQ.'NC004010' .OR.
ccc  $                     SUBSET.EQ.'NC004011' .OR.
ccc  $                     SUBSET.EQ.'NC004014') THEN
ccc                        CALL UFBINT(LUBFI,ACRN_8,1,1,NLV,'ACRN')
ccc                        RID_8(1) = ACRN_8
ccc                     ELSE IF(SUBSET.EQ.'NC004008' .OR.
ccc  $                          SUBSET.EQ.'NC004012' .OR.
ccc  $                          SUBSET.EQ.'NC004013') THEN
ccc                        CALL UFBINT(LUBFI,ACID_8,1,1,NLV,'ACID')
ccc                        RID_8(1) = ACID_8
ccc                     ELSE IF(SUBSET.EQ.'NC007001' .OR.
ccc  $                          SUBSET.EQ.'NC007002') THEN
ccc                        SID = '        '
ccc                     ELSE
ccc                        SID = 'MISSING '
ccc                     END IF
ccc                  END IF
ccc                  IF(SUBSET(3:5).EQ.'255') THEN
ccc                     CALL UFBINT(LUBFI,PRV1_dump_8,1,255,NLV,
ccc  $                                                         'PRVSTG')
ccc                     IF(NLV.LT.1) PRVSTG_dump = '        '
ccc                     CALL UFBINT(LUBFI,PRV2_dump_8,1,255,NLV,
ccc  $                                                        'SPRVSTG')
ccc                     IF(NLV.LT.1) SPRVSTG_dump = '        '
ccc                     PRINT 9105, SID,(NINT(RID_8(II)),II=2,3),
ccc  $                   (RID_8(II),II=4,5), NINT(RID_8(6)),
ccc  $                   NINT(RID_8(7)),PRVSTG_dump,SPRVSTG_dump
 9105 FORMAT(5X,'- Retain ',A8,' at ',2(I2.2),' UTC',F7.2,' (N+/S-) ',
     $ 'LAT',F8.2,' (E+/W-) LON, RSRD=',I5,' EXPRSRD=',I5,', PRVID=',A8,
     $ ' SPRVID=',A8)
ccc                  ELSE
ccc                     PRINT 215, SID,(NINT(RID_8(II)),II=2,3),
ccc  $                   (RID_8(II),II=4,5),NINT(RID_8(6)),
ccc  $                   NINT(RID_8(7))
  215 FORMAT(5X,'- Retain report ',A8,' at ',2(I2.2),' UTC, ',F6.2,
     $ ' (N+/S-) LAT, ',F7.2,'(E+/W-) LON, RSRD=',I5,' EXPRSRD=',I5)
ccc                  END IF
ccc               END IF
cppppp
                  CALL OPENMB(LUBFJ,SUBSET,IDATE)
                  call ufbcnt(lubfj,ireco,isubo)
                  if(ireco.ne.ireco_last) then
C Encode minutes into Sec. 1 of new output message header if non-zero
                     if(iminu.ne.0) call minimg(lubfj,iminu)
                  end if
                  ireco_last  = ireco
                  CALL UFBCPY(LUBFI,LUBFJ)
                  CALL WRITSB(LUBFJ)
                  IF(SUBSET(1:2).EQ.'NC') THEN
                     READ(SUBSET(3:5),'(I3)') ISUBSET_345
                     READ(SUBSET(6:8),'(I3)') ISUBSET_678
                  ELSE
                     ISUBSET_678 = 256
                     ISUBSET_345 = -99
                     DO II = 1,20
                        IF(SUBSET.EQ.PREPBUFR_MSGTYP(II)) THEN
                           ISUBSET_345 = II
                           EXIT
                        END IF
                     ENDDO
                     IF(ISUBSET_345.EQ.-99) ISUBSET_345 = 21
                  END IF
                  IF(SUBSET(1:2).EQ.'NC' .AND.
     $               ((ISUBSET_345.LT.0.OR.ISUBSET_345.GT.255) .OR.
     $                (ISUBSET_678.LT.0.OR.ISUBSET_678.GT.255))) THEN
                     PRINT 7116, SUBSET
 7116 FORMAT(/'+++++BUFR_REMOREST: WARNING: INVALID BUFR MESSAGE ',
     $ 'TYPE AND/OR SUBTYPE READ IN, SUBSET = ',A,' CANNOT INCREMENT'
     $ /29X,'NON-RESTRICTED REPORT COPIED COUNTER FOR THIS SUBSET'/)
                  ELSE
                     IUSUB_this(ISUBSET_345,ISUBSET_678) =
     $                          IUSUB_this(ISUBSET_345,ISUBSET_678) + 1
                  END IF                  
                  IUSUB = IUSUB + 1
               ENDDO  LOOP1n2
               CYCLE  LOOP1
C**********************************************************************
            ELSE IF(SUBSET.EQ.MSG_MASKA(I) .OR. (MSG_MASKA(I)(6:8).EQ.
     $       'xxx'.AND.SUBSET(1:5).EQ.MSG_MASKA(I)(1:5)))  THEN
               IF(IMASK_T29(1,I).EQ.000)  THEN
                  if(subset(1:2).eq.'NC') then

c  .. DUMP file case
c     --------------
                     PRINT 1113
 1113 FORMAT('  ###>> ALL rpts in msg RESTRICTED - unpk each rpt, MASK ',
     $ 'all id''s (chg to "MASKSTID" or "X"''s), set to non-restr, ',
     $ ' copy to output file')
                   else

c  .. PREPBUFR file case
c     ------------------
                     print 1114
                  end if
               ELSE

c  .. PREPBUFR file case
c     ------------------
                  PRINT 1114
 1114 FORMAT('  ###>> some rpts in msg may be RESTRICTED: if so & w/i ',
     $ 'expir time, MASK id (chg to "MASKSTID"), set to non-restr, cpy',
     $ ' to output file')
                  DO J = 1,10
                     IF(IMASK_T29(J,I).NE.99999)  PRINT 1115,
     $                IMASK_T29(J,I)
 1115 FORMAT('  -- rpts in dump type ',I3,' mixed restr./non-restr.- ',
     $ 'for restr. rpts w/i expir. time, MASK id, set to non-restr & ',
     $ 'copy to output file')
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
                  CALL UFBINT(LUBFI,RID_8,8,1,NLV,
     $                        'SID RPT YOB XOB TYP T29 RSRD EXPRSRD')
                  IF(IBFMS(RID_8(5)).EQ.0)  THEN

C  Come here for PREPBUFR files (report id is in mnemonic "SID")
C   -- check for a match of dump report type
C  -------------------------------------------------------------

                     DO J = 1,10
                        IF(IMASK_T29(1,I).EQ.000.OR.
     $                     IMASK_T29(J,I).EQ.NINT(RID_8(6)))  THEN

C    .... dump report types match - look for restricted flag (mnemonic
C         "RSRD") and time of expiration on restriction (mnemonic
C         "EXPRSRD") (if "EXPRSRD" is missing set it to 99999999 hours
C          essentially meaning the report is restricted for all time)
C         ------------------------------------------------------------
                           if(rid_8(7).gt.0.and.ibfms(rid_8(7)).EQ.0)
     $                      then
                              if(ibfms(rid_8(8)).ne.0)
     $                         rid_8(8) = 99999999.
                              if(IDIFF_HR_m4.le.rid_8(8))  then
                                 PRINT 1104, SID,(RID_8(II),II=2,4),
     $                            (NINT(RID_8(II)),II=5,8)
 1104 FORMAT(5X,'- Chg ID of rpt ',A8,' at',F6.2,' UTC, ',F6.2,
     $ ' LAT,',F7.2,' E LON, RTYP=',I3,', DTYP=',I5,
     $ ', RSRD=',I5,', EXPRSRD=',I5,' TO "MASKSTID"')
                                 SID = 'MASKSTID'
                                 SID_8 = RID_8(1)
                                 IMSUB_this(J,I) = IMSUB_this(J,I) + 1

C    .... update report id to masked value - "MASKSTID"
C         ---------------------------------------------
                                 CALL UFBINT(LUBFJ,SID_8,1,1,IRET,'SID')

C    .... re-set RSRD & EXPRSRD to "almost" missing so rpt is no longer
C          restricted
C         -----------------------r-------------------------------------
                       call ufbint(lubfj,rsrd_8,2,1,iret,'RSRD EXPRSRD')
                                 EXIT
                              END IF
                           END IF
                        END IF
                     ENDDO
                  ELSE

C  Come here for DATA DUMP files where all reports are restricted
C   {report id is in mnemonic "RPID" (and for surface ship reports in
C   Table A entry 'NC001001' and 'NC001101' also in mnemonic "SHPC8"),
C   report id may also be embedded in replicated raw bulletin header
C   string if it is present}
C  -------------------------------------------------------------------

                     IUPDATE_RAWRPT = 0
                     CALL UFBINT(LUBFI,RID_8,7,1,NLV,
     $                          'RPID HOUR MINU CLAT CLON RSRD EXPRSRD')
                     PRINT 1110, SID,(NINT(RID_8(II)),II=2,3),
     $                (RID_8(II),II=4,5), NINT(RID_8(6)),NINT(RID_8(7))
 1110 FORMAT('  - Chg all instances of rpt id ',A8,' ',2(I2.2),' UTC, ',
     $ F6.2,'(N+/S-) LAT ',F7.2,'(E+/W-) LON, RSRD=',I5,', EXPRSRD=',I5,
     $ ' to MASKSTID or X''s')
                     SID_orig = SID
                     SID = 'MASKSTID'
                     RPID_8 = RID_8(1)
                     IMSUB_this(1,I) = IMSUB_this(1,I) + 1

C    .... update report id in output file to masked value - "MASKSTID"
C         ------------------------------------------------------------
                     CALL UFBINT(LUBFJ,RPID_8,1,1,IRET,'RPID')

C    .... re-set RSRD & EXPRSRD to "almost" missing so rpt is no longer
C          restricted
C         -----------------------r-------------------------------------
                     call ufbint(lubfj,rsrd_8,2,1,iret,'RSRD EXPRSRD')
                     CALL UFBINT(LUBFJ,RID_8(1),1,1,IRET,'RPID')
                     IF(SUBSET.EQ.'NC001001'.or.
     $                  subset.eq.'NC001101') THEN

C    .... update 2nd rpt id in ship rpts in output file to masked value
C         - "MASKSTID"
C         -------------------------------------------------------------
                        CALL UFBINT(LUBFJ,RPID_8,1,1,IRET,'SHPC8')
                     END IF

C    .... see if a replicated raw report bulletin header string is
C         present
C         --------------------------------------------------------
                     CALL UFBINT(LUBFI,RASTR_8,1,255,NLV,'RRSTG')
                     IST = 1
                     IF(NLV.GT.0)  THEN

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
                           IF(SID_orig(II:II).EQ.' ')  THEN
                              ICHAR_id = II - 1
                              EXIT
                           END IF
                        ENDDO
cppppp
ccc   print *, 'original sid had ',ICHAR_id,' characters'
cppppp
                        IF(ICHAR_id.GE.1)  THEN

C        .... next parse through entire raw report bulletin header
C             string looking for one or more occurrences of report id
C             and "X" out the id
C             -------------------------------------------------------
                           DO II = 1,IEN
                              IF(II+ICHAR_id-1.GT.IEN)  EXIT
                              IF(RAWRPT_STG(II:II+ICHAR_id-1).EQ.
     $                           SID_orig(1:ICHAR_id))  THEN
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
                  IF(SID.EQ.'MASKSTID')  THEN
                     IMSUB = IMSUB + 1
                  ELSE
                     IF(SUBSET(1:2).EQ.'NC') THEN
                        READ(SUBSET(3:5),'(I3)') ISUBSET_345
                        READ(SUBSET(6:8),'(I3)') ISUBSET_678
                     ELSE
                        ISUBSET_678 = 256
                        ISUBSET_345 = -99
                        DO II = 1,20
                           IF(SUBSET.EQ.PREPBUFR_MSGTYP(II)) THEN
                              ISUBSET_345 = II
                              EXIT
                           END IF
                        ENDDO
                        IF(ISUBSET_345.EQ.-99) ISUBSET_345 = 21
                     END IF
                     IF(SUBSET(1:2).EQ.'NC' .AND.
     $                  ((ISUBSET_345.LT.0.OR.ISUBSET_345.GT.255) .OR.
     $                   (ISUBSET_678.LT.0.OR.ISUBSET_678.GT.255))) THEN
                        PRINT 7116, SUBSET
                     ELSE
                        IUSUB_this(ISUBSET_345,ISUBSET_678) =
     $                          IUSUB_this(ISUBSET_345,ISUBSET_678) + 1
                     END IF
                     IUSUB = IUSUB + 1
                  END IF
                  CALL WRITSB(LUBFJ)
               ENDDO  LOOP1n3
               CYCLE  LOOP1
            END IF
         ENDDO  LOOP1n1
C**********************************************************************
         PRINT 114
  114 FORMAT(' ALL reports in this message are NON-RESTRICTED - copy',
     $ ' this message, intact, to output BUFR file')
         IF(SUBSET(1:2).EQ.'NC') THEN
            READ(SUBSET(3:5),'(I3)') ISUBSET_345
            READ(SUBSET(6:8),'(I3)') ISUBSET_678
         ELSE
            ISUBSET_678 = 256
            ISUBSET_345 = -99
            DO II = 1,20
               IF(SUBSET.EQ.PREPBUFR_MSGTYP(II)) THEN
                  ISUBSET_345 = II
                  EXIT
               END IF
            ENDDO
            IF(ISUBSET_345.EQ.-99) ISUBSET_345 = 21
         END IF
         IF(SUBSET(1:2).EQ.'NC' .AND.
     $      ((ISUBSET_345.LT.0.OR.ISUBSET_345.GT.255) .OR.
     $       (ISUBSET_678.LT.0.OR.ISUBSET_678.GT.255))) THEN
            PRINT 7116, SUBSET
         ELSE
            IUSUB_this(ISUBSET_345,ISUBSET_678) =
     $                       IUSUB_this(ISUBSET_345,ISUBSET_678) + ISUB
         END IF
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

      PRINT 106, IRSUB
  106 FORMAT(//'==> A TOTAL OF',I11,' RESTRICTED   REPORTS WERE ',
     $ 'SKIPPED')
      DO I = 1,20
         IF(IRSUB_this_MR(I).GT.0) THEN
            PRINT 1106, IRSUB_this_MR(I),MSG_RESTR(I)
            DO J = 0,255
               IF(IRSUB_this_sub_MR(I,J).GT.0) THEN
                  PRINT 2106, IRSUB_this_sub_MR(I,J),MSG_RESTR(I)(1:5),J
               END IF
            ENDDO
         END IF
      ENDDO
      DO I = 1,20
         IF(IRSUB_this_MM(I).GT.0) THEN
            PRINT 1106, IRSUB_this_MM(I),MSG_MIXED(I)
            DO J = 0,255
               IF(IRSUB_this_sub_MM(I,J).GT.0) THEN
                  PRINT 2106, IRSUB_this_sub_MM(I,J),MSG_MIXED(I)(1:5),J
               END IF
            ENDDO
         END IF
      ENDDO
 1106 FORMAT(16X,'--> ',I11,' reports from message type ',A)
 2106 FORMAT(21X,'--> ',I11,' reports from message type ',A,I3.3)

      PRINT 116, IMSUB
  116 FORMAT(//'==> A TOTAL OF',I11,' RESTRICTED   REPORTS WERE ',
     $ 'MASKED AND THEN COPIED')
      DO I = 1,20
         DO J = 1,10
            IF(IMSUB_this(J,I).GT.0) THEN
               IF(IMASK_T29(J,I).GT.0) THEN
                  PRINT 1108,IMSUB_this(J,I),MSG_MASKA(I),IMASK_T29(J,I)
               ELSE
                  PRINT 1106,IMSUB_this(J,I),MSG_MASKA(I)
               END IF
            END IF
         ENDDO
      ENDDO
 1108 FORMAT(16X,'--> ',I11,' reports from message type ',A,
     $ ', DUMP report type ',I5)

      PRINT 117, IUSUB
  117 FORMAT(//'==> A TOTAL OF',I11,' NON-RESTRICTED REPORTS WERE ',
     $ 'COPIED'/)
      IPRINT_FLAG = 0
      DO I = 1,21
         IF(IUSUB_this(I,256).GT.0) THEN
            PRINT 2108, IUSUB_this(I,256),PREPBUFR_MSGTYP(I)
            IPRINT_FLAG = 1
         END IF
      ENDDO
 2108 FORMAT(16X,'--> ',I11,' reports from message type ',A)

      IF(IPRINT_FLAG.EQ.0) THEN
         DO I = 0,255
            DO J = 0,255
               IF(IUSUB_this(I,J).GT.0) THEN
                  PRINT 2107, IUSUB_this(I,J),I,J
               END IF
            ENDDO
         ENDDO
      END IF
 2107 FORMAT(16X,'--> ',I11,' reports from message type NC',I3.3,I3.3)

      PRINT 3117
 3117 FORMAT(//'PROGRAM COMPLETED SUCCESSFULLY'/)


      CALL W3TAGE('BUFR_REMOREST')

      STOP

      END

