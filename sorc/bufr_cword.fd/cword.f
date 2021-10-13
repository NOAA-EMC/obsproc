C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C  
C MAIN PROGRAM:  BUFR_CWORD
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2014-01-16
C
C ABSTRACT: CONVERTS BINARY BYTE STREAM BUFR FILES BACK AND FORTH
C   FROM A FORTRAN BLOCKED FORMAT. THE INPUT FILE MAY EITHER BE BLOCKED
C   OR UNBLOCKED, HOWEVER NORMALLY THE INPUT FILE IS UNBLOCKED WHEN
C   BLOCKING IS PERFORMED HERE AND THE INPUT FILE IS BLOCKED WHEN
C   UNBLOCKING IS PERFORMED HERE. CAN OPTIONALLY REMOVE THE DICTIONARY
C   MESSAGES FROM AN UNPUT FILE WHEN EITHER BLOCKING OR UNBLOCKING IT.
C
C PROGRAM HISTORY LOG:
C 1999-08-19  J. WOOLLEN  ORIGINAL VERSION FOR IMPLEMENTATION
C 2004-03-19  D. KEYSER   INCREASED SIZE OF ARRAY MBAY FROM 3000 WORDS
C                         TO 5000 WORDS TO ALLOW IT TO PROCESS BUFR
C                         MESSAGES WITH UP TO 20K BYTES
C 2005-11-29  J. ATOR     REWRITTEN USING BUFRLIB C I/O LOGIC TO HANDLE
C                         ANY INPUT BUFR FILES (INCLUDING FILES WHICH
C                         CONTAIN EXTRANEOUS CHARACTERS (E.G. BULLETIN
C                         HEADERS) AND/OR WHICH PREVIOUSLY REQUIRED THE
C                         USE OF APPLICATION PROGRAM GRABBUFR) AND TO
C                         REMOVE DIRECT LINKS TO BUFRLIB COMMON BLOCKS
C 2007-11-28  D. KEYSER   INCREASED LIMIT FOR I/O FILENAME LENGTH FROM
C                         80 CHARACTERS TO 120 CHARACTERS
C 2011-01-05  D. KEYSER   INCREASED SIZE OF ARRAY MBAY FROM 37500
C                         4-BYTE WORDS TO 625000 4-BYTE WORDS TO ALLOW
C                         IT TO PROCESS BUFR MESSAGES WITH UP TO
C                         2.5 MBYTES (INSTEAD OF UP TO JUST 150K
C                         BYTES); NOW PRINTS DIAGNOSTICS IF EITHER NO
C                         INPUT MESSAGES WERE PROCESSED OR IF ONE OR
C                         MORE INPUT MESSAGES WERE NOT PROCESSED (FOR
C                         WHATEVER REASON); NOW LOOKS FOR SCRIPT
C                         ENVIRONMENT VARIABLE "DX_SKIP" VIA CALL TO
C                         GETENV, IF "YES" OR "yes" WILL NOT COPY BUFR
C                         DICTIONARY (TABLE) MESSAGES TO OUTPUT WHEN
C                         BLOCKING (BEFORE DICTIONARY MESSAGES WERE
C                         ALWAYS COPIED) (DOES NOT APPLY TO UNBLOCKING,
C                         DICTIONARY MESSAGES WILL CONTINUE TO BE
C                         COPIED IN THIS CASE), THE DEFAULT (WHEN
C                         "DX_SKIP" IS NOT FOUND) IS TO COPY DICTIONARY
C                         MESSAGES WHEN BLOCKING (AS BEFORE)
C 2012-10-13  D. KEYSER   CHANGES TO RUN ON WCOSS.  SCRIPT ENVIRONMENT
C                         VARIABLE "DX_SKIP" WILL NOW WORK FOR
C                         UNBLOCKING AS WELL AS FOR BLOCKING.
C 2014-01-16  D. KEYSER   INCREASED LIMIT FOR I/O FILENAME LENGTH FROM
C                         120 CHARACTERS TO 500 CHARACTERS. RENAMED
C                         FROM CWORDSH TO BUFR_CWORD.
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT (OPERATION TYPE, INPUT FILENAME,
C                OUTPUT FILENAME)
C
C   OUTPUT FILES: 
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - BLOCKED FORTRAN FILE OUTPUT FOR BLOCKING OPERATION
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C     SYSTEM:    - GET_ENVIRONMENT_VARIABLE
C       W3NCO    - W3TAGB   W3TAGE   ERREXIT
C       BUFRLIB  - CCBFL    COBFL    CRBMG    CWBMG    PADMSG
C                  IUPBS01
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   8 - INCORRECT INPUT PARAMETER, NO OUTPUT FILE CREATED
C
C REMARKS: ONE SCRIPT ENVIRONMENT VARIABLE IS READ IN:
C            DX_SKIP         - If = 'YES' or 'yes', then only non-
C                              dictionary (i.e., non-table) BUFR
C                              messages read in will be copied to
C                              output when blocking or unblocking
C                              (i.e., dictionary messages will not be
C                              copied when blocking or unblocking).
C                            - Otherwise (including if DX_SKIP is not
C                              set), all BUFR messages read in
C                              (including BUFR dictionary messages)
C                              will be copied to output when blocking
C                              or unblocking -- this is the default
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$

      program bufr_cword

      parameter(mxbufr=2500000)
      parameter(mxbufrd4=mxbufr/4)

      character*500 bfile,ufile
      character*8  cword
      character*3  dx_skip,dx_skip_ORIG
      character*1  bufr(mxbufr)
      dimension    mbay(mxbufrd4)
      equivalence  (bufr(1),mbay(1))

      call w3tagb('BUFR_CWORD',2014,0016,0332,'NP22')
 
      istop = 0
      igo   = 5

c     Read and process the input arguments.
 
      call get_environment_variable('DX_SKIP',dx_skip)
      dx_skip_ORIG = dx_skip
      if(dx_skip_ORIG.EQ.'   ')  dx_skip = 'NO'
 
      read(5,'(a)') cword
      if(cword.eq.'block') then
         read(5,'(a)') ufile
         read(5,'(a)') bfile
      elseif(cword.eq.'unblk') then
         read(5,'(a)') bfile
         read(5,'(a)') ufile
      else
         print *,'cword must be block or unblk'
         call w3tage('BUFR_CWORD')
         call errexit(8)
      endif
 
c     Open the input and output files.

      if(cword.eq.'block') then
         print '(" blocking from: ",A)', trim(ufile)
         print '("            to: ",A)', trim(bfile)
         call cobfl(ufile,'r')
         open(51,file=bfile,form='unformatted')
      else
         print '(" unblocking from: ",A)', trim(bfile)
         print '("              to: ",A)', trim(ufile)
         call cobfl(bfile,'r')
         call cobfl(ufile,'w')
      endif

c     Read the next message from the input file.

      call crbmg(bufr,mxbufr,nbyt,ierr)
      if(ierr.eq.-1) then
         print '(" Return value from crbmg is -1 on first BUFR message",
     .    " read; input file is empty; no output file created.")'
         go to 88
      elseif(ierr.lt.-1) then
         print '(" Return value from crbmg is -2 on first BUFR message",
     .    " read; I/O error reading first input BUFR message; no ",
     .    "output file created.")'
         go to 88
      endif

      do while(ierr.ge.0)
         if(ierr.eq.0) then

c         Pad the end of the message with zeroed-out bytes up to the
c         next 8-byte boundary.

            call padmsg(mbay,mxbufrd4,npbyt)
            ntbyt = nbyt + npbyt

c         Write the message plus padding to the output file...

            if(cword.eq.'block') then

c           using a FORTRAN write (check value of dx_skip to see if
c            dictionary messages should be skipped in write).

               if(iupbs01(mbay,'MTYP').eq.11 .and.
     .            (dx_skip.eq.'YES' .or. dx_skip.eq.'yes')) then
                  print '(" BUFR dictionary (table) message read; ",
     .             "message not written to output because DX_SKIP is ",
     .             "set to ""YES"" or ""yes"".")'
               else
                  write(51) (bufr(i),i=1,ntbyt)
                  igo = 0
               endif

            else

c           using a C write.

               if(iupbs01(mbay,'MTYP').eq.11 .and.
     .            (dx_skip.eq.'YES' .or. dx_skip.eq.'yes')) then
                  print '(" BUFR dictionary (table) message read; ",
     .             "message not written to output because DX_SKIP is ",
     .             "set to ""YES"" or ""yes"".")'
               else
                  call cwbmg(bufr,ntbyt,ierw)
                  if(ierw.ne.0) then
                  print '(" return value from cwbmg is ",I0," - I/O ",
     .             "error occurred while writing; message not written ",
     .             "to output")', ierw
                     istop = 4
                  else
                     igo = 0
                  endif
               endif
            endif
         else
            if(ierr.eq.1) then
               print '(" return value from crbmg is 1 - BUFR message ",
     .          "array overflow, increase size of array; message not ",
     .          "written to output")'
            elseif(ierr.eq.2) then
               print '(" return value from crbmg is 2 -""7777"" ",
     .          "indicator not found in expected location; message not",
     .          " written to output")'
            else
               print '(" return value from crbmg is ",I0,"; message ",
     .          "not written to output")', ierr
            endif
            istop = 4
         endif
         call crbmg(bufr,mxbufr,nbyt,ierr)
      enddo
      if(ierr.eq.-1) then
         print *,'done'
      elseif(ierr.lt.-1) then
         print '(" return value from crbmg is -2 - I/O error reading ",
     .    "input message; message not written to output")'
         istop = 4
      endif

c     Close the input and output files.

      call ccbfl
      if(cword.eq.'block') close(51)
 
   88 continue

      if(cword.eq.'block') then
         if(igo.eq.5) then
            print '(" ***WARNING: BUFR_CWORD - No input BUFR messages ",
     .       "were blocked into output file - no output file created")'
         else if(istop.eq.4) then
            print '(" ***WARNING: BUFR_CWORD - One or more input BUFR ",
     .       "messages could not be blocked into output file - output ",
     .       "file is incomplete")'
         endif
      else
         if(igo.eq.5) then
            print '(" ***WARNING: BUFR_CWORD - No input BUFR messages",
     .       " were unblocked into output file - no output file ",
     .       "created")'
         else if(istop.eq.4) then
            print '(" ***WARNING: BUFR_CWORD - One or more input BUFR",
     .       " messages could not be unblocked into output file - ",
     .       "output file is incomplete")'
         endif
      endif

      call w3tage('BUFR_CWORD')

      stop

      end
