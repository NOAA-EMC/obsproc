#!/bin/ksh
#####################################################################
echo "----------------------------------------------------------"
echo "exdump_post.sh version $obsproc_ver - If requested:       "
echo "       1) Generates combined dump STATUS file             "
echo "       2) Prepares data counts for the SDM                "
echo "       3) Removes or masks restricted data from today's   "
echo "          dump files                                      "
echo "       4) Removes restricted data from 2-day old aircar   "
echo "          and aircft dump files                           "
echo "       5) Converts dump files to unblocked format         "
echo "          (meaningless on WCOSS)                          "
echo "       6) Lists the contents of dump files                "
echo "       7) Updates dump data count average tables          "
echo "----------------------------------------------------------"
#####################################################################
#
# modification history:
# Mar 29 2000 - Original script (Keyser)
# Jul 16 2003 - Added step to remove or mask restricted 
#                 data (Keyser)
# MAR 01 2006 - Moved step to do data counts here (was in 
#                 dump job) (Keyser)
# OCT 15 2011 - RAP replaces RUC (Keyser)
# 12 Mar 2013 JWhiting - ported to WCOSS (linux) platforms:
#       cp'd fr CCS ver of 1 May 2012
#       renamed (sms -> ecf)
#       updated docblock
#       set to run under ksh (sh is bash on WCOSS)
#       updated logical unit specification (FORT* in place of XLF*)
#       replaced "timex" w/ "time -p"
# 15 Mar 2013 Keyser - remove RUCS
# 28 Apr 2014 JWhiting - config'd for Vertical Structure (VS)
#  1 May 2014          - enabled urma network
#  2 Feb 2015 Keyser   - Added new dump type "efclam" to list of dump types
#                        whose files that can potentially be unblocked.
# 10 Jan 2016 Keyser   - Added "NC004011" and "NC004103" in dump file
#                        "aircft" and "NC021242" in (just added) dump file
#                        "saphir" to the list of Table A entries for BUFR
#                        messages which contain all restricted reports in
#                        BUFR_REMOREST processing.
# 26 Mar 2017 Stokes   - Updated to run on Cray-XC40 as well as IBM iDataPlex.
#                      - Added MPMD methods to run lister threads in parallel
#                        and eliminated use of common background processes.
#                      - If creating combined dump status file, send to $COMOUT
#                        rather than $COMIN.
# 22 May 2018 Melchior - Updated to run on Dell-p3 as well as Cray-XC40 and
#                        IBM iDataPlex.
# 13 Jan 2020 SPA SH   - turn off alert of gdas.tCCz.saphir.tm00.bufr_d.nr
#                        per Dataflow/customer request
# 20 Aug 2020 Melchior - Added "NC000100" in dump file "adpsfc" to the list of
#                        Table A entries for BUFR messages which contain all
#                        restricted reports in BUFR_REMOREST processing.
#                        Added explicit path to thread, to become $DATA/thread.
# 21 Oct 2020 Melchior - Modified to include gpsro (NC003010) to remove the
#                        restricted data from gpsro dump file, which now
#                        includes commercial data. Alerts for nr gpsro dump
#                        files are not enabled for RAP and CDAS networks.
# 26 Jan 2021 Melchior - Added "NC001101" in dump file "sfcshp" to the list of
#                        Table A entries for BUFR messges which contain all
#                        restricted reports in BUFR_REMOREST processing, but
#                        whose reports are retained and instead station IDs
#                        are masked.
# 09 Dec 2021 Esposito - Updated for use on WCOSS2.
# ?? ??? 202? Keyser   - Added new "remorest" processing to remove restricted
#                        data from 2-day old "aircar" and "aircft" dump files.
#####################################################################

# NOTE: NET is changed to gdas in the parent Job script for the gdas RUN
#       (was gfs - NET remains gfs for gfs RUN)
# -----------------------------------------------------------------------

set -aux

# Make sure we are in the $DATA directory
cd $DATA

jlogfile=${jlogfile:=""}

msg="HAS BEGUN on `hostname`"
$DATA/postmsg "$jlogfile" "$msg"
 
#  determine local system name and type if available
#  -------------------------------------------------
SITE=${SITE:-""}

cat break > $pgmout

PROCESS_REMOREST_dm2=${PROCESS_REMOREST_dm2:-NO}
MPMD=${MPMD:-YES}
CHGRP_RSTPROD=${CHGRP_RSTPROD:-YES}
UPDATE_AVERAGE_FILE=${UPDATE_AVERAGE_FILE:-YES}

hr_fraction=""
set +u
if [ -n "$cycM" ]; then
set -u
  case "$cycM" in
    00) hr_fraction='.00' ;;
    15) hr_fraction='.25' ;;
    30) hr_fraction='.50' ;;
    45) hr_fraction='.75' ;;
     *) err_xd=9
        msg="###FATAL ERROR in model script: incorrect cycM='${cycM}' - exiting"
        echo "$msg"
        postmsg "$jlogfile" "$msg"
        exit $err_xd ;;
  esac
fi

tmhr=`echo $tmmark|cut -c3-4`
export dumptime=`$NDATE -$tmhr $PDY$cyc`$hr_fraction
export dumptime10=`$NDATE -$tmhr $PDY$cyc`

net=$NET

[[ $RUN == rap_p ]]  &&  net=$RUN
[[ $RUN == rap_e ]]  &&  net=$RUN

net_uc=$(echo $net | tr [a-z] [A-Z])
set +u
[ -n "$cycM" ]  &&  net_uc=RTMA_RU
set -u
tmmark_uc=$(echo $tmmark | tr [a-z] [A-Z])

msg="$net_uc ANALYSIS TIME IS $PDY$cyc"
set +u
[ -n "$cycM" ]  &&  msg="$msg:${cycM}"
set -u
$DATA/postmsg "$jlogfile" "$msg"

set +x
echo
echo "CENTER DATA DUMP DATE-TIME FOR $tmmark_uc $net_uc IS $dumptime"
echo
set -x
 
retcode=0


########################################################################
#    Generate a "combined" dump STATUS file if one is not yet present  #
#     use STATUS files from TWO (SPLIT) dump jobs if they are present  #
########################################################################


if [ ! -s $COMIN/$RUN.$cycle.status.$tmmark.bufr_d ]; then

   file1=$COMIN/$RUN.$cycle.status1.$tmmark.bufr_d
   file2=$COMIN/$RUN.$cycle.status2.$tmmark.bufr_d

   if [ -s $file1 -a -s $file2 ]; then
      mcountp1=`grep -n "##################################################\
#############################" $file1 | cut -f1 -d:`
      head -n `expr $mcountp1 - 3` $file1 > status1
      mcount=`grep -n "PARENT SCRIPT IS" status1 | cut -f1 -d:`
      head -n $mcount status1 > top_part
      mcount=`expr \`cat <status1 | wc -l\` - $mcount`
      tail -n $mcount status1 > bottom_part
      grep "PARENT SCRIPT IS" $file2 > insert
      cat top_part insert bottom_part > status1
      mcount=`grep -n "THIS STATUS FILE CREATED" status1 | cut -f1 -d:`
      total=`cat <status1 | wc -l`
      head -n `expr $mcount - 1` status1 > top_part
      tail -n `expr $total - $mcount` status1 > bottom_part
      echo "             THIS STATUS FILE CREATED `date -u`" > insert
      cat top_part insert bottom_part > status1
      mcount=`grep -n "GEOGRAPHICAL DOMAIN IS DATA GROUP DEPENDENT" $file2 \
       | cut -f1 -d:`
      total2=`cat <$file2 | wc -l`
      tail -n `expr $total2 - $mcount - 1` $file2 > bottom_part
      mcount=`grep -n "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\
%%%%%%%%%%%%%%%%%%%%%%%%%%%" bottom_part | cut -f1 -d:`
      head -n `expr $mcount - 1` bottom_part > insert
      cat status1 insert > status
      total1=`cat <$file1 | wc -l`
      tail -n `expr $total1 - $mcountp1 - 2` $file1 > bottom_part
      cat status bottom_part > status1
      mcount=`grep -n " ++++ SPECIFIC INFORMATION ON EACH DATA GROUP DUMP \
++++" $file2 | cut -f1 -d:`
      tail -n `expr $total2 - $mcount - 1` $file2 > bottom_part
      cat status1 bottom_part > $COMOUT/$RUN.$cycle.status.$tmmark.bufr_d
      rm $file1 $file2 bottom_part insert status status1 top_part
   elif [ -s $file1 ]; then
      cp $file1 $COMOUT/$RUN.$cycle.status.$tmmark.bufr_d
      rm $file1
      msg="***WARNING: DUMP status file successfully generated but only from \
dump status1 file - dump status2 file not present"
      $DATA/postmsg "$jlogfile" "$msg"
   elif [ -s $file2 ]; then
      cp $file2 $COMOUT/$RUN.$cycle.status.$tmmark.bufr_d
      rm $file2
      msg="***WARNING: DUMP status file successfully generated but only from \
dump status2 file - dump status1 file not present"
      $DATA/postmsg "$jlogfile" "$msg"
   fi
   msg="DUMP status file successfully generated"
   $DATA/postmsg "$jlogfile" "$msg"
fi



########################################################################
#                  Prepare Data Counts for the SDM                     #
########################################################################

[ "$PROCESS_DATACOUNTS" = 'YES' ]  &&  $USHobsproc/bufr_datacount.sh



if [ "$PROCESS_REMOREST" = 'YES' ]; then

########################################################################
#   Remove or Mask Restricted Data from today's BUFR Data Dump Files   #
########################################################################

#  For dump files "adpsfc", "aircar", "aircft", "msonet", "lghtng" and "sfcshp"
#  for today (this network, run and cycle):
#  If either the restriction indicator (mnemonic RSRD) is set for a report, or
#  the report is in a Table A entry pre-determined to contain only restricted
#  reports, then the report is always restricted {regardless of the time in
#  hours for the expiration on restriction (mnemonic EXPRSRD)}.
#    ---> Since we are running in near-realtime, there is no need to test on
#         the value of EXPRSRD.  This will always be > current time difference.
#    ---> For all Table A entries listed in parm card namelist switch
#         "MSG_RESTR" (see below):
#            All reports are considered to be restricted by definition of this
#            switch. They will be skipped by program BUFR_REMOREST.
#    ---> For all Table A entries listed in parm card namelist switch
#         "MSG_MIXED" (see below):
#            RSRD may not be set for all reports. Those with RSRD set are
#            considered to be restricted regardless of EXPRSRD because
#            "DIFF_HR" (the difference in hours between the current UTC wall-
#            clock date and the dump file center date),is exported as 0 into
#            program BUFR_REMOREST (EXPRSRD will always be > actual DIFF_HR
#            since we are running in near-realtime).  They will be skipped by
#            program BUFR_REMOREST.  Those reports that do not have RSRD set
#            are considered to be non-restricted and will be retained by
#            program BUFR_REMOREST.
#    ---> For all Table A entries listed in parm card namelist switch
#         "MSG_MASKA" (see below):
#            All reports are considered to be restricted by definition of this
#            switch.  Reports are not removed but all occurrences of their
#            report ids are unilaterally masked out by program BUFR_REMOREST.
#            In addition, program BUFR_REMOREST re-sets their values for RSRD
#            and EXPRSRD to missing so that the reports are no longer
#            considered to be restricted.
# -----------------------------------------------------------------------------

   msg="REMOVE OR MASK RESTRICTED DATA FROM $tmmark_uc $net_uc DATA DUMPS \
CENTERED ON $dumptime"
   $DATA/postmsg "$jlogfile" "$msg"

   export DIFF_HR=0

cat <<\EOFparm > bufr_remorest.datadump.parm
=========================================================================

  Cards for DUMP Version of BUFR_REMOREST -- Version 18 November 202?

 &SWITCHES
   MSG_RESTR = 'NC000000',   ! These are the Table A Entries for BUFR messages
               'NC000100',   !  for which ALL reports are considered to be
               'NC004003',   !  restricted. All reports will be REMOVED by
               'NC004004',   !  program BUFR_REMOREST.
	       'NC004006',   !  (up to 20 Tabel A entries)
               'NC004007',
               'NC004008',
               'NC004009',
               'NC004010',
               'NC004011',
               'NC004012',
               'NC004013',
               'NC004103',
               'NC007001',
               'NC007002',
               'NC021242',
               'NC255xxx',   ! move this to MSG_MIXED when we know for sure that RSRD values are correct for each provider/sub-provider
   MSG_MIXED = 'NC012004',   ! These are the Table A Entries for BUFR messages
               'NC003010',   !  which may contain a MIXTURE of reports with and
               '        ',   !  without mnemonic "RSRD" being set. If "RSRD" is
               '        ',   !  not set -or- it is set and mnemonic "EXPRSRD"
               '        ',   !  is also set and has a value less than "DIFF_HR"
               '        ',   !  (the difference in hrs between the current UTC
	       '        ',   !  wall-clock date and the dump file center date)
               '        ',   !  minus 4, the report will be RETAINED by program
               '        ',   !  BUFR_REMOREST. Otherwise, it will be REMOVED by
               '        ',   !  program BUFR_REMOREST.
               '        ',   !  (up to 20 Tabel A entries)
               '        ',
   MSG_MASKA = 'NC001001',   ! These are the Table A Entries for BUFR messages
               'NC001101'    !  for which ALL reports are considered to be 
                             !  restricted.  They will not be removed, but all
                             !  occurrences of their report ids will be
                             !  unilaterally CHANGED, by program BUFR_REMOREST,
                             !  to either "MASKSTID" (where the id is stored by
                             !  itself)  or to all "X"'s where the number of
                             !  "X"'s corresponds to the number of characters
                             !  in the original report id (where the id is
                             !  embedded in the raw report bulletin header
                             !  string). In addition, program BUFR_REMOREST re-
                             !  sets their values for "RSRD" and "EXPRSRD" to
                             !  missing so that the reports are no longer
                             !  considered to be restricted.
                             !  (up to 20 Table A entries)

 /

    Note 1: A particular Table A entry should NEVER appear in more than one of
            MSG_RESTR, MSG_MIXED or MSG_MASKA.
    Note 2: Any Table A entry not in either MSG_RESTR, MSG_MIXED or MSG_MASKA
            is assumed to be a Table A entry for BUFR messages for which ALL
            reports are NON-RESTRICTED (these messages are copied intact, no
            reports are unpacked).
    Note 3: Always fill in these arrays beginning with word 1.  If there are
            less than 20 words filled in an array, either set the extra words
            to "        " (8 blank characters) or do not specify them here
            (they default to "        ").
    Note 4: For data dump Table A entries in the form "NCtttsss", where "ttt"
            is the BUFR message type and "sss" is the BUFR message subtype, if
            the last three characters (the subtype) is specified as 'xxx', then
            ALL BUFR messages of that type are either treated as having all
            restricted data all which is to be removed (if in MSG_RESTR), mixed
            data some of which is to be removed (if in MSG_MIXED) or all
            restricted data all of which is to have its report id masked (if in
            MSG_MASKA), regardless of the message subtype. (For example, if
            MSG_RESTR(1)='NC255xxx', then ALL mesonet BUFR messages are
            considered to have all restricted data and are all removed
            regardless of their subtype.)

=========================================================================
EOFparm

   REMX=${REMX:-$EXECobsproc/bufr_remorest}
   REMC=${REMC:-bufr_remorest.datadump.parm}

   for file in adpsfc aircar aircft msonet sfcshp lghtng gpsipw saphir gpsro
   do
      filestem=$RUN.$cycle.$file.$tmmark.bufr_d
      [ -f $COMIN/$filestem ]  ||  continue

      cp $COMIN/$filestem $filestem

      $USHobsproc/bufr_remorest.sh $filestem
      rc=$?
      if [ $rc -gt 0 ] ; then
         [ $rc -gt $retcode ]  && retcode=$rc
         msg="**WARNING: ERROR generating non-restricted $file BUFR file \
(rc = $rc)"
         $DATA/postmsg "$jlogfile" "$msg"
      else
         msg="Successful generation of non-restricted $file BUFR file"
         $DATA/postmsg "$jlogfile" "$msg"
         cp $filestem $COMOUT/$filestem.nr
         chmod 664 $COMOUT/$filestem.nr
	 if [ "$SENDDBN" = "YES" ] ; then
           NETUP=`echo $RUN | tr {a-z} {A-Z}`
           if  [[ $NETUP != 'GDAS' ]] || [[ $file != "saphir" ]]; then    ### no alert gdas.tCCz.saphir.tm00.bufr_d.nr 
             if [[ $NETUP != 'CDAS' ]] || [[ $file != "gpsro" ]]; then    ### no alert cdas.tCCz.gpsro.tm00.bufr_d.nr
               if [[ $NETUP != 'RAP' ]] || [[ $file != "gpsro" ]]; then   ### no alert rap.tCCz.gpsro.tm00.bufr_d.nr
                   $DBNROOT/bin/dbn_alert MODEL ${NETUP}_BUFR_${file}_nr $job \
                   $COMOUT/$filestem.nr
	       fi
             fi
           fi
         fi
      fi

   done

fi #  endif loop $PROCESS_REMOREST


# PROCESS_REMOREST_dm2 = YES runs only on demand, on dev machines
# Unrestrict 48h old aircraft obs(exc. TAMDAR), add *.ur file to $COMOUTm2 
dev_m=$(grep backup /lfs/h1/ops/prod/config/prodmachinefile | cut -d: -f2)
this_m=$(cat /etc/cluster_name)

if [ $PROCESS_REMOREST_dm2 = YES -a $this_m = $dev_m ]; then
########################################################################
#     Remove Restriction on Data in 2-day old "aircar" and "aircft"    #
#                         BUFR Data Dump files                         #
########################################################################

#  For dump files "aircar" and "aircft" for 2-days ago (this network, run and
#  cycle):
#    ---> For all Table A entries listed in parm card namelist switch
#         "MSG_MIXED" (see below):
#            If the restriction indicator (mnemonic RSRD) is set for a report,
#            then its value for time in hours for the expiration on restriction
#            (mnemonic EXPRSRD) is examined. If "DIFF_HR" (the difference in
#            hours between the current UTC wall-clock date and the dump file
#            center date, exported into program BUFR_REMOREST) minus 4 is less
#            than or equal to EXPRSRD, then the report is considered to be
#            restricted and will be skipped by program BUFR_REMOREST.
#            Otherwise, the report is considered to be non-restricted and will
#            be retained by program BUFR_REMOREST.
#    ---> Since we are running 2-days late, we want to test on the value of
#         EXPRSRD (which currently should be set to 48 hours for any reports in
#         these dumps which have RSRD set).  This will allow non-rstprod users
#         to have access to full "aircar" and "aircft" dumps after EXPRSRD + 4
#         (52) hours.
# -----------------------------------------------------------------------------

   dumptimeM2=`$NDATE -$tmhr $PDYm2$cyc`

   msg="REMOVE OR MASK RESTRICTED DATA FROM $tmmark_uc $net_uc DATA DUMPS \
CENTERED ON $dumptimeM2 (2-days ago)"
   $DATA/postmsg "$jlogfile" "$msg"

   ymdh=$(date -u +'%Y%m%d%H')
   export DIFF_HR=`$NHOUR $ymdh            $dumptimeM2`
#                         current_date       dump_date

   msg="Any reports with EXPRSRD less than `expr $DIFF_HR - 4 ` hrs will now \
be retained"
   $DATA/postmsg "$jlogfile" "$msg"

cat <<\EOF_EXPRSRDparm > bufr_remorest.datadump_EXPRSRD.parm
=========================================================================

  Cards for DUMP Version of BUFR_REMOREST -- Version 1.1.0 (09 September 2015)
  (documentation in bufr_remorest.datadump.parm above for $PROCESS_REMOREST
   also applies here)

 &SWITCHES
   MSG_RESTR = '        ',
   MSG_MIXED = 'NC004003',
               'NC004004',
               'NC004006',
               'NC004007',
               'NC004008',
               'NC004009',
               'NC004010',
               'NC004011',
               'NC004012',
               'NC004013',
               'NC004014',
               'NC004015',
               'NC004103',
   MSG_MASKA = '        '

 /

=========================================================================
EOF_EXPRSRDparm

   REMX=${REMX:-$EXECobsproc_shared_bufr_remorest/bufr_remorest}
   REMC=${REMC_EXPRSRD:-bufr_remorest.datadump_EXPRSRD.parm}

   for file in aircar aircft
   do
      filestem=$RUN.$cycle.$file.$tmmark.bufr_d
      [ -f $COMINm2/$filestem ]  ||  continue

      cp $COMINm2/$filestem $filestem

      $USHobsproc/bufr_remorest.sh $filestem
      rc=$?
      if [ $rc -gt 0 ] ; then
         [ $rc -gt $retcode ]  && retcode=$rc
         msg="**WARNING: ERROR generating non-restricted $file BUFR file \
from 2-days ago (rc = $rc) -- existing file made 2-days ago is not overwritten"
         $DATA/postmsg "$jlogfile" "$msg"
      else
         msg="Successful generation of non-restricted $file BUFR file from \
2-days ago -- overwrite existing file made 2-days ago"
         $DATA/postmsg "$jlogfile" "$msg"
         cp $filestem $COMOUTm2/$filestem.ur
         chmod 664 $COMOUTm2/$filestem.ur
	 if [ $SENDDBN = "YES" ] ; then
             NETUP=`echo $RUN | tr {a-z} {A-Z}`           # can this be net_uc?
             $DBNROOT/bin/dbn_alert MODEL ${NETUP}_BUFR_${file}_nr $job \
             $COMOUTm2/$filestem.ur 
	 fi
      fi

   done

fi #  endif loop $PROCESS_REMOREST_dm2


if [ "$PROCESS_UNBLKBUFR" = 'YES' ]; then

   msg="START THE UNBLOCKING OF $tmmark_uc $net_uc DATA DUMPS CENTERED ON \
$dumptime"
   $DATA/postmsg "$jlogfile" "$msg"

########################################################################
#                  Unblock BUFR Data Dump Files                        #
# {Note: will also unblock non-restricted aircar and aircft dump files #
#        from 2-days ago if they were overwritten in REMOREST above    #
#        (when $PROCESS_REMOREST_dm2 = YES)}                           #
# ---> ON WCOSS dump files are already unblocked, so this whole set of #
#      processing can hopefully be removed someday!!                   #
########################################################################

   aircraft_nr_dm2=""
   [ $PROCESS_REMOREST_dm2 = YES -a $this_m = $dev_m ] && \
    aircraft_nr_dm2="aircar_nr_dm2 aircft_nr_dm2"

   for file in adpsfc adpupa aircar aircft satwnd sfcshp spssmi proflr \
               vadwnd goesnd erscat sfcbog erswnd ssmip  ssmipn ssmit  \
               atovs  qkscat qkswnd trmm   sptrmm geoimr 1bamua 1bamub \
               1bhrs3 1bmhs  1bhrs4 airs   airswm amsre  gpsipw msonet \
               rassda nexrad gpsro  airsev goesfv wndsat wdsatr osbuv8 \
               ascatt ascatw mtiasi avcsam avcspm gome   lghtng omi    \
               esamua esamub eshrs3 esmhs  ssmisu sevcsr lgycld efclam \
	       $aircraft_nr_dm2 
#  --> don't add any new dumps here since files are already unblocked
#      on WCOSS!!

   do
      file_orig=$file
      if [ $PROCESS_REMOREST_dm2 = YES -a $this_m = $dev_m ]; then
	 if [ $file = aircar_nr_dm2 -o $file = aircft_nr_dm2 ]; then
            file=`echo $file | cut -d"_" -f1`
	    COMIN_save=$COMIN
	    COMIN=$COMINm2
	    COMOUT_save=$COMOUT
	    COMOUT=$COMOUTm2
	 fi
      fi
      for qual in "" ".nr"
      do
	 [ \( $file_orig = aircar_nr_dm2 -o $file_orig = aircft_nr_dm2 \) -a \
	    "$qual" != .nr ] && continue
         qualdbn=$qual
         COMIN_here=$COMIN
         if [ "$qual" = .nr ]; then
            qualdbn=_nr
            COMIN_here=$COMOUT
         fi
         filestem=$RUN.$cycle.$file.$tmmark.bufr_d
         [ -f $COMIN_here/$filestem$qual ]  ||  continue

         cp $COMIN_here/$filestem$qual $file.$qual

# ---> ON WCOSS dump files are already unblocked, so for now just copy each one
#      to the unblok file location used before on CCS - hopefully this can be
#      removed someday!

         cp -p $file.$qual $file.unblock$qual >> $pgmout 2>&1
         rc=$?
         if [ $rc -gt 0 ] ; then
            [ $rc -gt $retcode ]  && retcode=$rc
            msg="**WARNING: ERROR copying $file$qual BUFR file to \
$file.unblock$qual (rc = $rc)"
            $DATA/postmsg "$jlogfile" "$msg"
         else
            if [ $file_orig = aircar_nr_dm2 -o $file_orig = aircft_nr_dm2 ];then
	       msg="$file$qual BUFR file from 2-days ago SUCCESSFULLY copied \
to $file.unblock$qual -- overwrite existing file made 2-days ago"
            else
               msg="$file$qual BUFR file SUCCESSFULLY copied to $file.unblock$qual"
	    fi
            $DATA/postmsg "$jlogfile" "$msg"
            cp $file.unblock$qual $COMOUT/$filestem.unblok$qual
            chmod 664 $COMOUT/$filestem.unblok$qual
            if [ "$SENDDBN" = "YES" ] ; then
              NETUP=`echo $RUN | tr {a-z} {A-Z}`

######################################################################
##  DO NOT alert unblocked gpsipw dump file (either restricted or   ##
##  non-restricted) until DATAFLOW adds alert types                 ##
##  ${NETUP}_BUFR_gpsipw_unblok and ${NETUP}_BUFR_gpsipw_unblok.nr  ##
######################################################################
              if [ $file != "gpsipw" ]; then
                $DBNROOT/bin/dbn_alert MODEL \
                 ${NETUP}_BUFR_${file}_unblok${qualdbn} $job \
                 $COMOUT/$filestem.unblok$qual
              fi
            fi
            if [ "$CHGRP_RSTPROD" = 'YES' ]; then
               if [ "$qual" != .nr ]; then
                  if [ -s $COMOUT/$filestem.nr ]; then
                     chgrp rstprod $COMOUT/$filestem.unblok$qual
                     errch=$?
                     if [ $errch -eq 0 ]; then
                        chmod 640 $COMOUT/$filestem.unblok$qual
                        msg="NOTE: \"unblocked\" (now really just copied) \
$file file contains RESTRICTED data, only users in rstprod group have read \
permission"
                        $DATA/postmsg "$jlogfile" "$msg"
                        echo " "
                        echo "$msg"
                        echo " "
                     else
                        cp /dev/null $COMOUT/$filestem.unblok$qual
                        msg="**WARNING: \"unblocked\" (now really just copied) \
$file file contains RESTRICTED data, since user $USER is not in rstprod group \
a null file is copied in its place"
                        $DATA/postmsg "$jlogfile" "$msg"
                        echo " "
                        echo "$msg"
                        echo " "
                     fi
                  fi
               fi
            fi
         fi

      done
      if [ $PROCESS_REMOREST_dm2 = YES -a $this_m = $dev_m ]; then
	 if [ $file_orig = aircar_nr_dm2 -o $file_orig = aircft_nr_dm2 ]; then
            COMIN=$COMIN_save
	    COMOUT=$COMOUT_save
	 fi
      fi

   done

fi #  endif loop $PROCESS_UNBLKBUFR



if [ "$PROCESS_LISTERS" = 'YES' ]; then

   msg="START THE LISTING OF $tmmark_uc $net_uc DATA DUMPS CENTERED ON \
$dumptime"
   $DATA/postmsg "$jlogfile" "$msg"

   retr=TRUE
   [ "$NET" = 'gfs' -o "$NET" = 'gdas' ]  &&  retr=FALSE
   radn=TRUE
   [ "$NET" = 'rap' ]  &&  radn=FALSE

   cat << EOFlistdumps > parms
 &PDATA
  IDAT10 = $dumptime10, IWINDE = -3, IWINDL = +2, IPRINT = 0,
  SNDG = FALSE, RETR = $retr, RADN = $radn
 /
EOFlistdumps

   cat <<\EOFthread > $DATA/thread
#VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
#  Herefile "thread" which can run in parallel (via MPMD methods) to list dumps
#VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
{ echo
      set -x
      mkdir -p $DATA/thread${1}
      cd $DATA/thread${1}
      file=$2
      filestem=$3

      cp $COMIN/$filestem $file

      cat <<EOFc > parms1
 &RDATA
  DSNAME_LC = "$file  "
 /
EOFc

      cat parms1 $DATA/parms > parms

      pgm=`basename  $LSTX`
      export pgm
      . $DATA/prep_step

      echo "  for $file BUFR file" >> $DATA/thread${1}/$pgmout

      unset FORT00 `env | grep "FORT[0-9]\{1,\}" | awk -F= '{print $1}'`
      export FORT21=$file

      time -p $LSTX < parms > $file.listing 2> errfile
      errlst=$?
      echo "$errlst" > $DATA/error${1}
      cp $file.listing $COMOUT/$filestem.listing
      chmod 664 $COMOUT/$filestem.listing
      cat errfile >> $DATA/thread${1}/$pgmout
      if [ "$errlst" -ne '0' ]; then
         set +x
         echo
         echo \
" ---------------------------------------------------------------------------"
         echo \
" ********** ERROR IN PROGRAM BUFR_LISTDUMPS for $file BUFR file **********"
         echo \
" ---------------------------------------------------------------------------"
         echo
         set -x
         msg="*WARNING: Program BUFR_LISTDUMPS DID NOT complete successfully \
for $file BUFR file (rc = $errlst)"
         $DATA/postmsg "$jlogfile" "$msg"
      else
         set +x
         echo
         echo \
" ---------------------------------------------------------------------------"
         echo \
" ********** COMPLETED PROGRAM BUFR_LISTDUMPS for $file BUFR file **********"
         echo \
" ---------------------------------------------------------------------------"
         echo
         set -x
         msg="Program BUFR_LISTDUMPS completed normally for $file BUFR file"
         $DATA/postmsg "$jlogfile" "$msg"
         if [ "$CHGRP_RSTPROD" = 'YES' ]; then
            if [ -s $COMOUT/$filestem.nr ]; then
               chgrp rstprod $COMOUT/$filestem.listing
               errch=$?
               if [ $errch -eq 0 ]; then
                  chmod 640 $COMOUT/$filestem.listing
                  msg="NOTE: $file listing file contains RESTRICTED data, \
only users in rstprod group have read permission"
                  $DATA/postmsg "$jlogfile" "$msg"
                  echo " "
                  echo "$msg"
                  echo " "
               else
                  cp /dev/null $COMOUT/$filestem.listing
                  msg="**WARNING: $file listing file contains RESTRICTED \
data, since user $USER is not in rstprod group a null file is copied in its \
place"
                  $DATA/postmsg "$jlogfile" "$msg"
                  echo " "
                  echo "$msg"
                  echo " "
               fi
            fi
         fi
      fi
} > $DATA/thread${1}.out 2>&1
#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
#                     End of herefile "thread"
#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
EOFthread
   chmod 775 $DATA/thread

   index=0

########################################################################
#                    List BUFR Data Dump Files                         #
########################################################################

   LSTX=${LSTX:-$EXECobsproc/bufr_listdumps}

   for file in msonet satwnd goesfv aircar ascatw adpsfc gpsipw sfcshp \
               aircft adpupa proflr vadwnd rassda goesnd wdsatr spssmi \
               sfcbog qkswnd erscat 
   do
      filestem=$RUN.$cycle.$file.$tmmark.bufr_d
      [ -f $COMIN/$filestem ]  ||  continue
      index=`expr $index + 1`
      if [[ "${MPMD}" == YES ]]; then
         echo $DATA/thread $index $file $filestem  >> $DATA/mpmd.cmdfile
      else # run serially... this may be slow!
         $DATA/thread $index $file $filestem 
      fi
   done

   if [[ "${MPMD}" == YES ]]; then
      if [ -s $DATA/mpmd.cmdfile ]; then
      echo
      echo "------------------------------------------------------------------"
      echo "Invoking poe or poe-like mpmd processing: `date -u`"
      echo

      ntasks=$(wc -l $DATA/mpmd.cmdfile | awk '{print $1}')
      if [ $ntasks -eq 1 ]; then
         # no need for overhead of aprun or mpirun.lsf and cfp if only one thread
         chmod +x $DATA/mpmd.cmdfile
         $DATA/mpmd.cmdfile 2>&1
      else
         which cfp
         which_cfp_err=$?
         mpiexec -np 14 --cpu-bind verbose,core cfp $DATA/mpmd.cmdfile   
   fi
else
   echo
   echo "==> There are no tasks in MPMD Command File - MPMD not run"
   echo
fi
echo
echo "Ending mpmd processing  : `date -u`"
echo "--------------------------------------------------------------------"
echo
echo
fi

   for n in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
   do
      [ -d $DATA/thread${n} ]  || break
      cat $DATA/thread${n}.out
      cat break $DATA/thread${n}/$pgmout break >> $pgmout
      err=`cat $DATA/error${n}`
      [ $err -gt $retcode ]  && retcode=$err
   done

fi #  endif loop $PROCESS_LISTERS



if [ "$PROCESS_AVGTABLES" = 'YES' ]; then

########################################################################
#    If this is GDAS network (normally at 18Z only) ...                #
#    Update Data Count Average Tables for gdas Network                 #
#                                                                      #
#    If this is GFS network (normally at 18Z only) ...                 #
#    Update Data Count Average Tables for gfs Network                  #
#                                                                      #
#    If this is NAM tm00 network (normally at 18Z only) ...            #
#    Update Data Count Average Tables for nam tm00 Network             #
#                                                                      #
#    If this is RAP network (normally full cycle RAP at 23Z only) ...  #
#    Update Data Count Average Tables for rap Network                  #
#                                                                      #
#    If this is RTMA network (not RTMA_RU and normally at 23Z only) ...#
#    Update Data Count Average Tables for rtma Network                 #
#                                                                      #
#    If this is URMA network (normally at 23Z only) ...                #
#    Update Data Count Average Tables for urma Network                 #
########################################################################

   if [ "$NET" = 'gdas' -o "$NET" = 'gfs' -o "$NET" = 'nam' ]; then
      networks=$NET
   elif [ "$NET" = 'rap' -a "$RUN" = 'rap' ]; then
      networks=rap
   elif [ "$NET" = 'rtma' ]; then
      networks=rtma
   elif [ "$NET" = 'urma' ]; then
      networks=urma
   fi

   mkdir -p $AVGDarch_OUT

   for network in $networks
   do
      network_uc=$(echo $network | tr [a-z] [A-Z])

      $USHobsproc/bufr_avgdata.sh $network
      errsc=$?
      if [ $UPDATE_AVERAGE_FILE = YES ]; then
      if [ "$errsc" -eq '0' ]; then
         rm $AVGDarch_OUT/obcount_30davg.${network}.current
         cp obcount_30davg.${network}.current \
          $AVGDarch_OUT/obcount_30davg.${network}.current
         chmod 775 $AVGDarch_OUT/obcount_30davg.${network}.current
         msg="Data count average table SUCCESSFULLY updated for $network_uc \
network"
         $DATA/postmsg "$jlogfile" "$msg"
         typeset -Z2 this_month last_month
         this_month=`echo $PDY | cut -c5-6`
         year=`echo $PDY | cut -c1-4`
         last_month=`expr $this_month - 1`
         if [ "$last_month" -eq '0' ]; then
            last_month=12
            year=`expr $year - 1`
         fi
         if [ "$network" = 'nam' -o "$network" = 'gfs' -o "$network" = 'gdas' ]
         then
        if [ ! -s $AVGDarch_OUT/obcount_30davg.${network}.${year}${last_month} ]
            then

#  If no data count average table found for previous month, save this one

               rm $AVGDarch_OUT/obcount_30davg.${network}.*${last_month}
               cp obcount_30davg.${network}.current \
                $AVGDarch_OUT/obcount_30davg.${network}.${year}${last_month}
              chmod 775 $AVGDarch_OUT/obcount_30davg.${network}.${year}${last_month}
               msg="DATA COUNT AVERAGE table for ${year}${last_month} saved \
for $network_uc network"
               $DATA/postmsg "$jlogfile" "$msg"
            fi
         fi
      else
         msg="Data count average table NOT updated for $network_uc network"
         $DATA/postmsg "$jlogfile" "$msg"
      fi
      else
         msg="Data count average table NOT updated for $network_uc network \
by choice"
         $DATA/postmsg "$jlogfile" "$msg"
      fi
   done

fi #  endif loop $PROCESS_AVGTABLES

#######################
 

# GOOD RUN
set +x
echo " "
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " "
set -x


# save standard output

cat  break $pgmout break > allout
cat allout
# rm allout
 
if [ $retcode -eq 0 ] ; then
   msg="HAS COMPLETED NORMALLY. #################################"
   $DATA/postmsg "$jlogfile" "$msg"
else
   msg="HAS COMPLETED WITH RETURN CODE $retcode. ################"
   $DATA/postmsg "$jlogfile" "$msg"
fi

################## END OF SCRIPT #######################
