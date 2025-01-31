#!/bin/bash
############################################################################
echo "-----------------------------------------------------------          "
echo "exnam_dump.sh     - NAM model data dump processing                   "
echo "-----------------------------------------------------------          "
echo "History: Jan 10 2000 - Original script.                              "
echo "         Sep 10 2014 - Use parallel scripting to process dump groups."
echo "         Feb  3 2015 - Dump cris, atms, ssmisu and sevcsr.  Dump new "
echo "                       satwnd types NC005019, NC005080, NC005090.    "
echo "                       Reorder cfp commands to speed up run.         "
echo "         Aug 16 2016 - GPSIPW dump window reset for new data stream. "
echo "         Sep 23 2016 - Dump windows adjusted  for tm06-tm00 to       "
echo "                       support NAMv4 release which runs hourly       "
echo "                       catch-up cycles with dump windows             "
echo "                       (-0.50,+0.50) with some exceptions.           "
echo "                       lghtng added to Dump group #8.                "
echo "         May 21 2019 - Updated to run on Dell-p3.                    "
echo "                       Skip 2 tanks in msonet dump group that NAM    "
echo "                       will not process; frozen code.                "
echo "                       Removed the copy of 8th mesh snow depth file. "
echo "                       Removed dumping of radwnd dump group; ingest  "
echo "                       job not being ported to Dell-p3.              "
echo "                       Disabled background threading.                "
echo "         Feb 22 2021 - Disabled DBN alerts for gpsro dump files.     "
echo "                       These gpsro dump files have the potential to  "
echo "                       contain commercial data.  The equivalent      "
echo "                       non-restricted gpsro dump files are alerted   "
echo "                       instead.                                      "
echo "         Mar 09 2021 - Incremented subsets for the sfcshp dump groups"
echo "                       to match bufr_dumplist. Removed tideg from    "
echo "                       sfcshp dump group to make unique dump file.   "
echo "                     - Copy bufr_dumplist to COMOUT.                 "
echo "         Dec 09 2021 - Updated for use on WCOSS2                     " 
echo "         Oct 17 2023 - Split msonet to msonet and msone1 (b255/xx030)"
echo "         Mar 14 2024 - Separate msonet to own dump group             "
############################################################################

set -xau

# set some variables if they have not already been set

set +u

# JOB_NUMBER = 1 indicates the prepbufr dump job.
# JOB_NUMBER = 2 indicates the non-prepbufr dump job.
# JOB_NUMBER not present indicates dump BOTH prepbufr and non-prepbufr data.
# -----------------------------------------------------------------------------
# Dump group #1 (non-pb) = 1bamua 1bmhs gpsro mtiasi esamua eshrs3 sevcsr atms
# Dump group #2 (pb) = vadwnd satwnd
# Dump group #3 (pb) = proflr rassda sfcshp tideg adpsfc ascatt
# Dump group #4 (pb) = gpsipw (moved msonet)
# Dump group #5 (pb) = aircft aircar goesnd  --> dump is GLOBAL
# Dump group #6 (non-pb) = nexrad            --> dump is GLOBAL
# Dump group #7 (non-pb) = goesfv lgycld     --> dump is GLOBAL
# Dump group #8 (non-pb) = 1bhrs4 airsev osbuv8 esmhs ssmisu cris lghtng
# Dump group #9 (pb) = adpupa
# Dump group #10 (pb)= msonet->msone0
# Dump group #11 (pb)= msone1
# Dump group #12 STATUS FILE
# -----------------------------------------------------------------------------

#VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
# The settings below are based on a future change when the DUMP job will dump
#  only types that go into PREPBUFR and the DUMP2 job will dump only types that
#  do not go into PREPBUFR.  This will speed up the DUMP + PREP processing.
# Although the logic is in place to now do this (see below), for now we will
#  continue to dump only "nexrad" in the NAM DUMP2 jobs via the following
#  switch settings ...
# -----------------------------------------------------------------------------

if [ -n "$JOB_NUMBER" ]; then  
set -u
   if [ $JOB_NUMBER = 2 ]; then
      DUMP_group1="NO"
      DUMP_group7="NO"
      DUMP_group8="NO"
   else
      DUMP_group1="YES"
      DUMP_group7="YES"
      DUMP_group8="YES"
   fi
fi
set +u
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

if [ -n "$JOB_NUMBER" ]; then  
set -u
   if [ $JOB_NUMBER = 2 ]; then
      dump_ind=DUMP2
      DUMP_group1=${DUMP_group1:-"YES"}
      DUMP_group2=${DUMP_group2:-"NO"}
      DUMP_group3=${DUMP_group3:-"NO"}
      DUMP_group4=${DUMP_group4:-"NO"}
      DUMP_group5=${DUMP_group5:-"NO"}
      DUMP_group6=${DUMP_group6:-"YES"}
      DUMP_group7=${DUMP_group7:-"YES"}
      DUMP_group8=${DUMP_group8:-"YES"}
      DUMP_group9=${DUMP_group9:-"NO"}
      DUMP_group10=${DUMP_group10:-"NO"}
      DUMP_group11=${DUMP_group11:-"NO"}
   else
      dump_ind=DUMP
      DUMP_group1=${DUMP_group1:-"NO"}
      DUMP_group2=${DUMP_group2:-"YES"}
      DUMP_group3=${DUMP_group3:-"YES"}
      DUMP_group4=${DUMP_group4:-"YES"}
      DUMP_group5=${DUMP_group5:-"YES"}
      DUMP_group6=${DUMP_group6:-"NO"}
      DUMP_group7=${DUMP_group7:-"NO"}
      DUMP_group8=${DUMP_group8:-"NO"}
      DUMP_group9=${DUMP_group9:-"YES"}
      DUMP_group10=${DUMP_group10:-"YES"}
      DUMP_group11=${DUMP_group11:-"YES"}
   fi
else
   dump_ind=DUMP
   DUMP_group1=${DUMP_group1:-"YES"}
   DUMP_group2=${DUMP_group2:-"YES"}
   DUMP_group3=${DUMP_group3:-"YES"}
   DUMP_group4=${DUMP_group4:-"YES"}
   DUMP_group5=${DUMP_group5:-"YES"}
   DUMP_group6=${DUMP_group6:-"YES"}
   DUMP_group7=${DUMP_group7:-"YES"}
   DUMP_group8=${DUMP_group8:-"YES"}
   DUMP_group9=${DUMP_group9:-"YES"}
   DUMP_group10=${DUMP_group10:-"YES"}
   DUMP_group11=${DUMP_group11:-"YES"}
fi

if [ $tmmark = tm00 ]; then
   ADPUPA_wait=${ADPUPA_wait:-"YES"}
########ADPUPA_wait=${ADPUPA_wait:-"NO"} # saves ~15 sec if ADPUPA_wait=NO
   CHECK_STATUS=${CHECK_STATUS:-"NO"}
else
   ADPUPA_wait=${ADPUPA_wait:-"NO"}
   CHECK_STATUS=${CHECK_STATUS:-"YES"}
fi


# send extra output of DUMP2 for monitoring purposes.
set +u
if [ -n "$JOB_NUMBER" ]; then  
   [ $JOB_NUMBER = 2 ]  && export PS4='$SECONDS + '
fi
set -u

# Make sure we are in the $DATA directory
cd $DATA

msg="HAS BEGUN on `hostname`"
$DATA/postmsg "$jlogfile" "$msg"
 
cat break > $pgmout

tmhr=`echo $tmmark|cut -c3-4`
export dumptime=`$NDATE -$tmhr $PDY$cyc`
export cycp=`echo $dumptime|cut -c9-10`

RUN_uc=$(echo $RUN | tr [a-z] [A-Z])
tmmark_uc=$(echo $tmmark | tr [a-z] [A-Z])

msg="$RUN_uc ANALYSIS TIME IS $PDY$cyc"
$DATA/postmsg "$jlogfile" "$msg"

set +x
echo
echo "CENTER DATA DUMP DATE-TIME FOR $tmmark_uc $RUN_uc IS $dumptime"
echo
set -x

export COMSP=$COMOUT/$RUN.${cycle}.
 
if [ "$PROCESS_GRIBFLDS" = 'YES' ]; then

####################################
####################################
#  copy imssnow from $TANK_GRIBFLDS
####################################
####################################

   imssnow=$TANK_GRIBFLDS/$PDY/wgrbbul/imssnow.grb
   imssold=$TANK_GRIBFLDS/$PDYm1/wgrbbul/imssnow.grb

   if [ -s $imssnow ]; then
      cp $imssnow ${COMSP}imssnow.grb
##### $CNVGRIB -g12 -p40 ${COMSP}imssnow.grb ${COMSP}imssnow.grb.grib2
##### $WGRIB2 ${COMSP}imssnow.grb.grib2 -s > ${COMSP}imssnow.grb.grib2.idx
      msg="todays IMS snow grib file located and copied to ${COMSP}imssnow.grb"
      $DATA/postmsg "$jlogfile" "$msg"
      if [ "$SENDDBN" = 'YES' ]; then
         $DBNROOT/bin/dbn_alert MODEL NAMIMSSNOW $job ${COMSP}imssnow.grb
      fi
##### if [ "$SENDDBN" = 'YES' ]; then
#####    $DBNROOT/bin/dbn_alert MODEL NAMIMSSNOW_GB2 $job \
#####     ${COMSP}imssnow.grb.grib2
#####    $DBNROOT/bin/dbn_alert MODEL NAMIMSSNOW_GB2_WIDX $job \
#####     ${COMSP}imssnow.grb.grib2.idx
##### fi
   elif [ -s $imssold ]; then
      cp $imssold ${COMSP}imssnow.grb
      msg="**todays IMS snow grib file not located - copy 1-day old file"
      $DATA/postmsg "$jlogfile" "$msg"
      if [ "$SENDDBN" = 'YES' ]; then
         $DBNROOT/bin/dbn_alert MODEL NAMIMSSNOW $job ${COMSP}imssnow.grb
      fi
   else
      set +x
      echo " "
      echo " ##########################################"
      echo " cannot locate IMS snow grib file "
      echo " non-fatal error (no longer used by NAM,"
      echo " but might cause failure in RCDAS)"
      echo " ##########################################"
      echo " "
      set -x
      msg="**CANNOT LOCATE IMS SNOW GRIB FILE --> non-fatal"
      $DATA/postmsg "$jlogfile" "$msg"
   fi

# US AFWA 8th mesh snow depth data discontinued 5/31/2019
#  snowdepth=$TANK_GRIBFLDS/$PDY/wgrbbul/snowdepth.grb
#  snowdeold=$TANK_GRIBFLDS/$PDYm1/wgrbbul/snowdepth.grb
#  if [ -s $snowdepth ]; then
#     cp $snowdepth ${COMSP}snowdepth.grb
#     msg="todays snow depth grib file located and copied to ${COMSP}snowdepth.grb"
#     $DATA/postmsg "$jlogfile" "$msg"
#  elif [ -s $snowdeold ]; then
#     cp $snowdeold ${COMSP}snowdepth.grb
#     msg="**todays snow depth grib file not located - copy 1-day old file"
#     $DATA/postmsg "$jlogfile" "$msg"
#  else
#     set +x
#     echo " "
#     echo " ##########################################"
#     echo " cannot locate snow depth grib file "
#     echo " non-fatal error (no longer used by NAM,"
#     echo " but might cause failure in RCDAS)"
#     echo " ##########################################"
#     echo " "
#     set -x
#     msg="**CANNOT LOCATE SNOW DEPTH GRIB FILE --> non-fatal"
#     $DATA/postmsg "$jlogfile" "$msg"
#  fi

fi  #  endif loop $PROCESS_GRIBFLDS


echo "=======> Dump group 1 (thread_1) not executed." > $DATA/1.out
echo "=======> Dump group 2 (thread_2) not executed." > $DATA/2.out
echo "=======> Dump group 3 (thread_3) not executed." > $DATA/3.out
echo "=======> Dump group 4 (thread_4) not executed." > $DATA/4.out
echo "=======> Dump group 5 (thread_5) not executed." > $DATA/5.out
echo "=======> Dump group 6 (thread_6) not executed." > $DATA/6.out
echo "=======> Dump group 7 (thread_7) not executed." > $DATA/7.out
echo "=======> Dump group 8 (thread_8) not executed." > $DATA/8.out
echo "=======> Dump group 9 (thread_9) not executed." > $DATA/9.out
echo "=======> Dump group 10 (thread_10) not executed." > $DATA/10.out
echo "=======> Dump group 11 (thread_11) not executed." > $DATA/11.out

err1=0
err2=0
err3=0
err4=0
err5=0
err6=0
err7=0
err8=0
err9=0
err10=0
err11=0
if [ "$PROCESS_DUMP" = 'YES' ]; then

#######################################################################
#######################################################################
#  The data "dump" script for tm06, tm05, tm04, tm03, tm02, tm01, tm00
#######################################################################
#######################################################################

msg="START THE $tmmark_uc $RUN_uc DATA $dump_ind CENTERED ON $dumptime"
$DATA/postmsg "$jlogfile" "$msg"

if [ $CHECK_STATUS = YES -a -s ${COMSP}status${JOB_NUMBER}.${tmmark}.bufr_d ]
then

msg="**WARNING: status${JOB_NUMBER} file already exists for $tmmark \
$PDY$cyc run - no data dumps produced"
$DATA/postmsg "$jlogfile" "$msg"

else

set +x
#----------------------------------------------------------------
cat<<\EOF>thread_1; chmod +x thread_1
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_1
echo Executing on node  `hostname`
echo Starting time: `date -u`
echo "********************************************************************"
echo
set -x

export STATUS=NO
export DUMP_NUMBER=1

#============================================================================
# Dump # 1 : 1BAMUA, 1BMHS, GPSRO, MTIASI, ESAMUA, ESHRS3, SEVCSR, ATMS
#              (1)    (1)    (1)     (1)     (1)     (1)     (1)    (1)
#            -- TOTAL NUMBER OF SUBTYPES = 8
#            for catch-up cycles tm06-01:
#              time window radius is (-0.50,+0.50) hours for all types except
#              1BAMUA, 1BMHS, MTIASI, ATMS where it is (-1.00,+0.99) hours
#            for on-time cycles tm00:
#              time window radius is (-0.75,+1.50) hours
#============================================================================

if [ "$tmhr" = "00" ]; then
  for dump_file in 1bamua 1bmhs gpsro mtiasi esamua eshrs3 sevcsr atms; do
    eval DTIM_earliest_$dump_file=\${DTIM_earliest_$dump_file:-"-0.75"}
    eval DTIM_latest_$dump_file=\${DTIM_latest_$dump_file:-"+1.50"}
  done
else
  for dump_file in 1bamua 1bmhs mtiasi atms; do
    eval DTIM_earliest_$dump_file=\${DTIM_earliest_$dump_file:-"-1.00"}
    eval DTIM_latest_$dump_file=\${DTIM_latest_$dump_file:-"+0.99"}
  done
fi

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 1bamua 1bmhs gpsro mtiasi \
 esamua eshrs3 sevcsr atms
error1=$?
echo "$error1" > $DATA/error1

if [ "$SENDDBN" = 'YES' ]; then
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_1bamua $job ${COMSP}1bamua.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_1bmhs  $job ${COMSP}1bmhs.$tmmark.bufr_d
# gpsro dump file has nr version which is alerted from exdump_post.sh
# $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_gpsro  $job ${COMSP}gpsro.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_mtiasi $job ${COMSP}mtiasi.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_esamua $job ${COMSP}esamua.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_eshrs3 $job ${COMSP}eshrs3.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_sevcsr $job ${COMSP}sevcsr.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_atms   $job ${COMSP}atms.$tmmark.bufr_d
fi

set +x
echo "********************************************************************"
echo Script thread_1
echo Finished executing on node  `hostname`
echo Ending time  : `date -u`
echo "********************************************************************"
set -x
} > $DATA/1.out 2>&1
EOF
set -x

set +x
#----------------------------------------------------------------
cat<<\EOF>thread_2; chmod +x thread_2
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_2
echo Executing on node  `hostname`
echo Starting time: `date -u`
echo "********************************************************************"
echo
set -x

export STATUS=NO
export DUMP_NUMBER=2

#============================================================================
# Dump # 2 : VADWND, SATWND
#              (2)    (14) 
#            -- TOTAL NUMBER OF SUBTYPES = 16
#            for catch-up cycles tm06-01:
#              time window radius is (-0.50,+0.50) hours for all types except
#              SATWND subtypes 005/010, 005/011, 005/012, 005/019 where it is
#              (-0.50,-0.01) hours
#            for on-time cycles tm00:
#              time window radius is (-0.75,+1.50) hours for all types except
#              SATWND subtypes 005/064, 005/065, 005/066 where it is
#              (-0.75,+1.49) hours and SATWND subtypes 005/010, 005/011,
#              005/012, 005/019 where it is (-0.75,-0.01) hours
#============================================================================

# Skip all Indian satellite winds in SATWND (not in domain)

export SKIP_005021=YES
export SKIP_005022=YES
export SKIP_005023=YES

if [ "$tmhr" = "00" ]; then
  for dump_file in vadwnd 005070 005071 005080 005090 005044 005045 005046; do
    eval DTIM_earliest_$dump_file=\${DTIM_earliest_$dump_file:-"-0.75"}
    eval DTIM_latest_$dump_file=\${DTIM_latest_$dump_file:-"+1.50"}
  done

  DTIM_earliest_005064=${DTIM_earliest_005064:-"-0.75"}
  DTIM_latest_005064=${DTIM_latest_005064:-"+1.49"}
  DTIM_earliest_005065=${DTIM_earliest_005065:-"-0.75"}
  DTIM_latest_005065=${DTIM_latest_005065:-"+1.49"}
  DTIM_earliest_005066=${DTIM_earliest_005066:-"-0.75"}
  DTIM_latest_005066=${DTIM_latest_005066:-"+1.49"}

  DTIM_earliest_005010=${DTIM_earliest_005010:-"-0.75"}
  DTIM_latest_005010=${DTIM_latest_005010:-"-0.01"}
  DTIM_earliest_005011=${DTIM_earliest_005011:-"-0.75"}
  DTIM_latest_005011=${DTIM_latest_005011:-"-0.01"}
  DTIM_earliest_005012=${DTIM_earliest_005012:-"-0.75"}
  DTIM_latest_005012=${DTIM_latest_005012:-"-0.01"}
  DTIM_earliest_005019=${DTIM_earliest_005019:-"-0.75"}
  DTIM_latest_005019=${DTIM_latest_005019:-"-0.01"}
else
  DTIM_latest_005010=${DTIM_latest_005010:-"-0.01"}
  DTIM_latest_005011=${DTIM_latest_005011:-"-0.01"}
  DTIM_latest_005012=${DTIM_latest_005012:-"-0.01"}
  DTIM_latest_005019=${DTIM_latest_005019:-"-0.01"}
fi

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 vadwnd satwnd
error2=$?
echo "$error2" > $DATA/error2

if [ "$SENDDBN" = 'YES' ]; then
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_vadwnd $job ${COMSP}vadwnd.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_satwnd $job ${COMSP}satwnd.$tmmark.bufr_d
fi

set +x
echo "********************************************************************"
echo Script thread_2
echo Finished executing on node  `hostname`
echo Ending time  : `date -u`
echo "********************************************************************"
set -x
} > $DATA/2.out 2>&1
EOF
set -x

set +x
#----------------------------------------------------------------
cat<<\EOF>thread_3; chmod +x thread_3
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_3
echo Executing on node  `hostname`
echo Starting time: `date -u`
echo "********************************************************************"
echo
set -x

export STATUS=NO
export DUMP_NUMBER=3

#========================================================================
# Dump # 3 : PROFLR, RASSDA, SFCSHP, ADPSFC, ASCATT, TIDEG
#              (3)     (1)    (11)     (5)     (1)    (1)
#            -- TOTAL NUMBER OF SUBTYPES = 22
#
# ===> Dumping of WNDSAT removed from here until new ingest feed is established
#      (had been dumped with a time window radius of (-1.50,+1.50) hours in old
#       NAM/NDAS)
#
#            for catch-up cycles tm06-01:
#              time window radius is (-0.50,+0.50) hours for all types except
#              PROFLR where it is (-2.50,+2.50) hours
#            for on-time cycles tm00:
#              time window radius is (-0.75,+1.50) hours for all types except
#              PROFLR where it is (-2.50,+2.50) hours
#=======================================================================

# Dump PROFLR with wide time window to improve PREPOBS_PROFCQC performance
#  (time window will be winnowed down in output from PREPOBS_PROFCQC, see
#   parm cards for output time window)

if [ "$tmhr" = "00" ]; then
  for dump_file in rassda sfcshp tideg adpsfc ascatt; do
    eval DTIM_earliest_$dump_file=\${DTIM_earliest_$dump_file:-"-0.75"}
    eval DTIM_latest_$dump_file=\${DTIM_latest_$dump_file:-"+1.50"}
  done
fi

DTIM_earliest_proflr=${DTIM_earliest_proflr:-"-2.50"}
DTIM_latest_proflr=${DTIM_latest_proflr:-"+2.50"}

# Skip Japanese profiler reports in PROFLR (not in domain)

export SKIP_002013=YES

# Skip mobile synoptic reports in ADPSFC (not in domain)

export SKIP_000002=YES

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 proflr rassda sfcshp tideg \
 adpsfc ascatt
error3=$?
echo "$error3" > $DATA/error3

if [ "$SENDDBN" = 'YES' ]; then
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_proflr $job ${COMSP}proflr.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_rassda $job ${COMSP}rassda.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_sfcshp $job ${COMSP}sfcshp.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_tideg $job ${COMSP}tideg.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_adpsfc $job ${COMSP}adpsfc.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_ascatt $job ${COMSP}ascatt.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_ascatw $job ${COMSP}ascatw.$tmmark.bufr_d
fi

set +x
echo "********************************************************************"
echo Script thread_3
echo Finished executing on node  `hostname`
echo Ending time  : `date -u`
echo "********************************************************************"
set -x
} > $DATA/3.out 2>&1
EOF
set -x

set +x
#----------------------------------------------------------------
cat<<\EOF>thread_4; chmod +x thread_4
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_4
echo Executing on node  `hostname`
echo Starting time: `date -u`
echo "********************************************************************"
echo
set -x

export STATUS=NO
export DUMP_NUMBER=4

#===========================================================================
# Dump # 4 : [MSONET,] GPSIPW -- TOTAL NUMBER OF SUBTYPES = [31] 1
#             [(30)]     (1) - moved msone* to separate groups!
#            for catch-up cycles tm06-01:
#              time window radius is (-0.50,+0.50) hours for MSONET
#            for on-time tm00 cycles:
#              time window radius is (-0.75,+1.50) hours for MSONET
#            for all cycles:
#              time window radius is (-0.22,-0.12) hours (~ -13 to -7 min) for
#              GPSIPW
#            {note: new Ground Based GPS-IPW/ZTD (from U.S.-ENI and foreign
#                   GNSS providers) is currently limited to obs closest to 
#                   cycle-time that result in a U.S.-ENI dump count that is
#                   not too much larger than that from the previous U.S. (only)
#                   GSD-feed, since the ENI reports are available every 5 min
#                   while the GSD reports were available only every 30 min.
#                   Also accounts for an approximate 80-min latency present in
#                   the U.S.-ENI reports.}
#===========================================================================

DTIM_earliest_gpsipw=${DTIM_earliest_gpsipw:-"-0.22"}
DTIM_latest_gpsipw=${DTIM_latest_gpsipw:-"-0.12"}

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 gpsipw
error4=$?
echo "$error4" > $DATA/error4

#+++++++++++++++++++
#  Temporary logic to change "gpsipw" dump file's group to "rstprod" and its
#   permission to "640" so that it can only be read by users in the rstprod
#   group (new "gpsipw" dump file is restricted; currently the logic to restrict
#   dump files is done in obsproc_dump ush script bufr_dump_obs.sh, where the
#   list of files to restrict is hardwired. Since obsproc_dump is not included
#   in the bundle which includes the switch to new "gpsipw" we must do it here.
#   Future change will either be to add "gpsipw" to list of dump files to
#   restrict in next obsproc_dump release, or to parameterize the list of files
#   to be restricted in this script and pass it to bufr_dump_obs.sh.  In either
#   case, once that is done this logic can be removed.  Also, must place this
#   logic here since below a dbn_alert is issued to post the "gpsipw" dump file
#   to ftpprd server (and it must be restricted before transfer!).

if [ -f ${COMSP}gpsipw.${tmmark}.bufr_d ]; then
CHGRP_RSTPROD=${CHGRP_RSTPROD:-YES}
if [ "$CHGRP_RSTPROD" = 'YES' ]; then
   chgrp rstprod ${COMSP}gpsipw.${tmmark}.bufr_d
   errch=$?
   if [ $errch -eq 0 ]; then
      chmod 640 ${COMSP}gpsipw.${tmmark}.bufr_d
      msg="NOTE: gpsipw dump contains RESTRICTED data, only users in rstprod \
group have read permission"
      set +u
      [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
      set -u
      set +x
      echo
      echo
      echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
      echo "$msg"
      echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
      echo
      echo
      set -x
   else
      cp /dev/null ${COMSP}gpsipw.${tmmark}.bufr_d
      msg="**WARNING: gpsipw dump contains RESTRICTED data, since user $USER \
is not in rstprod group a null file is copied in its place"
      set +u
      [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
      set -u
      set +x
      echo
      echo
      echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
      echo "$msg"
      echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
      echo
      echo
      set -x
   fi
fi
fi
#+++++++++++++++++++

if [ "$SENDDBN" = 'YES' ]; then
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_gpsipw $job ${COMSP}gpsipw.$tmmark.bufr_d
fi

set +x
echo "********************************************************************"
echo Script thread_4
echo Finished executing on node  `hostname`
echo Ending time  : `date -u`
echo "********************************************************************"
set -x
} > $DATA/4.out 2>&1
EOF
set -x

set +x
#----------------------------------------------------------------
cat<<\EOF>thread_5; chmod +x thread_5
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_5
echo Executing on node  `hostname`
echo Starting time: `date -u`
echo "********************************************************************"
echo
set -x

export STATUS=NO
export DUMP_NUMBER=5

#===========================================================================
# Dump # 5 : AIRCFT, AIRCAR, GOESND -- TOTAL NUMBER OF SUBTYPES = 11
#              (8)     (2)     (1)
#            for all cycles:
#              time window radius is (-3.25,+3.25) hours for all types except
#              GOESND where it is (-1.25,-0.01) hours
#===========================================================================

# Dump AIRCFT and AIRCAR with wide time window and globally to improve
#  PREPOBS_PREPACQC track-check performance
#  (time window will be winnowed down to +/- 1.50 hours in output from
#   PREPOBS_PREPACQC, and limited to north of 20S latitude)

# Dump GOESND globally to allow job to run much quicker w/o the need for
#   geographical filtering (all GOES reports are in expanded NAM domain anyway)

export LALO=0  # GLOBAL dumps here

DTIM_earliest_003002=${DTIM_earliest_003002:-"-1.25"}
DTIM_latest_003002=${DTIM_latest_003002:-"-0.01"}

# Skip GOES 1x1 radiances in GOESND (already dumped in GOESFV)

export SKIP_003003=YES

$ushscript_dump/bufr_dump_obs.sh $dumptime 3.25 1 aircft aircar goesnd
error5=$?
echo "$error5" > $DATA/error5

if [ "$SENDDBN" = 'YES' ]; then
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_aircft $job ${COMSP}aircft.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_aircar $job ${COMSP}aircar.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_goesnd $job ${COMSP}goesnd.$tmmark.bufr_d
fi

set +x
echo "********************************************************************"
echo Script thread_5
echo Finished executing on node  `hostname`
echo Ending time  : `date -u`
echo "********************************************************************"
set -x
} > $DATA/5.out 2>&1
EOF
set -x

set +x
#----------------------------------------------------------------
cat<<\EOF>thread_6; chmod +x thread_6
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_6
echo Executing on node  `hostname`
echo Starting time: `date -u`
echo "********************************************************************"
echo
set -x

export STATUS=NO
export DUMP_NUMBER=6

#===========================================================================
# Dump # 6 : NEXRAD -- TOTAL NUMBER OF SUBTYPES = 8
#              (8)
#            for catch-up cycles tm06-01:
#              time window radius is (-0.50,+0.50) hours for NEXRAD
#            for on-time tm00 cycles:
#              time window radius is (-0.75,+1.50) hours for NEXRAD
#===========================================================================

# Dump globally since all reports over CONUS (and geographical filtering is
#  computationally expensive)

export LALO=0

# NEXRAD tanks are hourly

# Initialize radial wind and reflectivity dumps to SKIP all hourly tanks
set +x
for dump_file in 006010 006011 006012 006013 006014 006015 006016 006017 \
                 006018 006019 006020 006021 006022 006023 006024 006025 \
                 006026 006027 006028 006029 006030 006031 006032 006033; do
set -x
  eval SKIP_$dump_file=YES
set +x
done

for dump_file in 006040 006041 006042 006043 006044 006045 006046 006047 \
                 006048 006049 006050 006051 006052 006053 006054 006055 \
                 006056 006057 006058 006059 006060 006061 006062 006063; do
set -x
  eval SKIP_$dump_file=YES
set +x
done
set -x

#-------------------------------------------------------------------------------
# Now unset the SKIP for those hourly radial wind and relectivity tanks that
#  are w/i requested dump center cycle time window so these will be read
# This is based on center data dump time and run analysis cycle time
#-------------------------------------------------------------------------------
# 00z run analysis cycle time:
#  tm06 (17.50-18.50 Z) - unsets SKIP on tanks 006/ 027, 028, 057, 058
#  tm05 (18.50-19.50 Z) - unsets SKIP on tanks 006/ 028, 029, 058, 059
#  tm04 (19.50-20.50 Z) - unsets SKIP on tanks 006/ 029, 030, 059, 060
#  tm03 (20.50-21.50 Z) - unsets SKIP on tanks 006/ 030, 031, 060, 061
#  tm02 (21.50-22.50 Z) - unsets SKIP on tanks 006/ 031, 032, 061, 062
#  tm01 (22.50-23.50 Z) - unsets SKIP on tanks 006/ 032, 033, 062, 063
#  tm00 (23.25-01.50 Z) - unsets SKIP on tanks 006/ 033, 010, 011, 063, 040, 041
# 06z run analysis cycle time:
#  tm06 (23.50-00.50 Z) - unsets SKIP on tanks 006/ 033, 010, 063, 040
#  tm05 (00.50-01.50 Z) - unsets SKIP on tanks 006/ 010, 011, 040, 041
#  tm04 (01.50-02.50 Z) - unsets SKIP on tanks 006/ 011, 012, 041, 042
#  tm03 (02.50-03.50 Z) - unsets SKIP on tanks 006/ 012, 013, 042, 043
#  tm02 (03.50-04.50 Z) - unsets SKIP on tanks 006/ 013, 014, 043, 044
#  tm01 (04.50-05.50 Z) - unsets SKIP on tanks 006/ 014, 015, 044, 045
#  tm00 (05.25-07.50 Z) - unsets SKIP on tanks 006/ 015, 016, 017, 045, 046, 047
# 12z run analysis cycle time:
#  tm06 (05.50-06.50 Z) - unsets SKIP on tanks 006/ 015, 016, 045, 046
#  tm05 (06.50-07.50 Z) - unsets SKIP on tanks 006/ 016, 017, 046, 047
#  tm04 (07.50-08.50 Z) - unsets SKIP on tanks 006/ 017, 018, 047, 048
#  tm03 (08.50-09.50 Z) - unsets SKIP on tanks 006/ 018, 019, 048, 049
#  tm02 (09.50-10.50 Z) - unsets SKIP on tanks 006/ 019, 020, 049, 050
#  tm01 (10.50-11.50 Z) - unsets SKIP on tanks 006/ 020, 021, 050, 051
#  tm00 (11.25-13.50 Z) - unsets SKIP on tanks 006/ 021, 022, 023, 051, 052, 053
# 18z run analysis cycle time:
#  tm06 (11.50-12.50 Z) - unsets SKIP on tanks 006/ 021, 022, 051, 052
#  tm05 (12.50-13.50 Z) - unsets SKIP on tanks 006/ 022, 023, 052, 053
#  tm04 (13.50-14.50 Z) - unsets SKIP on tanks 006/ 023, 024, 053, 054
#  tm03 (14.50-15.50 Z) - unsets SKIP on tanks 006/ 024, 025, 054, 055
#  tm02 (15.50-16.50 Z) - unsets SKIP on tanks 006/ 025, 026, 055, 056
#  tm01 (16.50-17.50 Z) - unsets SKIP on tanks 006/ 026, 027, 056, 057
#  tm00 (17.25-19.50 Z) - unsets SKIP on tanks 006/ 027, 028, 029, 057, 058, 059
#-------------------------------------------------------------------------------

for cycz in 00 06 12 18; do  # loop through run analysis cycle times
  [ $cyc -ne $cycz ] && continue
  cycx=$cyc
  [ $cyc -eq 00 ] &&  cycx=24
# --> # loop through catch-up cycles (tm06-tm01) for analysis cycle time
  for hrm in 6 5 4 3 2 1; do
    [ $cycp -ne `expr $cycx - $hrm` ] && continue
    subtyp1=`expr $cycp  +  9 `
    [ $subtyp1 -eq 9 ] && subtyp1=33
    subtyp2=`expr $cycp  + 10 `
    unset SKIP_0060${subtyp1}
    unset SKIP_0060${subtyp2}
    unset SKIP_0060`expr $subtyp1 + 30 `
    unset SKIP_0060`expr $subtyp2 + 30 `
    break 2
  done
  [ $cycp -ne $cyc ] && break
# --> come here if on-time cycle (tm00) for analysis cycle
  subtyp1=`expr $cycp  +  9 `
  [ $subtyp1 -eq 9 ] && subtyp1=33
  subtyp2=`expr $cycp  + 10 `
  subtyp3=`expr $cycp  + 11 `
  unset SKIP_0060${subtyp1}
  unset SKIP_0060${subtyp2}
  unset SKIP_0060${subtyp3}
  unset SKIP_0060`expr $subtyp1 + 30 `
  unset SKIP_0060`expr $subtyp2 + 30 `
  unset SKIP_0060`expr $subtyp3 + 30 `
  break
done

if [ "$tmhr" = "00" ]; then
  DTIM_earliest_nexrad=${DTIM_earliest_nexrad:-"-0.75"}
  DTIM_latest_nexrad=${DTIM_latest_nexrad:-"+1.50"}
fi

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 nexrad
error6=$?
echo "$error6" > $DATA/error6

if [ "$SENDDBN" = 'YES' ]; then
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_nexrad $job ${COMSP}nexrad.$tmmark.bufr_d
fi

set +x
echo "********************************************************************"
echo Script thread_6
echo Finished executing on node  `hostname`
echo Ending time  : `date -u`
echo "********************************************************************"
set -x
} > $DATA/6.out 2>&1
EOF
set -x

set +x
#----------------------------------------------------------------
cat<<\EOF>thread_7; chmod +x thread_7
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_7
echo Executing on node  `hostname`
echo Starting time: `date -u`
echo "********************************************************************"
echo
set -x

export STATUS=NO
export DUMP_NUMBER=7

#===========================================================================
# Dump # 7 : GOESFV, LGYCLD -- TOTAL NUMBER OF SUBTYPES = 2
#              (1)     (1)
#            for catch-up cycles tm06-01:
#              time window radius is (-0.50,+0.50) hours for all types
#            for on-time tm00 cycles:
#              time window radius is (-0.75,+1.50) hours for GOESFV
#              time window radius is (-0.50,+0.50) hours for LGYCLD
#            dump is global
#              (dump is much quicker w/o geographical filtering, all GOES
#               reports are in NAM domain anyway)
#===========================================================================

export LALO=0  # GLOBAL dumps here

if [ "$tmhr" = "00" ]; then
  DTIM_earliest_goesfv=${DTIM_earliest_goesfv:-"-0.75"}
  DTIM_latest_goesfv=${DTIM_latest_goesfv:-"+1.50"}
fi

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 goesfv lgycld
error7=$?
echo "$error7" > $DATA/error7

if [ "$SENDDBN" = 'YES' ]; then
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_goesfv $job ${COMSP}goesfv.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_lgycld $job ${COMSP}lgycld.$tmmark.bufr_d
fi

set +x
echo "********************************************************************"
echo Script thread_7
echo Finished executing on node  `hostname`
echo Ending time  : `date -u`
echo "********************************************************************"
set -x
} > $DATA/7.out 2>&1
EOF
set -x

set +x
#----------------------------------------------------------------
cat<<\EOF>thread_8; chmod +x thread_8
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_8
echo Executing on node  `hostname`
echo Starting time: `date -u`
echo "********************************************************************"
echo
set -x

export STATUS=NO
export DUMP_NUMBER=8
#============================================================================
# Dump # 8 : 1BHRS4, AIRSEV, OSBUV8, ESMHS, SSMISU, CRIS, LGHTNG
#              (1)     (1)     (1)    (1)     (1)    (1)    (2)
#            -- TOTAL NUMBER OF SUBTYPES = 8
#            for catch-up cycles tm06-01:
#              time window radius is (-0.50,+0.50) hours for all types except
#              1BHRS4, AIRSEV, OSBUV8, CRIS where it is (-1.00,+0.99) hours
#            for on-time cycles tm00:
#              time window radius is (-0.75,+1.50) hours for all types
#============================================================================

if [ "$tmhr" = "00" ]; then
  for dump_file in 1bhrs4 airsev osbuv8 esmhs ssmisu cris lghtng; do
    eval DTIM_earliest_$dump_file=\${DTIM_earliest_$dump_file:-"-0.75"}
    eval DTIM_latest_$dump_file=\${DTIM_latest_$dump_file:-"+1.50"}
  done
else
  for dump_file in 1bhrs4 airsev osbuv8 cris; do
    eval DTIM_earliest_$dump_file=\${DTIM_earliest_$dump_file:-"-1.00"}
    eval DTIM_latest_$dump_file=\${DTIM_latest_$dump_file:-"+0.99"}
  done
fi

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 1bhrs4 airsev osbuv8 esmhs \
 ssmisu cris lghtng
error8=$?
echo "$error8" > $DATA/error8

if [ "$SENDDBN" = 'YES' ]; then
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_1bhrs4 $job ${COMSP}1bhrs4.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_airsev $job ${COMSP}airsev.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_osbuv8 $job ${COMSP}osbuv8.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_esmhs  $job ${COMSP}esmhs.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_ssmisu $job ${COMSP}ssmisu.$tmmark.bufr_d
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_cris   $job ${COMSP}cris.$tmmark.bufr_d
### not needed? ##  $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_lghtng $job ${COMSP}lghtng.$tmmark.bufr_d
fi

set +x
echo "********************************************************************"
echo Script thread_8
echo Finished executing on node  `hostname`
echo Ending time  : `date -u`
echo "********************************************************************"
set -x
} > $DATA/8.out 2>&1
EOF
set -x

set +x
#----------------------------------------------------------------
cat<<\EOF>thread_9; chmod +x thread_9
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_9
echo Executing on node  `hostname`
echo Starting time: `date -u`
echo "********************************************************************"
echo
set -x

export STATUS=NO
export DUMP_NUMBER=9

#===========================================================================
# Dump # 9 : ADPUPA -- TOTAL NUMBER OF SUBTYPES = 6
#              (6)
#            for catch-up cycles tm06-01:
#              time window radius is (-0.50,+0.50) hours for ADPUPA
#            for on-time cycles tm00:
#              time window radius is (-0.75,+1.50) hours for ADPUPA
#            (note: ADPUPA is dumped last and after all other dumping has
#                   completed in the tm00 NAM dump - ONLY - in order to
#                   maximize availability of ADPUPA data for tm00 NAM)
#===========================================================================

if [ "$tmhr" = "00" ]; then
  DTIM_earliest_adpupa=${DTIM_earliest_adpupa:-"-0.75"}
  DTIM_latest_adpupa=${DTIM_latest_adpupa:-"+1.50"}
fi

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 adpupa
error9=$?
echo "$error9" > $DATA/error9

if [ "$SENDDBN" = 'YES' ]; then
 $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_adpupa $job ${COMSP}adpupa.$tmmark.bufr_d
fi

set +x
echo "********************************************************************"
echo Script thread_9
echo Finished executing on node  `hostname`
echo Ending time  : `date -u`
echo "********************************************************************"
set -x
} > $DATA/9.out 2>&1
EOF
set -x

set +x
#----------------------------------------------------------------
cat<<\EOF>thread_10; chmod +x thread_10
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_10
echo Executing on node  `hostname`
echo Starting time: `date -u`
echo "********************************************************************"
echo
set -x

export STATUS=NO
export DUMP_NUMBER=10

#===========================================================================
# Dump # 10 : MSONET -- TOTAL NUMBER OF SUBTYPES = 1
#              (1)
#            for catch-up cycles tm06-01:
#              time window radius is (-0.50,+0.50) hours for MSONET
#            for on-time tm00 cycles:
#              time window radius is (-0.75,+1.50) hours for MSONET
#===========================================================================

if [ "$tmhr" = "00" ]; then
  DTIM_earliest_msone0=${DTIM_earliest_msone0:-"-0.75"}
  DTIM_latest_msone0=${DTIM_latest_msone0:-"+1.50"}
fi

export SKIP_255031=YES  # Skip for port to Dell since no new data allowed.
export SKIP_255101=YES  # Also, b/c NAM is frozen; no new data.

SENDCOM=NO $ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 msone0
error10=$?
echo "$error10" > $DATA/error10

#if [ "$SENDDBN" = 'YES' ]; then
# $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_msonet $job ${COMSP}msonet.$tmmark.bufr_d
#fi

set +x
echo "********************************************************************"
echo Script thread_10
echo Finished executing on node  `hostname`
echo Ending time  : `date -u`
echo "********************************************************************"
set -x
} > $DATA/10.out 2>&1
EOF
set -x


set +x
#----------------------------------------------------------------
cat<<\EOF>thread_11; chmod +x thread_11
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_11
echo Executing on node  `hostname`
echo Starting time: `date -u`
echo "********************************************************************"
echo
set -x

export STATUS=NO
export DUMP_NUMBER=11

#===========================================================================
# Dump # 11 : MSONE1 -- TOTAL NUMBER OF SUBTYPES = 1
#              (1)
#            for catch-up cycles tm06-01:
#              time window radius is (-0.50,+0.50) hours for MSONE1
#            for on-time tm00 cycles:
#              time window radius is (-0.75,+1.50) hours for MSONE1
#===========================================================================

if [ "$tmhr" = "00" ]; then
  DTIM_earliest_msone1=${DTIM_earliest_msone1:-"-0.75"}
  DTIM_latest_msone1=${DTIM_latest_msone1:-"+1.50"}
fi

export SKIP_255031=YES  # Skip for port to Dell since no new data allowed.
export SKIP_255101=YES  # Also, b/c NAM is frozen; no new data.

SENDCOM=NO $ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 msone1
error11=$?
echo "$error11" > $DATA/error11

#if [ "$SENDDBN" = 'YES' ]; then
# $DBNROOT/bin/dbn_alert MODEL NAM_BUFR_msonet $job ${COMSP}msone1.$tmmark.bufr_d
#fi

set +x
echo "********************************************************************"
echo Script thread_11
echo Finished executing on node  `hostname`
echo Ending time  : `date -u`
echo "********************************************************************"
set -x
} > $DATA/11.out 2>&1
EOF
set -x


#----------------------------------------------------------------
# Now launch the threads

#  determine local system name and type if available
#  -------------------------------------------------
SITE=${SITE:-""}

set +u
launcher=${launcher:-"cfp"}  # if not "cfp", threads will be run serially.

if [ "$launcher" = cfp ]; then
   > $DATA/poe.cmdfile
# To better take advantage of cfp, execute the longer running commands first.
# Some reordering was done here based on recent sample runtimes.
# (Note: this may need some re-tuning after NAMv4!)
   [ $DUMP_group4 = YES ]  &&  echo ./thread_4 >> $DATA/poe.cmdfile  # moved up
   [ $DUMP_group8 = YES ]  &&  echo ./thread_8 >> $DATA/poe.cmdfile  # moved up
   [ $DUMP_group1 = YES ]  &&  echo ./thread_1 >> $DATA/poe.cmdfile
   [ $DUMP_group2 = YES ]  &&  echo ./thread_2 >> $DATA/poe.cmdfile
   [ $DUMP_group3 = YES ]  &&  echo ./thread_3 >> $DATA/poe.cmdfile
   [ $DUMP_group5 = YES ]  &&  echo ./thread_5 >> $DATA/poe.cmdfile
   [ $DUMP_group6 = YES ]  &&  echo ./thread_6 >> $DATA/poe.cmdfile
   [ $DUMP_group7 = YES ]  &&  echo ./thread_7 >> $DATA/poe.cmdfile
   [ $DUMP_group9 = YES -a $ADPUPA_wait != YES ]  &&  echo ./thread_9 >> $DATA/poe.cmdfile
   [ $DUMP_group10 = YES ]  &&  echo ./thread_10 >> $DATA/poe.cmdfile
   [ $DUMP_group11 = YES ]  &&  echo ./thread_11 >> $DATA/poe.cmdfile

   if [ -s $DATA/poe.cmdfile ]; then
      export MP_CSS_INTERRUPT=yes  # ??
      launcher_DUMP=${launcher_DUMP:-mpiexec}
      #$launcher_DUMP -np 10 --cpu-bind verbose,core cfp $DATA/poe.cmdfile
      NPROCS=${NPROCS:-11}
      $launcher_DUMP -np $NPROCS --cpu-bind verbose,core cfp $DATA/poe.cmdfile
      errpoe=$?
      if [ $errpoe -ne 0 ]; then
         $DATA/err_exit "***FATAL: EXIT STATUS $errpoe RUNNING POE COMMAND FILE"
      fi
   else
      echo
      echo "==> There are no tasks in POE Command File - POE not run"
      echo
   fi
else
   echo "Run threads serially"
   [ $DUMP_group1 = YES ]  &&  ./thread_1
   [ $DUMP_group2 = YES ]  &&  ./thread_2
   [ $DUMP_group3 = YES ]  &&  ./thread_3
   [ $DUMP_group4 = YES ]  &&  ./thread_4
   [ $DUMP_group5 = YES ]  &&  ./thread_5
   [ $DUMP_group6 = YES ]  &&  ./thread_6
   [ $DUMP_group7 = YES ]  &&  ./thread_7
   [ $DUMP_group8 = YES ]  &&  ./thread_8
   [ $DUMP_group9 = YES -a $ADPUPA_wait != YES ]  &&  ./thread_9
   [ $DUMP_group10 = YES ]  &&  ./thread_10
   [ $DUMP_group11 = YES ]  &&  ./thread_11
fi

#  if ADPUPA_wait is YES, adpupa is dumped AFTER all other dump threads have
#   run (normally done in real-time tm00 NAM runs to dump as late as possible
#   in order to maximize data availability in tm00 NAM runs)
#  --------------------------------------------------------------------------

[ $DUMP_group9 = YES -a $ADPUPA_wait  = YES ]  &&  ./thread_9

cat $DATA/1.out $DATA/2.out $DATA/3.out $DATA/4.out $DATA/5.out $DATA/6.out \
 $DATA/7.out $DATA/8.out $DATA/9.out $DATA/10.out $DATA/11.out

set +x
echo " "
echo " "
set -x

[ -s $DATA/error1 ] && err1=`cat $DATA/error1`
[ -s $DATA/error2 ] && err2=`cat $DATA/error2`
[ -s $DATA/error3 ] && err3=`cat $DATA/error3`
[ -s $DATA/error4 ] && err4=`cat $DATA/error4`
[ -s $DATA/error5 ] && err5=`cat $DATA/error5`
[ -s $DATA/error6 ] && err6=`cat $DATA/error6`
[ -s $DATA/error7 ] && err7=`cat $DATA/error7`
[ -s $DATA/error8 ] && err8=`cat $DATA/error8`
[ -s $DATA/error9 ] && err9=`cat $DATA/error9`
[ -s $DATA/error10 ] && err10=`cat $DATA/error10`
[ -s $DATA/error11 ] && err11=`cat $DATA/error11`
#===============================================================================

export STATUS=YES
export DUMP_NUMBER=12
$ushscript_dump/bufr_dump_obs.sh $dumptime 3.00 1 null


#  endif test for existence of status file
fi

#  endif loop $PROCESS_DUMP
fi

#================================================================
#================================================================


if [ "$PROCESS_DUMP" = 'YES' ]; then

  if [ "$err1" -gt '5' -o "$err2" -gt '5' -o "$err3" -gt '5' -o \
       "$err4" -gt '5' -o "$err5" -gt '5' -o "$err6" -gt '5' -o \
       "$err7" -gt '5' -o "$err8" -gt '5' -o "$err9" -gt '5' -o \
       "$err10" -gt '5' -o "$err11" -gt '5' ]; then
      for n in $err1 $err2 $err3 $err4 $err5 $err6 $err7 $err8 $err9 $err10 $err11
      do
         if [ "$n" -gt '5' ]; then
            if [ "$n" -ne '11' -a "$n" -ne '22' ]; then

## fatal error in dumping of BUFR obs. files

               set +x
echo
echo " ###################################################### "
echo " --> > 22 RETURN CODE FROM DATA DUMP, $err1, $err2, $err3, $err4, \
$err5, $err6, $err7, $err8, $err9, $err10, $err11"
echo " --> @@ F A T A L   E R R O R @@   --  ABNORMAL EXIT    "
echo " ###################################################### "
echo
               set -x
               $DATA/err_exit
               exit 9
            fi
         fi
      done

## a status code of 11 or 22 from dumping of BUFR obs. files
## is non-fatal but still worth noting

      set +x
      echo
      echo " ###################################################### "
      echo " --> > 5 RETURN CODE FROM DATA DUMP, $err1, $err2, $err3, $err4, \
$err5, $err6, $err7, $err8, $err9, $err10, $err11"
      echo " --> NOT ALL DATA DUMP FILES ARE COMPLETE - CONTINUE    "
      echo " ###################################################### "
      echo
      set -x
   fi

#  concatenate msone0 and msone1, b/c prepobs only wants one file
   cat ${DATA}/msone0.ibm  ${DATA}/msone1.ibm > ${COMSP}msonet.${tmmark}.bufr_d

#  concatenate msone0 and msone1, b/c prepobs only wants one file
   cat ${DATA}/msone0.ibm ${DATA}/msone1.ibm > ${DATA}/msonet.ibm
   cpfs ${DATA}/msonet.ibm ${COMSP}msonet.${tmmark}.bufr_d
   chmod 640 ${COMSP}msonet.${tmmark}.bufr_d
   chgrp rstprod ${COMSP}msonet.${tmmark}.bufr_d

#  endif loop $PROCESS_DUMP
fi

#
# copy bufr_dumplist to $COMOUT per NCO SPA request
# -------------------------------------------------
echo "Copy bufr_dumplist to comout"
LIST_cp=$COMOUT/${RUN}.t${cyc}z.bufr_dumplist.${tmmark}
cp ${FIXbufr_dump}/bufr_dumplist $LIST_cp
chmod 644 $LIST_cp

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
cat break $pgmout break > allout
cat allout
# rm allout
 
sleep 10

msg='ENDED NORMALLY.'
$DATA/postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
