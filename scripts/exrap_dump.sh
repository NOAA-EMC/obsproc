#!/bin/ksh
################################################################################
echo "------------------------------------------------------------------------ "
echo "exrap_dump.sh     - Rapid Refresh Full and Partial Cycle Dump processing "
echo "------------------------------------------------------------------------ "
echo "History: Oct 13 2011 - Original script.                                  "
echo "         Sep 10 2014 - Use parallel scripting to process dump groups.    "
echo "         Feb  3 2015 - Dump new satwnd types NC005019, NC005080,         "
echo "                       NC005090. Reorder cfp commands and add new thread "
echo "                       to speed up run.                                  "
echo "         Apr  8 2015   Add test for number of dump groups expected in    "
echo "                       upstream job script ($num_DUMP_group). If doesn't "
echo "                       agree with what is expected here, abort (done in  "
echo "                       RUN rap_eh only).                                 "
echo "         Aug 18 2016 - GPSIPW dump window reset for new data stream.     "
echo "         Aug 24 2017 - Update to run on Cray-XC40 or IBM iDataPlex.      "
echo "                       Dump new radiance types amsr2, atmsdb, crisdb,    "
echo "                       esatms, escris, esiasi, iasidb, saphir, sevasr.   "
echo "                       Expand lghtng dump window. Copy grib2 imssnow     "
echo "                       file. Skip 002007 in dump group #3; temporary     "
echo "                       until next bufr_dumplist upgrade.                 "
echo "         Jun 04 2019, Oct 24 2019; Apr 06, 2020; Sep 14, 2020:           "
echo "                     - Update to add GOES-16/17 DMW data to satwnd dump. "
echo "                       Update to dump crisf4, gsrasr, and gsrcsr data.   "
echo "                     - Remove rap_eh processing; not required for HRRRv4."
echo "                     - Remove the skip of NPN wind profiler data.  The   "
echo "                       NPN tank has been removed from bufr_dumplist      "
echo "                       proflr definition.                                "
echo "                     - Remove radwnd processing; discontinued.           " 
echo "                     - Remove GOES-15 tanks in satwnd dump group.        "
echo "                     - Remove goesnd dump group (GOES-15 clouds &        "
echo "                       radiances).                                       "
echo "                     - Add DBN alerts for all satellite data.            "
echo "                     - Remove crisdb and escris and replace with crsfdb. "
echo "                       On Apr 22, 2020, NSR CrIS data from NPP was       "
echo "                       replaced by FSR. The cris dump group is obsolete  "
echo "                       with the change to FSR.                           "
echo "                     - Update to Dump group #2 to disable processing of  "
echo "                       legacy EUMETSAT AMV subsets 005064, 005065,       "
echo "                       and 005066.  Add DTIM settings for new BUFR format"
echo "                       EUMETSAT AMV subsets 005067, 005068, and 005069 in"
echo "                       the satwnd dump. On Oct 6, 2020, EUMETSAT AMV     "
echo "                       format changed to use new WMO BUFR sequence       " 
echo "                       3-10-077.                                         "
echo "                     - moved lghtng to it's own unique dump group, new   "
echo "                       Dump group #9.  This was done so that the lghtng  "
echo "                       bufr_d file can be available as soon as possible  "
echo "                       for HRRR.                                         "
echo "         Jun 22 2021 - Incremented subsets for sfcshp dump groups to     "
echo "                       match bufr_dumplist.  Removed tideg from sfcshp   "
echo "                       dump group to make individual dump file.          "
echo "                     - Copy bufr_dumplist to COMOUT.                     "
echo "         Dec 09 2021 - Updated to run on WCOSS2                          "
echo "         Aug 10 2022 - Added subpfl,saldn to dump #1,snocvr to dump #3.  "
echo "                       added gmi1cr dump group #7                        "
echo "                       b005/xx081 added to satwnd                        "
echo "         Sep 30 2022 - Enable dumping of UPRAIR data in group #2.        "
echo "         Oct 11 2023 - Split msonet to msonet and msone1, msone1=255.030 "
echo "                       concatenate msonet and msone1 right after dump    "
echo "                     - Pull adpupa and uprair into own Dump group        "
echo "         Mar 14 2024 - Split gsrasr and gsrcsr to own dump hroups        "
echo "         Feb 18 2025 - Split gpsipw to own dump group                    "
################################################################################

set -xau

# set some variables if they have not already been set

set +u

# JOB_NUMBER = 1 indicates the prepbufr dump job.
# JOB_NUMBER = 2 indicates the non-prepbufr dump job.
# JOB_NUMBER not present indicates dump BOTH prepbufr and non-prepbufr data.
# ------------------------------------------------------------------------
# Dump group #1 (non-pb) = 1bamua 1bmhs esamua esmhs atms mtiasi sevcsr
#                          gpsro esiasi iasidb esatms atmsdb sevasr amsr2
# Dump group #2 (pb) = vadwnd satwnd
# Dump group #3 (pb) = proflr rassda sfcshp adpsfc ascatt tideg snocvr
#                          subpfl saldrn
# Dump group #4 (pb) = msonet->msone0 (gpsipw to #13) 
# Dump group #5 (pb) = aircft aircar
# Dump group #6 (non-pb) = nexrad
# Dump group #7 (non-pb) = airsev 1bhrs4 eshrs3 lgycld ssmisu osbuv8 crsfdb
#                          saphir gmi1cr
# Dump group #8 (non-pb) = gsrasr (gsrcsr to #12)
# Dump group #9 (non-pb) = lghtng + adpupa
# Dump group #10(pb) = msone1 # ONLY tank b255/xx030, the largest
# Dump group #11(pb) = [adpupa to #9] uprair
# Dump group #12 (non-pb)= gsrcsr
# Dump group #13 (pb) = gpsipw
# Dump group #14 STATUS FILE
# ------------------------------------------------------------------------

#VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
# The settings below are based on a future change when the DUMP job will dump
#  only types that go into PREPBUFR and the DUMP2 job will dump only types that
#  do not go into PREPBUFR.  This will speed up the DUMP + PREP processing.
# Although the logic is in place to now do this (see below), for now we will
#  continue to run only a DUMP job which will dump ALL types (no DUMP2 job) -
#  since JOB_NUMBER is not imported to this script, the logic below will dump
#  all types ...
# -----------------------------------------------------------------------------
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
      DUMP_group9=${DUMP_group9:-"YES"}
      DUMP_group10=${DUMP_group10:-"NO"}
      DUMP_group11=${DUMP_group11:-"NO"}
      DUMP_group12=${DUMP_group12:-"YES"}
      DUMP_group13=${DUMP_group13:-"YES"}
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
      DUMP_group9=${DUMP_group9:-"NO"}
      DUMP_group10=${DUMP_group10:-"YES"}
      DUMP_group11=${DUMP_group11:-"YES"}
      DUMP_group12=${DUMP_group12:-"NO"}
      DUMP_group13=${DUMP_group13:-"NO"}
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
   DUMP_group12=${DUMP_group12:-"YES"}
   DUMP_group13=${DUMP_group13:-"YES"}
fi

# Oct 2019; disable -- not needed for HRRRv4
#if [ $RUN = rap_eh ]; then
#   set +u
#   if [ $num_DUMP_group -ne 7 ]; then
#      set +x
#      echo
#      echo "The nmmber of dump groups expected in the job script does not "
#      echo "agree with the number expected here!!!. Exiting..."
#      echo
#      set -x
#      $DATA/err_exit
#      exit 9
#   fi
#   set -u
#fi

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

export dumptime=`cut -c7-16 ncepdate`

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

###################################
#  copy imssnow from $TANK_GRIBFLDS
###################################

   imssnow=$TANK_GRIBFLDS/$PDY/wgrbbul
   imssold=$TANK_GRIBFLDS/$PDYm1/wgrbbul
   typeset -A flarr
   flarr[imssnow]=imssnow96.grb
   flarr["imssnow.grib2"]=imssnow96.grb.grib2
   for i in "${!flarr[@]}"
   do
     if [ -s $imssnow/${flarr[$i]} ]; then
       cp $imssnow/${flarr[$i]} ${COMSP}${i}
       msg="todays IMS snow grib file (${flarr[$i]}) located and copied \
to ${COMSP}${i}"
       $DATA/postmsg "$jlogfile" "$msg"
     elif [ -s $imssold/${flarr[$i]} ]; then
       cp $imssold/${flarr[$i]} ${COMSP}${i}
       msg="**todays IMS snow grib file (${flarr[$i]}) not located - copy \
1-day old file"
       $DATA/postmsg "$jlogfile" "$msg"
     else
       set +x
       echo " "
       echo " ###############################################"
       echo " cannot locate IMS snow grib file: ${flarr[$i]} "
       echo " ###############################################"
       echo " "
       set -x
#      echo "no imssnow data availble for RAP" | mail geoffrey.manikin@noaa.gov
       msg="**CANNOT LOCATE IMS SNOW GRIB FILE: ${flarr[$i]}"
       $DATA/postmsg "$jlogfile" "$msg"
     fi
   done
fi  #  endif loop $PROCESS_GRIBFLDS

# NAP is introduced so that uprair can run early on his own
NAP=${NAP:-120} #b/c cron is moved to run 2min (120s) early

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
echo "=======> Dump group 12 (thread_12) not executed." > $DATA/12.out
echo "=======> Dump group 13 (thread_13) not executed." > $DATA/13.out
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
err12=0
err13=0

#restrict processing of unexpected big tanks
#this block appear in all /scripts/ex*_dump.sh proessing msone0 and msone1 
TANK_MAX_255003=${TANK_MAX_255003:-3221225472} #3Gb
TANK_MAX_255004=${TANK_MAX_255004:-2684354560} #2.5Gb
TANK_MAX_255030=${TANK_MAX_255030:-4187593114} #3.9Gb
if [ -s ${TANK}/${PDY}/b255/xx003 ] && [ "$(stat -c '%s' ${TANK}/${PDY}/b255/xx003)" -gt "$TANK_MAX_255003" ]; then
 export SKIP_255003=YES
 msg="WARNING: TANK b255/xx003 exceeds TANK_MAX_255003 => not dumped"
 echo $msg | mail.py -s "$msg" 
#echo $msg | mail.py -s "$msg" -c iliana.genkova@noaa.gov
fi
if [ -s ${TANK}/${PDY}/b255/xx004 ] && [ "$(stat -c '%s' ${TANK}/${PDY}/b255/xx004)" -gt "$TANK_MAX_255004" ]; then
 export SKIP_255004=YES
 msg="WARNING: TANK b255/xx004 exceeds TANK_MAX_255004 => not dumped"
 echo $msg | mail.py -s "$msg" 
# echo $msg | mail.py -s "$msg" -c iliana.genkova@noaa.gov
 fi
if [ -s ${TANK}/${PDY}/b255/xx030 ] && [ "$(stat -c '%s' ${TANK}/${PDY}/b255/xx030)" -gt "$TANK_MAX_255030" ]; then
 export SKIP_255030=YES
 msg="WARNING: TANK b255/xx030 exceeds TANK_MAX_255030 => not dumped"
 echo $msg | mail.py -s "$msg" 
#echo $msg | mail.py -s "$msg" -c iliana.genkova@noaa.gov
fi
#end of block


if [ "$PROCESS_DUMP" = 'YES' ]; then

##################################
##################################
#  The data "dump" script for TM00
##################################
##################################

msg="START THE $tmmark_uc $RUN_uc DATA $dump_ind CENTERED ON $dumptime"
$DATA/postmsg "$jlogfile" "$msg"

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

sleep ${NAP} # to reverse 2min early start of jrap_dump in cron
export STATUS=NO
export DUMP_NUMBER=1

#===============================================================================
# Dump # 1 : 1BAMUA, 1BMHS,  ESAMUA, ESMHS, ATMS, MTIASI, SEVCSR, GPSRO,
#              (1)    (1)     (1)     (1)   (1)    (1)    (1)     (1)
#            ESIASI, IASIDB, ESATMS, ATMSDB, SEVASR, AMSR2, SUBPFL, SALDRN
#              (1)    (1)     (1)     (1)     (1)     (1)    (1)    (1)
#             TOTAL NUMBER OF SUBTYPES = 16
#===============================================================================
 
if [ "$RUN" = 'rap_p' ]; then

#  ===> For RUN = rap_p -- partial cycle runs
#       -------------------------------------

   def_time_window_1=1.0 # default time window for dump 1 is -1.0 to +1.0 hours

# Time window is -1.00 to +0.99 hours for 1BAMUA, 1BMHS, AMSR2
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_latest_1bamua=${DTIM_latest_1bamua:-"+0.99"}      # earliest is default
    DTIM_latest_1bmhs=${DTIM_latest_1bmhs:-"+0.99"}       # earliest is default
    DTIM_latest_amsr2=${DTIM_latest_amsr2:-"+0.99"}       # earliest is default

# Time window is -1.00 to +0.99 hours for ATMS, ESATMS, ATMSDB,
#  MTIASI, ESIASI, IASIDB
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_earliest_atms=${DTIM_earliest_atms:-"-1.00"}
   DTIM_latest_atms=${DTIM_latest_atms:-"+0.99"}
   DTIM_earliest_esatms=${DTIM_earliest_esatms:-"-1.00"}
   DTIM_latest_esatms=${DTIM_latest_esatms:-"+0.99"}
   DTIM_earliest_atmsdb=${DTIM_earliest_atmsdb:-"-1.00"}
   DTIM_latest_atmsdb=${DTIM_latest_atmsdb:-"+0.99"}
   DTIM_earliest_mtiasi=${DTIM_earliest_mtiasi:-"-1.00"}
   DTIM_latest_mtiasi=${DTIM_latest_mtiasi:-"+0.99"}
   DTIM_earliest_esiasi=${DTIM_earliest_esiasi:-"-1.00"}
   DTIM_latest_esiasi=${DTIM_latest_esiasi:-"+0.99"}
   DTIM_earliest_iasidb=${DTIM_earliest_iasidb:-"-1.00"}
   DTIM_latest_iasidb=${DTIM_latest_iasidb:-"+0.99"}

# Time window is -1.00 to +0.99 hours for SEVCSR, SEVASR, GPSRO
  DTIM_earliest_sevcsr=${DTIM_earliest_sevcsr:-"-1.00"}
  DTIM_latest_sevcsr=${DTIM_latest_sevcsr:-"+0.99"}
  DTIM_earliest_sevasr=${DTIM_earliest_sevasr:-"-1.00"}
  DTIM_latest_sevasr=${DTIM_latest_sevasr:-"+0.99"}
  DTIM_earliest_gpsro=${DTIM_earliest_gpsro:-"-1.00"}
  DTIM_latest_gpsro=${DTIM_latest_gpsro:-"+0.99"}
 
# Time window is -0.50 to +0.49 hours for ESAMUA, ESMHS
   DTIM_earliest_esamua=${DTIM_earliest_esamua:-"-0.50"}
   DTIM_latest_esamua=${DTIM_latest_esamua:-"+0.49"}
    DTIM_earliest_esmhs=${DTIM_earliest_esmhs:-"-0.50"}
    DTIM_latest_esmhs=${DTIM_latest_esmhs:-"+0.49"}

# Uncertain on time window
   DTIM_earliest_subpfl=${DTIM_earliest_subpfl:-"-1.00"}
   DTIM_latest_subpfl=${DTIM_latest_subpfl:-"+0.99"}
   DTIM_earliest_saldrn=${DTIM_earliest_saldrn:-"-1.00"}
   DTIM_latest_saldrn=${DTIM_latest_saldrn:-"+0.99"}


else

#  ===> For RUN = rap, rap_e -- full cycle runs (including early at 00/12z)
#       -------------------------------------------------------------------

   def_time_window_1=3.0 # default time window for dump 1 is -3.0 to +3.0 hours

# Time window is -3.00 to +2.99 hours for 1BAMUA, 1BMHS
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_latest_1bamua=${DTIM_latest_1bamua:-"+2.99"}      # earliest is default
    DTIM_latest_1bmhs=${DTIM_latest_1bmhs:-"+2.99"}       # earliest is default

# Time window is -3.00 to -2.01 hours for AMSR2
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_latest_amsr2=${DTIM_latest_amsr2:-"-2.01"}        # earliest is default

# Time window is -2.00 to +1.99 hours for ATMS, ESATMS, ATMSDB,
#  MTIASI, ESIASI, IASIDB
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_earliest_atms=${DTIM_earliest_atms:-"-2.00"}
   DTIM_latest_atms=${DTIM_latest_atms:-"+1.99"}
   DTIM_earliest_esatms=${DTIM_earliest_esatms:-"-2.00"}
   DTIM_latest_esatms=${DTIM_latest_esatms:-"+1.99"}
   DTIM_earliest_atmsdb=${DTIM_earliest_atmsdb:-"-2.00"}
   DTIM_latest_atmsdb=${DTIM_latest_atmsdb:-"+1.99"}
   DTIM_earliest_mtiasi=${DTIM_earliest_mtiasi:-"-2.00"}
   DTIM_latest_mtiasi=${DTIM_latest_mtiasi:-"+1.99"}
   DTIM_earliest_esiasi=${DTIM_earliest_esiasi:-"-2.00"}
   DTIM_latest_esiasi=${DTIM_latest_esiasi:-"+1.99"}
   DTIM_earliest_iasidb=${DTIM_earliest_iasidb:-"-2.00"}
   DTIM_latest_iasidb=${DTIM_latest_iasidb:-"+1.99"}

# Time window is -2.00 to +1.99 hours for SEVCSR, SEVASR, GPSRO, GSRASR, GSRCSR
   DTIM_earliest_sevcsr=${DTIM_earliest_sevcsr:-"-2.00"}
   DTIM_latest_sevcsr=${DTIM_latest_sevcsr:-"+1.99"}
   DTIM_earliest_sevasr=${DTIM_earliest_sevasr:-"-2.00"}
   DTIM_latest_sevasr=${DTIM_latest_sevasr:-"+1.99"}
   DTIM_earliest_gpsro=${DTIM_earliest_gpsro:-"-2.00"}
   DTIM_latest_gpsro=${DTIM_latest_gpsro:-"+1.99"}

# Time window is -1.00 to +1.00 hours for ESAMUA, ESMHS
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_earliest_esamua=${DTIM_earliest_esamua:-"-1.00"}
   DTIM_latest_esamua=${DTIM_latest_esamua:-"+1.00"}
    DTIM_earliest_esmhs=${DTIM_earliest_esmhs:-"-1.00"}
    DTIM_latest_esmhs=${DTIM_latest_esmhs:-"+1.00"}

# Uncertain on time window
   DTIM_earliest_subpfl=${DTIM_earliest_subpfl:-"-2.00"}
   DTIM_latest_subpfl=${DTIM_latest_subpfl:-"+1.99"}
   DTIM_earliest_saldrn=${DTIM_earliest_saldrn:-"-2.00"}
   DTIM_latest_saldrn=${DTIM_latest_saldrn:-"+1.99"}

fi

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_1} 1 1bamua \
 1bmhs esamua esmhs atms mtiasi sevcsr gpsro esiasi iasidb esatms \
 atmsdb sevasr amsr2 subpfl saldrn
error1=$?
echo "$error1" > $DATA/error1

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

sleep ${NAP} # to reverse 2min early start of jrap_dump in cron
export STATUS=NO
export DUMP_NUMBER=2

#==========================================================================
# Dump # 2 : VADWND, SATWND
#              (2)    (19)    
#            -- TOTAL NUMBER OF SUBTYPES = 21
#==========================================================================

# Skip all Indian satellite winds in SATWND (not in domain)
export SKIP_005021=YES
export SKIP_005022=YES
export SKIP_005023=YES

# Skip old bufr NPP VIIRS AMVs
export SKIP_005090=YES

# Skip old bufr EUMETSAT AMVs
#For testing, skip in ecflow or obsproc_rap.ver file
#export SKIP_005064=YES
#export SKIP_005065=YES
#export SKIP_005066=YES

# Add GOES-16/17/18 DMW data to SATWND
export ADD_satwnd="005030 005031 005032 005034 005039"

# Time window -1.50 to +1.49 hours for EUMETSAT SATWND for full and partial
#  cycle runs

# Time window -1.00 to -0.01 hours for GOES SATWND for full and partial cycle
#  runs

DTIM_earliest_005030=${DTIM_earliest_005030:-"-1.00"}
DTIM_latest_005030=${DTIM_latest_005030:-"-0.01"}
 DTIM_earliest_005031=${DTIM_earliest_005031:-"-1.00"}
 DTIM_latest_005031=${DTIM_latest_005031:-"-0.01"}
DTIM_earliest_005032=${DTIM_earliest_005032:-"-1.00"}
DTIM_latest_005032=${DTIM_latest_005032:-"-0.01"}
 DTIM_earliest_005034=${DTIM_earliest_005034:-"-1.00"}
 DTIM_latest_005034=${DTIM_latest_005034:-"-0.01"}
DTIM_earliest_005039=${DTIM_earliest_005039:-"-1.00"}
DTIM_latest_005039=${DTIM_latest_005039:-"-0.01"}
 DTIM_earliest_005064=${DTIM_earliest_005064:-"-1.50"}
 DTIM_latest_005064=${DTIM_latest_005064:-"+1.49"}
DTIM_earliest_005065=${DTIM_earliest_005065:-"-1.50"}
DTIM_latest_005065=${DTIM_latest_005065:-"+1.49"}
 DTIM_earliest_005066=${DTIM_earliest_005066:-"-1.50"}
 DTIM_latest_005066=${DTIM_latest_005066:-"+1.49"}
DTIM_earliest_005067=${DTIM_earliest_005067:-"-1.50"}
DTIM_latest_005067=${DTIM_latest_005067:-"+1.49"}
 DTIM_earliest_005068=${DTIM_earliest_005068:-"-1.50"}
 DTIM_latest_005068=${DTIM_latest_005068:-"+1.49"}
DTIM_earliest_005069=${DTIM_earliest_005069:-"-1.50"}
DTIM_latest_005069=${DTIM_latest_005069:-"+1.49"}
DTIM_earliest_005081=${DTIM_earliest_005081:-"-1.50"}
DTIM_latest_005081=${DTIM_latest_005081:-"+1.49"}

if [ "$RUN" = 'rap_p' ]; then

#  ===> For RUN = rap_p -- partial cycle runs
#       -------------------------------------

   def_time_window_2=1.5 # default time window for dump 2 is -1.5 to +1.5 hours

# Time window is -0.50 to +0.50 hours for VADWND
   DTIM_earliest_vadwnd=${DTIM_earliest_vadwnd:-"-0.50"}
   DTIM_latest_vadwnd=${DTIM_latest_vadwnd:-"+0.50"}

# Time window is -1.50 to +1.50 hours for NON-EUMETSAT & NON-GOES SATWND
#  (default)


else

#  ===> For RUN = rap, rap_e -- full cycle runs (including early at 00/12z)
#       -------------------------------------------------------------------

   if [ $cyc -eq 00 -o $cyc -eq 12 ]; then
      def_time_window_2=1.5  # default time window for dump 2 is -1.5 to +1.5
                             # hours for 00 or 12z
   else
      def_time_window_2=2.5  # default time window for dump 2 is -2.5 to +2.5
                             # hours for all other cycles
   fi

# Time window is -1.00 to +1.00 hours for VADWND
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_earliest_vadwnd=${DTIM_earliest_vadwnd:-"-1.00"}
   DTIM_latest_vadwnd=${DTIM_latest_vadwnd:-"+1.00"}

# Time window is -1.50 to +1.50 hours for NON-EUMETSAT & NON-GOES SATWND at 00
#  or 12z (default)

# Time window is -2.50 to +2.50 hours for NON-EUMETSAT & NON-GOES SATWND at
#  all other cycles (default)

fi

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_2} 1 vadwnd \
 satwnd 
error2=$?
echo "$error2" > $DATA/error2

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

sleep ${NAP} # to reverse 2min early start of jrap_dump in cron
export STATUS=NO
export DUMP_NUMBER=3

#========================================================================
# Dump # 3 : PROFLR, RASSDA, SFCSHP, ADPSFC, ASCATT, TIDEG, SNOCVR
#              (3)     (1)    (11)     (3)     (1)    (1)    (1)
#            -- TOTAL NUMBER OF SUBTYPES = 21
#
# ===> Dumping of WNDSAT removed from here until new ingest feed is established
#      (had been dumped with a time window radius of -0.50 to +0.50 hours in
#       rap_p and -2.00 to +2.00 hours in rap and rap_e)
#
#========================================================================

# Skip Japanese profiler reports in PROFLR (not in domain)

export SKIP_002013=YES

# Skip mobile synoptic reports in ADPSFC (not in domain)

export SKIP_000002=YES


def_time_window_3=0.5 # default time window for dump 3 is -0.5 to +0.5 hours

# Time window -1.50 to +1.50 hours for PROFLR for full and partial cycle runs
#  (note: time window increased over +/- 0.5 hr standard to improve
#         PREPOBS_PROFCQC performance, it will be winnowed back down to
#         -0.50 to +0.50 hours in output from PREPOBS_PROFCQC)
DTIM_earliest_proflr=${DTIM_earliest_proflr:-"-1.50"}
DTIM_latest_proflr=${DTIM_latest_proflr:-"+1.50"}

# Time window -0.50 to +0.50 hours for RASSDA, SFCSHP, ADPSFC, TIDEG for full
#  and partial cycle runs (default)

if [ "$RUN" = 'rap_p' ]; then

#  ===> For RUN = rap_p -- partial cycle runs
#       -------------------------------------

# Time window is -0.50 to +0.50 hours for ASCATT (default)

   DTIM_earliest_snocvr=${DTIM_earliest_snocvr:-"-0.50"}
   DTIM_latest_snocvr=${DTIM_latest_snocvr:-"+0.50"}
   # dummy=idum  # dummy entry since nothing in this if-block

else

#  ===> For RUN = rap, rap_e -- full cycle runs (including early at 00/12z)
#       -------------------------------------------------------------------

# Time window is -2.00 to +2.00 hours for ASCATT
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_earliest_ascatt=${DTIM_earliest_ascatt:-"-2.00"}
   DTIM_latest_ascatt=${DTIM_latest_ascatt:-"+2.00"}
   DTIM_earliest_snocvr=${DTIM_earliest_snocvr:-"-2.00"}
   DTIM_latest_snocvr=${DTIM_latest_snocvr:-"+2.00"}

fi

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_3} 1 proflr \
 rassda sfcshp adpsfc ascatt tideg snocvr
error3=$?
echo "$error3" > $DATA/error3

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

sleep ${NAP} # to reverse 2min early start of jrap_dump in cron
export STATUS=NO
export DUMP_NUMBER=4

#============================================================================
# Dump # 4 : MSONET, GPSIPW(moved to Dump#13) -- TOTAL NUMBER OF SUBTYPES = 31
#             (30)     (1)
#============================================================================

def_time_window_4=0.5 # default time window for dump 4 is -0.5 to +0.5 hours

# Time window -0.50 to +0.50 hours for MSONET for full and partial cycle runs
#  (default)


SENDCOM=NO $ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_4} 1 msone0
error4=$?
echo "$error4" > $DATA/error4

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

sleep ${NAP} # to reverse 2min early start of jrap_dump in cron
export STATUS=NO
export DUMP_NUMBER=5

#===========================================================================
# Dump # 5 : AIRCFT, AIRCAR, GOESND -- TOTAL NUMBER OF SUBTYPES = 12
#              (8)     (2)     (2)
#===========================================================================

export LALO=0  # GLOBAL dumps here (AIRCFT and AIRCAR dumped globally to
               # improve PREPOBS_PREPACQC track-check performance; GOESND
               # dumped globally to allow job to run much quicker w/o the need
               # for geographical filtering (all GOES reports are in expanded
               # NAM domain anyway)

def_time_window_5=3.25 # default time window for dump 5 is -3.25 to +3.25 hours


# Time window -3.25 to +3.25 hours for AIRCFT and AIRCAR for full and partial
#  cycle runs (default)
#  {note: time window increased to improve PREPOBS_PREPACQC track-check
#         performance; time window will be winnowed down to +/- 1.00 hours in
#         output from PREPOBS_PREPACQC (for full and partial cycle runs), and
#         geographical domain will be limited to north of 20S latitude)


# Time window -1.25 to -0.01 hours for GOESND cloud (only) for full and partial
#  cycle runs
#  (note: going back -1.25 hours is necessary to gather a complete set of
#         GOES-E information, while the cutting off at -0.01 hours avoids a
#         duplication of GOES-W data from the next input dataset time)
DTIM_earliest_003002=${DTIM_earliest_003002:-"-1.25"}
DTIM_latest_003002=${DTIM_latest_003002:-"-0.01"}

if [ "$RUN" = 'rap_p' ]; then

#  ===> For RUN = rap_p -- partial cycle runs
#       -------------------------------------

# Time window is -0.50 to +0.50 hours for GOESND soundings/radiances (only)
   DTIM_earliest_003003=${DTIM_earliest_003003:-"-0.50"}
   DTIM_latest_003003=${DTIM_latest_003003:-"+0.50"}

else

#  ===> For RUN = rap, rap_e -- full cycle runs (including early at 00/12z)
#       -------------------------------------------------------------------

# Time window is -1.00 to +1.00 hours for GOESND soundings/radiances (only)
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_earliest_003003=${DTIM_earliest_003003:-"-1.00"}
   DTIM_latest_003003=${DTIM_latest_003003:-"+1.00"}

fi

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_5} 1 aircft \
 aircar
error5=$?
echo "$error5" > $DATA/error5

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

sleep ${NAP} # to reverse 2min early start of jrap_dump in cron
export STATUS=NO
export DUMP_NUMBER=6

#===========================================================================
# Dump # 6 : NEXRAD -- TOTAL NUMBER OF SUBTYPES = 4
#              (4)
#===========================================================================

export LALO=0  # GLOBAL dumps here (NEXRAD dumped globally to allow job to run
               # much quicker w/o the need for geographical filtering (all
               # radar reports are over CONUS anyway)

def_time_window_6=0.5 # default time window for dump 6 is -0.5 to +0.5 hours

# Time window -0.50 to +0.49 hours for NEXRAD for full and partial cycle runs
DTIM_latest_nexrad=${DTIM_latest_nexrad:-"+0.49"}         # earliest is default

# NEXRAD tanks are hourly
# Process only those hourly tanks w/i requested dump center cycle time window

SKIP_006010=YES # radial wind  00Z
SKIP_006011=YES # radial wind  01Z
SKIP_006012=YES # radial wind  02Z
SKIP_006013=YES # radial wind  03Z
SKIP_006014=YES # radial wind  04Z
SKIP_006015=YES # radial wind  05Z
SKIP_006016=YES # radial wind  06Z
SKIP_006017=YES # radial wind  07Z
SKIP_006018=YES # radial wind  08Z
SKIP_006019=YES # radial wind  09Z
SKIP_006020=YES # radial wind  10Z
SKIP_006021=YES # radial wind  11Z
SKIP_006022=YES # radial wind  12Z
SKIP_006023=YES # radial wind  13Z
SKIP_006024=YES # radial wind  14Z
SKIP_006025=YES # radial wind  15Z
SKIP_006026=YES # radial wind  16Z
SKIP_006027=YES # radial wind  17Z
SKIP_006028=YES # radial wind  18Z
SKIP_006029=YES # radial wind  19Z
SKIP_006030=YES # radial wind  20Z
SKIP_006031=YES # radial wind  21Z
SKIP_006032=YES # radial wind  22Z
SKIP_006033=YES # radial wind  23Z

SKIP_006040=YES # reflectivity 00Z
SKIP_006041=YES # reflectivity 01Z
SKIP_006042=YES # reflectivity 02Z
SKIP_006043=YES # reflectivity 03Z
SKIP_006044=YES # reflectivity 04Z
SKIP_006045=YES # reflectivity 05Z
SKIP_006046=YES # reflectivity 06Z
SKIP_006047=YES # reflectivity 07Z
SKIP_006048=YES # reflectivity 08Z
SKIP_006049=YES # reflectivity 09Z
SKIP_006050=YES # reflectivity 10Z
SKIP_006051=YES # reflectivity 11Z
SKIP_006052=YES # reflectivity 12Z
SKIP_006053=YES # reflectivity 13Z
SKIP_006054=YES # reflectivity 14Z
SKIP_006055=YES # reflectivity 15Z
SKIP_006056=YES # reflectivity 16Z
SKIP_006057=YES # reflectivity 17Z
SKIP_006058=YES # reflectivity 18Z
SKIP_006059=YES # reflectivity 19Z
SKIP_006060=YES # reflectivity 20Z
SKIP_006061=YES # reflectivity 21Z
SKIP_006062=YES # reflectivity 22Z
SKIP_006063=YES # reflectivity 23Z

if [ $cyc -eq 00 ]; then   # (23.50 - 00.49 Z)
   unset SKIP_006033 # radial wind  23Z
   unset SKIP_006010 # radial wind  00Z
###unset SKIP_006063 # reflectivity 23Z
###unset SKIP_006040 # reflectivity 00Z
elif [ $cyc -eq 01 ]; then   # (00.50 - 01.49 Z)
   unset SKIP_006010 # radial wind  00Z
   unset SKIP_006011 # radial wind  01Z
###unset SKIP_006040 # reflectivity 00Z
###unset SKIP_006041 # reflectivity 01Z
elif [ $cyc -eq 02 ]; then   # (01.50 - 02.49 Z)
   unset SKIP_006011 # radial wind  01Z
   unset SKIP_006012 # radial wind  02Z
###unset SKIP_006041 # reflectivity 01Z
###unset SKIP_006042 # reflectivity 02Z
elif [ $cyc -eq 03 ]; then   # (02.50 - 03.49 Z)
   unset SKIP_006012 # radial wind  02Z
   unset SKIP_006013 # radial wind  03Z
###unset SKIP_006042 # reflectivity 02Z
###unset SKIP_006043 # reflectivity 03Z
elif [ $cyc -eq 04 ]; then   # (03.50 - 04.49 Z)
   unset SKIP_006013 # radial wind  03Z
   unset SKIP_006014 # radial wind  04Z
###unset SKIP_006043 # reflectivity 03Z
###unset SKIP_006044 # reflectivity 04Z
elif [ $cyc -eq 05 ]; then   # (04.50 - 05.49 Z)
   unset SKIP_006014 # radial wind  04Z
   unset SKIP_006015 # radial wind  05Z
###unset SKIP_006044 # reflectivity 04Z
###unset SKIP_006045 # reflectivity 05Z
elif [ $cyc -eq 06 ]; then   # (05.50 - 06.49 Z)
   unset SKIP_006015 # radial wind  05Z
   unset SKIP_006016 # radial wind  06Z
###unset SKIP_006045 # reflectivity 05Z
###unset SKIP_006046 # reflectivity 06Z
elif [ $cyc -eq 07 ]; then   # (06.50 - 07.49 Z)
   unset SKIP_006016 # radial wind  06Z
   unset SKIP_006017 # radial wind  07Z
###unset SKIP_006046 # reflectivity 06Z
###unset SKIP_006047 # reflectivity 07Z
elif [ $cyc -eq 08 ]; then   # (07.50 - 08.49 Z)
   unset SKIP_006017 # radial wind  07Z
   unset SKIP_006018 # radial wind  08Z
###unset SKIP_006047 # reflectivity 07Z
###unset SKIP_006048 # reflectivity 08Z
elif [ $cyc -eq 09 ]; then   # (08.50 - 09.49 Z)
   unset SKIP_006018 # radial wind  08Z
   unset SKIP_006019 # radial wind  09Z
###unset SKIP_006048 # reflectivity 08Z
###unset SKIP_006049 # reflectivity 09Z
elif [ $cyc -eq 10 ]; then   # (09.50 - 10.49 Z)
   unset SKIP_006019 # radial wind  09Z
   unset SKIP_006020 # radial wind  10Z
###unset SKIP_006049 # reflectivity 09Z
###unset SKIP_006050 # reflectivity 10Z
elif [ $cyc -eq 11 ]; then   # (10.50 - 11.49 Z)
   unset SKIP_006020 # radial wind  10Z
   unset SKIP_006021 # radial wind  11Z
###unset SKIP_006050 # reflectivity 10Z
###unset SKIP_006051 # reflectivity 11Z
elif [ $cyc -eq 12 ]; then   # (11.50 - 12.49 Z)
   unset SKIP_006021 # radial wind  11Z
   unset SKIP_006022 # radial wind  12Z
###unset SKIP_006051 # reflectivity 11Z
###unset SKIP_006052 # reflectivity 12Z
elif [ $cyc -eq 13 ]; then   # (12.50 - 13.49 Z)
   unset SKIP_006022 # radial wind  12Z
   unset SKIP_006023 # radial wind  13Z
###unset SKIP_006052 # reflectivity 12Z
###unset SKIP_006053 # reflectivity 13Z
elif [ $cyc -eq 14 ]; then   # (13.50 - 14.49 Z)
   unset SKIP_006023 # radial wind  13Z
   unset SKIP_006024 # radial wind  14Z
###unset SKIP_006053 # reflectivity 13Z
###unset SKIP_006054 # reflectivity 14Z
elif [ $cyc -eq 15 ]; then   # (14.50 - 15.49 Z)
   unset SKIP_006024 # radial wind  14Z
   unset SKIP_006025 # radial wind  15Z
###unset SKIP_006054 # reflectivity 14Z
###unset SKIP_006055 # reflectivity 15Z
elif [ $cyc -eq 16 ]; then   # (15.50 - 16.49 Z)
   unset SKIP_006025 # radial wind  15Z
   unset SKIP_006026 # radial wind  16Z
###unset SKIP_006055 # reflectivity 15Z
###unset SKIP_006056 # reflectivity 16Z
elif [ $cyc -eq 17 ]; then   # (16.50 - 17.49 Z)
   unset SKIP_006026 # radial wind  16Z
   unset SKIP_006027 # radial wind  17Z
###unset SKIP_006056 # reflectivity 16Z
###unset SKIP_006057 # reflectivity 17Z
elif [ $cyc -eq 18 ]; then   # (17.50 - 18.49 Z)
   unset SKIP_006027 # radial wind  17Z
   unset SKIP_006028 # radial wind  18Z
###unset SKIP_006057 # reflectivity 17Z
###unset SKIP_006058 # reflectivity 18Z
elif [ $cyc -eq 19 ]; then   # (18.50 - 19.49 Z)
   unset SKIP_006028 # radial wind  18Z
   unset SKIP_006029 # radial wind  19Z
###unset SKIP_006058 # reflectivity 18Z
###unset SKIP_006059 # reflectivity 19Z
elif [ $cyc -eq 20 ]; then   # (19.50 - 20.49 Z)
   unset SKIP_006029 # radial wind  19Z
   unset SKIP_006030 # radial wind  20Z
###unset SKIP_006059 # reflectivity 19Z
###unset SKIP_006060 # reflectivity 20Z
elif [ $cyc -eq 21 ]; then   # (20.50 - 21.49 Z)
   unset SKIP_006030 # radial wind  20Z
   unset SKIP_006031 # radial wind  21Z
###unset SKIP_006060 # reflectivity 20Z
###unset SKIP_006061 # reflectivity 21Z
elif [ $cyc -eq 22 ]; then   # (21.50 - 22.49 Z)
   unset SKIP_006031 # radial wind  21Z
   unset SKIP_006032 # radial wind  22Z
###unset SKIP_006061 # reflectivity 21Z
###unset SKIP_006062 # reflectivity 22Z
elif [ $cyc -eq 23 ]; then   # (22.50 - 23.49 Z)
   unset SKIP_006032 # radial wind  22Z
   unset SKIP_006033 # radial wind  23Z
###unset SKIP_006062 # reflectivity 22Z
###unset SKIP_006063 # reflectivity 23Z
fi

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_6} 1 nexrad
error6=$?
echo "$error6" > $DATA/error6

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

sleep ${NAP} # to reverse 2min early start of jrap_dump in cron
export STATUS=NO
export DUMP_NUMBER=7

#==========================================================================
# Dump # 7 : AIRSEV, 1BHRS4, ESHRS3, LGYCLD, SSMISU, OSBUV8, CRSFDB,
#              (1)     (2)     (1)     (1)     (1)     (1)     (1)
#            SAPHIR, CRISF4, GMI1CR
#              (1)     (1)   (1)
#             TOTAL NUMBER OF SUBTYPES = 11
#=========================================================================
 
# Time window -0.50 to +0.50 hours for LGYCLD for all cycle runs
DTIM_earliest_lgycld=${DTIM_earliest_lgycld:-"-0.50"}
DTIM_latest_lgycld=${DTIM_latest_lgycld:-"+0.50"}

if [ "$RUN" = 'rap_p' ]; then

#  ===> For RUN = rap_p -- partial cycle runs
#       -------------------------------------

   def_time_window_7=1.0 # default time window for dump 7 is -1.0 to +1.0 hours

# Time window is -1.00 to +0.99 hours for 1BHRS4
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_latest_1bhrs4=${DTIM_latest_1bhrs4:-"+0.99"}      # earliest is default

# Time window is -1.00 to +0.99 hours for AIRSEV
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_latest_airsev=${DTIM_latest_airsev:-"+0.99"}      # earliest is default

# Time window is -1.00 to +0.99 hours for CRSFDB, CRISF4
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_earliest_crsfdb=${DTIM_earliest_crsfdb:-"-1.00"}
   DTIM_latest_crsfdb=${DTIM_latest_crsfdb:-"+0.99"}
   DTIM_earliest_crisf4=${DTIM_earliest_crisf4:-"-1.00"}
   DTIM_latest_crisf4=${DTIM_latest_crisf4:-"+0.99"}

# Time window is -0.50 to +0.49 hours for ESHRS3
   DTIM_earliest_eshrs3=${DTIM_earliest_eshrs3:-"-0.50"}
   DTIM_latest_eshrs3=${DTIM_latest_eshrs3:-"+0.49"}

# Time window is -1.00 to +0.99 hours for SSMISU, OSBUV8, SAPHIR
   DTIM_earliest_ssmisu=${DTIM_earliest_ssmisu:-"-1.00"}
   DTIM_latest_ssmisu=${DTIM_latest_ssmisu:-"+0.99"}
   DTIM_earliest_osbuv8=${DTIM_earliest_osbuv8:-"-1.00"}
   DTIM_latest_osbuv8=${DTIM_latest_osbuv8:-"+0.99"}
   DTIM_earliest_saphir=${DTIM_earliest_saphir:-"-1.00"}
   DTIM_latest_saphir=${DTIM_latest_saphir:-"+0.99"}

# Time window is guesstimated as -1.00 to +0.99 hours for rap_p GMI1CR
   DTIM_earliest_gmi1cr=${DTIM_earliest_gmi1cr:-"-1.00"}
   DTIM_latest_gmi1cr=${DTIM_latest_gmi1cr:-"+0.99"}

else

#  ===> For RUN = rap, rap_e -- full cycle runs (including early at 00/12z)
#       -------------------------------------------------------------------

   def_time_window_7=3.0 # default time window for dump 7 is -3.0 to +3.0 hours

# Time window is -3.00 to +2.99 hours for 1BHRS4
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_latest_1bhrs4=${DTIM_latest_1bhrs4:-"+2.99"}      # earliest is default

# Time window is -3.00 to +2.99 hours for AIRSEV
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_latest_airsev=${DTIM_latest_airsev:-"+2.99"}      # earliest is default

# Time window is -2.00 to +1.99 hours for CRSFDB, CRISF4
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_earliest_crsfdb=${DTIM_earliest_crsfdb:-"-2.00"}
   DTIM_latest_crsfdb=${DTIM_latest_crsfdb:-"+1.99"}
   DTIM_earliest_crisf4=${DTIM_earliest_crisf4:-"-2.00"}
   DTIM_latest_crisf4=${DTIM_latest_crisf4:-"+1.99"}

# Time window is -1.00 to +1.00 hours for ESHRS3
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_earliest_eshrs3=${DTIM_earliest_eshrs3:-"-1.00"}
   DTIM_latest_eshrs3=${DTIM_latest_eshrs3:-"+1.00"}

# Time window is -2.00 to +1.99 hours for SSMISU, OSBUV8
   DTIM_earliest_ssmisu=${DTIM_earliest_ssmisu:-"-2.00"}
   DTIM_latest_ssmisu=${DTIM_latest_ssmisu:-"+1.99"}
   DTIM_earliest_osbuv8=${DTIM_earliest_osbuv8:-"-2.00"}
   DTIM_latest_osbuv8=${DTIM_latest_osbuv8:-"+0.99"}

# Time window is -3.00 to +2.99 hours for SAPHIR
   DTIM_earliest_saphir=${DTIM_earliest_saphir:-"-3.00"}
   DTIM_latest_saphir=${DTIM_latest_saphir:-"+2.99"}

# Time window is guesstimated as -3.00 to +2.99 hours for GMI1CR
   DTIM_earliest_gmi1cr=${DTIM_earliest_gmi1cr:-"-3.00"}
   DTIM_latest_gmi1cr=${DTIM_latest_gmi1cr:-"+2.99"}

fi

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_7} 1 1bhrs4 \
 airsev eshrs3 lgycld ssmisu osbuv8 crsfdb saphir crisf4 gmi1cr
error7=$?
echo "$error7" > $DATA/error7

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

sleep ${NAP} # to reverse 2min early start of jrap_dump in cron
export STATUS=NO
export DUMP_NUMBER=8

#===============================================================================
# Dump # 8 : GSRASR,[ GSRCSR - moved to own group ] 
#              (1)  [  (1)]
#             TOTAL NUMBER OF SUBTYPES = 1 [2]
#===============================================================================
 
if [ "$RUN" = 'rap_p' ]; then

#  ===> For RUN = rap_p -- partial cycle runs
#       -------------------------------------

   def_time_window_8=1.0 # default time window for dump 8 is -1.0 to +1.0 hours

# Time window is -1.00 to +0.99 hours for GSRASR, GSRCSR
  DTIM_earliest_gsrasr=${DTIM_earliest_gsrasr:-"-1.00"}
  DTIM_latest_gsrasr=${DTIM_latest_gsrasr:-"+0.99"}
 
else

#  ===> For RUN = rap, rap_e -- full cycle runs (including early at 00/12z)
#       -------------------------------------------------------------------

   def_time_window_8=3.0 # default time window for dump 8 is -3.0 to +3.0 hours

# Time window is -2.00 to +1.99 hours for GSRASR, GSRCSR
   DTIM_earliest_gsrasr=${DTIM_earliest_gsrasr:-"-2.00"}
   DTIM_latest_gsrasr=${DTIM_latest_gsrasr:-"+1.99"}

fi

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_8} 1 gsrasr
error8=$?
echo "$error8" > $DATA/error8

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

sleep ${NAP} # to reverse 2min early start of jrap_dump in cron
export STATUS=NO
export DUMP_NUMBER=9

#==========================================================================
# Dump # 9 : LGHTNG + ADPUPA
#             TOTAL NUMBER OF SUBTYPES = 1 + 1
#=========================================================================

# Time window -1.00 to +0.50 hours for LGHTNG for all cycle runs
DTIM_earliest_lghtng=${DTIM_earliest_lghtng:-"-1.00"}
DTIM_latest_lghtng=${DTIM_latest_lghtng:-"+0.50"}

# Time window -1.00 to +1.00 hours for ADPUPA/UPRAIR w/ full & partial cycle runs
#  (note: time window increased over +/- 0.5 hr standard to get more data)

DTIM_earliest_adpupa=${DTIM_earliest_adpupa:-"-1.00"}
DTIM_latest_adpupa=${DTIM_latest_adpupa:-"+1.00"}

if [ "$RUN" = 'rap_p' ]; then
#  ===> For RUN = rap_p -- partial cycle runs
#       -------------------------------------
   def_time_window_9=1.0 # default time window for dump 9 is -1.0 to +1.0 hours
else
#  ===> For RUN = rap, rap_e -- full cycle runs (including early at 00/12z)
#       -------------------------------------------------------------------
   def_time_window_9=3.0 # default time window for dump 9 is -3.0 to +3.0 hours
fi

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_9} 1 lghtng adpupa
error9=$?
echo "$error9" > $DATA/error9

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

### NEW GROUP MSONET IG #10
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

sleep ${NAP} # to reverse 2min early start of jrap_dump in cron
export STATUS=NO
export DUMP_NUMBER=10

#============================================================================
# Dump # 10 : MSONE1 -- TOTAL NUMBER OF SUBTYPES = 1
#              (1)
#============================================================================

def_time_window_10=0.5 # default time window for dump 10 is -0.5 to +0.5 hours

# Time window -0.50 to +0.50 hours for MSONET for full and partial cycle runs
#  (default)

SENDCOM=NO $ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_10} 1 msone1
error10=$?
echo "$error10" > $DATA/error10

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
### NEW GROUP MSONET IG end #10

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

# UPRAIR need to start early
#sleep ${NAP} # to reverse 2min early start of jrap_dump in cron
export STATUS=NO
export DUMP_NUMBER=11

#==========================================================================
# Dump # 11 :  ADPUPA minus UPRAIR
#               (6)   minus  (5)
#            -- TOTAL NUMBER OF SUBTYPES = 11-5
#==========================================================================
#
if [ "$RUN" = 'rap_p' ]; then

#  ===> For RUN = rap_p -- partial cycle runs
#       -------------------------------------

   def_time_window_11=1.5 # default time window for dump 11 is -1.5 to +1.5 hours

else

#  ===> For RUN = rap, rap_e -- full cycle runs (including early at 00/12z)
#       -------------------------------------------------------------------

   if [ $cyc -eq 00 -o $cyc -eq 12 ]; then
      def_time_window_11=1.5  # default time window for dump 11 is -1.5 to +1.5
                              # hours for 00 or 12z
   else
      def_time_window_11=2.5  # default time window for dump 11 is -2.5 to +2.5
                              # hours for all other cycles
   fi

fi

# Time window -1.00 to +1.00 hours for ADPUPA/UPRAIR w/ full & partial cycle runs
#  (note: time window increased over +/- 0.5 hr standard to get more data)

DTIM_earliest_uprair=${DTIM_earliest_uprair:-"-1.00"}
DTIM_latest_uprair=${DTIM_latest_uprair:-"+1.00"}

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_11} 1 uprair
error11=$?
echo "$error11" > $DATA/error11

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


set +x
#----------------------------------------------------------------
cat<<\EOF>thread_12; chmod +x thread_12
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_12
echo Executing on node  `hostname`
echo Starting time: `date -u`
echo "********************************************************************"
echo
set -x

sleep ${NAP} # to reverse 2min early start of jrap_dump in cron
export STATUS=NO
export DUMP_NUMBER=12

#===============================================================================
# Dump # 12 :  GSRCSR
#              (1)
#             TOTAL NUMBER OF SUBTYPES = 1
#===============================================================================

if [ "$RUN" = 'rap_p' ]; then

#  ===> For RUN = rap_p -- partial cycle runs
#       -------------------------------------

   def_time_window_12=1.0 # default time window for dump 12 is -1.0 to +1.0 hours

# Time window is -1.00 to +0.99 hours for GSRASR, GSRCSR
  DTIM_earliest_gsrcsr=${DTIM_earliest_gsrcsr:-"-1.00"}
  DTIM_latest_gsrcsr=${DTIM_latest_gsrcsr:-"+0.99"}

else

#  ===> For RUN = rap, rap_e -- full cycle runs (including early at 00/12z)
#       -------------------------------------------------------------------

   def_time_window_12=3.0 # default time window for dump 12 is -3.0 to +3.0 hours

# Time window is -2.00 to +1.99 hours for GSRASR, GSRCSR
   DTIM_earliest_gsrcsr=${DTIM_earliest_gsrcsr:-"-2.00"}
   DTIM_latest_gsrcsr=${DTIM_latest_gsrcsr:-"+1.99"}

fi

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_12} 1 gsrcsr
error12=$?
echo "$error12" > $DATA/error12

set +x
echo "********************************************************************"
echo Script thread_12
echo Finished executing on node  `hostname`
echo Ending time  : `date -u`
echo "********************************************************************"
set -x
} > $DATA/12.out 2>&1
EOF
set -x

set +x
cat<<\EOF>thread_13; chmod +x thread_13
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_13
echo Executing on node  `hostname`
echo Starting time: `date -u`
echo "********************************************************************"
echo
set -x

sleep ${NAP} # to reverse 2min early start of jrap_dump in cron
export STATUS=NO
export DUMP_NUMBER=13

#===========================================================================
# Dump # 13 : GPSIPW -- TOTAL NUMBER OF SUBTYPES = 1
#              (1)
#===========================================================================

def_time_window_13=0.5 # default time window for dump 13 is -0.5 to +0.5 hours

# gpsipw
if [ "$RUN" = 'rap_p' ]; then

#  ===> For RUN = rap_p -- partial cycle runs
#         -------------------------------------
  
  if [ $cyc -ne 08 -a $cyc -ne 20 ]; then

# Time window -0.05 to +0.05 hours (-3 to +3 min) for GPSIPW at all cycles
#   except 08 and 20z

    DTIM_earliest_gpsipw=${DTIM_earliest_gpsipw:-"-0.05"}
    DTIM_latest_gpsipw=${DTIM_latest_gpsipw:-"+0.05"}

  else

# Time window -0.55 to -0.45 hours (-33 to -27 min) for GPSIPW for 08 or 20z
#  cycle

     DTIM_earliest_gpsipw=${DTIM_earliest_gpsipw:-"-0.55"}
     DTIM_latest_gpsipw=${DTIM_latest_gpsipw:-"-0.45"}

  fi

else

#  ===> For RUN = rap, rap_e -- full cycle runs (including early at 00/12z)
#       -------------------------------------------------------------------

# Time window -1.05 to -0.95 hours (-63 to -57 min) for GPSIPW

   DTIM_earliest_gpsipw=${DTIM_earliest_gpsipw:-"-1.05"}
   DTIM_latest_gpsipw=${DTIM_latest_gpsipw:-"-0.95"}
                                                                                     
fi

#  {note: new Ground Based GPS-IPW/ZTD (from U.S.-ENI and foreign GNSS
#         providers) is currently limited to obs closest to cycle-time that
#         result in a U.S.-ENI dump count that is not too much larger than that
#         from the previous U.S. (only) GSD-feed, since the ENI reports are
#         available every 5 min while the GSD reports were available only every
#         30 min. Also accounts for an approximate 80-min latency present in
#         the U.S.-ENI reports.}
                                                                                     
$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_13} 1 gpsipw
error13=$?
echo "$error13" > $DATA/error13

set +x
echo "********************************************************************"
echo Script thread_13
echo Finished executing on node  `hostname`
echo Ending time  : `date -u`
echo "********************************************************************"
set -x
} > $DATA/13.out 2>&1
EOF
set -x

#----------------------------------------------------------------
# Now launch the threads

# determine local system name and type if available
# -------------------------------------------------
SITE=${SITE:-""}

set +u
launcher=${launcher:-"cfp"}  # if not "cfp", threads will run serially.

if [ "$launcher" = cfp ]; then
   > $DATA/poe.cmdfile

# To better take advantage of cfp, execute the longer running commands first.
# Some reordering was done here based on recent sample runtimes.
   [ $DUMP_group11 = YES ]  &&  echo ./thread_11 >> $DATA/poe.cmdfile
   [ $DUMP_group9 = YES ]  &&  echo ./thread_9 >> $DATA/poe.cmdfile  # lghtng 1st
   [ $DUMP_group7 = YES ]  &&  echo ./thread_7 >> $DATA/poe.cmdfile  # moved up
   [ $DUMP_group1 = YES ]  &&  echo ./thread_1 >> $DATA/poe.cmdfile
   [ $DUMP_group4 = YES ]  &&  echo ./thread_4 >> $DATA/poe.cmdfile  # moved up
   [ $DUMP_group8 = YES ]  &&  echo ./thread_8 >> $DATA/poe.cmdfile  # new
   [ $DUMP_group6 = YES ]  &&  echo ./thread_6 >> $DATA/poe.cmdfile  # moved up
   [ $DUMP_group5 = YES ]  &&  echo ./thread_5 >> $DATA/poe.cmdfile  # moved up
   [ $DUMP_group2 = YES ]  &&  echo ./thread_2 >> $DATA/poe.cmdfile
   [ $DUMP_group3 = YES ]  &&  echo ./thread_3 >> $DATA/poe.cmdfile
   [ $DUMP_group10 = YES ]  &&  echo ./thread_10 >> $DATA/poe.cmdfile
   #[ $DUMP_group11 = YES ]  &&  echo ./thread_11 >> $DATA/poe.cmdfile
   [ $DUMP_group12 = YES ]  &&  echo ./thread_12 >> $DATA/poe.cmdfile 
   [ $DUMP_group13 = YES ]  &&  echo ./thread_13 >> $DATA/poe.cmdfile

   if [ -s $DATA/poe.cmdfile ]; then
      export MP_CSS_INTERRUPT=yes  # ??
      launcher_DUMP=${launcher_DUMP:-mpiexec}
      #$launcher_DUMP -np 3 --cpu-bind verbose,core cfp $DATA/poe.cmdfile
      NPROCS=${NPROCS:-14}
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
      echo "Runing threads serially"
      [ $DUMP_group1 = YES ]  &&  ./thread_1
      [ $DUMP_group2 = YES ]  &&  ./thread_2
      [ $DUMP_group3 = YES ]  &&  ./thread_3
      [ $DUMP_group4 = YES ]  &&  ./thread_4
      [ $DUMP_group5 = YES ]  &&  ./thread_5
      [ $DUMP_group6 = YES ]  &&  ./thread_6
      [ $DUMP_group7 = YES ]  &&  ./thread_7
      [ $DUMP_group8 = YES ]  &&  ./thread_8
      [ $DUMP_group9 = YES ]  &&  ./thread_9
      [ $DUMP_group10 = YES ]  &&  ./thread_10
      [ $DUMP_group11 = YES ]  &&  ./thread_11
      [ $DUMP_group12 = YES ]  &&  ./thread_12
      [ $DUMP_group13 = YES ]  &&  ./thread_13
fi

cat $DATA/1.out $DATA/2.out $DATA/3.out $DATA/4.out $DATA/5.out $DATA/6.out $DATA/7.out $DATA/8.out $DATA/9.out $DATA/10.out $DATA/11.out $DATA/12.out $DATA/13.out

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
[ -s $DATA/error12 ] && err12=`cat $DATA/error12`
[ -s $DATA/error13 ] && err13=`cat $DATA/error13`

#===============================================================================

export STATUS=YES
export DUMP_NUMBER=14
$ushscript_dump/bufr_dump_obs.sh $dumptime 3.00 1 null

#  endif loop $PROCESS_DUMP
fi

echo " " >> $pgmout
echo "##################################################################\
####################"  >> $pgmout
echo " " >> $pgmout

#====================================================
#====================================================

if [ "$PROCESS_DUMP" = 'YES' ]; then

   if [ "$err1" -gt '5' -o "$err2" -gt '5' -o "$err3" -gt '5' -o \
        "$err4" -gt '5' -o "$err5" -gt '5' -o "$err6" -gt '5' -o \
        "$err7" -gt '5' -o "$err8" -gt '5' -o "$err9" -gt '5' -o \
	"$err10" -gt '5' -o "$err11" -gt '5' -o "$err12" -gt '5' -o \
       	"$err13" -gt '5' ]; then
      for n in $err1 $err2 $err3 $err4 $err5 $err6 $err7 $err8 $err9 $err10 $err11 $err12 $err13
      do
         if [ "$n" -gt '5' ]; then
            if [ "$n" -ne '11' -a "$n" -ne '22' ]; then

## fatal error in dumping of BUFR obs. files

               set +x
echo
echo " ###################################################### "
echo " --> > 22 RETURN CODE FROM DATA DUMP, $err1, $err2, $err3, $err4, \
$err5, $err6, $err7, $err8, $err9, $err10, $err11, $err12, $err13"
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
$err5, $err6, $err7, $err8, $err9, $err10, $err11, $err12, $err13"
      echo " --> NOT ALL DATA DUMP FILES ARE COMPLETE - CONTINUE    "
      echo " ###################################################### "
      echo
      set -x
   fi

#  concatenate msone0 and msone1, b/c prepobs only wants one file
   cat ${DATA}/msone0.ibm ${DATA}/msone1.ibm > ${DATA}/msonet.ibm
   cpfs ${DATA}/msonet.ibm ${COMSP}msonet.${tmmark}.bufr_d
   chmod 640 ${COMSP}msonet.${tmmark}.bufr_d
   chgrp rstprod ${COMSP}msonet.${tmmark}.bufr_d

#  endif loop $PROCESS_DUMP   
fi

grep -q "004.004 in data group aircar for .............-.........\
.... HAS      0 REPORTS" ${COMSP}status.$tmmark.bufr_d
err_grep1=$?
grep -q "004.007 in data group aircar for .............-.........\
.... HAS      0 REPORTS" ${COMSP}status.$tmmark.bufr_d
err_grep2=$?
if [ $err_grep1 -eq 0 -a $err_grep2 -eq 0 ]; then
   msg="***WARNING: NO ACARS data from either ARINC (004.004) or AFWA \
(004.007), run assimilation without any ACARS data"
   $DATA/postmsg "$jlogfile" "$msg"
fi


if [ $SENDDBN = YES ]; then
   if [ -s ${COMSP}1bamua.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_1bamua $job ${COMSP}1bamua.tm00.bufr_d
   fi
   if [ -s ${COMSP}1bhrs4.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_1bhrs4 $job ${COMSP}1bhrs4.tm00.bufr_d
   fi
   if [ -s ${COMSP}1bmhs.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_1bmhs $job ${COMSP}1bmhs.tm00.bufr_d
   fi
   if [ -s ${COMSP}airsev.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_airsev $job ${COMSP}airsev.tm00.bufr_d
   fi
   if [ -s ${COMSP}amsr2.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_amsr2 $job ${COMSP}amsr2.tm00.bufr_d
   fi
   if [ -s ${COMSP}ascatt.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_ascatt $job ${COMSP}ascatt.tm00.bufr_d
   fi
   if [ -s ${COMSP}ascatw.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_ascatw $job ${COMSP}ascatw.tm00.bufr_d
   fi
   if [ -s ${COMSP}atms.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_atms $job ${COMSP}atms.tm00.bufr_d
   fi
   if [ -s ${COMSP}atmsdb.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_atmsdb $job ${COMSP}atmsdb.tm00.bufr_d
   fi
   if [ -s ${COMSP}crsfdb.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_crsfdb $job ${COMSP}crsfdb.tm00.bufr_d
   fi
   if [ -s ${COMSP}crisf4.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_crisf4 $job ${COMSP}crisf4.tm00.bufr_d
   fi
   if [ -s ${COMSP}esamua.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_esamua $job ${COMSP}esamua.tm00.bufr_d
   fi
   if [ -s ${COMSP}esatms.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_esatms $job ${COMSP}esatms.tm00.bufr_d
   fi
   if [ -s ${COMSP}eshrs3.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_eshrs3 $job ${COMSP}eshrs3.tm00.bufr_d
   fi
   if [ -s ${COMSP}esiasi.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_esiasi $job ${COMSP}esiasi.tm00.bufr_d
   fi
   if [ -s ${COMSP}esmhs.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_esmhs $job ${COMSP}esmhs.tm00.bufr_d
   fi
   if [ -s ${COMSP}gpsipw.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_gpsipw $job ${COMSP}gpsipw.tm00.bufr_d
   fi
   if [ -s ${COMSP}gsrasr.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_gsrasr $job ${COMSP}gsrasr.tm00.bufr_d
   fi
   if [ -s ${COMSP}gsrcsr.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_gsrcsr $job ${COMSP}gsrcsr.tm00.bufr_d
   fi
   if [ -s ${COMSP}iasidb.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_iasidb $job ${COMSP}iasidb.tm00.bufr_d
   fi
   if [ -s ${COMSP}lghtng.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_lghtng $job ${COMSP}lghtng.tm00.bufr_d
   fi
   if [ -s ${COMSP}lgycld.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_lgycld $job ${COMSP}lgycld.tm00.bufr_d
   fi
   if [ -s ${COMSP}mtiasi.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_mtiasi $job ${COMSP}mtiasi.tm00.bufr_d
   fi
   if [ -s ${COMSP}nexrad.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_nexrad $job ${COMSP}nexrad.tm00.bufr_d
   fi
   if [ -s ${COMSP}osbuv8.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_osbuv8 $job ${COMSP}osbuv8.tm00.bufr_d
   fi
   if [ -s ${COMSP}rassda.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_rassda $job ${COMSP}rassda.tm00.bufr_d
   fi
   if [ -s ${COMSP}saphir.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_saphir $job ${COMSP}saphir.tm00.bufr_d
   fi
   if [ -s ${COMSP}satwnd.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_satwnd $job ${COMSP}satwnd.tm00.bufr_d
   fi
   if [ -s ${COMSP}sevasr.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_sevasr $job ${COMSP}sevasr.tm00.bufr_d
   fi
   if [ -s ${COMSP}sevcsr.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_sevcsr $job ${COMSP}sevcsr.tm00.bufr_d
   fi
   if [ -s ${COMSP}ssmisu.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_ssmisu $job ${COMSP}ssmisu.tm00.bufr_d
   fi
   if [ -s ${COMSP}uprair.tm00.bufr_d ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN_uc}_BUFR_uprair $job ${COMSP}uprair.tm00.bufr_d
   fi
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
