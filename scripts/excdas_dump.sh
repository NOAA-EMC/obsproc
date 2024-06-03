#!/bin/ksh
#
###########################################################################
echo "--------------------------------------------------------------------"
echo "excdas_dump.sh - CDAS network data dump processing              "
echo "--------------------------------------------------------------------"
echo "History: Dec  3 2014 - Original script, split off from              "
echo "                       exglobal_dump.sh.ecf and tailored exclusively"
echo "                       to CDAS.                                     "
echo "         Feb  2 2015 - Dump window for GOES satwnd types from tanks "
echo "                       NC005010, NC005011 & NC005012 changed from   "
echo "                       -1.00 to -0.01 hours about center dump time  "
echo "                       to -3.00 to +2.99 hours about center dump    "
echo "                       Dump window for new satwnd types NC005091,   "
echo "                       NC005080 & NC005091 set to -3.00 to +2.99    "
echo "                       hours about center dump time.                "
echo "         Jul 17 2017 - Add cfp option and remove background threads."
echo "         Sep 06 2019 - Updated to run on Dell-p3. Change background "
echo "                       threads to serial. Skip 2 tanks in msonet    "
echo "                       dump group that CDAS will not process;       "
echo "                       disabled copy of snowdepth and sstoi from    "
echo "                       dcom to com; disabled dump of radwnd data    "
echo "                       (ROC ending this data product and not        "
echo "                       transitioned to ph3).                        "
echo "         Sep 14 2020 - Updated to remove obsolete dump mnemonic     "
echo "                       from Dump group #1: goesfv.                  "
echo "                     - Updated Dump group #8 to skip legacy Meteosat"
echo "                       AMV subsets: 005064, 005065, 005066.  These  "
echo "                       subsets are replaced by new WMO BUFR sequence"
echo "                       subsets: 005067, 005068, 005069.  The format "
echo "                       change from EUMETSAT occurs Oct 6, 2020.     "
echo "                     - Updated Dump group #8 to process new WMO BUFR"
echo "                       sequence subset for VIIRS AMVs: 005091.      "
echo "                       Removed obsolete subset: 005090.             "
echo "         Mar 09 2021 - Incremented subsets for the sfcshp dump      "
echo "                       groups to match bufr_dumplist.  Removed tideg"
echo "                       from sfcshp dump group to make unique dump   "
echo "                       file.                                        "
echo "                     - Copy bufr_dumplist to COMOUT.                "
echo "         Mar 08 2022 - Enable the dumping of 002017 in vadwnd dump  "
echo "                       group.                                       "
echo "         Jul 30 2022 - Subpfl, saldrn, snocvr, and gmi1cr added     "
echo "                       to dump group #9.    "
echo "         Sep 30 2022 - Don't / Enable dumping of UPRAIR data in     "
echo "                       group #3./ b/c it is too slow                "
echo "         Oct 17 2023 - Split msonet to msonet (#5) and msone1 (#10) "
echo "                      concatenate msonet and msone1 right after dump"
echo "                      Turn off msonet and msone1 - not needed       "
###########################################################################

set -xau

# set some variables if they have not already been set

set +u

# JOB_NUMBER = 1 indicates the prepbufr dump job.
# JOB_NUMBER = 2 indicates the non-prepbufr dump job.
# JOB_NUMBER not present indicates dump BOTH prepbufr and non-prepbufr data.
# -----------------------------------------------------------------------------
# Dump group #1 (non-pb, TIME_TRIM = OFF) =
#               1bamua 1bhrs4 1bmhs osbuv8 mtiasi
#               avcsam avcspm esamua eshrs3 esmhs 
#               airsev gome gpsro lgycld lghtng
#
# Dump group #2 (pb, TIME_TRIM = OFF) =
#               sfcshp tideg atovs adpsfc
#
# Dump group #3 (pb, TIME_TRIM = OFF) =
#               adpupa gpsipw ascatt
#               # pull out uprair, too slow
# Dump group #4 (pb, TIME_TRIM = default = ON) =
#               aircar aircft proflr vadwnd rassda
#
# Dump group #5 (pb, TIME_TRIM = OFF) =
#               msonet
#
# Dump group #6 (non-pb, TIME_TRIM = OFF) =
#               nexrad
#
# Dump group #7 (non-pb, TIME_TRIM = OFF) =
#               omi ssmisu sevcsr bathy tesac trkob atms cris mls
#
# Dump group #8 (pb, TIME_TRIM = default = ON) =
#               satwnd
#
# Dump group #9 (non-pb, TIME_TRIM = default = ON) =
#               geoimr subpfl saldrn snocvr gmi1cr
#
# Dump group #10 (pb, TIME_TRIM = OFF) =
#               msone1 # ONLY tank b255/xx030, the largest
#
# Dump group #11 STATUS FILE
# -----------------------------------------------------------------------------

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
      DUMP_group8=${DUMP_group8:-"NO"}
      DUMP_group9=${DUMP_group9:-"YES"}
      DUMP_group10=${DUMP_group10:="NO"}
   else
      dump_ind=DUMP
      DUMP_group1=${DUMP_group1:-"NO"}
      DUMP_group2=${DUMP_group2:-"YES"}
      DUMP_group3=${DUMP_group3:-"YES"}
      DUMP_group4=${DUMP_group4:-"YES"}
      #DUMP_group5=${DUMP_group5:-"YES"}
      DUMP_group5=${DUMP_group5:-"NO"}
      DUMP_group6=${DUMP_group6:-"NO"}
      DUMP_group7=${DUMP_group7:-"NO"}
      DUMP_group8=${DUMP_group8:-"YES"}
      DUMP_group9=${DUMP_group9:-"NO"}
      #DUMP_group10=${DUMP_group10:="YES"}
      DUMP_group10=${DUMP_group10:="NO"}
   fi
else
   dump_ind=DUMP
   DUMP_group1=${DUMP_group1:-"YES"}
   DUMP_group2=${DUMP_group2:-"YES"}
   DUMP_group3=${DUMP_group3:-"YES"}
   DUMP_group4=${DUMP_group4:-"YES"}
   #DUMP_group5=${DUMP_group5:-"YES"}
   DUMP_group5=${DUMP_group5:-"NO"}
   DUMP_group6=${DUMP_group6:-"YES"}
   DUMP_group7=${DUMP_group7:-"YES"}
   DUMP_group8=${DUMP_group8:-"YES"}
   DUMP_group9=${DUMP_group9:-"YES"}
   #DUMP_group10=${DUMP_group10:="YES"}
   DUMP_group10=${DUMP_group10:="NO"}
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

export dumptime=`cut -c7-16 ncepdate`
export cycp=`echo $dumptime|cut -c9-10`

export NET_uc=$(echo $NET | tr [a-z] [A-Z])
export tmmark_uc=$(echo $tmmark | tr [a-z] [A-Z])

msg="$NET_uc ANALYSIS TIME IS $PDY$cyc"
$DATA/postmsg "$jlogfile" "$msg"

set +x
echo
echo "CENTER DATA DUMP DATE-TIME FOR $tmmark_uc $NET_uc IS $dumptime"
echo
set -x

export COMSP=$COMOUT/$RUN.${cycle}.

if [ "$PROCESS_GRIBFLDS" = 'YES' ]; then

########################################################
########################################################
#  copy snogrb (0.5 deg) from $TANK_GRIBFLDS  # disabled in obsproc_cdas.v2.5.0
#  copy snogrb_t574      from $TANK_GRIBFLDS  # disabled in obsproc_cdas.v2.5.0
#  copy engicegrb        from $COM_ENGICE
#  copy sstgrb           from $COM_SSTOI
#  generate sstgrb index file
########################################################
########################################################
# copy of snowgrb and snogrb_t574 disabled in obsproc_cdas.v2.5.0
#  snogrb=$TANK_GRIBFLDS/$PDY/wgrbbul/snowdepth.global.grb
#  snoold=$TANK_GRIBFLDS/$PDYm1/wgrbbul/snowdepth.global.grb
#
#  if [ -s $snogrb ]; then
#     cp $snogrb ${COMSP}snogrb
#     msg="todays 0.5 degree snow grib file located and copied to ${COMSP}snogrb"
#     $DATA/postmsg "$jlogfile" "$msg"
#  elif [ -s $snoold ]; then
#     cp $snoold ${COMSP}snogrb
#     msg="**todays 0.5 degree snow grib file not located - copy 1-day old file"
#     $DATA/postmsg "$jlogfile" "$msg"
#  else
#     set +x
#     echo " "
#     echo " #####################################################"
#     echo " cannot locate 0.5 degree snow grib file - fatal error"
#     echo " #####################################################"
#     echo " "
#     set -x
#     msg="**CANNOT LOCATE 0.5 DEGREE SNOW GRIB FILE --> EXIT DATA DUMP"
#     $DATA/postmsg "$jlogfile" "$msg"
#     $DATA/err_exit
#  fi
#
#  snogrb_t574=$TANK_GRIBFLDS/$PDY/wgrbbul/snowdepth.t574.grb
#  snoold_t574=$TANK_GRIBFLDS/$PDYm1/wgrbbul/snowdepth.t574.grb
#
#  if [ -s $snogrb_t574 ]; then
#     cp $snogrb_t574 ${COMSP}snogrb_t574
#     msg="todays T574 snow grib file located and copied to ${COMSP}snogrb_t574"
#     $DATA/postmsg "$jlogfile" "$msg"
#  elif [ -s $snoold_t574 ]; then
#     cp $snoold_t574 ${COMSP}snogrb_t574
#     msg="**todays T574 snow grib file not located - copy 1-day old file"
#     $DATA/postmsg "$jlogfile" "$msg"
#  else
#     set +x
#     echo " "
#     echo " ###############################################"
#     echo " cannot locate T574 snow grib file - fatal error"
#     echo " ###############################################"
#     echo " "
#     set -x
#     msg="**CANNOT LOCATE T574 SNOW GRIB FILE --> EXIT DATA DUMP"
#     $DATA/postmsg "$jlogfile" "$msg"
#     $DATA/err_exit
#  fi

   engicegrb=${COM_ENGICE}.$PDY/engice.t00z.grb
   engiceold=${COM_ENGICE}.$PDYm1/engice.t00z.grb

   if [ -s $engicegrb ]; then
      cp $engicegrb ${COMSP}engicegrb
      msg="todays engice grib file located and copied to ${COMSP}engicegrb"
      $DATA/postmsg "$jlogfile" "$msg"
   elif [ -s $engiceold ]; then
      cp $engiceold ${COMSP}engicegrb
      msg="**todays engice grib file not located - copy 1-day old file"
      $DATA/postmsg "$jlogfile" "$msg"
   else
      set +x
      echo " "
      echo " ############################################"
      echo " cannot locate engice grib file - fatal error"
      echo " ############################################"
      echo " "
      set -x
      msg="**CANNOT LOCATE ENGICE GRIB FILE --> EXIT DATA DUMP"
      $DATA/postmsg "$jlogfile" "$msg"
      $DATA/err_exit
   fi

# copy of sstgrb and ssstold disabled in obsproc_cdas.v2.5.0
#  sstgrb=${COM_SSTOI}.$PDY/sstoi_grb
#  sstold=${COM_SSTOI}.$PDYm1/sstoi_grb

#  if [ -s $sstgrb ]; then
#     cp $sstgrb ${COMSP}sstgrb
#     msg="todays sst grib file located and copied to ${COMSP}sstgrb"
#     $DATA/postmsg "$jlogfile" "$msg"
#  elif [ -s $sstold ]; then
#     cp $sstold ${COMSP}sstgrb
#     msg="**todays sst grib file not located - copy 1-day old file"
#     $DATA/postmsg "$jlogfile" "$msg"
#  else
#     set +x
#     echo " "
#     echo " #########################################"
#     echo " cannot locate sst grib file - fatal error"
#     echo " #########################################"
#     echo " "
#     set -x
#     msg="**CANNOT LOCATE SST GRIB FILE --> EXIT DATA DUMP"
#     $DATA/postmsg "$jlogfile" "$msg"
#     $DATA/err_exit
#  fi

#  rm errfile
#  $GRBINDEX ${COMSP}sstgrb ${COMSP}sstgrb.index 2> errfile
#  errindx=$?
#  [ "$errindx" -ne '0' ] && cat errfile
#  rm errfile

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

#restrict processing of unexpected big tanks
#this block appear in all /scripts/ex*_dump.sh proessing msonet and msone1
TANK_MAX_255003=${TANK_MAX_255003:-3221225472} #3Gb
TANK_MAX_255004=${TANK_MAX_255004:-1610612736} #1.5Gb
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
#echo $msg | mail.py -s "$msg" -c iliana.genkova@noaa.gov
 fi
if [ -s ${TANK}/${PDY}/b255/xx030 ] && [ "$(stat -c '%s' ${TANK}/${PDY}/b255/xx030)" -gt "$TANK_MAX_255030" ]; then
 export SKIP_255030=YES
 msg="WARNING: TANK b255/xx030 exceeds TANK_MAX_255030 => not dumped"
 echo $msg | mail.py -s "$msg" 
#echo $msg | mail.py -s "$msg" -c iliana.genkova@noaa.gov
fi
#end of block

if [ "$PROCESS_DUMP" = 'YES' ]; then

####################################
####################################
#  The data "dump" script for tm00
####################################
####################################

msg="START THE $tmmark_uc $NET_uc DATA $dump_ind CENTERED ON $dumptime"
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

export STATUS=NO
export DUMP_NUMBER=1

#=========================================================================
# NOTES ABOUT THIS DUMP GROUP:
#   (1) time window radius is -3.00 to +2.99 hours on all types
#   (2) TIME TRIMMING IS NOT DONE IN THIS DUMP
#
#--------------------------------------------------------------------------
# Dump # 1 : 1BAMUA: 1 subtype(s)
#            1BHRS4: 1 subtype(s)
#            1BMHS:  1 subtype(s)
#            RADWND: 2 subtype(s)
#            OSBUV8: 1 subtype(s)
#            MTIASI: 1 subtype(s)
#            AVCSAM: 1 subtype(s)
#            AVCSPM: 1 subtype(s)
#            ESAMUA: 1 subtype(s)
#            ESHRS3: 1 subtype(s)
#            ESMHS:  1 subtype(s)
#            AIRSEV: 1 subtype(s)
#            GOME:   1 subtype(s)
#            GPSRO:  1 subtype(s)
#            LGYCLD: 1 subtype(s)
#            LGHTNG: 2 subtype(s)
#            ----------------------
#            TOTAL NUMBER OF SUBTYPES = 18
#
#=========================================================================

DTIM_latest_lgycld=${DTIM_latest_lgycld:-"+2.99"}
DTIM_latest_lghtng=${DTIM_latest_lghtng:-"+2.99"}
DTIM_latest_1bamua=${DTIM_latest_1bamua:-"+2.99"}
DTIM_latest_1bhrs4=${DTIM_latest_1bhrs4:-"+2.99"}
DTIM_latest_1bmhs=${DTIM_latest_1bmhs:-"+2.99"}
DTIM_latest_osbuv8=${DTIM_latest_osbuv8:-"+2.99"}
DTIM_latest_mtiasi=${DTIM_latest_mtiasi:-"+2.99"}
DTIM_latest_avcsam=${DTIM_latest_avcsam:-"+2.99"}
DTIM_latest_avcspm=${DTIM_latest_avcspm:-"+2.99"}
DTIM_latest_esamua=${DTIM_latest_esamua:-"+2.99"}
DTIM_latest_eshrs3=${DTIM_latest_eshrs3:-"+2.99"}
DTIM_latest_esmhs=${DTIM_latest_esmhs:-"+2.99"}

DTIM_latest_airsev=${DTIM_latest_airsev:-"+2.99"}
DTIM_latest_gome=${DTIM_latest_gome:-"+2.99"}
DTIM_latest_gpsro=${DTIM_latest_gpsro:-"+2.99"}

TIME_TRIM=off

$ushscript_dump/bufr_dump_obs.sh $dumptime 3.0 1 1bamua \
 1bhrs4 1bmhs osbuv8 mtiasi avcsam avcspm esamua \
 eshrs3 esmhs airsev gome gpsro lgycld lghtng
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

export STATUS=NO
export DUMP_NUMBER=2

#==========================================================================
# NOTES ABOUT THIS DUMP GROUP:
#   (1) time window radius is -3.00 to +2.99 hours on all types
#   (2) TIME TRIMMING IS NOT DONE IN THIS DUMP
#
#--------------------------------------------------------------------------
# Dump # 2 : SFCSHP:11 subtype(s)
#            ATOVS:  1 subtype(s)
#            ADPSFC: 4 subtype(s)
#            TIDEG:  1 subtype(s)
#  xxxxxxxxx WNDSAT: 1 subtype(s) (if present in past 10 days of tanks)
# ===> Dumping of WNDSAT removed from here until new ingest feed is established
#      (had been dumped with a time window radius of -3.00 to +2.99 hours)
#            ----------------------
#            TOTAL NUMBER OF SUBTYPES = 17
#
#==========================================================================

DTIM_latest_sfcshp=${DTIM_latest_sfcshp:-"+2.99"}
DTIM_latest_tideg=${DTIM_latest_tideg:-"+2.99"}
DTIM_latest_atovs=${DTIM_latest_atovs:-"+2.99"}
DTIM_latest_adpsfc=${DTIM_latest_adpsfc:-"+2.99"}
#-----------------------------------------------
# check for wndsat tank presence in past 10 days
wndsat=""
err_check_tanks=0
##########sh $USHbufr_dump/check_tanks.sh wndsat
##########err_check_tanks=$?
err_check_tanks=99 # comment out 2 lines above & add this line to ensure wndsat
                   # is not ever dumped
if [ $err_check_tanks -eq 0 ];then
   wndsat=wndsat
   DTIM_latest_wndsat=${DTIM_latest_wndsat:-"+2.99"}
fi
#-----------------------------------------------

TIME_TRIM=off

$ushscript_dump/bufr_dump_obs.sh $dumptime 3.0 1 sfcshp tideg atovs adpsfc $wndsat
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

export STATUS=NO
export DUMP_NUMBER=3

#====================================================================
# NOTES ABOUT THIS DUMP GROUP:
#   (1) time window radius is -3.00 to +2.99 hours on all types
#   (2) TIME TRIMMING IS NOT DONE IN THIS DUMP
#
#--------------------------------------------------------------------------
# Dump #3:   ADPUPA: 6 subtype(s)
#            #UPRAIR: 5 subtype(s) - pull out, too slow
#            GPSIPW: 1 subtype(s)
#            ASCATT: 1 subtype(s)
#            ---------------------
#            TOTAL NUMBER OF SUBTYPES = 13
#
#====================================================================

# other dump types
# ----------------
DTIM_latest_adpupa=${DTIM_latest_adpupa:-"+2.99"}
DTIM_latest_uprair=${DTIM_latest_uprair:-"+2.99"}
DTIM_latest_gpsipw=${DTIM_latest_gpsipw:-"+2.99"}
DTIM_latest_ascatt=${DTIM_latest_ascatt:-"+2.99"}

TIME_TRIM=off

# $ushscript_dump/bufr_dump_obs.sh $dumptime 3.0 1 adpupa uprair gpsipw ascatt
$ushscript_dump/bufr_dump_obs.sh $dumptime 3.0 1 adpupa gpsipw ascatt
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

export STATUS=NO
export DUMP_NUMBER=4

#=======================================================================
# NOTES ABOUT THIS DUMP GROUP:
#   (1) time window radius is -3.00 to +2.99 hours on all types
#       EXCEPT: AIRCFT where it is +/- 3.25 hours
#               AIRCAR where it is +/- 3.25 hours
#               PROFLR where it is -4.00 to +3.99 hours
#   (2) TIME TRIMMING IS DONE IN THIS DUMP
#
#--------------------------------------------------------------------------
# Dump # 4 : AIRCAR: 2 subtype(s)
#            AIRCFT: 8 subtype(s)
#            PROFLR: 4 subtype(s)
#            VADWND: 1 subtype(s)
#            RASSDA: 1 subtype(s)
#            ------------------------ 
#            TOTAL NUMBER OF SUBTYPES = 16
#
#=======================================================================

# Skip NeXRaD VAD WINDS FROM LEVEL 2 DECODER (not ready to be handled in GSI)
# 3/9/2022 -- enable the dumping of 002017 in the vadwnd dump group.
#export SKIP_002017=YES

# Dump AIRCFT and AIRCAR with wide time window to improve PREPOBS_PREPACQC
#  track-check performance
#  (time window will be winnowed down to +/- 3.00 hours in output from
#   PREPOBS_PREPACQC)

# Dump PROFLR with wide time window to improve PREPOBS_PROFCQC performance
#  (time window will be winnowed down in output from PREPOBS_PROFCQC, see
#   parm cards for output time window)

DTIM_earliest_aircft=${DTIM_earliest_aircft:-"-3.25"}
DTIM_latest_aircft=${DTIM_latest_aircft:-"+3.25"}

DTIM_earliest_aircar=${DTIM_earliest_aircar:-"-3.25"}
DTIM_latest_aircar=${DTIM_latest_aircar:-"+3.25"}

DTIM_earliest_proflr=${DTIM_earliest_proflr:-"-4.00"}
DTIM_latest_proflr=${DTIM_latest_proflr:-"+3.99"}

DTIM_latest_vadwnd=${DTIM_latest_vadwnd:-"+2.99"}
DTIM_latest_rassda=${DTIM_latest_rassda:-"+2.99"}

$ushscript_dump/bufr_dump_obs.sh $dumptime 3.0 1 aircar aircft proflr vadwnd \
 rassda
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

export STATUS=NO
export DUMP_NUMBER=5

#===================================================================
# NOTES ABOUT THIS DUMP GROUP:
#   (1) time window radius is -3.00 to +2.99 hours on all types 
#   (2) TIME TRIMMING IS NOT DONE IN THIS DUMP
#
#--------------------------------------------------------------------------
# Dump # 5 : MSONET: 30 subtype(s)
#            ------------------------
#            TOTAL NUMBER OF SUBTYPES = 30
#
#===================================================================

#IG
DTIM_earliest_msonet=${DTIM_latest_msonet:-"-1.99"}
DTIM_latest_msonet=${DTIM_latest_msonet:-"+2.00"}

#DTIM_latest_msonet=${DTIM_latest_msonet:-"+2.99"}

export SKIP_255031=YES  # Skip for port to Dell since no new data allowed.
export SKIP_255101=YES  # Also, b/c CDAS has not tested these providers. 

TIME_TRIM=on
#TIME_TRIM=off

$ushscript_dump/bufr_dump_obs.sh $dumptime 3.0 1 msonet
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

export STATUS=NO
export DUMP_NUMBER=6

#===================================================================
# NOTES ABOUT THIS DUMP GROUP:
#   (1) time window radius is -3.00 to +2.99 hours on all types
#   (2) TIME TRIMMING IS NOT DONE IN THIS DUMP
#
#--------------------------------------------------------------------------
# Dump # 6 : NEXRAD: 8 subtype(s)
#            -----------------------
#            TOTAL NUMBER OF SUBTYPES = 8
#
#===================================================================

DTIM_latest_nexrad=${DTIM_latest_nexrad:-"+2.99"}

TIME_TRIM=off

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

if [ $cycp -eq 00 ]; then   # (22.5 - 01.5 Z)
   unset SKIP_006032 # radial wind  22Z
   unset SKIP_006033 # radial wind  23Z
   unset SKIP_006010 # radial wind  00Z
   unset SKIP_006011 # radial wind  01Z
   unset SKIP_006062 # reflectivity 22Z
   unset SKIP_006063 # reflectivity 23Z
   unset SKIP_006040 # reflectivity 00Z
   unset SKIP_006041 # reflectivity 01Z
elif [ $cycp -eq 06 ]; then # (04.5 - 07.5 Z)
   unset SKIP_006014 # radial wind  04Z
   unset SKIP_006015 # radial wind  05Z
   unset SKIP_006016 # radial wind  06Z
   unset SKIP_006017 # radial wind  07Z
   unset SKIP_006044 # reflectivity 04Z
   unset SKIP_006045 # reflectivity 05Z
   unset SKIP_006046 # reflectivity 06Z
   unset SKIP_006047 # reflectivity 07Z
elif [ $cycp -eq 12 ]; then # (10.5 - 13.5 Z)
   unset SKIP_006020 # radial wind  10Z
   unset SKIP_006021 # radial wind  11Z
   unset SKIP_006022 # radial wind  12Z
   unset SKIP_006023 # radial wind  13Z
   unset SKIP_006050 # reflectivity 10Z
   unset SKIP_006051 # reflectivity 11Z
   unset SKIP_006052 # reflectivity 12Z
   unset SKIP_006053 # reflectivity 13Z
elif [ $cycp -eq 18 ]; then # (16.5 - 19.5 Z)
   unset SKIP_006026 # radial wind  16Z
   unset SKIP_006027 # radial wind  17Z
   unset SKIP_006028 # radial wind  18Z
   unset SKIP_006029 # radial wind  19Z
   unset SKIP_006056 # reflectivity 16Z
   unset SKIP_006057 # reflectivity 17Z
   unset SKIP_006058 # reflectivity 18Z
   unset SKIP_006059 # reflectivity 19Z
fi

$ushscript_dump/bufr_dump_obs.sh $dumptime 3.0 1 nexrad
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
#------------------------------------------------------------------------------
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

#=========================================================================
# NOTES ABOUT THIS DUMP GROUP:
#   (1) time window radius is -3.00 to +2.99 hours on all types
#   (2) TIME TRIMMING IS NOT DONE IN THIS DUMP
#
#--------------------------------------------------------------------------
# Dump # 7 : OMI:    1 subtype(s)
#            SSMISU: 1 subtype(s)
#            SEVCSR: 1 subtype(s)
#            BATHY:  1 subtype(s)
#            TESAC:  1 subtype(s)
#            TRKOB:  1 subtype(s)
#            ATMS:   1 subtype(s) (if present in past 10 days of tanks)
#            CRIS:   1 subtype(s) (if present in past 10 days of tanks)
#            MLS:    1 subtype(s) (if present in past 10 days of tanks)
#            ----------------------
#            TOTAL NUMBER OF SUBTYPES = 9 
#
#=========================================================================

DTIM_latest_omi=${DTIM_latest_omi:-"+2.99"}
DTIM_latest_ssmisu=${DTIM_latest_ssmisu:-"+2.99"}
#-----------------------------------------------
DTIM_latest_sevcsr=${DTIM_latest_sevcsr:-"+2.99"}
DTIM_latest_bathy=${DTIM_latest_bathy:-"+2.99"}
DTIM_latest_tesac=${DTIM_latest_tesac:-"+2.99"}
DTIM_latest_trkob=${DTIM_latest_trkob:-"+2.99"}
#-----------------------------------------------
# check for atms tank presence in past 10 days
atms=""
err_check_tanks=0
sh $USHbufr_dump/check_tanks.sh atms
err_check_tanks=$?
if [ $err_check_tanks -eq 0 ];then
   atms=atms
   DTIM_latest_atms=${DTIM_latest_atms:-"+2.99"}
fi
#-----------------------------------------------
# check for cris tank presence in past 10 days
cris=""
err_check_tanks=0
sh $USHbufr_dump/check_tanks.sh cris
err_check_tanks=$?
if [ $err_check_tanks -eq 0 ];then
   cris=cris
   DTIM_latest_cris=${DTIM_latest_cris:-"+2.99"}
fi
#-----------------------------------------------
# check for mls tank presence in past 10 days
mls=""
err_check_tanks=0
sh $USHbufr_dump/check_tanks.sh mls
err_check_tanks=$?
if [ $err_check_tanks -eq 0 ];then
   mls=mls
   DTIM_latest_mls=${DTIM_latest_mls:-"+2.99"}
fi
#-----------------------------------------------

TIME_TRIM=off

$ushscript_dump/bufr_dump_obs.sh $dumptime 3.0 1 omi ssmisu sevcsr \
 bathy tesac trkob $atms $cris $mls
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

export STATUS=NO
export DUMP_NUMBER=8

#=======================================================================
# NOTES ABOUT THIS DUMP GROUP:
#   (1) time window radius is +/- 1.5 hrs for all SATWND types
#       EXCEPT: SATWND subtypes 005/010, 005/011 and 005/12, 005/019, 005/064,
#               005/065, 005/066, 005/067, 005/068, 005/069 005/070, 005/071, 005/080 and 005/091 where
#               it is -3.00 to +2.99 hours.
#   (2) TIME TRIMMING IS DONE IN THIS DUMP
#
#--------------------------------------------------------------------------
# Dump # 8 : SATWND: 17 subtype(s)
#            ------------------------ 
#            TOTAL NUMBER OF SUBTYPES = 74
#
#=======================================================================

# Skip legacy EUMETSAT AMV subsets; for testing, skipped in trigger or version file
#export SKIP_005064=YES
#export SKIP_005065=YES
#export SKIP_005066=YES

# satwnd types
# ------------
DTIM_earliest_005010=${DTIM_earliest_005010:-"-3.00"}
DTIM_latest_005010=${DTIM_latest_005010:-"+2.99"}
DTIM_earliest_005011=${DTIM_earliest_005011:-"-3.00"}
DTIM_latest_005011=${DTIM_latest_005011:-"+2.99"}
DTIM_earliest_005012=${DTIM_earliest_005012:-"-3.00"}
DTIM_latest_005012=${DTIM_latest_005012:-"+2.99"}
DTIM_earliest_005019=${DTIM_earliest_005019:-"-3.00"}
DTIM_latest_005019=${DTIM_latest_005019:-"+2.99"}

DTIM_earliest_005064=${DTIM_earliest_005064:-"-3.00"}
DTIM_latest_005064=${DTIM_latest_005064:-"+2.99"}
DTIM_earliest_005065=${DTIM_earliest_005065:-"-3.00"}
DTIM_latest_005065=${DTIM_latest_005065:-"+2.99"}
DTIM_earliest_005066=${DTIM_earliest_005066:-"-3.00"}
DTIM_latest_005066=${DTIM_latest_005066:-"+2.99"}

DTIM_earliest_005067=${DTIM_earliest_005067:-"-3.00"}
DTIM_latest_005067=${DTIM_latest_005067:-"+2.99"}
DTIM_earliest_005068=${DTIM_earliest_005068:-"-3.00"}
DTIM_latest_005068=${DTIM_latest_005068:-"+2.99"}
DTIM_earliest_005069=${DTIM_earliest_005069:-"-3.00"}
DTIM_latest_005069=${DTIM_latest_005069:-"+2.99"}

DTIM_earliest_005070=${DTIM_earliest_005070:-"-3.00"}
DTIM_latest_005070=${DTIM_latest_005070:-"+2.99"}
DTIM_earliest_005071=${DTIM_earliest_005071:-"-3.00"}
DTIM_latest_005071=${DTIM_latest_005071:-"+2.99"}
DTIM_earliest_005080=${DTIM_earliest_005080:-"-3.00"}
DTIM_latest_005080=${DTIM_latest_005080:-"+2.99"}
DTIM_earliest_005081=${DTIM_earliest_005081:-"-1.50"}
DTIM_latest_005081=${DTIM_latest_005081:-"+1.49"}
DTIM_earliest_005091=${DTIM_earliest_005091:-"-3.00"}
DTIM_latest_005091=${DTIM_latest_005091:-"+2.99"}

$ushscript_dump/bufr_dump_obs.sh $dumptime 1.5 1 satwnd
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

export STATUS=NO
export DUMP_NUMBER=9

#=======================================================================
# NOTES ABOUT THIS DUMP GROUP:
#   (1) time window radius is -3.00 to +2.99 hours on all types
#       EXCEPT:  GEOIMR where it is -0.50 to +0.50 hour
#   (2) TIME TRIMMING IS DONE IN THIS DUMP
#
#--------------------------------------------------------------------------
# Dump # 9 : GEOIMR: 1 subtype(s)
#            SUBPFL: 1 subtype(s)
#            SALDRN: 1 subtype(s)
#            SNOCVR: 1 subtype(s)
#            GMI1CR: 1 subtype(s)
#            ------------------------ 
#            TOTAL NUMBER OF SUBTYPES = 5
#
#=======================================================================

DTIM_earliest_geoimr=${DTIM_earliest_geoimr:-"-0.50"}
DTIM_latest_geoimr=${DTIM_latest_geoimr:-"+0.50"}
DTIM_earliest_subpfl=${DTIM_earliest_subpfl:-"-3.00"}
DTIM_latest_subpfl=${DTIM_latest_subpfl:-"+2.99"}
DTIM_earliest_saldrn=${DTIM_earliest_saldrn:-"-3.00"}
DTIM_latest_saldrn=${DTIM_latest_saldrn:-"+2.99"}
DTIM_earliest_snocvr=${DTIM_earliest_snocvr:-"-3.00"}
DTIM_latest_snocvr=${DTIM_latest_snocvr:-"+2.99"}
DTIM_earliest_gmi1cr=${DTIM_earliest_gmi1cr:-"-3.00"}
DTIM_latest_gmi1cr=${DTIM_latest_gmi1cr:-"+2.99"}

$ushscript_dump/bufr_dump_obs.sh $dumptime 3.0 1 geoimr subpfl saldrn snocvr gmi1cr
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

#===================================================================
# NOTES ABOUT THIS DUMP GROUP:
#   (1) time window radius is -3.00 to +2.99 hours on all types 
#   (2) TIME TRIMMING IS NOT DONE IN THIS DUMP
#
#--------------------------------------------------------------------------
# Dump # 10 : MSONET: 1 subtype(s) 255/030
#            ------------------------
#            TOTAL NUMBER OF SUBTYPES = 1
#
#===================================================================

DTIM_earliest_msone1=${DTIM_latest_msone1:-"-1.99"}
DTIM_latest_msone1=${DTIM_latest_msone1:-"+2.00"}

TIME_TRIM=on #off

$ushscript_dump/bufr_dump_obs.sh $dumptime 3.0 1 msone1
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

#----------------------------------------------------------------
# Now launch the threads

# determine local system name and type if available
# -------------------------------------------------
SITE=${SITE:-""}

launcher=${launcher:-"cfp"}  # if not "cfp", threads will run serially

if [ "$launcher" = cfp ]; then
   set -u
   > $DATA/poe.cmdfile

# To better take advantage of cfp, execute the longer running commands first.
# Some reordering was done here based on recent sample runtimes.
   [ $DUMP_group5 = YES ]  &&  echo ./thread_5 >> $DATA/poe.cmdfile  # moved up
   [ $DUMP_group1 = YES ]  &&  echo ./thread_1 >> $DATA/poe.cmdfile
   [ $DUMP_group8 = YES ]  &&  echo ./thread_8 >> $DATA/poe.cmdfile  # moved up
   [ $DUMP_group6 = YES ]  &&  echo ./thread_6 >> $DATA/poe.cmdfile  # moved up
   [ $DUMP_group2 = YES ]  &&  echo ./thread_2 >> $DATA/poe.cmdfile
   [ $DUMP_group3 = YES ]  &&  echo ./thread_3 >> $DATA/poe.cmdfile
   [ $DUMP_group4 = YES ]  &&  echo ./thread_4 >> $DATA/poe.cmdfile
   [ $DUMP_group7 = YES ]  &&  echo ./thread_7 >> $DATA/poe.cmdfile
   [ $DUMP_group9 = YES ]  &&  echo ./thread_9 >> $DATA/poe.cmdfile
   [ $DUMP_group10 = YES ] &&  echo ./thread_10 >> $DATA/poe.cmdfile #msone1

   if [ -s $DATA/poe.cmdfile ]; then
      export MP_CSS_INTERRUPT=yes  # ??
      launcher_DUMP=${launcher_DUMP:-mpiexec}
      #$launcher_DUMP -np 10 --cpu-bind verbose,core cfp $DATA/poe.cmdfile
      NPROCS=${NPROCS:-10}
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
  echo "Running threads serially"
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
fi

cat $DATA/1.out $DATA/2.out $DATA/3.out $DATA/4.out $DATA/5.out $DATA/6.out $DATA/7.out $DATA/8.out $DATA/9.out $DATA/10.out

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
#===============================================================================

export STATUS=YES
export DUMP_NUMBER=11
$ushscript_dump/bufr_dump_obs.sh $dumptime 3.00 1 null

   if [ "$SENDCOM" = 'YES' -a "$COPY_TO_ARKV" = 'YES' ]; then

#=====================================================================
#=====================================================================
#  Copy dump files to arkv directory, preserving ownership & group id
#   but updating timestamp
#=====================================================================
#=====================================================================

      msg="Copy dump files to $COMARC"
      $DATA/postmsg "$jlogfile" "$msg"

      set +x
      for fil in `ls $COMOUT/cdas.${cycle}*bufr_d`; do
         set -x
         nfil="`basename $fil | awk -F. '{print $3}'`.${PDY}${cyc}.bufr_d"
         cp --preserve=mode,ownership $fil ${COMARC}/${nfil}
      done

#     endif loop $COPY_TO_ARKV
   fi


#  endif loop $PROCESS_DUMP
fi

echo " " >> $pgmout
echo "##################################################################\
####################"  >> $pgmout
echo " " >> $pgmout

#================================================================
#================================================================



if [ "$PROCESS_DUMP" = 'YES' ]; then

   if [ "$err1" -gt '5' -o "$err2" -gt '5' -o "$err3" -gt '5' -o \
        "$err4" -gt '5' -o "$err5" -gt '5' -o "$err6" -gt '5' -o \
        "$err7" -gt '5' -o "$err8" -gt '5' -o "$err9" -gt '5' -o \
        "$err10" -gt '5' ]; then
      for n in $err1 $err2 $err3 $err4 $err5 $err6 $err7 $err8 $err9 $err10
      do
         if [ "$n" -gt '5' ]; then
            if [ "$n" -ne '11' -a "$n" -ne '22' ]; then
       
## fatal error in dumping of BUFR obs. files
       
               set +x
echo
echo " ###################################################### "
echo " --> > 22 RETURN CODE FROM DATA DUMP, $err1, $err2, $err3, $err4, \
$err5, $err6, $err7, $err8, $err9 $err10"
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
$err5, $err6, $err7, $err8, $err9 $err10 "
      echo " --> NOT ALL DATA DUMP FILES ARE COMPLETE - CONTINUE    "
      echo " ###################################################### "
      echo
      set -x
   fi

#  endif loop $PROCESS_DUMP
fi

##  concatenate msonet and msone1, b/c prepobs only wants one file
#cat ${COMSP}msone1.tm00.bufr_d >> ${COMSP}msonet.tm00.bufr_d

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
cat  break $pgmout break > allout
cat allout
# rm allout

sleep 10

msg='ENDED NORMALLY.'
$DATA/postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
