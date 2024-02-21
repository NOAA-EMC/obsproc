#!/bin/bash
#####################################################################
echo "----------------------------------------------------------    "
echo "exrtma_dump.sh - RTMA model data dump processing              "
echo "----------------------------------------------------------    "
echo "History:                                                      "
echo " May 19 2006 - Original script.                               "
echo " Sep 10 2014 - Use parallel scripting to process dump groups. "
echo " Feb  3 2015 - Dump new satwnd types NC005019, NC005080,      "
echo "                NC005090. Dump new type efclam.               "
echo " Jul  2 2017 - enabled potential for minute-precison cycles   "
echo " Jun 29 2018 - removed efclam dumps in rtma_ru runs           "
echo " May 23 2019 - Updated to run on Dell-p3. Disabled background "
echo "                threading.                                    "
echo " Oct 22 2019 - Temporary bug fix to limit adpsfc processing to"
echo "                previous day's tank for rtma_ru 0000z cylce.  "
echo " Feb 13 2020 - enabled efclam dumps in rtma_ru runs           "
echo "             - disabled (SKIP) NC005090, NC005091 (VIIRS AMVs)"
echo " Sep 14 2020 - Updated Dump group #1 to skip legacy           "
echo "                Meteosat AMV subsets: 005064, 005065,         "
echo "                006066.  These subsets are replaced by        "
echo "                new WMO BUFR sequence subsets: 005067,        "
echo "                005068, 005069. The format change from        "
echo "                EUMETSAT occurs Oct 6, 2020.                  "
echo " Mar 09 2021 - Incremented subsets for the sfcshp dump groups "
echo "                to match bufr_dumplist. Removed tideg from    "
echo "                sfcshp dump group to make unique dump file.   "
echo "             - Copy bufr_dumplist to COMOUT.                  "
echo " Dec 15 2021 - set for use on WCOSS2.                         "
echo " Jul 31 2022 - Subpfl,saldrn,snocvr & gmi1cr dump group added "
echo " Dec 07 2022 - Re-Added in Dumps 3 through 11 to match a      "
echo "                previous version of 3drtma available on       "
echo "		      WCOSS1. Also removed saphir from Group11.     "
echo " Jun 16 2023 - Adjusted timings to match URMA. Also added     "
echo "               SATMAR data.                                   "
echo " Feb 16 2024 - Adjusted dump windows based on RTMA/URMA team  "
echo "               feedback. Added SATWHR to new dump group 12    "
#####################################################################

set -x

# Make sure we are in the $DATA directory
cd $DATA

msg="HAS BEGUN on `hostname`"
postmsg "$jlogfile" "$msg"
 
cat break > $pgmout

# Imported variable cycM, if it exists, contains cycle time minutes
#  If cycM is imported, set dumptime w/ fractional hour based on value of cycM,
#   and reset cycle to 4-digit value t<HHMM>z

hr_fraction=""
if [ -n "$cycM" ]; then
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
  cycle=t${cyc}${cycM}z
fi

export dumptime=`cut -c7-16 ncepdate`$hr_fraction

tmmark_uc=$(echo $tmmark | tr [a-z] [A-Z])

msg="RTMA ANALYSIS TIME IS $PDY$cyc"
[ -n "$cycM" ]  &&  msg="$msg:${cycM}"
postmsg "$jlogfile" "$msg"

set +x
echo
echo "CENTER DATA DUMP DATE-TIME FOR $tmmark_uc RTMA IS $dumptime"
echo
set -x
 
export COMSP=$COMOUT/$RUN.${cycle}.
 
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
if [ "$PROCESS_DUMP" = 'YES' ]; then

###################################
###################################
#  The data "dump" script for tm00
###################################
###################################

msg="START THE $tmmark_uc RTMA DATA DUMP CENTERED ON $dumptime"
postmsg "$jlogfile" "$msg"

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

#========================================================================
# Dump # 1 : ASCATT, SATWND*, EFCLAM, SNOCVR, GMI1CR --
#              (1)    (14)     (1)    (1)     (1)
#                                  TOTAL NUMB OF SUBTYPES = 18
# ===> Dumping of WNDSAT removed from here until new ingest feed is established
#      (had been dumped with a time window radius of -6.00 to 0.00 hours)
#
#     * VIIRS NPP/NOAA-20 IR lg-wave cld motion data disabled (GSI only
#     monitors nc005090 and does not support nc005091).
#
#            time window radius is -6.00 to 0.00 hours for ASCATT
#            for SATWND subtypes 005/010, 005/011, 005/012 and 005/019 at all
#            times:
#              time window radius is -1.00 to -0.01 hours
#            for SATWND subtypes 005/064, 005/065, 005/066, 005/067, 005/068
#            and 005/069 at all times:
#              time window radius is -1.50 to +1.49 hours
#            for all other SATWND subtypes:
#              time window radius is +/- 2.5 hours
#            for EFCLAM:
#              time window radius is +/- 0.5 hours
#=======================================================================

# Skip all Indian satellite winds in SATWND (not in domain)

export SKIP_005021=YES
export SKIP_005022=YES
export SKIP_005023=YES

# Skip legacy EUMETSAT AMV subsets; for testing skip in trigger or version file
#export SKIP_005064=YES
#export SKIP_005065=YES
#export SKIP_005066=YES

# Skip VIIRS NPP/NOAA-20 IR long wave derived cld motion

export SKIP_005090=YES        # old sequence (discontinued)
export SKIP_005091=YES        # new sequence (unsupported in GSI)

DTIM_earliest_005010=-1.00
DTIM_latest_005010=-0.01
DTIM_earliest_005011=-1.00
DTIM_latest_005011=-0.01
DTIM_earliest_005012=-1.00
DTIM_latest_005012=-0.01
DTIM_earliest_005019=-1.00
DTIM_latest_005019=-0.01

DTIM_earliest_005064=-1.50
DTIM_latest_005064=+1.49
DTIM_earliest_005065=-1.50
DTIM_latest_005065=+1.49
DTIM_earliest_005066=-1.50
DTIM_latest_005066=+1.49

DTIM_earliest_005067=-1.50
DTIM_latest_005067=+1.49
DTIM_earliest_005068=-1.50
DTIM_latest_005068=+1.49
DTIM_earliest_005069=-1.50
DTIM_latest_005069=+1.49

DTIM_earliest_ascatt=-3.00
DTIM_latest_ascatt=3.00

DTIM_earliest_efclam=-0.50
DTIM_latest_efclam=+0.50

DTIM_earliest_snocvr=${DTIM_earliest_snocvr:-"-2.00"}
DTIM_latest_snocvr=${DTIM_latest_snocvr:-"+2.00"}
#DTIM_earliest_snocvr=-1.50
#DTIM_latest_snocvr=+1.49

DTIM_earliest_gmi1cr=-3.00
DTIM_latest_gmi1cr=+2.99

DTIM_earliest_005081=${DTIM_earliest_005081:-"-1.50"}
DTIM_latest_005081=${DTIM_latest_005081:-"+1.49"}
#DTIM_earliest_005081=-1.50
#DTIM_latest_005081=+1.49

$ushscript_dump/bufr_dump_obs.sh $dumptime 1.5 1 ascatt satwnd efclam snocvr gmi1cr
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

#========================================================================
# Dump # 2 : SFCSHP, ADPSFC, TIDEG, SUBPFL, SALDRN
#              (11)     (6)   (1)   (1)     (1)
#            -- TOTAL NUMBER OF SUBTYPES = 20
#            time window radius is +/- 0.50 hours for SFCSHP and ADPSFC
#=======================================================================

# Skip mobile synoptic reports in ADPSFC (not in domain)

export SKIP_000002=YES

DTIM_earliest_subpfl=-1.50
DTIM_latest_subpfl=+1.50
DTIM_earliest_saldrn=-0.50
DTIM_latest_saldrn=+0.50

# for rtma_ru_0000 read only from previous day's tank
# Temporary bug fix
if [ $RUN = "rtma_ru" -a $cycle = "t0000z" ]; then
  DTIM_latest_000000=-0.01
fi

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 sfcshp tideg adpsfc subpfl saldrn
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

#===========================================================================
# Dump # 3 : MSONET -- TOTAL NUMBER OF SUBTYPES = 30
#            time window radius is 0.50 hours
#===========================================================================

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 msonet
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

#========================================================================
# Dump # 4 : METARH
#              (1)
#            -- TOTAL NUMBER OF SUBTYPES = 1
#            time window radius is +/- 0.50 hours for METARH
#=======================================================================

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 metarh
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

#==========================================================================
# Dump # 5 : VADWND,  ...    ADPUPA
#              (2)             (6)
#            -- TOTAL NUMBER OF SUBTYPES = 8
#==========================================================================
# Time window -1.00 to +1.00 hours for ADPUPA for full and partial cycle runs
#  (note: time window increased over +/- 0.5 hr standard to get more data)
DTIM_earliest_adpupa=${DTIM_earliest_adpupa:-"-1.50"}
DTIM_latest_adpupa=${DTIM_latest_adpupa:-"+1.50"}

def_time_window_5=2.5  # default time window for dump 5 is -2.5 to +2.5
                       # hours for all other cycles

# Time window is -1.00 to +1.00 hours for VADWND
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_earliest_vadwnd=${DTIM_earliest_vadwnd:-"-1.50"}
   DTIM_latest_vadwnd=${DTIM_latest_vadwnd:-"+1.50"}

# Time window is -1.50 to +1.50 hours for NON-EUMETSAT & NON-GOES SATWND at 00
#  or 12z (default)
# Time window is -2.50 to +2.50 hours for NON-EUMETSAT & NON-GOES SATWND at
#  all other cycles (default)

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_5} 1 vadwnd adpupa
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

#========================================================================
# Dump # 6 : PROFLR, RASSDA, SATMAR     ...     ...
#              (3)     (1)
#            -- TOTAL NUMBER OF SUBTYPES = 4
#
#========================================================================
# Skip Japanese profiler reports in PROFLR (not in domain)
export SKIP_002013=YES

def_time_window_6=1.5 # default time window for dump 5 is -1.5 to +1.5 hours

# Time window -1.50 to +1.50 hours for PROFLR for full and partial cycle runs
#  (note: time window increased over +/- 0.5 hr standard to improve
#         PREPOBS_PROFCQC performance, it will be winnowed back down to
#         -0.50 to +0.50 hours in output from PREPOBS_PROFCQC)
DTIM_earliest_proflr=${DTIM_earliest_proflr:-"-1.50"}
DTIM_latest_proflr=${DTIM_latest_proflr:-"+1.50"}

DTIM_earliest_satmar=${DTIM_earliest_satmar:-"-3.00"}
DTIM_latest_satmar=${DTIM_latest_satmar:-"+3.00"}

# Time window -0.50 to +0.50 hours for RASSDA   ...     ...   for full and
#  partial cycle runs (default)

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_6} 1 proflr rassda satmar
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

export STATUS=NO
export DUMP_NUMBER=7

#============================================================================
# Dump # 7 :  ...    GPSIPW -- TOTAL NUMBER OF SUBTYPES = 1
#                      (1)
#============================================================================
def_time_window_7=0.5 # default time window for dump 6 is -0.5 to +0.5 hours

# Time window -1.05 to -0.95 hours (-63 to -57 min) for GPSIPW
DTIM_earliest_gpsipw=${DTIM_earliest_gpsipw:-"-1.05"}
DTIM_latest_gpsipw=${DTIM_latest_gpsipw:-"-0.95"}
#  {note: new Ground Based GPS-IPW/ZTD (from U.S.-ENI and foreign GNSS
#         providers) is currently limited to obs closest to cycle-time that
#         result in a U.S.-ENI dump count that is not too much larger than that
#         from the previous U.S. (only) GSD-feed, since the ENI reports are
#         available every 5 min while the GSD reports were available only every
#         30 min. Also accounts for an approximate 80-min latency present in
#         the U.S.-ENI reports.}

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_7} 1 gpsipw
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

#===========================================================================
# Dump # 8 : AIRCFT, AIRCAR, GOESND -- TOTAL NUMBER OF SUBTYPES = 12
#              (8)     (2)     (2)
#===========================================================================
export LALO=0  # GLOBAL dumps here (AIRCFT and AIRCAR dumped globally to
               # improve PREPOBS_PREPACQC track-check performance; GOESND
               # dumped globally to allow job to run much quicker w/o the need
               # for geographical filtering (all GOES reports are in expanded
               # NAM domain anyway)

def_time_window_8=0.75 # default time window for dump 7 is -0.75 to +0.75 hours

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

#  ===> For RUN = rap, rap_e -- full cycle runs (including early at 00/12z)
#       -------------------------------------------------------------------
# Time window is -1.00 to +1.00 hours for GOESND soundings/radiances (only)
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_earliest_003003=${DTIM_earliest_003003:-"-1.00"}
   DTIM_latest_003003=${DTIM_latest_003003:-"+1.00"}

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_8} 1 aircft \
 aircar
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

#===============================================================================
# Dump # 9 : 1BAMUA, 1BMHS,  ESAMUA, ESMHS, ATMS, MTIASI, SEVCSR, GPSRO,
#              (1)    (1)      (1)     (1)   (1)    (1)    (1)     (1)
#            ESIASI, IASIDB, ESATMS, ATMSDB, SEVASR, AMSR2
#              (1)    (1)     (1)     (1)     (1)     (1)
#             TOTAL NUMBER OF SUBTYPES = 16
#===============================================================================
#  ===> For RUN = rap, rap_e -- full cycle runs (including early at 00/12z)
#       -------------------------------------------------------------------
   def_time_window_9=3.0 # default time window for dump 9 is -3.0 to +3.0 hours

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

# Time window is -2.00 to +1.99 hours for SEVCSR, SEVASR, GPSRO
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

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_9} 1 1bamua \
   1bmhs esamua esmhs atms mtiasi sevcsr gpsro esiasi iasidb esatms \
   atmsdb sevasr amsr2
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

#===========================================================================
# Dump # 10 : NEXRAD -- TOTAL NUMBER OF SUBTYPES = 4
#              (4)
#===========================================================================
export LALO=0  # GLOBAL dumps here (NEXRAD dumped globally to allow job to run
               # much quicker w/o the need for geographical filtering (all
               # radar reports are over CONUS anyway)

def_time_window_10=1.5 # default time window for dump 10 is -0.5 to +0.5 hours

# Time window -0.50 to +0.49 hours for NEXRAD for full and partial cycle runs
#DTIM_latest_nexrad=${DTIM_latest_nexrad:-"+0.49"}         # earliest is default
# NEXRAD tanks are hourly
# Process only those hourly tanks w/i requested dump center cycle time window
# !!! this needs adjusting for 15min cycling RTMA_RU -JW !!!
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

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_10} 1 nexrad
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

#==========================================================================
# Dump # 11 : 1BHRS4, AIRSEV, LGHTNG, ESHRS3, LGYCLD, CRISF4, SSMISU, OSBUV8,
#               (1)     (1)     (2)     (1)     (1)    (1)     (1)     (1)
#             CRSFDB
#               (1) 
#             TOTAL NUMBER OF SUBTYPES = 11
#=========================================================================
# Time window -1.00 to +0.50 hours for LGHTNG for all cycle runs
   DTIM_earliest_lghtng=${DTIM_earliest_lghtng:-"-1.50"}
   DTIM_latest_lghtng=${DTIM_latest_lghtng:-"+1.50"}

# Time window -0.50 to +0.50 hours for LGYCLD for all cycle runs
   DTIM_earliest_lgycld=${DTIM_earliest_lgycld:-"-1.50"}
   DTIM_latest_lgycld=${DTIM_latest_lgycld:-"+1.50"}
   
   def_time_window_11=3.0 # default time window for dump 11 is -3.0 to +3.0 hours

# Time window is -3.00 to +2.99 hours for 1BHRS4
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_latest_1bhrs4=${DTIM_latest_1bhrs4:-"+2.99"}      # earliest is default

# Time window is -3.00 to +2.99 hours for AIRSEV
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_latest_airsev=${DTIM_latest_airsev:-"+2.99"}      # earliest is default

# Time window is -2.00 to +1.99 hours for CRISF4, CRSFDB
#  (note: time window increased over +/- 0.5 hr standard to get more data)
   DTIM_earliest_crisf4=${DTIM_earliest_crisf4:-"-2.00"}
   DTIM_latest_crisf4=${DTIM_latest_crisf4:-"+1.99"}
   DTIM_earliest_crsfdb=${DTIM_earliest_crsfdb:-"-2.00"}
   DTIM_latest_crsfdb=${DTIM_latest_crsfdb:-"+1.99"}

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

$ushscript_dump/bufr_dump_obs.sh $dumptime ${def_time_window_11} 1 1bhrs4 \
           airsev lghtng eshrs3 lgycld crisf4 ssmisu osbuv8 crsfdb
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

export STATUS=NO
export DUMP_NUMBER=12

#===========================================================================
# Dump # 12 : SATWHR
#            time window radius is 1.00 hour
#===========================================================================

DTIM_earliest_satwhr=${DTIM_earliest_satwhr:-"-1.00"}
DTIM_latest_satwhr=${DTIM_latest_satwhr:-"+1.00"}

$ushscript_dump/bufr_dump_obs.sh $dumptime 1.0 1 satwhr
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
   echo ./thread_3 >> $DATA/poe.cmdfile  # moved up
   echo ./thread_1 >> $DATA/poe.cmdfile
   echo ./thread_2 >> $DATA/poe.cmdfile
   echo ./thread_4 >> $DATA/poe.cmdfile
   echo ./thread_5 >> $DATA/poe.cmdfile
   echo ./thread_6 >> $DATA/poe.cmdfile
   echo ./thread_7 >> $DATA/poe.cmdfile
   echo ./thread_8 >> $DATA/poe.cmdfile
   echo ./thread_9 >> $DATA/poe.cmdfile
   echo ./thread_10 >> $DATA/poe.cmdfile
   echo ./thread_11 >> $DATA/poe.cmdfile #other btemps, radiances
   echo ./thread_12 >> $DATA/poe.cmdfile #satwhr

   if [ -s $DATA/poe.cmdfile ]; then
      export MP_CSS_INTERRUPT=yes  # ??
      launcher_DUMP=${launcher_DUMP:-mpiexec}
      $launcher_DUMP -np 12 --cpu-bind verbose,core cfp $DATA/poe.cmdfile
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
   echo "Running serial threads"
   ./thread_1
   ./thread_2
   ./thread_3
   ./thread_4
   ./thread_5
   ./thread_6
   ./thread_7
   ./thread_8
   ./thread_9
   ./thread_10
   ./thread_11
   ./thread_12
#  wait
fi

cat $DATA/1.out $DATA/2.out $DATA/3.out $DATA/4.out $DATA/5.out $DATA/6.out \
    $DATA/7.out $DATA/8.out $DATA/9.out $DATA/10.out $DATA/11.out $DATA/12.out

set +x
echo " "
echo " "
set -x

err1=`cat $DATA/error1`
err2=`cat $DATA/error2`
err3=`cat $DATA/error3`
err4=`cat $DATA/error4`
err5=`cat $DATA/error5`
err6=`cat $DATA/error6`
err7=`cat $DATA/error7`
err8=`cat $DATA/error8`
err9=`cat $DATA/error9`
err10=`cat $DATA/error10`
err11=`cat $DATA/error11`
err12=`cat $DATA/error12`

#================================================================

export STATUS=YES
export DUMP_NUMBER=13
$ushscript_dump/bufr_dump_obs.sh $dumptime 3.00 1 null


#  endif loop $PROCESS_DUMP
fi

#================================================================
#================================================================


if [ "$PROCESS_DUMP" = 'YES' ]; then

   if [ "$err1" -gt '5' -o "$err2" -gt '5' -o "$err3" -gt '5' \
     -o "$err4" -gt '5' -o "$err5" -gt '5' -o "$err6" -gt '5' \
     -o "$err7" -gt '5' -o "$err8" -gt '5' -o "$err9" -gt '5' \
     -o "$err10" -gt '5' -o "$err11" -gt '5' -o "$err12" -gt '5'] ; then
      for n in $err1 $err2 $err3 $err4 $err5 $err6 $err7 $err8 $err9 $err10 $err11 $err12
      do
         if [ "$n" -gt '5' ]; then
            if [ "$n" -ne '11' -a "$n" -ne '22' ]; then

## fatal error in dumping of BUFR obs. files

               set +x
echo
echo " ###################################################### "
echo " --> > 22 RETURN CODE FROM DATA DUMP, \
$err1, $err2, $err3, $err4, $err5, $err6, $err7, $err8, $err9, $err10, $err11, $err12"
echo " --> @@ F A T A L   E R R O R @@   --  ABNORMAL EXIT    "
echo " ###################################################### "
echo
               set -x
               err_exit
               exit 9
            fi
         fi
      done

## a status code of 11 or 22 from dumping of BUFR obs. files
## is non-fatal but still worth noting

      set +x
      echo
      echo " ###################################################### "
      echo " --> > 5 RETURN CODE FROM DATA DUMP, \
$err1, $err2, $err3, $err4, $err5, $err6, $err7, $err8, $err9, $err10, $err11, $err12"
      echo " --> NOT ALL DATA DUMP FILES ARE COMPLETE - CONTINUE    "
      echo " ###################################################### "
      echo
      set -x
   fi

#  endif loop $PROCESS_DUMP
fi

if [ "$RUN" == "rtma_ru" ] && [ "${SENDDBN^^}" = YES ] && [ -s ${COMSP}satwnd.tm00.bufr_d ]; then
   $DBNROOT/bin/dbn_alert MODEL RTMA_RU_BUFR_satwnd $job ${COMSP}satwnd.tm00.bufr_d
fi

#
# copy bufr_dumplist to $COMOUT per NCO SPA request
# -------------------------------------------------
echo "Copy bufr_dumplist to comout"
if [ $RUN = 'rtma_ru' ]; then
   LIST_cp=$COMOUT/${RUN}.t${cyc}${cycM}z.bufr_dumplist.${tmmark}
else
   LIST_cp=$COMOUT/${RUN}.t${cyc}z.bufr_dumplist.${tmmark}
fi
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
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
