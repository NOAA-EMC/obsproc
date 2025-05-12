#!/bin/ksh
# Run under ksh
############################################################################
echo "-------------------------------------------------------------------- "
echo "exdump_monitor.sh - Data dump monitor processing                 "
echo "-------------------------------------------------------------------- "
echo "History: Jan 03 2001 - Original script.                              "
echo "         Sep 10 2014 - Use parallel scripting to process dump groups."
echo "         Feb  3 2015 - Dump new satwnd types NC005019, NC005080,     "
echo "                       NC005090. Dump new type efclam. Removed dump  "
echo "                       of "trmm".                                    "
echo "         Aug 16 2016 - GPSIPW dump window reset for new data stream. "
echo "         Jan 12 2017 - Dump new satellite data types.                "
echo "         Nov 07 2018 - Updated to run on Dell-p3.  Dump new satwnd   "
echo "                       types NC005024, NC005025, NC005026, NC005030, "
echo "                       NC005031, NC005032, NC005034, NC005039.  Dump "
echo "                       new ozone types ompsn8 and ompst8.            "
echo "         Sep 14 2020 - set DTIM_EARLIEST_005091=-3.00 and            "
echo "                       DTIM_LATEST_005091=+2.99.  DTIM_* settings for"
echo "                       005090 are obsolete and removed.              "
echo "                     - Update to remove crisdb and escris and replace"
echo "                       with crsfdb.  On Apr 22, 2020, NSR CrIS data  "
echo "                       from NPP was replaced by FSR. The cris dump   "
echo "                       group is obsolete with the change to FSR and  "
echo "                       has been removed.                             "
echo "                     - Update to dump gsrcsr, gsrasr, ompslp, ahicsr,"
echo "                       sstvcw, sstvpw, leogeo, hdob. leogeo is added "
echo "                       to the ADD_satwnd definition.  sstvcw and     "
echo "                       sstvpw were added to a new unique dump group, "
echo "                       #8.                                           "
echo "                     - Update to remove obsolete GOES-15 data: goesnd"
echo "                       and goesfv.                                   "
echo "                     - Update to remove obsolete dump group: radwnd. "
echo "                     - set DTIM_* settings for new WMO BUFR sequence "
echo "                       Meteosat AMV data. Subsets 005064, 005065, and"
echo "                       006066 are replaced by subsets 005067, 005068,"
echo "                       and 005069.                                   "
echo "         Jun 21 2021 - Incremented subsets for the sfcshp dump groups"
echo "                       to match bufr_dumplist.  Removed tideg from   "
echo "                       sfcshp dump group tom ake individual dump     "
echo "                       file.                                         " 
echo "                     - Copy bufr_dumplist to COMOUT.                 "
############################################################################

set -aux

# Make sure we are in the $DATA directory
cd $DATA

msg="HAS BEGUN on `hostname`"
$DATA/postmsg "$jlogfile" "$msg"

cat break > $pgmout

export dumptime=`cut -c7-16 ncepdate`
export cycp=`echo $dumptime|cut -c9-10`

# Min and max times for this job are now created, based on a plus or minus
#  3-minute window

min_min=-3
min_max=3

dumptime_min=`$MDATE $min_min ${dumptime}30`   
dumptime_max=`$MDATE $min_max ${dumptime}30`


currdate=`date -u +%Y%m%d%H%M`

set +x
echo
echo "CENTER DATA DUMP DATE-TIME FOR MONITORING IS $dumptime"
echo
echo "CURRENT DATE IS $currdate"
echo
set -x

TIME_CHECK=${TIME_CHECK:-YES}
if [ $TIME_CHECK = YES ];then
   if [ $currdate -gt $dumptime_max -o $currdate -lt $dumptime_min ];then
msg="CENTER DATA DUMP TIME OUTSIDE ALLOWABLE TIME WINDOW, NO DUMPS PRODUCED"
      set +x
      echo
      echo $msg
      echo
      set -x
      $DATA/postmsg "$jlogfile" "$msg"
      set +x
      echo " "
      echo " ****** PROCESSING COMPLETED NORMALLY BUT NO DUMPS PRODUCED"
      echo " ****** PROCESSING COMPLETED NORMALLY BUT NO DUMPS PRODUCED"
      echo " ****** PROCESSING COMPLETED NORMALLY BUT NO DUMPS PRODUCED"
      echo " ****** PROCESSING COMPLETED NORMALLY BUT NO DUMPS PRODUCED"
      echo " "
      set -x
# save standard output
      cat  break $pgmout break > allout
      cat allout
      # rm allout

      sleep 10

      msg='ENDED NORMALLY, BUT NO DUMPS PRODUCED.'
      $DATA/postmsg "$jlogfile" "$msg"
      exit
   fi
fi

export COMSP=${DATA}${COMOUT}/$RUN.${cycle}.
mkdir -p ${DATA}${COMOUT}

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
###########################
###########################
#  The data "dump" script
###########################
###########################

msg="START THE DATA DUMP CENTERED ON $dumptime"
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
# Dump # 1 : ADPUPA, 1BMHS, 1BHRS4, OSBUV8, TESAC, TRKOB,
#              (6)    (1)     (1)     (1)     (1)   (1)
#            EFCLAM, SAPHIR, ATMSDB, OMPSN8, OMPST8, OMPSLP
#              (1)     (1)     (1)     (1)     (1)    (1)
#            -- TOTAL NUMBER OF SUBTYPES = 17
#  time window radius is -0.50 to +0.49 hours on ADPUPA, TESAC, EFCLAM,
#  time window radius is -1.50 to -0.51 hours on TRKOB,  ATMSDB
#  time window radius is -2.00 to -1.01 hours on OSBUV8, 1BMHS,  1BHRS4
#                                                OMPSN8, OMPST8
#  time window radius is -2.50 to -1.51 hours on SAPHIR
#  time window radius is -7.00 to -6.01 hours on OMPSLP
#=========================================================================

DTIM_latest_adpupa=+0.49
DTIM_latest_tesac=+0.49
DTIM_latest_efclam=+0.49

DTIM_earliest_1bmhs=-2.00
DTIM_latest_1bmhs=-1.01

DTIM_earliest_1bhrs4=-2.00
DTIM_latest_1bhrs4=-1.01

DTIM_earliest_trkob=-1.50
DTIM_latest_trkob=-0.51

DTIM_earliest_osbuv8=-2.00
DTIM_latest_osbuv8=-1.01

DTIM_earliest_ompsn8=-2.00
DTIM_latest_ompsn8=-1.01

DTIM_earliest_ompst8=-2.00
DTIM_latest_ompst8=-1.01

DTIM_earliest_ompslp=-7.00
DTIM_latest_ompslp=-6.01

DTIM_earliest_saphir=-2.50
DTIM_latest_saphir=-1.51

DTIM_earliest_atmsdb=-1.50
DTIM_latest_atmsdb=-0.51


$ushscript_dump/bufr_dump_obs.sh $dumptime 0.50 1 adpupa osbuv8 1bmhs \
 1bhrs4 tesac trkob efclam saphir atmsdb ompsn8 ompst8 ompslp
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
# Dump # 2 : SFCSHP, ADPSFC, RASSDA, AIRSEV, GOESFV, MTIASI, GOME, AVCSAM
#             (11)     (4)     (1)    (1)     (1)      (1)    (1)    (1)
#            ESIASI, ESATMS, SEVASR, TIDEG
#              (1)     (1)     (1)    (1)
#            -- TOTAL NUMBER OF SUBTYPES = 25
#
# ===> Dumping of WNDSAT removed from here until new ingest feed is established
#      (had been dumped with a time window radius of -3.00 to -2.01 hours)
#
#  time window radius is -0.50 to +0.49 hours on SFCSHP (all types except
#                                                tide gauge from CREX),
#                                                ADPSFC, RASSDA, GOESFV,
#                                                SEVASR
#  time window radius is -1.00 to -0.01 hours on ESIASI, SFCSHP (tide gauge
#                                                from CREX only)
#  time window radius is -1.50 to -0.51 hours on GOME, ESATMS
#  time window radius is -2.00 to -1.01 hours on MTIASI
#  time window radius is -2.50 to -1.51 hours on AIRSEV, AVCSAM
#==========================================================================

DTIM_latest_001001=+0.49
DTIM_latest_001002=+0.49
DTIM_latest_001003=+0.49
DTIM_latest_001004=+0.49
DTIM_latest_adpsfc=+0.49
DTIM_latest_rassda=+0.49
DTIM_latest_sevasr=+0.49

DTIM_earliest_001005=-1.00
DTIM_latest_001005=-0.01

DTIM_earliest_airsev=-2.50
DTIM_latest_airsev=-1.51

DTIM_earliest_mtiasi=-2.00
DTIM_latest_mtiasi=-1.01

DTIM_earliest_gome=-1.50
DTIM_latest_gome=-0.51

DTIM_earliest_avcsam=-2.50
DTIM_latest_avcsam=-1.51

DTIM_earliest_esiasi=-1.00
DTIM_latest_esiasi=-0.01

DTIM_earliest_esatms=-1.50
DTIM_latest_esatms=-0.51

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.50 1 sfcshp adpsfc rassda \
 airsev mtiasi gome avcsam esiasi esatms sevasr tideg
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
# Dump # 3 : SATWND, 1BAMUA, GPSRO, AVCSPM, AMSR2, CRSFDB
#             (17)     (1)    (1)     (1)    (1)     (1)
#            -- TOTAL NUMBER OF SUBTYPES = 25
#  time window radius is -1.00 to -0.01 hours on all GOES SATWND types
#  time window radius is -1.50 to -0.51 hours on all EUMETSAT SATWND types,
#                                                    CRSFDB
#  time window radius is -2.00 to -1.01 hours on 1BAMUA, AMSR2
#  time window radius is -2.50 to -1.51 hours on all other    SATWND types,
#                                                    GPSRO, AVCSPM
#  time window radius is -4.00 to -3.01 hours on all POES (MODIS, AVHRR, VIIRS)
#                                                    SATWND types
#===========================================================================

# Skip Indian water vapor satellite winds in SATWND (never any data)
SKIP_005023=YES

# Skip legacy Meteosat AMV subsets; for testing, skipping happens in trigger or version file
#SKIP_005064=YES
#SKIP_005065=YES
#SKIP_005066=YES

#ADD_satwnd="005024 005025 005026 005030 005031 005032 005034 005039 005072"
ADD_satwnd="005030 005031 005032 005034 005039 005072"

DTIM_earliest_1bamua=-2.00
DTIM_latest_1bamua=-1.01

DTIM_earliest_005064=-1.50
DTIM_latest_005064=-0.51

DTIM_earliest_005065=-1.50
DTIM_latest_005065=-0.51

DTIM_earliest_005066=-1.50
DTIM_latest_005066=-0.51

DTIM_earliest_005067=-1.50
DTIM_latest_005067=-0.51

DTIM_earliest_005068=-1.50
DTIM_latest_005068=-0.51

DTIM_earliest_005069=-1.50
DTIM_latest_005069=-0.51

DTIM_earliest_005010=-1.00
DTIM_latest_005010=-0.01

DTIM_earliest_005011=-1.00
DTIM_latest_005011=-0.01

DTIM_earliest_005012=-1.00
DTIM_latest_005012=-0.01

DTIM_earliest_005019=-1.00
DTIM_latest_005019=-0.01

DTIM_earliest_005021=-2.50
DTIM_latest_005021=-1.51

DTIM_earliest_005022=-2.50
DTIM_latest_005022=-1.51

DTIM_earliest_005024=-1.00
DTIM_latest_005024=-0.01

DTIM_earliest_005025=-1.00
DTIM_latest_005025=-0.01

DTIM_earliest_005026=-1.00
DTIM_latest_005026=-0.01

DTIM_earliest_005030=-1.00
DTIM_latest_005030=-0.01

DTIM_earliest_005031=-1.00
DTIM_latest_005031=-0.01

DTIM_earliest_005032=-1.00
DTIM_latest_005032=-0.01

DTIM_earliest_005034=-1.00
DTIM_latest_005034=-0.01

DTIM_earliest_005039=-1.00
DTIM_latest_005039=-0.01

DTIM_earliest_005044=-2.50
DTIM_latest_005044=-1.51

DTIM_earliest_005045=-2.50
DTIM_latest_005045=-1.51

DTIM_earliest_005046=-2.50
DTIM_latest_005046=-1.51

DTIM_earliest_gpsro=-2.50
DTIM_latest_gpsro=-1.51

DTIM_earliest_005070=-4.00
DTIM_latest_005070=-3.01

DTIM_earliest_005071=-4.00
DTIM_latest_005071=-3.01

DTIM_earliest_005072=-4.00
DTIM_latest_005072=-3.01

DTIM_earliest_005080=-4.00
DTIM_latest_005080=-3.01

DTIM_earliest_005091=-4.00
DTIM_latest_005091=-3.01

DTIM_earliest_avcspm=-2.50
DTIM_latest_avcspm=-1.51

DTIM_earliest_amsr2=-2.00
DTIM_latest_amsr2=-1.01

DTIM_earliest_crsfdb=-1.50
DTIM_latest_crsfdb=-0.51

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.50 1 satwnd gpsro 1bamua avcspm \
 amsr2 crsfdb
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

#=========================================================================
# Dump # 4 : AIRCAR, AIRCFT, PROFLR, VADWND, GEOIMR, ASCATT, HDOB
#              (2)     (8)     (4)     (2)     (1)     (1)   (1)
#            -- TOTAL NUMBER OF SUBTYPES = 19
#  time window radius is -0.50 to +0.49 hours on AIRCAR, AIRCFT,
#                                                PROFLR, VADWND, HDOB
#  time window radius is -1.00 to -0.01 hours on GEOIMR
#  time window radius is -1.50 to -0.51 hours on ASCATT
#=========================================================================

DTIM_latest_aircar=+0.49
DTIM_latest_aircft=+0.49
DTIM_latest_proflr=+0.49
DTIM_latest_vadwnd=+0.49
DTIM_latest_hdob=+0.49

DTIM_earliest_geoimr=-1.00
DTIM_latest_geoimr=-0.01

DTIM_earliest_ascatt=-1.50
DTIM_latest_ascatt=-0.51

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.50 1 aircar aircft proflr vadwnd \
 geoimr ascatt hdob
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
# Dump # 5 : MSONET, GPSIPW, GSRCSR, GSRASR, AHICSR 
#             (30)     (1)    (1)      (1)    (1)
#            -- TOTAL NUMBER OF SUBTYPES = 34
#  time window radius is -0.50 to +0.49 hours on MSONET, GSRCSR,
#                                                GSRASR
#  time window radius is -1.00 to -0.01 hours on GPSIPW, AHICSR
#===================================================================

DTIM_latest_msone0=+0.49
DTIM_latest_gsrcsr=+0.49
DTIM_latest_gsrasr=+0.49

DTIM_earliest_gpsipw=-1.00
DTIM_latest_gpsipw=-0.01
DTIM_earliest_ahicsr=-1.00
DTIM_latest_ahicsr=-0.01

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.50 1 msone0 gpsipw \
 gsrcsr gsrasr ahicsr
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
# Dump # 6 : NEXRAD -- TOTAL NUMBER OF SUBTYPES = 4
#             (4)
#  time window radius is -0.50 to +0.49 hours
#===================================================================

DTIM_latest_nexrad=+0.49

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

if [ $cycp -eq 00 ]; then   # (23.50 - 00.49 Z)
   unset SKIP_006033 # radial wind  23Z
   unset SKIP_006010 # radial wind  00Z
   unset SKIP_006063 # reflectivity 23Z
   unset SKIP_006040 # reflectivity 00Z
elif [ $cycp -eq 01 ]; then   # (00.50 - 01.49 Z)
   unset SKIP_006010 # radial wind  00Z
   unset SKIP_006011 # radial wind  01Z
   unset SKIP_006040 # reflectivity 00Z
   unset SKIP_006041 # reflectivity 01Z
elif [ $cycp -eq 02 ]; then   # (01.50 - 02.49 Z)
   unset SKIP_006011 # radial wind  01Z
   unset SKIP_006012 # radial wind  02Z
   unset SKIP_006041 # reflectivity 01Z
   unset SKIP_006042 # reflectivity 02Z
elif [ $cycp -eq 03 ]; then   # (02.50 - 03.49 Z)
   unset SKIP_006012 # radial wind  02Z
   unset SKIP_006013 # radial wind  03Z
   unset SKIP_006042 # reflectivity 02Z
   unset SKIP_006043 # reflectivity 03Z
elif [ $cycp -eq 04 ]; then   # (03.50 - 04.49 Z)
   unset SKIP_006013 # radial wind  03Z
   unset SKIP_006014 # radial wind  04Z
   unset SKIP_006043 # reflectivity 03Z
   unset SKIP_006044 # reflectivity 04Z
elif [ $cycp -eq 05 ]; then   # (04.50 - 05.49 Z)
   unset SKIP_006014 # radial wind  04Z
   unset SKIP_006015 # radial wind  05Z
   unset SKIP_006044 # reflectivity 04Z
   unset SKIP_006045 # reflectivity 05Z
elif [ $cycp -eq 06 ]; then   # (05.50 - 06.49 Z)
   unset SKIP_006015 # radial wind  05Z
   unset SKIP_006016 # radial wind  06Z
   unset SKIP_006045 # reflectivity 05Z
   unset SKIP_006046 # reflectivity 06Z
elif [ $cycp -eq 07 ]; then   # (06.50 - 07.49 Z)
   unset SKIP_006016 # radial wind  06Z
   unset SKIP_006017 # radial wind  07Z
   unset SKIP_006046 # reflectivity 06Z
   unset SKIP_006047 # reflectivity 07Z
elif [ $cycp -eq 08 ]; then   # (07.50 - 08.49 Z)
   unset SKIP_006017 # radial wind  07Z
   unset SKIP_006018 # radial wind  08Z
   unset SKIP_006047 # reflectivity 07Z
   unset SKIP_006048 # reflectivity 08Z
elif [ $cycp -eq 09 ]; then   # (08.50 - 09.49 Z)
   unset SKIP_006018 # radial wind  08Z
   unset SKIP_006019 # radial wind  09Z
   unset SKIP_006048 # reflectivity 08Z
   unset SKIP_006049 # reflectivity 09Z
elif [ $cycp -eq 10 ]; then   # (09.50 - 10.49 Z)
   unset SKIP_006019 # radial wind  09Z
   unset SKIP_006020 # radial wind  10Z
   unset SKIP_006049 # reflectivity 09Z
   unset SKIP_006050 # reflectivity 10Z
elif [ $cycp -eq 11 ]; then   # (10.50 - 11.49 Z)
   unset SKIP_006020 # radial wind  10Z
   unset SKIP_006021 # radial wind  11Z
   unset SKIP_006050 # reflectivity 10Z
   unset SKIP_006051 # reflectivity 11Z
elif [ $cycp -eq 12 ]; then   # (11.50 - 12.49 Z)
   unset SKIP_006021 # radial wind  11Z
   unset SKIP_006022 # radial wind  12Z
   unset SKIP_006051 # reflectivity 11Z
   unset SKIP_006052 # reflectivity 12Z
elif [ $cycp -eq 13 ]; then   # (12.50 - 13.49 Z)
   unset SKIP_006022 # radial wind  12Z
   unset SKIP_006023 # radial wind  13Z
   unset SKIP_006052 # reflectivity 12Z
   unset SKIP_006053 # reflectivity 13Z
elif [ $cycp -eq 14 ]; then   # (13.50 - 14.49 Z)
   unset SKIP_006023 # radial wind  13Z
   unset SKIP_006024 # radial wind  14Z
   unset SKIP_006053 # reflectivity 13Z
   unset SKIP_006054 # reflectivity 14Z
elif [ $cycp -eq 15 ]; then   # (14.50 - 15.49 Z)
   unset SKIP_006024 # radial wind  14Z
   unset SKIP_006025 # radial wind  15Z
   unset SKIP_006054 # reflectivity 14Z
   unset SKIP_006055 # reflectivity 15Z
elif [ $cycp -eq 16 ]; then   # (15.50 - 16.49 Z)
   unset SKIP_006025 # radial wind  15Z
   unset SKIP_006026 # radial wind  16Z
   unset SKIP_006055 # reflectivity 15Z
   unset SKIP_006056 # reflectivity 16Z
elif [ $cycp -eq 17 ]; then   # (16.50 - 17.49 Z)
   unset SKIP_006026 # radial wind  16Z
   unset SKIP_006027 # radial wind  17Z
   unset SKIP_006056 # reflectivity 16Z
   unset SKIP_006057 # reflectivity 17Z
elif [ $cycp -eq 18 ]; then   # (17.50 - 18.49 Z)
   unset SKIP_006027 # radial wind  17Z
   unset SKIP_006028 # radial wind  18Z
   unset SKIP_006057 # reflectivity 17Z
   unset SKIP_006058 # reflectivity 18Z
elif [ $cycp -eq 19 ]; then   # (18.50 - 19.49 Z)
   unset SKIP_006028 # radial wind  18Z
   unset SKIP_006029 # radial wind  19Z
   unset SKIP_006058 # reflectivity 18Z
   unset SKIP_006059 # reflectivity 19Z
elif [ $cycp -eq 20 ]; then   # (19.50 - 20.49 Z)
   unset SKIP_006029 # radial wind  19Z
   unset SKIP_006030 # radial wind  20Z
   unset SKIP_006059 # reflectivity 19Z
   unset SKIP_006060 # reflectivity 20Z
elif [ $cycp -eq 21 ]; then   # (20.50 - 21.49 Z)
   unset SKIP_006030 # radial wind  20Z
   unset SKIP_006031 # radial wind  21Z
   unset SKIP_006060 # reflectivity 20Z
   unset SKIP_006061 # reflectivity 21Z
elif [ $cycp -eq 22 ]; then   # (21.50 - 22.49 Z)
   unset SKIP_006031 # radial wind  21Z
   unset SKIP_006032 # radial wind  22Z
   unset SKIP_006061 # reflectivity 21Z
   unset SKIP_006062 # reflectivity 22Z
elif [ $cycp -eq 23 ]; then   # (22.50 - 23.49 Z)
   unset SKIP_006032 # radial wind  22Z
   unset SKIP_006033 # radial wind  23Z
   unset SKIP_006062 # reflectivity 22Z
   unset SKIP_006063 # reflectivity 23Z
fi

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.50 1 nexrad
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

#======================================================================
# Dump # 7 : ATOVS, OMI, ESAMUA, ESHRS3, ESMHS, SSMISU, SEVCSR, LGHTNG,
#             (1)   (1)    (1)     (1)    (1)     (1)     (1)     (2)
#            LGYCLD, ATMS, BATHY, MLS, CRISF4, IASIDB  --
#              (1)    (1)   (1)   (1)   (1)     (1)
#            -- TOTAL NUMBER OF SUBTYPES = 15
#  time window radius is -0.50 to +0.49 hours on ESAMUA, ESHRS3, ESMHS,
#                                                SEVCSR, LGHTNG, LGYCLD,
#  time window radius is -1.50 to -0.51 hours on SSMISU, ATMS,
#                                                CRISF4, IASIDB
#  time window radius is -2.00 to -1.01 hours on ATOVS, MLS
#  time window radius is -2.50 to -1.51 hours on OMI
#  time window radius is -7.00 to -6.01 hours on BATHY
#======================================================================

DTIM_latest_esamua=+0.49
DTIM_latest_eshrs3=+0.49
DTIM_latest_esmhs=+0.49
DTIM_latest_sevcsr=+0.49
DTIM_latest_lghtng=+0.49
DTIM_latest_lgycld=+0.49

DTIM_earliest_atovs=-2.00
DTIM_latest_atovs=-1.01

DTIM_earliest_omi=-2.50
DTIM_latest_omi=-1.51

DTIM_earliest_bathy=-7.00
DTIM_latest_bathy=-6.01

DTIM_earliest_ssmisu=-1.50
DTIM_latest_ssmisu=-0.51

DTIM_earliest_mls=-2.00
DTIM_latest_mls=-1.01

DTIM_earliest_atms=-1.50
DTIM_latest_atms=-0.51

DTIM_earliest_crisf4=-1.50
DTIM_latest_crisf4=-0.51

DTIM_earliest_iasidb=-1.50
DTIM_latest_iasidb=-0.51

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.50 1 atovs omi esamua \
 eshrs3 esmhs ssmisu sevcsr lghtng lgycld atms bathy mls crisf4 iasidb
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

#======================================================================
# Dump # 8 : SSTVCW, SSTVPW
#              (1)    (1)
#            -- TOTAL NUMBER OF SUBTYPES = 2 
#  time window radius is -0.50 to +0.49 hours on SSTVCW, SSTVPW
#======================================================================

DTIM_latest_sstvcw=+0.49
DTIM_latest_sstvpw=+0.49

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.50 1 sstvcw sstvpw 
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

#-------NEW MESON1 

set +x
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

#===================================================================
# Dump # 9 : MSONE1
#            (1)
#            -- TOTAL NUMBER OF SUBTYPES = 1
#  time window radius is -0.50 to +0.49 hours on MSONET
#===================================================================
DTIM_latest_msone1=+0.49
DTIM_earliest_msone1=-1.00

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.50 1 msone1
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

#-------NEW UPRAIR 

set +x
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
# Dump # 10: UPRAIR SALDRN SOFARW
#             (1)
#            -- TOTAL NUMBER OF SUBTYPES = 3
#===================================================================
DTIM_latest_uprair=+0.49
DTIM_earliest_uprair=-0.49

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.50 1 uprair saldrn sofarw
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

set +u
launcher=${launcher:-"cfp"}  # if not "cfp", threads will be run serially.
if [ "$launcher" = cfp ]; then
   > $DATA/poe.cmdfile
## To better take advantage of cfp, execute the longer running commands first.
## Some reordering was done here based on recent sample runtimes.
   echo ./thread_6 >> $DATA/poe.cmdfile  # moved up
   echo ./thread_2 >> $DATA/poe.cmdfile  # moved up
   echo ./thread_3 >> $DATA/poe.cmdfile  # moved up
   echo ./thread_7 >> $DATA/poe.cmdfile  # moved up
   echo ./thread_5 >> $DATA/poe.cmdfile  # moved up
   echo ./thread_8 >> $DATA/poe.cmdfile  # moved up
   echo ./thread_1 >> $DATA/poe.cmdfile
   echo ./thread_4 >> $DATA/poe.cmdfile
   echo ./thread_9 >> $DATA/poe.cmdfile
   echo ./thread_10 >> $DATA/poe.cmdfile

   if [ -s $DATA/poe.cmdfile ]; then
      export MP_CSS_INTERRUPT=yes  # ??
      launcher_DUMP=${launcher_DUMP:-mpiexec}
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
fi
cat $DATA/1.out $DATA/2.out $DATA/3.out $DATA/4.out $DATA/5.out $DATA/6.out \
 $DATA/7.out $DATA/8.out  $DATA/9.out  $DATA/10.out

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

#===============================================================================

export STATUS=YES
export DUMP_NUMBER=11
$ushscript_dump/bufr_dump_obs.sh $dumptime 0.50 1 null


cp -p ${DATA}${COMOUT}/$RUN.${cycle}.status.tm00.bufr_d $COMOUT

#  endif loop $PROCESS_DUMP
fi

echo " " >> $pgmout
echo "##################################################################\
####################"  >> $pgmout
echo " " >> $pgmout

#================================================================
#================================================================

if [ "$PROCESS_DATACOUNTS" = 'YES' ]; then

#  Prepare the data counts for the SDM if requested
#  ------------------------------------------------

   $ushscript_datacount/bufr_datacount.sh
fi

#================================================================
#================================================================


if [ "$PROCESS_DUMP" = 'YES' ]; then

   if [ "$err1" -gt '5' -o "$err2" -gt '5' -o "$err3" -gt '5' -o \
        "$err4" -gt '5' -o "$err5" -gt '5' -o "$err6" -gt '5'  -o \
        "$err7" -gt '5' -o "$err8" -gt '5' -o \
        "$err9" -gt '5' -o "$err10" -gt '5'	]; then
      for n in $err1 $err2 $err3 $err4 $err5 $err6 $err7 $err8 $err9 $err10
      do
         if [ "$n" -gt '5' ]; then
            if [ "$n" -ne '11' -a "$n" -ne '22' ]; then
       
## fatal error in dumping of BUFR obs. files
       
               set +x
echo
echo " ###################################################### "
echo " --> > 22 RETURN CODE FROM DATA DUMP, $err1, $err2, $err3, $err4, \
$err5, $err6, $err7 $err8 $err9 $err10"
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
$err5, $err6, $err7 $err8 $err9 $err10 "
      echo " --> NOT ALL DATA DUMP FILES ARE COMPLETE - CONTINUE    "
      echo " ###################################################### "
      echo
      set -x
   fi


   cat ${DATA}/msone0.ibm ${DATA}/msone1.ibm > ${DATA}/msonet.ibm
   cpfs ${DATA}/msonet.ibm ${COMSP}msonet.${tmmark}.bufr_d
   chmod 640 ${COMSP}msonet.${tmmark}.bufr_d
   chgrp rstprod ${COMSP}msonet.${tmmark}.bufr_d

#  endif loop $PROCESS_DUMP
fi


if [ "$PROCESS_AVGTABLES" = 'YES' ]; then

########################################################################
#    Update Data Count Average Tables for All Cycles in Dump Monitor   #
#               (Normally done only in 23Z run of this job)            #
########################################################################

   msg="Attempt to update data count average table dump monitoring"
   $DATA/postmsg "$jlogfile" "$msg"

   $USHobsproc/bufr_avgdata.sh $NET
   errsc=$?
   if [ "$errsc" -eq '0' ]; then
      rm $AVGDarch_OUT/obcount_30davg.${NET}.current
      cp obcount_30davg.${NET}.current \
       $AVGDarch_OUT/obcount_30davg.${NET}.current
      chmod 775 $AVGDarch_OUT/obcount_30davg.${NET}.current
      msg="Data count average table SUCCESSFULLY updated for dump monitoring"
      $DATA/postmsg "$jlogfile" "$msg"
      typeset -Z2 this_month last_month
      this_month=`echo $PDY | cut -c5-6`
      year=`echo $PDY | cut -c1-4`
      last_month=`expr $this_month - 1`
      if [ "$last_month" -eq '0' ]; then
         last_month=12
         year=`expr $year - 1`
      fi
###   if [ ! -s $AVGDarch_OUT/obcount_30davg.${NET}.${year}${last_month} ]
###   then

#  If no data count average table found for previous month, save this one
#   (Currently not done in data monitoring)

###      rm $AVGDarch_OUT/obcount_30davg.${NET}.*${last_month}
###      cp obcount_30davg.${NET}.current \
###       $AVGDarch_OUT/obcount_30davg.${NET}.${year}${last_month}
###      chmod 775 $AVGDarch_OUT/obcount_30davg.${NET}.${year}${last_month}
###      msg="DATA COUNT AVERAGE table for ${year}${last_month} saved \
###for dump monitoring"
###      $DATA/postmsg "$jlogfile" "$msg"
###   fi
   else
      msg="Data count average table NOT updated for dump monitoring"
      $DATA/postmsg "$jlogfile" "$msg"
   fi

#  endif loop $PROCESS_AVGTABLES
fi

#######################

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
