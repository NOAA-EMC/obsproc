#!/bin/bash
#####################################################################
echo "----------------------------------------------------------    "
echo "exurma_dump.sh - URMA model data dump processing          "
echo "----------------------------------------------------------    "
echo "History: May 19 2006 - Original script.                       "
echo "         Sep 10 2014 - Use parallel scripting to process dump "
echo "                       groups.                                "
echo "         Feb  3 2015 - Dump new satwnd types NC005019,        "
echo "                       NC005080, NC005090. Dump new type      "
echo "                       efclam.                                "
echo "         Jul 14 2017 - Added types NC255031 (Urbanet) and     "
echo "                       NC255101 (COOPmd) to msonet dumps      "
echo "         Aug 25 2017 - Dump new satmar types (12)             "
echo "         Jul  2 2018 - rm'd ADD_MSONET for Urbanet & COOPmd;  "
echo "                         expected to be in msonet dump group  "
echo "         Jul  9 2019 - Updated to run on Dell-p3. Disabled    "
echo "                       background threading.                  "
echo "         Sep 14 2020 - Updated Dump group #1 to skip legacy   "
echo "                       Meteosat AMV subsets: 005064, 005065,  "
echo "                       006066.  These subsets are replaced by "
echo "                       new WMO BUFR sequence subsets: 005067, "
echo "                       005068, 005069. The format change from "
echo "                       EUMETSAT occurs Oct 6, 2020.           "
echo "         Jun 12 2021 - Incremented subsets for the sfcshp dump"
echo "                       groups to match bufr_dumplist.  Removed"
echo "                       tideg from sfcshp dump group to make   "
echo "                       unique dump file.                      " 
echo "                     - Copy bufr_dumplist to COMOUT.          "
echo "         Dec 15 2021 - set for use on WCOSS2.                 "
echo "         Aug  1 2022 - Added SUBPFL, SALDRN, SNOCVR,          "
echo "                       and GMI1CR types.                      "
#####################################################################

set -x

# Make sure we are in the $DATA directory
cd $DATA

msg="HAS BEGUN on `hostname`"
postmsg "$jlogfile" "$msg"
 
cat break > $pgmout

export dumptime=`cut -c7-16 ncepdate`

tmmark_uc=$(echo $tmmark | tr [a-z] [A-Z])

msg="URMA ANALYSIS TIME IS $PDY$cyc"
postmsg "$jlogfile" "$msg"

set +x
echo
echo "CENTER DATA DUMP DATE-TIME FOR $tmmark_uc URMA IS $dumptime"
echo
set -x
 
export COMSP=$COMOUT/$RUN.${cycle}.
 
err1=0
err2=0
err3=0
err4=0
if [ "$PROCESS_DUMP" = 'YES' ]; then

###################################
###################################
#  The data "dump" script for tm00
###################################
###################################

msg="START THE $tmmark_uc URMA DATA DUMP CENTERED ON $dumptime"
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
# Dump # 1 : ASCATT, SATWND, EFCLAM, GMI1CR
#              (1)    (14)     (1)    (1)
#            -- TOTAL NUMBER OF SUBTYPES = 17
# ===> Dumping of WNDSAT removed from here until new ingest feed is established
#      (had been dumped with a time window radius of -6.00 to 0.00 hours)
#
#            time window radius is -6.00 to 0.00 hours for ASCATT
#            for SATWND subtypes 005/010, 005/011, 005/012 and 005/019 at all
#            times:
#              time window radius is -1.00 to -0.01 hours
#            for SATWND subtypes 005/064, 005/065, 005/066, 005/067, 005/068,
#            and 005/069 at all times:
#            time window radius is -1.50 to +1.49 hours
#            for all other SATWND subtypes:
#              time window radius is +/- 2.5 hours
#            for EFCLAM:
#              time window radius is +/- 0.5 hours
#=======================================================================

# Skip all Indian satellite winds in SATWND (not in domain)
export SKIP_005021=YES
export SKIP_005022=YES
export SKIP_005023=YES

# Skip legacy Meteosat AMV subsets.  Replaced by new BUFR sequence subsets Oct 2020.
# for testing skip in trigger or version file
#export SKIP_005064=YES
#export SKIP_005065=YES
#export SKIP_005066=YES

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

DTIM_earliest_ascatt=-6.00
DTIM_latest_ascatt=0.00

DTIM_earliest_efclam=-0.50
DTIM_latest_efclam=+0.50

#DTIM_earliest_infav1=-2.50
#DTIM_latest_infav1=+2.49
#DTIM_earliest_005081=${DTIM_earliest_005081:-"-3.00"}
#DTIM_latest_005081=${DTIM_latest_005081:-"+2.99"}
DTIM_earliest_005081=-3.00
DTIM_latest_005081=+1.00

DTIM_earliest_gmi1cr=${DTIM_earliest_gmi1cr:-"-3.00"}
DTIM_latest_gmi1cr=${DTIM_latest_gmi1cr:-"+2.99"}

$ushscript_dump/bufr_dump_obs.sh $dumptime 2.5 1 ascatt satwnd efclam gmi1cr
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
# Dump # 2 : SFCSHP, ADPSFC, TIDEG, SUBPFL, SALDRN, SNOCVR
#             (11)     (6)    (1)    (1)    (1)    (1)
#            -- TOTAL NUMBER OF SUBTYPES = 21
#            time window radius is +/- 0.50 hours for SFCSHP and ADPSFC
#=======================================================================

# Skip mobile synoptic reports in ADPSFC (not in domain)

export SKIP_000002=YES

DTIM_earliest_subpfl=${DTIM_earliest_subpfl:-"-2.00"}
DTIM_latest_subpfl=${DTIM_latest_subpfl:-"+1.99"}
DTIM_earliest_saldrn=${DTIM_earliest_saldrn:-"-2.00"}
DTIM_latest_saldrn=${DTIM_latest_saldrn:-"+1.99"}
DTIM_earliest_snocvr=${DTIM_earliest_snocvr:-"-2.00"}
DTIM_latest_snocvr=${DTIM_latest_snocvr:-"+1.99"}

$ushscript_dump/bufr_dump_obs.sh $dumptime 0.5 1 sfcshp tideg adpsfc \
           subpfl saldrn snocvr
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

#===========================================================================
# Dump # 4 : SATMAR -- TOTAL NUMBER OF SUBTYPES = 12 (4 active)
#            time window radius is 0.50 hours
#===========================================================================

$ushscript_dump/bufr_dump_obs.sh $dumptime 3.0 1 satmar
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

   if [ -s $DATA/poe.cmdfile ]; then
      export MP_CSS_INTERRUPT=yes  # ??
      launcher_DUMP=${launcher_DUMP:-mpiexec}
      $launcher_DUMP -np 3 --cpu-bind verbose,core cfp $DATA/poe.cmdfile
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
fi
   

cat $DATA/1.out $DATA/2.out $DATA/3.out $DATA/4.out

set +x
echo " "
echo " "
set -x

err1=`cat $DATA/error1`
err2=`cat $DATA/error2`
err3=`cat $DATA/error3`
err4=`cat $DATA/error4`


#================================================================

export STATUS=YES
export DUMP_NUMBER=5
$ushscript_dump/bufr_dump_obs.sh $dumptime 3.00 1 null


#  endif loop $PROCESS_DUMP
fi

#================================================================
#================================================================


if [ "$PROCESS_DUMP" = 'YES' ]; then

   if [ "$err1" -gt '5' -o "$err2" -gt '5' -o "$err3" -gt '5' -o "$err4" -gt '5' ]; then
      for n in $err1 $err2 $err3 $err4
      do
         if [ "$n" -gt '5' ]; then
            if [ "$n" -ne '11' -a "$n" -ne '22' ]; then

## fatal error in dumping of BUFR obs. files

               set +x
echo
echo " ###################################################### "
echo " --> > 22 RETURN CODE FROM DATA DUMP, $err1, $err2, $err3, $err4"
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
      echo " --> > 5 RETURN CODE FROM DATA DUMP, $err1, $err2, $err3, $err4"
      echo " --> NOT ALL DATA DUMP FILES ARE COMPLETE - CONTINUE    "
      echo " ###################################################### "
      echo
      set -x
   fi

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
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
