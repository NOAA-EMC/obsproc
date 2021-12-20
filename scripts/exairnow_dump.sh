#!/bin/ksh
#####################################################################
echo "---------------------------------------------------------------------"
echo "exairnow_dump.sh.ecf - Daily AIRNOW data dump and PREPBUFR processing"
echo "---------------------------------------------------------------------"
echo "History: Apr 26 2004 - Original script."
echo "         Jun 21 2013 - modified for WCOSS (linux) platforms"
echo "         Oct 16 2014 - modified to run under vertical structure (VS)"
echo "                       added Particulate Matter (PM) processing"
echo "         Dec 18 2019 - modified to run on WCOSS ph3"
#####################################################################
# modification history
#  26 Apr 2004 - original script
#  21 Jun 2013 - cp'd CCS:exairnow_dump.sh.ecf of 5 Jan 2009;
#                modified to run on WCOSS (linux) platforms
#                  renamed w/ ecf extension
#                  set to run under ksh
#                  replaced XLF logical unit num spec w/ FORTxx logic
#                  replaced timex w/ time -p
#  16 Oct 2014 - cp'd prod ver of Jun 17 15:57 2014
#                converted to run under vertical structure
#                added PM processing logic (fort52, pb_anowpm)
#  18 Dec 2019 - modified to run on WCOSS ph3
#---
set -ax

# Make sure we are in the $DATA directory
cd $DATA

##msg="Daily AIRNOW data dump and PREPBUFR processing has begun on `hostname`"
##postmsg "$jlogfile" "$msg"
 
cat break > $pgmout

########################################################
#  First, dump the AIRNOW data
########################################################

if [ -z "$pdy" ]; then
   tmhr=`echo $tmmark|cut -c3-5`
   export dumptime=`$NDATE -$tmhr $PDY$cyc`
else
   export dumptime=`echo $pdy`$cyc
fi

set +x
echo
echo "CENTER DATA DUMP DATE-TIME IS $dumptime"
echo
set -x
 
export COMSP=$COMOUT/${RUN}.${cycle}.
 
err1=0

msg="Start the daily AIRNOW dump centered on $dumptime - $tmmark"
postmsg "$jlogfile" "$msg"

#----------------------------------------------------------------
cat<<\EOF>thread_1; chmod +x thread_1
set -uax

cd $DATA

{ echo
set +x
echo "********************************************************************"
echo Script thread_1
echo Executing on node  `hostname`
echo Starting time: `date`
echo "********************************************************************"
echo
set -x

export STATUS=NO
export DUMP_NUMBER=1

#============================================================================
# Dump # 1 : AIRNOW -- TOTAL NUMBER OF SUBTYPES = 1
#            time window radius is -11.00 to +12.99 hours
#============================================================================

DTIM_latest_airnow=+12.99

$ushscript_dump/bufr_dump_obs.sh $dumptime 11.00 1 airnow
error1=$?

echo "$error1" > $DATA/error1

set +x
echo "********************************************************************"
echo Script thread_1
echo Finished executing on node  `hostname`
echo Ending time  : `date`
echo "********************************************************************"
set -x
} > $DATA/1.out 2>&1
EOF

#----------------------------------------------------------------

./thread_1

cat $DATA/1.out
err1=`cat $DATA/error1`

#####msg=`grep "  HAS  " $DATA/job1/dumpjb.out`
#####$DATA/postmsg "$jlogfile" "$msg"

export STATUS=YES
export DUMP_NUMBER=2
$ushscript_dump/bufr_dump_obs.sh $dumptime 0.50 1 null

#================================================================
#================================================================

if [ "$err1" -gt '5' ]; then
   for n in $err1
   do
      if [ "$n" -gt '5' ]; then
         if [ "$n" -ne '11' -a "$n" -ne '22' ]; then

## fatal error in dumping of BUFR obs. files

            set +x
echo
echo " ###################################################### "
echo " --> > 22 RETURN CODE FROM DATA DUMP, $err1 "
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
   echo " --> > 5 RETURN CODE FROM DATA DUMP, $err1 "
   echo " --> NOT ALL DATA DUMP FILES ARE COMPLETE - CONTINUE    "
   echo " ###################################################### "
   echo
   set -x
fi

if [ $err1 -gt 0 ]; then

   msg="Since AIRNOW dump file is EMPTY, NO PREPBUFR processing is performed"
   postmsg "$jlogfile" "$msg"
   touch ${COMSP}prepbufr.$tmmark

else

##########################################################################
#  Next, put the AIRNOW data into a mini-PREPBUFR file (all by itself)
##########################################################################

   EXECPREP=${EXECPREP:-$HOMEobsproc/exec}
   FIXPREP=${FIXPREP:-$HOMEobsproc/fix}

   PANX=${PANX:-$EXECPREP/prepobs_prepanow}
   PANT=${PANT:-$FIXPREP/prepobs_anow.bufrtable}

   msg="center time for PREPBUFR processing is $dumptime - $tmmark"
   postmsg "$jlogfile" "$msg"

   pgm=`basename  $PANX`
   if [ -s prep_step ]; then
      . prep_step
   else
      [ -f errfile ] && rm errfile
      unset FORT00 `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`
   fi

   cp ${COMSP}airnow.$tmmark.bufr_d airnow.dump

   echo "$dumptime"      > cdate10.dat
   export FORT21=airnow.dump
   export FORT25=$PANT
   export FORT30=cdate10.dat
   export FORT51=prepbufr
   export FORT52=pb_anowpm

   time -p $PANX > prepanow.out 2> errfile
   err=$?
   ###cat errfile
   cat errfile >> prepanow.out
   set +u
   [ -n "$pgmout" ]  &&  cat prepanow.out >> $pgmout
   set -u
   set +x
   echo
   echo 'The foreground exit status for PREPOBS_PREPANOW is ' $err
   echo
   set -x
   if [ -s err_chk ]; then
      err_chk
   else
      if test "$err" -gt '0'
      then
#########kill -9 ${qid}
         exit 555
      fi
   fi

   if [ "$err" -gt '0' ]; then
      exit 9
   else
# save final form of prepbufr file in COMSP
      cp prepbufr ${COMSP}prepbufr.$tmmark
      cp pb_anowpm ${COMSP}anowpm.pb.$tmmark
   fi

fi


# GOOD RUN
# this is done in J-job
###set +x
###echo " "
###echo " ****** AIRNOW DUMP AND PREPBUFR PROCESSING COMPLETED NORMALLY"
###echo " ****** AIRNOW DUMP AND PREPBUFR PROCESSING COMPLETED NORMALLY"
###echo " ****** AIRNOW DUMP AND PREPBUFR PROCESSING COMPLETED NORMALLY"
###echo " ****** AIRNOW DUMP AND PREPBUFR PROCESSING COMPLETED NORMALLY"
###echo " "
###set -x


# save standard output
###cat break $pgmout break > allout
cat break $pgmout break >> ../allout
###cat allout
### # rm allout

###sleep 10

###msg='ENDED NORMALLY.'
###postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
