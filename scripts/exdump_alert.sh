#!/bin/ksh
# exdump_alert.sh
#
# modification history
#  5 Aug 1999 ?        - original script
#    MAR 2000 ?        - Modified for implementation of the IBM SP
# 20 Mar 2013 JWhiting - ported for use on WCOSS (linux) platforms:
#                         set to run under ksh
#                         no logic changes
#  9 Sep 2014 JWhiting - removed echo reference to 'IBM SP'
#                        trapped RUN=gdas to set correct file prefix
# 18 Apr 2017 DStokes  - Updated to handle gdas file with prefix gdas or gdas1
#                      - Added logic to have job fail if the dump_alert_flag
#                        file is not available or cannot be printed via cat.
#                      - Modified to check dump_alert_flags with $tmmark suffix.
# 19 Sep 2018 SMelchior- Removed temporary logic to handle gdas1 prefix.
# 14 Dec 2021 NEsposito- Updated for use on WCOSS2; removed .ecf filename extension
#####################################################################
echo " "

set -u

cd $DATA
jlogfile=${jlogfile:-""}

########################################
set -x
msg="HAS BEGUN!"
postmsg "$jlogfile" "$msg"
########################################


# List NCEP date
export analtime=`cut -c7-16 ncepdate`

set +x
echo "CENTER DATA DUMP DATE-TIME FOR $NET IS $analtime"
set -x
 
# ---------------------------------------
# check for severly low data counts 
# ---------------------------------------


tmmark=${tmmark:-tm00}

alert_flag_file=$COMIN/$RUN.$cycle.dump_alert_flag.$tmmark

RETC=$(cat $alert_flag_file)
err=$?
if [ $err -eq 0 ]; then
   export RETC
   if [ $RETC -eq 6 ]
      then
      msg="DATACOUNT low on 1 or more CRITICAL ob types - SDM should check \
ALERTLOG messages in ${COMROOT}/logs/alertlog/$RUN.t${cyc}z"
      postmsg "$jlogfile" "$msg"
      echo $msg > errfile
      export err=6
      err_chk
   elif [ $RETC -eq 5 ]
      then
      msg="DATACOUNT low on 1 or more non-critical ob types - SDM should check \
ALERTLOG messages in ${COMROOT}/logs/alertlog/$RUN.t${cyc}z"
      postmsg "$jlogfile" "$msg"
  fi
else
  msg="FATAL ERROR:  cat of $alert_flag_file failed. status $err"
  postmsg "$jlogfile" "$msg"
  echo $msg > errfile
  err_exit
fi

#####################################################################
# GOOD RUN
set +x
echo "************** $job COMPLETED NORMALLY"
echo "************** $job COMPLETED NORMALLY"
echo "************** $job COMPLETED NORMALLY"
set -x
#####################################################################

msg="HAS COMPLETED NORMALLY!"
postmsg "$jlogfile" "$msg"
############## END OF SCRIPT #######################

