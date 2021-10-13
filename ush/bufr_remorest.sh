#!/bin/ksh
# bufr_remorest.sh
# 
# modificaton history
#  13 Mar 2013 SMelchior - ported to WCOSS (linux) platforms:
#                - cp'd CCS ver of 5 Jan 2009
#                - updated logical unit links (rm'd XLF*)
#                - replaced "timex" w/ "time -p"
#  15 Mar 2013 JWhiting - set to run under ksh;
#                         added modification history documentation
#  09 Mar 2017 DStokes -  Changed exit values of 555 (which is out of the
#                         acceptable range) to 55 to ensure that the parent
#                         script sees the intended value.
#-----
#
# This ush script either removes reports that are restricted from
#  resdistribution, or masks the id of reports that are restricted from
#  resdistribution prior to writing out updated BUFR file (normally either
#  PREPBUFR or data dump files).
#
# It is normally executed by either the model script exprep_post.sh.sms (for
#  PREPBUFR files) or exdump_post.sh.sms (for BUFR data dump files) but it
#  can also be executed from a checkout parent script.
# --------------------------------------------------------------------------

set -aux

qid=$$

# Positional parameters passed in:
#   1 - path to COPY OF input file (which normally contains both restricted
#       and unrestricted reports --> becomes output BUFR file upon
#       successful completion of this script (note that input BUFR file is
#       NOT saved by this script)

# Imported variables that must be passed in:
#   DATA - path to working directory
#   REMX - path to BUFR_REMOREST program executable
#   REMC - path to BUFR_REMOREST program parm cards

cd $DATA
PRPI=$1
if [ ! -f $PRPI ] ; then exit 1 ;fi
if [ ! -s $PRPI ] ; then exit 0 ;fi

[ -f $PRPI.unrestricted ] && rm $PRPI.unrestricted

pgm=`basename  $REMX`
if [ -s prep_step ]; then
   set +u
   . prep_step
   set -u
else
   [ -f errfile ] && rm errfile
   export FORT01=0
   unset `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`
fi

echo $PRPI > filename
export FORT11=filename
export FORT21=$PRPI
export FORT51=$PRPI.unrestricted
time -p $REMX< $REMC > outout 2> errfile
err=$?
##cat errfile
cat errfile >> outout
cat outout >> remorest.out
set +u
[ -n "$pgmout" ]  &&  cat outout >> $pgmout
set -u
rm outout
set +x
echo
echo 'The foreground exit status for BUFR_REMOREST is ' $err
echo
set -x
if [ -s err_chk ]; then
   err_chk
else
   if test "$err" -gt '0'
   then
######kill -9 ${qid}
      exit 55
   fi
fi

if [ "$err" -gt '0' ]; then
   exit 9
else
   mv $PRPI.unrestricted $PRPI
fi

exit 0

