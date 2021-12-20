#!/bin/ksh
# Run under ksh

#############################################################################
echo "----------------------------------------------------------------------"
echo "exurma_makeprepbufr.sh - URMA model prepbufr processing"
echo "----------------------------------------------------------------------"
echo "History: Aug 30 2013 - Original script."
#############################################################################

set -x

# Make sure we are in the $DATA directory
cd $DATA

msg="HAS BEGUN on `hostname`"
postmsg "$jlogfile" "$msg"

cat break > $pgmout

CHGRP_RSTPROD=${CHGRP_RSTPROD:-YES}

export COMSP=${COMSP:-$COMIN/${RUN}.${cycle}.}

cdate10=`cut -c7-16 ncepdate`

msg="CENTER TIME FOR PREPBUFR PROCESSING IS $cdate10"
postmsg "$jlogfile" "$msg"

ksh $ushscript_prep/prepobs_makeprepbufr.sh $cdate10
errsc=$?

[ "$errsc" -ne '0' ]  &&  exit $errsc

if [ "$CHGRP_RSTPROD" = 'YES' ]; then
   msg="NOTE: These files (if present) are RESTRICTED to rstprod group: \
prepbufr"
   postmsg "$jlogfile" "$msg"
set +x
   echo " "
   echo "$msg"
   echo " "
set -x
fi
warning=no

if [ "$PREPDATA" = 'YES' ]; then

# save snapshot of prepbufr file after PREPOBS_PREPDATA in COMOUT
   cp prepda.prepdata $COMOUT/${RUN}.${cycle}.prepbufr.$tmmark

   if [ "$CHGRP_RSTPROD" = 'YES' ]; then
      chgrp rstprod $COMOUT/${RUN}.${cycle}.prepbufr.$tmmark
      errch=$?
      if [ $errch -eq 0 ]; then
         chmod 640 $COMOUT/${RUN}.${cycle}.prepbufr.$tmmark
      else
         cp /dev/null $COMOUT/${RUN}.${cycle}.prepbufr.$tmmark
         warning=yes
      fi
   fi
#bsm - troubleshooting
echo "is the prepbufr file good?"
echo `ls -l $COMOUT/${RUN}.${cycle}.prepbufr.$tmmark`

# save current prepbufr mnemonic table in COMOUT if either it isn't already
#  there for a previous cycle or if it has changed from a previous cycle
   if [ ! -s $COMOUT/*prep.bufrtable ]; then
      cp prep.bufrtable  $COMOUT/${RUN}.${cycle}.prep.bufrtable
   else
      diff `ls -t  $COMOUT/*prep.bufrtable | head -n1` prep.bufrtable \
       > /dev/null 2>&1
      errdiff=$?
      [ "$errdiff" -ne '0' ]  &&  \
       cp prep.bufrtable  $COMOUT/${RUN}.${cycle}.prep.bufrtable
   fi
fi

if [ "$warning" = 'yes' ]; then
   msg="**WARNING: Since user $USER is not in rstprod group all RESTRICTED \
files are replaced with a null file"
   postmsg "$jlogfile" "$msg"
set +x
   echo " "
   echo "$msg"
   echo " "
set -x
fi

########################################################

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
