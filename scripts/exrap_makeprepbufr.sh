#!/bin/ksh
# Run under ksh

################################################################################
echo "------------------------------------------------------------------------"
echo "exrap_makeprepbufr.sh -"
echo "                Rapid Refresh Full and Partial Cycle PREPBUFR processing"
echo "------------------------------------------------------------------------"
echo "History: Mar 1 2013 - Original script."
################################################################################

set -x

# Make sure we are in the $DATA directory
cd $DATA

msg="HAS BEGUN on `hostname`"
$DATA/postmsg "$jlogfile" "$msg"

cat break > $pgmout

CHGRP_RSTPROD=${CHGRP_RSTPROD:-YES}

export COMSP=${COMSP:-$COMIN/${RUN}.${cycle}.}

cdate10=`cut -c7-16 ncepdate`

#  Unless an overriding value for $errPREPDATA_limit is imported, the highest
#   allowed foreground exit status for program PREPOBS_PREPDATA (where any exit
#   status higher than this is considered a failure) is dependent upon the
#   center hour (cycle) time in which it is running

if [ "$cyc" = '00' -o "$cyc" = '12' ]; then
   export errPREPDATA_limit=${errPREPDATA_limit:-0}
else
   export errPREPDATA_limit=${errPREPDATA_limit:-4}
fi

msg="CENTER TIME FOR PREPBUFR PROCESSING IS $cdate10"
$DATA/postmsg "$jlogfile" "$msg"

ksh ${ushscript_prep}/prepobs_makeprepbufr.sh $cdate10
errsc=$?

[ "$errsc" -ne '0' ]  &&  exit $errsc

if [ "$CHGRP_RSTPROD" = 'YES' ]; then
   msg="NOTE: These files (if present) are RESTRICTED to rstprod group: \
prepbufr_pre-qc, prepbufr, prepbufr.acft_profiles*, acqc_???*, \
acqc_merged*_sorted"
      $DATA/postmsg "$jlogfile" "$msg"
set +x
      echo " "
      echo "$msg"
      echo " "
set -x
fi
warning=no

if [ "$PREPDATA" = 'YES' ]; then

# save snapshot of prepbufr file after PREPOBS_PREPDATA in COMOUT
   cp prepda.prepdata ${COMOUT}/${RUN}.${cycle}.prepbufr_pre-qc.tm00

   if [ "$CHGRP_RSTPROD" = 'YES' ]; then
      chgrp rstprod ${COMOUT}/${RUN}.${cycle}.prepbufr_pre-qc.tm00
      errch=$?
      if [ $errch -eq 0 ]; then
         chmod 640 ${COMOUT}/${RUN}.${cycle}.prepbufr_pre-qc.tm00
      else
         cp /dev/null ${COMOUT}/${RUN}.${cycle}.prepbufr_pre-qc.tm00
         warning=yes
      fi
   fi

# save current prepbufr mnemonic table in COMOUT if either it isn't already
#  there for a previous cycle or if it has changed from a previous cycle
   if [ ! -s ${COMOUT}/*prep.bufrtable ]; then
      cp prep.bufrtable  ${COMOUT}/${RUN}.${cycle}.prep.bufrtable
   else
      diff `ls -t  ${COMOUT}/*prep.bufrtable | head -n1` prep.bufrtable \
       > /dev/null 2>&1
      errdiff=$?
      [ "$errdiff" -ne '0' ]  &&  \
       cp prep.bufrtable  ${COMOUT}/${RUN}.${cycle}.prep.bufrtable
   fi
fi

if [ "$GETGUESS" = 'YES' ]; then

# save global sigma guess file valid at center PREPBUFR date/time (encoded
#  into PREPBUFR file and used by q.c. programs) in COMOUT
   if [ -s sgesprepA ]; then
      cp sgesprep  $COMOUT/${RUN}.${cycle}.sgesprep_before.tm00
      cp sgesprepA $COMOUT/${RUN}.${cycle}.sgesprep_after.tm00
   else
      cp sgesprep $COMOUT/${RUN}.${cycle}.sgesprep.tm00
   fi

# save path name of this global sigma guess file in COMOUT
   if [ -s sgesprepA_pathname ]; then
      cp sgesprep_pathname  \
       $COMOUT/${RUN}.${cycle}.sgesprep_pathname_before.tm00
      cp sgesprepA_pathname \
       $COMOUT/${RUN}.${cycle}.sgesprep_pathname_after.tm00
   else
      cp sgesprep_pathname $COMOUT/${RUN}.${cycle}.sgesprep_pathname.tm00
   fi
fi

if [ "$DO_QC" = 'YES' ]; then

# save final form of prepbufr file in COMOUT
   cp prepda.${cycle} $COMOUT/${RUN}.${cycle}.prepbufr.tm00
   if [ "$CHGRP_RSTPROD" = 'YES' ]; then
      chgrp rstprod $COMOUT/${RUN}.${cycle}.prepbufr.tm00
      errch=$?
      if [ $errch -eq 0 ]; then
         chmod 640 $COMOUT/${RUN}.${cycle}.prepbufr.tm00
      else
         cp /dev/null $COMOUT/${RUN}.${cycle}.prepbufr.tm00
         warning=yes
      fi
   fi
   if [ "$SENDDBN" = 'YES' ] ; then
      $DBNROOT/bin/dbn_alert MODEL FSL_PREPBUFR $job \
       $COMOUT/${RUN}.${cycle}.prepbufr.tm00
   fi

# save prepacqc prepbufr.acft_profiles file in COMOUT
   if [ -s prepbufr.acft_profiles ]; then
      cp prepbufr.acft_profiles  \
       $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles.tm00
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles.tm00
            warning=yes
         fi
      fi
   fi

# save prepacqc prepbufr.acft_profiles_sfc file in COMOUT
   if [ -s prepbufr.acft_profiles_sfc ]; then
      cp prepbufr.acft_profiles_sfc \
       $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles_sfc.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
          chgrp rstprod $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles_sfc.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles_sfc.tm00
         else
            cp /dev/null \
             $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles_sfc.tm00
            warning=yes
         fi
      fi
   fi

# save prepacqc output files in COMOUT
   if [ -s acftqc_*.sus ]; then
      mv acftqc_*.sus acftqc_sus
      cp acftqc_sus $COMOUT/${RUN}.${cycle}.acqc_sus.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_sus.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_sus.tm00
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_sus.tm00
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.stk ]; then
      mv acftqc_*.stk acftqc_stk
      cp acftqc_stk $COMOUT/${RUN}.${cycle}.acqc_stk.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_stk.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_stk.tm00
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_stk.tm00
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.spk ]; then
      mv acftqc_*.spk acftqc_spk
      cp acftqc_spk $COMOUT/${RUN}.${cycle}.acqc_spk.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_spk.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_spk.tm00
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_spk.tm00
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.ord ]; then
      mv acftqc_*.ord acftqc_ord
      cp acftqc_ord $COMOUT/${RUN}.${cycle}.acqc_ord.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_ord.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_ord.tm00
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_ord.tm00
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.lst ]; then
      mv acftqc_*.lst acftqc_lst
      cp acftqc_lst $COMOUT/${RUN}.${cycle}.acqc_lst.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_lst.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_lst.tm00
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_lst.tm00
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.inv ]; then
      mv acftqc_*.inv acftqc_inv
      cp acftqc_inv $COMOUT/${RUN}.${cycle}.acqc_inv.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_inv.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_inv.tm00
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_inv.tm00
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.inc ]; then
      mv acftqc_*.inc acftqc_inc
      cp acftqc_inc $COMOUT/${RUN}.${cycle}.acqc_inc.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_inc.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_inc.tm00
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_inc.tm00
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.grc ]; then
      mv acftqc_*.grc acftqc_grc
      cp acftqc_grc $COMOUT/${RUN}.${cycle}.acqc_grc.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_grc.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_grc.tm00
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_grc.tm00
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.dup ]; then
      mv acftqc_*.dup acftqc_dup
      cp acftqc_dup $COMOUT/${RUN}.${cycle}.acqc_dup.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_dup.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_dup.tm00
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_dup.tm00
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.log ]; then
      mv acftqc_*.log acftqc_log
      cp acftqc_log $COMOUT/${RUN}.${cycle}.acqc_log.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_log.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_log.tm00
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_log.tm00
            warning=yes
         fi
      fi
   fi

   if [ -s merged.reports.post_acftobs_qc.sorted ]; then
      cp merged.reports.post_acftobs_qc.sorted \
       $COMOUT/${RUN}.${cycle}.acqc_merged_sorted.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_merged_sorted.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_merged_sorted.tm00
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_merged_sorted.tm00
            warning=yes
         fi
      fi
   fi

   if [ -s merged.profile_reports.post_acftobs_qc.sorted ]; then
      cp merged.profile_reports.post_acftobs_qc.sorted \
       $COMOUT/${RUN}.${cycle}.acqc_merged.prof_sorted.tm00
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_merged.prof_sorted.tm00
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_merged.prof_sorted.tm00
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_merged.prof_sorted.tm00
            warning=yes
         fi
      fi
   fi

# save cqcbufr output files in COMOUT
   touch cqc_events
   cp cqc_events $COMOUT/${RUN}.${cycle}.cqc_events.tm00
   touch cqc_stncnt
   cp cqc_stncnt $COMOUT/${RUN}.${cycle}.cqc_stncnt.tm00
   touch cqc_stnlst
   cp cqc_stnlst $COMOUT/${RUN}.${cycle}.cqc_stnlst.tm00
   touch cqc_sdm
   cp cqc_sdm $COMOUT/${RUN}.${cycle}.cqc_sdm.tm00
   touch cqc_radcor
   cp cqc_radcor $COMOUT/${RUN}.${cycle}.cqc_radcor.tm00

# save cqcvad bird algorithm q.c. info list in COMOUT
   touch cqcvad.birdqc
   cp cqcvad.birdqc  $COMOUT/${RUN}.${cycle}.cqcvad.birdqc.tm00

fi

if [ "$warning" = 'yes' ]; then
   msg="**WARNING: Since user $USER is not in rstprod group all RESTRICTED \
files are replaced with a null file"
   $DATA/postmsg "$jlogfile" "$msg"
set +x
   echo " "
   echo "$msg"
   echo " "
set -x
fi

###############################################################

# GOOD RUN
set +x
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
set -x



# save standard output
cat break $pgmout break > allout
cat allout
# rm allout

sleep 10

msg='ENDED NORMALLY.'
$DATA/postmsg "$jlogfile" "$msg"

################## END OF SCRIPT ##############################
