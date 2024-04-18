#!/bin/ksh
# Run under ksh

################################################################
echo "---------------------------------------------------------"
echo "exnam_makeprepbufr.sh   -   NAM model prepbufr processing"
echo "---------------------------------------------------------"
echo "History: Mar 1 2013 - Original script."
################################################################

set -x

# Make sure we are in the $DATA directory
cd $DATA

msg="HAS BEGUN on `hostname`"
postmsg "$jlogfile" "$msg"

cat break > $pgmout

CHGRP_RSTPROD=${CHGRP_RSTPROD:-YES}

export COMSP=${COMSP:-$COMIN/${RUN}.${cycle}.}
# secure turn off SYNDAT for nam
# export COMSPtcvital=${COMSPtcvital:-$COMINtcvital/${RUN}.${cycle}.}

tmhr=`echo $tmmark|cut -c3-4`
cdate10=`$NDATE -$tmhr $PDY$cyc`
cycp=`echo $cdate10|cut -c9-10`

#  Unless an overriding value for $errPREPDATA_limit is imported, the highest
#   allowed foreground exit status for program PREPOBS_PREPDATA (where any exit
#   status higher than this is considered a failure) is dependent upon the
#   center hour time in which it is running

if [ "$cycp" = '00' -o "$cycp" = '12' ]; then
   export errPREPDATA_limit=${errPREPDATA_limit:-0}
else
   export errPREPDATA_limit=${errPREPDATA_limit:-4}
fi

RUN_uc=$(echo $RUN | tr [a-z] [A-Z])
tmmark_uc=$(echo $tmmark | tr [a-z] [A-Z])

msg="$RUN_uc ANALYSIS TIME IS $PDY$cyc"
$DATA/postmsg "$jlogfile" "$msg"

msg="CENTER PREPBUFR PROCESSING TIME FOR $tmmark_uc $RUN_uc IS $cdate10"
postmsg "$jlogfile" "$msg"

ksh ${ushscript_prep}/prepobs_makeprepbufr.sh $cdate10
errsc=$?

[ "$errsc" -ne '0' ]  &&  exit $errsc

if [ "$CHGRP_RSTPROD" = 'YES' ]; then
   msg="NOTE: These files (if present) are RESTRICTED to rstprod group: \
prepbufr_pre-qc, prepbufr, prepbufr.acft_profiles*, acqc_???*, \
acqc_merged*_sorted"
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
   cp prepda.prepdata $COMOUT/${RUN}.${cycle}.prepbufr_pre-qc.$tmmark

   if [ "$CHGRP_RSTPROD" = 'YES' ]; then
      chgrp rstprod $COMOUT/${RUN}.${cycle}.prepbufr_pre-qc.$tmmark
      errch=$?
      if [ $errch -eq 0 ]; then
         chmod 640 $COMOUT/${RUN}.${cycle}.prepbufr_pre-qc.$tmmark
      else
         cp /dev/null $COMOUT/${RUN}.${cycle}.prepbufr_pre-qc.$tmmark
         warning=yes
      fi
   fi

   if [ "$tmmark" = 'tm06' -o "$tmmark" = 'tm00' ]; then

# for tm06 or tm00 runs (only) save current prepbufr mnemonic table in
#  COMOUT if either it isn't already there for a previous cycle at tm06 or
#  tm00 or if it has changed from a previous cycle at tm06 or tm00
      if [ ! -s $COMOUT/*prep.bufrtable.$tmmark ]; then
         cp prep.bufrtable  $COMOUT/${RUN}.${cycle}.prep.bufrtable.$tmmark
      else
         diff `ls -t  $COMOUT/*prep.bufrtable.$tmmark | head -n1` prep.bufrtable \
          > /dev/null 2>&1
         errdiff=$?
         [ "$errdiff" -ne '0' ]  &&  \
          cp prep.bufrtable  $COMOUT/${RUN}.${cycle}.prep.bufrtable.$tmmark
      fi
   fi
fi

# save global sigma guess file valid at center PREPBUFR date/time in COMOUT if
#  it hasn't already been saved here by previous tropical cyclone relocation
#  processing (encoded into PREPBUFR file and used by q.c. programs)
if [ -s sgesprep ]; then
   if [ -s sgesprepA ]; then
      cp sgesprep  $COMOUT/${RUN}.${cycle}.sgesprep_before.$tmmark
      cp sgesprepA $COMOUT/${RUN}.${cycle}.sgesprep_after.$tmmark
   else
      [ ! -s $COMOUT/${RUN}.${cycle}.sgesprep.$tmmark ]  &&  \
       cp sgesprep $COMOUT/${RUN}.${cycle}.sgesprep.$tmmark
   fi

# save path name of this global sigma guess file in COMOUT
   if [ "$GETGUESS" = 'YES' ]; then
      if [ -s sgesprepA_pathname ]; then
         cp sgesprep_pathname  \
          $COMOUT/${RUN}.${cycle}.sgesprep_pathname_before.$tmmark
         cp sgesprepA_pathname \
          $COMOUT/${RUN}.${cycle}.sgesprep_pathname_after.$tmmark
      else

#   if the target file already exists, it was created in previous
#    tropcy_relocate.sh script because either there was an error or no
#    tcvitals were present - in this case the target file points to the orig.
#    getges global sigma guess (since the guess was not modified by relocation)
#    - otherwise sgesprep_pathname will either contain either the path to the
#      getges guess (if tropical cyclone relocation did not run previously) or
#      it will contain the path to the modified sgesprep guess (if tropical
#      cyclone relocation did run previously and did modify the guess)
#   ---------------------------------------------------------------------------

         [ ! -s $COMOUT/${RUN}.${cycle}.sgesprep_pathname.$tmmark ]  &&  \
         cp sgesprep_pathname $COMOUT/${RUN}.${cycle}.sgesprep_pathname.$tmmark
      fi
   fi
fi

# save synthetic bogus files in COMOUT
[ -s bogrept ]  &&  cp bogrept  $COMOUT/${RUN}.${cycle}.syndata.bogrept.$tmmark
[ -s bogdata ]  &&  cp bogdata  $COMOUT/${RUN}.${cycle}.syndata.bogdata.$tmmark
[ -s dthistry ] &&  cp dthistry $COMOUT/${RUN}.${cycle}.syndata.dthistry.$tmmark

if [ "$DO_QC" = 'YES' ]; then

# save final form of prepbufr file in COMOUT
   cp prepda.t${cycp}z $COMOUT/${RUN}.${cycle}.prepbufr.$tmmark
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

# save prepacqc prepbufr.acft_profiles file in COMOUT
   if [ -s prepbufr.acft_profiles ]; then
      cp prepbufr.acft_profiles \
       $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles.$tmmark
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles.$tmmark
            warning=yes
         fi
      fi
   fi

# save prepacqc prepbufr.acft_profiles_sfc file in COMOUT
   if [ -s prepbufr.acft_profiles_sfc ]; then
      cp prepbufr.acft_profiles_sfc \
       $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles_sfc.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
        chgrp rstprod $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles_sfc.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles_sfc.$tmmark
         else
            cp /dev/null \
             $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles_sfc.$tmmark
            warning=yes
         fi
      fi
   fi

# save prepacqc output files in COMOUT
   if [ -s acftqc_*.sus ]; then
      mv acftqc_*.sus acftqc_sus
      cp acftqc_sus $COMOUT/${RUN}.${cycle}.acqc_sus.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_sus.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_sus.$tmmark
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_sus.$tmmark
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.stk ]; then
      mv acftqc_*.stk acftqc_stk
      cp acftqc_stk $COMOUT/${RUN}.${cycle}.acqc_stk.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_stk.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_stk.$tmmark
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_stk.$tmmark
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.spk ]; then
      mv acftqc_*.spk acftqc_spk
      cp acftqc_spk $COMOUT/${RUN}.${cycle}.acqc_spk.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_spk.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_spk.$tmmark
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_spk.$tmmark
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.ord ]; then
      mv acftqc_*.ord acftqc_ord
      cp acftqc_ord $COMOUT/${RUN}.${cycle}.acqc_ord.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_ord.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_ord.$tmmark
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_ord.$tmmark
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.lst ]; then
      mv acftqc_*.lst acftqc_lst
      cp acftqc_lst $COMOUT/${RUN}.${cycle}.acqc_lst.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_lst.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_lst.$tmmark
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_lst.$tmmark
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.inv ]; then
      mv acftqc_*.inv acftqc_inv
      cp acftqc_inv $COMOUT/${RUN}.${cycle}.acqc_inv.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_inv.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_inv.$tmmark
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_inv.$tmmark
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.inc ]; then
      mv acftqc_*.inc acftqc_inc
      cp acftqc_inc $COMOUT/${RUN}.${cycle}.acqc_inc.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_inc.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_inc.$tmmark
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_inc.$tmmark
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.grc ]; then
      mv acftqc_*.grc acftqc_grc
      cp acftqc_grc $COMOUT/${RUN}.${cycle}.acqc_grc.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_grc.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_grc.$tmmark
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_grc.$tmmark
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.dup ]; then
      mv acftqc_*.dup acftqc_dup
      cp acftqc_dup $COMOUT/${RUN}.${cycle}.acqc_dup.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_dup.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_dup.$tmmark
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_dup.$tmmark
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.log ]; then
      mv acftqc_*.log acftqc_log
      cp acftqc_log $COMOUT/${RUN}.${cycle}.acqc_log.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_log.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_log.$tmmark
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_log.$tmmark
            warning=yes
         fi
      fi
   fi

   if [ -s merged.reports.post_acftobs_qc.sorted ]; then
      cp merged.reports.post_acftobs_qc.sorted \
       $COMOUT/${RUN}.${cycle}.acqc_merged_sorted.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_merged_sorted.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_merged_sorted.$tmmark
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_merged_sorted.$tmmark
            warning=yes
         fi
      fi
   fi

   if [ -s merged.profile_reports.post_acftobs_qc.sorted ]; then
      cp merged.profile_reports.post_acftobs_qc.sorted \
       $COMOUT/${RUN}.${cycle}.acqc_merged.prof_sorted.$tmmark
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_merged.prof_sorted.$tmmark
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_merged.prof_sorted.$tmmark
         else
            cp /dev/null $COMOUT/${RUN}.${cycle}.acqc_merged.prof_sorted.$tmmark
            warning=yes
         fi
      fi
   fi

# save cqcbufr output files in COMOUT
   touch cqc_events
   cp cqc_events $COMOUT/${RUN}.${cycle}.cqc_events.$tmmark
   touch cqc_stncnt
   cp cqc_stncnt $COMOUT/${RUN}.${cycle}.cqc_stncnt.$tmmark
   touch cqc_stnlst
   cp cqc_stnlst $COMOUT/${RUN}.${cycle}.cqc_stnlst.$tmmark
   touch cqc_sdm
   cp cqc_sdm $COMOUT/${RUN}.${cycle}.cqc_sdm.$tmmark
   touch cqc_radcor
   cp cqc_radcor $COMOUT/${RUN}.${cycle}.cqc_radcor.$tmmark

# save cqcvad bird algorithm q.c. info list in COMOUT
   touch cqcvad.birdqc
   cp cqcvad.birdqc  $COMOUT/${RUN}.${cycle}.cqcvad.birdqc.$tmmark

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
