#!/bin/sh

###########################################################################
echo "--------------------------------------------------------------------"
echo "excdas_makeprepbufr.sh - CDAS model prepbufr processing         "
echo "--------------------------------------------------------------------"
echo "History: Dec  3 2014 - Original script, split off from              "
echo "                       exglobal_makeprepbufr.sh.ecf and tailored    "
echo "                       exclusively to CDAS.                         "
echo "         Aug 18 2017 - Use variable $RUN instead of $model to define"
echo "                       input directories.                           "
###########################################################################

set -x

# Make sure we are in the $DATA directory
cd $DATA

msg="HAS BEGUN on `hostname`"
$DATA/postmsg "$jlogfile" "$msg"

cat break > $pgmout

CHGRP_RSTPROD=${CHGRP_RSTPROD:-YES}

export COMSP=${COMSP:-$COMIN/${RUN}.${cycle}.}


if [ "$DO_QC" = 'YES' -a "$CQCBUFR" = 'YES' -a -n "$COM_IN" -a -n "$CQCC" ];then

# If running PREPOBS_CQCBUFR, must check its data cards to see if
#  namelist switch DOTMP is TRUE - if so, must get prepbufr_pre-qc files
#   from t-24, t-12, t+12, t+24 to feed into PREPOBS_CQCBUFR

   DOTMP=`grep DOTMP $CQCC | awk -F, \
    '{print $1; print $2; print $3; print $4; print$5}' | grep DOTMP | \
    awk -F= '{print $2}'`

   if [[ $DOTMP = *T* ]]; then
      [ -s ${COM_IN}/${RUN}.${PDYm1}/${RUN}.${cycle}.prepbufr_pre-qc ] && \
       export PRPI_m24=\
${COM_IN}/${RUN}.${PDYm1}/${RUN}.${cycle}.prepbufr_pre-qc
      [ -s ${COM_IN}/${RUN}.${PDYp1}/${RUN}.${cycle}.prepbufr_pre-qc ] && \
       export PRPI_p24=\
${COM_IN}/${RUN}.${PDYp1}/${RUN}.${cycle}.prepbufr_pre-qc
      tdate10=`$NDATE -12 $PDY$cyc`
      cyc_m12=`echo $tdate10|cut -c9-10`
      pdy_m12=`echo $tdate10|cut -c1-8`
      [ -s ${COM_IN}/${RUN}.${pdy_m12}/${RUN}.t${cyc_m12}z.prepbufr_pre-qc ] \
       && export PRPI_m12=\
${COM_IN}/${RUN}.${pdy_m12}/${RUN}.t${cyc_m12}z.prepbufr_pre-qc
      tdate10=`$NDATE +12 $PDY$cyc`
      cyc_p12=`echo $tdate10|cut -c9-10`
      pdy_p12=`echo $tdate10|cut -c1-8`
      [ -s ${COM_IN}/${RUN}.${pdy_p12}/${RUN}.t${cyc_p12}z.prepbufr_pre-qc ] \
       && export PRPI_p12=\
${COM_IN}/${RUN}.${pdy_p12}/${RUN}.t${cyc_p12}z.prepbufr_pre-qc
   fi
fi

cdate10=`cut -c7-16 ncepdate`

msg="CENTER TIME FOR PREPBUFR PROCESSING IS $cdate10"
$DATA/postmsg "$jlogfile" "$msg"

sh $ushscript_prep/prepobs_makeprepbufr.sh $cdate10
errsc=$?

[ "$errsc" -ne '0' ]  &&  exit $errsc

if [ "$CHGRP_RSTPROD" = 'YES' ]; then
   msg="NOTE: These files (if present) are RESTRICTED to rstprod group: \
prepbufr_pre-qc, prepbufr, prepbufr.acft_profiles*, acqc_???*, \
acqc_merged*_sorted, tosslist, prepbufr.unblok"
   $DATA/postmsg "$jlogfile" "$msg"
set +x
   echo
   echo "$msg"
   echo
set -x
fi
warning=no

if [ "$PREPDATA" = 'YES' ]; then

# save snapshot of prepbufr file after PREPOBS_PREPDATA in COMOUT
   cpfs prepda.prepdata $COMOUT/${RUN}.${cycle}.prepbufr_pre-qc
   chmod 664 $COMOUT/${RUN}.${cycle}.prepbufr_pre-qc

   if [ "$CHGRP_RSTPROD" = 'YES' ]; then
      chgrp rstprod $COMOUT/${RUN}.${cycle}.prepbufr_pre-qc
      errch=$?
      if [ $errch -eq 0 ]; then
         chmod 640 $COMOUT/${RUN}.${cycle}.prepbufr_pre-qc
      else
         cpfs /dev/null $COMOUT/${RUN}.${cycle}.prepbufr_pre-qc
         warning=yes
      fi
   fi

# save current prepbufr mnemonic table in COMOUT if either it isn't already
#  there for a previous cycle or if it has changed from a previous cycle
   if [ ! -s $COMOUT/*prep.bufrtable ]; then
      cpfs prep.bufrtable  $COMOUT/${RUN}.${cycle}.prep.bufrtable
   else
      diff `ls -t  $COMOUT/*prep.bufrtable | head -n1` prep.bufrtable \
       > /dev/null 2>&1
      errdiff=$?
      [ "$errdiff" -ne '0' ]  &&  \
       cpfs prep.bufrtable  $COMOUT/${RUN}.${cycle}.prep.bufrtable
   fi
fi

# NOTE: Tropical cyclone relocation processing currently does not run in the
#       CDAS network.  This logic is copied from exglobal_makeprepbufr.sh.ecf
#       in case it ever does.
# ---------------------------------------------------------------------------
# save global sigma guess file(s) in COMOUT if they haven't already been saved
#  here by previous tropical cyclone relocation processing
[ -s sgm3prep -a ! -s $COMOUT/${RUN}.${cycle}.sgm3prep ]  &&  \
 cpfs sgm3prep $COMOUT/${RUN}.${cycle}.sgm3prep
[ -s sgp3prep -a ! -s $COMOUT/${RUN}.${cycle}.sgp3prep ]  &&  \
 cpfs sgp3prep $COMOUT/${RUN}.${cycle}.sgp3prep
if [ -s sgesprep ]; then
   if [ -s sgesprepA ]; then
      cpfs sgesprep  $COMOUT/${RUN}.${cycle}.sgesprep_before
      cpfs sgesprepA $COMOUT/${RUN}.${cycle}.sgesprep_after
   else
      [ ! -s $COMOUT/${RUN}.${cycle}.sgesprep ]  &&  \
       cpfs sgesprep $COMOUT/${RUN}.${cycle}.sgesprep
   fi

# save path name of global sigma guess file valid at center PREPBUFR
#  date/time (encoded into PREPBUFR file and used by q.c. programs) in COMOUT
   if [ "$GETGUESS" = 'YES' ]; then
      if [ -s sgesprepA_pathname ]; then
         cpfs sgesprep_pathname \
          $COMOUT/${RUN}.${cycle}.sgesprep_pathname_before.$tmmark
         cpfs sgesprepA_pathname \
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
         cpfs sgesprep_pathname $COMOUT/${RUN}.${cycle}.sgesprep_pathname.$tmmark
      fi
   fi
fi

# save synthetic bogus files in COMOUT (currently does not run in CDAS network)
[ -s bogrept ]  &&  cpfs bogrept  $COMOUT/${RUN}.${cycle}.syndata.bogrept
[ -s bogdata ]  &&  cpfs bogdata  $COMOUT/${RUN}.${cycle}.syndata.bogdata
[ -s dthistry ] &&  cpfs dthistry $COMOUT/${RUN}.${cycle}.syndata.dthistry

if [ "$DO_QC" = 'YES' ]; then

# save final form of prepbufr file in COMOUT
   cpfs prepda.${cycle} $COMOUT/${RUN}.${cycle}.prepbufr
   chmod 664 $COMOUT/${RUN}.${cycle}.prepbufr
   if [ "$CHGRP_RSTPROD" = 'YES' ]; then
      chgrp rstprod $COMOUT/${RUN}.${cycle}.prepbufr
      errch=$?
      if [ $errch -eq 0 ]; then
         chmod 640 $COMOUT/${RUN}.${cycle}.prepbufr
      else
         cpfs /dev/null $COMOUT/${RUN}.${cycle}.prepbufr
         warning=yes
      fi
   fi

# save prepacqc prepbufr.acft_profiles file in COMOUT
   if [ -s prepbufr.acft_profiles ]; then
      cpfs prepbufr.acft_profiles  $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles
            warning=yes
         fi
      fi
   fi

# save prepacqc prepbufr.acft_profiles_sfc file in COMOUT
   if [ -s prepbufr.acft_profiles_sfc ]; then
      cpfs prepbufr.acft_profiles_sfc  \
       $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles_sfc
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles_sfc
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles_sfc
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.prepbufr.acft_profiles_sfc
            warning=yes
         fi
      fi
   fi

# save prepacqc output files in COMOUT
   if [ -s acftqc_*.sus ]; then
      mv acftqc_*.sus acftqc_sus
      cpfs acftqc_sus $COMOUT/${RUN}.${cycle}.acqc_sus
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_sus
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_sus
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.acqc_sus
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.stk ]; then
      mv acftqc_*.stk acftqc_stk
      cpfs acftqc_stk $COMOUT/${RUN}.${cycle}.acqc_stk
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_stk
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_stk
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.acqc_stk
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.spk ]; then
      mv acftqc_*.spk acftqc_spk
      cpfs acftqc_spk $COMOUT/${RUN}.${cycle}.acqc_spk
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_spk
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_spk
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.acqc_spk
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.ord ]; then
      mv acftqc_*.ord acftqc_ord
      cpfs acftqc_ord $COMOUT/${RUN}.${cycle}.acqc_ord
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_ord
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_ord
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.acqc_ord
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.lst ]; then
      mv acftqc_*.lst acftqc_lst
      cpfs acftqc_lst $COMOUT/${RUN}.${cycle}.acqc_lst
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_lst
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_lst
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.acqc_lst
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.inv ]; then
      mv acftqc_*.inv acftqc_inv
      cpfs acftqc_inv $COMOUT/${RUN}.${cycle}.acqc_inv
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_inv
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_inv
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.acqc_inv
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.inc ]; then
      mv acftqc_*.inc acftqc_inc
      cpfs acftqc_inc $COMOUT/${RUN}.${cycle}.acqc_inc
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_inc
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_inc
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.acqc_inc
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.grc ]; then
      mv acftqc_*.grc acftqc_grc
      cpfs acftqc_grc $COMOUT/${RUN}.${cycle}.acqc_grc
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_grc
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_grc
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.acqc_grc
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.dup ]; then
      mv acftqc_*.dup acftqc_dup
      cpfs acftqc_dup $COMOUT/${RUN}.${cycle}.acqc_dup
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_dup
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_dup
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.acqc_dup
            warning=yes
         fi
      fi
   fi

   if [ -s acftqc_*.log ]; then
      mv acftqc_*.log acftqc_log
      cpfs acftqc_log $COMOUT/${RUN}.${cycle}.acqc_log
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_log
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_log
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.acqc_log
            warning=yes
         fi
      fi
   fi

   if [ -s merged.reports.post_acftobs_qc.sorted ]; then
      cpfs merged.reports.post_acftobs_qc.sorted \
       $COMOUT/${RUN}.${cycle}.acqc_merged_sorted
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_merged_sorted
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_merged_sorted
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.acqc_merged_sorted
            warning=yes
         fi
      fi
   fi

   if [ -s merged.profile_reports.post_acftobs_qc.sorted ]; then
      cpfs merged.profile_reports.post_acftobs_qc.sorted \
       $COMOUT/${RUN}.${cycle}.acqc_merged.prof_sorted
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.acqc_merged.prof_sorted
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.acqc_merged.prof_sorted
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.acqc_merged.prof_sorted
            warning=yes
         fi
      fi
   fi

# save cqcbufr output files in COMOUT
   touch cqc_events
   cpfs cqc_events $COMOUT/${RUN}.${cycle}.cqc_events
   touch cqc_stncnt
   cpfs cqc_stncnt $COMOUT/${RUN}.${cycle}.cqc_stncnt
   touch cqc_stnlst
   cpfs cqc_stnlst $COMOUT/${RUN}.${cycle}.cqc_stnlst
   touch cqc_sdm
   cpfs cqc_sdm $COMOUT/${RUN}.${cycle}.cqc_sdm
   touch cqc_radcor
   cpfs cqc_radcor $COMOUT/${RUN}.${cycle}.cqc_radcor

# save oiqc tosslist in COMOUT (if it runs)
   if [ -s tosslist ]; then
      cpfs tosslist  $COMOUT/${RUN}.${cycle}.tosslist
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.tosslist
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.tosslist
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.tosslist
            warning=yes
         fi
      fi
   fi

# make unblocked prepbufr file
# ---> ON WCOSS prepbufr is already unblocked, so for now just copy it to the
#      unblok file location used before on CCS - hopefully this can be removed
#      someday!
   cp -p  prepda.${cycle} prepda.${cycle}.unblok
   err_cp=$?
   if [ $err_cp -eq 0 ]; then
      cpfs prepda.${cycle}.unblok $COMOUT/${RUN}.${cycle}.prepbufr.unblok
      chmod 664 $COMOUT/${RUN}.${cycle}.prepbufr.unblok
      if [ "$CHGRP_RSTPROD" = 'YES' ]; then
         chgrp rstprod $COMOUT/${RUN}.${cycle}.prepbufr.unblok
         errch=$?
         if [ $errch -eq 0 ]; then
            chmod 640 $COMOUT/${RUN}.${cycle}.prepbufr.unblok
         else
            cpfs /dev/null $COMOUT/${RUN}.${cycle}.prepbufr.unblok
            warning=yes
         fi
      fi
   fi

fi

if [ "$warning" = 'yes' ]; then
   msg="**WARNING: Since user $USER is not in rstprod group all RESTRICTED \
files are replaced with a null file"
   $DATA/postmsg "$jlogfile" "$msg"
set +x
   echo
   echo "$msg"
   echo
set -x
fi

if [ "$SENDCOM" = 'YES' -a "$COPY_TO_ARKV" = 'YES' ]; then

#=====================================================================
#=====================================================================
#  Copy PREP files to arkv directory, preserving ownership & group id
#   but updating timestamp
#=====================================================================
#=====================================================================

   msg="Copy PREP files to $COMARC"
   $DATA/postmsg "$jlogfile" "$msg"

   if [ $JOB_NUMBER -eq 1 ]; then
      cp --preserve=mode,ownership \
       $COMOUT/cdas.${cycle}.prepbufr_pre-qc ${COMARC}/prepbufr${PDY}${cyc}
######cp --preserve=mode,ownership $LSB_OUTPUTFILE \
###### ${COMARC}/prep1${PDY}${cyc}.out # moved down in script to include stdout
   elif [ $JOB_NUMBER -eq 2 ]; then
      [ -s cqc_events ] && cp --preserve=mode,ownership cqc_events \
       ${COMARC}/cqc_events.${PDY}${cyc}
      [ -s cqc_stncnt ] && cp --preserve=mode,ownership cqc_stncnt \
       ${COMARC}/cqc_stncnt.${PDY}${cyc}
      [ -s cqc_stnlst ] && cp --preserve=mode,ownership cqc_stnlst \
       ${COMARC}/cqc_stnlst.${PDY}${cyc}
      [ -s cqc_wndpbm ] && cp --preserve=mode,ownership cqc_wndpbm \
       ${COMARC}/cqc_wndpbm.${PDY}${cyc}
      [ -s tosslist ] && cp --preserve=mode,ownership tosslist \
       ${COMARC}/tosscat${PDY}${cyc}
######cp --preserve=mode,ownership $LSB_OUTPUTFILE \
###### ${COMARC}/prep2${PDY}${cyc}.out # moved down in script to include stdout
   fi
fi

########################################################

# GOOD RUN
set +x
echo
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo
set -x


# save standard output
cat break $pgmout break > allout
cat allout
# rm allout


#if [ "$SENDCOM" = 'YES' -a "$COPY_TO_ARKV" = 'YES' ]; then
#
##===========================================================================
##===========================================================================
##  Must wait until here to copy "dayfiles" to arkv directory so that stdout
##   is included (preserve ownership & group id but update timestamp)
##===========================================================================
##===========================================================================
#
#   msg="Copy dayfile to $COMARC"
#   $DATA/postmsg "$jlogfile" "$msg"
#
#   if [ $JOB_NUMBER -eq 1 ]; then
#      cp --preserve=mode,ownership $LSB_OUTPUTFILE ${COMARC}/prep1${PDY}${cyc}.out
#   elif [ $JOB_NUMBER -eq 2 ]; then
#      cp --preserve=mode,ownership $LSB_OUTPUTFILE ${COMARC}/prep2${PDY}${cyc}.out
#   fi
#fi

sleep 10

msg='ENDED NORMALLY.'
$DATA/postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
