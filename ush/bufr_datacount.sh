#!/bin/ksh
echo
echo "------------------------------------------------"
echo "Datacount Reporting for the SDM"
echo "------------------------------------------------"
echo "Latest update:"
echo "  Mar 2017 DStokes - Cray port: Use mail.py utility in order to send"
echo "                     dump status info to SDM from any node"
echo "------------------------------------------------"
echo
set -aux

################################################################################
#
# additional modification history
#  MAR 2000 Sager  - First implementation of this new script.
#  AUG 2000 Keyser - Redesign.
#  OCT 2010 Malone - No longer print to SDM print.  Rather e-mail SDM account.
#  12 Mar 2013 JWhiting - ported to WCOSS (Linux) platforms:
#                - cp'd CCS ver of 5 Jan 2009
#                - updated logical unit links (rm'd XLF*)
#                - replaced "timex" w/ "time -p"
#  14 Sep 2015 SMelchior - modified sed commands to allow for more
#                  generalized search matches.
#  10 Jan 2017 D, Keyser - Minor comment and echo changes to account
#                  for NAM having multiple $tmmark times after NAMv4
#                  (still consider only tm00). Added $tmmark qualifier
#                  to end of output dump alert and log filenames.
#  26 Mar 2017 DStokes   - Cray port: Replaced mail command with mail.py utility
#                  in order to send dump status info to SDM from any node
#  11 Apr 2018 SMelchior - removed final "gdas1" usage.
#  15 Dec 2021 NEsposito - update HOMEbufr_util to HOMEbufr_dump
#
################################################################################
#
# Imported variables that must be passed in:
#   DATA        - path to working directory
#   job         - job name (e.g., 'gdas_dump_post_12' or
#                                 'nam_dump_post_tm00_12')
#   NET         - network  {'nam' (tm00), 'gfs', or 'gdas'}
#                 NOTE: NET is changed to 'gdas' in the parent Job script
#                       for the 'gdas' RUN (was 'gfs' - NET remains 'gfs' for
#                       'gfs' RUN)
#   RUN         - analysis/model run  {'nam' (tm00), 'gfs', or 'gdas'}
#   COMIN       - FIRST choice for path to input $COMROOT directory containing
#                 the dump status file
#   COMOUT      - path to output $COMROOT directory; SECOND choice for path to
#                 input $COMROOT directory containing the dump status file (if dump
#                 status file not found in $COMIN)
#   PDY         - analysis YYYYMMDD
#   cycle       - analysis cycle time (e.g., 't00z')
#   tmmark      - dump time relative to cycle time (e.g., 'tm00')
#                  Note: Only 'tm00' currently valid
#   dumptime    - data dump center time in YYYYMMDDHH
#   pgmout      - string indicating path to for standard output file
#   jlogfile    - string indicating path to job log file
#   AVGDarch_IN - path to directory containing 30-day average files
#   ALERTL      - path to directory containing alert logs
#   SENDCOM     - send files to output $COMROOT directory
#   SENDECF     - flag events on ECF
#   SENDSDM     - email files to SDM 
#
# Imported Variables that must be passed in under certain conditions:
#   EXECobsproc
#               - formerly EXECobsproc_dump_post prior to WCOSS2.  Path to
#                 obsproc executable directory containing bufr_datacount.
#                 (invoked only if $DTCX is not imported)
#   FIXbufr_dump
#               - formerly FIXobsproc_shared_bufr_dumplist prior to WCOSS2
#               - path to obsproc fix directory containing the
#                 bufr_dumplist file
#                 (invoked only if $LIST_CNT is not imported)
#
# Imported variables that can be passed in:
#   DTCX        - path to bufr_datacount executable
#                  (defaults to $EXECobsproc/bufr_datacount if not passed in)
#   LIST_CNT    - path to the bufr_dumplist file
#                  (defaults to $FIXbufr_dump/bufr_dumplist
#                   if not passed in)


DTCX=${DTCX:-$EXECobsproc/bufr_datacount}
LIST_CNT=${LIST_CNT:-$FIXbufr_dump/bufr_dumplist}

cd $DATA

tmmark_uc=$(echo $tmmark | tr [a-z] [A-Z])
NET_uc=$(echo $NET | tr [a-z] [A-Z])
cycle_uc=$(echo $cycle | tr [a-z] [A-Z] | cut -c2-)

msg="CHECK DATA COUNTS FOR $tmmark_uc $NET_uc FOR $dumptime"
postmsg "$jlogfile" "$msg"

RETC=0
#  --> after NAMv4 impl. it makes sense to add $tmmark to end of below filename
#      (eventually transition JDUMP_ALERT to read new form of filename and
#       delete old form of filename)
echo $RETC > $COMOUT/${RUN}.${cycle}.dump_alert_flag
echo $RETC > $COMOUT/${RUN}.${cycle}.dump_alert_flag.$tmmark


# ------------------------------------------------------------------------
# Subset the bufr_dumplist, bufr_d status, current 30-day avg,
#  3-month ago 30-day avg, 6-month ago 30-day avg, 9-month ago 30-day avg,
#  and 12-month ago 30-day avg files
# ------------------------------------------------------------------------

grep --text -e "^_" -e "^:" $LIST_CNT | grep -Fe "#>" > dumplist

if [ -s $COMIN/${RUN}.${cycle}.status.tm00.bufr_d ]; then
   status_bufr_d=$COMIN/${RUN}.${cycle}.status.tm00.bufr_d
elif [ -s $COMOUT/${RUN}.${cycle}.status.tm00.bufr_d ]; then
   status_bufr_d=$COMOUT/${RUN}.${cycle}.status.tm00.bufr_d
else
   status_bufr_d=/dev/null
fi
grep --text -Fe " REPORTS" $status_bufr_d  | grep -Fe " in data group " | \
 grep -Fe " HAS" | grep -v -Fe "Domain" > delete
awk -F" HAS" '{print$2}' delete | awk -F" REPORTS" '{print$1}' | \
 sed "s/[^0-9]/ /g" > delete.r
awk -F" in data group " '{print $1}' delete | \
 sed "s/[^0-9][^0-9][^0-9]^\.[^0-9][^0-9][^0-9]/ /g" | \
 sed "s/[^0-9]/ /g" > delete.l
paste delete.l delete.r > ${RUN}.${cycle}.status.tm00.bufr_d
rm delete delete.l delete.r

> obcount_30davg.${NET}.current
if [ -s $AVGDarch_IN/obcount_30davg.${NET}.current ]; then
   grep --text "^#   " $AVGDarch_IN/obcount_30davg.${NET}.current | \
    sed "s/#  ......./     /g" | sed "s/[^0-9]/ /g" \
    > obcount_30davg.${NET}.current
fi

typeset -Z2 this_month arch_month
typeset -Z4 this_year  arch_year
this_month=`echo $PDY | cut -c5-6`
this_year=`echo $PDY | cut -c1-4`
for months_ago in 3 6 9 12; do
   a_month=`expr $this_month - $months_ago`
   arch_year=$this_year
   if [ "$a_month" -le '0' ]; then
      a_month=`expr $a_month + 12`
      arch_year=`expr $arch_year - 1`
   fi
   arch_month=$a_month

   > obcount_30davg.${NET}.${months_ago}months_ago
   if [ -s $AVGDarch_IN/obcount_30davg.${NET}.${arch_year}${arch_month} ]; then
     grep --text "^#   " $AVGDarch_IN/obcount_30davg.${NET}.${arch_year}${arch_month} | \
       sed "s/#  ......./     /g" | sed "s/[^0-9]/ /g" \
       > obcount_30davg.${NET}.${months_ago}months_ago
   fi
done


# ---------------------------------------------------------------------------
# Pass in script info to bufr_datacount program via standard input data cards
# ---------------------------------------------------------------------------

cat <<EOFd > datacount.cards
 &INPUT
  cdate10  = '$dumptime',
  network  = '$NET_uc',
  tmmark   = '$tmmark_uc',
  cycle    = '$cycle_uc',
  job      = '$job',
  currdate = '`date -u`',
  klimit   = 20,

  skip(021,025,1,1,1) = TRUE,  ! skip 1bhrs3 in NAM 00Z (tm00), current cnt only
  skip(021,025,1,2,1) = TRUE,  ! skip 1bhrs3 in NAM 06Z (tm00), current cnt only
  skip(021,025,1,3,1) = TRUE,  ! skip 1bhrs3 in NAM 12Z (tm00), current cnt only
  skip(021,025,2,2,1) = TRUE,  ! skip 1bhrs3 in GFS 06Z, current count only

  skip(021,024,1,1,1) = TRUE,  ! skip 1bamub in NAM 00Z (tm00), current cnt only
  skip(021,024,1,2,1) = TRUE,  ! skip 1bamub in NAM 06Z (tm00), current cnt only
  skip(021,024,1,3,1) = TRUE,  ! skip 1bamub in NAM 12Z (tm00), current cnt only
  skip(021,024,2,2,1) = TRUE,  ! skip 1bamub in GFS 06Z, current count only

  skip(005,070,2,1,1) = TRUE,  ! skip infmo  in GFS 00Z, current count only
  skip(005,070,2,2,1) = TRUE,  ! skip infmo  in GFS 06Z, current count only
  skip(005,070,2,3,1) = TRUE,  ! skip infmo  in GFS 12Z, current count only
  skip(005,070,2,4,1) = TRUE,  ! skip infmo  in GFS 18Z, current count only

  skip(005,071,2,1,1) = TRUE,  ! skip h20mo  in GFS 00Z, current count only
  skip(005,071,2,2,1) = TRUE,  ! skip h20mo  in GFS 06Z, current count only
  skip(005,071,2,3,1) = TRUE,  ! skip h20mo  in GFS 12Z, current count only
  skip(005,071,2,4,1) = TRUE,  ! skip h20mo  in GFS 18Z, current count only

  week_end(004,002,1,1,2) = TRUE, !pirep in NAM 00Z (tm00), reduce lim if hol
  week_end(004,002,1,2,3) = TRUE, !pirep in NAM 06Z (tm00), reduce lim wkend/hol
  week_end(004,002,1,3,3) = TRUE, !pirep in NAM 12Z (tm00), reduce lim wkend/hol
  week_end(004,002,1,4,2) = TRUE, !pirep in NAM 18Z (tm00), reduce lim if hol

  week_end(004,002,2,1,2) = TRUE, !pirep in GFS 00Z, reduce lim if holiday
  week_end(004,002,2,2,3) = TRUE, !pirep in GFS 06Z, reduce lim if wkend or hol
  week_end(004,002,2,3,3) = TRUE, !pirep in GFS 12Z, reduce lim if wkend or hol
  week_end(004,002,2,4,2) = TRUE, !pirep in GFS 18Z, reduce lim if holiday

  week_end(004,002,3,1,2) = TRUE, !pirep in GDAS 00Z, reduce lim if holiday
  week_end(004,002,3,2,3) = TRUE, !pirep in GDAS 06Z, reduce lim if wkend or hol
  week_end(004,002,3,3,3) = TRUE, !pirep in GDAS 12Z, reduce lim if wkend or hol
  week_end(004,002,3,4,2) = TRUE, !pirep in GDAS 18Z, reduce lim if holiday

  week_end(004,003,1,3,2) = TRUE, !asdar in NAM 12Z (tm00), reduce lim if hol
  week_end(004,003,1,4,2) = TRUE, !asdar in NAM 18Z (tm00), reduce lim if hol

  week_end(004,003,2,2,2) = TRUE, !asdar in GFS 06Z, reduce lim if holiday
  week_end(004,003,2,3,2) = TRUE, !asdar in GFS 12Z, reduce lim if holiday
  week_end(004,003,2,4,2) = TRUE, !asdar in GFS 18Z, reduce lim if holiday

  week_end(004,003,3,2,2) = TRUE, !asdar in GDAS 06Z, reduce lim if holiday
  week_end(004,003,3,3,2) = TRUE, !asdar in GDAS 12Z, reduce lim if holiday
  week_end(004,003,3,4,2) = TRUE, !asdar in GDAS 18Z, reduce lim if holiday

  week_end(004,004,1,2,3) = TRUE, !acars in NAM 06Z (tm00), reduce lim wkend/hol
  week_end(004,004,1,3,3) = TRUE, !acars in NAM 12Z (tm00), reduce lim wkend/hol

  week_end(004,004,2,2,3) = TRUE, !acars in GFS 06Z, reduce lim if wkend or hol
  week_end(004,004,2,3,3) = TRUE, !acars in GFS 12Z, reduce lim if wkend or hol

  week_end(004,004,3,2,3) = TRUE, !acars in GDAS 06Z, reduce lim if wkend or hol
  week_end(004,004,3,3,3) = TRUE  !acars in GDAS 12Z, reduce lim if wkend or hol

  week_end(003,001,1,2,4) = TRUE  !geost  in NAM 06Z (tm00), reduce lim if eclip
  week_end(003,002,1,2,4) = TRUE  !geosth in NAM 06Z (tm00), reduce lim if eclip

  week_end(005,010,1,2,4) = TRUE  !infus  in NAM 06Z (tm00), skip if eclipse
  week_end(005,011,1,2,4) = TRUE  !h2ius  in NAM 06Z (tm00), skip if eclipse
  week_end(005,012,1,2,4) = TRUE  !visus  in NAM 06Z (tm00), skip if eclipse
  week_end(005,013,1,2,4) = TRUE  !ptrus  in NAM 06Z (tm00), skip if eclipse

  week_end(021,041,2,2,4) = TRUE  !geoimr in GFS 06Z,  reduce lim if eclipse
  week_end(021,041,3,2,4) = TRUE  !geoimr in GDAS 06Z, reduce lim if eclipse

 /
  -->  skip(itype,isubt,inet,icyc,ichktp)  (SKIPS dump alert processing)

  -->  week_end(itype,isubt,inet,icyc,iwkend)  {REDUCES current count dump
        alert threshold by 10% if today is a weekend, by 20% if today is in
        the last week of the year or today is Thanksgiving, or by 15% for
        GOES sndg/rad/retr/wind reports if today is an eclipse day (wind
        for GFS or GDAS networks only) -- SKIPS dump alert processing for
        GOES wind reports for NAM (tm00) network if today is an eclipse day}

       where itype  = BUFR type
       where isubt  = BUFR subtype
       where inet   = network {1-NAM (tm00), 2-GFS, 3-GDAS}
       where icyc   = cycle   (1-00Z, 2-06Z, 3-12Z, 4-18Z)
       where ichktp = check type
                       (1-current count vs. current 30-day avg,
                        2-current 30-day avg vs. previous 30-day avg,
                        3-both checks 1 and 2)
       where iwkend = indicator when to reduce current dump count threshold
                       {1-reduce threshold only if today is a weekend,
                        2-reduce threshold only if today is a holiday,
                        3-reduce threshold if today is either a weekend
                          or a holiday or both
                        4-if today is an eclipse day, either reduce threshold
                          for GOES sndg/rad/retr/wind reports (wind for GFS
                          or GDAS networks only) or SKIP dump alert
                          processing for GOES wind reports for NAM tm00 network}

EOFd


# ---------------------------------------------------
# Execute bufr_datacount to check for low data counts
# ---------------------------------------------------


pgm=`basename  $DTCX`
set +u
. prep_step
set -u
startmsg

> deficient
> excessive
> trend_m03
> trend_m06
> trend_m09
> trend_m12

   unset FORT00 `env | grep "FORT[0-9]\{1,\}" | awk -F= '{print $1}'`
   export FORT10="dumplist"
   export FORT11="$RUN.${cycle}.status.tm00.bufr_d"
   export FORT13="obcount_30davg.${NET}.current"
   export FORT14="obcount_30davg.${NET}.3months_ago"
   export FORT15="obcount_30davg.${NET}.6months_ago"
   export FORT16="obcount_30davg.${NET}.9months_ago"
   export FORT17="obcount_30davg.${NET}.12months_ago"
   export FORT51="updated.status.tm00.bufr_d"
   export FORT52="deficient"
   export FORT53="excessive"
   export FORT54="trend_m03"
   export FORT55="trend_m06"
   export FORT56="trend_m09"
   export FORT57="trend_m12"

time -p $DTCX < datacount.cards >> $pgmout 2> errfile
RETC=$?

[ ! -s ${RUN}.${cycle}.status.tm00.bufr_d ] && RETC=99
[ ! -s obcount_30davg.${NET}.current ] && RETC=99

cat errfile >> $pgmout

if [ $RETC -eq 0 -o $RETC -eq 4 -o $RETC -eq 5 -o $RETC -eq 6 ]; then


# -------------------------------------------------------------
# Generate updated status file and post results to job log file
# -------------------------------------------------------------

   msg="bufr_datacount COMPLETED NORMALLY WITH CONDITION CODE $RETC "
   postmsg "$jlogfile" "$msg"
   [ $SENDCOM = YES ]  && cp updated.status.tm00.bufr_d  \
                           $COMOUT/${RUN}.${cycle}.updated.status.tm00.bufr_d

#  --> after NAMv4 impl. it makes sense to add $tmmark to end of below filename
#      (eventually retain new form of filename and delete old form of filename
#       which is still created further down in script)
##### echo "Alert File For $cycle_uc $NET_uc On $PDY" > $ALERTL/${NET}.${cycle}
   echo "Alert File For $tmmark_uc $cycle_uc $NET_uc On $PDY" > \
    $ALERTL/${NET}.${cycle}.$tmmark
   echo " " >> $ALERTL/${NET}.${cycle}.$tmmark
   echo "Comparison of Data Counts for This Run vs. Current 30-Day Avg. Data \
Counts" >> $ALERTL/${NET}.${cycle}.$tmmark
   echo "--------------------------------------------------------------------\
------" >> $ALERTL/${NET}.${cycle}.$tmmark
   echo " " >> $ALERTL/${NET}.${cycle}.$tmmark

   if [ $RETC -eq 4 -o $RETC -eq 5  -o  $RETC -eq 6 ]; then
      nindx=`wc deficient  | awk '{ print $1 }'`
      mindx=0
      until [ "$mindx" -eq "$nindx" ]
      do
         mindx=`expr $mindx + 1`
         msg=`head -n${mindx} deficient | tail -n1`
         postmsg "$jlogfile" "$msg"
      done

# -------------------------------------------------------
# Also post alerts to alert log file if r.c. is 4, 5 or 6
# -------------------------------------------------------

      cat deficient >> $ALERTL/${NET}.${cycle}.$tmmark
      msg="DATACOUNT low -- SDM should check ALERTLOG messages"
      postmsg "$jlogfile" "$msg"
   else
      echo "There are no low data count alerts for today." >> \
       $ALERTL/${NET}.${cycle}.$tmmark
      RETC=0
   fi

   if [ -s excessive ]; then
      nindx=`wc excessive  | awk '{ print $1 }'`
      mindx=0
      until [ "$mindx" -eq "$nindx" ]
      do
         mindx=`expr $mindx + 1`
         msg=`head -n${mindx} excessive | tail -n1`
         postmsg "$jlogfile" "$msg"
      done
   fi

#  --> for now copy new form of filename to old form,
#      but eventually delete this line
   cp -p  $ALERTL/${NET}.${cycle}.$tmmark $ALERTL/${NET}.${cycle}

# ------------------------------------------------------------------
#  Post long term alerts to alert log file (but not to job log file)
# ------------------------------------------------------------------

   kflag=0
   for months_ago in 03 06 09 12; do
      echo " "
#  --> after NAMv4 impl. it makes sense to add $tmmark to end of below filename
#      (eventually retain new form of filename and delete old form of filename
#       which is still created further down in script)
##### echo "Alert File For $cycle_uc $NET_uc" On $PDY > \
#####  $ALERTL/trend_vs_${months_ago}months_ago.${NET}.${cycle}
      echo "Alert File For $tmmark_uc $cycle_uc $NET_uc" On $PDY > \
       $ALERTL/trend_vs_${months_ago}months_ago.${NET}.${cycle}.$tmmark
      echo " " >> \
       $ALERTL/trend_vs_${months_ago}months_ago.${NET}.${cycle}.$tmmark
      echo "Current 30-Day Avg. Data Counts vs. 30-Day Avg. Data Counts From \
$months_ago Months Ago" >> \
       $ALERTL/trend_vs_${months_ago}months_ago.${NET}.${cycle}.$tmmark
      echo "-----------------------------------------------------------------\
-------------" >> \
      $ALERTL/trend_vs_${months_ago}months_ago.${NET}.${cycle}.$tmmark
      echo " " >> \
       $ALERTL/trend_vs_${months_ago}months_ago.${NET}.${cycle}.$tmmark

      if [ -s trend_m${months_ago} ]; then
         cat trend_m${months_ago} >> \
          $ALERTL/trend_vs_${months_ago}months_ago.${NET}.${cycle}.$tmmark
         if [ $kflag -eq 0 ]; then
            msg="one or more DATACOUNT trends high or low -- SDM should check \
ALERTLOG messages"
            postmsg "$jlogfile" "$msg"
            kflag=1
         fi
      else
         echo "There are no high or low data count alerts for today." \
          >> $ALERTL/trend_vs_${months_ago}months_ago.${NET}.${cycle}.$tmmark
      fi
      echo " " >> \
       $ALERTL/trend_vs_${months_ago}months_ago.${NET}.${cycle}.$tmmark
      echo "##################################################################\
#############" >> \
       $ALERTL/trend_vs_${months_ago}months_ago.${NET}.${cycle}.$tmmark

#  --> for now copy new form of filename to old form,
#      but eventually delete this line
      cp -p $ALERTL/trend_vs_${months_ago}months_ago.${NET}.${cycle}.$tmmark \
       $ALERTL/trend_vs_${months_ago}months_ago.${NET}.${cycle}

   done

   if test "$SENDSDM" = 'YES'; then


# ---------------------------------------------
# E-mail the updated status file to the SDM
# ---------------------------------------------

      text=$COMOUT/${RUN}.${cycle}.updated.status.tm00.bufr_d
      MODEL=$(echo $RUN | tr [a-z] [A-Z])
      cat ${text} | mail.py -s "${cyc}Z ${MODEL} Updated Data Dump File" sdm@noaa.gov
      echo `hostname` 

   fi
else
      msg="bufr_datacount TERMINATED ABNORMALLY WITH C. CODE $RETC --> \
non-fatal (RETC set to 0, no alerts generated)"
      postmsg "$jlogfile" "$msg"
      RETC=0
fi


# ---------------------------------------------------
# Store return code in appropriate $COMROOT directory
# ---------------------------------------------------

#  --> after NAMv4 impl. it makes sense to add $tmmark to end of below filename
#      (eventually transition JDUMP_ALERT to read new form of filename and
#       delete old form of filename)
echo $RETC > $COMOUT/${RUN}.${cycle}.dump_alert_flag
echo $RETC > $COMOUT/${RUN}.${cycle}.dump_alert_flag.$tmmark


# ---------------------
# Release alert via ecf
# ---------------------

cyc=`echo $cycle | cut -c2-3`
[ $SENDECF = YES ] && ecflow_client --event release_${RUN}${cyc}_dump_alert


