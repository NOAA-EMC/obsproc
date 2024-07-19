#!/bin/ksh
echo
echo "-------------------------------------------------"
echo "Datacount Averaging"             
echo "-------------------------------------------------"
echo "AUTHOR: Dennis Keyser "
echo "History: AUG 2000 - First implementation of this new script."
echo "   2013-03-19 -- ported to WCOSS (Whiting)"
echo "   2013-03-20 -- RUCS removed (Keyser)"
echo "   2013-09-09 -- URMA added (Keyser)"
echo "   2014-08-31 -- Save the last 32 days rather than the last 30 days but"
echo "                 still average only over the past 30 days (Melchior)"
echo "   2016-12-02 -- Added tm00 qualifier to all status file copy"
echo "                 statements (Keyser)"
echo "   2017-03-28 -- Port to WCOSS Cray.  Replaced hardwired pathnames with"
echo "                 NCO standard variables.  Prepare for gdas filename"
echo "                 change (gdas1->gdas) (Stokes)"
echo "   2017-11-20 -- Accounts for NAM now having tm06 through tm00 time"
echo "                 markers (only tm00 is expected here for this and all"
echo "                 other networks)."
echo "   2018-04-17 -- Removed refernce to 'gdas1'.  Prepared for FV3GFS."

echo

set -aux

# Imported positional parameters:
#   $1     - string indicating network {'nam', 'gfs', 'gdas', 'rap', 'rtma',
#            'urma' or 'dump', tm00 for all networks}
#           (Note: This is not defined the same as $NET in the j-job)
#
# Imported variables that must be passed in:
#   envir  - processing environment ('prod' or 'test')
#   DATA   - path to working directory
#   PDY    - today's YYYYMMDD
#   PDYm1  - yesterday's YYYYMMDD
#   NDATE  - pointer to ndate executable (typically set via prod_util module)
#
# Imported Variables that must be passed in under certain conditions:
#   EXECobsproc
#               - formerly EXECobsproc_shared_bufr_avgdata prior to WCOSS2
#               - path to obsproc executable directory containing bufr_avgdata
#                 (invoked only if $AVGX is not imported)
#   FIXbufr_dump_
#               - formerly FIXobsproc_shared_bufr_dumplist prior to WCOSS2
#               - path to bufr_dump fix directory containing the
#                 bufr_dumplist file
#                 (invoked only if $DUMPLIST is not imported)
#
# Imported variables that can be passed in:
#   COPY       - string indicating whether status,and possibly updated status,
#                file copying should be performed, along with cleaning of "old"
#                directories
#                (Default: YES)
#   UPDATE_AVERAGE_FILE
#              - string indicating whether the data count average file(s)
#                should be updated {one might one to set this to NO if only
#                interested in performing COPY (see above)}
#                (Default: YES)
#   pgmout     - string indicating path to for standard output file
#                (Default: none, if not exported into this scipt, ignored)
#   jlogfile   - string indicating path to job log file
#                (Default: none, if not exported into this scipt, ignored)
#   COMROOTp1  - path to com directory (typically, NCO com on IBM-P1)
#                (Default: none)
#   COMROOT    - path to com directory (typically, NCO com on current system)
#                (Default: none)
#   HOMEarch   - path to archive directory
#                (Default: $COMROOTp1/arch/${envir})   (IBM phase 1)
#   OBCNTarch  - path to archive (copy) of dump status (*.t*z.status*), and
#                possibly updated status (*.t*z.updated.status*), files over
#                many (30?) days (one directory back; actual directory is
#                hardwired to $OBCNTarch/${1}); status files will be copied to
#                $OBCNTarch/${1}/${1}.YYYYMMDD directory and updated status
#                files will be copied to
#                $OBCNTarch/${1}/${1}.YYYYMMDD/updated.status directory;
#                updated status files are copied only for nam, gfs or gdas
#                networks (tm00 for all networks)
#                (Default: $HOMEarch/obcount_30day);
#   comin      - path to today's dump status and updated status files
#                (Default: $COMROOT/${qual1}/$(envir}/${qual2}.${PDY})
#   comin_m1   - path to yesterday's dump status and updated status files
#                (Default: $COMROOT/${qual1}/$(envir}/${qual2}.${PDYm1})
#   DUMPLIST   - path to the bufr_dumplist file
#                (Default: $FIXbufr_dump/bufr_dumplist)
#   AVGX       - path to bufr_avgdata executable
#                (Default: $EXECobsproc/bufr_avgdata)

err=0
net=$1
net_uc=$(echo $net | tr [a-z] [A-Z])
if [ $net = gdas ]; then
   qual1=gfs
   qual2=gdas
   qual3=gdas
else
   qual1=$net
   qual2=$net
   qual3=$net
fi


COPY=${COPY:-YES}
UPDATE_AVERAGE_FILE=${UPDATE_AVERAGE_FILE:-YES}

HOMEarch=${HOMEarch:-$COMROOTp1/arch/${envir}}
OBCNTarch=${OBCNTarch:-$HOMEarch/obcount_30day}
comin=${comin:-$COMROOT/${qual1}/${envir}/${qual2}.${PDY}}
comin_m1=${comin_m1:-$COMROOT/${qual1}/${envir}/${qual2}.${PDYm1}}
# For GFSv16, must go up two directory levels in $comin and $comin_m1 to arrive
# at the g*s.${PDY} directory to locate all cycle *status*bufr_d files
if [ $net = gfs -o $net = gdas ]; then
  comin_noatmos=$(dirname $comin)
  comin_nocyc=$(dirname $comin_noatmos)
  comin_m1_noatmos=$(dirname $comin_m1)
  comin_m1_nocyc=$(dirname $comin_m1_noatmos)
fi
DUMPLIST=${DUMPLIST:-$FIXbufr_dump/bufr_dumplist}
AVGX=${AVGX:-$EXECobsproc/bufr_avgdata}
jlogfile=${jlogfile:=""}

if [ $COPY = YES ]; then

#  Copy the status files, and possibly the updated status files, from today's
#  and yesterday's runs for this network (tm00 for all networks) (yesterday's
#  is copied just in case this didn't run yesterday) and add the EOF string to
#  the end of each status file (note: the updated status files are only copied
#  if this is the nam, gfs or gdas network - the EOF string is not added to the
#  updated status files)
#  ----------------------------------------------------------------------------

mkdir -p $OBCNTarch/${net}.${PDY}
chmod 775 $OBCNTarch/${net}.${PDY}
mkdir -p $OBCNTarch/${net}.${PDYm1}
chmod 775 $OBCNTarch/${net}.${PDYm1}
[ $net = dump -o $net = rap -o $net = rtma -o $net = urma ] && set +x
if [ $net = gfs -o $net = gdas ]; then
   cp -p ${comin_nocyc}/*/*/${qual3}.t*z.status.tm00.bufr_d $OBCNTarch/${net}.${PDY}
else
   cp -p ${comin}/${qual3}.t*z.status.tm00.bufr_d $OBCNTarch/${net}.${PDY}
fi
err_copy_status=$?
chmod 775 $OBCNTarch/${net}.${PDY}/*
if [ $err_copy_status -eq 0 ]; then
   msg="$net_uc tm00 status file(s) for today successfully copied to \
$OBCNTarch/${net}.${PDY}"
   set +x
   echo
   echo "$msg"
   echo
   set -x
   [ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg"
fi
if [ $net = gfs -o $net = gdas ]; then
   cp -p ${comin_m1_nocyc}/*/*/${qual3}.t*z.status.tm00.bufr_d $OBCNTarch/${net}.${PDYm1}
else
   cp -p ${comin_m1}/${qual3}.t*z.status.tm00.bufr_d $OBCNTarch/${net}.${PDYm1}
fi
err_copy_status_yesterday=$?
chmod 775 $OBCNTarch/${net}.${PDYm1}/*
if [ $err_copy_status_yesterday -eq 0 ]; then
   msg="$net_uc tm00 status file(s) for yesterday successfully copied to \
$OBCNTarch/${net}.${PDYm1}"
   set +x
   echo
   echo "$msg"
   echo
   set -x
   [ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg"
fi
if [ $net = nam -o $net = gfs -o $net = gdas ]; then
   mkdir -p $OBCNTarch/${net}.${PDY}/updated.status
   chmod 775 $OBCNTarch/${net}.${PDY}/updated.status
   mkdir -p $OBCNTarch/${net}.${PDYm1}/updated.status
   chmod 775 $OBCNTarch/${net}.${PDYm1}/updated.status
   if [ $net = nam ]; then
      cp -p ${comin}/${qual3}.t*z.updated.status.tm00.bufr_d \
      $OBCNTarch/${net}.${PDY}/updated.status
   else
      cp -p ${comin_nocyc}/*/*/${qual3}.t*z.updated.status.tm00.bufr_d \
       $OBCNTarch/${net}.${PDY}/updated.status
   fi
   err_copy_updated_status=$?
   chmod 775 $OBCNTarch/${net}.${PDY}/updated.status/*
   if [ $err_copy_updated_status -eq 0 ]; then
      msg="$net_uc tm00 updated status file(s) for today successfully copied \
to $OBCNTarch/${net}.${PDY}/updated.status"
      set +x
      echo
      echo "$msg"
      echo
      set -x
      [ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg"
   fi
   if [ $net = nam ]; then
      cp -p ${comin_m1}/${qual3}.t*z.updated.status.tm00.bufr_d \
       $OBCNTarch/${net}.${PDYm1}/updated.status
   else
      cp -p ${comin_m1_nocyc}/*/*/${qual3}.t*z.updated.status.tm00.bufr_d \
       $OBCNTarch/${net}.${PDYm1}/updated.status
   fi
   err_copy_updated_status_yesterday=$?
   chmod 775 $OBCNTarch/${net}.${PDYm1}/updated.status/*
   if [ $err_copy_updated_status_yesterday -eq 0 ]; then
      msg="$net_uc tm00 updated status file(s) for yesterday successfully \
copied to $OBCNTarch/${net}.${PDYm1}/updated.status"
      set +x
      echo
      echo "$msg"
      echo
      set -x
      [ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg"
   fi
fi
set -x

cd $OBCNTarch/${net}.${PDY}

ls *.bufr_d > $DATA/ls.files_today.bufr

for n in `cat $DATA/ls.files_today.bufr`; do
   if [ $net = gfs -o $net = gdas ]; then
      [ -s ${comin_nocyc}/*/*/${n} ] && echo "EOF @@@" >> $n
   else
      [ -s ${comin}/${n} ] && echo "EOF @@@" >> $n
   fi
done

cd $OBCNTarch/${net}.${PDYm1}

ls *.bufr_d > $DATA/ls.files_yesterday.bufr

for n in `cat $DATA/ls.files_yesterday.bufr`; do
   if [ $net = gfs -o $net = gdas ]; then
      [ -s ${comin_m1_nocyc}/*/*/${n} ] && echo "EOF @@@" >> $n
   else
      [ -s ${comin_m1}/${n} ] && echo "EOF @@@" >> $n
   fi
done

cd $DATA


#  Clean out all status and updated status file directories older than 32-days
#  ---------------------------------------------------------------------------

pdy_earliest=$($NDATE -768 ${PDY}00 | cut -c1-8)
set +x
echo
echo "pdy_earliest is " $pdy_earliest
echo
set -x

set +x
ls -d $OBCNTarch/* > ls.dir
set -x

if [ -s ls.dir ];then
   lines=`cat ls.dir | wc -l`
else
   lines=0
fi

n=1

until [ $n -gt $lines ]
do
   eval this_dir=`echo \`head -n ${n} ls.dir | tail -n 1\``
##echo "this_dir is " $this_dir
   eval this_dir_date=`echo \`head -n ${n} ls.dir | tail -n 1\` | \
    awk -F"${net}/${net}" '{print$2}' | cut -c2-9`
##echo "this_dir_date is " $this_dir_date

# make sure $this_dir is a directory w/ a date in its name and not some other
#  file/directory (which we would retain before continuing the date check)
# ---------------------------------------------------------------------------
   echo $this_dir_date|egrep '^[0-9]+$'
   if [ $? -eq 0 ]; then # this_dir_date has 8 digits, expect it to be YYYYMMDD
      if [ $this_dir_date -le $pdy_earliest ]; then
         rm -r -f $this_dir
      else
         break
      fi
   fi
   n=`expr $n + 1`
done
msg="All $net_uc tm00 status file and updated status file (where applicable) \
directories older than 32-days cleaned out"
set +x
echo
echo "$msg"
echo
set -x
[ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg"

fi    # fi on COPY = YES test


if [ $UPDATE_AVERAGE_FILE = YES ]; then
 
msg="Attempt to update data count average table for $net_uc tm00 network"
set +x
echo
echo "$msg"
echo
set -x
[ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg"

#  Grep out the individual counts for all types, based on cycle
#  ------------------------------------------------------------

if [ $net = dump -o $net = rap -o $net = rtma -o $net = urma ] ; then
   cycles="t00z t01z t02z t03z t04z t05z t06z t07z t08z t09z t10z t11z \
           t12z t13z t14z t15z t16z t17z t18z t19z t20z t21z t22z t23z"
else
   cycles="t00z t06z t12z t18z"
fi

# Determine the date for 30 days ago 
pdym30d=$($NDATE -720 ${PDY}00)
pdym30d=$(echo $pdym30d | sed 's/00$//')  # remove trailing 00 

# build array for last 30 days files
for dir in $OBCNTarch/$net*; do
  set +x
  netday=$(echo $dir | awk -F'/' '{print $NF}')
  # test day is no more than 30 days from $PDY
  dtg=$(echo $netday | awk -F'.' '{print $NF}')
  if [ "$dtg" -gt "$pdym30d" ]; then # "-gt" here since 30 days ago to now
                                     # contains 31 directories and we only
                                     # want the last 30 directories
    arr30d=("${arr30d[@]}" "$dtg")
  fi
  set -x
done

for cycle_avg in $cycles; do
   > accum.obs.counts.${cycle_avg}
   for dtg in "${arr30d[@]}"; do
     set +x
     grep --text -h -e " REPORTS" -e "^EOF @@@" \
     $OBCNTarch/${net}.${dtg}/*${cycle_avg}.status.*bufr_d | \
     grep -e " in data group " -e "^EOF @@@" | grep -e " HAS" -e "^EOF @@@" | \
     grep -v -e "Domain" >> delete
     awk -F" HAS" '{print$2}' delete | awk -F" REPORTS" '{print$1}' | \
      sed "s/[^0-9]/ /g" >> delete.r
     awk -F" in data group " '{print $1}' delete | \
      sed "s/[^0-9][^0-9][^0-9]^\.[^0-9][^0-9][^0-9]/ /g" | \
      sed "s/[^0-9]/ /g" >> delete.l
     paste -d" " delete.l delete.r | sed "s/^[^0-9]/EOF @@@       0/g" \
      >> accum.obs.counts.${cycle_avg}
     rm delete delete.l delete.r
     set -x
   done
done

grep --text -e "^_" -e "^:" $DUMPLIST | grep -Fe "#>" > dumplist


#  Generate a new average file
#  ---------------------------

final_yr=`echo $PDY | cut -c1-4`
final_mn=`echo $PDY | cut -c5-6`
final_dy=`echo $PDY | cut -c7-8`
final_date=${final_mn}/${final_dy}/${final_yr}


cat <<EOFd > avgdata.cards
 &INPUT
   NETWORK='$net_uc',
   CURRDATE='$final_date'
 /
EOFd

pgm=`basename  $AVGX`
if [ -s prep_step ]; then
   set +u
   . prep_step
   set -u
else
   [ -f errfile ] && rm errfile

   unset FORT00 `env | grep "FORT[0-9]\{1,\}" | awk -F= '{print $1}'`

fi

export FORT10=dumplist
# unit 21 opened internally by pgm and holds accumulated obs. for each cycle
# unit 25 opened internally by pgm and holds accumulated sat 1b for each cycle
export FORT51=obcount_30davg.${net}.current
time -p $AVGX< avgdata.cards > outout 2> errfile
err=$?
###cat errfile
cat errfile >> outout
cat outout >> avgdata.out
set +u
[ -n "$pgmout" ]  &&  cat outout >> $pgmout
set -u
rm outout
set +x
echo
echo 'The foreground exit status for BUFR_AVGDATA is ' $err
echo
set -x
if [ $err -eq 0 ]; then

   set +x
   echo " -------------------------------------------------------------------- "
   echo " ***** COMPLETED PROGRAM BUFR_AVGDATA for $net_uc tm00 NETWORK *****  "
   echo " -------------------------------------------------------------------- "
   set -x
   msg="bufr_avgdata completed normally for $net_uc tm00 network"
   [ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg"
else

msg="BUFR_AVGDATA TERMINATED ABNORMALLY WITH CONDITION CODE $err --> non-fatal"
   set +x
   echo
   echo "$msg"
   echo
   set -x
   [ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg"

fi
fi    # fi on UPDATE_AVERAGE_FILE = YES test

if [ $COPY = YES ]; then

#  Remove any empty directories (these should never be empty unless
#   there is a problem - removing them here may help detect a 
#   "break" in the script sooner than if all 30-days are missing)
#  ----------------------------------------------------------------

   rmdir $OBCNTarch/${net}.${PDYm1}
   rmdir $OBCNTarch/${net}.${PDY}

fi

exit $err

