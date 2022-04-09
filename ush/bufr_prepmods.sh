#!/bin/ksh
# bufr_prepmods.sh - 30 Jan 2013
#  trigger for multiday dumping of MODS data
#
#  prog flow:
#   for each of select datatypes
#    -run dumpjb for 24hours around noon of input ($1) date
#    -run bufr_prepmods to create MB file
# ---------
# mods:
# 13 NOV 2003 JAW/DAK Copied from JAW's checkout version and configured
#             into a "production" ush script
#  7 May 2004 JAW  disabled metar & ssmipn processing
# 26 May 2004 JAW  enabled metar & ssmipn processing;  disabled 
#                  'monthly' datatype (bathy,tesac,trkob) processing; 
#                  trapped blank MODSDIR
# 14 Jun 2004 JAW  Disabled ERSAL processing (new BTable doesn't 
#                  support it); also, fixed invalid data type trap bug 
#                  (couldn't distiguish ERSAL from NERSAL).
# 16 Jun 2004 JAW  fixed bug in dtype parsing (quotes on grep alltypes)
#  2 Aug 2005 JAW  referenced bufrtable in /nwprod/fix (not MODSDIR)
# 15 Oct 2007 JAW  enabled Envisat Altimeter dtype (envsal, NC031109);
#                  parameterized BTABdir.
#  5 Mar 2008 JAW  enabled rstprod permissions for SHIPS dtyp
#                  removed copy of dump files in COMOUT
#  6 Mar 2012 JAW  added SHIPSU as supported data type;
#                  added SHPALL dump group support
# 30 Jan 2013 JAW  Ported to WCOSS, modified for linux platforms:
#                   replaced "timex" w/ "time -p"
#                   replaced XLF logical unit number specifications w/
#                   appropriate FORTxx logic.
# 22 Feb 2019 JAW  Ported to phase 3 (Dell) platforms:
#                   added environment variables and parameterized for vertical
#                     structure layout;
# =========

# Positional parameters passed in:
#   1 - Dump date (YYYYMMDD)

# Imported variables that must be passed in:
#   DATA         - path to working directory
#   pgmout       - string indicating path to for standard output file
#   EXECmods     - path to bufr_prepmods executable directory
#   MODSDIR      - path to MODSBUFR database directory

# Imported variables that can be passed in:
#   MODX      - path to bufr_prepmods executable (defaults to
#                 $EXECmods/bufr_prepmods)
#   jlogfile  - path to job log file (skipped over by this script if not
#                 passed in)
#   BTABdir   - path to bufrtable directory (default: $HOMEobsproc/fix/)

ver='02/26/2019 -- WCOSS phase 3 compatible'
echo "Welcome to bufr_prepmods.sh of $ver"

set -uax  # x=echo's exec; u=error on unset variables; a=auto export'

cd $DATA

err_grand=0

if [ "$#" -ne '1' ]; then
   msg="**NON-FATAL ERROR PROGRAM  BUFR_PREPMODS  dump date not in \
positional parameter 1"
   set +x
   echo
   echo $msg
   echo
   set -x
   echo $msg >> $pgmout
   set +u
   [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
   set -u
   exit 99
fi

if [ ".$MODSDIR" = '.' ]; then
   msg="**NON-FATAL ERROR PROGRAM  BUFR_PREPMODS -- MODSDIR parameter \
not specified"
   set +x
   echo
   echo $msg
   echo
   set -x
   echo $msg >> $pgmout
   set +u
   [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
   set -u
   exit 99
fi

MODX=${MODX:-$EXECmods/bufr_prepmods}

#  Day to dump is script argument
#  ------------------------------
cymd=$1
dhr=${dhr:-11.999}
year=`echo $cymd|cut -c 1-4`
mnth=`echo $cymd|cut -c 5-6`
days=`echo $cymd|cut -c 7-8`

set +x
echo
echo "  Dump date is $cymd"
echo
set -x

#  list of datatypes to dump a day into the mods database
#  ------------------------------------------------------
 typ000=' metar'
 typ001=' ships  shipsb  shipsu  shipub  dbuoy  dbuoyb  mbuoy  mbuoyb  lcman  cmanb  tideg  slpbg  cstgd'
 typ012='               sstns  sstnvh ssmipn'  # ersal
 typ031=' ' #'bathy  tesac  trkob'
 typHAlt='    nersal ngfoal ntpxal njsnal  envsal'
 typ031="$typ031$typHAlt"

 gtypes=' shpall'

alltypes=${typ000}${typ001}${typ012}${typ031}${gtypes}
dtypes=${dtypes:-"$alltypes"}


##echo "dtypes=>>$dtypes<<" | sed 's/  */ /g'

# Loop thru each data type
# ------------------------
for dtyp in $dtypes ; do

  if [ -z "`echo \" $alltypes \" | grep \" $dtyp \"`" ] ; then 
    msg="$dtyp not supported by BUFR_PREPMODS - skip"
    set +x
    echo
    echo $msg
    echo
    set -x
    echo $msg >> $pgmout
    set +u
    [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
    set -u
  else

#  make and/or move to directory for this data for this day
#  --------------------------------------------------------
    #DBASE=$MODSDIR/$year/$mnth/$dtyp
    DBASE=$MODSDIR/$mnth/$dtyp
    mkdir -p $DBASE

    if [ ".$dtyp" = '.ships' -o \
         ".$dtyp" = '.shipsb' -o \
	 ".$dtyp" = '.shpall' ] ; then   # restrict access to SHIPS/SHIPSB/SHPALL directory
      chgrp rstprod $DBASE
      chmod 750     $DBASE
    fi # dtyp = ships/shipsb/shpall

#  run the dumpjb script for this datatype  
#  ---------------------------------------
    # turn off duplicate removal for mbuoyb
    if [ $dtyp = 'mbuoyb' ];
    then
	export DUPC_TAC=off
	export DUPC=off
    else
	export DUPC_TAC=on
	export DUPC=on
    fi

    > $dtyp.ibm

    time -p $DUMP ${cymd}12 $dhr $dtyp >> $pgmout 2> errfile
    errret=$?
    cat errfile >> $pgmout
    cat $dtyp.out >> $pgmout
    rm errfile
    set +x
    echo
    echo "DUMPJB error r.c. is $errret for $dtyp"
    echo
    set -x

    if [ $errret -gt 0 -a $errret -ne 11 ] ; then 
      if [ $errret -le 22 ] ; then 
         msg="$dtyp dump is EMPTY, errret=$errret - skip"
      else
        msg="DUMPJB failed attempting to dump $dtyp, errret=$errret - skip"
        err_grand=99
      fi
      set +x
      echo
      echo $msg
      echo
      set -x
      echo $msg >> $pgmout
      set +u
      [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
      set -u
      cp -p $dtyp.out $COMOUT/$dtyp.$cymd.bufr_d.out_$errret
      continue  # to next data type
    else

      if [ $errret -eq 11 ] ; then 
        typ0=`grep 'HAS        0' $dtyp.out | cut -c1-7`
        for t in $typ0 ; do 
          msg="$dtyp dump is INCOMPLETE (rc=$errret) for type(s) $t"
          set +x ; echo ; echo $msg ; echo ; set -x
          echo $msg >> $pgmout
          set +u ; [ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg" ; set -u
        done # t in typ0
      fi # errret = 11
      msg="script DUMPJB completed successfully for $cymd for $dtyp"
      set +x
      echo
      echo $msg
      echo
      set -x
      echo $msg >> $pgmout
      set +u
      [ -n "$jlogfile" ]  &&  postmsg "$jlogfile" "$msg"
      set -u
      cp -p $dtyp.out $COMOUT/$dtyp.$cymd.bufr_d.out
    fi

#  Run bufr_prepmods to make a MODSBUFR file from the dump file
#  ---------------------------------------------------------
    pgm=`basename $MODX`
    export pgm
    if [ -s prep_step ]; then
       set +u
       . prep_step
       set -u
    else
       [ -f errfile ] && rm errfile
       unset FORT00 `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`
    fi

    set +x ; echo 
    echo "prepmods: running $pgm for $dtyp $cymd"
    echo ; set -x

    BTABdir=${BTABdir:-$HOMEobsproc/fix}

    export FORT20="$dtyp.ibm"
    export FORT21="$BTABdir/bufrtab.mods"
    export FORT50="$dtyp.$cymd"

    time -p $MODX >> $pgmout 2> errfile
    errget=$?
    cat errfile >> $pgmout
    rm errfile
    set +x
    echo
    echo "The foreground exit status for $pgm is " $errget
    echo
    set -x
    if [ "$errget" -ne '0' ];then
      msg="**FATAL ERROR PROGRAM  $pgm  FOR $cymd FOR $dtyp - \
RETURN CODE $errget"
      set +x
      echo
      echo $msg
      echo
      set -x
      echo $msg >> $pgmout
      set +u
      [ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg"
      set -u
      err_grand=99
    else
      cp -p $dtyp.$cymd $DBASE/$dtyp.$cymd
      if [ ".$dtyp" = '.ships' -o \
	   ".$dtyp" = '.shipsb' -o \
           ".$dtyp" = '.shpall' ] ; then   # restrict access to SHIPS/SHIPSB/SHPALL files
        chgrp rstprod $DBASE/$dtyp.$cymd
        chmod 640     $DBASE/$dtyp.$cymd
      fi # dtyp = ships/shipsb/shpall
      msg="program  $pgm completed normally for $cymd FOR $dtyp"
      set +x
      echo
      echo $msg
      echo
      set -x
      echo $msg >> $pgmout
      set +u
      [ -n "$jlogfile" ] && postmsg "$jlogfile" "$msg"
      set -u
    fi
    set +x
    echo
    echo "----------------------------------------------------------"
    echo "*****  COMPLETED PROGRAM $pgm for $dtyp $cymd  *****"
    echo "----------------------------------------------------------"
    echo
    set -x

#--End of loop for each data type
#--------------------------------
  fi # dtyp in alltypes
done # for dtyp in dtypes


# End of script
# -------------

err=$err_grand

if [ "$err_grand" -gt '0' ]; then
  if [ -s $DATA/err_exit ]; then
    $DATA/err_exit
  else
####kill -9 ${qid}
    exit 555
  fi
  exit 9
fi

