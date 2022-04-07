#!/bin/bash
###########################################################################
echo "--------------------------------------------------------------------"
echo "exmods.sh  - MODSBUFR data dump processing                       "
echo "--------------------------------------------------------------------"
echo "History: May  7 2004 - Original script.                             "
echo "         May 25 2004 - run day-10 daily tank only, exit if missing. "
echo "         Mar  6 2012 - parameterized DCOMROOT & TANK env vars.      "
echo "         Feb 26 2019 - set for use on WCOSS phase 3 (Dell) platforms"
echo "         Dec 15 2021 - set for use on WCOSS2                        "
echo "--------------------------------------------------------------------"
###########################################################################

set -x

# Make sure we are in the $DATA directory
cd $DATA

msg="HAS BEGUN on `hostname`"
postmsg "$jlogfile" "$msg"


# Run for day today-9 only (oldest daily tank)
# exit if tank does not exist

set +x; echo -e "\n---> path to finddate.sh below is: `which finddate.sh`"; set -x
PDYm9=$(finddate.sh $PDY d-9)


# -- set default TANK env var (location of source data tanks for dumpjb)
export TANK=${TANK:-"$DCOMROOT/$dfix"}

if [ ! -d $TANK/$PDYm9 ] ; then 
  echo "exmods: tank dir not found - $PDYm9"
  set +x
  echo " "
  echo " ###################################################### "
  echo " --> SOURCE TANK $TANK/$PDYm9 NOT FOUND "
  echo " --> @@ F A T A L   E R R O R @@   --  ABNORMAL EXIT    "
  echo " ###################################################### "
  echo " "
  set -x
  err_exit
  exit 9
fi # source tank exists

cat break > $pgmout

msg="DUMP DATE IS $PDYm9"
postmsg "$jlogfile" "$msg"

echo ; echo "exmods: calling $USHmods/bufr_prepmods.sh $PDYm9" ; echo 
$USHmods/bufr_prepmods.sh $PDYm9
err_prepmods=$?

# save standard output
cat  break $pgmout break > allout
cat allout
# rm allout

# GOOD RUN
if [[ $err_prepmods -eq 0 ]] ; then 
  set +x
  echo " "
  echo " ****** PROCESSING COMPLETED NORMALLY"
  echo " "
  set -x

  sleep 10

  msg='ENDED NORMALLY.'
  postmsg "$jlogfile" "$msg"

# BAD RUN
else # prepmods error return

  set +x ; echo " "
  echo " ****** ERROR in bufr_prepmods - rc='$err_prepmods'"
  echo " " ; set -x
  $DATA/err_exit

fi # prepmods error return

exit
################## END OF SCRIPT #######################
