#!/bin/ksh

set +u
# Make sure we are in the $DATA directory 
cd $DATA       
msg="HAS BEGUN on `hostname`"
$DATA/postmsg "$jlogfile" "$msg"

if [ "$PROCESS_CHEM" = 'YES' ]; then

   msg="HELLO CHEM"
   $DATA/postmsg "$jlogfile" "$msg"

fi

msg='ENDED NORMALLY.'
$DATA/postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
