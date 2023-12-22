#!/bin/bash

#examples
# 47 2,8,14,20 * * *  jtyp=gfs  cyc=`date --date="-2 hours" +\%H` PDY=`date +\%Y\%m\%d` desc=wc2para /u/iliana.genkova/bin/cycqsub /lfs/h2/emc/obsproc/noscrub/iliana.genkova/Trigs.KEEP/jglobal_dump.wc2.pbs > /dev/null 2>&1
# 50 5,11,17,23 * * * jtyp=gdas cyc=`date --date="-5 hours" +\%H` PDY=`date +\%Y\%m\%d` desc=wc2para /u/iliana.genkova/bin/cycqsub /lfs/h2/emc/obsproc/noscrub/iliana.genkova/Trigs.KEEP/jglobal_dump.wc2.pbs > /dev/null 2>&1

# Handy:
# cyc=`date --date="-2 hours" +\%H`
# PDY=`date --date="-1 day" +\%Y\%m\%d`
# cyc=`date +\%H` PDY=`date +\%Y\%m\%d`
#
#ncepdate is a text file with contents:
#DATE  20230802120000WASHINGTON
#
#dumptime=`cut -c7-16 ncepdate`
#the line above ONLY works with FILE (i.e. ncepdate is a file)
#
#cycp=`echo $dumptime|cut -c9-10`
#where dumptime is an exported variable

# incoming data : ll $DCOMROOT/20231113/sst
# CRON
# 15 3,9,15,21 * * * $gcd && source /lfs/h2/emc/obsproc/noscrub/iliana.genkova/MARINE_code/create_SST_6h_dumps.sh

PDY=`date +\%Y\%m\%d`
cyc=`date --date="-3 hours" +\%H`

#PDY=20231119
#cyc=06

rm -rf all_includes
rm -rf all_excludes

    printf '%s' "*" > all_excludes

if [ $cyc -eq 00 ]; then
    export PDYm1=`date --date="-1 day" +\%Y\%m\%d`
    echo 'PDYm1 is' $PDYm1
    printf '%s%s\n' "${PDYm1}" "21*OSPO-L3*.nc" >  all_includes
    printf '%s%s\n' "${PDYm1}" "22*OSPO-L3*.nc" >> all_includes
    printf '%s%s\n' "${PDYm1}" "23*OSPO-L3*.nc" >> all_includes
    printf '%s%s\n' "${PDY}"   "00*OSPO-L3*.nc" >> all_includes
    printf '%s%s\n' "${PDY}"   "01*OSPO-L3*.nc" >> all_includes
    printf '%s%s\n' "${PDY}"   "02*OSPO-L3*.nc" >> all_includes

else
    printf '%s%02d%s\n' "${PDY}" "$(($cyc - 3 ))" "*OSPO-L3*.nc" >  all_includes
    printf '%s%02d%s\n' "${PDY}" "$(($cyc - 2 ))" "*OSPO-L3*.nc" >> all_includes
    printf '%s%02d%s\n' "${PDY}" "$(($cyc - 1 ))" "*OSPO-L3*.nc" >> all_includes
    printf '%s%02d%s\n' "${PDY}" "$(($cyc + 0 ))" "*OSPO-L3*.nc" >> all_includes
    printf '%s%02d%s\n' "${PDY}" "$(($cyc + 1 ))" "*OSPO-L3*.nc" >> all_includes
    printf '%s%02d%s\n' "${PDY}" "$(($cyc + 2 ))" "*OSPO-L3*.nc" >> all_includes
fi

echo '---'
cat all_includes
echo '---'

in_dir=$DCOMROOT
#out_dir=/lfs/h2/emc/obsproc/noscrub/iliana.genkova/MARINE_code  #$PWD
out_dir=/lfs/h2/emc/obsproc/noscrub/iliana.genkova/CRON/SOCA/com/obsproc/v1.2  #gfs.20231026/00/atmos/

# 20230802235000-OSPO-L3_GHRSST-SSTsubskin-AVHRRF_MB-ACSPO_V2.80-v02.0-fv01.0.nc
# ncdump -h 20230802235000-OSPO-L3_GHRSST-SSTsubskin-AVHRRF_MB-ACSPO_V2.80-v02.0-fv01.0.nc | grep start_time
#               :start_time = "20230802T235000Z" ;
# ncdump -h 20230802235000-OSPO-L3_GHRSST-SSTsubskin-AVHRRF_MB-ACSPO_V2.80-v02.0-fv01.0.nc | grep stop_time
#               :stop_time = "20230802T235959Z" ;
# dcom arrival time: 00:42 00:52 01:12 01:22 01:32 01:42 ... ABOUT EVERY 10 MINITES
#

mkdir -p $out_dir/gdas.${PDY}/${cyc}/sst

# EXAMPLE OF file called "all_includes"
#all_includes
#2023080221*
#2023080222*
#2023080223*
# *_s2023080212*

# remove the --dry-run at some point !
# --include must be before --exclude
# good:
# rsync -nrv --dry-run  --include="2023080716*.nc" --exclude="*"  $DCOMROOT/${PDY}/sst/ $PWD//${PDY}/${cc}/
# good:
# rsync -avhWr --no-compress --progress --dry-run --include-from=all_includes --exclude-from=all_excludes $DCOMROOT/${PDY}/sst/ $out_dir//${PDY}/${cyc}/
# rsync -nrv                            --dry-run --include-from=all_includes --exclude-from=all_excludes $DCOMROOT/${PDY}/sst/ $out_dir//${PDY}/${cyc}/

if [ $cyc -eq 00 ]; then
  rsync -avhWr --no-compress --progress --include-from=all_includes --exclude-from=all_excludes $DCOMROOT/${PDYm1}/sst/ $out_dir/gdas.${PDY}/${cyc}/sst
fi
  rsync -avhWr --no-compress --progress --include-from=all_includes --exclude-from=all_excludes $DCOMROOT/${PDY}/sst/ $out_dir/gdas.${PDY}/${cyc}/sst


