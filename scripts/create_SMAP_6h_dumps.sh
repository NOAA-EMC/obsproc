#!/bin/bash

# $DCOMROOT/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT*
#
# [clogin07 /lfs/h2/emc/obsproc/noscrub/iliana.genkova/MARINE_code]$ ll $DCOMROOT/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT*
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 08:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46603_A_20231023T012845.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 08:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46603_D_20231023T012845.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 08:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46604_A_20231023T030712.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 12:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46604_D_20231023T030712.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 12:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46605_A_20231023T044541.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 12:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46605_D_20231023T044541.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 14:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46606_A_20231023T062409.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 14:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46606_D_20231023T062409.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 16:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46607_A_20231023T080236.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 16:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46607_D_20231023T080236.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 16:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46608_A_20231023T094104.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 17:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46608_D_20231023T094104.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 17:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46609_A_20231023T111932.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 21:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46609_D_20231023T111932.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 21:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46610_A_20231023T125759.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 23 19:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46610_D_20231023T125759.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 24 21:36 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46611_A_20231023T143628.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 24 21:36 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46611_D_20231023T143628.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 24 00:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46612_A_20231023T161456.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 24 03:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46612_D_20231023T161456.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 24 03:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46613_A_20231023T175323.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 24 03:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46613_D_20231023T175323.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 24 03:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46614_A_20231023T193151.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 24 03:35 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46614_D_20231023T193151.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 24 03:36 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46615_A_20231023T211018.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 24 21:36 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46615_D_20231023T211018.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 24 21:36 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46616_A_20231023T224846.h5
#-rw-rw-r-- 1 dfprod prod 12076064 Oct 24 21:36 /lfs/h1/ops/prod/dcom/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT_46616_D_20231023T224846.h5
#
#OBSERVATIONS
# files arrive/are completed/ 8-9h after the filename time 

export Din=$DCOMROOT #/20231023/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT*
#export Dout=/lfs/h2/emc/obsproc/noscrub/iliana.genkova/MARINE_code/
export Dout=/lfs/h2/emc/obsproc/noscrub/iliana.genkova/CRON/SOCA/com/obsproc/v1.2  #gfs.20231026/00/atmos/

##for testing
#cdate=20231128
#cdatem1=20231127
#cyc=06

cdate=`date +\%Y\%m\%d`
cdatem1=`date -d "$cdate -1 day" +%Y%m%d`
echo 'cdate and cdatem1 : ' $cdate $cdatem1
cyc=`date --date="-9 hours" +\%H` #cron runing at 9UTC  will process 00UTC cycle
echo 'cyc : ' $cyc

if [[ $cyc == '18' ]]; then # 9h latency puts us into the next day
	cdate=`date -d "$cdate -1 day" +%Y%m%d`
        cdatem1=`date -d "$cdate -2 day" +%Y%m%d`
fi

#for cyc in 00 06 12 18; do # use this in retro/catchup processing
mkdir -p $Dout/gdas.$cdate/$cyc/sss
cycdatem3h=`date -d "$cdate $cyc 3 hours ago" +%Y%m%d%H`
cycdatep3h=`date -d "$cdate $cyc +3 hours" +%Y%m%d%H`
echo $cycdatem3h $cycdatep3h
if [[ $cyc == '00' ]]; then
   lofm3=`ls $Din/${cdatem1}/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT*`
   for filem1 in $lofm3; do
       sdatem1=`echo $filem1 | cut -d'_' -f7` #i.e. take the 7th string delimited/separated by "_" !!! watch for dir names with "_"
       yyyymmddHMm1=${sdatem1:0:8}${sdatem1:9:2}${sdatem1:11:2}
       if [[ $yyyymmddHMm1 -ge $cycdatem3h'00' ]]; then
          cp $filem1 $Dout/gdas.$cdate/$cyc/sss/.
#	  ll $filem1
       fi
   done
   lof=`ls $Din/${cdate}/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT*`
   for file in $lof; do
    sdate=`echo $file | cut -d'_' -f7`
    yyyymmddHM=${sdate:0:8}${sdate:9:2}${sdate:11:2}
    if [[ $yyyymmddHM -le $cycdatep3h'00' ]]; then
       cp  $file $Dout/gdas.$cdate/$cyc/sss/.
#       ll $file
    fi
   done
else
   echo 'CYCLE IS ' ${cyc}
   lof=`ls $Din/${cdate}/wtxtbul/satSSS/SMAP/SMAP_L2B_SSS_NRT*`
   #echo ${lof}
   for file in $lof; do
       #echo 'file is' $file
       sdate=`echo $file | cut -d'_' -f7`
       #echo 'sdate is ' $sdate
       yyyymmddHM=${sdate:0:8}${sdate:9:2}${sdate:11:2}
       #echo 'yyyymmddHM is ' ${yyyymmddHM}
       #echo 'cycdatem3h is ' ${cycdatem3h}
       #echo 'cycdatep3h is ' ${cycdatep3h}
       if [[ $yyyymmddHM -ge $cycdatem3h'00' && $yyyymmddHM -le $cycdatep3h'00' ]]; then
          cp  $file $Dout/gdas.$cdate/$cyc/sss/.
#          ll $file
       fi
   done
fi #cyc

echo ' '
echo cycle $cyc DONE
echo ' '

#done #cyc
