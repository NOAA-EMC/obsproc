#!/bin/bash

# ll $DCOMROOT/202310*/seaice/pda/AMSR2*
#
#llt $DCOMROOT/202310*/seaice/pda/AMSR2-SEAICE-??_v2r2_GW1_s20231023*
#-rw-r--r-- 1 dfprod prod 63975388 Oct 23 04:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310230009050_e202310230148030_c202310230219240.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 23 04:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310230148050_e202310230327040_c202310230358190.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 23 04:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310230009050_e202310230148030_c202310230219240.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 23 04:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310230148050_e202310230327040_c202310230358190.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 23 06:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310230327060_e202310230503040_c202310230536400.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 23 06:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310230327060_e202310230503040_c202310230536400.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 23 08:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310230503050_e202310230642030_c202310230714050.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 23 08:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310230503050_e202310230642030_c202310230714050.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 23 10:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310230642050_e202310230818040_c202310230850220.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 23 10:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310230642050_e202310230818040_c202310230850220.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 23 12:09 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310230818060_e202310230957040_c202310231028320.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 23 12:09 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310230957050_e202310231136030_c202310231206510.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 23 12:09 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310230818060_e202310230957040_c202310231028320.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 23 12:09 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310230957050_e202310231136030_c202310231206510.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 23 14:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310231136040_e202310231312040_c202310231344040.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 23 14:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310231136040_e202310231312040_c202310231344040.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 23 16:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310231312050_e202310231451030_c202310231523330.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 23 16:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310231312050_e202310231451030_c202310231523330.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 23 18:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310231451050_e202310231630030_c202310231702130.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 23 18:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310231451050_e202310231630030_c202310231702130.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 23 20:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310231630040_e202310231809040_c202310231842370.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 23 20:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310231630040_e202310231809040_c202310231842370.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 24 00:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310231809050_e202310231951030_c202310232023000.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 24 00:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310231951050_e202310232133040_c202310232205560.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 24 00:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310232133060_e202310232312040_c202310232345520.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 24 00:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310231809050_e202310231951030_c202310232023000.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 24 00:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310231951050_e202310232133040_c202310232205560.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 24 00:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310232133060_e202310232312040_c202310232345520.nc
#-rw-r--r-- 1 dfprod prod 63975388 Oct 24 02:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202310232312050_e202310240051030_c202310240126240.nc
#-rw-r--r-- 1 dfprod prod 40954311 Oct 24 02:00 /lfs/h1/ops/prod/dcom/20231023/seaice/pda/AMSR2-SEAICE-SH_v2r2_GW1_s202310232312050_e202310240051030_c202310240126240.nc
#
# OBSERVATIONS:
# Number of files per day varies
# last file of yesterday (by _s date) may have ~2h of today's data
# today's data continue coming until ~2AM next day
# each file arrives 3-4 hours later
# run cron on 4 hour delay!!!

export Din=$DCOMROOT #/20231023/seaice/pda/AMSR2*
#export Dout=/lfs/h2/emc/obsproc/noscrub/iliana.genkova/MARINE_code/
export Dout=/lfs/h2/emc/obsproc/noscrub/iliana.genkova/CRON/SOCA/com/obsproc/v1.2  #gfs.20231026/00/atmos/

#for testing
#cdate=20231023
#cdatem1=20231022
#cyc=06

cdate=`date +\%Y\%m\%d`
cdatem1=`date -d "$cdate -1 day" +%Y%m%d`
echo 'cdate and cdatem1 : ' $cdate $cdatem1
cyc=`date --date="-4 hours" +\%H` #cron runs at 4UTC, and will process 00UTC cycle
echo 'cyc : ' $cyc

#00
if [[ $cyc == '00' ]]; then
echo cycle 00 
mkdir -p $Dout/gdas.$cdate/$cyc/icec
ls $Dout/gdas.$cdate/$cyc/icec
# -3
lof=`ls $Din/${cdatem1}/seaice/pda/AMSR2*`
for file in $lof; do
    sdate=`echo $file | cut -d'_' -f5`
    edate=`echo $file | cut -d'_' -f6`
    avg=$(echo $((${sdate:1:10}+${edate:1:10})) / 2 | bc -l)
#    if [ ${sdate:1:10} -gt 2021063020 ]; then
    if [ $(printf "%.0f" "$avg") -gt ${cdatem1}21 ]; then
          echo $file
          cp -p $file $Dout/gdas.${cdate}/${cyc}/icec
    fi
done
#+3
lof=`ls $Din/${cdate}/seaice/pda/AMSR2*`
for file in $lof; do
    sdate=`echo $file | cut -d'_' -f5`
    edate=`echo $file | cut -d'_' -f6`
    avg=$(echo $((${sdate:1:10}+${edate:1:10})) / 2 | bc -l)
#    if [ ${sdate:1:10} -gt 2021063020 ]; then
    if [ $(printf "%.0f" "$avg") -le ${cdate}03 ]; then
          echo $file
          cp -p $file $Dout/gdas.${cdate}/${cyc}/icec
    fi
done
echo ' '
fi

#06
if [[ $cyc == '06' ]]; then
echo cycle 06
mkdir -p $Dout/gdas.${cdate}/${cyc}/icec
# -3 = +3
lof=`ls $Din/${cdate}/seaice/pda/AMSR2*`
for file in $lof; do
    sdate=`echo $file | cut -d'_' -f5`
    edate=`echo $file | cut -d'_' -f6`
    avg=$(echo $((${sdate:1:10}+${edate:1:10})) / 2 | bc -l)
#    if [ ${sdate:1:10} -gt 2021063020 ]; then
    if [[ $(printf "%.0f" "$avg") -ge ${cdate}03 && $(printf "%.0f" "$avg") -le ${cdate}09 ]]; then
          echo $file
          cp -p $file $Dout/gdas.${cdate}/${cyc}/icec
    fi
done
echo ' '
fi


#12
if [[ $cyc == '12' ]]; then
echo cycle 12
mkdir -p $Dout/gdas.${cdate}/${cyc}/icec
# -3 = +3
lof=`ls $Din/${cdate}/seaice/pda/AMSR2*`
for file in $lof; do
    sdate=`echo $file | cut -d'_' -f5`
    edate=`echo $file | cut -d'_' -f6`
    avg=$(echo $((${sdate:1:10}+${edate:1:10})) / 2 | bc -l)
#    if [[ ${sdate:1:10} -gt 2021063020 ]; then
    if [[ $(printf "%.0f" "$avg") -ge ${cdate}09 && $(printf "%.0f" "$avg") -le ${cdate}15 ]]; then
          echo $file
          cp -p $file $Dout/gdas.${cdate}/${cyc}/icec
    fi
done
echo ' '
fi


#18
if [[ $cyc == '18' ]]; then
echo cycle 18
mkdir -p $Dout/gdas.${cdate}/${cyc}/icec
# -3 = +3
lof=`ls $Din/${cdate}/seaice/pda/AMSR2*`
for file in $lof; do
    sdate=`echo $file | cut -d'_' -f5`
    edate=`echo $file | cut -d'_' -f6`
    avg=$(echo $((${sdate:1:10}+${edate:1:10})) / 2 | bc -l)
#    if [ ${sdate:1:10} -gt 2021063020 ]; then
    if [[ $(printf "%.0f" "$avg") -ge ${cdate}15 && $(printf "%.0f" "$avg") -le ${cdate}21 ]]; then
          echo $file
          cp -p $file $Dout/gdas.${cdate}/${cyc}/icec
    fi
done
echo ' '
fi

#exit 0 #kicks me out of WCOSS2

#start=`echo "$Din/20210630/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202106300038170_e202106300217150_c202106300335580.nc" | cut -d'_' -f5`
#echo ${start:1:10}
#end=`echo "$Din/20210630/seaice/pda/AMSR2-SEAICE-NH_v2r2_GW1_s202106300038170_e202106300217150_c202106300335580.nc" | cut -d'_' -f6`
#echo ${end:1:10}
#exit 0

##SIMPLE ALTERNATIVE FOR SST (from SHASTRI PATURI)
#####################################
## SST
######################################
#for cdate in 20210701 20210702; do
#cdatem1=`date -d "$cdate -1 day" +%Y%m%d`
#echo $cdate $cdatem1
## SST GOES (ABI) and HIMAWARI (AHI)
#for typ in ABI AHI; do
# #00
# cp -p $Din/${cdatem1}/sst/${cdatem1}{21..23}*${typ}* $Dout/gdas.${cdate}/00/sst/
# cp -p $Din/${cdate}/sst/${cdate}{00..03}*${typ}* $Dout/gdas.${cdate}/00/sst/
# ## 06
# cp -p $Din/${cdate}/sst/${cdate}{03..05}*${typ}* $Dout/gdas.${cdate}/00/sst/
# cp -p $Din/${cdate}/sst/${cdate}{06..09}*${typ}* $Dout/gdas.${cdate}/00/sst/
# ## 12
# cp -p $Din/${cdate}/sst/${cdate}{09..11}*${typ}* $Dout/gdas.${cdate}/00/sst/
# cp -p $Din/${cdate}/sst/${cdate}{12..15}*${typ}* $Dout/gdas.${cdate}/00/sst/
# ## 18
# cp -p $Din/${cdate}/sst/${cdate}{15..17}*${typ}* $Dout/gdas.${cdate}/00/sst/
# cp -p $Din/${cdate}/sst/${cdate}{18..21}*${typ}* $Dout/gdas.${cdate}/00/sst/
#done #typ
##done #cdate
