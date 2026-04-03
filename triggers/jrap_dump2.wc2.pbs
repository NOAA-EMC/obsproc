#!/bin/bash
#PBS -N obsproc_%JTYP%_dump2_%PDY%_%CC%_%DESC%
#PBS -j oe
#PBS -S /bin/bash
#PBS -q dev
#PBS -A OBSPROC-DEV
#PBS -l walltime=00:45:00
#PBS -l place=vscatter,select=1:ncpus=14:mem=120gb
####PBS -l place=vscatter,select=1:ncpus=8:mem=50gb
#PBS -l debug=true
#####################################################################
# Submit notes:
# Run from [ps]tmp when running manually
# When running from cron, output is written to /u/$USER. Bottom of trigger mv's to stmp
# For specific PDY:
# > jtyp=rap|rap_e|rap_p cyc=00 PDY=20170126 desc=somethingfun /u/Shelley.Melchior/bin/cycqsub /path/to/triggers/jrap_dump.wc2.pbs
# For latest/current PDY:
# > jtyp=rap cyc=00 /u/Shelley.Melchior/bin/cycqsub /path/to/triggers/jrap_dump.wc2.pbs
# cycqsub location: /u/Shelley.Melchior/bin
#####################################################################

set -xu


export envir=prod
export cyc=%CC%
type=%DESC%
JTYP=%JTYP%
DESC=%DESC%
if [ $JTYP = "rap_e" ]; then
  export job=$(echo $JTYP | cut -c1-3)_dump_erly_$cyc
elif [ $JTYP = "rap_p" ]; then
  export job=$(echo $JTYP | cut -c1-3)_dump_pcyc_$cyc
else
  export job=${JTYP}_dump_$cyc
fi
export jobid=$job.$PBS_JOBID
export PDY=%PDY%

userROOT=/lfs/h2/emc/obsproc/noscrub/${USER}

export obsproc_ver=v5.0
export obsproc_ver_pckg=v1.0.2
export obsNET=obsproc
PACKAGEROOTpara=/lfs/h1/ops/para/packages
#export HOMEobsproc=${PACKAGEROOTpara}/obsproc.${obsproc_ver_pckg}   # NCO para
#export HOMEobsproc=${PACKAGEROOT}/obsproc.${obsproc_ver_pckg}        # NCO prod
#export HOMEobsproc=${userROOT}/install/obsproc-rmprep      # local
#export HOMEobsproc=${userROOT}/GIT/play/obsproc.RAPall
export HOMEobsproc=${userROOT}/GIT/play/obsproc.G             # IG local  

VERSION_FILE=$HOMEobsproc/versions/run.ver
if [ -f $VERSION_FILE ]; then
  . $VERSION_FILE
else
  echo Need version info...  Exiting...
  exit 7
fi

export bufr_dump_ver=5.0.0 #1.0.0

# Load the modules specified in $VERSION_FILE
module load libjpeg
module load grib_util/${grib_util_ver}
module load netcdf/${netcdf_ver}
module load intel/${intel_ver}
module load craype/${craype_ver}
module load cray-mpich/${cray_mpich_ver}
module load cray-pals/${cray_pals_ver}
module load cfp/${cfp_ver}
# use para installation
#module use /apps/ops/para/nco/modulefiles/compiler/intel/19.1.3.304 
# use local bufr_dump installation
module unload bufr_dump
#module use ${userROOT}/install/bufr_dump/modulefiles
#module use ${userROOT}/install/bufr-dump-rel.1.0.0/modulefiles
module use /lfs/h2/emc/obsproc/noscrub/${USER}/GIT/play/bufr-dump.G/install/modulefiles #Gv1.1.0
module load bufr_dump/${bufr_dump_ver}

# Be sure the modules are loaded 
if [[ $(echo $LOADEDMODULES | egrep -c "(^|:)prod_util/") -eq 0 ]]; then echo "prod_util is not loaded!"; fi
if [[ $(echo $LOADEDMODULES | egrep -c "(^|:)prod_envir/") -eq 0 ]]; then echo "prod_envir is not loaded!"; fi
if [[ $(echo $LOADEDMODULES | egrep -c "(^|:)libjpeg/") -eq 0 ]]; then echo "libjpeg is not loaded!"; fi
if [[ $(echo $LOADEDMODULES | egrep -c "(^|:)grib_util/") -eq 0 ]]; then echo "grib_util is not loaded!"; fi
if [[ $(echo $LOADEDMODULES | egrep -c "(^|:)netcdf/") -eq 0 ]]; then echo "netcdf is not loaded!"; fi
if [[ $(echo $LOADEDMODULES | egrep -c "(^|:)bufr_dump/") -eq 0 ]]; then echo "bufr_dump is not loaded!"; fi
if [[ $(echo $LOADEDMODULES | egrep -c "(^|:)intel/") -eq 0 ]]; then echo "intel is not loaded!"; fi
if [[ $(echo $LOADEDMODULES | egrep -c "(^|:)craype/") -eq 0 ]]; then echo "craype is not loaded!"; fi
if [[ $(echo $LOADEDMODULES | egrep -c "(^|:)cray-mpich/") -eq 0 ]]; then echo "cray-mpich is not loaded!"; fi
if [[ $(echo $LOADEDMODULES | egrep -c "(^|:)cray-pals/") -eq 0 ]]; then echo "cray-pals is not loaded!"; fi
if [[ $(echo $LOADEDMODULES | egrep -c "(^|:)cfp/") -eq 0 ]]; then echo "cfp is not loaded!"; fi

export SENDECF=NO   # developer
export SENDSDM=NO   # developer
export SENDDBN=NO   # developer

export TANK=$DCOMROOT
#test
#export TANK_002101=/lfs/h1/ops/dev/dcom/ # 002103->002101dev uprair from ship, to catch Urban sky

#test tank size check for MSONET and MSONE1 tanks
#export TANK_MAX_255003=500
#export TANK_001103=/lfs/h1/ops/dev/dcom  #NDBC buoys in BUFR have HSMSL (think GL,M.Morris)
export TANK_004030=/lfs/h1/ops/dev/dcom #Windborne and Sorcerer
export TANK_002102=/lfs/h1/ops/dev/dcom/ # 002103->002101->002102 dev uprair from ship, to catch Urban sky

export DATAROOT=/lfs/h2/emc/stmp/${USER}
export jlogfile=/lfs/h2/emc/stmp/${USER}/${JTYP}.$PDY.jlogfile

export COMOUT_ROOT=${DATAROOT}/CRON/${DESC}/com

export LOUD=on #ON
export KEEPDATA=YES
export DEBUG_LEVEL=3


$HOMEobsproc/jobs/JOBSPROC_RAP_DUMP2
err=$?


# When run from cron, the stdout is written to /u/$USER
# mv to stmp
pbsjobid=$(echo $PBS_JOBID | cut -d'.' -f1)
cronlogfile=/u/$USER/$PBS_JOBNAME.o$pbsjobid
outputdir=/lfs/h2/emc/stmp/$USER/CRON/${DESC}/output
if [ -f "$cronlogfile" ]; then
  mkdir -p $outputdir
  mv $cronlogfile ${outputdir}/$PBS_JOBNAME.o$pbsjobid
fi

# ... existing Script B code ...
if [ $err -eq 0 ]; then
    touch /lfs/h2/emc/stmp/${USER}/${JTYP}_dump2_done.txt
    # Check if A is already done
    if [ -f /lfs/h2/emc/stmp/${USER}/${JTYP}_dump_done.txt ]; then
        echo "B finished last. Triggering Script C."
        echo "submit jrap_dump_post"
  jtyp=$JTYP PDY=$PDY desc=$DESC bash -l /u/$USER/bin/cycqsub \
  ${userROOT}/Trigs.KEEP/jrap_dump_post.wc2.pbs.G

        # Clean up flags for the next run
        rm -f /lfs/h2/emc/stmp/${USER}/${JTYP}_*_done.txt
    fi
fi

exit

# If you wish to only run the dump job, un-comment the exit line below
echo "leaving second rap dump job"
exit

echo "we are not here"

## Kick off dump_post job
#if [ $err -eq 0 ]; then
#  echo "submit jrap_dump_post"
#  jtyp=$JTYP PDY=$PDY desc=$DESC bash -l /u/$USER/bin/cycqsub \
#  /lfs/h2/emc/obsproc/noscrub/$USER/Trigs.KEEP/jrap_dump_post.wc2.pbs.G
#fi

## Kick off prep job
#if [ $err -eq 0 ]; then
#  echo "submit jrap_prep"
#  jtyp=$JTYP PDY=$PDY desc=$DESC bash -l /u/$USER/bin/cycqsub \
#  /lfs/h2/emc/obsproc/noscrub/$USER/Trigs.KEEP/jrap_prep.wc2.pbs.G
#fi

exit
