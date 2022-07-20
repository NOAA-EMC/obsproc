#!/bin/sh  
#PBS -N obsproc_%JTYP%_dump_%PDY%_%CC%_%DESC%
#PBS -j oe
#PBS -S /bin/bash
#PBS -q dev
#PBS -l walltime=00:20:00
#PBS -A OBSPROC-DEV
#PBS -l place=vscatter,select=1:ncpus=14:mem=60gb
#PBS -l debug=true

##############################################
# Submit notes:%
# Run from [ps]tmp when running manually
# When running from cron, output is written to /u/$USER. Bottom of trigger mv's to stmp
# For specific PDY:
# > jtyp=[gdas][gfs] cyc=00 PDY=20170126 desc=somethingfun /u/Shelley.Melchior/bin/cycqsub /path/to/triggers/jglobal_dump.wc2.pbs
# For latest/current PDY:
# > jtyp=[gdas][gfs] cyc=00 desc=somethingfun /u/Shelley.Melchior/bin/cycqsub /path/to/triggers/jglobal_dump.wc2.pbs
# cycqsub location: /u/Shelley.Melchior/bin 
##############################################

set -xu

export envir=prod

export cyc=%CC%
DESC=%DESC%
JTYP=%JTYP%
export job=${JTYP}_dump_$cyc
export jobid=$job.$PBS_JOBID
export PDY=%PDY%

userROOT=/lfs/h2/emc/obsproc/noscrub/$USER

export obsproc_ver=v1.0
export bufr_dump_ver=1.0.0
export obsNET=obsproc
PACKAGEROOTpara=/lfs/h1/ops/para/packages
#export HOMEobsproc=${PACKAGEROOTpara}/obsproc.${obsproc_ver}   # NCO para
#export HOMEobsproc=${PACKAGEROOT}/obsproc.${obsproc_ver}        # NCO prod
export HOMEobsproc=${userROOT}/wrapper_scripts/JOBS/Sentinel_6_issue97485/obsproc_run_Sentinel_6_issue97485      # local

#VERSION_FILE=$HOMEobsproc/versions/run.ver
VERSION_FILE=${userROOT}/wrapper_scripts/JOBS/Sentinel_6_issue97485/obsproc/versions/run.ver
if [ -f $VERSION_FILE ]; then
   . $VERSION_FILE
else
  echo Need version info...  Exiting...
 exit 7
fi

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
module unload bufr_dump
#module use /apps/ops/para/nco/modulefiles/compiler/intel/19.1.3.304 
module use ${userROOT}/githubrun/bufr_dump_run/modulefiles
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
export SENDDBN=NO   # developer
export SENDSDM=NO   # developer

#export TANK=$DCOMROOT
#export TANK_000015=$DCOMROOT			#echo $DCOMROOT /lfs/h1/ops/prod/dcom   prod tanks?
#export TANK_000015=/lfs/h1/ops/dev/dcom        #dev tanks?
export TANK_003010=/lfs/h1/ops/dev/dcom 

export DATAROOT=/lfs/h2/emc/ptmp/$USER 
export jlogfile=/lfs/h2/emc/ptmp/$USER/${JTYP}.$PDY.jlogfile

export COMOUT_ROOT=${DATAROOT}/CRON/${DESC}/com
export COMPONENT=atmos

export DEBUG_LEVEL=3
export LOUD=ON
export KEEPDATA=YES


$HOMEobsproc/jobs/JOBSPROC_GLOBAL_DUMP
err=$?

# When run from cron, the stdout is written to /u/$USER
# mv to stmp
pbsjobid=$(echo $PBS_JOBID | cut -d'.' -f1)
cronlogfile=/u/$USER/$PBS_JOBNAME.o$pbsjobid
#cronlogfile=/lfs/h2/emc/ptmp/$USER/$PBS_JOBNAME.o$pbsjobid
outputdir=/lfs/h2/emc/ptmp/${USER}/CRON/${DESC}/output
if [ -f "$cronlogfile" ]; then
  mkdir -p $outputdir
  mv $cronlogfile ${outputdir}/$PBS_JOBNAME.o$pbsjobid
fi

# If you wish to only run the dump job, un-comment the exit line below


# Kick off dump_post job
if [ $err -eq 0 ]; then
  echo "submit jglobal_dump_post"
  jtyp=$JTYP PDY=$PDY desc=$DESC bash -l /lfs/h2/emc/obsproc/noscrub/Steve.Stegall/wrapper_scripts/JOBS/Sentinel_6_issue97485/obsproc/triggers/cycqsub \
  /lfs/h2/emc/obsproc/noscrub/Steve.Stegall/wrapper_scripts/JOBS/Sentinel_6_issue97485/obsproc/triggers/jglobal_dump_post.wc2.pbs.Sentinel_6
fi

# Kick off prep job
if [ $err -eq 0 ]; then
  echo "submit jglobal_prep"
  jtyp=$JTYP PDY=$PDY desc=$DESC bash -l /lfs/h2/emc/obsproc/noscrub/Steve.Stegall/wrapper_scripts/JOBS/Sentinel_6_issue97485/obsproc/triggers/cycqsub \
  /lfs/h2/emc/obsproc/noscrub/Steve.Stegall/wrapper_scripts/JOBS/Sentinel_6_issue97485/obsproc/triggers/jglobal_prep.wc2.pbs_Sentinel_6
fi

exit
