#!/bin/bash

set -eux

# Location of PWD and package source directory.
readonly pkg_root=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)

# User Options
BUILD_TYPE=${BUILD_TYPE:-"Release"}
BUILD_DIR=${BUILD_DIR:-"$pkg_root/build"}
CMAKE_OPTS=${CMAKE_OPTS:-}
INSTALL_TARGET=${INSTALL_TARGET:-"wcoss2"}
INSTALL_PREFIX=${INSTALL_PREFIX:-"$pkg_root/install"}

# By default; build as if NCO is executing this script
BUILD_4NCO=${BUILD_4NCO:-"YES"}

target=$(echo $INSTALL_TARGET | tr [:upper:] [:lower:])
if [[ "$target" =~ ^(wcoss2|hera|orion|jet)$ ]]; then
  source $pkg_root/versions/build.ver
  set +x
  module use $pkg_root/modulefiles
  module load obsproc_$target
  module list
  set -x
fi

# Re-use or create a new BUILD_DIR (Default: create new BUILD_DIR)
[[ ${BUILD_CLEAN:-"YES"} =~ [yYtT] ]] && rm -rf $BUILD_DIR
mkdir -p $BUILD_DIR && cd $BUILD_DIR

# Collect BUILD Options
CMAKE_OPTS+=" -DCMAKE_BUILD_TYPE=$BUILD_TYPE"

# Install destination for built executables, libraries, CMake Package config
CMAKE_OPTS+=" -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX"

# NCO/Jobs expects executables in exec/
CMAKE_OPTS+=" -DCMAKE_INSTALL_BINDIR=exec"

# build and install.
cmake $CMAKE_OPTS $pkg_root
make -j ${BUILD_JOBS:-8} VERBOSE=${BUILD_VERBOSE:-}
make install

[[ ${BUILD_4NCO} =~ [yYtT] ]] || exit 0

#############################################################################
# This section to be removed when NCO is comfortable with the typical
# `cmake`, `make` and `make install` process.
# To abide by current NCO working practices,
# manually copy compiled executables from `$INSTALL_PREFIX/`
# directory to `pkg_root` and then remove `$INSTALL_PREFIX/`
mkdir -p $pkg_root/exec
cp -f $INSTALL_PREFIX/exec/* $pkg_root/exec/
rm -rf $INSTALL_PREFIX
#############################################################################

# Remove build directory upon successfull build and install
rm -rf ${BUILD_DIR}

exit 0
