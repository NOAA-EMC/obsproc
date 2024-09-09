#!/bin/bash

set -eux

# Location of PWD and package source directory.
readonly pkg_root=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)

# User Options
BUILD_TYPE=${BUILD_TYPE:-"Release"}
BUILD_DIR=${BUILD_DIR:-"${pkg_root}/build"}
CMAKE_OPTS=${CMAKE_OPTS:-}
INSTALL_TARGET=${INSTALL_TARGET:-"wcoss2"}
INSTALL_PREFIX=${INSTALL_PREFIX:-"${pkg_root}/install"}

target="${INSTALL_TARGET,,}"
if [[ "${target}" =~ ^(wcoss2|hera|orion|jet|hercules|gaea)$ ]]; then
  # prepare the target specific build.ver and run.ver
  cd "${pkg_root}/versions" || exit 1
  rm -f build.ver run.ver
  cp "build.${target}.ver" "build.ver"
  cp "run.${target}.ver"   "run.ver"
  cd "${pkg_root}" || exit 1
  export HOMEobsproc="${pkg_root}"
  source "${pkg_root}/versions/build.ver"
  unset HOMEobsproc
  set +x
  module purge
  module use "${pkg_root}/modulefiles"
  module load "obsproc_${target}"
  module list
  set -x
else
  echo "WARNING: Unknown target"
fi

# Create a build directory and cd into it.
[[ -d build  ]] && rm -rf build
mkdir -p build && cd build

# build and install.
cmake -DCMAKE_INSTALL_PREFIX="${INSTALL_PREFIX}" \
      -DCMAKE_INSTALL_BINDIR=exec \
      -DCMAKE_BUILD_TYPE="${BUILD_TYPE}" \
      ..
make -j "${BUILD_JOBS:-6}" VERBOSE="${BUILD_VERBOSE:-}"
make install

# Remove build directory upon successfull build and install
cd "${pkg_root}"
rm -rf build

exit 0
