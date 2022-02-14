help([[
Load environment to build obsproc on Orion
]])

load("cmake/3.18.1")

prepend_path("MODULEPATH", "/apps/contrib/NCEP/libs/hpc-stack/modulefiles/stack")
load("hpc/1.1.0")
load("hpc-intel/2018.4")
load("hpc-impi/2018.4")

-- Load common modules for this package
load("obsproc_common")

whatis("Description: obsproc build environment")
