help([[
Load environment to build obsproc on WCOSS2
]])

load("envvar")
load("PrgEnv-intel")
load("intel/19.1.3.304")
load("craype/2.7.8")
load("cray-mpich/8.1.4")

-- Load common modules for this package
load("obsproc_common")

whatis("Description: obsproc build environment")
