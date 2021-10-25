help([[
Load environment to build obsproc on WCOSS2
]])

load("envvar")
load("PrgEnv-intel")
load(pathJoin("intel/19.1.3.304"))

-- Load common modules for this package
load("obsproc_common")

whatis("Description: obsproc build environment")
