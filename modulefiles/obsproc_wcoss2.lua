help([[
Load environment to build obsproc on WCOSS2
]])

PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver")
intel_ver=os.getenv("intel_ver")
cmake_ver=os.getenv("cmake_ver")
craype_ver=os.getenv("craype_ver")
cray_mpich_ver=os.getenv("cray_mpich_ver")

-- load("envvar")
load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))
load(pathJoin("intel", intel_ver))
load(pathJoin("cmake", cmake_ver))
load(pathJoin("craype", craype_ver))
load(pathJoin("cray-mpich", cray_mpich_ver))

-- Load common modules for this package
load("obsproc_common")

whatis("Description: obsproc build environment")
