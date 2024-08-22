help([[
Load environment to build obsproc on WCOSS2
]])

PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver")
intel_ver=os.getenv("intel_ver")
cmake_ver=os.getenv("cmake_ver")
craype_ver=os.getenv("craype_ver")
cray_mpich_ver=os.getenv("cray_mpich_ver")

hdf5_ver=os.getenv("hdf5_ver")
netcdf_ver=os.getenv("netcdf_ver")
bacio_ver=os.getenv("bacio_ver")
w3emc_ver=os.getenv("w3emc_ver")
sp_ver=os.getenv("sp_ver")
sigio_ver=os.getenv("sigio_ver")
nemsio_ver=os.getenv("nemsio_ver")
bufr_ver=os.getenv("bufr_ver")

load("envvar")
load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))
load(pathJoin("intel", intel_ver))
load(pathJoin("cmake", cmake_ver))
load(pathJoin("craype", craype_ver))
load(pathJoin("cray-mpich", cray_mpich_ver))

load(pathJoin("hdf5", hdf5_ver))
load(pathJoin("netcdf", netcdf_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("sp", sp_ver))
load(pathJoin("sigio", sigio_ver))
load(pathJoin("nemsio", nemsio_ver))
load(pathJoin("bufr", bufr_ver))

whatis("Description: obsproc build environment")
