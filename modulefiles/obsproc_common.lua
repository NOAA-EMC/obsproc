help([[
Load common modules to build obsproc on all machines
]])

netcdf_ver=os.getenv("netcdf_ver") or "default"
bacio_ver=os.getenv("bacio_ver") or "default"
w3emc_ver=os.getenv("w3emc_ver") or "default"
w3nco_ver=os.getenv("w3nco_ver") or "default"
sp_ver=os.getenv("sp_ver") or "default"
sigio_ver=os.getenv("sigio_ver") or "default"
nemsio_ver=os.getenv("nemsio_ver") or "default"
bufr_ver=os.getenv("bufr_ver") or "default"

load(pathJoin("netcdf", netcdf_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("w3nco", w3nco_ver))
load(pathJoin("sp", sp_ver))
load(pathJoin("sigio", sigio_ver))
load(pathJoin("nemsio", nemsio_ver))
load(pathJoin("bufr", bufr_ver))
