help([[
Load environment to build obsproc on NOAA cloud
]])

prepend_path("MODULEPATH", os.getenv("spack_stack_mod_path"))

stack_intel_ver=os.getenv("stack_intel_ver") or "2021.10.0"
stack_impi_ver=os.getenv("stack_impi_ver") or "2021.10.0"
cmake_ver=os.getenv("cmake_ver") or "3.23.1"

load("gnu")
load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("cmake", cmake_ver))

-- Load common modules for this package
load("obsproc_common")

whatis("Description: obsproc build environment")
