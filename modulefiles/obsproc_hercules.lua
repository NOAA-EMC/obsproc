help([[
Load environment to build obsproc on Hercules
]])

prepend_path("MODULEPATH", os.getenv("spack_stack_mod_path"))

stack_intel_ver=os.getenv("stack_intel_ver")
stack_impi_ver=os.getenv("stack_impi_ver")
cmake_ver=os.getenv("cmake_ver")

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))

load(pathJoin("cmake", cmake_ver))

-- Load common modules for this package
load("obsproc_common")

-- On Hercules, MKL needs to be loaded separately
load(pathJoin("intel-oneapi-mkl", intel_mkl_ver))

whatis("Description: obsproc build environment")
