help([[
Load environment to build obsproc on Jet
]])

prepend_path("MODULEPATH", os.getenv("spack_stack_mod_path"))

stack_intel_ver=os.getenv("stack_intel_ver") or "None"
stack_impi_ver=os.getenv("stack_impi_ver") or "None"
cmake_ver=os.getenv("cmake_ver") or "None"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("cmake", cmake_ver))

-- Load common modules for this package
load("obsproc_common")

setenv("FC", "mpiifort")

whatis("Description: obsproc build environment")
