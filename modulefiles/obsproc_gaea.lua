help([[
Load environment to build obsproc on Gaea
]])

prepend_path("MODULEPATH", os.getenv("spack_stack_mod_path"))

stack_intel_ver=os.getenv("stack_intel_ver") or "None"
stack_cray_mpich_ver=os.getenv("stack_cray_mpich_ver") or "None"
cmake_ver=os.getenv("cmake_ver") or "None"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-cray-mpich", stack_cray_mpich_ver))
load(pathJoin("cmake", cmake_ver))

-- Load common modules for this package
load("obsproc_common")

whatis("Description: obsproc build environment")
