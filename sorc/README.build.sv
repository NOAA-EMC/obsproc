Library versions and variables are defined by sourcing load_libs.rc which loads library modules.
  -- This is done for you in build.sh.
  -- To test with versions of libraries that don't yet have proper module files,
     one can modify setlibs.rc as needed and modify build.sh to source setlibs.rc
     instead of load_libs.rc


To build ALL codes, simply run:
  ./build.sh
or, if on theia (or hera, etc):
  SITE=theia ./build.sh   (or the equivalent if not in an sh-like shell)

To build only select codes (to save time):
  ./build.sh <some_code.fd ...>

(NOTE: If environment variable clobber is exported to build.sh with value
       "clobber_no", then the make clobber step is skipped.  This may be useful
       if one is debugging only one source code for a program with multiple
       source codes.)


To install builds (creating exec dir if necessary):
  ./install.sh
or
  ./install.sh <some_code.fd ...>

This assumes the exec dir belongs one directory back from this directory (and two 
directories back from each code directory).  If you don't trust install scripts
and targets, don't use 'em.


To clean directories
  ./clean.sh
or
  ./clean.sh <some_code.fd ...>


Examples:
  ./build.sh
  ./install.sh
  ./clean.sh
 - or, for a specific code -
  ./build.sh <some_code.fd>
  ./install.sh <some_code.fd>
  ./clean.sh <some_code.fd>
  
