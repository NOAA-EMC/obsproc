SHELL=/bin/sh

# If this makefile is invoked by the command line
#       make -f makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named CMD.
#
# For a quick method to move that executable to it's expected target
# location ../../exec,  use the command line
#       make -f makefile install
#
# To remove all files produced by this makefile but leave the executable(s) use
# the command line
#       make -f makefile clean
#
# To remove all files produced by this makefile including the executable(s) use
# the command line
#       make -f makefile clobber
#
# To remove the source files and this makefile use the command line
#       make -f makefile void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to Fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.

SRCS = 	syndata.f

OBJS =  syndata.o


# Tunable parameters
#
# FC            Name of the compiling system to use
# LDFLAGS       Flags to the loader
# LIBS          List of libraries
# CMD           Name of the executable
# FFLAGS        Flags to the compiler
# DEBUG         Flags for extra debugging
# DEBUG2        Flags for even more extra debugging

FC = ifort
#LDFLAGS = -Xlinker -t -mkl
#LDFLAGS = -Xlinker -M -mkl
LDFLAGS = -mkl
LIBS = ${W3EMC_LIB4} ${NEMSIO_LIB} ${BUFR_LIB4} ${W3NCO_LIB4} ${SIGIO_LIB4} ${SP_LIB4} ${BACIO_LIB4} ${NETCDF_LDFLAGS}
CMD = syndat_syndata

# For now DO NOT comment out DEBUG2 option - want to run in ops with debugger on to make sure
#  no problems
DEBUG =                -g -traceback
DEBUG2 =        -ftrapuv -check all -check nooutput_conversion -fp-stack-check -fstack-protector
FFLAGS = -O2 -convert big_endian -list -assume noold_ldout_format -auto $(DEBUG) $(DEBUG2)


# Lines from here on down should not need to be changed.  They are the actual
# rules which make uses to build CMD.

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)


.PHONY: clean clobber void install
clean:
	-rm -f $(OBJS) xref.db *.lst *.mod

clobber:	clean
	-rm -f $(CMD)

void:	clobber
	-rm -f $(SRCS) makefile

install:
	mv $(CMD) ../../exec/
