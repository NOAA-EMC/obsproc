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

SRCS = glerldictmod.f90 glerlmain.f90 landtowater.f90 spd_to_dir.f90 td_to_q.f90 uv_to_sd.f90 watertoland.f90 uz.f uzl.f

#OBJS = glerldictmod.o landtowater.o spd_to_dir.o td_to_q.o uv_to_sd.o watertoland.o

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
#LDFLAGS = 
LIBS = ${W3NCO_LIB4} ${BUFR_LIB4}
CMD = prepobs_glerladj

DEBUG =                -g -traceback
DEBUG2 =        -ftrapuv -check all -check nooutput_conversion -fp-stack-check -fstack-protector
FFLAGS = -O3 -CB

OBJS = glerldictmod.o glerlmain.o landtowater.o sped_to_dir.o td_to_q.o uv_to_sd.o watertoland.o uz.o uzl.o

all:		$(CMD)

$(CMD):		$(SRCS)
	$(FC) $(LDFLAGS) -o $(@) $(SRCS) $(LIBS) $(DEBUG) $(DEBUG2) $(FFLAGS)


.PHONY: clean clobber void install
clean:
	-rm -f $(OBJS) xref.db *.lst *.mod

clobber:        clean
	-rm -f $(CMD)

void:   clobber
	-rm -f $(SRCS) makefile

install:
	mv $(CMD) ../../exec/
