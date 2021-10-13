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

SRCS = 	prepacqc.f acftobs_qc.f tranQCflags.f output_acqc_prof.f output_acqc_noprof.f input_acqc.f indexc40.f sub2mem_mer.f sub2mem_um.f pmat.f90 pmat2.f90 pmat3.f90 pietc.f90 pspl.f90 pkind.f90

OBJS =   prepacqc.o acftobs_qc.o tranQCflags.o output_acqc_prof.o output_acqc_noprof.o input_acqc.o indexc40.o sub2mem_mer.o sub2mem_um.o pmat.o pmat2.o pmat3.o pietc.o pspl.o pkind.o


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
#LDFLAGS = -Xlinker -t
#LDFLAGS = -Xlinker -M
LIBS = ${W3NCO_LIB4} ${W3EMC_LIB4} ${BUFR_LIB4}
CMD = prepobs_prepacqc

DEBUG =                -g -traceback
#DEBUG2 =        -ftrapuv -check all -check nooutput_conversion -fp-stack-check -fstack-protector
FFLAGS = -O2 -convert big_endian -list -assume noold_ldout_format $(DEBUG) $(DEBUG2)


# Lines from here on down should not need to be changed.  They are the actual
# rules which make uses to build CMD.

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

.SUFFIXES : .o .f .f90

.f90.o:
	@echo
	@echo '---> Compiling $<'
	$(FC) $(FFLAGS) -c $<

.f.o:
	@echo
	@echo '---> Compiling $<'
	$(FC) $(FFLAGS) -c $<

# ------------
# Dependencies
# ------------
pkind.o : pkind.f90
pietc.o : pietc.f90 pkind.o
pmat.o  : pmat.f90 pkind.o pietc.o
pmat2.o : pmat2.f90 pkind.o pmat.o
pmat3.o : pmat3.f90 pkind.o pietc.o pmat.o pmat2.o
pspl.o: pspl.f90 pkind.o pietc.o pmat2.o pmat3.o
sub2mem_mer.o : sub2mem_mer.f pkind.o pspl.o

.PHONY: clean clobber void install
clean:
	-rm -f $(OBJS) xref.db *.lst *.mod

clobber:	clean
	-rm -f $(CMD)

void:	clobber
	-rm -f $(SRCS) makefile

install:
	mv $(CMD) ../../exec/
