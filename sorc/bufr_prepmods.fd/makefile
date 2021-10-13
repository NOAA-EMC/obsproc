# makefile (bufr_prepmods) - 14 Feb 2019
# 
#  4 Feb 2013 JWhiting cp'd fr IBM/AIX ver of 22 Mar 2012 (dated 11 Nov 2003)
#              config's for use on WCOSS linux (ifort cmplr, new lib specs)
#                rm'd profiler refs
#                parameterized cmpl debug options (-g, -traceback))
#                set optimization to default (-O2)
#                rm'd unneeded compiler options (list, source, etc)
#                set to use default variable sizes (-i4, -r4)
#                 added developmental cmpl options (CHKOUT)
# 14 Feb 2019 JWhiting modified for use on phase 3 (DELL) platforms)
#                config'd to use inherited library specs
# ----
#
# Source files & objects
SRCS	= bufr_prepmods.f clonlat.f mnemonics.f obname.f raddate.f ssmipn.f tideg.f
OBJS	= $(SRCS:.f=.o) 


FC =            ifort
LIBS=           $(W3NCO_LIB4) $(W3EMC_LIB4) $(BUFR_LIB4)
#LDFLAGS=

CMD = 		bufr_prepmods

DEBUG=          -g -traceback
DEBUG2=        -ftrapuv  -check all  -fp-stack-check  -fstack-protector
FFLAGS= -O2 $(DEBUG) $(DEBUG2)

# Lines from here on down should not need to be changed.  They are the actual
# rules which make uses to build CMD.

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC)           $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD)

void:	clobber
	-rm -f $(SRCS) makefile

install:
	mv $(CMD) ../../exec/

# -- end of Makefile

