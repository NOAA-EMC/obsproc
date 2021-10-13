# makefile - prepobs_prepanow
#
# Required environment variables
#   W3NC0_LIB4 - full spec for w3nco (4-byte) library
#   BUFR_LIB4  - full spec for BUFRLIB (4-byte) library
#
# modification history
#  14 Oct 2014 - cp'd fr WCOSS prod ver of 13 Aug 2013
#                config'd for vertical structure (LIBS)
#                updated w/ env vars (eg, W3NCO_LIB4, BUFR_LIB4)
#----
#
SRCS=		prepanow.f 
OBJS=		$(SRCS:.f=.o)

FC=		ifort
#LIBS=		-L/nwprod/lib -lw3nco_v2.0.6_4 -lbufr_v10.2.5_4_64
LIBS=		${W3NCO_LIB4} ${BUFR_LIB4}

DEBUG=		-g -traceback
#DEBUG2=		-ftrapuv -check all -check nooutput_conversion -fp-stack-check -fstack-protector
FFLAGS=		-O2 $(DEBUG) $(DEBUG2)
LDFLAGS=	

CMD=		prepobs_prepanow

# Lines from here on down should not need to be changed.  They are the actual
# # rules which make uses to build CMD.

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS) *.lst

clobber:        clean
	-rm -f $(CMD)

install:
	mv $(CMD) ../../exec/

# -- end of makefile (prepobs_prepanow)
