#
#
#
LIB = bufrc

LIBRARY = ../fark/lib/lib$(LIB).a
#
include  config.linux
#
include  options_linux
#
TARGETS      = all clean
#
include sources
#
OBJECTS.F    = $(SOURCES.F:.F=.o) 
OBJECTS.c    = $(SOURCES.c:.c=.o)
OBJECTS      = $(OBJECTS.F) $(OBJECTS.c)
FASTOBJ.o    = $(FASTSRC.c:.c=.o)
FASTOBJ      = $(FASTOBJ.o)
#
#
.PRECIOUS: $(LIBRARY)

all: $(LIBRARY)

$(FASTOBJ.o): $(FASTSRC.c)
	$(CC) $(FASTCFLAGS) -c $(FASTSRC.c)

$(LIBRARY): $(OBJECTS) $(FASTOBJ)
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@

include make.dep

clean:
	\rm -f $(OBJECTS) $(FASTOBJ) *.f
