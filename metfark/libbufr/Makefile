#
#                                 Makefile for bufrdc
#
LIB = bufr
#
LIBRARY = ../fark/lib/lib$(LIB).a
#
include config.linux
#
include options_linux
#
TARGETS      = all clean
#
include sources
#
OBJECTS.F    = $(SOURCES.F:.F=.o) 
OBJECTS.f    = $(SOURCES.f:.f=.o)
OBJECTS.c    = $(SOURCES.c:.c=.o)
OBJECTS      = $(OBJECTS.F) $(OBJECTS.f) $(OBJECTS.c)
#
#
.PRECIOUS: $(LIBRARY)

all: $(LIBRARY)

$(LIBRARY): $(OBJECTS)
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@

clean:
	\rm -f $(OBJECTS)
