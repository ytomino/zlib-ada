SOURCEDIR=../source

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

ifeq ($(TARGET),$(HOST))
GNATMAKE=gnatmake
GCC=$(dir $(shell which $(GNATMAKE)))gcc
BUILDDIR=build
IMPORTDIR:=import
else
GNATMAKE=$(TARGET)-gnatmake
GCC=$(TARGET)-gcc
BUILDDIR=build-$(TARGET)
IMPORTDIR:=import-$(TARGET)
ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS=-lm -lpthread
endif
endif

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR:=
MFLAGS=--RTS=$(abspath $(DRAKE_RTSDIR))
endif

CFLAGS=-gnata -gnatwa -gnatyy-3chbs

OFFICIAL_DEMO:=$(addprefix $(BUILDDIR)/,$(notdir $(basename $(wildcard official-demo/*.adb))))
ifneq ($(DRAKE),)
OFFICIAL_DEMO:=$(filter-out $(BUILDDIR)/mtest,$(OFFICIAL_DEMO))
endif

.PHONY: all clean

all: $(BUILDDIR)/test_di $(BUILDDIR)/version $(OFFICIAL_DEMO)

$(BUILDDIR)/%: %.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard $(SOURCEDIR)/*)
	cd $(BUILDDIR) && $(GNATMAKE) -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< -largs $(LFLAGS)

$(OFFICIAL_DEMO): $(BUILDDIR)/%: official-demo/%.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard $(SOURCEDIR)/*)
	cd $(BUILDDIR) && $(GNATMAKE) -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) ../$< -largs $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada -gcc=$(GCC) $(addprefix -isystem ,$(INCDIR)) -p -D $(IMPORTDIR) $+
endif

clean:
	-rm -rf build* import*
	-rm test_di.gz testzlib.out testzlib.zlb testzlib.in
