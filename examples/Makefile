SOURCEDIR=../source

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

VERSION=$(shell gcc -dumpversion)

BUILDDIR=$(TARGET).build

ifeq ($(TARGET),$(HOST))
GNATMAKE=gnatmake
GCC=$(dir $(shell which $(GNATMAKE)))gcc
BINLN=bin
else
GNATMAKE=$(TARGET)-gnatmake
GCC=$(TARGET)-gcc
BINLN=
ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS=-lm -lpthread
endif
endif

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR=
MFLAGS=--RTS=$(abspath $(DRAKE_RTSDIR))
else
IMPORTDIR=$(BUILDDIR)/import
MFLAGS=-I$(abspath $(IMPORTDIR))
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))

CFLAGS=-gnata -gnatwa -gnatyy-3chbs

OFFICIAL_DEMO:=$(addprefix $(BUILDDIR)/,$(notdir $(basename $(wildcard official-demo/*.adb))))
ifneq ($(DRAKE),)
OFFICIAL_DEMO:=$(filter-out $(BUILDDIR)/mtest,$(OFFICIAL_DEMO))
endif

.PHONY: all clean $(EXAMPLES)

all: $(BUILDDIR)/str $(BUILDDIR)/stream $(BUILDDIR)/test_di $(BUILDDIR)/version $(OFFICIAL_DEMO) $(BINLN)

$(BUILDDIR)/%: %.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard $(SOURCEDIR)/*)
	cd $(BUILDDIR) && $(GNATMAKE) -g $(MFLAGS) -I../$(SOURCEDIR) $(CFLAGS) ../$< -largs $(LFLAGS)

$(OFFICIAL_DEMO): $(BUILDDIR)/%: official-demo/%.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard $(SOURCEDIR)/*)
	cd $(BUILDDIR) && $(GNATMAKE) -g $(MFLAGS) -I../$(SOURCEDIR) ../$< -largs $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BINLN): $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

$(EXAMPLES): %: $(BUILDDIR)/%

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada -gcc=$(GCC) $(addprefix -isystem ,$(INCDIR)) -p -D $(IMPORTDIR) $+
endif

clean:
	-rm -rf *.build bin
	-rm test_di.gz testzlib.out testzlib.zlb testzlib.in
