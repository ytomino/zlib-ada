SOURCEDIR=../source

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

VERSION=$(shell gcc -dumpversion)

BUILDDIR=$(TARGET).build

ifeq ($(TARGET),$(HOST))
GNATPREFIX=
GCC=
BINLN=bin
else
GNATPREFIX=$(TARGET)-
GCC=$(TARGET)-gcc
BINLN=
endif

GFLAGS=
CFLAGS=-ggdb -pipe -gnatef -gnata -gnatwa -gnatyy-3chbs
MFLAGS=-D $(BUILDDIR)
BFLAGS=-E
LFLAGS=-ggdb
HFLAGS=

ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS=-lm -lpthread
endif

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR=
GFLAGS+=--RTS=$(abspath $(DRAKE_RTSDIR))
else
IMPORTDIR=$(BUILDDIR)/import
MFLAGS+=-I$(IMPORTDIR)
endif

ifneq ($(GCC),)
GFLAGS+=$(filter -m32 -m64,$(GCC))
MFLAGS+=--GCC="$(GCC)"
LFLAGS+=--GCC="$(GCC)"
HFLAGS+=-gcc="$(GCC)"
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))

OFFICIAL_DEMO:=$(notdir $(basename $(wildcard official-demo/*.adb)))

.PHONY: all clean $(EXAMPLES) $(OFFICIAL_DEMO)

all: $(EXAMPLES) $(OFFICIAL_DEMO)

$(BUILDDIR)/%: %.adb $(wildcard $(SOURCEDIR)/*) | $(BUILDDIR) $(IMPORTDIR)
	$(GNATPREFIX)gnatmake -c -I$(SOURCEDIR) $< $(GFLAGS) $(MFLAGS) -cargs $(CFLAGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatmake -b $(basename $(notdir $<)).ali $(GFLAGS) -bargs $(BFLAGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o ../$@ $(basename $(notdir $<)).ali $(GFLAGS) $(LFLAGS)

$(BUILDDIR)/%: official-demo/%.adb $(wildcard $(SOURCEDIR)/*) | $(BUILDDIR) $(IMPORTDIR)
	$(GNATPREFIX)gnatmake -c -I$(SOURCEDIR) $< $(GFLAGS) $(MFLAGS) -cargs $(CFLAGS) -gnatwA -gnatyN
	cd $(BUILDDIR) && $(GNATPREFIX)gnatmake -b $(basename $(notdir $<)).ali $(GFLAGS) -bargs $(BFLAGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o ../$@ $(basename $(notdir $<)).ali $(GFLAGS) $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BINLN): | $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

$(EXAMPLES): %: $(BUILDDIR)/% $(BINLN)
$(OFFICIAL_DEMO): %: $(BUILDDIR)/% $(BINLN)

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada $(HFLAGS) -p -D $@ $+
	touch $@
endif

clean:
	-rm -rf *.build bin
	-rm test_di.gz testzlib.out testzlib.zlb testzlib.in
