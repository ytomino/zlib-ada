SOURCEDIR=../source

HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

BUILDDIR=$(TARGET).noindex

ifneq ($(TARGET),$(HOST))
 GCCPREFIX=$(TARGET)-
 BINLN=
else
 GCCPREFIX=
 BINLN=bin
endif
GCCSUFFIX=
GCC=$(GCCPREFIX)gcc$(GCCSUFFIX)

CFLAGS=-ggdb -pipe
CFLAGS_ADA=-gnata -gnatef -gnatwa -gnatyy-3bchms
LDFLAGS=-ggdb
HFLAGS=

ifneq ($(findstring freebsd,$(TARGET)),)
 LDFLAGS=-lm -lpthread
endif

ifneq ($(DRAKE_RTSROOT),)
 VERSION:=$(shell $(GCC) -dumpversion)
 DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
 IMPORTDIR=
else
 ifneq ($(wildcard import/$(TARGET)),)
  IMPORTDIR=import/$(TARGET)
 else
  IMPORTDIR=$(BUILDDIR)/import
 endif
endif

GARGS=$(addprefix --RTS=,$(abspath $(DRAKE_RTSDIR)))
MARGS=-D $(BUILDDIR) -gnatA $(addprefix -gnatec=,$(wildcard *.adc)) \
      $(addprefix -I,$(IMPORTDIR))
CARGS=$(CFLAGS) $(CFLAGS_ADA)
BARGS=-E
LARGS=$(LDFLAGS)

ifneq ($(GCC),gcc)
 HFLAGS+=-gcc="$(GCC)"
 GARGS+=$(filter -m32 -m64,$(GCC))
 MARGS+=--GCC="$(GCC)"
 LARGS+=--GCC="$(GCC)"
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))
TESTS=$(filter test_%,$(EXAMPLES))

OFFICIAL_DEMO:=$(notdir $(basename $(wildcard official-demo/*.adb)))

.PHONY: all check clean $(EXAMPLES) $(OFFICIAL_DEMO)

all: $(addprefix $(BUILDDIR)/,$(EXAMPLES) $(OFFICIAL_DEMO)) $(BINLN)

$(BUILDDIR)/%: %.adb $(wildcard $(SOURCEDIR)/*) | $(BUILDDIR) $(IMPORTDIR)
	$(GCCPREFIX)gnatmake$(GCCSUFFIX) -c -I$(SOURCEDIR) $< $(GARGS) $(MARGS) \
		-cargs $(CARGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatbind$(GCCSUFFIX) \
		$(basename $(notdir $<)).ali $(GARGS) $(BARGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatlink$(GCCSUFFIX) -o ../$@ \
		$(basename $(notdir $<)).ali $(GARGS) $(LARGS)

$(BUILDDIR)/%: official-demo/%.adb $(wildcard $(SOURCEDIR)/*) | $(BUILDDIR) $(IMPORTDIR)
	$(GCCTPREFIX)gnatmake$(GCCSUFFIX) -c -I$(SOURCEDIR) $< $(GARGS) $(MARGS) \
		-cargs $(CARGS) -gnatwA -gnatyN
	cd $(BUILDDIR) && $(GCCPREFIX)gnatbind$(GCCSUFFIX) \
		$(basename $(notdir $<)).ali $(GARGS) $(BARGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatlink$(GCCSUFFIX) -o ../$@ \
		$(basename $(notdir $<)).ali $(GARGS) $(LARGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BINLN): | $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

$(filter-out test_%,$(EXAMPLES)) $(OFFICIAL_DEMO): %: $(BUILDDIR)/% $(BINLN)

check: all $(TESTS)

$(TESTS): %: $(BUILDDIR)/%
	$<

ifeq ($(IMPORTDIR),$(BUILDDIR)/import)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada $(HFLAGS) -p -D $@ $+
	touch $@
endif

clean:
	-rm -r *.noindex bin
	-rm testzlib.out testzlib.zlb testzlib.in
