SOURCEDIR=../source
BUILDDIR=build
IMPORTDIR:=import

ifneq ($(DRAKE_RTSROOT),)
HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)
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

.PHONY: all clean $(BUILDDIR)/test_di $(OFFICIAL_DEMO)

all: $(BUILDDIR)/test_di $(OFFICIAL_DEMO)

$(BUILDDIR)/test_di: test_di.adb $(BUILDDIR) $(IMPORTDIR)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< $(LFLAGS)

$(OFFICIAL_DEMO): $(BUILDDIR)/%: official-demo/%.adb $(BUILDDIR) $(IMPORTDIR)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) ../$< $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster -p --to ada -gcc=$(dir $(shell which gnatmake))gcc -D $(IMPORTDIR) $+
endif

clean:
	-rm -rf build
	-rm -rf import
	-rm test_di.gz testzlib.out testzlib.zlb testzlib.in
