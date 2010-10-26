SOURCEDIR=../source
BUILDDIR=build
IMPORTDIR=import

ifeq ($(DRAKE),)
H=$(SOURCEDIR)/import.h
RTSDIR=
MFLAGS=
LFLAGS=
else
H=$(IMPORTDIR)/import2.h $(IMPORTDIR)/import-drake.h $(IMPORTDIR)/import-lib.h
RTSDIR=rts
MFLAGS=--RTS=$(abspath $(RTSDIR)) -a
LFLAGS=-largs -lc-gnat
endif

CFLAGS=-gnata -gnatwa -gnatyy-3chbs

OFFICIAL_DEMO=$(addprefix $(BUILDDIR)/,$(notdir $(basename $(wildcard official-demo/*.adb))))

.PHONY: all clean $(BUILDDIR)/test_di $(OFFICIAL_DEMO)

all: $(BUILDDIR)/test_di $(OFFICIAL_DEMO)

$(BUILDDIR)/test_di: test_di.adb $(BUILDDIR) $(IMPORTDIR)/c.ads $(RTSDIR)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< $(LFLAGS)

$(OFFICIAL_DEMO): $(BUILDDIR)/%: official-demo/%.adb $(BUILDDIR) $(IMPORTDIR)/c.ads $(RTSDIR)
	cd $(BUILDDIR) && gnatmake -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) ../$< $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(IMPORTDIR)/c.ads: $(H)
	headmaster -p --to ada -gcc=$(dir $(shell which gnatmake))gcc -D $(IMPORTDIR) $<

ifneq ($(DRAKE),)

$(IMPORTDIR)/import2.h:
	-mkdir $(IMPORTDIR)
	echo \#include \"import-drake.h\" > $@
	echo \#include \"import-lib.h\" >> $@

$(IMPORTDIR)/import-drake.h: $(DRAKE)/import.h
	ln -s $(abspath $<) $@

$(IMPORTDIR)/import-lib.h: $(SOURCEDIR)/import.h
	ln -s $(abspath $<) $@

$(RTSDIR):
	make -C $(DRAKE) RTSDIR=$(abspath $(RTSDIR)) IMPORTDIR=$(abspath $(IMPORTDIR))

endif

clean:
	-rm -rf build
	-rm -rf import
	-rm -rf rts
	-rm test_di.gz testzlib.out testzlib.zlb testzlib.in
