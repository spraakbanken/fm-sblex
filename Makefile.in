include config.mk

.PHONY: all install clean

all: force
	cd src; make -f ../config.mk -f Makefile;cd ..
saldo: force
	cd src; make -f ../config.mk -f Makefile saldo;cd ..
dalin: force
	cd src; make -f ../config.mk -f Makefile dalin;cd ..
fsv: force
	cd src; make -f ../config.mk -f Makefile fsv;cd ..
force: ;
install:
	$(INSTALL) -d $(bindir)
	$(INSTALL) bin/$(SALDO)$(EXEEXT) $(bindir)
	$(INSTALL) bin/$(DALIN)$(EXEEXT) $(bindir)
	$(INSTALL) bin/$(FSV)$(EXEEXT) $(bindir)
clean:
	find . '(' -name '*~' -o -name '*.hi' -o -name '*.ghi' -o -name '*.o' ')' -exec rm -f '{}' ';'
