GHCMAKE = $(GHC) --make
GHCFLAGS =-O2 -fno-warn-tabs -XForeignFunctionInterface -i./lib # -rtsopts -i./lib  # -prof -auto-all -caf-all -fforce-recomp
OFILES=./lib/trie_lib.c # ./lib/rts.c
SALDO_TMP=$(SALDO)-bin$(EXEEXT)
DALIN_TMP=$(DALIN)-bin$(EXEEXT)
FSV_TMP=$(FSV)-bin$(EXEEXT)
SALDOP=$(SALDO)$(EXEEXT)
DALINP=$(DALIN)$(EXEEXT)
FSVP=$(FSV)$(EXEEXT)
LATIN_TMP=$(LATIN)-bin$(EXEEXT)
LATINP=$(LATIN)$(EXEEXT)

all: saldo dalin fsv latin

saldo: ./lib/trie_lib.c ./lib/trie_lib.h # ./lib/rts.c
	$(GHCMAKE) ./saldo/Main.hs $(OFILES) $(GHCFLAGS) -i./saldo -o $(SALDO_TMP)
	strip $(SALDO_TMP)
	mv $(SALDO_TMP) ../bin/$(SALDOP)

dalin: ./lib/trie_lib.c ./lib/trie_lib.h # ./lib/rts.c
	$(GHCMAKE) ./dalin/Main.hs $(OFILES) $(GHCFLAGS) -i./dalin -o $(DALIN_TMP)
	strip $(DALIN_TMP)
	mv $(DALIN_TMP) ../bin/$(DALINP)

fsv: ./lib/trie_lib.c ./lib/trie_lib.h  # ./lib/rts.c
	$(GHCMAKE) ./fsv/Main.hs $(OFILES) $(GHCFLAGS) -i./fsv -o $(FSV_TMP)
	strip $(FSV_TMP)
	mv $(FSV_TMP) ../bin/$(FSVP)

latin: ./lib/trie_lib.c ./lib/trie_lib.h  # ./lib/rts.c
	$(GHCMAKE) ./latin/Main.hs $(OFILES) $(GHCFLAGS) -i./latin -o $(LATIN_TMP)
	strip $(LATIN_TMP)
	mv $(LATIN_TMP) ../bin/$(LATINP)

.PHONY: saldo dalin fsv
