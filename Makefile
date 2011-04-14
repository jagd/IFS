BINEXT=# or ".exe"
RM=rm
GHC=ghc
GHCFLAGS=--make -O3

EXES=Zweig$(BINEXT) Baum$(BINEXT) Blatt$(BINEXT) BlattAhorn$(BINEXT)

.PHONY: all clean

all: $(EXES)

BlattAhorn$(BINEXT): BlattAhorn.hs
	$(GHC) $(GHCFLAGS) $< -o $@

Zweig$(BINEXT): Zweig.hs
	$(GHC) $(GHCFLAGS) $< -o $@

Baum$(BINEXT): Baum.hs
	$(GHC) $(GHCFLAGS) $< -o $@

Blatt$(BINEXT): Blatt.hs
	$(GHC) $(GHCFLAGS) $< -o $@

clean:
	-$(RM) *.o *.hi
	# leichter zu Windows portieren
	-$(RM) IFS/*.hi IFS/*.o
	-$(RM) IFS/Render/*.hi IFS/Render/*.o
	-$(RM) $(EXES)
