BINEXT= # or ".exe"
RM=rm
GHC=ghc
GHCFLAGS=--make -O3

EXES=Zweig$(BINEXT) Baum$(BINEXT) Blatt$(BINEXT) 

.PHONY: all clean

all: $(EXES)

Zweig$(BINEXT): Zweig.hs
	$(GHC) $(GHCFLAGS) $< -o $@

Baum$(BINEXT): Baum.hs
	$(GHC) $(GHCFLAGS) $< -o $@

Blatt$(BINEXT): Blatt.hs
	$(GHC) $(GHCFLAGS) $< -o $@

clean:
	-$(RM) *.o *.hi
	-$(RM) IFS/*.hi IFS/*.o # leichter zu Windows portieren
	-$(RM) IFS/Render/*.hi IFS/Render/*.o
	-$(RM) $(EXES)
