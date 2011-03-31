BINEXT= # or ".exe"
RM=rm
GHC=ghc
GHCFLAGS=--make -O3

EXES=Zweig$(BINEXT)

.PHONY: all clean

all: $(EXES)

Zweig$(BINEXT): Zweig.hs
	$(GHC) $(GHCFLAGS) $< -o $@

clean:
	-$(RM) *.o *.hi
	-$(RM) IFS/*.hi IFS/*.o # leichter zu Windows portieren
	-$(RM) IFS/Render/*.hi IFS/Render/*.o
	-$(RM) $(EXES)
