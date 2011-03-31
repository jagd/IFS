Zweig: Zweig.hs
	ghc --make -O3 $< -o $@

clean:
	-rm *.o *.hi
