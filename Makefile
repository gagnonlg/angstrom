HFLAGS = -O -W -fno-warn-unused-binds

all: Generator Exs

Generator: Generator.hs
	ghc $(HFLAGS) -c Generator.hs

Exs: Generator Ex1.hs
	ghc $(HFLAGS) Ex1.hs
	ghc $(HFLAGS) Ex2.hs

sounds: Exs
	./Ex1
	./Ex2

clean:
	rm -f *.o
	rm -f *.hi
	rm -f *.raw
	rm -f *.flac
	rm -f Ex1
	rm -f Ex2
	rm -f Scratch

# vim: ai:ts=4:sw=4:et!
