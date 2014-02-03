HFLAGS = -O -W

all: Generator Exs

Generator: Generator.hs
	ghc $(HFLAGS) -c Generator.hs

Exs: Generator Ex1.hs
	ghc $(HFLAGS) Ex1.hs

sounds: Exs
	./Ex1

clean:
	rm -f *.o
	rm -f *.hi
	rm -f *.raw
	rm -f *.flac
	rm -f Ex1

# vim: ai:ts=4:sw=4:et!
