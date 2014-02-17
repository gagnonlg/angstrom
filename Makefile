HFLAGS = -O -W -fno-warn-unused-binds

all: Generator Exs

AudioStream: AudioStream.hs
	ghc $(HFLAGS) -c AudioStream.hs

Generator: Generator.hs
	ghc $(HFLAGS) -c Generator.hs

Exs: Generator AudioStream examples/Ex1.hs examples/Ex2.hs
	ghc $(HFLAGS) examples/Ex1.hs
	ghc $(HFLAGS) examples/Ex2.hs

sounds: Exs
	./examples/Ex1
	./examples/Ex2

clean:
	rm -f *.o
	rm -f examples/*.o
	rm -f *.hi
	rm -f examples/*.hi
	rm -f *.raw
	rm -f *.flac
	rm -f examples/Ex1
	rm -f examples/Ex2
	rm -f Scratch

# vim: ai:ts=4:sw=4:et!
